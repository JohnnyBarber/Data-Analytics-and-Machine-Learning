# LDA topic allocation modeling, Topic 7b
# Load packages
library(rtweet)
library(data.table)
library(tm)
library(glmnet)
library(ggplot2)
library(pROC)
library(SnowballC)
library(dplyr)
library(wordcloud)
library(ROCR)
library(stargazer)
library(sandwich)
library(lmtest)
library(lda)
library(stringr)
library(lubridate)
library(tis)

setwd("C:/Users/Jiaqi Li/Desktop/class materials/quarter 3/Data Analysis and Machine Learning/final presentation")

trump <- read.csv("trumptweets.csv")
trump <- as.data.table(trump)

trump[, date := lubridate::ymd(substr(created_at, 1, 10))]

trump[,time := substr(created_at,12,19)]
trump[,`:=` (tNum=as.numeric(strptime(time,"%H:%M:%S")),
             cut=as.numeric(strptime("16:00:00","%H:%M:%S")))]
trump[,after4 := ifelse(tNum>cut,1,0)]
trump[,`:=` (tNum = NULL, cut = NULL)]

# select date in-sample
trumpIS <- trump[date>="2016-11-08" & date<="2018-6-30"]
# just learn topics of top 2 headlines per day (to save time)
data <- trumpIS$text

## Get rid of those pesky b's and backslashes you see if you inspect the raw data
data <- gsub('b"|b\'|\\\\|\\"', "", data)

## Get rid of all punctuation except headline separators, alternative to cleaning done in tm-package
data <- gsub("([<>])|[[:punct:]]", "\\1", data)


# lda routine requires documents to be in specific list format:
# A list whose length is equal to the number of documents, D. Each element of
# documents is an integer matrix with two rows.  Each column of
# documents[[i]] (i.e., document i) represents a word occurring in the document.
# documents[[i]][1, j] is a 0-indexed word identifier for the jth word in document
# i.  That is,  this should be an index - 1 into vocab. documents[[i]][2,  j] is an integer specifying the number of
# times that word appears in the document.

# create large list for lda routine, split (tokenize) on space
doc.list <- strsplit(data, "[[:space:]]+")
doc.list <- sapply(doc.list, tolower)

to_replace = c("will","thank","realdonaldtrump","tonight","trump2016",
               "amp","night","makeamericagreatagain", "today", "trump",
               "realdonald", "2015", "2016", "2017", "2018", "think",
               "make america great again", "make american great again", "maga")

doc.list <- sapply(doc.list, function(x){
  x[!(x %in% to_replace)]
})

# create a table of terms
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop_words or occur less than 100 times
del <- names(term.table) %in% tm::stopwords(kind = 'en') | term.table < 2
term.table <- term.table[!del]

# list of words in corpus, for use by lda
vocab = names(term.table)

# define function that helps getting the text data in right format for lda
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1,length(index))))
}

# create list version of data
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set
D <- length(documents)  # number of documents (1989)
W <- length(vocab)  # number of terms in the vocab (8848)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [11,3, 4...]

N <- sum(doc.length)  # total number of tokens/terms in the data
term.frequency <- as.integer(term.table) # Frequency of each of the terms

# lda model tuning parameters
K <- 5
G <- 5000     # Number of iterations to arrive at convergence 
alpha <- 0.02
eta <- 0.02

# Fit the model
set.seed(357)

# Begin lda execution
lda_fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                       num.iterations = G, alpha = alpha, 
                                       eta = eta, initial = NULL, burnin = 0,
                                       compute.log.likelihood = TRUE)


# show top topic words
top.words_Is <- top.topic.words(lda_fit$topics, num.words = 10, by.score = FALSE)
top.words_Is

# show top documents per topic
top.documents_Is <- top.topic.documents(lda_fit$document_sums, num.documents = 100, alpha = alpha)
top.documents_Is

# look at top documents in each topic (most representative)
data[top.documents_Is[1,1]]
data[top.documents_Is[1,2]]
data[top.documents_Is[1,3]]
data[top.documents_Is[1,4]]
data[top.documents_Is[1,5]]


# get the predictive model probability that a word will show up in document
pred.dist <- predictive.distribution(lda_fit$document_sums, lda_fit$topics, alpha = alpha, eta = eta)
# words in rows, document in columns
pred.dist[1:10,1:8]

# note that this can easily be used as a similarity measure to gauge similarity of two documents
# e.g.
sim_measure12 <- t(pred.dist[,1]) %*% pred.dist[,2]
sim_measure12
sim_measure13 <- t(pred.dist[,1]) %*% pred.dist[,3]
sim_measure13

# get probability that each document belongs to a certain topic
topic.proportions <- t(lda_fit$document_sums) / colSums(lda_fit$document_sums)
topic.proportions <- topic.proportions[(1:dim(topic.proportions)[1]),]
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- paste0("topic", 1:K)


# LDA and daily returns
lda_results <- cbind(trumpIS[, .(date, after4)], topic.proportions)
lda_results[, ret_date := as.Date(ifelse(after4, date + 1, date))]
lda_results[, ret_date := as.Date(ifelse(wday(ret_date) %in% c(1, 7) | isHoliday(ret_date), lubridate::ymd(nextBusinessDay(ret_date)), ret_date))]

# daily data
daily_data <- as.data.table(read.csv("GSPC.csv"))
daily_data[, date := lubridate::ymd(Date)]
daily_data[, ret := Adj.Close/shift(Adj.Close)-1]
daily_data[, paste0("lag", 1:20, "ret") := shift(ret, 1L:20L)]
daily_data[, paste0("lead", 1:20, "ret") := shift(ret, 1L:20L, type = "lead")]

# merge data
lda_ret <- merge(lda_results, daily_data, by.x = "ret_date", by.y = "date")


# # find weighted returns
lda_ret_complete <- na.omit(lda_ret)

# LDA FULL SAMPLE
# select date
trumpOos <- trump[date>="2016-11-08"]
# just learn topics of top 2 headlines per day (to save time)
data <- trumpOos$text

## Get rid of those pesky b's and backslashes you see if you inspect the raw data
data <- gsub('b"|b\'|\\\\|\\"', "", data)

## Get rid of all punctuation except headline separators, alternative to cleaning done in tm-package
data <- gsub("([<>])|[[:punct:]]", "\\1", data)

# create large list for lda routine, split (tokenize) on space
doc.list <- strsplit(data, "[[:space:]]+")
doc.list <- sapply(doc.list, tolower)

to_replace = c("will","thank","realdonaldtrump","tonight","trump2016",
               "amp","night","makeamericagreatagain", "today", "trump",
               "realdonald", "2015", "2016", "2017", "2018", "think",
               "make america great again", "make american great again", "maga")

doc.list <- sapply(doc.list, function(x){
  x[!(x %in% to_replace)]
})

# create a table of terms
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop_words or occur less than 100 times
del <- names(term.table) %in% tm::stopwords(kind = 'en') | term.table < 2
term.table <- term.table[!del]

# list of words in corpus, for use by lda
vocab = names(term.table)

# define function that helps getting the text data in right format for lda
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1,length(index))))
}

# create list version of data
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set
D <- length(documents)  # number of documents (1989)
W <- length(vocab)  # number of terms in the vocab (8848)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [11,3, 4...]

N <- sum(doc.length)  # total number of tokens/terms in the data
term.frequency <- as.integer(term.table) # Frequency of each of the terms

# lda model tuning parameters
K <- 5
G <- 5000     # Number of iterations to arrive at convergence 
alpha <- 0.02
eta <- 0.02

# Fit the model
set.seed(357)
# Begin lda execution
lda_fit_Oos <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                           num.iterations = G, alpha = alpha, 
                                           eta = eta, initial = NULL, burnin = 0,
                                           compute.log.likelihood = TRUE)

# show top topic words
top.words_Oos <- top.topic.words(lda_fit_Oos$topics, num.words = 10, by.score = FALSE)
top.words_Oos

# get probability that each document belongs to a certain topic
topic.proportions <- t(lda_fit_Oos$document_sums) / colSums(lda_fit_Oos$document_sums)
topic.proportions <- topic.proportions[(1:dim(topic.proportions)[1]),]
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- paste0("topic", 1:K)


# LDA and daily returns
lda_results_Oos <- cbind(trumpOos[, .(date, after4)], topic.proportions)
lda_results_Oos[, ret_date := as.Date(ifelse(after4, date + 1, date))]
lda_results_Oos[, ret_date := as.Date(ifelse(wday(ret_date) %in% c(1, 7) | isHoliday(ret_date), lubridate::ymd(nextBusinessDay(ret_date)), ret_date))]



# S&P 500 Analysis
# regression discontinuity design
par(mfrow=c(2,3))
for (i in 1:5){
  ret <- (colMeans(unique(lda_ret_complete[get(paste0("topic",i))==1, c(paste0("lag", 20:1, "ret"), "ret", paste0("lead", 1:20, "ret"))])))
  plot(ret, pch = 16, main = paste0("Topic", i), xlab = "Days", ylab = "Daily Returns")
  abline(v=21, col = "red")
  x1 <- 1:21
  x2 <- 21:41
  lm1 <- lm(ret[1:21]~x1)
  lm2 <- lm(ret[21:41]~x2)
  segments(1, fitted(lm1)[1], 21, fitted(lm1)[21], col = "green")
  segments(21, fitted(lm2)[1], 41, fitted(lm2)[21], col = "orange")
}
# trend analysis
par(mfrow=c(3,2))
for (i in 1:5){
  cumRet <- cumprod(1+colMeans(unique(lda_ret_complete[get(paste0("topic",i))==1, c(paste0("lag", 20:1, "ret"), "ret", paste0("lead", 1:20, "ret"))])))-1
  plot(cumRet, type = "l", main = paste0("Topic", i), xlab = "Days", ylab = "Cumulative Returns")
  abline(v=21, col = "red")
  x1 <- 1:20
  x2 <- 21:41
  lm1 <- lm(cumRet[1:20]~x1)
  lm2 <- lm(cumRet[21:41]~x2)
  abline(lm1, col = "green")
  abline(lm2, col = "orange")
}

length(unique(lda_ret_complete[topic4==1]$ret_date))

# S&P 500 Sectors Analysis
sector_ret <- as.data.table(read.csv("data/SP 500 Sectors Historical Data.csv"))
setnames(sector_ret, "ï..Date", "date")
sector_ret[, date := mdy(date)]
# sector names
secNam <- colnames(sector_ret[,!"date"])
# merge sector returns with lda results
lda_sec <- merge(lda_results, sector_ret, by.x = "ret_date", by.y = "date")

for (i in 1:5){
  par(mfrow=c(3,4))
  for (sec in secNam){
    # analyze by sector
    secData <- lda_sec[, .(ret_date, get(paste0("topic",i)), get(sec))]
    setnames(secData, "V3", "ret")
    secData[, paste0("lag", 1:20, "ret") := shift(ret, 1L:20L)]
    secData[, paste0("lead", 1:20, "ret") := shift(ret, 1L:20L, type = "lead")]
    secData <- na.omit(secData)
    
    ret <- (colMeans(unique(secData[V2==1, c(paste0("lag", 20:1, "ret"), "ret", paste0("lead", 1:20, "ret"))])))
    plot(ret, type = "l", main = paste0("Topic", i, "-", "Sector - ", sec), xlab = "Days", ylab = "Daily Returns")
    abline(v=21, col = "red")
    x1 <- 1:21
    x2 <- 21:41
    lm1 <- lm(ret[1:21]~x1)
    lm2 <- lm(ret[21:41]~x2)
    segments(1, fitted(lm1)[1], 21, fitted(lm1)[21], col = "green")
    segments(21, fitted(lm2)[1], 41, fitted(lm2)[21], col = "orange")
  }
}


# TREND ANALYSIS
for (i in 1:5){
  par(mfrow=c(3,4))
  secCoefRatio <- c()
  for (sec in secNam){
    secData <- lda_sec[, .(ret_date, get(paste0("topic",i)), get(sec))]
    setnames(secData, "V3", "ret")
    secData[, paste0("lag", 1:20, "ret") := shift(ret, 1L:20L)]
    secData[, paste0("lead", 1:20, "ret") := shift(ret, 1L:20L, type = "lead")]
    secData <- na.omit(secData)
    
    cumRet <- cumprod(1+colMeans(unique(secData[V2==1, c(paste0("lag", 20:1, "ret"), "ret", paste0("lead", 1:20, "ret"))])))-1
    plot(cumRet, type = "l", main = paste0("Topic", i, "-", "Sector - ", sec), xlab = "Days", ylab = "Cumulative Returns")
    abline(v=21, col = "red")
    x1 <- 1:20
    x2 <- 21:41
    lm1 <- lm(cumRet[1:20]~x1)
    lm2 <- lm(cumRet[21:41]~x2)
    abline(lm1, col = "green")
    abline(lm2, col = "orange")
    secCoefRatio[sec] <- (coef(lm2)[2]-coef(lm1)[2])/coef(lm1)[2]*sign(coef(lm1)[2])
  }
  
  toLong <- secNam[which(secCoefRatio==max(secCoefRatio))]
  toShort <- secNam[which(secCoefRatio==min(secCoefRatio))]
  others <- secNam[!secNam %in% c(toLong, toShort)]
  
  # Trading Strategy - In-Sample
  # construct equally-weighted returns
  sec_trade_Is <- copy(sector_ret[date<="2018-12-31"])
  sec_trade_Is[, eqlWt := apply(.SD[, !"date"], 1, mean)]
  # merge sector returns with topic i
  sec_trade_Is <- merge(unique(lda_results[get(paste0("topic", i))==1,.(ret_date, topic = get(paste0("topic", i)))]), sec_trade_Is, by.x = "ret_date", by.y = "date", all.y = T)
  sec_trade_Is[is.na(topic)]$topic <- 0
  
  
  # adjust weight dummy
  sec_trade_Is[, paste0("topic_lag", 0:5) := shift(topic, 0L:5L, fill = 0)]
  sec_trade_Is[, adjDummy := ifelse(topic_lag0==1 |topic_lag1 ==1|
                                             topic_lag2==1 |topic_lag3 ==1|
                                             topic_lag4==1 |topic_lag5 ==1, 1, 0)]
  
  # adjust weight returns
  sec_trade_Is$eqlWt_adj <- 0
  sec_trade_Is[adjDummy==0]$eqlWt_adj <- sec_trade_Is[adjDummy==0]$eqlWt
  sec_trade_Is[adjDummy==1]$eqlWt_adj <- rowSums(sec_trade_Is[adjDummy==1,others, with = F]*1/11) + 
    sec_trade_Is[adjDummy==1,toLong, with = F]*1/11*2 + 
    sec_trade_Is[adjDummy==1,toShort, with = F]*1/11*-1
  
  
  # find cumulative returns
  sec_trade_Is[, cumEqlWt := cumprod(1+eqlWt)-1]
  sec_trade_Is[, cumEqlWt_adj := cumprod(1+eqlWt_adj)-1]
  
  par(mfrow=c(1,1))
  sec_trade_topic1_plot <- sec_trade_Is[,.(cumEqlWt, cumEqlWt_adj)]
  matplot(sec_trade_topic1_plot, type = "l", main = paste0("Topic ", i," - In-Sample: Long ", toLong, ", Short ", toShort), ylab = "Cumulative Returns")
  legend("bottomright", legend = c("Equal-weighted Benchmark", "Adjusted-weights Portfolio"), col = 1:2, lty = 1:2)
  
  # OUT OF SAMPLE PREDICTION
  # construct equally-weighted returns
  sec_trade_Oos <- copy(sector_ret[date>"2017-06-30"])
  sec_trade_Oos[, eqlWt := apply(.SD[, !"date"], 1, mean)]
  # merge sector returns with topic i
  sec_trade_Oos <- merge(unique(lda_results_Oos[get(paste0("topic", i))==1,.(ret_date, topic = get(paste0("topic", i)))]), sec_trade_Oos, by.x = "ret_date", by.y = "date", all.y = T)
  sec_trade_Oos[is.na(topic)]$topic <- 0
  # adjust weight dummy
  sec_trade_Oos[, paste0("topic_lag", 0:5) := shift(topic, 0L:5L, fill = 0)]
  sec_trade_Oos[, adjDummy := ifelse(topic_lag0==1 |topic_lag1 ==1|
                                              topic_lag2==1 |topic_lag3 ==1|
                                              topic_lag4==1 |topic_lag5 ==1, 1, 0)]
  
  # adjust weight returns
  sec_trade_Oos$eqlWt_adj <- 0
  sec_trade_Oos[adjDummy==0]$eqlWt_adj <- sec_trade_Oos[adjDummy==0]$eqlWt
  sec_trade_Oos[adjDummy==1]$eqlWt_adj <- rowSums(sec_trade_Oos[adjDummy==1,others, with = F]*1/11) + 
    sec_trade_Oos[adjDummy==1,toLong, with = F]*1/11*5 + 
    sec_trade_Oos[adjDummy==1,toShort, with = F]*1/11*-4
  
  # find cumulative returns
  sec_trade_Oos[, cumEqlWt := cumprod(1+eqlWt)-1]
  sec_trade_Oos[, cumEqlWt_adj := cumprod(1+eqlWt_adj)-1]
  
  par(mfrow=c(1,1))
  sec_trade_Oos_plot <- sec_trade_Oos[,.(cumEqlWt, cumEqlWt_adj)]
  matplot(sec_trade_Oos_plot, type = "l", main = paste0("Topic ", i," - Out-of-Sample: Long ", toLong, ", Short ", toShort), ylab = "Cumulative Returns")
  legend("bottomright", legend = c("Equal-weighted Benchmark", "Adjusted-weights Portfolio"), col = 1:2, lty = 1:2)
  
}


