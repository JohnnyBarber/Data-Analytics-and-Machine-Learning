library(rtweet)
library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(xts)
library(tm)
library(glmnet)
library(pROC)
library(SnowballC)
library(ROCR)
library(stargazer)
library(sandwich)
library(lmtest)

trump = read.csv("trumptweets.csv")
trump = as.data.frame(lapply(trump,as.character), stringsAsFactors=FALSE ) %>% as.data.table()

trump[,new_text := tolower(text)]
to_replace = c("will","thank","realdonaldtrump","realdonald","mrtrump","realtrump",
               "tonight","trump2016","&amp","amp","night","makeamericagreatagain",
               "today", "donald","trump", "make america great again")
f_replace = function(x){
  out = str_replace(trump$new_text,x,"")
  return(out)
}
for(i in to_replace){
  trump[, new_text := f_replace(i)]
}

trump$new_text = gsub("b\"|b'|\\\\|\\\"", "", trump$new_text)
trump$new_text = gsub("([<>])|[[:punct:]]", "\\1", trump$new_text)
trump.df1=gsub("http.*"," ",trump$new_text)
trump.df1=gsub("https.*"," ",trump.df1)
trump.df1=gsub("#.*"," ",trump.df1)
trump.df1=gsub("@.*"," ",trump.df1)

trump[,new_text := trump.df1]

trump$date <- gsub("T.*$","", trump$created_at, perl=T)
trump[,date := as.Date(date)]
trump[,time := substr(created_at,12,19)]
trump[,`:=` (tNum=as.numeric(strptime(time,"%H:%M:%S")),
             cut=as.numeric(strptime("16:00:00","%H:%M:%S")))]
trump[,after4 := ifelse(tNum>cut,1,0)]
trump[,`:=` (tNum = NULL, cut = NULL)]

#SP index
trump = trump[,list(date,time,hashtags,new_text,after4)]
SP = read.csv("SP index.csv") %>% as.data.table()
SP[,date:=as.Date(as.character(caldt),format = "%Y%m%d")]
combine = merge(trump,SP,by="date",all.x = T, allow.cartesian = T)
start = as.Date("2009-05-04")
end = as.Date("2018-12-28")
combine = combine[date >= start & date <= end,]
combine[,label := ifelse(sprtrn > 0, 1, 0)]

trumpFreq = read.csv("trumptweets.csv")
ts_plot(trumpFreq,"weeks")


#data-------------------------------------------------
Corpus = Corpus(VectorSource(combine$new_text[25000:38373]))
Corpus = tm_map(Corpus,removePunctuation)
Corpus = tm_map(Corpus, content_transformer(gsub),pattern="\t", replacement = " ")
Corpus = tm_map(Corpus,removeNumbers)
Corpus = tm_map(Corpus,tolower)
Corpus = tm_map(Corpus,removeWords,c(stopwords(kind="SMART"),"<s>"))
Corpus = tm_map(Corpus,stripWhitespace)

dtm = DocumentTermMatrix(Corpus)
#inspect(dtm[5:10,801:810])

freq = colSums(as.matrix(dtm))
ord_corpus = order(freq)
freq[tail(ord_corpus)]

word_freq = data.table(word = names(freq),freq = freq)
out = ggplot(word_freq[freq>300], aes(word,freq)) + 
  geom_bar(fill = "royalblue4", color = "black", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
out

#Text Regression-----------------------------------------------------------------------------
sentiment = c(word_freq[freq>200]$word)
dtm_sentiment = dtm[, sentiment]
x_data_sent = as.matrix(dtm_sentiment)
y_data = as.factor(combine$label[25000:38373])

glm.fit <- glm(y_data ~ x_data_sent, family = "binomial")
summary(glm.fit)

pred_fit =  as.numeric(predict(glm.fit, type = "response"))
ROC_enet = roc(response = na.omit(y_data), predictor = pred_fit, 
               smooth = TRUE, plot = TRUE, grid = TRUE)

simple_roc <- function(labels, scores) {
  labels <- labels[order(scores, decreasing = TRUE)]
  data.frame(TPR = cumsum(labels)/sum(labels), FPR = cumsum(!labels)/sum(!labels),
             labels) }

y_data=na.omit(y_data)
glm_roc <- simple_roc(y_data == "1", pred_fit) 
TPR1 <- glm_roc$TPR
FPR1 <- glm_roc$FPR
data1 <- data.table(TPR = TPR1, FPR = FPR1)
ggplot(data1, aes(x = FPR, y = TPR)) + geom_line(color = "tomato2", size = 1.2) + 
  ggtitle("ROC Curve for dtm_sentiment logit model") + 
  geom_abline(slope = 1, intercept = 0, linetype = "longdash") + theme_bw()

#compare--------------------------------------------------------------------------
b = as.character(y_data) %>% as.numeric()
test = cbind(x_data_sent,b) %>% na.omit()
a = test[,1:63]
b = as.factor(test[,64])

glmnet.fit <- cv.glmnet(x = a, y = b, family = "binomial", alpha = 0.5)
plot.cv.glmnet(glmnet.fit)

plot.glmnet(glmnet.fit$glmnet.fit, "lambda", label = TRUE)

#try elastic net prediction
preds_enet <- as.numeric(predict(glmnet.fit, newx = a, type = "response",
                                 s = "lambda.min"))
glmnet_roc <- simple_roc(b == "1", preds_enet)
TPR2 <- glmnet_roc$TPR
FPR2 <- glmnet_roc$FPR
data2 <- data.table(TPR = TPR2, FPR = FPR2)
data1[, `:=`(Model, "Logistic")]
data2[, `:=`(Model, "Elastic Net")]
data12 <- rbind(data1, data2)
# Plot the corresponding ROC curve
ggplot(data12, aes(x = FPR, y = TPR, color = Model)) + geom_line(size = 1.2) +
  ggtitle("ROC Curve for dtm_sentiment (logit and elastic net)") + 
  geom_abline(slope = 1, intercept = 0, linetype = "longdash") + theme_bw()
