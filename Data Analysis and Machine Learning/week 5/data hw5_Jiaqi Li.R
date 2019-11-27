library(data.table)
library(dplyr)
library(readxl)
library(ggplot2)

####################################################################
#                             Part A                               #
####################################################################

#---------------------------question 1-----------------------------#
Fport = read_excel("French_Portfolio_Returns.xlsx", skip = 1,
                   sheet = 3) %>% as.data.table()
Fport[, Date := as.Date(as.character(Date*100+1),format="%Y%m%d")]
setkey(Fport,Date)
sample = Fport[Date>="1963-07-01" & Date <= "2016-12-01",]
sample[,`:=`(Month = .I,Date = NULL)]

#---------------------------question 2-----------------------------#
names(sample)[1:138] = c(1:138)
Data = reshape(sample,varying = names(sample)[1:138],
               v.names = "ExRet", timevar = "Portfolio",
               times = names(sample)[1:138],
               direction = "long")
Data$id=NULL
Data$Month = as.numeric(Data$Month)
Data$Portfolio = as.numeric(Data$Portfolio)
Data$ExRet = as.numeric(Data$ExRet)

#---------------------------question 3-----------------------------#
Data[,`:=`(lag1Ret = shift(ExRet),
           lag2Ret = shift(ExRet,2)),
     by = Portfolio]
Data[,sumlagRet := Reduce(`+`,shift(ExRet,3:12)),by=Portfolio]

Data[,`:=`(lag1Ret_2 = shift(ExRet)^2,
           lag2Ret_2 = shift(ExRet,2)^2,
           sumlagRet_2 = sumlagRet^2),by = Portfolio]


####################################################################
#                             Part B                               #
####################################################################
library(glmnet)
library(foreign)
library(randomForest)
library(lfe)
library(xgboost)

#---------------------------question 1-----------------------------#
train = Data[complete.cases(Data),]
setkey(train,Month)
y_train = as.vector(as.matrix(train[.(13:(642-(2016-2009)*12)),list(ExRet)]))
x_train = as.matrix(train[.(13:(642-(2016-2009)*12)),list(Month, Portfolio,
          lag1Ret,lag2Ret,sumlagRet,lag1Ret_2,lag2Ret_2,sumlagRet_2)])
y_test = as.vector(as.matrix(train[.((642-(2016-2009)*12+1):642),list(ExRet)]))
x_test = as.matrix(train[.((642-(2016-2009)*12+1):642),list(Month, Portfolio,
          lag1Ret,lag2Ret,sumlagRet,lag1Ret_2,lag2Ret_2,sumlagRet_2)])

RF_train = randomForest(x_train,y_train,ntree = 500,maxnodes = 30,mtry = 2)

RF_train_pred = predict(RF_train,x_train)
train_felm = as.data.frame(cbind(y_train,RF_train_pred,
                                 Month = train[.(13:(642-(2016-2009)*12))]$Month))
summary(felm(y_train~RF_train_pred |0|0|Month, data = train_felm))
RF_test_pred = predict(RF_train,x_test)
test_felm = as.data.frame(cbind(y_test,RF_test_pred,
                                Month = train[.((642-(2016-2009)*12+1):642)]$Month))
summary(felm(y_test~RF_test_pred |0|0|Month, data = test_felm))

port_ret = NULL
row_counter_end = 0
for (i in (642-(2016-2009)*12+1):642)
{
  month_length <- Data[Month==i,]$ExRet %>% length()
  row_counter_start = row_counter_end + 1
  row_counter_end = row_counter_end + month_length
  x_temp <- RF_test_pred[row_counter_start:row_counter_end]
  y_temp <- y_test[row_counter_start:row_counter_end]
  fit_yr <- lm(y_temp ~ x_temp)
  temp <- coefficients(fit_yr)
  port_ret = rbind(port_ret,temp[2])
}
fm_RF_output = list(SR_Return = mean(port_ret)/sd(port_ret),
                    tstat_MeanRet = sqrt((2016-2009)*12-1)*
                      mean(port_ret)/sd(port_ret))
fm_RF_output

#---------------------------question 2-----------------------------#
params1 <- list(booster = "gbtree", objective = "reg:linear",
               eta = 0.1, gamma = 0, max_depth = 1)
params2 <- list(booster = "gbtree", objective = "reg:linear",
               eta = 0.1, gamma = 0, max_depth = 6)
params3 <- list(booster = "gbtree", objective = "reg:linear",
               eta = 0.3, gamma = 0, max_depth = 1)
params4 <- list(booster = "gbtree", objective = "reg:linear",
               eta = 0.3, gamma = 0, max_depth = 6)

for(i in 1:4){
  sink('NUL')
  xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
  xgbcv <- xgb.cv(params = get(paste("params",i,sep="")), data = xgb_train,
                  nfold = 10, nrounds = 200, showsd = T, print_every_n = 10)
  cv_nrounds = which.min(xgbcv$evaluation_log$test_rmse_mean)
  xgb_optb <- xgboost(params = get(paste("params",i,sep="")), data = xgb_train,
                      nround = cv_nrounds)
  xgb_train_pred <- predict(xgb_optb, xgb_train)
  train_felm = as.data.frame(cbind(y_train,xgb_train_pred,
                     Month = train[.(13:(642-(2016-2009)*12))]$Month))
  assign(paste("XGB_train",i,sep=""),
         summary(felm(y_train~xgb_train_pred|0|0|Month,data = train_felm)))
  xgb_test <- xgb.DMatrix(data = x_test, label = y_test)
  xgb_test_pred <- predict(xgb_optb, xgb_test)
  test_felm <- as.data.frame(cbind(y_test,xgb_test_pred,
                     Month = train[.((642-(2016-2009)*12+1):642)]$Month))
  assign(paste("XGB_test",i,sep=""),
         summary(felm(y_test~xgb_test_pred|0|0|Month,data = test_felm)))
  
  port_ret = NULL
  row_counter_end = 0
  for (j in (642-(2016-2009)*12+1):642)
  {
    month_length <- Data[Month==j,]$ExRet %>% length()
    row_counter_start = row_counter_end + 1
    row_counter_end = row_counter_end + month_length
    x_temp <- xgb_test_pred[row_counter_start:row_counter_end]
    y_temp <- y_test[row_counter_start:row_counter_end]
    fit_yr <- lm(y_temp ~ x_temp)
    temp <- coefficients(fit_yr)
    port_ret = rbind(port_ret,temp[2])
  }
  assign(paste("fm_xgb_output",i,sep=""),list(SR_Return = mean(port_ret)/sd(port_ret),
                      tstat_MeanRet = sqrt((2016-2009)*12-1)*mean(port_ret)/sd(port_ret)))
  sink()
}
#set 1
XGB_train1
XGB_test1
fm_xgb_output1

#set 2
XGB_train2
XGB_test2
fm_xgb_output2

#set 3
XGB_train3
XGB_test3
fm_xgb_output3

#set 4
XGB_train4
XGB_test4
fm_xgb_output4


####################################################################
#                             Part C                               #
####################################################################
library(MTS)
library(zoo)
library(sandwich)

#---------------------------question 1-----------------------------#
#names(sample)[1:138] = names(Fport)[2:139]
sample2 = Fport[Date>="1963-07-01" & Date <= "2016-12-01",]
sample2 = apply(sample2[,!"Date"],2,as.numeric) %>% as.data.table() %>% na.omit()
N = length(sample2$Agric)

APCA_loading = function(data,start,end,K){
  if(start < 0){
    return(list(NA))
  }else{
    sink('NUL')
    temp = apca(data[start:end,],K)
    sink()
    return(list(temp$loadings))
  }
}

apca_routine = list()
for(i in 1:N){
  test = APCA_loading(sample2,i-60,i,5)
  apca_routine = c(apca_routine,test)
}

N_loading = length(apca_routine)
for(i in 1:5){
  assign(paste("fact",i,"_ret",sep = ""),rep(NA,60))
}
for(i in 61:N_loading){
  fact1_ret[i] = sum(sample2[i,]*apca_routine[[i-1]][,1])
  fact2_ret[i] = sum(sample2[i,]*apca_routine[[i-1]][,2])
  fact3_ret[i] = sum(sample2[i,]*apca_routine[[i-1]][,3])
  fact4_ret[i] = sum(sample2[i,]*apca_routine[[i-1]][,4])
  fact5_ret[i] = sum(sample2[i,]*apca_routine[[i-1]][,5])
}
fact_ret = cbind(fact1_ret,fact2_ret,fact3_ret,fact4_ret,fact5_ret)

#part a------------------------------------------------------------
avgAnnFactRet = c(); AnnFactSD = c(); AnnFactSR = c(); Corr1mon = c()
for(i in 1:5){
  avgAnnFactRet[i] = mean(get(paste("fact",i,"_ret",sep="")),na.rm = T)*12
  AnnFactSD[i] = sd(get(paste("fact",i,"_ret",sep="")),na.rm = T)*sqrt(12)
  AnnFactSR[i] = avgAnnFactRet[i]/AnnFactSD[i]
  Corr1mon[i] = acf(na.omit(get(paste("fact",i,"_ret",sep=""))))$acf[2]
}
avgAnnFactRet
AnnFactSD
AnnFactSR
Corr1mon

#part b------------------------------------------------------------
FactTstat = c()
for(i in 1:5){
  FactTstat[i] = AnnFactSR[i]*sqrt((N-61+1)/12)
  assign(paste("se",i,sep=""),
         sqrt(vcovHAC(lm(get(paste("fact",i,"_ret",sep=""))~1))))
}
FactTstat

#part c------------------------------------------------------------


#part d------------------------------------------------------------
signal = sample2
lambda = NULL
for (i in 61:N){
  x_temp <- apca_routine[[i-1]]
  x_signal <- t(signal[i-1,])
  y_temp <- t(sample2[i,])
  fit_yr <- lm(y_temp ~ x_temp + x_signal + 0)
  temp <- coefficients(fit_yr)
  lambda = rbind(lambda,temp)
}
SignalMean = mean(lambda[,6])
SignalSD = sd(lambda[,6])
SignalTstat = sqrt(N-62+1)*SignalMean/SignalSD
SignalTstat
