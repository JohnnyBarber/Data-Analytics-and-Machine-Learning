library(readxl)
library(ggplot2)
library(dplyr)
library(data.table)
library(haven)
library(glmnet)

setwd("C:/Users/Jiaqi Li/Desktop/class materials/quarter 3/Data Analysis and Machine Learning/week 4")

sample = read_dta("StockRetAcct_insample.dta") %>% as.data.table() %>% na.omit()
sample[,`:=` (ex_ret = exp(lnAnnRet)-exp(lnRf),lnIssue2 = lnIssue^2, 
              lnProf2 = lnProf^2,lnInv2 = lnInv^2,lnME2 = lnME^2,
              lnIssue_ME = lnIssue*lnME,lnProf_ME = lnProf*lnME,
              lnInv_ME = lnInv*lnME)]

####################################################################
#                               (i)                                #
####################################################################
final = sample[,`:=` (demean_lnIssue = lnIssue - mean(lnIssue,na.rm = T),
                      demean_lnIssue2 = lnIssue2 - mean(lnIssue2,na.rm = T),
                      demean_lnIssue_ME = lnIssue_ME - mean(lnIssue_ME,na.rm = T),
                      demean_lnProf = lnProf - mean(lnProf,na.rm = T),
                      demean_lnProf2 = lnProf2 - mean(lnProf2,na.rm = T),
                      demean_lnProf_ME = lnProf_ME - mean(lnProf_ME,na.rm = T),
                      demean_lnInv = lnInv - mean(lnInv,na.rm = T),
                      demean_lnInv2 = lnInv2 - mean(lnInv2,na.rm = T),
                      demean_lnInv_ME = lnInv_ME - mean(lnInv_ME,na.rm = T),
                      demean_lnME = lnME - mean(lnME,na.rm = T),
                      demean_lnME2 = lnME2 - mean(lnME2,na.rm = T),
                      intercept = 1),by = year]

#final[,mkt_ret := mean(ex_ret), by = year]

final[,`:=` (ret_demean_LnIssue = sum(demean_lnIssue*ex_ret,na.rm=T),
             ret_demean_lnIssue2 = sum(demean_lnIssue2*ex_ret,na.rm=T),
             ret_demean_lnIssue_ME = sum(demean_lnIssue_ME*ex_ret,na.rm=T),
             ret_demean_lnProf = sum(demean_lnProf*ex_ret,na.rm=T),
             ret_demean_lnProf2 = sum(demean_lnProf2*ex_ret,na.rm=T),
             ret_demean_lnProf_ME = sum(demean_lnProf_ME*ex_ret,na.rm=T),
             ret_demean_lnInv = sum(demean_lnInv*ex_ret,na.rm=T),
             ret_demean_lnInv2 = sum(demean_lnInv2*ex_ret,na.rm=T),
             ret_demean_lnInv_ME = sum(demean_lnInv_ME*ex_ret,na.rm=T),
             ret_demean_lnME = sum(demean_lnME*ex_ret,na.rm=T),
             ret_demean_lnME2 = sum(demean_lnME2*ex_ret,na.rm=T),
             ret_all = sum(intercept*ex_ret,na.rm=T)),by = year]

# final[,`:=` (ret_demean_LnIssue = coef(lm(ex_ret~demean_lnIssue))[2],
#              ret_demean_lnIssue2 = coef(lm(ex_ret~demean_lnIssue2))[2],
#              ret_demean_lnIssue_ME = coef(lm(ex_ret~demean_lnIssue_ME))[2],
#              ret_demean_lnProf = coef(lm(ex_ret~demean_lnProf))[2],
#              ret_demean_lnProf2 = coef(lm(ex_ret~demean_lnProf2))[2],
#              ret_demean_lnProf_ME = coef(lm(ex_ret~demean_lnProf_ME))[2],
#              ret_demean_lnInv = coef(lm(ex_ret~demean_lnInv))[2],
#              ret_demean_lnInv2 = coef(lm(ex_ret~demean_lnInv2))[2],
#              ret_demean_lnInv_ME = coef(lm(ex_ret~demean_lnInv_ME))[2],
#              ret_demean_lnME = coef(lm(ex_ret~demean_lnME))[2],
#              ret_demean_lnME2 = coef(lm(ex_ret~demean_lnME2))[2]),by = year]

M = final[,list(ret_demean_LnIssue,ret_demean_lnIssue2,ret_demean_lnIssue_ME,
                ret_demean_lnProf,ret_demean_lnProf2,ret_demean_lnProf_ME,
                ret_demean_lnInv,ret_demean_lnInv2,ret_demean_lnInv_ME,
                ret_demean_lnME,ret_demean_lnME2,ret_all)]
sample_mean = apply(M,2,function(x){mean(x,na.rm = T)})
vcov = var(M)
SR = sample_mean/diag(sqrt(vcov))

####################################################################
#                               (ii)                              #
####################################################################
K_fold = list(c(1980:1999),c(1980:1984,1990:2004),c(1980:1989,1995:2004),
              c(1980:1994,2000:2004),c(1985:2004))
out_sample = list(c(2000:2004),c(1985:1999),c(1990:1994),c(1995:1999),c(1980:1984))
M = final[,list(ret_demean_LnIssue,ret_demean_lnIssue2,ret_demean_lnIssue_ME,
                ret_demean_lnProf,ret_demean_lnProf2,ret_demean_lnProf_ME,
                ret_demean_lnInv,ret_demean_lnInv2,ret_demean_lnInv_ME,
                ret_demean_lnME,ret_demean_lnME2,ret_all,year)]
alpha = 0.5
setkey(M,year)
test = M[.(1980:2004)]
MSE = c()
lambda = c()
for(i in 1:5){
  temp = as.data.frame(test[.(unlist(K_fold[i]))])[,-13]
  temp_mean = apply(temp,2,function(x){mean(x,na.rm = T)})
  temp_M = cov(temp)
  lambda_test = glmnet(temp_M,temp_mean,family = "gaussian",alpha = alpha,standardize = T)$lambda
  lambda = c(lambda,lambda_test)
}
lambda = seq(1.1,10,0.1)
for(i in 1:5){
  temp = as.data.frame(test[.(unlist(K_fold[i]))])[,-13]
  temp_mean = apply(temp,2,function(x){mean(x,na.rm = T)})
  temp_M = var(temp)
  Lamb = glmnet(temp_M,temp_mean,family = "gaussian",alpha = alpha,standardize = T,lambda = lambda)
  out_temp = as.data.frame(test[.(unlist(out_sample[i]))])[,-13]
  out_vcov = var(out_temp,na.rm = T)
  try = predict(Lamb,out_vcov,s=lambda,type = "response")
  #out_return = (out_vcov)%*%try[-1,]
  MSE = rbind(MSE,colMeans((temp_mean-try)^2))
}
compare = colMeans(MSE)
Lamb_pos = which(compare == min(compare))
lambda = Lamb$lambda[Lamb_pos]
test_in = as.data.frame(test[,-13])
b = glmnet(var(test_in),colMeans(test_in),family = "gaussian",alpha = alpha,standardize = T,lambda = lambda)$beta


####################################################################
#                              (iii)                               #
####################################################################
test = M[.(2005:2014)][,year := NULL]
test_ret = as.vector(b)%*%t(as.matrix(test))
MeanRet = mean(test_ret)
StdRet = sd(test_ret)
SRRet = MeanRet/StdRet


####################################################################
#                              (iiii)                              #
####################################################################
setkey(sample,year)
sample[,MktRet := MEwt*ex_ret]
iiii = c(sample[.(2005:2014)]$MktRet,test_ret/length(test_ret))
iiii = cbind(iiii,c(rep("Market",length(sample[.(2005:2014)]$MktRet)),
                    rep("Predict",length(test_ret))))
iiii = as.data.table(iiii)
iiii[,Ret := cumsum(iiii),by = V2]
time = c(1:(length(sample[.(2005:2014)]$year))*2)
iiii = cbind(iiii,time)
colnames(iiii) = c("Return","Porfolio Type","Cumulative Return","Time")
qplot(Time,`Cumulative Return`,geom ="line", data = iiii,
      color = `Porfolio Type`,main = "Machine Learning based Trading") +
  theme_bw()
