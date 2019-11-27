library(ggplot2)
library(data.table)
library(dplyr)
library(haven)

####################################################################
#                              (a)                                 #
####################################################################
#------------------------------(i)---------------------------------#
data = read_dta("LendingClub_LoanStats3a_v12.dta") %>% as.data.table()
data = data[loan_status == "Fully Paid" | loan_status == "Charged Off",]
data[,Default := ifelse(loan_status == "Charged Off",1,0)]

#-----------------------------(ii)---------------------------------#
Def_rate = mean(data$Default)

####################################################################
#                              (b)                                 #
####################################################################
#------------------------------(i)---------------------------------#
reg = glm(Default~grade,family = "binomial",data=data)
summary(reg)

#-----------------------------(ii)---------------------------------#
library(car)
Anova(reg)

#-----------------------------(iii)--------------------------------#
phat_temp = jitter(predict(reg,type="response")) 
deciles = cut(phat_temp,breaks = quantile(phat_temp,probs=c(seq(from=0,to=1,by=0.1))),
              include.lowest = TRUE)
deciles = as.numeric(deciles)
df = data.frame(deciles=deciles,phat=phat,default=data$Default)
lift = aggregate(df,by=list(deciles),FUN="mean",data=df)
lift = lift[,c(2,4)]
lift[,3] = lift[,2]/mean(data$Default)
names(lift) = c("decile","Mean Response","Lift Factor")
lift

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels),
             FPR=cumsum(!labels)/sum(!labels),
             labels)
}
phat = predict(reg,type="response")
glm_simple_roc <- simple_roc(data$Default=="1", phat)
TPR <- glm_simple_roc$TPR
FPR <- glm_simple_roc$FPR

qplot(FPR,TPR,xlab="FPR",ylab="TPR",col=I("blue"),
      main="ROC Curve for Logistic Regression Default Model",
      size=I(0.75)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size=I(1.0)) + 
  theme_bw()

#-----------------------------(iiii)-------------------------------#
P = sum(data$Default)
N = -sum(data$Default-1)
TN = N*(1-FPR)
FN = P*(1-TPR)
temp1 = cbind(profit = TN*1-FN*10,phat = sort(phat,decreasing = T)) %>% 
  as.data.frame()
temp2 = cbind(TPR,FPR) %>% as.data.frame()
temp = cbind(temp1,temp2)
cutoff = temp[which(temp$profit == max(temp$profit)),]
cutoff

qplot(FPR,TPR,xlab="FPR",ylab="TPR",col=I("blue"),
      main="ROC Curve for Logistic Regression Default Model",
      size=I(0.75)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size=I(1.0)) + 
  geom_point(aes(x=cutoff$FPR,y=cutoff$TPR),shape = 16,
             color = I("orange"), size = 3) + 
  theme_bw()

default_prob = cutoff$phat
default_prob

####################################################################
#                              (c)                                 #
####################################################################
#------------------------------(i)---------------------------------#
reg_ci = glm(Default~loan_amnt+annual_inc,family = "binomial",data = data)
phat_ci = jitter(predict(reg_ci,type="response")) 
deciles_ci = cut(phat_ci,breaks = quantile(phat_ci,probs=c(seq(from=0,to=1,by=0.1))),
              include.lowest = TRUE)
deciles_ci = as.numeric(deciles_ci)
df_ci = data.frame(deciles=deciles_ci,phat=phat_ci,default=data$Default)
lift_ci = aggregate(df_ci,by=list(deciles_ci),FUN="mean",data=df_ci)
lift_ci = lift_ci[,c(2,4)]
lift_ci[,3] = lift_ci[,2]/mean(data$Default)
names(lift_ci) = c("decile","Mean Response","Lift Factor")
lift_ci

glm_simple_roc_ci <- simple_roc(data$Default=="1", phat_ci)
TPR_ci <- glm_simple_roc_ci$TPR
FPR_ci <- glm_simple_roc_ci$FPR

qplot(FPR_ci,TPR_ci,xlab="FPR",ylab="TPR",col=I("red"),
      main="i (red) vs grade (blue)",
      size=I(0.75)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size=I(1.0)) + 
  geom_line(aes(y=TPR),color=I("blue"),size=I(0.75),linetype = 2) +
  theme_bw()

#-----------------------------(ii)---------------------------------#
reg_cii = glm(Default~loan_amnt+annual_inc+term+int_rate,family = "binomial",data = data)
phat_cii = jitter(predict(reg_cii,type="response")) 
deciles_cii = cut(phat_cii,breaks = quantile(phat_cii,probs=c(seq(from=0,to=1,by=0.1))),
                 include.lowest = TRUE)
deciles_cii = as.numeric(deciles_cii)
df_cii = data.frame(deciles=deciles_cii,phat=phat_cii,default=data$Default)
lift_cii = aggregate(df_cii,by=list(deciles_cii),FUN="mean",data=df_cii)
lift_cii = lift_cii[,c(2,4)]
lift_cii[,3] = lift_cii[,2]/mean(data$Default)
names(lift_cii) = c("decile","Mean Response","Lift Factor")
lift_cii

glm_simple_roc_cii <- simple_roc(data$Default=="1", phat_cii)
TPR_cii <- glm_simple_roc_cii$TPR
FPR_cii <- glm_simple_roc_cii$FPR

qplot(FPR_cii,TPR_cii,xlab="FPR",ylab="TPR",col=I("green"),
      main="ii (green) vs i (red) vs grade (blue)",
      size=I(0.75)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size=I(1.0)) + 
  geom_line(aes(y=TPR),color=I("blue"),size=I(0.75),linetype = 2) +
  geom_line(aes(y=TPR_ci),color=I("red"),size=I(0.75),linetype = 2) +
  theme_bw()

#----------------------------(iii)---------------------------------#
data[,int_rate_2 := int_rate^2]
reg_ciii = glm(Default~loan_amnt+annual_inc+term+int_rate+int_rate_2,family = "binomial",data = data)
summary(reg_ciii)

phat_ciii = jitter(predict(reg_ciii,type="response")) 
glm_simple_roc_ciii <- simple_roc(data$Default=="1", phat_ciii)
TPR_ciii <- glm_simple_roc_ciii$TPR
FPR_ciii <- glm_simple_roc_ciii$FPR

qplot(FPR_ciii,TPR_ciii,xlab="FPR",ylab="TPR",col=I("pink"),
      main="iii(pink) vs ii (green) vs i (red) vs grade (blue)",
      size=I(0.75)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size=I(1.0)) + 
  geom_line(aes(y=TPR),color=I("blue"),size=I(0.75),linetype = 2) +
  geom_line(aes(y=TPR_ci),color=I("red"),size=I(0.75),linetype = 2) +
  geom_line(aes(y=TPR_cii),color=I("green"),size=I(0.75),linetype = 2) +
  theme_bw()