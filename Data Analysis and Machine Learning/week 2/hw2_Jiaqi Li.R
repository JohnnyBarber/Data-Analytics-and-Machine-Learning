library(data.table)
library(ggplot2)
library(dplyr)
library(haven)
library(gridExtra)
library(lfe)
library(stargazer)

####################################################################
#                           Question 1                             #
####################################################################

#-------------------------------part 1-----------------------------#
data = read_dta("StockRetAcct_insample.dta") %>% as.data.table()
data[,ExRet:=exp(lnAnnRet)-exp(lnRf)]
data = data[complete.cases(data),]
setorder(data, FirmID, year)
#data[,lnInv := shift(lnInv)]
#data = data[-1,]
time = unique(data$year)
p_ret = NULL
for(i in min(time):max(time)){
  temp = data[year == i,]
  temp_ret = lm(ExRet~lnInv, data = temp) %>% coef()
  p_ret = rbind(p_ret,temp_ret[2])
}
p_stat = list(mean_ret = mean(p_ret), std_ret = sd(p_ret),
              SR = mean(p_ret)/sd(p_ret),
              t_stat = sqrt(1+max(time)-min(time))*mean(p_ret)/sd(p_ret))

#-------------------------------part 3-----------------------------#
p_ret_rmNoise = NULL
for(i in min(time):max(time)){
  temp = data[year == i,]
  temp_ret = lm(ExRet~lnInv+as.factor(ff_ind), data = temp) %>% coef()
  p_ret_rmNoise = rbind(p_ret_rmNoise,temp_ret[2])
}
p_statp_rmNoise = list(mean_ret = mean(p_ret_rmNoise), std_ret = sd(p_ret_rmNoise),
              SR = mean(p_ret_rmNoise)/sd(p_ret_rmNoise),
              t_stat = sqrt(1+max(time)-min(time))*mean(p_ret_rmNoise)/sd(p_ret_rmNoise))

#-------------------------------part 4-----------------------------#
timeline = c(min(time):max(time),2015)
cum_p_ret = 0; cum_p_ret_rmNoise = 0
for(j in 1:(length(timeline)-1)){
  cum_p_ret = rbind(cum_p_ret,cum_p_ret[j]+log(1+p_ret[j]))
  cum_p_ret_rmNoise = rbind(cum_p_ret_rmNoise,cum_p_ret_rmNoise[j]+log(1+p_ret_rmNoise[j]))
}

qplot(timeline,exp(cum_p_ret),geom = "line",color = I("blue"),
      main = "New Value (Red) vs. Old Value (Blue)",
      xlab = "year", ylab = "cumulative return")+
  geom_line(aes(y=exp(cum_p_ret_rmNoise)),color=I("red"))+
  scale_colour_manual(values=c("blue","red")) + theme_bw()


####################################################################
#                           Question 2                             #
####################################################################

#-------------------------------part 1-----------------------------#
data[,lead_rv := shift(rv, type = "lead"), by = FirmID]
data[,ff_ind := as.factor(ff_ind)]

rv_panel_1without = felm(lead_rv ~ rv, data = data)
stargazer(rv_panel_1without, type = 'text', report = 'vc*t')

rv_panel_1with = felm(lead_rv ~ rv | 0 | 0 | ff_ind+year,
                      data = data)
stargazer(rv_panel_1without,rv_panel_1with,
          type = 'text', report = 'vc*t')

rv_panel_2with = felm(lead_rv ~ rv | year | 0 | ff_ind+year,
                      data = data)
stargazer(rv_panel_1with,rv_panel_2with,
          type = 'text', report = 'vc*t')

rv_panel_2without = felm(lead_rv ~ rv | year | 0 | 0,
                      data = data)
stargazer(rv_panel_2with, rv_panel_2without,
          type = 'text', report = 'vc*t')

#-------------------------------part 2-----------------------------#
data[,lead_5_rv := shift(rv, 5, type = "lead"), by = FirmID]
data[,ff_ind := as.factor(ff_ind)]

rv_panel_1without = felm(lead_5_rv ~ rv, data = data)
stargazer(rv_panel_1without, type = 'text', report = 'vc*t')

rv_panel_1with = felm(lead_5_rv ~ rv | 0 | 0 | ff_ind+year,
                      data = data)
stargazer(rv_panel_1without,rv_panel_1with,
          type = 'text', report = 'vc*t')

rv_panel_2with = felm(lead_5_rv ~ rv | year | 0 | ff_ind+year,
                      data = data)
stargazer(rv_panel_1with,rv_panel_2with,
          type = 'text', report = 'vc*t')

rv_panel_2without = felm(lead_5_rv ~ rv | year | 0 | 0,
                         data = data)
stargazer(rv_panel_2with, rv_panel_2without,
          type = 'text', report = 'vc*t')
