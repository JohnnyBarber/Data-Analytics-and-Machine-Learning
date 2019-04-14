library(ggplot2)
library(car)
data = mtcars
name = colnames(data)
qplot(wt,mpg,data=data,main="test",col=cyl)+geom_smooth(method="lm",col="red")+theme_bw()
qplot(wt,mpg,data=data,main="test",col=I("blue"),facets = cyl~.)+geom_smooth(method="lm",col=I("red"))+theme_bw()

library(DataAnalytics)
data(mvehicles)
cars = mvehicles[mvehicles$bodytype != "Truck",]
qplot(luxury,log(emv),data=cars,col = bodytype) + theme_bw() + geom_smooth(method = "lm",col="red")
qplot(luxury,log(emv),data=cars,col = bodytype) + theme_bw() + geom_smooth(col="red")


library(data.table)
test = as.data.table(cars)
head(test)
setkey(test,make,year,model)

test[.("BMW",2012,"X6")]

name = unique(test[,make])
for(i in 1:length(name)){
  assign(name[i],test[name[i]])
}

test.mean = test[,mean(lnemv),by=make]
test.mean2 = rep(0,length(name))
for(i in 1:length(name)){
  test.mean2[i] = mean(as.data.frame(get(name[i])[,"lnemv"]))
}
