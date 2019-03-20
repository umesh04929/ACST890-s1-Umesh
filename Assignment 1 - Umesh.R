#Q-1.
bondprice <- function(coup=0,f,n=0,y)       #coup = coupon value, f = face value, n = no. of coupons
                                            #y = interest rate vector in decimal format (half yearly)
{
          t = seq(0.5,length(y)/2,0.5)     #t is being calculated as number of half year periods  
          price = 0
          couponpv = sum(coup*(exp(-y[1:length(y)]*t)))
          fvpv = f*exp(-y[length(y)]*(length(y))/2)
          price = couponpv + fvpv
          return(price)
}


#Q-3.
#(a)
dataset <- read.csv(file.choose(),header=T)

#(b)
dataset <- na.omit(dataset)
                 
#(c)
attach(dataset)
plot(time,gdp,xlab="Time",ylab="GDP (%)",main="Singapore GDP growth")

#(d)
datasetperiod1 = subset(dataset,period == 1)
datasetperiod2 = subset(dataset,period == 2)
datasetperiod3 = subset(dataset,period == 3)
meanperiod1 = mean(datasetperiod1$gdp)
meanperiod2 = mean(datasetperiod2$gdp)
meanperiod3 = mean(datasetperiod3$gdp)
sdperiod1 = sd(datasetperiod1$gdp)
sdperiod2 = sd(datasetperiod2$gdp)
sdperiod3 = sd(datasetperiod3$gdp)
stat.table = data.frame(cbind(c(1,2,3),c(meanperiod1,meanperiod2,meanperiod3),
c(sdperiod1,sdperiod2,sdperiod3)))
colnames(stat.table) = c("Period","Mean","SD")

#(e)
pairs(~gdp+exp+epg+hpr+gdpus+oil+crd+bci,data=dataset)

#(f)
simplereg <- lm(gdp~exp, data=dataset)
summary(simplereg)

#(g)
multiplereg <- lm(gdp~exp+epg+hpr+oil+gdpus+crd,data=dataset)
summary(multiplereg)
                 

#(h)
q=quantile(dataset$gdp,0.05)
state = rep("normal",nrow(dataset))
state[dataset$gdp<q] = "crisis"
state = as.factor(state)
dataset = data.frame(dataset,state)

trdataset = subset(dataset,period<3)
logisticreg = glm(state~bci,data=trdataset,family = binomial)
summary(logisticreg)
