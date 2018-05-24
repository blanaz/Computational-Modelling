setwd("~/Denmark/Study/Semester IV/Social Dynamics/Exam/Output")

library(pacman)
pacman::p_load(readr,groupdata2,ggplot2,tidyverse,lmerTest,lme4,MuMIn,data.table,jpeg,grid,tidyr,rethinking,gridExtra,readxl,effsize,metafor,pastecs)
library(metafor)
library(lme4)
library(brms)
library(tidyverse)
d = read.csv("100 runs, setting 1, without intervention.csv", stringsAsFactors = F)
d1 = read.csv ("100 runs, setting 1, with intervention.csv", stringsAsFactors = F)
plot(d$count.links.with..weight...0.05.)
plot(d$sum..link.length..of.links.with..weight...0.05.)
plot(d$sum..weight..of.links.with..weight...0.05.)
plot(d$X.sum..weight..of.links.with..weight...0.05....count.links.with..weight...0.05..)

plot(d$X1.3)
plot(d1$X1.3)



ggplot(d,aes(ticks,X2.3))+geom_bar(stat="identity",position = "dodge")+
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())



Mmm <- rethinking::map(
  alist(
    X1.3 ~ dnorm(mu, sigma),
    mu <- a + b*intervention, 
    a ~ dnorm(0,1),
    b ~ dnorm(0.76,0.27),
    sigma ~ dcauchy(0,2) #2 and 3 are big 
  ),
  data = data)
precis(Mmm)

#creating a new data frame with the average of 100 non drug simulation 
mean(d1$X1.3)
uhlalala1 = c(mean(d$X1.3),
mean(d$X2.3),
mean(d$X3.3),
mean(d$X4.3),
mean(d$X5.3),
mean(d$X6.3),
mean(d$X7.3),
mean(d$X8.3),
mean(d$X9.3),
mean(d$X10.3),
mean(d$X11.3),
mean(d$X12.3),
mean(d$X13.3),
mean(d$X14.3),
mean(d$X15.3),
mean(d$X16.3),
mean(d$X17.3),
mean(d$X18.3),
mean(d$X19.3),
mean(d$X20.3),
mean(d$X21.3),
mean(d$X22.3),
mean(d$X23.3),
mean(d$X24.3),
mean(d$X25.3),
mean(d$X26.3),
mean(d$X27.3),
mean(d$X28.3),
mean(d$X29.3),
mean(d$X30.3),
mean(d$X31.3),
mean(d$X32.3),
mean(d$X33.3),
mean(d$X34.3),
mean(d$X35.3),
mean(d$X36.3),
mean(d$X37.3),
mean(d$X38.3),
mean(d$X39.3),
mean(d$X40.3),
mean(d$X41.3),
mean(d$X42.3),
mean(d$X43.3),
mean(d$X44.3),
mean(d$X45.3),
mean(d$X46.3),
mean(d$X47.3),
mean(d$X48.3),
mean(d$X49.3),
mean(d$X50.3),
mean(d$X51.3),
mean(d$X52.3),
mean(d$X53.3),
mean(d$X54.3),
mean(d$X55.3),
mean(d$X56.3),
mean(d$X57.3),
mean(d$X58.3),
mean(d$X59.3),
mean(d$X60.3),
mean(d$X61.3),
mean(d$X62.3),
mean(d$X63.3),
mean(d$X64.3),
mean(d$X65.3),
mean(d$X66.3),
mean(d$X67.3),
mean(d$X68.3),
mean(d$X69.3),
mean(d$X70.3),
mean(d$X71.3),
mean(d$X72.3),
mean(d$X73.3),
mean(d$X74.3),
mean(d$X75.3),
mean(d$X76.3),
mean(d$X77.3),
mean(d$X78.3),
mean(d$X79.3),
mean(d$X80.3),
mean(d$X81.3),
mean(d$X82.3),
mean(d$X83.3),
mean(d$X84.3),
mean(d$X85.3),
mean(d$X86.3),
mean(d$X87.3),
mean(d$X88.3),
mean(d$X89.3),
mean(d$X90.3),
mean(d$X91.3),
mean(d$X92.3),
mean(d$X93.3),
mean(d$X94.3),
mean(d$X95.3),
mean(d$X96.3),
mean(d$X97.3),
mean(d$X98.3),
mean(d$X99.3),
mean(d$X100.3)
)

frame = data.frame(uhlalala,uhlalala1)
plot(frame$uhlalala1)
plot(density(frame$uhlalala))
plot(density(frame$uhlalala1))

write.csv(frame, file = "avg link weight.csv")
write.csv(frame2, file = "coef.csv")
write.csv(frame3, file = "avg link weight 5000.csv")
write.csv(frame4, file = "coef 5000.csv")
write.csv(frame5, file = "avg link weight 1000.csv")
write.csv(frame6, file = "coef 1000.csv")

#coeff
mean(d1$X1.4)
coef1 = c(mean(d$X1.4),
              mean(d$X2.4),
              mean(d$X3.4),
              mean(d$X4.4),
              mean(d$X5.4),
              mean(d$X6.4),
              mean(d$X7.4),
              mean(d$X8.4),
              mean(d$X9.4),
              mean(d$X10.4),
              mean(d$X11.4),
              mean(d$X12.4),
              mean(d$X13.4),
              mean(d$X14.4),
              mean(d$X15.4),
              mean(d$X16.4),
              mean(d$X17.4),
              mean(d$X18.4),
              mean(d$X19.4),
              mean(d$X20.4),
              mean(d$X21.4),
              mean(d$X22.4),
              mean(d$X23.4),
              mean(d$X24.4),
              mean(d$X25.4),
              mean(d$X26.4),
              mean(d$X27.4),
              mean(d$X28.4),
              mean(d$X29.4),
              mean(d$X30.4),
              mean(d$X31.4),
              mean(d$X32.4),
              mean(d$X33.4),
              mean(d$X34.4),
              mean(d$X35.4),
              mean(d$X36.4),
              mean(d$X37.4),
              mean(d$X38.4),
              mean(d$X39.4),
              mean(d$X40.4),
              mean(d$X41.4),
              mean(d$X42.4),
              mean(d$X43.4),
              mean(d$X44.4),
              mean(d$X45.4),
              mean(d$X46.4),
              mean(d$X47.4),
              mean(d$X48.4),
              mean(d$X49.4),
              mean(d$X50.4),
              mean(d$X51.4),
              mean(d$X52.4),
              mean(d$X53.4),
              mean(d$X54.4),
              mean(d$X55.4),
              mean(d$X56.4),
              mean(d$X57.4),
              mean(d$X58.4),
              mean(d$X59.4),
              mean(d$X60.4),
              mean(d$X61.4),
              mean(d$X62.4),
              mean(d$X63.4),
              mean(d$X64.4),
              mean(d$X65.4),
              mean(d$X66.4),
              mean(d$X67.4),
              mean(d$X68.4),
              mean(d$X69.4),
              mean(d$X70.4),
              mean(d$X71.4),
              mean(d$X72.4),
              mean(d$X73.4),
              mean(d$X74.4),
              mean(d$X75.4),
              mean(d$X76.4),
              mean(d$X77.4),
              mean(d$X78.4),
              mean(d$X79.4),
              mean(d$X80.4),
              mean(d$X81.4),
              mean(d$X82.4),
              mean(d$X83.4),
              mean(d$X84.4),
              mean(d$X85.4),
              mean(d$X86.4),
              mean(d$X87.4),
              mean(d$X88.4),
              mean(d$X89.4),
              mean(d$X90.4),
              mean(d$X91.4),
              mean(d$X92.4),
              mean(d$X93.4),
              mean(d$X94.4),
              mean(d$X95.4),
              mean(d$X96.4),
              mean(d$X97.4),
              mean(d$X98.4),
              mean(d$X99.4),
              mean(d$X100.4)
)

frame2 = data.frame(coef,coef1)
plot(density(frame2$coef))
plot(density(frame2$coef1))
plot(frame2$coef)
plot(frame2$coef1)

d = read.csv("without intervention 2 last 1000.csv")
d1 = read.csv("100 runs, setting 2, with intervention last 1000.csv")

#do without then with with count links (above threshold)
avgLinkWeight = c(mean(d$X1.3),
         mean(d$X2.3),
         mean(d$X3.3),
         mean(d$X4.3),
         mean(d$X5.3),
         mean(d$X6.3),
         mean(d$X7.3),
         mean(d$X8.3),
         mean(d$X9.3),
         mean(d$X10.3),
         mean(d$X11.3),
         mean(d$X12.3),
         mean(d$X13.3),
         mean(d$X14.3),
         mean(d$X15.3),
         mean(d$X16.3),
         mean(d$X17.3),
         mean(d$X18.3),
         mean(d$X19.3),
         mean(d$X20.3),
         mean(d$X21.3),
         mean(d$X22.3),
         mean(d$X23.3),
         mean(d$X24.3),
         mean(d$X25.3),
         mean(d$X26.3),
         mean(d$X27.3),
         mean(d$X28.3),
         mean(d$X29.3),
         mean(d$X30.3),
         mean(d$X31.3),
         mean(d$X32.3),
         mean(d$X33.3),
         mean(d$X34.3),
         mean(d$X35.3),
         mean(d$X36.3),
         mean(d$X37.3),
         mean(d$X38.3),
         mean(d$X39.3),
         mean(d$X40.3),
         mean(d$X41.3),
         mean(d$X42.3),
         mean(d$X43.3),
         mean(d$X44.3),
         mean(d$X45.3),
         mean(d$X46.3),
         mean(d$X47.3),
         mean(d$X48.3),
         mean(d$X49.3),
         mean(d$X50.3),
         mean(d$X51.3),
         mean(d$X52.3),
         mean(d$X53.3),
         mean(d$X54.3),
         mean(d$X55.3),
         mean(d$X56.3),
         mean(d$X57.3),
         mean(d$X58.3),
         mean(d$X59.3),
         mean(d$X60.3),
         mean(d$X61.3),
         mean(d$X62.3),
         mean(d$X63.3),
         mean(d$X64.3),
         mean(d$X65.3),
         mean(d$X66.3),
         mean(d$X67.3),
         mean(d$X68.3),
         mean(d$X69.3),
         mean(d$X70.3),
         mean(d$X71.3),
         mean(d$X72.3),
         mean(d$X73.3),
         mean(d$X74.3),
         mean(d$X75.3),
         mean(d$X76.3),
         mean(d$X77.3),
         mean(d$X78.3),
         mean(d$X79.3),
         mean(d$X80.3),
         mean(d$X81.3),
         mean(d$X82.3),
         mean(d$X83.3),
         mean(d$X84.3),
         mean(d$X85.3),
         mean(d$X86.3),
         mean(d$X87.3),
         mean(d$X88.3),
         mean(d$X89.3),
         mean(d$X90.3),
         mean(d$X91.3),
         mean(d$X92.3),
         mean(d$X93.3),
         mean(d$X94.3),
         mean(d$X95.3),
         mean(d$X96.3),
         mean(d$X97.3),
         mean(d$X98.3),
         mean(d$X99.3),
         mean(d$X100.3)
)

#with
avgLinkWeight1 = c(mean(d1$X1.3),
          mean(d1$X2.3),
          mean(d1$X3.3),
          mean(d1$X4.3),
          mean(d1$X5.3),
          mean(d1$X6.3),
          mean(d1$X7.3),
          mean(d1$X8.3),
          mean(d1$X9.3),
          mean(d1$X10.3),
          mean(d1$X11.3),
          mean(d1$X12.3),
          mean(d1$X13.3),
          mean(d1$X14.3),
          mean(d1$X15.3),
          mean(d1$X16.3),
          mean(d1$X17.3),
          mean(d1$X18.3),
          mean(d1$X19.3),
          mean(d1$X20.3),
          mean(d1$X21.3),
          mean(d1$X22.3),
          mean(d1$X23.3),
          mean(d1$X24.3),
          mean(d1$X25.3),
          mean(d1$X26.3),
          mean(d1$X27.3),
          mean(d1$X28.3),
          mean(d1$X29.3),
          mean(d1$X30.3),
          mean(d1$X31.3),
          mean(d1$X32.3),
          mean(d1$X33.3),
          mean(d1$X34.3),
          mean(d1$X35.3),
          mean(d1$X36.3),
          mean(d1$X37.3),
          mean(d1$X38.3),
          mean(d1$X39.3),
          mean(d1$X40.3),
          mean(d1$X41.3),
          mean(d1$X42.3),
          mean(d1$X43.3),
          mean(d1$X44.3),
          mean(d1$X45.3),
          mean(d1$X46.3),
          mean(d1$X47.3),
          mean(d1$X48.3),
          mean(d1$X49.3),
          mean(d1$X50.3),
          mean(d1$X51.3),
          mean(d1$X52.3),
          mean(d1$X53.3),
          mean(d1$X54.3),
          mean(d1$X55.3),
          mean(d1$X56.3),
          mean(d1$X57.3),
          mean(d1$X58.3),
          mean(d1$X59.3),
          mean(d1$X60.3),
          mean(d1$X61.3),
          mean(d1$X62.3),
          mean(d1$X63.3),
          mean(d1$X64.3),
          mean(d1$X65.3),
          mean(d1$X66.3),
          mean(d1$X67.3),
          mean(d1$X68.3),
          mean(d1$X69.3),
          mean(d1$X70.3),
          mean(d1$X71.3),
          mean(d1$X72.3),
          mean(d1$X73.3),
          mean(d1$X74.3),
          mean(d1$X75.3),
          mean(d1$X76.3),
          mean(d1$X77.3),
          mean(d1$X78.3),
          mean(d1$X79.3),
          mean(d1$X80.3),
          mean(d1$X81.3),
          mean(d1$X82.3),
          mean(d1$X83.3),
          mean(d1$X84.3),
          mean(d1$X85.3),
          mean(d1$X86.3),
          mean(d1$X87.3),
          mean(d1$X88.3),
          mean(d1$X89.3),
          mean(d1$X90.3),
          mean(d1$X91.3),
          mean(d1$X92.3),
          mean(d1$X93.3),
          mean(d1$X94.3),
          mean(d1$X95.3),
          mean(d1$X96.3),
          mean(d1$X97.3),
          mean(d1$X98.3),
          mean(d1$X99.3),
          mean(d1$X100.3)
)
frame8 = data.frame(avgLinkWeight,avgLinkWeight1)
write.csv(frame8, "avgLinkWeight last 1000 2.csv")

#do without then with with sum of link lengths (above threshold)
count = c(mean(d$X1.1),
              mean(d$X2.1),
              mean(d$X3.1),
              mean(d$X4.1),
              mean(d$X5.1),
              mean(d$X6.1),
              mean(d$X7.1),
              mean(d$X8.1),
              mean(d$X9.1),
              mean(d$X10.1),
              mean(d$X11.1),
              mean(d$X12.1),
              mean(d$X13.1),
              mean(d$X14.1),
              mean(d$X15.1),
              mean(d$X16.1),
              mean(d$X17.1),
              mean(d$X18.1),
              mean(d$X19.1),
              mean(d$X20.1),
              mean(d$X21.1),
              mean(d$X22.1),
              mean(d$X23.1),
              mean(d$X24.1),
              mean(d$X25.1),
              mean(d$X26.1),
              mean(d$X27.1),
              mean(d$X28.1),
              mean(d$X29.1),
              mean(d$X30.1),
              mean(d$X31.1),
              mean(d$X32.1),
              mean(d$X33.1),
              mean(d$X34.1),
              mean(d$X35.1),
              mean(d$X36.1),
              mean(d$X37.1),
              mean(d$X38.1),
              mean(d$X39.1),
              mean(d$X40.1),
              mean(d$X41.1),
              mean(d$X42.1),
              mean(d$X43.1),
              mean(d$X44.1),
              mean(d$X45.1),
              mean(d$X46.1),
              mean(d$X47.1),
              mean(d$X48.1),
              mean(d$X49.1),
              mean(d$X50.1),
              mean(d$X51.1),
              mean(d$X52.1),
              mean(d$X53.1),
              mean(d$X54.1),
              mean(d$X55.1),
              mean(d$X56.1),
              mean(d$X57.1),
              mean(d$X58.1),
              mean(d$X59.1),
              mean(d$X60.1),
              mean(d$X61.1),
              mean(d$X62.1),
              mean(d$X63.1),
              mean(d$X64.1),
              mean(d$X65.1),
              mean(d$X66.1),
              mean(d$X67.1),
              mean(d$X68.1),
              mean(d$X69.1),
              mean(d$X70.1),
              mean(d$X71.1),
              mean(d$X72.1),
              mean(d$X73.1),
              mean(d$X74.1),
              mean(d$X75.1),
              mean(d$X76.1),
              mean(d$X77.1),
              mean(d$X78.1),
              mean(d$X79.1),
              mean(d$X80.1),
              mean(d$X81.1),
              mean(d$X82.1),
              mean(d$X83.1),
              mean(d$X84.1),
              mean(d$X85.1),
              mean(d$X86.1),
              mean(d$X87.1),
              mean(d$X88.1),
              mean(d$X89.1),
              mean(d$X90.1),
              mean(d$X91.1),
              mean(d$X92.1),
              mean(d$X93.1),
              mean(d$X94.1),
              mean(d$X95.1),
              mean(d$X96.1),
              mean(d$X97.1),
              mean(d$X98.1),
              mean(d$X99.1),
              mean(d$X100.1)
)

#with
count1 = c(mean(d1$X1.1),
               mean(d1$X2.1),
               mean(d1$X3.1),
               mean(d1$X4.1),
               mean(d1$X5.1),
               mean(d1$X6.1),
               mean(d1$X7.1),
               mean(d1$X8.1),
               mean(d1$X9.1),
               mean(d1$X10.1),
               mean(d1$X11.1),
               mean(d1$X12.1),
               mean(d1$X13.1),
               mean(d1$X14.1),
               mean(d1$X15.1),
               mean(d1$X16.1),
               mean(d1$X17.1),
               mean(d1$X18.1),
               mean(d1$X19.1),
               mean(d1$X20.1),
               mean(d1$X21.1),
               mean(d1$X22.1),
               mean(d1$X23.1),
               mean(d1$X24.1),
               mean(d1$X25.1),
               mean(d1$X26.1),
               mean(d1$X27.1),
               mean(d1$X28.1),
               mean(d1$X29.1),
               mean(d1$X30.1),
               mean(d1$X31.1),
               mean(d1$X32.1),
               mean(d1$X33.1),
               mean(d1$X34.1),
               mean(d1$X35.1),
               mean(d1$X36.1),
               mean(d1$X37.1),
               mean(d1$X38.1),
               mean(d1$X39.1),
               mean(d1$X40.1),
               mean(d1$X41.1),
               mean(d1$X42.1),
               mean(d1$X43.1),
               mean(d1$X44.1),
               mean(d1$X45.1),
               mean(d1$X46.1),
               mean(d1$X47.1),
               mean(d1$X48.1),
               mean(d1$X49.1),
               mean(d1$X50.1),
               mean(d1$X51.1),
               mean(d1$X52.1),
               mean(d1$X53.1),
               mean(d1$X54.1),
               mean(d1$X55.1),
               mean(d1$X56.1),
               mean(d1$X57.1),
               mean(d1$X58.1),
               mean(d1$X59.1),
               mean(d1$X60.1),
               mean(d1$X61.1),
               mean(d1$X62.1),
               mean(d1$X63.1),
               mean(d1$X64.1),
               mean(d1$X65.1),
               mean(d1$X66.1),
               mean(d1$X67.1),
               mean(d1$X68.1),
               mean(d1$X69.1),
               mean(d1$X70.1),
               mean(d1$X71.1),
               mean(d1$X72.1),
               mean(d1$X73.1),
               mean(d1$X74.1),
               mean(d1$X75.1),
               mean(d1$X76.1),
               mean(d1$X77.1),
               mean(d1$X78.1),
               mean(d1$X79.1),
               mean(d1$X80.1),
               mean(d1$X81.1),
               mean(d1$X82.1),
               mean(d1$X83.1),
               mean(d1$X84.1),
               mean(d1$X85.1),
               mean(d1$X86.1),
               mean(d1$X87.1),
               mean(d1$X88.1),
               mean(d1$X89.1),
               mean(d1$X90.1),
               mean(d1$X91.1),
               mean(d1$X92.1),
               mean(d1$X93.1),
               mean(d1$X94.1),
               mean(d1$X95.1),
               mean(d1$X96.1),
               mean(d1$X97.1),
               mean(d1$X98.1),
               mean(d1$X99.1),
               mean(d1$X100.1)
)

frame9 = data.frame(count,count1)
write.csv(frame9, "count 1000 2.csv")
#after reshaping in excel, reading all files in, creating a new dataframe of all with drugs variable
count = read.csv("./dataframe 1000 2/count 1000 2.csv")
lengthsum = read.csv("./dataframe 1000 2/lengthsum 1000 2.csv")
coef = read.csv("./dataframe 1000 2/coef last 1000 2.csv")
avgLinkWeight = read.csv("./dataframe 1000 2/avgLinkWeight last 1000 2.csv")
oneK = merge(count,lengthsum, all = T)
oneK = merge(oneK,coef, all = T)
oneK = merge(oneK, avgLinkWeight, all = T)
write.csv(oneK, "oneK.csv")


#after reshaping in excel, reading all files in, creating a new dataframe of all with drugs variable
count = read.csv("./dataframe 5000/count last 5000.csv")
lengthsum = read.csv("./dataframe 5000/lengthsum 5000.csv")
coef = read.csv("./dataframe 5000/coef 5000.csv")
avgLinkWeight = read.csv("./dataframe 5000/avg link weight 5000.csv")
fiveK = merge(count,lengthsum, all = T)
fiveK = merge(fiveK,coef, all = T)
fiveK = merge(fiveK, avgLinkWeight, all = T)
write.csv(fiveK, "fiveK.csv")


#after cleaning, read the analyseable dataframes in

oneK = read.csv("./dataframe 1000/oneKs.csv")
fiveK = read.csv("./dataframe 5000/fiveKs.csv")
fiveK$Drugs = as.factor(fiveK$Drugs)
oneK2$Drugs = as.factor(oneK2$Drugs)
oneK$Drugs = as.integer(oneK$Drugs)
ggplot(oneK,aes(simNum,coef.s, fill = Drugs)) + geom_point() + geom_smooth(method = "auto" )

#+geom_bar(stat="identity",position = "dodge")+facet_wrap(~Drugs)

#__________________________________________________________________________
# first constructing the oneK model
#checking mean and sd of variable
round(stat.desc(oneK$avgLinkWeight),3)
round(stat.desc(oneK$coef),3)
#scaling?

oneK$avgLinkWeight.s = scale(oneK$avgLinkWeight)
oneK$count.s = scale(oneK$count)
oneK$lengthsum.s = scale(oneK$lengthsum)
oneK$coef.s = scale(oneK$coef)

fiveK$avgLinkWeight.s = scale(fiveK$avgLinkWeight)
fiveK$count.s = scale(fiveK$count)
fiveK$lengthsum.s = scale(fiveK$lengthsum)
fiveK$coef.s = scale(fiveK$coef)
#the one that works
 oneK$Drugs = as.integer(oneK$Drugs)
 drugmodel <- rethinking::map(
     alist(
         avgLinkWeight.s ~ dnorm(mu, sigma),
         mu <- a + b*Drugs, 
         a ~ dnorm(0,1),
         b ~ dnorm(0,1),
         sigma ~ dunif(0,2)
       ),
     data = oneK)
 #______________________________________-

drugmodel <- rethinking::map(
  alist(
    Drugs ~ dnorm(mu, sigma),
    mu <- a + b1*coef.s + b2*avgLinkWeight.s, 
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    sigma ~ dunif(0,1)
     ),
  data = oneK)



precis(drugmodel,corr=T)
vcov(drugmodel)
plot(diag(vcov(drugmodel)))

plot(precis(drugmodel),main = "Predicting avgLinkWeight from Drugs(b)")


#____________________________________________________________________________
#fiveK model

#checking mean and sd of variable
round(stat.desc(fiveK$avgLinkWeight),3)
round(stat.desc(fiveK$coef),3)
#scaling?

fiveK$avgLinkWeight.s = scale(fiveK$avgLinkWeight)
fiveK$count.s = scale(fiveK$count)
fiveK$lengthsum.s = scale(fiveK$lengthsum)
fiveK$coef.s = scale(fiveK$coef)

#the one that works
fiveK$Drugs = as.integer(fiveK$Drugs)
drugmodel <- rethinking::map(
  alist(
    avgLinkWeight.s ~ dnorm(mu, sigma),
    mu <- a + b*Drugs, 
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,2)
  ),
  data = fiveK)
#______________________________________-

drugmodel <- rethinking::map(
  alist(
    Drugs ~ dnorm(mu, sigma),
    mu <- a + b1*coef.s + b2*avgLinkWeight.s, 
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ),
  data = fiveK)



precis(drugmodel,corr=T)
vcov(drugmodel)
plot(diag(vcov(drugmodel)))

plot(precis(drugmodel),main = "Predicting avgLinkWeight from Drugs(b)")
#______________________________________________--
#so now, do logistic regression
m10.1 <- map(
  alist(
    Drugs ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ) ,
  data=oneK )
precis(m10.1)
plot(precis(m10.1))

m10.2 <- map(
    alist(
    Drugs ~ dbinom( 1 , p ) ,
    logit(p) <- a + bp*avgLinkWeight ,
    a ~ dnorm(0,2) ,
    bp ~ dnorm(0.8,0.05)
    
  ) ,
  data=oneK)



precis(m10.2)

logistic( c(6.76,19.97) )
          
plot(precis(m10.2),main = "Predicting Drugs(a) from avgLinkWeight(bp) and coef(bq)")
#sim 2______________________________________________________________________________________


d = read.csv("without intervention 2 last 1000.csv")
d1 = read.csv("with intervention 2 last 1000.csv")

#do without then with with count links (above threshold)
count = c(mean(d$X1.1),
          mean(d$X2.1),
          mean(d$X3.1),
          mean(d$X4.1),
          mean(d$X5.1),
          mean(d$X6.1),
          mean(d$X7.1),
          mean(d$X8.1),
          mean(d$X9.1),
          mean(d$X10.1),
          mean(d$X11.1),
          mean(d$X12.1),
          mean(d$X13.1),
          mean(d$X14.1),
          mean(d$X15.1),
          mean(d$X16.1),
          mean(d$X17.1),
          mean(d$X18.1),
          mean(d$X19.1),
          mean(d$X20.1),
          mean(d$X21.1),
          mean(d$X22.1),
          mean(d$X23.1),
          mean(d$X24.1),
          mean(d$X25.1),
          mean(d$X26.1),
          mean(d$X27.1),
          mean(d$X28.1),
          mean(d$X29.1),
          mean(d$X30.1),
          mean(d$X31.1),
          mean(d$X32.1),
          mean(d$X33.1),
          mean(d$X34.1),
          mean(d$X35.1),
          mean(d$X36.1),
          mean(d$X37.1),
          mean(d$X38.1),
          mean(d$X39.1),
          mean(d$X40.1),
          mean(d$X41.1),
          mean(d$X42.1),
          mean(d$X43.1),
          mean(d$X44.1),
          mean(d$X45.1),
          mean(d$X46.1),
          mean(d$X47.1),
          mean(d$X48.1),
          mean(d$X49.1),
          mean(d$X50.1),
          mean(d$X51.1),
          mean(d$X52.1),
          mean(d$X53.1),
          mean(d$X54.1),
          mean(d$X55.1),
          mean(d$X56.1),
          mean(d$X57.1),
          mean(d$X58.1),
          mean(d$X59.1),
          mean(d$X60.1),
          mean(d$X61.1),
          mean(d$X62.1),
          mean(d$X63.1),
          mean(d$X64.1),
          mean(d$X65.1),
          mean(d$X66.1),
          mean(d$X67.1),
          mean(d$X68.1),
          mean(d$X69.1),
          mean(d$X70.1),
          mean(d$X71.1),
          mean(d$X72.1),
          mean(d$X73.1),
          mean(d$X74.1),
          mean(d$X75.1),
          mean(d$X76.1),
          mean(d$X77.1),
          mean(d$X78.1),
          mean(d$X79.1),
          mean(d$X80.1),
          mean(d$X81.1),
          mean(d$X82.1),
          mean(d$X83.1),
          mean(d$X84.1),
          mean(d$X85.1),
          mean(d$X86.1),
          mean(d$X87.1),
          mean(d$X88.1),
          mean(d$X89.1),
          mean(d$X90.1),
          mean(d$X91.1),
          mean(d$X92.1),
          mean(d$X93.1),
          mean(d$X94.1),
          mean(d$X95.1),
          mean(d$X96.1),
          mean(d$X97.1),
          mean(d$X98.1),
          mean(d$X99.1),
          mean(d$X100.1)
)

#with
count1 = c(mean(d1$X1.1),
           mean(d1$X2.1),
           mean(d1$X3.1),
           mean(d1$X4.1),
           mean(d1$X5.1),
           mean(d1$X6.1),
           mean(d1$X7.1),
           mean(d1$X8.1),
           mean(d1$X9.1),
           mean(d1$X10.1),
           mean(d1$X11.1),
           mean(d1$X12.1),
           mean(d1$X13.1),
           mean(d1$X14.1),
           mean(d1$X15.1),
           mean(d1$X16.1),
           mean(d1$X17.1),
           mean(d1$X18.1),
           mean(d1$X19.1),
           mean(d1$X20.1),
           mean(d1$X21.1),
           mean(d1$X22.1),
           mean(d1$X23.1),
           mean(d1$X24.1),
           mean(d1$X25.1),
           mean(d1$X26.1),
           mean(d1$X27.1),
           mean(d1$X28.1),
           mean(d1$X29.1),
           mean(d1$X30.1),
           mean(d1$X31.1),
           mean(d1$X32.1),
           mean(d1$X33.1),
           mean(d1$X34.1),
           mean(d1$X35.1),
           mean(d1$X36.1),
           mean(d1$X37.1),
           mean(d1$X38.1),
           mean(d1$X39.1),
           mean(d1$X40.1),
           mean(d1$X41.1),
           mean(d1$X42.1),
           mean(d1$X43.1),
           mean(d1$X44.1),
           mean(d1$X45.1),
           mean(d1$X46.1),
           mean(d1$X47.1),
           mean(d1$X48.1),
           mean(d1$X49.1),
           mean(d1$X50.1),
           mean(d1$X51.1),
           mean(d1$X52.1),
           mean(d1$X53.1),
           mean(d1$X54.1),
           mean(d1$X55.1),
           mean(d1$X56.1),
           mean(d1$X57.1),
           mean(d1$X58.1),
           mean(d1$X59.1),
           mean(d1$X60.1),
           mean(d1$X61.1),
           mean(d1$X62.1),
           mean(d1$X63.1),
           mean(d1$X64.1),
           mean(d1$X65.1),
           mean(d1$X66.1),
           mean(d1$X67.1),
           mean(d1$X68.1),
           mean(d1$X69.1),
           mean(d1$X70.1),
           mean(d1$X71.1),
           mean(d1$X72.1),
           mean(d1$X73.1),
           mean(d1$X74.1),
           mean(d1$X75.1),
           mean(d1$X76.1),
           mean(d1$X77.1),
           mean(d1$X78.1),
           mean(d1$X79.1),
           mean(d1$X80.1),
           mean(d1$X81.1),
           mean(d1$X82.1),
           mean(d1$X83.1),
           mean(d1$X84.1),
           mean(d1$X85.1),
           mean(d1$X86.1),
           mean(d1$X87.1),
           mean(d1$X88.1),
           mean(d1$X89.1),
           mean(d1$X90.1),
           mean(d1$X91.1),
           mean(d1$X92.1),
           mean(d1$X93.1),
           mean(d1$X94.1),
           mean(d1$X95.1),
           mean(d1$X96.1),
           mean(d1$X97.1),
           mean(d1$X98.1),
           mean(d1$X99.1),
           mean(d1$X100.1)
)
frame1 = data.frame(count,count1)
write.csv(frame1, "count last 1000 2.csv")

#do without then with with sum of link lengths (above threshold)
lengthsum = c(mean(d$X1.2),
              mean(d$X2.2),
              mean(d$X3.2),
              mean(d$X4.2),
              mean(d$X5.2),
              mean(d$X6.2),
              mean(d$X7.2),
              mean(d$X8.2),
              mean(d$X9.2),
              mean(d$X10.2),
              mean(d$X11.2),
              mean(d$X12.2),
              mean(d$X13.2),
              mean(d$X14.2),
              mean(d$X15.2),
              mean(d$X16.2),
              mean(d$X17.2),
              mean(d$X18.2),
              mean(d$X19.2),
              mean(d$X20.2),
              mean(d$X21.2),
              mean(d$X22.2),
              mean(d$X23.2),
              mean(d$X24.2),
              mean(d$X25.2),
              mean(d$X26.2),
              mean(d$X27.2),
              mean(d$X28.2),
              mean(d$X29.2),
              mean(d$X30.2),
              mean(d$X31.2),
              mean(d$X32.2),
              mean(d$X33.2),
              mean(d$X34.2),
              mean(d$X35.2),
              mean(d$X36.2),
              mean(d$X37.2),
              mean(d$X38.2),
              mean(d$X39.2),
              mean(d$X40.2),
              mean(d$X41.2),
              mean(d$X42.2),
              mean(d$X43.2),
              mean(d$X44.2),
              mean(d$X45.2),
              mean(d$X46.2),
              mean(d$X47.2),
              mean(d$X48.2),
              mean(d$X49.2),
              mean(d$X50.2),
              mean(d$X51.2),
              mean(d$X52.2),
              mean(d$X53.2),
              mean(d$X54.2),
              mean(d$X55.2),
              mean(d$X56.2),
              mean(d$X57.2),
              mean(d$X58.2),
              mean(d$X59.2),
              mean(d$X60.2),
              mean(d$X61.2),
              mean(d$X62.2),
              mean(d$X63.2),
              mean(d$X64.2),
              mean(d$X65.2),
              mean(d$X66.2),
              mean(d$X67.2),
              mean(d$X68.2),
              mean(d$X69.2),
              mean(d$X70.2),
              mean(d$X71.2),
              mean(d$X72.2),
              mean(d$X73.2),
              mean(d$X74.2),
              mean(d$X75.2),
              mean(d$X76.2),
              mean(d$X77.2),
              mean(d$X78.2),
              mean(d$X79.2),
              mean(d$X80.2),
              mean(d$X81.2),
              mean(d$X82.2),
              mean(d$X83.2),
              mean(d$X84.2),
              mean(d$X85.2),
              mean(d$X86.2),
              mean(d$X87.2),
              mean(d$X88.2),
              mean(d$X89.2),
              mean(d$X90.2),
              mean(d$X91.2),
              mean(d$X92.2),
              mean(d$X93.2),
              mean(d$X94.2),
              mean(d$X95.2),
              mean(d$X96.2),
              mean(d$X97.2),
              mean(d$X98.2),
              mean(d$X99.2),
              mean(d$X100.2)
)

#with
lengthsum1 = c(mean(d1$X1.2),
               mean(d1$X2.2),
               mean(d1$X3.2),
               mean(d1$X4.2),
               mean(d1$X5.2),
               mean(d1$X6.2),
               mean(d1$X7.2),
               mean(d1$X8.2),
               mean(d1$X9.2),
               mean(d1$X10.2),
               mean(d1$X11.2),
               mean(d1$X12.2),
               mean(d1$X13.2),
               mean(d1$X14.2),
               mean(d1$X15.2),
               mean(d1$X16.2),
               mean(d1$X17.2),
               mean(d1$X18.2),
               mean(d1$X19.2),
               mean(d1$X20.2),
               mean(d1$X21.2),
               mean(d1$X22.2),
               mean(d1$X23.2),
               mean(d1$X24.2),
               mean(d1$X25.2),
               mean(d1$X26.2),
               mean(d1$X27.2),
               mean(d1$X28.2),
               mean(d1$X29.2),
               mean(d1$X30.2),
               mean(d1$X31.2),
               mean(d1$X32.2),
               mean(d1$X33.2),
               mean(d1$X34.2),
               mean(d1$X35.2),
               mean(d1$X36.2),
               mean(d1$X37.2),
               mean(d1$X38.2),
               mean(d1$X39.2),
               mean(d1$X40.2),
               mean(d1$X41.2),
               mean(d1$X42.2),
               mean(d1$X43.2),
               mean(d1$X44.2),
               mean(d1$X45.2),
               mean(d1$X46.2),
               mean(d1$X47.2),
               mean(d1$X48.2),
               mean(d1$X49.2),
               mean(d1$X50.2),
               mean(d1$X51.2),
               mean(d1$X52.2),
               mean(d1$X53.2),
               mean(d1$X54.2),
               mean(d1$X55.2),
               mean(d1$X56.2),
               mean(d1$X57.2),
               mean(d1$X58.2),
               mean(d1$X59.2),
               mean(d1$X60.2),
               mean(d1$X61.2),
               mean(d1$X62.2),
               mean(d1$X63.2),
               mean(d1$X64.2),
               mean(d1$X65.2),
               mean(d1$X66.2),
               mean(d1$X67.2),
               mean(d1$X68.2),
               mean(d1$X69.2),
               mean(d1$X70.2),
               mean(d1$X71.2),
               mean(d1$X72.2),
               mean(d1$X73.2),
               mean(d1$X74.2),
               mean(d1$X75.2),
               mean(d1$X76.2),
               mean(d1$X77.2),
               mean(d1$X78.2),
               mean(d1$X79.2),
               mean(d1$X80.2),
               mean(d1$X81.2),
               mean(d1$X82.2),
               mean(d1$X83.2),
               mean(d1$X84.2),
               mean(d1$X85.2),
               mean(d1$X86.2),
               mean(d1$X87.2),
               mean(d1$X88.2),
               mean(d1$X89.2),
               mean(d1$X90.2),
               mean(d1$X91.2),
               mean(d1$X92.2),
               mean(d1$X93.2),
               mean(d1$X94.2),
               mean(d1$X95.2),
               mean(d1$X96.2),
               mean(d1$X97.2),
               mean(d1$X98.2),
               mean(d1$X99.2),
               mean(d1$X100.2)
)

frame2 = data.frame(lengthsum,lengthsum1)
write.csv(frame2, "lengthsum 1000 2.csv")


#
avgLinkWeight= c(mean(d$X1.3),
                 mean(d$X2.3),
                 mean(d$X3.3),
                 mean(d$X4.3),
                 mean(d$X5.3),
                 mean(d$X6.3),
                 mean(d$X7.3),
                 mean(d$X8.3),
                 mean(d$X9.3),
                 mean(d$X10.3),
                 mean(d$X11.3),
                 mean(d$X12.3),
                 mean(d$X13.3),
                 mean(d$X14.3),
                 mean(d$X15.3),
                 mean(d$X16.3),
                 mean(d$X17.3),
                 mean(d$X18.3),
                 mean(d$X19.3),
                 mean(d$X20.3),
                 mean(d$X21.3),
                 mean(d$X22.3),
                 mean(d$X23.3),
                 mean(d$X24.3),
                 mean(d$X25.3),
                 mean(d$X26.3),
                 mean(d$X27.3),
                 mean(d$X28.3),
                 mean(d$X29.3),
                 mean(d$X30.3),
                 mean(d$X31.3),
                 mean(d$X32.3),
                 mean(d$X33.3),
                 mean(d$X34.3),
                 mean(d$X35.3),
                 mean(d$X36.3),
                 mean(d$X37.3),
                 mean(d$X38.3),
                 mean(d$X39.3),
                 mean(d$X40.3),
                 mean(d$X41.3),
                 mean(d$X42.3),
                 mean(d$X43.3),
                 mean(d$X44.3),
                 mean(d$X45.3),
                 mean(d$X46.3),
                 mean(d$X47.3),
                 mean(d$X48.3),
                 mean(d$X49.3),
                 mean(d$X50.3),
                 mean(d$X51.3),
                 mean(d$X52.3),
                 mean(d$X53.3),
                 mean(d$X54.3),
                 mean(d$X55.3),
                 mean(d$X56.3),
                 mean(d$X57.3),
                 mean(d$X58.3),
                 mean(d$X59.3),
                 mean(d$X60.3),
                 mean(d$X61.3),
                 mean(d$X62.3),
                 mean(d$X63.3),
                 mean(d$X64.3),
                 mean(d$X65.3),
                 mean(d$X66.3),
                 mean(d$X67.3),
                 mean(d$X68.3),
                 mean(d$X69.3),
                 mean(d$X70.3),
                 mean(d$X71.3),
                 mean(d$X72.3),
                 mean(d$X73.3),
                 mean(d$X74.3),
                 mean(d$X75.3),
                 mean(d$X76.3),
                 mean(d$X77.3),
                 mean(d$X78.3),
                 mean(d$X79.3),
                 mean(d$X80.3),
                 mean(d$X81.3),
                 mean(d$X82.3),
                 mean(d$X83.3),
                 mean(d$X84.3),
                 mean(d$X85.3),
                 mean(d$X86.3),
                 mean(d$X87.3),
                 mean(d$X88.3),
                 mean(d$X89.3),
                 mean(d$X90.3),
                 mean(d$X91.3),
                 mean(d$X92.3),
                 mean(d$X93.3),
                 mean(d$X94.3),
                 mean(d$X95.3),
                 mean(d$X96.3),
                 mean(d$X97.3),
                 mean(d$X98.3),
                 mean(d$X99.3),
                 mean(d$X100.3)
)

#with
avgLinkWeight1 = c(mean(d1$X1.3),
               mean(d1$X2.3),
               mean(d1$X3.3),
               mean(d1$X4.3),
               mean(d1$X5.3),
               mean(d1$X6.3),
               mean(d1$X7.3),
               mean(d1$X8.3),
               mean(d1$X9.3),
               mean(d1$X10.3),
               mean(d1$X11.3),
               mean(d1$X12.3),
               mean(d1$X13.3),
               mean(d1$X14.3),
               mean(d1$X15.3),
               mean(d1$X16.3),
               mean(d1$X17.3),
               mean(d1$X18.3),
               mean(d1$X19.3),
               mean(d1$X20.3),
               mean(d1$X21.3),
               mean(d1$X22.3),
               mean(d1$X23.3),
               mean(d1$X24.3),
               mean(d1$X25.3),
               mean(d1$X26.3),
               mean(d1$X27.3),
               mean(d1$X28.3),
               mean(d1$X29.3),
               mean(d1$X30.3),
               mean(d1$X31.3),
               mean(d1$X32.3),
               mean(d1$X33.3),
               mean(d1$X34.3),
               mean(d1$X35.3),
               mean(d1$X36.3),
               mean(d1$X37.3),
               mean(d1$X38.3),
               mean(d1$X39.3),
               mean(d1$X40.3),
               mean(d1$X41.3),
               mean(d1$X42.3),
               mean(d1$X43.3),
               mean(d1$X44.3),
               mean(d1$X45.3),
               mean(d1$X46.3),
               mean(d1$X47.3),
               mean(d1$X48.3),
               mean(d1$X49.3),
               mean(d1$X50.3),
               mean(d1$X51.3),
               mean(d1$X52.3),
               mean(d1$X53.3),
               mean(d1$X54.3),
               mean(d1$X55.3),
               mean(d1$X56.3),
               mean(d1$X57.3),
               mean(d1$X58.3),
               mean(d1$X59.3),
               mean(d1$X60.3),
               mean(d1$X61.3),
               mean(d1$X62.3),
               mean(d1$X63.3),
               mean(d1$X64.3),
               mean(d1$X65.3),
               mean(d1$X66.3),
               mean(d1$X67.3),
               mean(d1$X68.3),
               mean(d1$X69.3),
               mean(d1$X70.3),
               mean(d1$X71.3),
               mean(d1$X72.3),
               mean(d1$X73.3),
               mean(d1$X74.3),
               mean(d1$X75.3),
               mean(d1$X76.3),
               mean(d1$X77.3),
               mean(d1$X78.3),
               mean(d1$X79.3),
               mean(d1$X80.3),
               mean(d1$X81.3),
               mean(d1$X82.3),
               mean(d1$X83.3),
               mean(d1$X84.3),
               mean(d1$X85.3),
               mean(d1$X86.3),
               mean(d1$X87.3),
               mean(d1$X88.3),
               mean(d1$X89.3),
               mean(d1$X90.3),
               mean(d1$X91.3),
               mean(d1$X92.3),
               mean(d1$X93.3),
               mean(d1$X94.3),
               mean(d1$X95.3),
               mean(d1$X96.3),
               mean(d1$X97.3),
               mean(d1$X98.3),
               mean(d1$X99.3),
               mean(d1$X100.3)
)

frame3 = data.frame(avgLinkWeight,avgLinkWeight1)
write.csv(frame3, "avgLinkWeight 1000 2.csv")

coef= c(mean(d$X1.4),
                 mean(d$X2.4),
                 mean(d$X3.4),
                 mean(d$X4.4),
                 mean(d$X5.4),
                 mean(d$X6.4),
                 mean(d$X7.4),
                 mean(d$X8.4),
                 mean(d$X9.4),
                 mean(d$X10.4),
                 mean(d$X11.4),
                 mean(d$X12.4),
                 mean(d$X13.4),
                 mean(d$X14.4),
                 mean(d$X15.4),
                 mean(d$X16.4),
                 mean(d$X17.4),
                 mean(d$X18.4),
                 mean(d$X19.4),
                 mean(d$X20.4),
                 mean(d$X21.4),
                 mean(d$X22.4),
                 mean(d$X23.4),
                 mean(d$X24.4),
                 mean(d$X25.4),
                 mean(d$X26.4),
                 mean(d$X27.4),
                 mean(d$X28.4),
                 mean(d$X29.4),
                 mean(d$X30.4),
                 mean(d$X31.4),
                 mean(d$X32.4),
                 mean(d$X33.4),
                 mean(d$X34.4),
                 mean(d$X35.4),
                 mean(d$X36.4),
                 mean(d$X37.4),
                 mean(d$X38.4),
                 mean(d$X39.4),
                 mean(d$X40.4),
                 mean(d$X41.4),
                 mean(d$X42.4),
                 mean(d$X43.4),
                 mean(d$X44.4),
                 mean(d$X45.4),
                 mean(d$X46.4),
                 mean(d$X47.4),
                 mean(d$X48.4),
                 mean(d$X49.4),
                 mean(d$X50.4),
                 mean(d$X51.4),
                 mean(d$X52.4),
                 mean(d$X53.4),
                 mean(d$X54.4),
                 mean(d$X55.4),
                 mean(d$X56.4),
                 mean(d$X57.4),
                 mean(d$X58.4),
                 mean(d$X59.4),
                 mean(d$X60.4),
                 mean(d$X61.4),
                 mean(d$X62.4),
                 mean(d$X63.4),
                 mean(d$X64.4),
                 mean(d$X65.4),
                 mean(d$X66.4),
                 mean(d$X67.4),
                 mean(d$X68.4),
                 mean(d$X69.4),
                 mean(d$X70.4),
                 mean(d$X71.4),
                 mean(d$X72.4),
                 mean(d$X73.4),
                 mean(d$X74.4),
                 mean(d$X75.4),
                 mean(d$X76.4),
                 mean(d$X77.4),
                 mean(d$X78.4),
                 mean(d$X79.4),
                 mean(d$X80.4),
                 mean(d$X81.4),
                 mean(d$X82.4),
                 mean(d$X83.4),
                 mean(d$X84.4),
                 mean(d$X85.4),
                 mean(d$X86.4),
                 mean(d$X87.4),
                 mean(d$X88.4),
                 mean(d$X89.4),
                 mean(d$X90.4),
                 mean(d$X91.4),
                 mean(d$X92.4),
                 mean(d$X93.4),
                 mean(d$X94.4),
                 mean(d$X95.4),
                 mean(d$X96.4),
                 mean(d$X97.4),
                 mean(d$X98.4),
                 mean(d$X99.4),
                 mean(d$X100.4)
)

#with
coef1 = c(mean(d1$X1.4),
               mean(d1$X2.4),
               mean(d1$X3.4),
               mean(d1$X4.4),
               mean(d1$X5.4),
               mean(d1$X6.4),
               mean(d1$X7.4),
               mean(d1$X8.4),
               mean(d1$X9.4),
               mean(d1$X10.4),
               mean(d1$X11.4),
               mean(d1$X12.4),
               mean(d1$X13.4),
               mean(d1$X14.4),
               mean(d1$X15.4),
               mean(d1$X16.4),
               mean(d1$X17.4),
               mean(d1$X18.4),
               mean(d1$X19.4),
               mean(d1$X20.4),
               mean(d1$X21.4),
               mean(d1$X22.4),
               mean(d1$X23.4),
               mean(d1$X24.4),
               mean(d1$X25.4),
               mean(d1$X26.4),
               mean(d1$X27.4),
               mean(d1$X28.4),
               mean(d1$X29.4),
               mean(d1$X30.4),
               mean(d1$X31.4),
               mean(d1$X32.4),
               mean(d1$X33.4),
               mean(d1$X34.4),
               mean(d1$X35.4),
               mean(d1$X36.4),
               mean(d1$X37.4),
               mean(d1$X38.4),
               mean(d1$X39.4),
               mean(d1$X40.4),
               mean(d1$X41.4),
               mean(d1$X42.4),
               mean(d1$X43.4),
               mean(d1$X44.4),
               mean(d1$X45.4),
               mean(d1$X46.4),
               mean(d1$X47.4),
               mean(d1$X48.4),
               mean(d1$X49.4),
               mean(d1$X50.4),
               mean(d1$X51.4),
               mean(d1$X52.4),
               mean(d1$X53.4),
               mean(d1$X54.4),
               mean(d1$X55.4),
               mean(d1$X56.4),
               mean(d1$X57.4),
               mean(d1$X58.4),
               mean(d1$X59.4),
               mean(d1$X60.4),
               mean(d1$X61.4),
               mean(d1$X62.4),
               mean(d1$X63.4),
               mean(d1$X64.4),
               mean(d1$X65.4),
               mean(d1$X66.4),
               mean(d1$X67.4),
               mean(d1$X68.4),
               mean(d1$X69.4),
               mean(d1$X70.4),
               mean(d1$X71.4),
               mean(d1$X72.4),
               mean(d1$X73.4),
               mean(d1$X74.4),
               mean(d1$X75.4),
               mean(d1$X76.4),
               mean(d1$X77.4),
               mean(d1$X78.4),
               mean(d1$X79.4),
               mean(d1$X80.4),
               mean(d1$X81.4),
               mean(d1$X82.4),
               mean(d1$X83.4),
               mean(d1$X84.4),
               mean(d1$X85.4),
               mean(d1$X86.4),
               mean(d1$X87.4),
               mean(d1$X88.4),
               mean(d1$X89.4),
               mean(d1$X90.4),
               mean(d1$X91.4),
               mean(d1$X92.4),
               mean(d1$X93.4),
               mean(d1$X94.4),
               mean(d1$X95.4),
               mean(d1$X96.4),
               mean(d1$X97.4),
               mean(d1$X98.4),
               mean(d1$X99.4),
               mean(d1$X100.4)
)

frame4 = data.frame(coef,coef1)
write.csv(frame4, "coef 1000 2.csv")

#after reshaping in excel, reading all files in, creating a new dataframe of all with drugs variable
count = read.csv("./dataframe 1000 2/count 1000 2.csv")
lengthsum = read.csv("./dataframe 1000 2/lengthsum 1000 2.csv")
coef = read.csv("./dataframe 1000 2/coef last 1000 2.csv")
avgLinkWeight = read.csv("./dataframe 1000 2/avgLinkWeight last 1000 2.csv")
oneK = merge(count,lengthsum, all = T)
oneK = merge(oneK,coef, all = T)
oneK = merge(oneK, avgLinkWeight, all = T)
write.csv(oneK, "oneK2.csv")


#after reshaping in excel, reading all files in, creating a new dataframe of all with drugs variable
count = read.csv("./dataframe 5000 2/count last 5000 2.csv")
lengthsum = read.csv("./dataframe 5000 2/lengthsum 5000 2.csv")
coef = read.csv("./dataframe 5000 2/coef last 5000 2.csv")
avgLinkWeight = read.csv("./dataframe 5000 2/avgLinkWeight last 5000 2.csv")
fiveK = merge(count,lengthsum, all = T)
fiveK = merge(fiveK,coef, all = T)
fiveK = merge(fiveK, avgLinkWeight, all = T)
write.csv(fiveK, "fiveK2.csv")


#after cleaning, read the analyseable dataframes in

oneK2 = read.csv("./dataframe 1000 2/oneK2s.csv")
fiveK2 = read.csv("./dataframe 5000 2/fiveK2s.csv")

oneK$avgLinkWeight.s = scale(oneK$avgLinkWeight)
oneK$count.s = scale(oneK$count)
oneK$lengthsum.s = scale(oneK$lengthsum)
oneK$coef.s = scale(oneK$coef)

fiveK$avgLinkWeight.s = scale(fiveK$avgLinkWeight)
fiveK$count.s = scale(fiveK$count)
fiveK$lengthsum.s = scale(fiveK$lengthsum)
fiveK$coef.s = scale(fiveK$coef)

write.csv(fiveK, "fiveKs.csv")
write.csv(fiveK2, "fiveK2s.csv")

write.csv(oneK, "oneKs.csv")
write.csv(oneK2, "oneK2s.csv")
#so now, do logistic regression
oneK$Drugs = as.integer(oneK$Drugs)
oneK2$Drugs = as.integer(oneK2$Drugs)

m10.2 <- map(
  alist(
    Drugs ~ dbinom( 1 , p ) ,
    logit(p) <- a + bp*avgLinkWeight.s  + bq*coef.s,
    a ~ dnorm(0,2) ,
    bp ~ dnorm(0,0.2),
    bq ~ dnorm(0,0.2)
    
  ) ,
  data=oneK)



precis(m10.2)

logistic( c(-0.09,0.51) )

plot(precis(m10.2),main = "Predicting Drugs(a) from avgLinkWeight(bp) and coef(bq)")
samples = extract.samples(m10.2)
HPDI(samples)

cheat = glm(Drugs~avgLinkWeight+coef, family = "binomial", fiveK)
summary(cheat)

plot(fiveK$coef)
fiveK2$Drugs = as.factor(fiveK2$Drugs)
ggplot(oneK2, aes(simNum,coef, fill = Drugs)) + geom_point() + geom_smooth(method = "lm")

#________________________________________________________________________________

oneK_m <- brm(Drugs ~ 1 + coef.s, 
         data = oneK,iter = 2000, cores = 2, chain = 2, family =  binomial("probit"))

summary(oneK_m,waic = T)
stanplot(oneK_m)
pp_check(oneK_m)

oneK_m2 <- brm(Drugs ~ 1 + avgLinkWeight.s , 
              data = oneK,iter = 2000, cores = 2, chain = 2, family =  binomial("probit"))
summary(oneK_m2,waic=T)

logistic()
waic(oneK_m,oneK_m2)

#coef has lower waic


oneK2_m <- brm(Drugs ~ 1 + coef.s , 
         data = oneK2,iter = 5000, cores = 2, chain = 2  ,family =  binomial("probit"))
summary(oneK2_m)




oneK2_m2 <- brm(Drugs ~ 1 + avgLinkWeight.s , 
               data = oneK2,iter = 5000, cores = 2, chain = 2, family = binomial("probit"))
summary(oneK2_m2)
waic(oneK2_m,oneK2_m2)

#avg link weight much better
waic(oneK2_m,oneK2_m2)


oneK_o <- brm(Drugs ~ 1 + avgLinkWeight.s * coef.s, 
                data = oneK,iter = 5000, cores = 2, chain = 2)
summary(oneK_o)
waic(oneK_o,oneK_m,oneK_m2)


stanplot(oneK_o)
plot(oneK2$avgLinkWeight.s)
