#-------------------- Import data file --------------------#
library(readxl)
#pubg_100000 <- read_excel("C:/Users/Alicia/Desktop/pubg_100000.xlsx")
pubg_100000 <- read_excel("C:/Users/Alicia/Downloads/all/train_V2.xlsx")
attach(pubg_100000)

#-------------------- Correlation --------------------#
column<-c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,21,22,23,24,25,26,27,28,29)
solo<-subset(pubg_100000[column],matchType=="solo-fpp"|matchType=="solo"|matchType=="normal-solo"|matchType=="normal-solo-fpp")

multiple<-subset(pubg_100000, matchType=="duo-fpp"|matchType=="duo"|
                              matchType=="squad-fpp"|matchType=="squad"|
                              matchType=="normal-squad-fpp"|matchType=="normal-squad"|
                              matchType=="normal-duo-fpp"| matchType=="normal-duo" )

#see all the correlation of variables in solo
#install.packages("corrplot")
library(corrplot)
res_solo<-cor(solo[5:27])
corrplot(res_solo, type = "upper", tl.col = "black", tl.srt = 45)

#see all the correlation of variables in multi
res_multiple<-cor(multiple[5:29])
corrplot(res_multiple, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#-------------------- Determine the regression model --------------------#
#regression model for solo mode
fit1 <- lm(winPlacePerc ~ walkDistance + killPlace + boosts + weaponsAcquired + damageDealt + kills, data=solo)
summary(fit1)

#regression model for multi mode
fit2 <- lm(winPlacePerc ~ walkDistance + killPlace + boosts + weaponsAcquired + heals + damageDealt, data=multiple)
summary(fit2)



library(readxl)
train_solo <- read_excel("C:/Users/Alicia/Downloads/all/solo.xlsx")
test_solo<- read_excel("C:/Users/Alicia/Downloads/all/solo_test.xlsx")

r2 = lm(train_solo$winPlacePerc ~ train_solo$walkDistance + train_solo$killPlace + 
        train_solo$boosts + train_solo$weaponsAcquired + train_solo$damageDealt + train_solo$kills)
r2.predict <- predict(r2 ,data=test_solo)
plot(r2.predict)
points(train_solo$winPlacePerc, col = 2)

write.csv(r2.predict,file="C:/Users/Alicia/Downloads/all/solo_predict.csv",row.names = FALSE)

train_multi<- read_excel("C:/Users/Alicia/Downloads/all/multi.xlsx")
test_multi<- read_excel("C:/Users/Alicia/Downloads/all/multi_test.xlsx")
r1 = lm(train_multi$winPlacePerc ~ train_multi$walkDistance +  train_multi$killPlace + train_multi$boosts + train_multi$weaponsAcquired + train_multi$heals +train_multi$damageDealt)
r1.predict <- predict(r1 ,data=test_multi)
plot(r1.predict)
points(train_multi$winPlacePerc, col = 2)

write.csv(r2.predict,file="C:/Users/Alicia/Downloads/all/multi_predict.csv",row.names = FALSE)



