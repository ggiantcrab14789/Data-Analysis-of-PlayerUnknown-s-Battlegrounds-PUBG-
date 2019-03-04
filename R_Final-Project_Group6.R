install.packages("nnet")
install.packages("car")
install.packages("afex")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("tree")
install.packages("party")
library(nnet)
library(car)
library(AER)
library(afex)
library(randomForest)
library(tree)
library(party)
require(rpart)
require(rpart.plot)
library(ggplot2)

#-------------------- 導入數據 並設定部分變數為factor變數 --------------------#
library(readr)
data <- read_csv("R Computing/Final Project/data.csv", 
                 col_types = cols(Education = col_factor(levels = c("1", 
                                                                    "2", "3", "4","5")), EnvironmentSatisfaction = col_number(), 
                                  JobInvolvement = col_number(), JobLevel = col_factor(levels = c("1", 
                                                                                                  "2", "3", "4","5")), JobSatisfaction = col_number(), 
                                  StockOptionLevel = col_factor(levels = c("0", 
                                                                           "1", "2", "3")), WorkLifeBalance = col_number()))
View(data)
attach(data)

#-------------------- 敘述統計分析 --------------------#
summary(data)
table(Attrition)
table(BusinessTravel)
table(Department)
table(Gender)
table(JobRole)
table(MaritalStatus)
table(OverTime)
table(WorkLifeBalance)
#繪製條形圖 觀察分佈
ggplot(data, aes(x = Age)) +
  geom_bar(fill = "lightblue")
#age近似服從正態分佈
ggplot(data, aes(x = BusinessTravel)) +
  geom_bar(fill = "lightblue")
#絕大部分員工都是travel rarely
ggplot(data, aes(x = Attrition)) +
  geom_bar(fill = "lightblue")
#極少數員工離職
ggplot(data, aes(x = Department)) +
  geom_bar(fill = "lightblue")
#大部分都是R&D
ggplot(data, aes(x = DistanceFromHome)) +
  geom_bar(fill = "lightblue")
#普遍離公司較近 不服從正態分佈
ggplot(data, aes(x = Education)) +
  geom_bar(fill = "lightblue")
#3、4最多
ggplot(data, aes(x = EnvironmentSatisfaction)) +
  geom_bar(fill = "lightblue")
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "lightblue")
#男多女少
ggplot(data, aes(x = JobInvolvement)) +
  geom_bar(fill = "lightblue")
#3最多
ggplot(data, aes(x = JobLevel)) +
  geom_bar(fill = "lightblue")
#1、2最多
ggplot(data, aes(x = JobSatisfaction)) +
  geom_bar(fill = "lightblue")
#3、4多
ggplot(data, aes(x = MaritalStatus)) +
  geom_bar(fill = "lightblue")
#married最多
boxplot(MonthlyIncome)
incomeinterval=ifelse(MonthlyIncome<=5000,'1(0-5000)',ifelse(MonthlyIncome>5000 & MonthlyIncome<=10000,'2(5000~10000)',ifelse(MonthlyIncome>10000 & MonthlyIncome<=15000,'3(10000~15000)','4(15000~)')))
ggplot(data, aes(x = incomeinterval)) +
  geom_bar(fill = "lightblue")
#5000以下薪酬的人最多
ggplot(data, aes(x = NumCompaniesWorked)) +
  geom_bar(fill = "lightblue")
#1个最多（很突出）
ggplot(data, aes(x = OverTime)) +
  geom_bar(fill = "lightblue")
#大部分不加班
ggplot(data, aes(x = PercentSalaryHike)) +
  geom_bar(fill = "lightblue")
#大部分在15以下
ggplot(data, aes(x = PerformanceRating)) +
  geom_bar(fill = "lightblue")
#只有3、4两个值，大部分为3
ggplot(data, aes(x = StockOptionLevel)) +
  geom_bar(fill = "lightblue")
#0、1最多
ggplot(data, aes(x = TotalWorkingYears)) +
  geom_bar(fill = "lightblue")
#10最多，不服從正態分布
ggplot(data, aes(x = TrainingTimesLastYear)) +
  geom_bar(fill = "lightblue")
#2、3最多
ggplot(data, aes(x = WorkLifeBalance)) +
  geom_bar(fill = "lightblue")
#3最多
ggplot(data, aes(x = YearsAtCompany)) +
  geom_bar(fill = "lightblue")
#絕大部分都不超过10年
ggplot(data, aes(x = YearsWithCurrManager)) +
  geom_bar(fill = "lightblue")
#0、2、7最多，分佈不規則

#-------------------- 1 --------------------#
#探究影響離職的因素
data$Attrition = as.integer(data$Attrition == "Yes")


#-------------------- 題目1.1 --------------------#
# 先看一下資料的分佈
plot(table(data$Attrition, data$BusinessTravel))
plot(table(data$Attrition, data$MaritalStatus))

# r1.1.1 是探討出差對於離職的關係
# r1.1.2 是探討婚姻對於離職的關係
# r1.1.3 是同時探討出差及婚姻對離職的關係
r1.1.1 = glm(data$Attrition ~ data$BusinessTravel, family = binomial(link='logit'), data)
r1.1.2 = glm(data$Attrition ~ data$MaritalStatus, family = binomial(link='logit'), data)
r1.1.3 = glm(data$Attrition ~ factor(data$BusinessTravel) + factor(data$MaritalStatus) + 
               factor(data$BusinessTravel):factor(data$MaritalStatus), family = binomial(link='logit'), data)

# 結果(s1.1.1) 顯示出差對於離職有顯著影響
# 結果(s1.1.2) 顯示婚姻對於離職有顯著影響
# 結果(s1.1.3) 同時考慮出差及婚姻對於離職有顯著影響
s1.1.1 = summary(r1.1.1)
s1.1.2 = summary(r1.1.2)
s1.1.3 = summary(r1.1.3)
print(s1.1.1)
print(s1.1.2)
print(s1.1.3)

#-------------------- 題目1.2 --------------------#
plot(data$Attrition, data$HourlyRate)

# r1.2 是探討時薪與離職的關係
r1.2 = glm(data$Attrition ~ data$HourlyRate, family = binomial(link='logit'), data)

# 結果(s1.2) 顯示部門工作時薪與離職無顯著影響
s1.2 = summary(r1.2)
print(s1.2)

#-------------------- 題目1.3 --------------------#
plot(data$Attrition, data$YearsInCurrentRole)

# r1.3 是探討待在目前職位的時間與離職的關係
r1.3 = glm(data$Attrition ~ data$YearsInCurrentRole, family = binomial(link='logit'), data)

# 結果(s1.2) 顯示部門工作時數與離職有顯著影響
s1.3 = summary(r1.3)
print(s1.3)

#-------------------- 題目1.4 --------------------#   
# r1.4 是探討與管理者相處時間與離職的關係 
r1.4 = glm(data$Attrition ~ data$YearsWithCurrManager, family = binomial(link='logit'), data)

# 結果(s1.4) 顯示與管理者相處時間與離職有顯著影響
s1.4 = summary(r1.4)
print(s1.4)

#-------------------- 題目1.5 --------------------#
# r1.5 是探討股權與離職的關係
r1.5 = glm(data$Attrition ~ data$StockOptionLevel, family = binomial(link='logit'), data)

# 結果(s1.5) 顯示股權與離職有顯著影響
s1.5 = summary(r1.5)
print(s1.5)

#-------------------- 題目1.6 --------------------#
# r1.6 是探討Work-Life Balance 與離職關係
r1.6 = glm(data$Attrition ~ data$WorkLifeBalance, family = binomial(link='logit'), data)

# 結果(s1.6) 顯示Work-Life Balance 與離職有顯著影響
s1.6 = summary(r1.6)
print(s1.6)

#-------------------- 題目1.7 --------------------#
# r1.7 是探討工作過的公司與離職關係
r1.7 = glm(data$Attrition ~ data$NumCompaniesWorked, family = binomial(link='logit'), data)

# 結果(s1.7) 顯示工作過的公司與離職無顯著影響
s1.7 = summary(r1.7)
print(s1.7)

#-------------------- 題目1.8 --------------------#
# r1.8 是探討公司距離與離職
r1.8 = glm(data$Attrition ~ data$DistanceFromHome, family = binomial(link='logit'), data)

# 結果(s1.8) 顯示公司距離與離職有顯著影響
s1.8 = summary(r1.8)
print(s1.8)


#-------------------- 2 --------------------#
#探究影響工作滿意度的因素
#看兩個變數之間的關係
satisfaction=factor(data$JobSatisfaction)
Marital=factor(data$MaritalStatus)
plot(satisfaction ~ Marital, data=data,off=0)
#married的工作滿意度最高
wlb=factor(data$WorkLifeBalance)
plot(satisfaction ~ wlb, data=data,off=0)
#滿意度排序wlb=3、1、4、2 非單調
plot(wlb ~ Marital, data=data,off=0)
#married員工balance較single低 原因可能是因為家庭事務繁重
bt=factor(data$BusinessTravel)
plot(satisfaction ~ bt, data=data,off=0)
#travel頻繁的滿意度較低，不出差的工作滿意度最高
plot(bt ~ Marital, data=data,off=0)
#企業裡married的員工出差最少（企業安排很人性化）

#回歸分析：探究影響工作滿意度的因素
m2.1=lm(JobSatisfaction ~ MaritalStatus,data=data)
summary(m2.1)
#婚姻狀況對滿意度的影響不顯著
m2.2=lm(JobSatisfaction ~ BusinessTravel,data=data)
summary(m2.2)
#出差對滿意度的影響不顯著
m2.3=lm(JobSatisfaction ~ YearsWithCurrManager,data=data)
summary(m2.3)
#與管理者相處的時間對滿意度的影響不顯著
m2.4=lm(JobSatisfaction ~ WorkLifeBalance,data=data)
summary(m2.4)
#worklifebalance對滿意度的影響不顯著
m2.5=lm(JobSatisfaction ~ NumCompaniesWorked,data=data)
summary(m2.5)
#顯著：工作過的公司數越多，顯著的滿意度越低->或許有些人就是天生喜歡跳槽
m2.6=lm(JobSatisfaction ~ DistanceFromHome,data=data)
summary(m2.6)
#通勤距離對滿意度的影響不顯著
m2.7=lm(JobSatisfaction ~ MonthlyIncome,data=data)
summary(m2.7)
#薪酬對滿意度的影響不顯著
m2.8=lm(JobSatisfaction ~ OverTime,data=data)
summary(m2.8)
#加班對滿意度的影響不顯著
m2.9=lm(JobSatisfaction ~ PerformanceRating,data=data)
summary(m2.9)
#performance rating對滿意度的影響不顯著


data <- read.csv("R Computing/Final Project/data.csv") 
#-------------------- 3.1 --------------------#
#探究影響Work-Life Balance的因素
satisfaction=factor(data$JobSatisfaction)
Marital=factor(data$MaritalStatus)
plot(satisfaction ~ Marital, data=data,off=0)

data.3.1 = droplevels(subset(data, MaritalStatus != "Single"))

# 進行邏輯回歸之前，先觀察資料分布狀況
plot(table(data$WorkLifeBalance, data$MaritalStatus))
plot(table(data$WorkLifeBalance, data$BusinessTravel))

# r3.1.1 是探討出差對於生活與工作的平衡的影響
# r3.1.2 是探討婚姻對於生活與工作的平衡的影響
# r3.1.3 是同時探討出差及婚姻對生活與工作的平衡的影響
r3.1.1 = lm(formula = data$WorkLifeBalance ~ factor(data$BusinessTravel), data)
r3.1.2 = lm(formula = data$WorkLifeBalance ~ factor(data$MaritalStatus), data)
r3.1.3 = lm(formula = data$WorkLifeBalance ~ factor(data$BusinessTravel) + factor(data$MaritalStatus) + 
              factor(data$BusinessTravel):factor(data$MaritalStatus), data)

# 結果(s3.1.1) 顯示出差對生活與工作的平衡無顯著影響
# 結果(s3.1.2) 顯示婚姻對生活與工作的平衡無顯著影響
# 結果(s3.1.3) 同時考慮出差及婚姻(含交互作用)對生活與工作的平衡無顯著影響
s3.1.1 = summary(r3.1.1)
s3.1.2 = summary(r3.1.2)
s3.1.3 = summary(r3.1.3)

# 以下反過來將將婚姻作依變數，以工作與生活的平衡及出差做依變數
# 進行類別資料分析之間，先觀察資料的分布狀況
plot(table(data.3.1$MaritalStatus, data.3.1$WorkLifeBalance))
plot(table(data.3.1$MaritalStatus, data.3.1$BusinessTravel))

# r3.1.4 是探討生活與工作的平衡對於婚姻的影響
# r3.1.5 是探討出差對於婚姻的影響
# r3.1.6 是同時探討生活與工作的平衡及出差對婚姻的影響
r3.1.4 = multinom(formula = data$MaritalStatus ~ data$WorkLifeBalance)
r3.1.5 = multinom(formula = data$MaritalStatus ~ factor(data$BusinessTravel))
r3.1.6 = multinom(formula = data$MaritalStatus ~ data$WorkLifeBalance + data$BusinessTravel + 
                    data$WorkLifeBalance:factor(data$BusinessTravel))
s3.1.4 = summary(r3.1.4)
s3.1.5 = summary(r3.1.5)
s3.1.6 = summary(r3.1.6)

# 以下是手動計算三個回歸式的p-value
# 結果(p3.1.5)顯示，出差對於婚姻狀態有顯著影響
z3.1.4 <- s3.1.4$coefficients / s3.1.4$standard.errors
p3.1.4 <- (1 - pnorm(abs(z3.1.4), 0, 1)) * 2
z3.1.5 <- s3.1.5$coefficients / s3.1.5$standard.errors
p3.1.5 <- (1 - pnorm(abs(z3.1.5), 0, 1)) * 2
z3.1.6 <- s3.1.6$coefficients / s3.1.6$standard.errors
p3.1.6 <- (1 - pnorm(abs(z3.1.6), 0, 1)) * 2
print(p3.1.4)
print(p3.1.5)
print(p3.1.6)

# r3.1.7 是探討婚姻對於出差的影響
r3.1.7 = multinom(formula = data$BusinessTravel ~ factor(data$Marital))
s3.1.7 = summary(r3.1.7)

# 以下是手動計算三個回歸式的p-value
# 結果(p3.1.5)顯示，出差對於婚姻狀態有顯著影響
z3.1.7 <- s3.1.7$coefficients / s3.1.7$standard.errors
p3.1.7 <- (1 - pnorm(abs(z3.1.7), 0, 1)) * 2
print(p3.1.7)


#-------------------- 3.2 --------------------#

# r3.2.1 是探討時薪對於工作與生活的平衡的影響
plot(data$HourlyRate, data$WorkLifeBalance)
r3.2.1 = lm(formula = data$WorkLifeBalance ~ data$HourlyRate)

# 結果(s3.2.1)顯示工作時薪對於工作與生活的平衡沒有顯著影響
s3.2.1 = summary(r3.2.1)

#-------------------- 3.3 --------------------#

# r3.3.1 是探討時薪對於工作與生活的平衡的影響
plot(data$DistanceFromHome, data$WorkLifeBalance)
r3.3.1 = lm(formula = data$WorkLifeBalance ~ data$DistanceFromHome)

# 結果(s3.3.1)顯示時薪對於工作與生活的平衡沒有顯著影響
s3.3.1 = summary(r3.3.1)

# r3.3.2 是探討加班對於工作與生活的平衡的影響
plot(data$OverTime, data$WorkLifeBalance)
r3.3.2 = lm(formula = data$WorkLifeBalance ~ factor(data$OverTime))

# 結果(s3.3.2)顯示加班對於工作與生活的平衡沒有顯著影響
s3.3.2 = summary(r3.3.2)

#-------------------- 3.4 --------------------#

# r3.4.1 是年齡對於工作與生活的平衡的影響
plot(data$Age, data$WorkLifeBalance)
r3.4.1 = lm(formula = data$WorkLifeBalance ~ data$Age)

# 結果(s3.4.1)顯示年齡對於工作與生活的平衡沒有顯著影響
s3.4.1 = summary(r3.4.1)

#-------------------- 3.5 --------------------#

# r3.5.1 是性別對於工作與生活的平衡的影響
plot(table(data$Gender, data$WorkLifeBalance))
r3.5.1 = lm(formula = data$WorkLifeBalance ~ factor(data$Gender))

# 結果(s3.5.1)顯示性別對於工作與生活的平衡沒有顯著影響
s3.5.1 = summary(r3.5.1)

#-------------------- 4 --------------------#

# r4.1.1 是性別對於工作與生活的平衡的影響
plot(data$Gender, data$Department)
plot(data$Gender, data$JobRole)
r4.1.1 = multinom(formula = data$Department ~ factor(data$Gender))
r4.1.2 = multinom(formula = data$JobRole ~ factor(data$Gender))
s4.1.1 = summary(r4.1.1)
s4.1.2 = summary(r4.1.2)

# 以下手動計算p-value幫助判斷自變數對依變數是否有顯著影響
# 結果(p4.1.1)顯示性別對工作的部門並無顯著的影響
# 結果(p4.1.2)顯示性別對製造經理有顯著(p-value<0.1)的影響，其他職位則無顯著影響
z4.1.1 <- s4.1.1$coefficients / s4.1.1$standard.errors
z4.1.2 <- s4.1.2$coefficients / s4.1.2$standard.errors
p4.1.1 <- (1 - pnorm(abs(z4.1.1), 0, 1)) * 2
p4.1.2 <- (1 - pnorm(abs(z4.1.2), 0, 1)) * 2
print(p4.1.1)
print(p4.1.2)

#-------------------- 5 --------------------#

# r5.1.1 是探討工作過的公司對於加薪的影響
plot(data$NumCompaniesWorked, data$PercentSalaryHike)
r5.1.1 = lm(formula = data$PercentSalaryHike ~ data$NumCompaniesWorked)

# 結果(s5.1.1)顯示工作過的公司對於加薪沒有顯著影響
s5.1.1 = summary(r5.1.1)

#-------------------- 相關分析 -------------------#

# 資料編碼與變數選擇
en.age = data$Age
en.att = c()
en.att[which(as.character(data$Attrition) == levels(data$Attrition)[2])] = 1
en.att[which(as.character(data$Attrition) == levels(data$Attrition)[1])] = 0
en.btrvl.set1 = c()
en.btrvl.set1[which(as.character(data$BusinessTravel) != levels(data$BusinessTravel)[1])] = 0
en.btrvl.set1[which(as.character(data$BusinessTravel) == levels(data$BusinessTravel)[1])] = 1
en.btrvl.set2 = c()
en.btrvl.set2[which(as.character(data$BusinessTravel) != levels(data$BusinessTravel)[2])] = 0
en.btrvl.set2[which(as.character(data$BusinessTravel) == levels(data$BusinessTravel)[2])] = 1
en.drate = data$DailyRate
en.dprmt.set1 = c()
en.dprmt.set1[which(as.character(data$Department) != levels(data$Department)[1])] = 0
en.dprmt.set1[which(as.character(data$Department) == levels(data$Department)[1])] = 1
en.dprmt.set2 = c()
en.dprmt.set2[which(as.character(data$Department) != levels(data$Department)[2])] = 0
en.dprmt.set2[which(as.character(data$Department) == levels(data$Department)[2])] = 1
en.fhome = data$DistanceFromHome
en.edu = data$Education
en.edufd.set1 = c()
en.edufd.set1[which(as.character(data$EducationField) != levels(data$EducationField)[1])] = 0
en.edufd.set1[which(as.character(data$EducationField) == levels(data$EducationField)[1])] = 1
en.edufd.set2 = c()
en.edufd.set2[which(as.character(data$EducationField) != levels(data$EducationField)[2])] = 0
en.edufd.set2[which(as.character(data$EducationField) == levels(data$EducationField)[2])] = 1
en.edufd.set3 = c()
en.edufd.set3[which(as.character(data$EducationField) != levels(data$EducationField)[3])] = 0
en.edufd.set3[which(as.character(data$EducationField) == levels(data$EducationField)[3])] = 1
en.edufd.set4 = c()
en.edufd.set4[which(as.character(data$EducationField) != levels(data$EducationField)[4])] = 0
en.edufd.set4[which(as.character(data$EducationField) == levels(data$EducationField)[4])] = 1
en.edufd.set5 = c()
en.edufd.set5[which(as.character(data$EducationField) != levels(data$EducationField)[5])] = 0
en.edufd.set5[which(as.character(data$EducationField) == levels(data$EducationField)[5])] = 1
en.envsa = data$EnvironmentSatisfaction
en.gen = c()
en.gen[which(as.character(data$Gender) == levels(data$Gender)[2])] = 1
en.gen[which(as.character(data$Gender) == levels(data$Gender)[1])] = 0
en.hrate = data$HourlyRate
en.jbinv = data$JobInvolvement
en.jblev = data$JobLevel
en.jbrol.set1 = c()
en.jbrol.set1[which(as.character(data$JobRole) != levels(data$JobRole)[1])] = 0
en.jbrol.set1[which(as.character(data$JobRole) == levels(data$JobRole)[1])] = 1
en.jbrol.set2 = c()
en.jbrol.set2[which(as.character(data$JobRole) != levels(data$JobRole)[2])] = 0
en.jbrol.set2[which(as.character(data$JobRole) == levels(data$JobRole)[2])] = 1
en.jbrol.set3 = c()
en.jbrol.set3[which(as.character(data$JobRole) != levels(data$JobRole)[3])] = 0
en.jbrol.set3[which(as.character(data$JobRole) == levels(data$JobRole)[3])] = 1
en.jbrol.set4 = c()
en.jbrol.set4[which(as.character(data$JobRole) != levels(data$JobRole)[4])] = 0
en.jbrol.set4[which(as.character(data$JobRole) == levels(data$JobRole)[4])] = 1
en.jbrol.set5 = c()
en.jbrol.set5[which(as.character(data$JobRole) != levels(data$JobRole)[5])] = 0
en.jbrol.set5[which(as.character(data$JobRole) == levels(data$JobRole)[5])] = 1
en.jbrol.set6 = c()
en.jbrol.set6[which(as.character(data$JobRole) != levels(data$JobRole)[6])] = 0
en.jbrol.set6[which(as.character(data$JobRole) == levels(data$JobRole)[6])] = 1
en.jbrol.set7 = c()
en.jbrol.set7[which(as.character(data$JobRole) != levels(data$JobRole)[7])] = 0
en.jbrol.set7[which(as.character(data$JobRole) == levels(data$JobRole)[7])] = 1
en.jbrol.set8 = c()
en.jbrol.set8[which(as.character(data$JobRole) != levels(data$JobRole)[8])] = 0
en.jbrol.set8[which(as.character(data$JobRole) == levels(data$JobRole)[8])] = 1
en.jbsat = data$JobSatisfaction
en.merry.set1 = c()
en.merry.set1[which(as.character(data$MaritalStatus) != levels(data$MaritalStatus)[1])] = 0
en.merry.set1[which(as.character(data$MaritalStatus) == levels(data$MaritalStatus)[1])] = 1
en.merry.set2 = c()
en.merry.set2[which(as.character(data$MaritalStatus) != levels(data$MaritalStatus)[2])] = 0
en.merry.set2[which(as.character(data$MaritalStatus) == levels(data$MaritalStatus)[2])] = 1
en.mthin = data$MonthlyIncome
en.mthrt = data$MonthlyRate
en.numcw = data$NumCompaniesWorked
en.ovrtm = c()
en.ovrtm[which(as.character(data$OverTime) == levels(data$OverTime)[2])] = 1
en.ovrtm[which(as.character(data$OverTime) == levels(data$OverTime)[1])] = 0
en.pctsh = data$PercentSalaryHike
en.pfrat = data$PerformanceRating
en.rlsat = data$RelationshipSatisfaction
en.soplv = data$StockOptionLevel
en.tolwy = data$TotalWorkingYears
en.tlsty = data$TrainingTimesLastYear
en.wlbal = data$WorkLifeBalance
en.yacom = data$YearsAtCompany
en.yicur = data$YearsInCurrentRole
en.ysltp = data$YearsSinceLastPromotion
en.ywmgr = data$YearsWithCurrManager

df.cor = cbind(en.age, en.att, en.btrvl.set1, en.btrvl.set2, en.drate, en.dprmt.set1, en.dprmt.set2, 
               en.fhome, en.edu, en.edufd.set1, en.edufd.set2, en.edufd.set3, en.edufd.set4, en.edufd.set5,
               en.envsa, en.gen, en.hrate, en.jbinv, en.jblev, en.jbrol.set1, en.jbrol.set2, en.jbrol.set3,
               en.jbrol.set4, en.jbrol.set5, en.jbrol.set6, en.jbrol.set7, en.jbrol.set8, en.jbsat,
               en.merry.set1, en.merry.set2, en.mthin, en.mthrt, en.numcw, en.ovrtm, en.pctsh, en.pfrat,
               en.rlsat, en.soplv, en.tolwy, en.tlsty, en.wlbal, en.yacom, en.yicur, en.ysltp, en.ywmgr)
View(round(cor(df.cor), 4))

#--------------------- 決策樹 --------------------#

# 隨機選擇80%的資料作為訓練資料、20%為測試資料
train.index = sample(x = 1:nrow(data), size = ceiling(0.8 * nrow(data)))
train = data[train.index, ]
test = data[-train.index, ]

# 選擇重要參數並建立模型
formula = data$Attrition ~ data$Age + data$BusinessTravel + data$DailyRate + data$Department + 
  data$DistanceFromHome + data$Education + data$EducationField + 
  data$EnvironmentSatisfaction + data$Gender + data$HourlyRate +
  data$JobInvolvement + data$JobLevel + data$JobRole + data$JobSatisfaction +
  data$MaritalStatus + data$MonthlyIncome + data$MonthlyRate +
  data$NumCompaniesWorked + data$OverTime + data$PercentSalaryHike + 
  data$PerformanceRating + data$RelationshipSatisfaction + 
  data$StockOptionLevel + data$TotalWorkingYears + data$TrainingTimesLastYear +
  data$WorkLifeBalance + data$YearsAtCompany + data$YearsInCurrentRole +
  data$YearsSinceLastPromotion + data$YearsWithCurrManager

# 使用CART演算法建立決策樹
cart.model = rpart(formula, data = train)
prp(cart.model,         # 模型
    faclen=1,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    extra=2)

# 輸出決策樹對各筆資料的機率預測結果
predict_tree = predict(cart.model, data)

# 使用條件推論樹演算法建立決策樹
CIT <- ctree(formula, data = train)
plot(CIT, type="simple")

#--------------------- 隨機森林 --------------------#

# 隨機選擇80%的資料作為訓練資料、20%為測試資料
train.index = sample(x = 1:nrow(data), size = ceiling(0.8 * nrow(data)))
train = data[train.index, ]
test = data[-train.index, ]

# 選擇重要參數並建立模型
formula = data$Attrition ~ data$Age + data$BusinessTravel + data$DailyRate + data$Department + 
  data$DistanceFromHome + data$Education + data$EducationField + 
  data$EnvironmentSatisfaction + data$Gender + data$HourlyRate +
  data$JobInvolvement + data$JobLevel + data$JobRole + data$JobSatisfaction +
  data$MaritalStatus + data$MonthlyIncome + data$MonthlyRate +
  data$NumCompaniesWorked + data$OverTime + data$PercentSalaryHike + 
  data$PerformanceRating + data$RelationshipSatisfaction + 
  data$StockOptionLevel + data$TotalWorkingYears + data$TrainingTimesLastYear +
  data$WorkLifeBalance + data$YearsAtCompany + data$YearsInCurrentRole +
  data$YearsSinceLastPromotion + data$YearsWithCurrManager

# 建立隨機森林模型，共建立500棵決策樹
RF.Attrition = randomForest(formula, data = train, importance = TRUE, proximity = TRUE
                            , ntree = 500, subset = train.index, na.action = na.fail)

importance(RF.Attrition) # 顯示各個指標對預測的重要程度
# 從MeanDecreaseGini便可看出來

#-------------------- 題目6 用logistic回歸建立預測員工是否有離職傾向的模型 --------------------#

# 隨機選擇50%的資料作為訓練資料、50%為測試資料
train.index = sample(x = 1:nrow(data), size = ceiling(0.5 * nrow(data)))
train = data[train.index, ]
test = data[-train.index, ]

# 選擇重要參數並建立模型

# 挑選上面試驗過的參數會讓p-value越小的
# 出差、婚姻、待在目前職位的時間、與管理者相處時間來建立模型
r6.1.1 = glm(train$Attrition ~ train$BusinessTravel + train$MaritalStatus + 
               train$YearsInCurrentRole + train$YearsWithCurrManager, family = binomial(link='logit'), data = train)
# 結果(s6.1.1) 
s6.1.1 = summary(r6.1.1)
print(s6.1.1)


# 挑選一般人認為會離職的重要因素來建立模型
# 出差、環境滿意度、待在目前職位的時間、工作滿意度、月收入、加班狀態、同事滿意度、工作生活平衡、
# 最後一次升遷時間、加薪
r6.1.2 = glm(train$Attrition ~ train$BusinessTravel + train$EnvironmentSatisfaction + train$YearsInCurrentRole 
             + train$JobSatisfaction + train$MonthlyIncome + train$OverTime
             + train$RelationshipSatisfaction + train$WorkLifeBalance + train$YearsSinceLastPromotion
             + train$PercentSalaryHike, family = binomial(link='logit'), data = train)

# 結果(s6.1.2)
s6.1.2 = summary(r6.1.2)
print(s6.1.2)


# 挑選我認為會影響離職的重要因素來建立模型
# 年齡、出差、環境滿意度、工作滿意度、月收入、部門、加班狀態、同事滿意度、與家的距離、工作時數、工作等級、工作職位、加薪
r6.1.3 = glm(train$Attrition ~ train$BusinessTravel + train$EnvironmentSatisfaction
             + train$JobSatisfaction + train$MonthlyIncome + train$OverTime
             + train$RelationshipSatisfaction + train$WorkLifeBalance  
             + train$Age + train$Department + train$DistanceFromHome
             + train$HourlyRate + train$JobLevel + train$JobRole + train$PercentSalaryHike, 
             family = binomial(link='logit'), data = train)

# 結果(s6.1.3)
s6.1.3 = summary(r6.1.3)
print(s6.1.3)


# 選擇重要參數並建立模型
formula = train$Attrition ~ train$Age + train$BusinessTravel + train$DailyRate + 
  train$Department + 
  train$DistanceFromHome + train$Education + train$EducationField + 
  train$EnvironmentSatisfaction + train$Gender + train$HourlyRate +
  train$JobInvolvement + train$JobLevel + train$JobRole + train$JobSatisfaction +
  train$MaritalStatus + train$MonthlyIncome + train$MonthlyRate +
  train$NumCompaniesWorked + train$OverTime + train$PercentSalaryHike + 
  train$PerformanceRating + train$RelationshipSatisfaction + 
  train$StockOptionLevel + train$TotalWorkingYears + train$TrainingTimesLastYear +
  train$WorkLifeBalance + train$YearsAtCompany + train$YearsInCurrentRole +
  train$YearsSinceLastPromotion + train$YearsWithCurrManager

# 運用stepwise 函式找出最佳模型
r6.1.4 = glm(formula = formula, family = binomial(link='logit'), data = train)
r6.1.4.stepwise = step(r6.1.4)
s6.1.4 = summary(r6.1.4.stepwise)
print(s6.1.4)



# data$Age、data$BusinessTravel、 data$DistanceFromHome、data$JobInvolvement 
# data$EducationField、data$EnvironmentSatisfaction 、data$Gender
# data$JobRole、data$JobSatisfaction、data$MaritalStatus、data$NumCompaniesWorked、
# data$OverTime、 data$RelationshipSatisfaction、data$StockOptionLevel、
# data$TotalWorkingYears、data$TrainingTimesLastYear、data$WorkLifeBalance、
# data$YearsAtCompany、data$YearsInCurrentRole、data$YearsSinceLastPromotion、
# data$YearsWithCurrManager
# 這些變數組成的模型為最佳
library(ROCR)
nrow(test)
pred1=prediction(fitted(r6.1.1), test$Attrition)
pred2=prediction(fitted(r6.1.2), test$Attrition)
pred3=prediction(fitted(r6.1.3), test$Attrition)
pred4=prediction(fitted(r6.1.4), test$Attrition)
table(true=test$Attrition, pred=round(fitted(r6.1.1)))
table(true=test$Attrition, pred=round(fitted(r6.1.2)))
table(true=test$Attrition, pred=round(fitted(r6.1.3)))
table(true=test$Attrition, pred=round(fitted(r6.1.4)))
plot(performance(pred1, "acc"))
plot(performance(pred2, "acc"))
plot(performance(pred3, "acc"))
plot(performance(pred4, "acc"))

precision1 = 
  (table(true=test$Attrition, pred=round(fitted(r6.1.1)))[1,1] +
     table(true=test$Attrition, pred=round(fitted(r6.1.1)))[2,2]) / nrow(test)
precision2 = 
  (table(true=test$Attrition, pred=round(fitted(r6.1.2)))[1,1] +
     table(true=test$Attrition, pred=round(fitted(r6.1.2)))[2,2]) / nrow(test)
precision3 = 
  (table(true=test$Attrition, pred=round(fitted(r6.1.3)))[1,1] +
     table(true=test$Attrition, pred=round(fitted(r6.1.3)))[2,2]) / nrow(test)
precision4 = 
  (table(true=test$Attrition, pred=round(fitted(r6.1.4)))[1,1] +
     table(true=test$Attrition, pred=round(fitted(r6.1.4)))[2,2]) / nrow(test)

