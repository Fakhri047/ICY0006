library(readr)
library(readr)
library(ggplot2)
library(datasets)
library(ggcorrplot)
library(dplyr)
library(ggpubr)
library(naniar)
library(MLmetrics)
library(e1071)
library(psych)
library(forecast)
library(fma)



getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
test <- read_csv("C:/Users/FRama/Desktop/R project/test.csv")
View(test)
archive <- read_csv("archive.csv")
View(archive)
describe(archive)

na.omit(archive)
vis_miss(archive)
gg_miss_upset(archive)

mean(archive$Year, trim = 0, na.rm = FALSE)
median(archive$Year, trim = 0, na.rm = FALSE)
getmode(archive$Year)
max(archive$Year)
min(archive$Year)
range(archive$Year, na.rm = FALSE)
IQR(archive$Year)
var(archive$Year)
sd(archive$Year)

mean(archive$`Carbon Dioxide (ppm)`, trim = 0, na.rm = FALSE)
median(archive$`Carbon Dioxide (ppm)`, trim = 0, na.rm = FALSE)
getmode(archive$`Carbon Dioxide (ppm)`)
max(archive$`Carbon Dioxide (ppm)`)
min(archive$`Carbon Dioxide (ppm)`)
range(archive$`Carbon Dioxide (ppm)`, na.rm = FALSE)
IQR(archive$`Carbon Dioxide (ppm)`)
var(archive$`Carbon Dioxide (ppm)`)
sd(archive$`Carbon Dioxide (ppm)`)


mean(archive$`Seasonally Adjusted CO2 (ppm)`, trim = 0, na.rm = FALSE)
median(archive$`Seasonally Adjusted CO2 (ppm)`, trim = 0, na.rm = FALSE)
getmode(archive$`Seasonally Adjusted CO2 (ppm)`)
max(archive$`Seasonally Adjusted CO2 (ppm)`, na.rm = FALSE)
min(archive$`Seasonally Adjusted CO2 (ppm)`, na.rm = FALSE)
range(archive$`Seasonally Adjusted CO2 (ppm)`, na.rm = FALSE)
IQR(archive$`Seasonally Adjusted CO2 (ppm)` , na.rm = FALSE)
var(archive$`Seasonally Adjusted CO2 (ppm)` , na.rm = FALSE)
sd(archive$`Seasonally Adjusted CO2 (ppm)`, na.rm = FALSE)

mean(archive$`Carbon Dioxide Fit (ppm)`, trim = 0, na.rm = FALSE)
median(archive$`Carbon Dioxide Fit (ppm)`, trim = 0, na.rm = FALSE)
getmode(archive$`Carbon Dioxide Fit (ppm)`)
max(archive$`Carbon Dioxide Fit (ppm)`, na.rm = FALSE)
min(archive$`Carbon Dioxide Fit (ppm)`, na.rm = FALSE)
range(archive$`Carbon Dioxide Fit (ppm)`, na.rm = FALSE)
IQR(archive$`Carbon Dioxide Fit (ppm)` , na.rm = FALSE)
var(archive$`Carbon Dioxide Fit (ppm)` , na.rm = FALSE)
sd(archive$`Carbon Dioxide Fit (ppm)`, na.rm = FALSE)

mean(archive$`Seasonally Adjusted CO2 Fit (ppm)`, trim = 0, na.rm = FALSE)
median(archive$`Seasonally Adjusted CO2 Fit (ppm)`, trim = 0, na.rm = FALSE)
getmode(archive$`Seasonally Adjusted CO2 Fit (ppm)`)
max(archive$`Seasonally Adjusted CO2 Fit (ppm)`, na.rm = FALSE)
min(archive$`Seasonally Adjusted CO2 Fit (ppm)`, na.rm = FALSE)
range(archive$`Seasonally Adjusted CO2 Fit (ppm)`, na.rm = FALSE)
IQR(archive$`Seasonally Adjusted CO2 Fit (ppm)` , na.rm = FALSE)
var(archive$`Seasonally Adjusted CO2 Fit (ppm)` , na.rm = FALSE)
sd(archive$`Seasonally Adjusted CO2 Fit (ppm)`, na.rm = FALSE)


Decimal_Date <- archive$`Decimal Date`
hist(Decimal_Date )

Carbon_Dioxide_ppm <- archive$`Carbon Dioxide (ppm)`
hist(Carbon_Dioxide_ppm)

Seasonally_Adjusted_Co2 <- archive$`Seasonally Adjusted CO2 (ppm)`
hist(Seasonally_Adjusted_Co2)

Carbon_Dioxide_Fit<- archive$`Carbon Dioxide Fit (ppm)`
hist(Carbon_Dioxide_Fit)

CO2_Fit <- archive$`Seasonally Adjusted CO2 Fit (ppm)`
hist(CO2_Fit)




boxplot(archive$`Carbon Dioxide (ppm)`)
boxplot(archive$`Seasonally Adjusted CO2 (ppm)`)
boxplot(archive$`Carbon Dioxide Fit (ppm)`)
boxplot(archive$`Seasonally Adjusted CO2 Fit (ppm)`)



p1 <- ggplot() + geom_line(aes(y = "Year", x = "Carbon Dioxide (ppm)"), data = archive)
p1
str(archive)
plot(archive$"Year",archive$"Carbon Dioxide (ppm)")

p1 <- ggplot() + geom_line(aes(y = "Year", x = "Seasonally Adjusted CO2 (ppm)"), data = archive)
p1
str(archive)
plot(archive$"Year",archive$"Seasonally Adjusted CO2 (ppm)")


onlynums <- archive[, unlist(lapply(archive, is.double))]

myval <- archive %>% select("Year", "Carbon Dioxide (ppm)")
cor(myval)
corr <- cor(myval)
ggcorrplot(corr)

corr <- round (cor(onlynums), 5)
ggcorrplot(corr)

cormat <- round(cor(archive),2)
p.mat <- cor_pmat(archive)
ggcorrplot(cormat,outline.col = "white",  hc.order = TRUE,
           type = "full",lab = TRUE,p.mat = p.mat)

ggscatter(archive, x = "Year", y = "Carbon Dioxide (ppm)",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab ="Year", ylab = "Carbon Dioxide (ppm)")

ggscatter(archive, x = "Year", y = "Carbon Dioxide Fit (ppm)",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab ="Year", ylab = "Carbon Dioxide Fit (ppm)")

ggscatter(archive, x = "Year", y = "Seasonally Adjusted CO2 (ppm)",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab ="Year", ylab = "Seasonally Adjusted CO2 (ppm)")

ggscatter(archive, x = "Year", y = "Seasonally Adjusted CO2 Fit (ppm)",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab ="Year", ylab = "Seasonally Adjusted CO2 Fit (ppm)")

ggscatter(archive, x = "Carbon Dioxide (ppm)", y = "Carbon Dioxide Fit (ppm)",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab ="Carbon Dioxide (ppm)", ylab = "Carbon Dioxide Fit (ppm)")

ggscatter(archive, x = "Seasonally Adjusted CO2 (ppm)", y = "Seasonally Adjusted CO2 Fit (ppm)",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab ="Seasonally Adjusted CO2 (ppm)", ylab = "Seasonally Adjusted CO2 Fit (ppm)")






plot(archive$`Year`,archive$`Carbon Dioxide (ppm)`,  xlab="Year", ylab="Carbon Dioxide (ppm)", pch=21)
linear.model<-lm(archive$`Carbon Dioxide (ppm)`~archive$Year)
abline(linear.model)
cor(archive$`Year`,archive$`Carbon Dioxide (ppm)`)

plot(archive$`Year`,archive$`Seasonally Adjusted CO2 (ppm)`,  xlab="Year", ylab="Seasonally Adjusted CO2 (ppm)", pch=21)
linear.model<-lm(archive$`Seasonally Adjusted CO2 (ppm)`~archive$Year)
abline(linear.model)
cor(archive$`Year`,archive$`Seasonally Adjusted CO2 (ppm)`)

plot(archive$`Year`,archive$`Seasonally Adjusted CO2 Fit (ppm)`,  xlab="Year", ylab="Seasonally Adjusted CO2 Fit (ppm)", pch=21)
linear.model<-lm(archive$`Year`~archive$`Seasonally Adjusted CO2 Fit (ppm)`)
abline(linear.model)
cor(archive$`Year`,archive$`Seasonally Adjusted CO2 Fit (ppm)`)

plot(archive$`Carbon Dioxide (ppm)`,archive$`Carbon Dioxide Fit (ppm)`,  xlab="Carbon Dioxide (ppm)", ylab="Carbon Dioxide Fit (ppm)", pch=21)
linear.model<-lm(archive$`Carbon Dioxide (ppm)`~archive$`Carbon Dioxide Fit (ppm)`)
abline(linear.model)
cor(archive$`Carbon Dioxide (ppm)`,archive$`Carbon Dioxide Fit (ppm)`)









prediction.linear <- predict (linear.model, archive)
paste0("linear model: ",
       MAPE(prediction.linear,archive$`Carbon Dioxide (ppm)`))

svm.model<-svm(archive$`Year` ~ archive$`Carbon Dioxide (ppm)`, data = archive)
prediction.svm <- predict(svm.model, archive$`Carbon Dioxide (ppm)`)
paste0("svm model: ",
       MAPE(prediction.svm,archive$Year))

plot( archive$`Carbon Dioxide (ppm)`,archive$`Year`, xlab="Carbon Dioxide (ppm)", ylab="Year", pch=21)
points (archive$`Carbon Dioxide (ppm)`,
        prediction.linear,
        col = "blue",
        pch = 16)
points(archive$`Carbon Dioxide (ppm)`,
       prediction.svm,
       col = "red",
       pch = 16)




tCarbon_Dioxide <- test$"Carbon Dioxide (ppm)"
tSeasonally_Adjusted <- test$"Seasonally Adjusted CO2 (ppm)"
tYear <- test$`Decimal Date`

yearCarbon.lm<-lm(tCarbon_Dioxide~tYear)
yearSeason.lm<-lm(tSeasonally_Adjusted~tYear)

yearCarbon.prediction.lm <- predict (yearCarbon.lm, test)
paste0("yearCarbon linear model: ",
       MAPE(yearCarbon.prediction.lm,tCarbon_Dioxide))

yearCarbon.svm<-svm(tCarbon_Dioxide ~ tYear, data = test)
yearCarbon.prediction.svm <- predict(yearCarbon.svm, tYear)
paste0("yearCarbon svm model: ",
       MAPE(yearCarbon.prediction.svm,tCarbon_Dioxide))

yearSeason.prediction.lm <- predict (yearSeason.lm, test)
paste0("yearSeason linear model: ",
       MAPE(yearSeason.prediction.lm,tSeasonally_Adjusted))

yearSeason.svm<-svm(tSeasonally_Adjusted ~ tYear, data = test)
yearSeason.prediction.svm <- predict(yearSeason.svm, tYear)
paste0("yearSeason svm model: ",
       MAPE(yearSeason.prediction.svm,tSeasonally_Adjusted))




plot( tYear,tCarbon_Dioxide, xlab="Year&Month", ylab="Carbon Dioxide (ppm)", pch=21)
points (tYear,
        yearCarbon.prediction.lm,
        col = "blue",
        pch = 16)
points(tYear,
       yearCarbon.prediction.svm,
       col = "red",
       pch = 16)
abline(lm(archive$`Carbon Dioxide (ppm)` ~ archive$Year , data = archive), col = "green")

plot( tYear,tSeasonally_Adjusted, xlab="Year&Month", ylab="Seasonally Adjusted CO2 (ppm)", pch=21)
points (tYear,
        yearSeason.prediction.lm,
        col = "blue",
        pch = 16)
points(tYear,
       yearSeason.prediction.svm,
       col = "red",
       pch = 16)
abline(lm(archive$`Seasonally Adjusted CO2 (ppm)` ~ archive$Year , data = archive), col = "green")




