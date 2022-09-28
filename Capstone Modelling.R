---
title: "R Notebook"
output: html_notebook
---

  
Load the files directly from https://data.sa.gov.au/data/api/3/action/package_list

```{r}

CrimeStatistics2017_18 <- read.csv(file = "https://data.sa.gov.au/data/dataset/860126f7-eeb5-4fbc-be44-069aa0467d11/resource/bf604730-9ec8-44dd-88a3-f024b387e0e4/download/2017-18-data_sa_crime.csv", header = TRUE)

CrimeStatistics2018_19 <- read.csv(file = "https://data.sa.gov.au/data/dataset/860126f7-eeb5-4fbc-be44-069aa0467d11/resource/809b11dd-9944-4406-a0c4-af6a5b332177/download/2018-19-data_sa_crime.csv", header = TRUE)

CrimeStatistics2019_20 <- read.csv(file = "https://data.sa.gov.au/data/dataset/860126f7-eeb5-4fbc-be44-069aa0467d11/resource/590083cd-be2f-4a6c-871e-0ec4c717717b/download/2019-20-fullyr-data_sa_crime.csv", header = TRUE)

CrimeStatistics2020_21 <- read.csv(file = "https://data.sa.gov.au/data/dataset/860126f7-eeb5-4fbc-be44-069aa0467d11/resource/e0b8d120-65e3-44e6-9e2e-aeb7aec59114/download/2020-21_data_sa_crime.csv", header = TRUE)

CrimeStatistics2021_22 <- read.csv(file = "https://data.sa.gov.au/data/dataset/860126f7-eeb5-4fbc-be44-069aa0467d11/resource/97524896-b976-4ed1-a26b-70bdc8248832/download/2021-22-data-sa-crime-q1-q4.csv", header = TRUE)

```
Combine the Crime Statistics

```{r}
data <-rbind( CrimeStatistics2017_18,CrimeStatistics2018_19, CrimeStatistics2019_20,
             CrimeStatistics2020_21, CrimeStatistics2021_22
)
data$Reported.Date <- as.Date(data$Reported.Date, format = "%d/%m/%Y")
head(data)
```
Enrich the data:
Add date hierarchy


```{r}

data$day <-format(data$Reported.Date, format ="%d")
data$month <-format(data$Reported.Date, format ="%m")
data$year <-format(data$Reported.Date, format ="%Y")
data$wday <-weekdays(data$Reported.Date)
```
Add holiday information


```{r}
adl_holidays <- rbind(
                      (read.csv(file= 
                                 ,header = TRUE),
                      (read.csv(file= 
                                 ,header = TRUE),
                       (read.csv(file= 
                                 ,header = TRUE),
                        (read.csv(file= 
                                 ,header = TRUE),
                         (read.csv(file= 
                                 ,header = TRUE),
                          (read.csv(file= 
                                 ,header = TRUE),
                           (read.csv(file= 
                                 ,header = TRUE),
```
Linear Model 

```{r}
numRows <- nrow(data)
set.seed(57) 
train <- sample(numRows, 0.8*numRows)
dataTrain <- data[train, ]
dataTest <- data[-train,]

```

Linear modelling Linear.Model.01

```{r}
Linear.Model.01 <- lm(
  formula = YearsAtCompany ~ .,
  data = dataTrain
)
summary(Linear.Model.01)

```
Use leaps package to work out which combinations best explain target

```{r}
library(leaps)
bestSubset <- regsubsets(
  YearsAtCompany ~ .,
  data = dataTrain,
  nvmax = 24 #number of dependant variables
)
summary(bestSubset)
```

```{r}
coef(bestSubset, 1:24)
```

```{r}
summary(bestSubset)$bic # Get all of the adjusted R^2 values
```
Judging which model is best there are different metrics

```{r}
which.max(summary(bestSubset)$adjr2)
which.min(summary(bestSubset)$rss)
which.min(summary(bestSubset)$cp)
which.min(summary(bestSubset)$bic)
```
Plotting the best(bic, for example) measure

```{r}
plot(
  summary(bestSubset)$bic,
  type = "l",
  main = "Figure 1: Number variables by BIC",
  xlab = "Number of Variables",
  ylab = "BIC",
)

bestBIC = which.min(summary(bestSubset)$bic)
points(    #showing the point with lowest bic for example
  x = bestBIC,
  y = summary(bestSubset)$bic[bestBIC],
  col = "red",
  pch = "x"
)
```

```{r}
coef(bestSubset, 5)
coef(bestSubset, 17)
coef(bestSubset, 22)
```
```{r}
#linear models deemed by by regsubsets, bic and c scores
#best 5
Linear.Model.02 <- lm(
  formula =  ,
  data = dataTrain,
  
)

#best 17
Linear.Model.03 <- lm(
  formula = ,
  data = dataTrain,
  
)

#Best 22

Linear.Model.04 <- lm(
  formula = ,
  data = dataTrain,
  
)



```
```{r}
#final test on linear models deemed by by regsubsets, bic and c scores
dataTest$PredictionYears.lm02 <- predict(
  Linear.Model.02,
  newdata = dataTest,
  type = "response"
)
dataTest$PredictionYears.lm03 <- predict(
  Linear.Model.03,
  newdata = dataTest,
  type = "response"
)

dataTest$PredictionYears.lm02 <- predict(
  Linear.Model.02,
  newdata = dataTest,
  type = "response"
)
dataTest$PredictionYears.lm04 <- predict(
  Linear.Model.04,
  newdata = dataTest,
  type = "response"
)

mean((dataTest$YearsAtCompany - dataTest$PredictionYears.lm02)^2) #least so it is best
mean((dataTest$YearsAtCompany - dataTest$PredictionYears.lm03)^2)
mean((dataTest$YearsAtCompany - dataTest$PredictionYears.lm04)^2)

#what about on train data

dataTrain$PredictionYears.lm02 <- predict(
  Linear.Model.02,
  newdata = dataTrain,
  type = "response"
)
dataTrain$PredictionYears.lm03 <- predict(
  Linear.Model.03,
  newdata = dataTrain,
  type = "response"
)
summary(Linear.Model.02)
mean((dataTrain$YearsAtCompany - dataTrain$PredictionYears.lm02)^2) #least so it is best
mean((dataTrain$YearsAtCompany - dataTrain$PredictionYears.lm03)^2)

```

```{r}
#checking assumptions of linearility
par(mfrow = c(1,3))
plot(Linear.Model.02$residuals, col = "blue", ylab = "Residuals")
hist(Linear.Model.02$residuals, xlab = "Residulas", main = "Residual plots")
boxplot(Linear.Model.02$residuals)
```


Logistic modelling


```{r}
#gml model with all variables 
gLinear.Model.05 <- glm(
  IsAttrition ~   ,
  data = dataTrain,
  family = binomial,
  control = list(maxit = 27)
)

summary(gLinear.Model.05)

gLinear.Model.06 <- glm(
  IsAttrition ~ ,
  data = dataTrain,
  family = binomial,
  control = list(maxit = 27)
)
summary(gLinear.Model.06)

```



```{r}

```











