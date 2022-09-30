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
rm(CrimeStatistics2017_18,CrimeStatistics2018_19, CrimeStatistics2019_20,
             CrimeStatistics2020_21, CrimeStatistics2021_22)
data$Reported.Date <- as.Date(data$Reported.Date, format = "%d/%m/%Y")
names(data)[2] <- 'Suburb'
names(data)[3] <- 'Postcode'
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
holidays21_23 <- read.csv(file ="https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/d256f989-8f49-46eb-9770-1c6ee9bd2661/download/australian_public_holidays_2023.csv", header = TRUE)

holidays20 <- read.csv(file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/c4163dc4-4f5a-4cae-b787-43ef0fcf8d8b/download/australian_public_holidays_2020.csv", header = TRUE)

holidays19 <- read.csv(file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/bda4d4f2-7fde-4bfc-8a23-a6eefc8cef80/download/australian_public_holidays_2019.csv", header = TRUE)
holidays19 <- holidays19[-1 ]

holidays18 <- read.csv(file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/253d63c0-af1f-4f4c-b8d5-eb9d9b1d46ab/download/australianpublicholidays-201718.csv", header = TRUE)
names(holidays18)[5] <- 'Jurisdiction'

holidays17 <- read.csv(file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/a24ecaf2-044a-4e66-989c-eacc81ded62f/download/australianpublicholidays-201617.csv", header = TRUE)
names(holidays17)[5] <- 'Jurisdiction'

#Filter for SA holidays only

sa_holidays <- rbind(holidays17,holidays18,holidays19,holidays20,holidays21_23)
rm(holidays17,holidays18,holidays19,holidays20,holidays21_23) #to clear memory
sa_holidays <-sa_holidays[sa_holidays$Jurisdiction == "SA",]
                     
#Format dates in SA holidays 

sa_holidays$Date <- as.Date(as.character(sa_holidays$Date), format = "%Y%m%d")
```
Adding the above holiday information
```{r}
v_sa_holidays <- sa_holidays$Date #to convert to vector
data$IsHoliday <- ifelse(data$Reported.Date %in% v_sa_holidays,1,0)

```


Linear Model 

```{r}
library(car)
names(data)[2] <- 'Suburb'
names(data)[3] <- 'Postcode'
numRows <- nrow(data)
set.seed(57) 
train <- sample(numRows, 0.8*numRows)
dataTrain <- data[train, ]
dataTest <- data[-train,]

```

Linear modelling Linear.Model.00 and 01

```{r}
#Train model 00
Linear.Model.00 <- lm(
  formula = Offence.count ~ Reported.Date + Postcode + Offence.Level.3.Description,
  data = dataTrain
)
```
```{r}
summary(Linear.Model.00)
#check for Multicollinearity

vif(Linear.Model.00)
```


```{r}
#Train model 01

Linear.Model.01 <- lm(
  formula = Offence.count ~Postcode +Offence.Level.3.Description+  day +month  +year +wday+ IsHoliday,
  data = dataTrain
)
summary(Linear.Model.01)
#check for Multicollinearity
vif(Linear.Model.01)
```

```{r}
#To add new factors in testData
Linear.Model.01$xlevels[["Offence.Level.3.Description"]] <-union( Linear.Model.01$xlevels[["Offence.Level.3.Description"]],c("Aggravated sexual assault", "Non-aggravated sexual assault", "Non-assaultive sexual offences"))

Linear.Model.01$xlevels[["Postcode"]] <-union( Linear.Model.01$xlevels[["Postcode"]],c("2044", "2228", "2469", "2481", "2486", "2762", "3001", "3109", "3125", "3136", "3180", "3840", "5768", "7109"))

```

Testing the model using held-out dataset
```{r}

dataTest$Prediction.lm01 <- predict(
  Linear.Model.01,
  newdata = dataTest, 
  type = "response"
)
```

```{r}
rm(Linear.Model.01)
Linear.Model.00$xlevels[["Offence.Level.3.Description"]] <-union( Linear.Model.00$xlevels[["Offence.Level.3.Description"]],c("Aggravated sexual assault", "Non-aggravated sexual assault", "Non-assaultive sexual offences"))

Linear.Model.00$xlevels[["Postcode"]] <-union( Linear.Model.00$xlevels[["Postcode"]],c("2044", "2228", "2469", "2481", "2486", "2762", "3001", "3109", "3125", "3136", "3180", "3840", "5768", "7109"))

dataTest$Prediction.lm00 <- predict(
  Linear.Model.00,
  newdata = dataTest, 
  type = "response"
)
```


```{r}
#checking assumptions of linearility
par(mfrow = c(1,3))
plot(Linear.Model.00$residuals, col = "blue", ylab = "Residuals")
hist(Linear.Model.00$residuals, xlab = "Residulas", main = "Residual plots")
boxplot(Linear.Model.00$residuals)
```



```{r}
#check the RSS-residual sum of squares, the lower the better
mean((dataTest$Offence.count - dataTest$Prediction.lm01)^2)

mean((dataTest$Offence.count - dataTest$Prediction.lm00)^2)
```








