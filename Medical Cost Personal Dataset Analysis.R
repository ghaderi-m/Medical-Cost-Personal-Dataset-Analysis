#install.packages('tinytex')
#install.packages("Dplyr")
#install.packages("ggforce")
#install.packages("ggcorrplot")
# install.packages("cowplot")
# install.packages("psych" , dependencies = TRUE)
library(readr)
library(dplyr)
library(caret)
library(psych)
library(tidyverse)
library(patchwork)
library(ggforce)
library(cowplot)
library(ggplot2)
library(cowplot)
library(ggcorrplot)
library(leaps) #for computing best subsets regression
library(broom)
library(modelr)

# install.packages("tinytex")
library(tinytex)
# tinytex::install_tinytex()




# Importing the Dataset

insurance <- read_csv("D:/_UniPD/Semster 2/Statistical Learning B/PROJECT/Dataset/insurance.csv")
summary(insurance)

# converting categorical variables to factors

insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)


summary(insurance)
str(insurance)
attach(insurance)


# Checking for duplicated rows

cat("number of duplicated rows = ",sum(duplicated(insurance)))

# Removing the duplicated row

insurance <- distinct(insurance)
dim(insurance)

# Checking for null values

cat("number of null values = ", sum(is.na(insurance)))

summary(insurance)


#=-=-=-=-=-==-=-=-=-=-=-=-=-#
# Exploratory Data Analysis #
#=-=-=-=-=-=-=-=-=-=-=-=-=-=#

## Boxplots ##

ggplot(data = insurance,aes(sex,charges)) + geom_boxplot(fill = c(6,4)) +
  theme_bw() + ggtitle("charges per sex")

ggplot(data = insurance,aes(smoker,charges)) + geom_boxplot(fill = c(3,2)) +
  theme_bw() + ggtitle("charges per smoker")

ggplot(data = insurance,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_bw() + ggtitle("charges per children") +
  xlab("Num of Children")

ggplot(data = insurance,aes(region,charges)) + geom_boxplot(fill = c(3:6)) +
  theme_bw() + ggtitle("charges per region")


## Histograms and Density plots ##

par(mfrow = c(1,3))

hist(charges, prob = TRUE,  col = "yellow", main = "Distribution density of charges",
     xlab="(charges)")
lines(density(charges), col="red", lwd = 3)

hist(age, prob = TRUE, col = "lightgreen", main = "Distribution density of age",
     xlab="Age")
lines(density(age), col="red", lwd = 3)

hist(bmi,prob = TRUE, col = "orange", main = "Distribution density of BMI",
     xlab="BMI")
lines(density(bmi), col="red", lwd = 3)

par(mfrow = c(1,1))


par(mfrow = c(1,4))

barplot(height = table(children),col = "pink",  main = "Distribution of children",
        xlab="No.of children")

barplot(height = table(smoker), col="red", main = "Distribution of Smoking",
        xlab = "Smoker")

barplot(height = table(sex), col="blue", main = "Distribution of Sex",
        xlab = "Sex")

barplot(height = table(region), col="purple", main = "Distribution of Region",
        xlab='Region')

par(mfrow = c(1,1))

# charges for smoker and non-smoker based on sex

ggplot(insurance, aes(x = smoker, y = charges, fill = sex)) +
  geom_col(position = "dodge")


## Scatter plots ##

# Interaction between bmi, age and smoker and their effect on charges

ggplot(insurance, aes(age, charges, color = smoker)) + 
  geom_point()

ggplot(insurance, aes(age, charges, color = sex)) + 
  geom_point()

ggplot(insurance, aes(bmi, charges, color = smoker)) + 
  geom_point()

ggplot(insurance, aes(bmi, charges, color = sex)) + 
  geom_point()


#=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-#
# Correlation between variables #
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# Converting variables to numeric first

ins <- insurance

ins$sex <- as.factor(ins$sex)
ins$smoker <- as.factor(ins$smoker)
ins$region <- as.factor(ins$region)
ins$children <- as.factor(ins$children)

ins$sex <- as.numeric(ins$sex)
ins$smoker <- as.numeric(ins$smoker)
ins$region <- as.numeric(ins$region)
ins$children <- as.numeric(ins$children)

cor(ins)
corr <- round(cor(ins), 3)

ggcorrplot(corr, type = "lower", lab = TRUE, outline.color = "black",
           lab_size = 5, legend.title = "Correlation")


#=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# introdicuing the new variable "obese" #
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

insurance$obese <- ifelse(insurance$bmi >= 30 , "yes" , "no")



#=-=-=-=-=-=-=-=-=-=#
# Regression Models #
#=-=-=-=-=-=-=-=-=-=#

# Splitting data into Train and Test

set.seed(134)
partition <- floor(0.75*nrow(insurance))
train.numbers <- sample(seq_len(nrow(insurance)), partition, replace = FALSE)
train <- insurance[train.numbers, ]
test <- insurance[-train.numbers, ]

# MODEL 1 (Full Model)
model.1 <- lm(charges ~. , data = train)
summary(model.1)
BIC(model.1)

par(mfrow = c(2,2))
plot(model.1)
par(mfrow = c(1,1))

## Metrics for MODEL 1
glance(model.1) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

data.frame(
  R2 = rsquare(model.1, data = train),
  RMSE = rmse(model.1, data = train),
  MAE = mae(model.1, data = train)
)


# Transformation of the response variable
##log transformation

# MODEL 2 (Full Model_Log Transformation)
model.2 <- lm(log(charges) ~. , data = train)
summary(model.2)
BIC(model.2)

par(mfrow = c(2,2))
plot(model.2)
par(mfrow = c(1,1))

## Metrics for MODEL 2
glance(model.2) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

data.frame(
  R2 = rsquare(model.2, data = insurance),
  RMSE = rmse(model.2, data = insurance),
  MAE = mae(model.2, data = insurance)
)

# Check for another transformation (square root)

# MODEL 3 (Full Model_ Transformation (charges)^(0.5))
model.3 <- lm((charges)^(1/2) ~. , data = train)
summary(model.3)
BIC(model.3)

par(mfrow = c(2,2))
plot(model.3)
par(mfrow = c(1,1))

## Metrics for MODEL 3
glance(model.3) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

data.frame(
  R2 = rsquare(model.3, data = insurance),
  RMSE = rmse(model.3, data = insurance),
  MAE = mae(model.3, data = insurance)
)


# MODEL 4 (Full Model_Polynomial)
model.4 <- lm(log(charges) ~. + poly(age , 2) , data = train)
summary(model.4)
BIC(model.4)

par(mfrow = c(2,2))
plot(model.4)
par(mfrow = c(1,1))

## Metrics for MODEL 4
glance(model.4) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

data.frame(
  R2 = rsquare(model.4, data = insurance),
  RMSE = rmse(model.4, data = insurance),
  MAE = mae(model.4, data = insurance)
)


# Variable Selection Stepwise Backward Elimination #

regfit.bwd <- regsubsets(charges ~.  , data = train, nvmax=7, method="backward")
bwd.summary <- summary(regfit.bwd)
plot(regfit.bwd, scale="bic")

## Looking at the Outmat Matrix 
bwd.summary$outmat
bwd.summary$bic
which.min(bwd.summary$bic)

# MODEL 5 Reduced Model Based on selected variables by Backward Elimination
model.5 <- lm(log(charges) ~ age + children + smoker + obese , data = train)
summary(model.5)
BIC(model.5)

par(mfrow = c(2,2))
plot(model.5)
par(mfrow = c(1,1))

# Metrics for MODEL 5
glance(model.5) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

data.frame(
  R2 = rsquare(model.5, data = insurance),
  RMSE = rmse(model.5, data = insurance),
  MAE = mae(model.5, data = insurance)
)


# Including interaction effect of smoker*obese #

# MODEL 6 Reduced Model Based on Backward Elimination including 
# interaction of smoker*age and smoker*obese
model.6 <- lm(log(charges) ~ age + children + smoker*age + smoker*obese , data = train)
summary(model.6)
BIC(model.6)

par(mfrow = c(2,2))
plot(model.6)
par(mfrow = c(1,1))

# Metrics for model 6
glance(model.6) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

data.frame(
  R2 = rsquare(model.6, data = insurance),
  RMSE = rmse(model.6, data = insurance),
  MAE = mae(model.6, data = insurance)
)


# MODEL 7 Concave transformation of the response variable with smoker*obese 
model.7 <- lm(charges/mean(charges) ~ age + children + smoker*obese , data = train)
summary(model.7)
BIC(model.7)

par(mfrow = c(2,2))
plot(model.7)

glance(model.7) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

data.frame(
  R2 = rsquare(model.7, data = train),
  RMSE = rmse(model.7, data = train),
  MAE = mae(model.7, data = train)
)
# Predicted Values vs Actual Values

test$predicted <- predict(model.7, newdata = test)

predicted.values <- (test$predicted) * mean(charges)

test %>%
  ggplot(aes(x = predicted.values , y = charges)) +
  geom_point(shape = "triangle", color = "blue") +
  geom_abline(color = "red") +
  ggtitle("Predicted Values vs.  Actual Values")

