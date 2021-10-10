#libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tseries)
library(tidyverse)
library(readxl)
library(e1071)
library(hrbrthemes)
library(caret)
library(np)
library(MLmetrics)
library(caret)
library(datarium)


#import data
my_data <- as.data.frame(read_excel("Data.xlsx")) 

#Data description
#removing outliers
index_oulier_expert <- which(my_data$entities >= 1490) #collects outliers from variable entities
my_data <- my_data[-index_oulier_expert, ] #deleting stored outliers from the data frame

index_oulier_expert <- which(my_data$roads >= 170) #collects outliers on a variable roads
my_data <- my_data[-index_oulier_expert, ] #deleting stored outliers from the data frame

index_oulier_expert <- which(my_data$population >= 165) #collects outliers on a variable population
my_data <- my_data[-index_oulier_expert, ] #deleting stored outliers from the data frame

index_oulier_expert <- which(my_data$crimes >= 2500) #collects outliers on a variable crimes
my_data <- my_data[-index_oulier_expert, ] #deleting stored outliers from the data frame

#box plots of variables
my_data %>%
  ggplot( aes(y = entities)) +
  geom_boxplot(fill="aquamarine") +
  ggtitle("Box Plot for New Registered Private Sector Entities") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Number of new entities")

my_data %>%
  ggplot( aes(y = roads )) +
  geom_boxplot(fill="coral1") +
  ggtitle("Box plot for country hard surface roads per 100 km2") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Paved roads per 100 km2")

my_data %>%
  ggplot( aes(y = population )) +
  geom_boxplot(fill="chartreuse1") +
  ggtitle("Box plot for the population index per km2") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Population rate per 1 km2")

my_data %>%
  ggplot( aes(y = crimes )) +
  geom_boxplot(fill="darkgoldenrod1") +
  ggtitle("A box plot for the number of reported crimes") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Number of crimes identified")

#Basic variable statistics
summary(my_data)
#skewness
skewness(my_data$entities)
skewness(my_data$roads)
skewness(my_data$population)
skewness(my_data$crimes)
#kurtosis
kurtosis(my_data$entities)
kurtosis(my_data$roads)
kurtosis(my_data$population)
kurtosis(my_data$crimes)

#normal distributiontesting
#variable histograms
my_data %>%
  ggplot( aes(x = entities)) +
  geom_histogram( binwidth=100, fill="aquamarine", color="#69b3a2", alpha=0.9) +
  ggtitle("Histogram for new registered private sector entities") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of new registered entities") +
  ylab("Frequency")

my_data %>%
  ggplot( aes(x = roads)) +
  geom_histogram( binwidth=10, fill="coral1", color="#FF81C9", alpha=0.9) +
  ggtitle("Histogram for country roads with hard surface per 100 km2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Index of poviat roads") +
  ylab("Frequency")

my_data %>%
  ggplot( aes(x = population)) +
  geom_histogram( binwidth=10, fill="darkolivegreen3", color="darkolivegreen4", alpha=0.9) +
  ggtitle("Histogram for the index of population per km2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Population index") +
  ylab("Frequency")

my_data %>%
  ggplot( aes(x = crimes)) +
  geom_histogram( binwidth=200, fill = "#FFDB6D", color = "#C4961A", alpha=0.9) +
  ggtitle("Histogram for the number of identified crimes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of crimes identified") +
  ylab("Frequency")

# Shapiro-Wilk normality tests
shapiro.test(my_data$entities)
shapiro.test(my_data$roads)
shapiro.test(my_data$population)
shapiro.test(my_data$crimes)

#The plot of the distribution density estimators for 3 selected nuclei for the explained variable
kernels <-  c("gaussian", "rectangular", "triangular")

colors <- c("#c7162b", "#26C662", "#f99b11")

ggplot(my_data, aes(x = entities)) + 
  geom_density(kernel = kernels[1], aes(color="#c7162b"), lwd = 1.5) + 
  geom_density(kernel = kernels[2], aes(color = "#26C662"), lwd = 1.5) + 
  geom_density(kernel = kernels[3], aes(color = "#f99b11"), lwd = 1.5) + 
  labs( x="New registered entities from the private sector", y="Frequency", title="Plot of distribution density estimators") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_identity(name = "",breaks = colors, labels = kernels, guide = "legend")

#Investigation of the relationship between the explanatory and explanatory variable
ggplot(my_data, aes(x = roads, y = entities)) +
  geom_point(color = "coral1") +
  theme_ipsum() +
  labs( x="Index of country roads with hard surface per 100 km2", y="New registered entities from the private sector") +
  theme_minimal() + theme(text = element_text(size = 13))

ggplot(my_data, aes(x = population, y = entities)) +
  geom_point(color = "green") +
  theme_ipsum() +
  labs( x="The indicator of the population index per 1 km2", y="New registered entities from the private sector") +
  theme_minimal() + theme(text = element_text(size = 13))

ggplot(my_data, aes(x = crimes, y = entities)) +
  geom_point(color = "orange") +
  theme_ipsum() +
  labs( x="Number of crimes identified", y="New registered entities from the private sector") +
  theme_minimal() + theme(text = element_text(size = 13))
  
#Pearson correlation
cor(my_data$entities, my_data$roads)
cor(my_data$entities, my_data$population)
cor(my_data$entities, my_data$crimes)

#Linear regression - estimation using the LSM method
#printing
lmodel1 <- lm( entities ~ roads, data = my_data)  
print(lmodel1)
summary(lmodel1)
#logarithmic variable
lmodel1_ln  <- lm(log10(entities) ~ log10(roads), data = my_data)
print(lmodel1_ln)
summary(lmodel1_ln)

lmodel2 <- lm(entities ~ population, data = my_data)  
print(lmodel2)
summary(lmodel2)
#logarithmic variable
lmodel2_ln  <- lm(log10(entities) ~ log10(population), data = my_data)
print(lmodel2_ln)
summary(lmodel2_ln)

lmodel3 <- lm(entities ~ crimes, data = my_data)  
print(lmodel3)
summary(lmodel3)
#logarithmic variable
lmodel3_ln  <- lm(log10(entities) ~ log10(crimes), data = my_data)
print(lmodel3_ln)
summary(lmodel3_ln)

#visualizations
my_data %>%
  ggplot(aes(x = roads, y = entities)) +
  geom_point() +
  geom_smooth(method=lm , color="coral1", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  labs( x="Index of country roads with hard surface per 100 km2", y="New registered entities from the private sector") +
  theme_minimal() + theme(text = element_text(size = 13))

my_data %>%
  ggplot(aes(x = population, y = entities)) +
  geom_point() +
  geom_smooth(method=lm , color="Green", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  labs( x="The indicator of the population index per 1 km2", y="New registered entities from the private sector") +
  theme_minimal() + theme(text = element_text(size = 13))

my_data %>%
  ggplot(aes(x = crimes, y = entities)) +
  geom_point() +
  geom_smooth(method=lm , color="Orange", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  labs( x="Number of crimes identified", y="New registered entities from the private sector") +
  theme_minimal() + theme(text = element_text(size = 13))

my_data %>%
  ggplot(aes(x = log10(roads), y = log10(entities))) +
  geom_point() +
  geom_smooth(method=lm , color="coral1", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  labs( x="Logarithmized Index of poviat hard surface roads per 100 km2", y="Logarithmized number of new registered entities") +
  theme_minimal() + theme(text = element_text(size = 13))

my_data %>%
  ggplot(aes(x = log10(population), y = log10(entities))) +
  geom_point() +
  geom_smooth(method=lm , color="Green", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  labs( x="Logarithmized index of population per 1 km2", y="Logarithmized number of new registered entities") +
  theme_minimal() + theme(text = element_text(size = 13))

my_data %>%
  ggplot(aes(x = log10(crimes), y = log10(entities))) +
  geom_point() +
  geom_smooth(method=lm , color="Orange", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  labs( x="Logarithmized number of identified crimes", y="Logarithmized number of new registered entities") +
  theme_minimal() + theme(text = element_text(size = 13))

#correlations
cor(log10(my_data$roads), log10(my_data$entities))
cor(log10(my_data$population), log10(my_data$entities))
cor(log10(my_data$crimes), log10(my_data$entities))

#errors
RMSE(lmodel1_ln$fitted.values,log10(my_data$entities))
MAPE(lmodel1_ln$fitted.values,log10(my_data$entities))
MAE(lmodel1_ln$fitted.values,log10(my_data$entities))


RMSE(lmodel2_ln$fitted.values,log10(my_data$entities))
MAPE(lmodel2_ln$fitted.values,log10(my_data$entities))
MAE(lmodel2_ln$fitted.values,log10(my_data$entities))

RMSE(lmodel3_ln$fitted.values,log10(my_data$entities))
MAPE(lmodel3_ln$fitted.values,log10(my_data$entities))
MAE(lmodel3_ln$fitted.values,log10(my_data$entities))

#Nonparametric regression
#roads
roads1 = npregbw(log10(entities) ~ log10(roads), data = my_data, regtype="lc", nmulti=2)
head(roads1)

np_roads = npreg(txdat = log10(my_data$roads), tydat = log10(my_data$entities), bws = roads1)
summary(np_roads)
#errors
RMSE(np_roads$mean,log10(my_data$entities))
MAPE(np_roads$mean,log10(my_data$entities))
MAE(np_roads$mean,log10(my_data$entities))

#coefficient of determination
( 1 - ( sum( (np_roads$mean - log10(my_roads$entities))^2 ) / sum( (log10(my_roads$entities) - mean(log10(my_roads$entities)))^2 ) ) ) 

#plot
plot(np_roads, col = 2, type = "o",
     xlab="Logarithmized index of poviat hard surface roads per 100 km2", ylab="Logarithmized number of new registered entities",
     xlim=c(min(log10(my_data$roads)), max(log10(my_data$roads))), 
     ylim=c(min(log10(my_data$entities)) ,max(log10(my_data$entities))))
points(log10(my_data$roads), log10(my_data$roads))
rug(log10(my_data$roads), side = 1); rug(log10(my_data$roads), side = 2)

#population
population2 = npregbw(log10(entities) ~ log10(population), data = my_data, regtype="lc", nmulti=2)
head(population2)

np_population = npreg(txdat = log10(my_data$population), tydat = log10(my_data$population), bws = population2)
summary(np_population)

#errors
RMSE(np_population$mean,log10(my_data$entities))
MAPE(np_population$mean, log10(my_data$entities))
MAE(np_population$mean, log10(my_data$entities))

#coefficient of determination
( 1 - ( sum( (np_population$mean - log10(my_data$entities))^2 ) / sum( (log10(my_data$entities) - mean(log10(my_data$entities)))^2 ) ) ) 

#plot
plot(np_population, col = 2, type = "o",
     xlab="Logarithmized index of population per 1 km2", ylab="Logarithmized number of new registered entities",
     xlim=c(min(log10(my_data$population)), max(log10(my_data$population))), 
     ylim=c(min(log10(my_data$entities)) ,max(log10(my_data$entities))))
points(log10(my_data$population), log10(my_data$entities))
rug(log10(my_data$population), side = 1); rug(log10(my_data$entities), side = 2)

#crimes
crimes3 = npregbw(log10(entities) ~ log10(crimes), data = my_data, regtype="lc", nmulti=2)
head(crimes3)

np_crimes = npreg(txdat = log10(my_data$crimes), tydat = log10(my_data$entities), bws = crimes3)
summary(np_przestepczosc)

#errors
RMSE(np_crimes$mean, log10(my_data$entities))
MAPE(np_crimes$mean, log10(my_data$entities))
MAE(np_crimes$mean, log10(my_data$entities))
#coefficient of determination
( 1 - ( sum( (np_crimes$mean - log10(my_data$entities))^2 ) / sum( (log10(my_data$entities) - mean(log10(my_data$entities)))^2 ) ) )

#plot
plot(np_entities, col = 2, type = "o",
     xlab="Logarithmized number of identified crimes", ylab="Logarithmized number of new registered entities",
     xlim=c(min(log10(my_data$crimes)), max(log10(my_data$crimes))), 
     ylim=c(min(log10(my_data$entities)) ,max(log10(my_data$entities))))
points(log10(my_data$crimes), log10(my_data$entities))
rug(log10(my_data$crimes), side = 1); rug(log10(my_data$entities), side = 2)

#k-fold cross-validation
data_crimes <- my_data[c("crimes", "entities")]
k <- 10
set.seed(42) 
rows <- sample(nrow(log10(data_crimes))) 
crimes_shuffled <- log10(data_crimes)[rows, ] #data shuffling
folds <- createFolds(crimes_shuffled$crimes , k = k, list = TRUE, returnTrain = FALSE) #dividing data into 10 subsets

#table on the bed and the coefficient of determination
prediction_error <- data.frame(c(0,0),c(0,0),c(0,0), c(0,0))
names(prediction_error) <- c("RMSE", "MAPE", "MAE", "R2")
row.names(prediction_error)  <- c("Linear regression", "Nonparametric regression")

for(i in 1:k){
  #training and test set
  rows <- folds[i]
  train_data <- log10(dane_przestepczosc)[-unlist(rows), ]
  test_data <- log10(dane_przestepczosc)[unlist(rows), ]
  
  #forecasting the value of a variable entities with a parametric regression model
  lm_model <- lm(entities ~ crimes, data = train_data)
  crimes_pred <- predict.lm(lm_model, test_data)
  
  #parametric regression errors collected into the table
  errs <- test_data[,2] - entities_pred
  prediction_error[1,1] <- prediction_error[1,1] + RMSE(entities_pred, test_data[,2]) #RMSE
  prediction_error[1,2] <- prediction_error[1,2] + MAPE(entities_pred, test_data[,2]) #MAPE
  prediction_error[1,3] <- prediction_error[1,3] + MAE(entities_pred, test_data[,2]) #MAE
  prediction_error[1,4] <- prediction_error[1,4] + ( 1 - ( sum( errs^2 ) / sum( (test_data[,2] - mean(test_data[,2]))^2 ) ) ) #wspolczynnik determinacji
  
  #forecasting the value of a variable entities with a nonparametric regression model
  bw = npregbw(entities ~ crimes, data = train_data, regtype = "lc", nmulti=2)
  regression_np = npreg(bw, exdat=test_data[,1], eydat=test_data[,2]) 
  
  #nonparametric regression errors collected into the table
  prediction_error[2,1] <- prediction_error[2,1] + RMSE(regression_np$mean, test_data[,2]) #RMSE
  prediction_error[2,2] <- prediction_error[2,2] + regression_np$MAPE #MAPE
  prediction_error[2,3] <- prediction_error[2,3] + regression_np$MAE #MAE
  prediction_error[2,4] <- prediction_error[2,4] + regression_np$R2 #coefficient of determination
}

#Averaging of prediction errors
for(i in 1:2){
  for(j in 1:4){
    prediction_error[i,j] <- prediction_error[i,j] / k
  }
}

prediction_error
