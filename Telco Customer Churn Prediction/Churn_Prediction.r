# Import packages
install.packages("googlesheets4")
install.packages("ggplot2")
install.packages("cowplot")
library(tidyverse)
library(googlesheets4)
library(ggplot2)
library(cowplot)
library(randomForest)
library(caret)
library(pROC)


# Import Data
telco <- read.csv(file = 'C:/Data analytic/Project/R/Source/Telco_Customer_Churn.csv', header = TRUE)

# Explore Data
head(telco)
glimpse(telco)

## DROP NA (missing values)
telco_clean_na <- na.omit(telco)
nrow(telco_clean_na)

glimpse(telco_clean_na)

## Change factor for Visual
telco_clean_na$SeniorCitizen <- as.factor(ifelse(telco_clean_na$SeniorCitizen==1, 'YES', 'NO'))

glimpse(telco_clean_na)



  


# create data visual for explore this data
plot1 <- ggplot(telco_clean_na, aes(x = gender, fill = Churn)) +
  geom_bar() +
  labs(title = "Gender")

plot2 <- ggplot(telco_clean_na, aes(x = SeniorCitizen, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "Senior Citizen")

plot3 <- ggplot(telco_clean_na, aes(x = Partner, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "Partner")

plot4 <- ggplot(telco_clean_na, aes(x = Dependents, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "Dependents")

plot5 <- ggplot(telco_clean_na, aes(x = PhoneService, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "PhoneService")  

plot6 <- ggplot(telco_clean_na, aes(x =MultipleLines, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "MultipleLines")

plot7 <- ggplot(telco_clean_na, aes(x =InternetService, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "InternetService")

plot8 <- ggplot(telco_clean_na, aes(x =OnlineSecurity, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "OnlineSecurity")
    
plot9 <- ggplot(telco_clean_na, aes(x =OnlineBackup, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "OnlineBackup")

plot10 <- ggplot(telco_clean_na, aes(x =DeviceProtection, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "DeviceProtection")

plot11 <- ggplot(telco_clean_na, aes(x =InternetService, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "InternetService")

plot12 <- ggplot(telco_clean_na, aes(x =TechSupport, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "TechSupport")

plot13 <- ggplot(telco_clean_na, aes(x =StreamingTV, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "StreamingTV")
  
plot14 <- ggplot(telco_clean_na, aes(x =StreamingMovies, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "StreamingMovies")

plot15 <- ggplot(telco_clean_na, aes(x =Contract, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "Contract")

plot16 <- ggplot(telco_clean_na, aes(x =PaperlessBilling, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "PaperlessBilling")

plot17 <- ggplot(telco_clean_na, aes(x =PaymentMethod, fill = Churn)) +
  geom_bar(position = 'fill') +
  labs(title = "PaymentMethod")     

# create graph plot
grid_1 <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)
grid_2 <- plot_grid(plot7, plot8, plot9, plot10, plot11, plot12, ncol = 3)
grid_3 <- plot_grid(plot13, plot14, plot15, plot16, ncol = 2)
grid_4 <- plot_grid(plot17)

# show graph
grid_1
grid_2
grid_3
grid_4

# create data visual for explore this data when data is numerical 
plot18 <- ggplot(telco_clean_na, aes(y = tenure, x = " ", fill = Churn)) +
  geom_boxplot() +
  labs(title = "tenure") +
  theme_minimal()

plot19 <- ggplot(telco_clean_na, aes(y = MonthlyCharges, x = " ", fill = Churn)) +
  geom_boxplot() +
  labs(title = "MonthlyCharges") +
  theme_minimal()

plot20 <- ggplot(telco_clean_na, aes(y = TotalCharges, x = " ", fill = Churn)) +
  geom_boxplot() +
  labs(title = "TotalCharges") +
  theme_minimal()  

# create graph plot in boxplot when data is stats
grid_5 <- plot_grid(plot18, ncol = 1)
grid_6 <- plot_grid(plot19, ncol = 1)
grid_7 <- plot_grid(plot20, ncol = 1)



# show graph in boxplot
grid_5
grid_6
grid_7



## Prepare data
# Cleaning data

telco_clean_na_1 <- data.frame(lapply(telco_clean_na, 
                           function(x) gsub("No internet service", "No", x)))

telco_clean_na_1 <- data.frame(lapply(telco_clean_na_1, 
                           function(x) gsub("No phone service", "No", x)))



## Standardization

num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco_clean_na_1[num_columns] <- sapply(telco_clean_na_1[num_columns], as.numeric)

telco_clean_na_2 <- telco_clean_na_1[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_std_1 <- data.frame(scale(telco_clean_na_2))

## create dummy for analyse

telco_cat <- telco_clean_na_1[,-c(1,6,19,20)]

#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))

head(dummy)

#Combining the data
telco_final <- cbind(telco_std_1,dummy)
head(telco_final)


## SPLIT DATA
set.seed(42)
n <- nrow(telco_final)
id <- sample(1:n, size=n*0.7) ## 70% train 30% test
train_data <- telco_final[id, ]
test_data <- telco_final[-id, ]

#Build the first model using all variables
model_1 = glm(Churn ~ ., data = train_data, family = "binomial")
summary(model_1)

# Train Model 2 for P value <= 0.5
model_2 = glm(Churn ~ tenure + TotalCharges + Contract.xOne.year + Contract.xTwo.year 
                    + PaperlessBilling + PaymentMethod.xElectronic.check
                    , data = train_data, family = "binomial" )
p_train_2 <- predict(model_2, type = "response")

pred_churn_2 <- factor(ifelse(p_train_2 >= 0.50, "Yes", "No"))
actual_churn_2 <- factor(ifelse(train_data$Churn == 1, "Yes", "No"))
table(actual_churn_2,pred_churn_2)
mean(actual_churn_2 == pred_churn_2)




## test model 2
p_test_2 <- predict(model_2, newdata = test_data, type = "response")

test_data$pred <- if_else(p_test_2 >= 0.5, "Yes", "No")
test_data$actual <- if_else(test_data$Churn == 1, "Yes", "No")
mean(test_data$actual == test_data$pred)


#Confusion Matrix.
table(test_data$pred, test_data$actual)

test_pred_churn_2 <- factor(ifelse(p_test_2 >= 0.50, 1, 0))
test_actual_churn_2 <- factor(ifelse(test_data$Churn == 1, 1, 0))
conf_matrix_2 <- confusionMatrix(test_actual_churn_2, test_pred_churn_2)


# Accuracy calculation
accuracy_2 <- sum(test_pred_churn_2 == test_actual_churn_2) / length(test_actual_churn_2)


# Accuray, Recall and F1 score
cat("Accuracy:", accuracy_2, "\n")
cat("Recall:", conf_matrix_2$byClass["Recall"], "\n")
cat("F1 Score:", conf_matrix_2$byClass["F1"], "\n")


### Train Model 3 Randomforrest

model.rf <- randomForest(Churn ~ ., data = train_data, proximity=FALSE,importance = FALSE,
                        ntree=500,mtry=4, do.trace=FALSE)

p_train_3 <- predict(model.rf, type = "response")

pred_churn_3 <- factor(ifelse(p_train_3 >= 0.50, "Yes", "No"))
actual_churn_3 <- factor(ifelse(train_data$Churn == 1, "Yes", "No"))
table(actual_churn_3,pred_churn_3)
mean(actual_churn_3 == pred_churn_3)


### Test model 3

p_test_3 <- predict(model.rf, newdata = test_data, type = "response")

test_data$pred_rf <- if_else(p_test_3 >= 0.5, "Yes", "No")
test_data$actual <- if_else(test_data$Churn == 1, "Yes", "No")
mean(test_data$actual == test_data$pred_rf)

#Confusion Matrix.
table(test_data$pred_rf, test_data$actual)

test_pred_churn_3 <- factor(ifelse(p_test_3 >= 0.50, 1, 0))
test_actual_churn_3 <- factor(ifelse(test_data$Churn == 1, 1, 0))
conf_matrix_3 <- confusionMatrix(test_actual_churn_3, test_pred_churn_3)

# Accuracy calculation
accuracy_3 <- sum(test_pred_churn_3 == test_actual_churn_3) / length(test_actual_churn_3)


# Accuray, Recall and F1 score
cat("Accuracy:", accuracy_3, "\n")
cat("Recall:", conf_matrix_3$byClass["Recall"], "\n")
cat("F1 Score:", conf_matrix_3$byClass["F1"], "\n")


## Checking the AUC for all two models:
options(repr.plot.width =10, repr.plot.height = 8)

glm.roc <- roc(response = test_data$Churn, predictor = as.numeric(p_test_2))
rf.roc <- roc(response = test_data$Churn, predictor = as.numeric(p_test_3))

plot(glm.roc,      legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
plot(rf.roc, col = "red" , add = TRUE, print.auc.y = 0.85, print.auc = TRUE)
legend("bottom", c("Random Forest", "Logistic"),
       lty = c(1,1), lwd = c(2, 2), col = c("red", "black"), cex = 0.75)