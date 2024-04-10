library(ggplot2)
library(readxl)
library(tidyverse)
library(caret)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(car)
library(stargazer)
library(GGally)
library(arsenal)
library(arm)


#open the data set and save it as "term"
term <- read_excel("/Users/jaimerd/Desktop/master/Statistics Business/Assigment2 /term.xlsx")

##data cleaning ------

#convert categorical variables into factors
term <- term %>% mutate_if(is.character,as.factor)

summary(term)
                           
##numeric variables
data_numeric <- term[sapply(term, is.numeric)]
data_numeric

#age
hist(term$age)
boxplot(term$age)
term %>% count(age>100)
term$age[term$age >100] <- NA
summary(term$age)

#try to impute outliers but the accuracy does not improve
term$age[term$age >100] <- mean(term$age)
term$age[term$age >100] <-median(term$age)

#contact duration (not important will not be used for the model)
hist(term$contact_duration)
boxplot(term$contact_duration)
summary(term$contact_duration)


#campaign () 
table(term$campaign)
boxplot(term$campaign)
term %>% count(campaign>33)
term$campaign[term$campaign > 33] <- NA
summary(term$campaign)

#pdays
hist(term$pdays)
table(term$pdays)
boxplot(term$pdays)
summary(term$pdays)

#previouscontacts
hist(term$previous_contacts)
table(term$previous_contacts)


#var rate
hist(term$emp_var_rate)
table(term$emp_var_rate)


#cons price index
hist(term$cons_price_idx)
table(term$cons_price_idx)
boxplot(term$cons_price_idx)

#cons conf index 
boxplot(term$cons_conf_idx)
table(term$cons_conf_idx)
summary(term$cons_conf_idx)

#Euribor 3 month
boxplot(term$euribor_3m)

#number of employees
boxplot(term$n_employed)

##categorical variables
#month
table(term$month)
levels(term$month)[levels(term$month) == "july"] <- "jul"

#day of the week
table(term$day_of_week)
levels(term$day_of_week)[levels(term$day_of_week) == "tues"] <- "tue"


##missing values = converted them to the level with the highest frequency 
summary(term$marital_status)
term$marital_status[is.na(term$marital_status)] <- 'married'

#omit missing values
term <- na.omit(term)

sumsummary(term)


##Correlations -----

#(hacer con las hipotesis)

#subscribed con numerical
corr1 <- t.test(age ~ subscribed, data = term)
corr1
corr2 <- t.test(campaign ~ subscribed, data = term)
corr2
corr3 <- t.test(pdays ~ subscribed, data = term)
corr3
corr4 <- t.test(euribor_3m ~ subscribed, data = term)
corr4
corr5 <- t.test(n_employed ~ subscribed, data = term)
corr5
corr6 <- t.test(previous_contacts  ~ subscribed, data = term)
corr6
corr7 <- t.test(emp_var_rate ~ subscribed, data = term)
corr7
corr8 <- t.test(cons_price_idx ~ subscribed, data = term)
corr8
corr9 <- t.test(cons_conf_idx ~ subscribed, data = term)
corr9

#subscribed con categorical
table1 <- table(term$subscribed, term$education_level)
correducational <- chisq.test(table1)
print(correducational)

table2 <- table(term$subscribed, term$contact_method)
corrcontact <- chisq.test(table2)
print(corrcontact)

table3 <- table(term$subscribed, term$occupation)
corroccu <- chisq.test(table3)
print(corroccu)

table4 <- table(term$subscribed, term$credit_default)
corrcredit <- chisq.test(table4)
print(corrcredit)

table5 <- table(term$subscribed, term$personal_loan)
corrpersonal <- chisq.test(table5)
print(corrpersonal)

table6 <- table(term$subscribed, term$housing_loan)
corrhouse <-chisq.test(table6)
print(corrhouse)

#correlations between numerical 
correlation_matrix <- cor(data_numeric, use = "complete.obs")
corrplot(correlation_matrix, method = "circle")



##Descriptive Statistics and correlations ------

configuration <- tableby.control(
  test = T,
  total = FALSE,
  numeric.test = "anova", cat.test = "chisq",
  numeric.stats = c("meansd"),
  cat.stats = c("countpct"))

descriptive_stats <- tableby(subscribed ~ .,
                     data = term,
                     control = configuration)

report <- summary(descriptive_stats, title = "Descriptive Statistic", text= TRUE)
write2word(report, file = "Descriptive_Statistics.docx")





#graph comparing subscribed yes vs no 
#calculate the percentage of "yes" "no". 1. A table is created to see the frequency of "yes" and "no". 2. with prop table each value is divided by the sum of the total, so the percentage is calculated when the table is multiplied by 100. 3. the table is converted into a data frame 
per_subscribed <- data.frame(prop.table(table(term$subscribed)) * 100)
ggplot(per_subscribed, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), position = position_stack(vjust = 0.5))+
  theme(legend.position = "none")



#Categorize ages into groups and create a bar plot of subscriptions by age group
term$age_group <- cut(term$age, breaks = c(0, 25, 35, 45, 55, 65, Inf), labels = c("0-25", "26-35", "36-45", "46-55", "56-65", "66+"))
ggplot(term, aes(x=age_group, fill = subscribed)) +
  geom_bar(position= "stack", width = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size=10))+
  theme(legend.position = "top")



#bar plot showing the occupation 
ggplot(term, aes(x = occupation, fill = subscribed)) +
  geom_bar(position = "stack", width = 0.7) +
  labs(
       x = "Occupation",
       y = "Customers",
       fill = "Subscribed") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),legend.position = "top") +
  coord_flip()
  
#Subscription Rates by occupation
ggplot(term, aes(x = occupation, fill = subscribed)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Occupation",
    y = "Percentage of Customers",
    fill = "Subscribed",
    title = "Subscription Rates by Occupation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top")



#bar plot marital status
ggplot(term, aes(x = marital_status, fill= subscribed)) +
  geom_bar(position= "stack", width = 0.5) +
  labs(
       x = "Marital Status",
       y = "Customers",
       fill = "Subscribed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle= 90, size=12),legend.position = "top")

#bar plot education level 
ggplot(term, aes(x = education_level, fill= subscribed)) +
  geom_bar(position= "stack", width = 0.7) +
  labs(
    x = "Education Level",
    y = "Customers",
    fill = "Subscribed") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),legend.position = "top")+
  coord_flip()

#bar graph credit default
ggplot(term, aes(x = credit_default, fill = subscribed)) +
  geom_bar(position = "stack", width = 0.5) +
  labs(
    x = "Credit Default",
    y = "Customers",
    fill = "Subscribed"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),legend.position = "top")

#subscriptions per month
#Create a data frame with the number of subscribers and group them by month in order.
month_subs <- term %>% 
  filter(subscribed == "yes") %>%
  mutate(month = factor(month, levels = c("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct","nov", "dec"))) %>%
  group_by(month) %>%
  summarise(subscriptions = n())

#line graph with subscribers per month
ggplot(month_subs, aes(x = month, y = subscriptions, group=1)) +
  geom_area(fill = "blue", alpha = 0.3) +
  geom_line(color = "blue", size =0.5) + 
  labs(title = "Subscribers per Month",
       x = "Month",
       y = "Subscriptions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90 ,size = 13))

#bar graph with the subscription rates by month
ggplot(term, aes(x = month, fill = subscribed)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Month",
    y = "Percentage of Customers",
    fill = "Subscribed",
    title = "Subscription Rates by Month"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "top")

#subscriptions per weekday
#Create a data frame with the number of subscribers and group them by weekday in order.
week_subs <- term %>% 
  filter(subscribed == "yes") %>%
  mutate(day_of_week = factor(day_of_week, levels = c("mon", "tue", "wed", "thu", "fri"))) %>%
  group_by(day_of_week) %>%
  summarise(subscriptions = n())

#line graph with subscribers per weekday
ggplot(week_subs, aes(x = day_of_week, y = subscriptions, group=1)) +
  geom_area(fill = "blue", alpha = 0.3) +
  geom_line(color = "blue", size =0.5) + 
  labs(title = "Subscribers per Week Day",
       x = "Day of the week",
       y = "Subscriptions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 13))

#Subscription Rates by Weekday
ggplot(term, aes(x = day_of_week, fill = subscribed)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Weekdays",
    y = "Percentage of Customers",
    fill = "Subscribed",
    title = "Subscription Rates by Weekday"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "top")


#box plot campaign for subscribers 
ggplot(term, aes(x= subscribed, y= campaign, fill=subscribed))+
  geom_boxplot()+ 
  labs(y = "Number of contacts") +
  theme_minimal()

#contact method
#create data frame for customers contacted by cellular
cellular <- term %>% 
  filter(contact_method == "cellular") %>%
  group_by(subscribed) %>%
  summarise(cellular_cont = n())

#bar graph for customers contacted by cellular 
ggplot(term, aes(x = subscribed, fill = contact_method)) +
  geom_bar(position = "stack", width = 0.5) +
  labs(
    x = "Subscribed",
    y = "Customers",
    fill = "Contact Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),legend.position = "top")

#Subscription Rates by contact method 
ggplot(term, aes(x = contact_method, fill = subscribed)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Contact Method",
    y = "Percentage of Customers",
    title = "Subscription Rates by Contact Method"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size=12),
    legend.position = "top"
  )



#Blot plot emp_var_rate
ggplot(term, aes(y = emp_var_rate, x = subscribed, fill= subscribed)) +
  geom_boxplot() +
  labs(y = "Employment variation rate") +
  theme_minimal()

#Box plot cons_price_idx
ggplot(term, aes(y = cons_price_idx, x = subscribed, fill= subscribed)) +
  geom_boxplot() +
  labs(y = "Consumer price index ") +
  theme_minimal()

#Box plot cons_conf_idx
ggplot(term, aes(y = cons_conf_idx, x = subscribed, fill= subscribed)) +
  geom_boxplot() +
  labs(y = "Consumer confidence index  ") +
  theme_minimal()

#Box plot euribor_3m
ggplot(term, aes(y = euribor_3m, x = subscribed, fill= subscribed)) +
  geom_boxplot() +
  labs(y = "Euribor 3 month rate") +
  theme_minimal()

#Box plot n_employed
ggplot(term, aes(y = n_employed, x = subscribed, fill= subscribed)) +
  geom_boxplot() +
  labs(y = "Number of employees") +
  theme_minimal()




##Model Development -----

levels(term$subscribed)

#omit missing values
term <- na.omit(term)

#split the data into train (80%) and test (20%) data set. 
set.seed(40425150)
index <- createDataPartition(term$subscribed, p=0.8, list=FALSE)
train <- term[index,]
test <- term[-index,]

#1 model
formula1 = subscribed  ~ age + occupation + marital_status + euribor_3m + cons_conf_idx
model1 <- glm(formula = formula1 , data = train, family= "binomial")
summary(model1)

#2 model
formula2 = subscribed  ~ age + occupation + marital_status + education_level + euribor_3m + cons_conf_idx
model2 <- glm( formula = formula2, data = train, family= "binomial" )
summary(model2)

#3 model
formula3 = subscribed  ~ age + marital_status + credit_default + month + campaign + poutcome + emp_var_rate 
model3 <- glm(formula = formula3, data = train, family= "binomial")
summary(model3)

#4 model
formula4 = subscribed ~ day_of_week + occupation + contact_method + campaign + month + euribor_3m + cons_price_idx + pdays
model4 <- glm(formula = formula4 , data = train, family = "binomial")
summary(model4)
exp(model4$coefficients)

#Pseudo R^2 
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}
#Source: Field et al. (2012)

logisticPseudoR2s(model1)
logisticPseudoR2s(model2)
logisticPseudoR2s(model3)
logisticPseudoR2s(model4)

#table with all the models 
stargazer(model1, model2, model3, model4, type = "html", out =
            "models.html")

##assumptions checking -----

#residuals
resid(model4)
train$standarisedResiduals <- rstandard(model4)
train$studentdResiduals <- rstudent(model4)
sum(train$standarisedResiduals > 1.96)
sum(train$standarisedResiduals > 2.58)
sum(train$standarisedResiduals > 3)
plot(model4)
summary(train$standarisedResiduals)
table(train$standarisedResiduals)

#binned residual plot
binnedplot(fitted(model4), 
           residuals(model4, type = "response"), 
           col.pts = 1, 
           col.int = "red")



#influential cases
train$cook <- cooks.distance(model4)
sum(train$cook > 1)

train$leverage <- hatvalues(model4)
sum(train$leverage > 0.0009)

cooks.distance(model4)

plot(model4,which=4)

#multicollinearity
vif(model4)

#linearity of the logit (log of numerical variables)
train$log_camp <- log(train$campaign)*train$campaign
train$log_pdays <- log(train$pdays)*train$pdays
train$log_euribor <- log(train$euribor_3m)*train$euribor_3m
train$log_cons <- log(train$cons_price_idx)*train$cons_price_idx



formula_linea = subscribed ~ day_of_week + occupation + contact_method + campaign + month + pdays + euribor_3m + cons_price_idx + log_camp + log_pdays +log_euribor +log_cons
model_check <- glm(formula = formula_linea , data = train, family = "binomial")
summary(model_check)




##Predictions with test data -----

predictions <- predict(model4,test, type = "response")
predictions
class_pred <- as.factor(ifelse(predictions > 0.5, "yes", "no"))
postResample(class_pred, test$subscribed)
conf_matrix <- confusionMatrix(class_pred, test$subscribed, positive = "yes")
conf_matrix

#ROC Curve
r <- multiclass.roc(test$subscribed, predictions, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')


##########################

predict_actual <- data.frame(Actual = test$subscribed, Predicted = class_pred)
data_counts <- as.data.frame(table(predict_actual))
ggplot(data_counts, aes(x = Actual, y = Freq, group = Predicted)) +
  geom_point(aes(color = Predicted), position = position_dodge(width = 0.3)) +
  labs(title = "Dot Plot de Valores Reales vs Predichos",
       x = "Categoría",
       y = "Frecuencia",
       color = "Legend") +
  theme_minimal()


#Source: finnstats, R-bloggers, 2021



##Fixing the imbalance data set ------

remotes::install_version("DMwR", version="0.4.1")

library ("DMwR")

smote_dataset <- as.data.frame(term)

#balancing the data
smote <-
  SMOTE(
    form = subscribed ~ .,
    data = smote_dataset,
    perc.over = 400,
    perc.under = 100
  )

set.seed(40425150)
index <- createDataPartition(smote$subscribed, p=0.8, list=FALSE)
train_smote <- smote[index,]
test_smote <- smote[-index,]

formula_smote = subscribed ~ day_of_week + occupation + contact_method + campaign + month + euribor_3m + cons_price_idx + pdays
model_smote <- glm(formula = formula_smote , data = train_smote, family = "binomial")
summary(model_smote)

#Predictions with test data

predictions_smote <- predict(model_smote,test_smote, type = "response")
predictions_smote
class_pred_smote <- as.factor(ifelse(predictions_smote > 0.5, "yes", "no"))
postResample(class_pred_smote, test_smote$subscribed)
conf_matrix_smote <- confusionMatrix(class_pred_smote, test_smote$subscribed, positive = "yes")
conf_matrix_smote

#ROC Curve
r_smote <- multiclass.roc(test_smote$subscribed, predictions_smote, percent = TRUE)
roc_smote <- r_smote[['rocs']]
r1_smote <- roc_smote[[1]]
plot.roc(r1_smote,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')






#improving accuracy with decision tree ------
library(rpart)

#imbalance data set
#decision tree with all variables except ID and contact duration
tree <- rpart(subscribed ~ age + occupation + marital_status + education_level + credit_default + housing_loan + personal_loan + contact_method + month + day_of_week + campaign + pdays + previous_contacts + poutcome + emp_var_rate + cons_price_idx +cons_conf_idx + euribor_3m + n_employed, data = train, method = "class")
print(tree)
predictions_tree <- predict(tree, test, type = "class")
predictions_tree
postResample(predictions_tree, test$subscribed)
cm_tree <- confusionMatrix(predictions_tree, test$subscribed)
cm_tree

#ROC Curve
predictions_tree_1 <- predict(tree, test, type = "prob")
probabilities_tree <- predictions_tree_1[, "yes"]
roc_curve_tree <- roc(test$subscribed, probabilities_tree)
auc(roc_curve_tree)

#smote
#decision tree with all variables except ID and contact duration
tree_smote <- rpart(subscribed ~ age + occupation + marital_status + education_level + credit_default + housing_loan + personal_loan + contact_method + month + day_of_week + campaign + pdays + previous_contacts + poutcome + emp_var_rate + cons_price_idx +cons_conf_idx + euribor_3m + n_employed, data = train_smote, method = "class")
print(tree_smote)
predictions_tree_smote <- predict(tree_smote, test_smote, type = "class")
predictions_tree_smote
postResample(predictions_tree_smote, test_smote$subscribed)
cm_tree_smote <- confusionMatrix(predictions, test_smote$subscribed)
cm_tree_smote

#ROC Curve
predictions_tree_1 <- predict(tree_smote, test_smote, type = "prob")
probabilities_tree <- predictions_tree_1[, "yes"]
roc_curve_tree <- roc(test_smote$subscribed, probabilities_tree)
auc(roc_curve_tree)

#improving accuracy with random forest -------

library(randomForest)

#imbalance data set 
#random forest with all variables except ID and contact duration
rf <- randomForest(subscribed ~ age + occupation + marital_status + education_level + credit_default + housing_loan + personal_loan + contact_method + month + day_of_week + campaign + pdays + previous_contacts + poutcome + emp_var_rate + cons_price_idx +cons_conf_idx + euribor_3m + n_employed, data = train)
rf
pred_rf <- predict(rf,test)
cm_rf <- confusionMatrix(pred_rf,test$subscribed,positive = "yes")
cm_rf

#smote
#random forest with all variables except ID and contact duration
rf_smote <- randomForest(subscribed ~ age + occupation + marital_status + education_level + credit_default + housing_loan + personal_loan + contact_method + month + day_of_week + campaign + pdays + previous_contacts + poutcome + emp_var_rate + cons_price_idx +cons_conf_idx + euribor_3m + n_employed, data = train_smote)
rf_smote
pred_rf_smote <- predict(rf_smote,test_smote)
cm_rf_smote <- confusionMatrix(pred_rf_smote,test_smote$subscribed,positive = "yes")
cm_rf_smote

#ROC Curve
predictions_rf_1 <- predict(rf_smote, test_smote, type = "prob")
probabilities_rf <- predictions_rf_1[, "yes"]
roc_curve_rf <- roc(test_smote$subscribed, probabilities_rf)
auc(roc_curve_rf)




data <- term %>% select (subscribed, day_of_week, occupation, contact_method, campaign, month, pdays, euribor_3m, cons_price_idx)
ggpairs (data, aes (colour= subscribed))








