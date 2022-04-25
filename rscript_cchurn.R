customer_churn <- read.csv("C:\\Users\\dadzie.4\\Desktop\\Data_files_R\\telco_data.csv")

View(customer_churn)
summary(customer_churn)
unique(customer_churn$Partner)

mean(customer_churn$MonthlyCharges)
median(customer_churn$MonthlyCharges)
range(customer_churn$MonthlyCharges)

sum(is.na(customer_churn))
customer_churn <-na.omit(customer_churn)

#extracting internet service 
customer_churn$InternetService -> customer_internet_service

#extracting paperless billing
customer_churn$PaperlessBilling -> customer_paperless_billing

#extracting streamingtv
customer_churn$StreamingTV -> customer

#extracting 3rd , 6th and 9th columns
customer_churn[,c(3,6,9)]->customer_random_columns

#getting the customers whose 'Internetservice' is 'DSL'
count=0 
for(i in 1:nrow(customer_churn)){
  if(customer_churn$InternetService[i]=="DSL"){
    count=count+1
  }
}
count

View(customer_churn)
library(dplyr)
customer_churn %>% filter(InternetService=="DSL") -> customer_dsl 
View(customer_dsl)
#filter more
customer_churn %>% filter(gender=="Male" & SeniorCitizen ==1 & PaymentMethod=="Electronic check") -> senior_male_check 

#customers whose tenure >70 or total score >8000
customer_churn %>% filter(tenure>70 | TotalCharges > 8000) -> customer_total_tenure

#count of churn
customer_churn %>% count(Churn) 

#Data Visualition - bar plot of phone service
ggplot(data = customer_churn, aes(x=PhoneService)) + geom_bar()

#assiging the fill color to pink 
ggplot(data = customer_churn, aes(x=PhoneService)) + geom_bar(fill="blue")
ggplot(data = customer_churn, aes(x=PhoneService)) + geom_bar(fill="blue", col="peru")


#bar plot of internet service 
ggplot(data = customer_churn, aes(x=InternetService, fill=InternetService)) + geom_bar(fill="blue", col="red")

#very beautiful graphs
ggplot(data = customer_churn, aes(x=InternetService, fill=InternetService)) + geom_bar()


#Scatter plot - total charges & tenure
ggplot(data = customer_churn, aes(y=TotalCharges, x=tenure)) + geom_point()

ggplot(data = customer_churn, aes(y=TotalCharges, x=tenure)) + geom_point(col="wheat3")

#Mapping the PaymentMethod to col aes
ggplot(data = customer_churn, aes(y=TotalCharges, x=tenure, col=PaymentMethod)) + geom_point()


#Mapping the gender to col aes
ggplot(data = customer_churn, aes(y=TotalCharges, x=tenure, col=gender)) + geom_point()

#Mapping senior citizen to col aes
ggplot(data = customer_churn, aes(y=TotalCharges, x=tenure, col=SeniorCitizen)) + geom_point()


#Creating boxplots using ggplot2
ggplot(data = customer_churn, aes(y=tenure, x=Partner)) + geom_boxplot()

ggplot(data = customer_churn, aes(y=tenure, x=Partner)) + geom_boxplot(fill="violet", col="snow3")

ggplot(data = customer_churn, aes(y=tenure, x=Partner)) + geom_boxplot(fill="violet")

#Building a Linear regression model 
library(caTools)
sample.split(customer_churn$tenure, SplitRatio = 0.70) -> split_tag 
subset(customer_churn, split_tag == T) -> train
subset(customer_churn, split_tag == F) -> test

lm(tenure~Contract, data=train) ->model1
predict(model1, newdata=test) ->predicted_values

head(predicted_values)

cbind(Actual=test$tenure, Predicted=predicted_values) ->final_data

as.data.frame(final_data) -> final_data
head(final_data)
#checking error b/n predicted n actual
final_data$Actual - final_data$Predicted -> error
head(error)

#combining the error var into the final_data file
cbind(final_data, error) -> final_data

head(final_data)

rmse <- sqrt(mean((final_data$error)^2))

rmse

#Multiple Linear Regression
sample.split(customer_churn$tenure, SplitRatio = 0.75) -> split_tag 
subset(customer_churn, split_tag == T) -> train
subset(customer_churn, split_tag == F) -> test

lm(tenure~ Dependents+MultipleLines+OnlineSecurity+OnlineBackup+DeviceProtection, data=train) -> mod_multi_linear

predict(mod_multi_linear, newdata=test) ->predicted_multi_linear
head(predicted_multi_linear)

cbind(Actual=test$tenure, Predicted=predicted_multi_linear) -> final_data

as.data.frame(final_data)-> final_data


