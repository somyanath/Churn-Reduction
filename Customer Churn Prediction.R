# Clear the environment
rm(list = ls())

#importing the required libraries
library("ggplot2")
library("corrgram")
library("randomForest")
library("C50")

# Load the file
customer_churn = read.csv("Train_data.csv")

# Explore the data
str(customer_churn)

# Check for missing values
sapply(customer_churn, function(x) sum(is.na(x)))

##### PRE PROCESSING ####

# Drop the unimportant variables which does not contribute to the model at all
# As, phone number does not contribute any significant information to the
# prediction model at all
customer_churn$phone.number = NULL

# Convert the area code from integer class to factor
# As, there are only 3 areas so it is better to convert the variable's data
# in the form of a factor as it improves the efficiency of the code
customer_churn$area.code = factor(customer_churn$area.code)

# We can also, plot the probability density functions of the numerical variables
# to check their distribution and also skewness
old_par = par("mar")
par(mar = c(1,1,1,1))
library(psych)
multi.hist(customer_churn[,sapply(customer_churn, is.numeric)], main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")
par(mar = old_par)


##### EDA & Feature Selection #####

# Creating the correlation analysis plot to analyse the dependecies of numeric
# variables
library(corrplot)
cor_plot = cor(customer_churn[,sapply(customer_churn, is.numeric)])
corrplot(cor_plot, method = "circle", addgrid.col = "darkgray",
         outline = TRUE, number.digits = 2)
#The correlation plot tells us that the numerical variables where there is
#"minutes" and "charges" are correlated. So, we can drop one of the two from every pair of variables


# Also,we can perform binning on the account length variable in order to check
# its distribution. As it is very large amount of data.
summary(customer_churn$account.length)
# BUt after checking its summary we can say that we can divide it into intervals
# so that we can check its distribution across the total population

# set up boundaries for bins
breaks <- c(0,13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247)
# specify interval/bin labels
labels <- c("0-13", "13-26", "26-39", "39-52", "52-65", "65-78", "78-91", "91-104",
            "104-117", "117-130", "130-143", "143-156", "156-169", "169-182", 
            "182-195", "195-208", "208-221", "221-234", "234-247")
# bucketing data points into bins
bins <- cut(customer_churn$account.length, breaks, include.lowest = T, right=FALSE, labels=labels)
# inspect bins
summary(bins)
# Plotting the account length distribution
plot(bins, main="Distribution of Customers w.r.t. Account Length", xlab="Account Length", ylab="Number of Customers",col="orange")


# Checking the distribution of churn in the observations
library(plyr)
churn_count = count(customer_churn, 'Churn')
ggplot(churn_count, aes(x=Churn, y = freq)) + xlab("True & False") + 
  geom_histogram(stat="identity") + ylab("Number of Customers")

# percentage of the false churn
paste0("The percentage of false churn is ", (churn_count$freq[1] / 3333)*100, " %")

# percentage of the true churn
paste0("The percentage of true churn is ", (churn_count$freq[2] / 3333)*100, " %")
# This churn rate implies that nearly 15% of the customers left the telecom
# company's services. Now the reason for the leave acan be anything from random 
# chance to any specific problem in the company's policies and charges.

# In order to be more sure about the reason for churn we can check the dependency
# of churn with different variables.


# Checking the distribution of states in the total observations
state_count = count(customer_churn, 'state')
ggplot(state_count, aes(x=state, y = freq)) + xlab("States") + 
  geom_histogram(stat="identity") + ylab("Count of States")
# The distribution gives us an idea that there is majority of only "WEST VIRGINIA"
# in the total observations


# International Plan count
Intplan_count = count(customer_churn, 'international.plan')

# percentage of the non-international plan subscribers
paste0("The percentage of customers not subscribed in international plan is ", (Intplan_count$freq[1] / 3333)*100, " %")

# percentage of the international plan subscribers
paste0("The percentage of customers subscribed in international plan is ", (Intplan_count$freq[2] / 3333)*100, " %")

# Voice-Mail Plan count
VoiceMailplan_count = count(customer_churn, 'voice.mail.plan')

# percentage of the non-voice-mail plan subscribers
paste0("The percentage of customers not subscribed in voice-mail plan is ", (VoiceMailplan_count$freq[1] / 3333)*100, " %")

# percentage of the non-voice-mail plan subscribers
paste0("The percentage of customers subscribed in voice-mail plan is ", (VoiceMailplan_count$freq[2] / 3333)*100, " %")

# Checking the distribution of different areas in the observations
# Area Code count
AreaCode_count = count(customer_churn, 'area.code')
ggplot(AreaCode_count, aes(x=area.code, y = freq)) + xlab("Area Codes") + 
  geom_histogram(stat="identity") + ylab("Number of Customers")

# Checking the distribution of number customer service calls in the observations
ServiceCall_count = count(customer_churn, 'number.customer.service.calls')
ggplot(ServiceCall_count, aes(x=number.customer.service.calls, y = freq)) + xlab("Number of Customer Service Calls") + 
  geom_histogram(stat="identity") + ylab("Number of Customers")
# The number of customer service calls is dominated by 2 followed by 0 & 2 and
# then followed by 3 & 4.

# Now, check the churn with different variables

# Now we check the churn with international plan subscribers
intlchurn_count = count(customer_churn, c('international.plan', 'Churn'))
# percentage of international plan subscribers and their false churn
paste0("International Plan and False Churn ", (intlchurn_count[3,3]/sum(intlchurn_count[c(3,4),3]))*100, " %")

# percentage of international plan subscribers and their true churn
paste0("International Plan and False Churn ", (intlchurn_count[4,3]/sum(intlchurn_count[c(3,4),3]))*100, " %")

# percentage of non-international plan subscribers and their false churn
paste0("Non-International Plan and False Churn ", (intlchurn_count[1,3]/sum(intlchurn_count[c(1,2),3]))*100, " %")

# percentage of non-international plan subscribers and their true churn
paste0("Non-International Plan and True Churn ", (intlchurn_count[2,3]/sum(intlchurn_count[c(1,2),3]))*100, " %")
# According to the findings, the churn is very high when the customer is a international
# plan subscriber i.e. above 40%.
# So, the International Plan is a very important variable in determining the churn

#Chi-Squared test of independence
factor_index = sapply(customer_churn, is.factor)
#In the previous we selected only the categorical variables

factor_data = customer_churn[,factor_index]

for (i in 1:5){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn, factor_data[,i])))
}
# According, to the chi-square test the "area.code" variable is not significant and can be
# rejected as the p-value is very high.

#Dimension reduction
customer_churn_del = subset(customer_churn, 
                             select = -c(area.code, total.day.charge, total.eve.charge, total.night.charge,
                                         total.intl.charge))


# Clear the environment except customer_churn_del
library(DataCombine)
rmExcept(c("customer_churn_del","customer_churn"))

##### MODEL DEVELOPMENT ####
customer_churn_model = customer_churn_del

# Divide the data into train and test using stratified sampling method
# createdatapartition is one more function used to create stratified samples
# Here in createdatapartiotion function 1st arg is of the reference variable on the basis
# of which splitting will take place, 2nd arg is the percentage of observation we need
# in our sample, list = false implies that theer will no repetitive observations
set.seed(1234)
train = customer_churn_model
test = read.csv("Test_data.csv")
test = subset(test, select = -c(phone.number, area.code, total.day.charge, total.eve.charge, total.night.charge,
                                        total.intl.charge))

##### LOGISTIC REGRESSION #####
# Logistic Regression
# glm is built-in function which helps us to build logistic regression on top of the dataset
LR_model = glm(Churn ~ ., data = train, family = "binomial"(link="logit"))

#summary of the model
summary(LR_model)
# The logistic regression test's result suggests us that the most important variables are
# "international.plan - yes", "total.day.minutes", "total.eve.minutes", "total.intl.minutes"
# "number.customer.service.calls" followed by "total.night.minutes" and "voice.mail/plan - yes"

#anova
anova(LR_model, test = "Chisq")
# Here, we can analyse the drop in the deviance when adding each variable. "international.plan" 
# and "number.customer.service.calls" followed by "total.day.minutes" significantly reduces the 
# residual deviance. But, the variables "voice.mail.plan", "total.eve.minutes","total.intl.minutes" 
# seems to improve the model very less even though they all have low p-values

test$Churn = as.character(test$Churn)
test$Churn[test$Churn == " True."] = "1"
test$Churn[test$Churn == " False."] = "0"

#predict using logistic regression
#Type = response gives us the probabilities
LR_predictions = predict(LR_model, newdata = test, type = "response")

#Convert probabilities
#Converting the probabilities into classes 1 & 0
LR_predictions = ifelse(LR_predictions > 0.5, 1, 0)

misClassifierError = mean(LR_predictions != test$Churn)

print(paste('Logistic Regression Accuracy', 1 - misClassifierError))

#Evaluate the performance of the classification model
ConfMatrix_LR = table(test$Churn, LR_predictions > 0.5)
#Accuracy = 87.16%
#FNR = 75%
output = cbind(test, LR_predictions)
write.csv(output,"LR_Output.csv", row.names = F)


##### NAIVE BAYES #####
library(e1071)

#Develop model
NB_model = naiveBayes(Churn ~ ., data = train)

#Predict the test cases
#type = class will internally divide the whole extracted probabilities into yes or no
#if we want the probabilities we can write type = raw
NB_predictions = predict(NB_model, test[,1:14], type = 'class')

#Look at the confusion matrix
ConfMatrix_NB = table(observed = test[,15], predicted = NB_predictions)
#Accuracy = 88.12%
#FNR = 71.42%
output = cbind(test, NB_predictions)
write.csv(output,"NB_Output.csv", row.names = F)

############# DECISION TREES ###########

#Decision Tree for classification
#Develop Model on training data
#1st arg is of the target var, here "Churn ~." full stop implies that except responded
#all the other variables are to be considered independent, we can also write the var
#names followed by + explicitly. 2nd arg is of the dataset. 3rd arg trails = 100
#implies that the compiler needs to build 100 trees one by one and select the best
#tree out of all trees. 4th arg rules = TRUE implies that the compiler needs
#to exctract the business rules out of the decision tree.
c50_model = C5.0(Churn ~., train, trials = 100, rules = TRUE)

#write rules into disk
write(capture.output(summary(c50_model)), "c50Rules.txt")

#Lets predict the test cases
#Predict is a function which helps in predicting the new tests cases with the help of
#existing model. 1st arg is of the mdoel name. 2nd arg is the reference to the data but here we
#want to remove the target variable so for that [,-17]. 3rd arg type = class gives the
#class to which the target variable comes out to be, means here or target variable
#is binary so for that we need to predict the test cases also in the form of binary numbers
c50_predictions = predict(c50_model, test[,-15], type = "class")

#Now all we have to do is compare the predicted value with the actual values of the target
#variable, then we have to look at how the variable can able to perform on the test data
#it mean that how accurate the model is.

#Now as the the target variable is categorical we can't do regression here as for regression
# we need a target variable which is continuos.

#Evaluate the performance of the classification model
#Here we are building a contingency table(confusion matrix). 1st arg is of the 
#actual values. 2nd arg is of the predcited values
confMatrix_c50 = table(test$Churn, c50_predictions)

#Here now we decide on the basis of the business problem that what is the best error metric
#Accuracy = 95.80%
#FNR = 29.01%
output = cbind(test, c50_predictions)
write.csv(output,"c50_Output.csv", row.names = F)

########### RANDOM FOREST CLASSIFIER ############

#Here we want to reduce the error rate
#RANDOM FOREST
#1st arg is of the target variable and saying that all the remaining variables are indep
#2nd arg is of dataset. 3rd arg importance = true suggests that apart from giving me
#output predictions laso give me the important variables. 4th arg is no. of trees.
#In this 1st take 100 trees, then take 500 and see if it improves the model.
RF_model = randomForest(Churn ~., train, importance = TRUE, ntree = 500)
print(RF_model)
plot(RF_model)

#Now, predict the test data using the random forest model
#1st arg is fo the RF model. 2nd arg is of the reference to the independent vars
RF_Predictions = predict(RF_model, test[,-15])

#Evaluate the performance of the classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
#Accuracy = 94.60%
#FNR = 30.35%
output = cbind(test, RF_Predictions)
write.csv(output,"RF_Output.csv", row.names = F)
