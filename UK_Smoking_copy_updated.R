library(tidyverse)
library(Metrics)
# importing the dataset
dataset=read.csv("smoking.csv")
# Data exploration
glimpse(dataset)
view(dataset)
head(dataset)
length(dataset)

# Getting a summary of the dataset
summary(dataset)
# Getting summary of just the stat
summary(dataset[sapply(dataset,is.numeric)])

# Viewing the variable types within the the categorical variables 
table(dataset$region)
table(dataset$highest_qualification)
table(dataset$gross_income)
table(dataset$type)
# missing data imputation
# Seeing if there is any missing data, 
colSums(is.na(dataset))
# found that there is data missing for amt_weekends and amt_weekdays, 1270 values for both
# imputation for amt_weekends, the missing data is for people that didn't smoke so the amount they would smoke is 0
table(dataset$smoke)
dataset$amt_weekends=ifelse(is.na(dataset$amt_weekends),
                   0,
                   dataset$amt_weekends)
# imputation for amt_weekdays, the missing data is for people that didn't smoke so the amount they would smoke is 0
dataset$amt_weekdays=ifelse(is.na(dataset$amt_weekdays),
                            0,
                            dataset$amt_weekdays)
colSums(is.na(dataset))


# Filtering data for smokers and non-smokers for future analysis
smokers=dataset%>% filter(smoke == "Yes")
non_smokers=dataset %>% filter(smoke == "No")
# adding the total cigarettes smoked by smokers (weekdays + weekends) 
smokers$total_cigarettes=smokers$amt_weekdays + smokers$amt_weekends

# Visualize smoking status by different demographic variables

#Creating Bar plot of smoking status by gender
ggplot(data=dataset, aes(x = gender, fill = smoke)) +
  geom_bar(position = "fill") +
  labs(title = "Smoking Status by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "Smoking Status")
# From the data we can see that around 25% of females and 25% of males smoke

# Creating a box plot of total amount of cigarettes smoked by gender
ggplot(smokers, aes(x = gender, y = total_cigarettes, fill = gender)) +
  geom_boxplot() +
  labs(title = "Total Amount of Cigarettes Smoked by Gender",
       x = "Gender",
       y = "Total Amount of Cigarettes Smoked",
       fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# From the box plot we can see on average that males smoked more total cigarettes than females, males also had more outliers. 

# Creating a Bar plot of smoking status by marital status
ggplot(data=dataset, aes(x = marital_status, fill = smoke)) +
  geom_bar(position = "fill") +
  labs(title = "Smoking Status by Marital Status",
       x = "Marital Status",
       y = "Proportion",
       fill = "Smoking Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# We can see that  married people and widowed people are less likely to smoke than divorced and separated people

# Creating a bar plot of smoking status by education level
ggplot(data=dataset, aes(x = highest_qualification, fill = smoke)) +
  geom_bar(position = "fill") +
  labs(title = "Smoking Status by Education Level",
       x = "Education Level",
       y = "Proportion",
       fill = "Smoking Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) 
# People with GCSE/CSE, GCSE/O level and ONC/BTEC are more likely to smoke. 

# Create a box plot of amount of cigarettes smoked by education level

ggplot(smokers, aes(x = highest_qualification, y = total_cigarettes)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Amount of Cigarettes Smoked by Education Level",
       x = "Education Level",
       y = "Amount of Cigarettes Smoked") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# The mean for GCSE/O leveles was signficantly higher than other education levels

# Create a box plot of smoking status by ethnicity
ggplot(data=dataset, aes(x = ethnicity, fill = smoke)) +
  geom_bar() +
  labs(title = "Smoking Status by Ethnicity",
       x = "Ethnicity",
       y = "Count",
       fill = "Smoking Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# From this we can see that the sample ethnicity is mostly white. 

# Create a box plot of total amount of cigarettes smoked by ethnicity
ggplot(smokers, aes(x = ethnicity, y = total_cigarettes, fill = ethnicity)) +
  geom_boxplot() +
  labs(title = "Total Amount of Cigarettes Smoked by Ethnicity",
       x = "Ethnicity",
       y = "Total Amount of Cigarettes Smoked",
       fill = "Ethnicity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# White ethnicity have wider range of distribution for total amount of cigarettes smoked. Also have the most outliers. 

# Create a Box plot of age by smoking status
ggplot(data=dataset, aes(x = smoke, y = age, fill = smoke)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Smoking Status",
       x = "Smoking Status",
       y = "Age",
       fill = "Smoking Status")
# Graph doesn't really show much, older people are less likely to smoke than younger people based on the mean. 

# Create a bar plot of smoking status by income level
ggplot(data=dataset, aes(x = gross_income, fill = smoke)) +
  geom_bar() +
  labs(title = "Smoking Status by Income Level",
       x = "Income Level",
       y = "Count",
       fill = "Smoking Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# compares income level to a person's smoking status. From this we can see that people with higher income are more likely to smoke. 
