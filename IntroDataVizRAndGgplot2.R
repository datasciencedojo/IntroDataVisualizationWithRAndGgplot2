#
# Copyright 2017 Data Science Dojo
#    
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 


#
# This R source code file corresponds to the Data Science Dojo webinar 
# titled "An Introduction to Data Visualization with R and ggplot2" 
#

#install.packages("ggplot2")
library(ggplot2)


# Load Titanic training data for analysis.
train <- read.csv("train.csv", stringsAsFactors = FALSE)


# Set up factors.
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)


# Start simple - understanding survival rates all-up. 
#
# As Survived is a factor (i.e., categorical) variables,
# a bar chart is a perfect visualization to use.
#
ggplot(train, aes(x = Survived)) + 
  geom_bar()

# If you really want percentages.
prop.table(table(train$Survived))


# Add some customization for labels and theme.
ggplot(train, aes(x = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Training Data Survival Rates")


# Next, we'll visualize the distribution of Fare. Given that
# Fare is a continuous variable, we have a couple of options.
# The first option is histogram.
ggplot(train, aes(x = Fare)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Fare (binwidth = 5)",
       title = "Titanic Training Fare Distribtion")


# We can also use a box and whiskers plot to visualize 
# continuous numeric data. With ggplot2 you have to specify
# a value for x that acts as a grouping/category.
#
# This is our first example of segmenting the data. Stated
# another way, this is an example of visually drilling into
# the data.
#
# Let's look at Fare segmented by Pclass.
#
ggplot(train, aes(x = Pclass, y = Fare)) +
  theme_bw() +
  geom_boxplot() +
  ggtitle("Distribution of Fare by Pclass")


# Our intuition tells use that there shound't be overlap 
# between 1st class Fares and 3rd class fares. Let's 
# engineer a quick feature to illustrate one method of 
# achieving this intuitive separation.
#
# NOTE - In a real world scenario you would analyze the
#        3rd class outliers in more detail to understand
#        what's going on in the data.
#
train$FareAdj <- log(1 + train$Fare)  / (as.numeric(train$Pclass) ^ 2)
ggplot(train, aes(x = Pclass, y = FareAdj)) +
  theme_bw() +
  geom_boxplot() +
  ggtitle("Distribution of FareAdj by Pclass")



ggplot(train, aes(x = FareAdj, fill = Survived)) +
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_histogram(binwidth = 0.1)


ggplot(train, aes(x = FareAdj, color = Survived, fill = Survived)) +
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_freqpoly(binwidth = 0.1, size = 1.0) +
  ggtitle("Distribution of FareAdj by Pclass")



ggplot(train, aes(x = FareAdj, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 0.1)


ggplot(train, aes(x = FareAdj, color = Survived, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_freqpoly(binwidth = 0.1, size = 1.0) +
  ggtitle("Distribution of FareAdj by Pclass")


