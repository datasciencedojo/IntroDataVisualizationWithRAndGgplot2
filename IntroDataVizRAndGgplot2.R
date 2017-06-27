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


# Load Titanic titanicing data for analysis. Open in spreadsheet view.
titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)
View(titanic)


# Set up factors.
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)


#
# We'll start our visual analysis of the data focusing on questions
# related to survival rates. Specifically, these questions will use
# the factor (i.e., categorical) variables in the data. Factor data
# is very common in the business context and ggplot2 offers many
# powerful features for visualizing factor data.
#


#
# First question - What was the survival rate? 
#
# As Survived is a factor (i.e., categorical) variable, a bar chart 
# is a great visualization to use.
#
ggplot(titanic, aes(x = Survived)) + 
  geom_bar()

# If you really want percentages.
prop.table(table(titanic$Survived))

# Add some customization for labels and theme.
ggplot(titanic, aes(x = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates")


#
# Second question - What was the survival rate by gender? 
#
# We can use color to look at two aspects (i.e., dimensions)
# of the data simultaneously.
#
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Sex")


#
# Third question - What was the survival rate by class of ticket? 
#
ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Pclass")


#
# Fourth question - What was the survival rate by class of ticket
#                   and gender?
#
# We can leverage facets to further segment the data and enable
# "visual drill-down" into the data.
#
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Pclass and Sex")




#
# Next, we'll move on to visualizing continuous (i.e., numeric)
# data using ggplot2. We'll explore visualizations of single 
# numeric variables (i.e., columns) and also illustrate how
# ggplot2 enables visual drill-down on numeric data.
#


#
# Fifth Question - What is the distribution of passenger ages?
#
# The histogram is a staple of visualizing numeric data as it very 
# powerfully communicates the distrubtion of a variable (i.e., column).
#
ggplot(titanic, aes(x = Age)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age (binwidth = 5)",
       title = "Titanic Age Distribtion")


#
# Sixth Question - What are the survival rates by age?
#
ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age (binwidth = 5)",
       title = "Titanic Survival Rates by Age")

# Another great visualization for this question is the box-and-whisker 
# plot.
ggplot(titanic, aes(x = Survived, y = Age)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age")


#
# Seventh Question - How does gender and age interact with respect to
#                    survival?
#
# The .
#
ggplot(titanic, aes(x = Age, color = Survived)) +
  theme_bw() +
  facet_wrap(~ Sex) +
  geom_freqpoly(binwidth = 5, size = 1.0) +
  ggtitle("Distribution of FareAdj by Pclass")




# Next, we'll visualize the distribution of Fare. Given that
# Fare is a continuous variable, we have a couple of options.
# The first option is histogram.
ggplot(titanic, aes(x = Fare)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Fare (binwidth = 5)",
       title = "Titanic titanicing Fare Distribtion")


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
ggplot(titanic, aes(x = Pclass, y = Fare)) +
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
titanic$FareAdj <- log(1 + titanic$Fare)  / (as.numeric(titanic$Pclass) ^ 2)
ggplot(titanic, aes(x = Pclass, y = FareAdj)) +
  theme_bw() +
  geom_boxplot() +
  ggtitle("Distribution of FareAdj by Pclass")



ggplot(titanic, aes(x = FareAdj, fill = Survived)) +
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_histogram(binwidth = 0.1)


ggplot(titanic, aes(x = FareAdj, color = Survived, fill = Survived)) +
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_freqpoly(binwidth = 0.1, size = 1.0) +
  ggtitle("Distribution of FareAdj by Pclass")



ggplot(titanic, aes(x = FareAdj, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 0.1)


ggplot(titanic, aes(x = FareAdj, color = Survived, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_freqpoly(binwidth = 0.1, size = 1.0) +
  ggtitle("Distribution of FareAdj by Pclass")


