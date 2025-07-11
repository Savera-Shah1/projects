# Reading the data file and assigning column names. 
data <- read.table("FlixIt.dat", header = FALSE, col.names = c("Cust_ID", "Hours_spent", "Children", "Income", "History"))

# Displaying the data with newly assigned column names, and displaying the summary. 
head(data)
summary(data)

#Accessing the Columns from dataset. 
data$Cust_ID
data$Hours_spent
data$Children
data$Income
data$History

# Relationship between Hours of FlixIt streaming and Income
model1 <- lm(Hours_spent ~ Income, data = data)
plot(Hours_spent ~ Income, data = data)
abline(model1)
summary(model1)

# Relationship between Hours of FlixIt streaming and Children
model2 <- lm(Hours_spent ~ Children, data = data)
plot(Hours_spent ~ Children, data = data)
abline(model2)
summary(model2)

# Relationship between Hours of FlixIt streaming and History
model3 <- lm(Hours_spent ~ History, data = data)
plot(Hours_spent ~ History, data = data)
abline(model3)
summary(model3)

# Multiple Regression:
FlixIt.MR <- lm(Hours_spent ~ Children+Income+History, data = data)
summary(FlixIt.MR)
summary(FlixIt.MR)$coefficient

#Coefficient of Partial Determination (CPD). 

# Install heplots and rgl. 
install.packages("rgl", type = "source")
library(heplots)
require(heplots)

# Calculate eta square. 
etasq(FlixIt.MR, anova = TRUE, partial = FALSE)




