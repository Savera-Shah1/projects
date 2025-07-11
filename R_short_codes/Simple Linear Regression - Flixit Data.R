# Read the .dat file and assign custom column names
data <- read.table("FlixIt.dat", header = FALSE, col.names = c("Cust_ID", "Hours_spent", "Children", "Income", "History"))

# Display the data with custom column names
head(data)
summary(data)

#Assigning the Columns Names
Cust_ID <- data$Cust_ID
Hours_spent <- data$Hours_spent
Children <- data$Children
Income <- data$Income
History <- data$History

# Relationship between Hours of FlixIt streaming and Income
model1 <- lm(Hours_spent ~ Income, data = data)
plot(Hours_spent ~ Income, data = data)
abline(model1)
summary(model1)

abline

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

#Defining the Regression Equation for Income and Hours of FlixIt streaming
y <- Hours_spent
income_intercept <- 82.2130
income_coef <- -0.1850

income_value <- 53
y <- income_intercept + (income_coef * income_value)
print(y)


#Defining the Regression Equation for Children and Hours of FlixIt streaming
Children_intercept <- 53.905
Children_coef <- 6.005

Children_value <- 2

y <- Children_intercept + (Children_coef * Children_value)
print(y)

#Defining the Regression Equation for History and Hours of FlixIt streaming
History_intercept <- 24.301
History_coef <- 8.700

History_value <- 3

y <- History_intercept + (History_coef * History_value)
print(y)
