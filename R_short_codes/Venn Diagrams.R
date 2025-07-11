install.packages("VennDiagram")
# Venn Diagram with three sets A, B, and C
library("VennDiagram")
draw.triple.venn(area1=.5, area2=.4, area3=.25, 
                 n12=.2, n23=0, n13=0, n123=0,
                 col = c("yellow", "blue", "green"), 
                 category = c('A', 'B', 'C'),fill=c("yellow", "blue","green"))

install.packages("dplyr")
library(dplyr)
data <- read.table("prices.txt", header = TRUE)
head(data)
sp500_pricechange <- diff(data$sp500)
print(names(data))
str(data)
tbill_pricechange <- diff(data$tbill)

tbill_movement <- ifelse(tbill_pricechange > 0, "Up", "Down")
sp500_movement <- ifelse(sp500_pricechange > 0, "Up", "Down")

# Create data frame with movements
movement_df <- data.frame(
  Tbill_Movement = tbill_movement,
  SP500_Movement = sp500_movement
)

# Create a contingency table
contingency_table <- table(movement_df$Tbill_Movement, movement_df$SP500_Movement)

# Print the contingency table
print(contingency_table)
addmargins(contingency_table)

#d(i)
# Calculating the daily SP 500 return as a percentage
sp500_return <- (sp500_pricechange / lag(data$sp500[-1])) * 100

# Calculate the relative frequency of SP 500 return > 3%
sp500_return_above_3 <- mean(sp500_return > 3)

# Print the result
print(sp500_return_above_3)

#d(ii)
# Calculating the daily Tbill return as a percentage
tbill_return <- (tbill_pricechange / lag(data$tbill[-1])) * 100

# Calculate the relative frequency for Tbill return < 1% and SP 500 return > 1%
Tbill_relativefrequency <- mean(tbill_return < 1 & sp500_return > 1)

# Print the result
print(Tbill_relativefrequency)
