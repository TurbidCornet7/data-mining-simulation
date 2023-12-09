library(tidyverse)
library(arules) # mining association rules and frequent itemsets
library(arulesViz) # visualization techniques for association rules
library(knitr) # dynamic report generation
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics
library(lubridate) # work with dates and times
library(magrittr)
library(jsonlite)
library(igraph)
library(rgl)
library(lattice)

View(X1_Retail_Transactions_Dataset)
View(X2_BasketAnalysis)
View(X3_MBA)
View(X4_Bakery)
View(X5_Groceries)
Sys.setlocale("LC_TIME", "English")

copied_df <- data.frame(X1_Retail_Transactions_Dataset)
View(copied_df)
copied_df$Product <- gsub("'", "\"", copied_df$Product)

# Convert the string representation to a vector of characters
copied_df$Product <- lapply(copied_df$Product, function(x) unlist(unname(fromJSON(x))))

result_3 <- copied_df %>%
  unnest_longer(Product)
print(result_3)
View(result_3)
str(result_3)

result <- result_3 %>%
  unnest_longer(Product) %>%
  mutate(value = 1) %>%
  pivot_wider(
    id_cols = Transaction_ID,
    names_from = Product,
    values_fill = list(0)
  )
View(result)


trans <- as(copied_df$Product, "transactions")

# Absolute Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")



itemFrequencyPlot(trans, topN=15, type="relative", col="wheat2",xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot")


print(itemFrequency(trans))

X1_Retail_Transactions_Dataset %>%
  mutate(Month = as.factor(month(Date))) %>%
  group_by(Month) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Month, y = Transactions)) +
  geom_bar(stat = "identity", fill = "mistyrose2", 
           show.legend = FALSE, color = "black") +
  geom_label(aes(label = Transactions)) +
  labs(title = "Transactions per month") +
  theme_bw()


X1_Retail_Transactions_Dataset %>%
  mutate(WeekDay=as.factor(weekdays(Date))) %>%
  group_by(WeekDay) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x=WeekDay, y=Transactions)) +
  geom_bar(stat="identity", fill="peachpuff2", 
           show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per weekday") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw()

X1_Retail_Transactions_Dataset %>%
  mutate(Hour=as.factor(hour(Date))) %>%
  group_by(Hour) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x=Hour, y=Transactions)) +
  geom_bar(stat="identity", fill="steelblue1", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per hour") +
  theme_bw()

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.025, 0.01, 0.005)

# Empty integers 
rules_sup10 <- integer(length = length(confidenceLevels))
rules_sup5 <- integer(length = length(confidenceLevels))
rules_sup1 <- integer(length = length(confidenceLevels))
rules_sup0.5 <- integer(length = length(confidenceLevels))

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  rules_sup10[i] <- length(apriori(trans, parameter = list(sup = supportLevels[1], 
                                                           conf = confidenceLevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)){
  rules_sup5[i] <- length(apriori(trans, parameter = list(sup = supportLevels[2], 
                                                          conf = confidenceLevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)){
  rules_sup1[i] <- length(apriori(trans, parameter = list(sup = supportLevels[3], 
                                                          conf = confidenceLevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)){
  rules_sup0.5[i] <- length(apriori(trans, parameter = list(sup = supportLevels[4], 
                                                            conf = confidenceLevels[i], target = "rules")))
}

# Combine the results into a data frame
df <- data.frame(
  confidenceLevels = rep(confidenceLevels, 4),
  supportLevels = rep(c(0.1, 0.05, 0.01, 0.005), each = length(confidenceLevels)),
  rules = c(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5),
  support = rep(c("10%", "5%", "1%", "0.5%"), each = length(confidenceLevels))
)

# Create a ggplot
ggplot(df, aes(x = confidenceLevels, y = rules, group = support, color = support)) +
  geom_line() +
  geom_point() +
  facet_wrap(~support, scales = "free_y", ncol = 2) +
  labs(x = "Confidence level", y = "Number of rules found", 
       title = "Apriori Algorithm Results for Different Support Levels") +
  theme_bw()


# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
rules_sup1_conf50 <- apriori(trans, parameter=list(sup=supportLevels[4], 
                                                   conf=confidenceLevels[13], target="rules"))
inspect(rules_sup1_conf50) #PROBLEM, it contains 0

# Scatter plot
plot(rules_sup1_conf50, measure=c("support", "lift"), shading="confidence")

# Graph (default layout)
plot(rules_sup1_conf50, method="graph")

# Graph (circular layout)
plot(rules_sup1_conf50, method="scatterplot")

#plot(rules_sup1_conf50, method="grouped") idk it doesn't work..
