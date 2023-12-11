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
#View(X2_BasketAnalysis)
#View(X3_MBA)
#View(X4_Bakery)
#View(X5_Groceries)
Sys.setlocale("LC_TIME", "English")

copied_df <- data.frame(X1_Retail_Transactions_Dataset)
View(copied_df)
copied_df$Product <- gsub("'", "\"", copied_df$Product)

# Convert the string representation to a vector of characters
copied_df$Product <- lapply(copied_df$Product, function(x) unlist(unname(fromJSON(x))))

category_values <- unique(copied_df$Customer_Category)
print(category_values)

cities_values <- unique(copied_df$City)
print(cities_values)


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
pdf("plots2/itemfrequencyplot.pdf")
# Absolute Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")
dev.off()

pdf("plots2/relativefrequencyplot.pdf")
itemFrequencyPlot(trans, topN=15, type="relative", col="wheat2",xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot")
dev.off()


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
ggsave("plots2/transactions_per_month.pdf")


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
ggsave("plots2/transactions_per_weekday.pdf")

X1_Retail_Transactions_Dataset %>%
  mutate(Hour=as.factor(hour(Date))) %>%
  group_by(Hour) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x=Hour, y=Transactions)) +
  geom_bar(stat="identity", fill="steelblue1", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per hour") +
  theme_bw()
ggsave("plots2/transactions_per_hour.pdf")


copied_df %>%
  mutate(Month = as.factor(month(Date))) %>%
  group_by(City, Month) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Month, y = Transactions, color = City, group = City)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per month") +
  theme_bw()
ggsave("plots2/transactions_per_month_lineplot.pdf")

copied_df %>%
  mutate(WeekDay=as.factor(weekdays(Date))) %>%
  group_by(WeekDay, City) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = WeekDay, y = Transactions, color = City, group = City)) +
  geom_line() +
  geom_point() + 
  labs(title="Transactions per weekday") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw()
ggsave("plots2/transactions_per_day_lineplot.pdf")

copied_df %>%
  mutate(Hour=as.factor(hour(Date))) %>%
  group_by(Hour, City) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Hour, y = Transactions, color = City, group = City)) +
  geom_line() +
  geom_point() + 
  labs(title="Transactions per hour") +
  theme_bw()
ggsave("plots2/transactions_per_hour_lineplot.pdf")


# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.001)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.025, 0.01, 0.001)

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
  supportLevels = rep(c(0.1, 0.05, 0.01, 0.001), each = length(confidenceLevels)),
  rules = c(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5),
  support = rep(c("10%", "5%", "1%", "0.1%"), each = length(confidenceLevels))
)

# Create a ggplot
ggplot(df, aes(x = confidenceLevels, y = rules, group = support, color = support)) +
  geom_line() +
  geom_point() +
  facet_wrap(~support, scales = "free_y", ncol = 2) +
  labs(x = "Confidence level", y = "Number of rules found", 
       title = "Apriori Algorithm Results for Different Support Levels") +
  theme_bw()
ggsave("plots2/apriori_differentResults.pdf")


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
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.1%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.1%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave("plots2/apriori_differentResultsSameFigure.pdf")

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
rules_sup1_conf50 <- apriori(trans, parameter=list(sup=supportLevels[4], 
                                                   conf=confidenceLevels[13], target="rules"))
inspect(rules_sup1_conf50) #PROBLEM, it contains 0

# Scatter plot
plot(rules_sup1_conf50, measure=c("support", "lift"), shading="confidence")
ggsave("plots2/rulesscatterplot.pdf")

# Graph (default layout)
plot(rules_sup1_conf50, method="graph")
ggsave("plots2/graph.pdf")

# Graph (circular layout)
plot(rules_sup1_conf50, method="scatterplot")
ggsave("plots2/rulesscatterplotConfidence.pdf")

plot(rules_sup1_conf50, method="grouped") #idk it doesn't work..
ggsave("plots2/grouped.pdf")

##############################
#Group By according New York
#############################
nyc <- copied_df %>% group_by(City) %>% filter (City == "New York") %>% ungroup()
trans_nyc <- as(nyc$Product, "transactions")
# Absolute Item Frequency Plot
pdf("plots2/itemfrequencyplotNYC.pdf")
itemFrequencyPlot(trans_nyc, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot New York City")
dev.off()

pdf("plots2/relativefrequencyplotNYC.pdf")
itemFrequencyPlot(trans_nyc, topN=15, type="relative", col="wheat2",xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot New York City")
dev.off()

nyc %>%
  mutate(Month = as.factor(month(Date))) %>%
  group_by(Month) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Month, y = Transactions)) +
  geom_bar(stat = "identity", fill = "mistyrose2", 
           show.legend = FALSE, color = "black") +
  geom_label(aes(label = Transactions)) +
  labs(title = "Transactions per month NYC") +
  theme_bw()
ggsave("plots2/transactions_per_monthNYC.pdf")


nyc %>%
  mutate(WeekDay=as.factor(weekdays(Date))) %>%
  group_by(WeekDay) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x=WeekDay, y=Transactions)) +
  geom_bar(stat="identity", fill="peachpuff2", 
           show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per weekday NYC") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw()
ggsave("plot2s/transactions_per_weekdayNYC.pdf")

nyc %>%
  mutate(Hour=as.factor(hour(Date))) %>%
  group_by(Hour) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x=Hour, y=Transactions)) +
  geom_bar(stat="identity", fill="steelblue1", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per hour NYC") +
  theme_bw()
ggsave("plots2/transactions_per_hourNYC.pdf")


##############################
#Group By according Miami
#############################
mia <- copied_df %>% group_by(City) %>% filter (City == "Miami") %>% ungroup()
trans_mia <- as(la$Product, "transactions")

# Absolute Item Frequency Plot
pdf("plots2/itemfrequencyplotMIA.pdf")
itemFrequencyPlot(trans_mia, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot Miami")
dev.off()

pdf("plots2/relativefrequencyplotMIA.pdf")
itemFrequencyPlot(trans_mia, topN=15, type="relative", col="wheat2",xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot Miami")
dev.off()

mia %>%
  mutate(Month = as.factor(month(Date))) %>%
  group_by(Month) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Month, y = Transactions)) +
  geom_bar(stat = "identity", fill = "mistyrose2", 
           show.legend = FALSE, color = "black") +
  geom_label(aes(label = Transactions)) +
  labs(title = "Transactions per month MIA") +
  theme_bw()
ggsave("plots2/transactions_per_monthMIA.pdf")


mia %>%
  mutate(WeekDay=as.factor(weekdays(Date))) %>%
  group_by(WeekDay) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x=WeekDay, y=Transactions)) +
  geom_bar(stat="identity", fill="peachpuff2", 
           show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per weekday MIA") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw()
ggsave("plots2/transactions_per_dayMIA.pdf")

mia %>%
  mutate(Hour=as.factor(hour(Date))) %>%
  group_by(Hour) %>%
  summarise(Transactions=n_distinct(Transaction_ID)) %>%
  ggplot(aes(x=Hour, y=Transactions)) +
  geom_bar(stat="identity", fill="steelblue1", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per hour MIA") +
  theme_bw()
ggsave("plots2/transactions_per_hourMIA.pdf")

##########################################################
combined_df <- bind_rows(nyc, mia)
combined_df$City <- as.factor(combined_df$City)
#names(combined_df)
str(combined_df)
#view(combined_df)

combined_df %>%
  mutate(Month = as.factor(month(Date))) %>%
  group_by(City, Month) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Month, y = Transactions, color = City, group = City)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per month Comparison") +
  theme_bw()
ggsave("plots2/transactions_per_month_lineplotNYCMIA.pdf")

combined_df %>%
  mutate(WeekDay=as.factor(weekdays(Date))) %>%
  group_by(City, WeekDay) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = WeekDay, y = Transactions, color = City, group = City)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per Day Comparison") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw()
ggsave("plots2/transactions_per_day_lineplotNYCMIA.pdf")

combined_df %>%
  mutate(Hour=as.factor(hour(Date))) %>%
  group_by(City, Hour) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Hour, y = Transactions, color = City, group = City)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per hour Comparison") +
  theme_bw()
ggsave("plots2/transactions_per_hour_lineplotNYCMIA.pdf")


###########################################################
#Now we do the plots according to the jobs!
#########################################################

copied_df$Customer_Category <- as.factor(copied_df$Customer_Category)

copied_df %>%
  mutate(Month = as.factor(month(Date))) %>%
  group_by(Customer_Category, Month) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Month, y = Transactions, color = Customer_Category, group = Customer_Category)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per month") +
  theme_bw()
ggsave("plots2/transactions_per_month_lineplotCategories.pdf")

copied_df %>%
  mutate(WeekDay=as.factor(weekdays(Date))) %>%
  group_by(Customer_Category, WeekDay) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = WeekDay, y = Transactions, color = Customer_Category, group = Customer_Category)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per Day") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw()
ggsave("plots2/transactions_per_day_lineplotCategories.pdf")

copied_df %>%
  mutate(Hour=as.factor(hour(Date))) %>%
  group_by(Customer_Category, Hour) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Hour, y = Transactions, color = Customer_Category, group = Customer_Category)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per hour") +
  theme_bw()
ggsave("plots2/transactions_per_hour_lineplotCategories.pdf")

####################################################################

teenager <- copied_df %>% group_by(Customer_Category) %>% filter (Customer_Category == "Teenager") %>% ungroup()
middle_aged <- copied_df %>% group_by(Customer_Category) %>% filter (Customer_Category == "Middle-Aged") %>% ungroup()
retiree <- copied_df %>% group_by(Customer_Category) %>% filter (Customer_Category == "Retiree") %>% ungroup()
combined_cs <- bind_rows(middle_aged,retiree,teenager)

combined_cs %>%
  mutate(Month = as.factor(month(Date))) %>%
  group_by(Customer_Category, Month) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Month, y = Transactions, color = Customer_Category, group = Customer_Category)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per month") +
  theme_bw()
ggsave("plots2/transactions_per_month_lineplotCategories2.pdf")


combined_cs %>%
  mutate(WeekDay=as.factor(weekdays(Date))) %>%
  group_by(Customer_Category, WeekDay) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = WeekDay, y = Transactions, color = Customer_Category, group = Customer_Category)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per Day") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw()
ggsave("plots2/transactions_per_day_lineplotCategories2.pdf")

combined_cs %>%
  mutate(Hour=as.factor(hour(Date))) %>%
  group_by(Customer_Category, Hour) %>%
  summarise(Transactions = n_distinct(Transaction_ID)) %>%
  ggplot(aes(x = Hour, y = Transactions, color = Customer_Category, group = Customer_Category)) +
  geom_line() +
  geom_point() + 
  #geom_label(aes(label = Transactions), show.legend = FALSE) +
  labs(title = "Transactions per hour") +
  theme_bw()
ggsave("plots2/transactions_per_hour_lineplotCategories.pdf")

#############################################
pdf("plots2/itemfrequencyplotTeenager.pdf")
trans_teenager <- as(teenager$Product, "transactions")
itemFrequencyPlot(trans_teenager, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot for Teeneger")

dev.off()

pdf("plots2/relativefrequencyplotTeenager.pdf")
itemFrequencyPlot(trans_teenager, topN=15, type="relative", col="wheat2",xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot for Teeneger")
dev.off()
############################################################
pdf("plots2/itemfrequencyplotMiddleAged.pdf")
trans_middle_aged <- as(middle_aged$Product, "transactions")
itemFrequencyPlot(trans_middle_aged, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot for Middle Aged People")
dev.off()

pdf("plots2/relativefrequencyplotMiddleAged.pdf")
itemFrequencyPlot(trans_middle_aged, topN=15, type="relative", col="wheat2",xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot for Middle Aged People")
dev.off()

###########################################################
pdf("plots2/itemfrequencyplotRetiree.pdf")
trans_retiree <- as(retiree$Product, "transactions")
itemFrequencyPlot(trans_retiree, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot for Retiree")
dev.off()

pdf("plots2/itemfrequencyplottRetiree.pdf")
itemFrequencyPlot(trans_retiree, topN=15, type="relative", col="wheat2",xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot for Retiree")
dev.off()

##########################################################

