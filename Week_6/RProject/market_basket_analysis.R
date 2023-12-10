library(tidyverse) # data manipulation
library(arules) # mining association rules and frequent itemsets
library(arulesViz) # visualization techniques for association rules
library(knitr) # dynamic report generation
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics
library(lubridate) # work with dates and times


# load the data -----------------------------------------------------------



items <- read_csv2("data/Assignment-1_Data.csv",
  col_types = list(BillNo = col_number()),
  col_select = c(BillNo, Itemname, Date)
)
items <- items[complete.cases(items), ]
items <- items |>
  janitor::clean_names()

write_csv(items, "data/transactions.csv")

trans <- read.transactions("data/transactions.csv", format = "single", cols = c(1, 2), sep = ",", rm.duplicates = TRUE)

pdf("plots/itemfrequencyplot.pdf")
itemFrequencyPlot(trans, topN = 15, type = "absolute", col = "wheat2", xlab = "Item name", ylab = "Frequency(absolute)", main = "Absolute Item Frequency Plot")
dev.off()

pdf("plots/relativefrequencyplot.pdf")
itemFrequencyPlot(trans, topN = 15, type = "relative", col = "lightcyan2", xlab = "Item name", ylab = "Frequency (relative)", main = "Relative Item Frequency Plot")
dev.off()

# Transactions per month --------------------------------------------------

trans_csv <- read_csv("data/transactions.csv")

trans_csv <- trans_csv |>
  mutate(date = as.POSIXct(date, format = "%d.%m.%Y %H:%M", tz = Sys.timezone()))

trans_csv |>
  mutate(month = as.factor(month(date))) |>
  group_by(month) |>
  summarise(transactions = n_distinct(bill_no)) |>
  ggplot(aes(x = month, y = transactions)) +
  geom_bar(stat = "identity", fill = "mistyrose2", show.legend = FALSE, colour = "black") +
  geom_label(aes(label = transactions)) +
  labs(title = "Transactions per month") +
  theme_bw()
ggsave("plots/transactions_per_month.pdf")


# Transactions per weekday ------------------------------------------------

trans_csv |>
  mutate(week_day = as.factor(weekdays(as.Date(date)))) |>
  group_by(week_day) |>
  summarise(transactions = n_distinct(bill_no)) |>
  ggplot(aes(x = week_day, y = transactions)) +
  geom_bar(stat = "identity", fill = "peachpuff2", show.legend = FALSE, colour = "black") +
  geom_label(aes(label = transactions)) +
  labs(title = "Transactions per weekday") +
  scale_x_discrete(limits = c(
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday"
  )) +
  theme_bw()
ggsave("plots/transactions_per_weekday.pdf")

# Transactions per hour ---------------------------------------------------


trans_csv |>
  mutate(hour = as.factor(hour(date))) |>
  group_by(hour) |>
  summarise(transactions = n_distinct(bill_no)) |>
  ggplot(aes(x = hour, y = transactions)) +
  geom_bar(stat = "identity", fill = "steelblue1", show.legend = FALSE, colour = "black") +
  labs(title = "Transactions per hour") +
  theme_bw()
ggsave("plots/transactions_per_hour.pdf")


# Apriori algorithm -------------------------------------------------------

supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers
rules_sup10 <- integer(length = 9)
rules_sup5 <- integer(length = 9)
rules_sup1 <- integer(length = 9)
rules_sup0.5 <- integer(length = 9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  rules_sup10[i] <- length(apriori(trans, parameter = list(
    sup = supportLevels[1],
    conf = confidenceLevels[i], target = "rules"
  )))
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  rules_sup5[i] <- length(apriori(trans, parameter = list(
    sup = supportLevels[2],
    conf = confidenceLevels[i], target = "rules"
  )))
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  rules_sup1[i] <- length(apriori(trans, parameter = list(
    sup = supportLevels[3],
    conf = confidenceLevels[i], target = "rules"
  )))
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  rules_sup0.5[i] <- length(apriori(trans, parameter = list(
    sup = supportLevels[4],
    conf = confidenceLevels[i], target = "rules"
  )))
}

# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10,
  geom = c("point", "line"),
  xlab = "Confidence level", ylab = "Number of rules found",
  main = "Apriori with a support level of 10%"
) +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5,
  geom = c("point", "line"),
  xlab = "Confidence level", ylab = "Number of rules found",
  main = "Apriori with a support level of 5%"
) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1,
  geom = c("point", "line"),
  xlab = "Confidence level", ylab = "Number of rules found",
  main = "Apriori with a support level of 1%"
) +
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5,
  geom = c("point", "line"),
  xlab = "Confidence level", ylab = "Number of rules found",
  main = "Apriori with a support level of 0.5%"
) +
  scale_y_continuous(breaks = seq(0, 130, 20)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)


# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data = num_rules, aes(x = confidenceLevels)) +

  # Plot line and points (support level of 10%)
  geom_line(aes(y = rules_sup10, colour = "Support level of 10%")) +
  geom_point(aes(y = rules_sup10, colour = "Support level of 10%")) +

  # Plot line and points (support level of 5%)
  geom_line(aes(y = rules_sup5, colour = "Support level of 5%")) +
  geom_point(aes(y = rules_sup5, colour = "Support level of 5%")) +

  # Plot line and points (support level of 1%)
  geom_line(aes(y = rules_sup1, colour = "Support level of 1%")) +
  geom_point(aes(y = rules_sup1, colour = "Support level of 1%")) +

  # Plot line and points (support level of 0.5%)
  geom_line(aes(y = rules_sup0.5, colour = "Support level of 0.5%")) +
  geom_point(aes(y = rules_sup0.5, colour = "Support level of 0.5%")) +

  # Labs and theme
  labs(
    x = "Confidence levels", y = "Number of rules found",
    title = "Apriori algorithm with different support levels"
  ) +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave("plots/supports.pdf")


# apriori execution -------------------------------------------------------


rules_sup1_conf80 <- apriori(trans, parameter = list(supp = supportLevels[3], conf = confidenceLevels[2], target = "rules"))

inspect(rules_sup1_conf80)

# Scatter plot
plot(rules_sup1_conf80, measure = c("support", "lift"), shading = "confidence")
ggsave("plots/rulesscatterplot.pdf")

# Graph (default layout)
plot(rules_sup1_conf80, method = "graph")
ggsave("plots/graph.pdf")


# Grouped matrix plot
plot(rules_sup1_conf80, method = "grouped")
ggsave("plots/groupedmatrix.pdf")
