library(tidyverse)
library(arules) # mining association rules and frequent itemsets
library(arulesViz) # visualization techniques for association rules
library(knitr) # dynamic report generation
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics
library(lubridate) # work with dates and times
library(magrittr)
library(jsonlite)

View(X1_Retail_Transactions_Dataset)
View(X2_BasketAnalysis)
View(X3_MBA)
View(X4_Bakery)
View(X5_Groceries)


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


transactions <- as(copied_df$Product, "transactions")

itemFrequencyPlot(transactions_3, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")



