install.packages("arules")
library(arules)

library(readr)
ap_ratings <- read_csv("~/Documents/DM/rating.csv")
ap_mov <- read_csv("~/Documents/DM/movie.csv")

ap_mov <- read.csv("movie.csv")
ap_ratings <- read.csv("rating.csv")

merge_data <- merge(ap_mov, ap_ratings, by = "movieId")
list_transaction <- split(merge_data$title,merge_data$userId)
list_transaction <- as(list_transaction, "transactions")
summary(list_transaction)

frequent_items <- apriori(
  list_transaction, 
  parameter = list(
    support = 0.05,        # Increase support to filter frequent items
    target = "frequent",
    maxlen = 3             # Restrict max itemset size to 3
  )
)

# Get and inspect the top 15 frequent itemsets
top_frequent_items <- head(sort(frequent_items, by = "support"), 15)
inspect(top_frequent_items)

# Adjust apriori parameters for association rules
rules <- apriori(
  list_transaction, 
  parameter = list(
    support = 0.2,         # Increase support to reduce computation
    confidence = 0.5,      # Higher confidence for stronger rules
    maxlen = 3,            # Restrict max itemset size to 3
    target = "rules"
  )
)

# Get and inspect the top 15 rules by confidence
top_rules <- head(sort(rules, by = "confidence"), 15)
inspect(top_rules)
