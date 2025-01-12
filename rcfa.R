install.packages("arules")
install.packages("dplyr")

library(arules)
library(dplyr)

ratings_path <- "rating.csv"
movies_path <- "movie.csv"

ratings <- read.csv(ratings_path)
movies <- read.csv(movies_path)

categorize_rating <- function(rating) {
  if (rating <= 2) {
    return("Low")
  } else if (rating <= 4) {
    return("Medium")
  } else {
    return("High")
  }
}

ratings$rating_category <- sapply(ratings$rating, categorize_rating)

merged_data <- merge(ratings, movies, by = "movieId")

genres_expanded <- merged_data %>% 
  mutate(genres_split = strsplit(as.character(genres), "|", fixed = TRUE)) %>% 
  unnest(genres_split) %>% 
  mutate(value = 1) %>% 
  spread(genres_split, value, fill = 0)

transactions <- genres_expanded %>% 
  select(-c(rating, rating_category, timestamp, title, genres)) %>% 
  group_by(userId) %>% 
  summarise_all(max)

trans <- as(transactions[-1], "transactions")

frequent_itemsets <- apriori(trans, parameter = list(supp = 0.1, target = "frequent itemsets"))

rules <- list()
min_confidence <- 0.6

for (i in seq_along(frequent_itemsets)) {
  itemset <- frequent_itemsets@items[[i]]
  support <- frequent_itemsets@quality$support[i]
  
  if (length(itemset) > 1) {
    for (j in seq_along(itemset)) {
      antecedent <- setdiff(itemset, itemset[j])
      consequent <- itemset[j]
      
      antecedent_support <- frequent_itemsets@quality$support[which(sapply(frequent_itemsets@items, function(x) setequal(x, antecedent)))]
      
      if (length(antecedent_support) > 0) {
        confidence <- support / antecedent_support
        
        if (confidence >= min_confidence) {
          rules <- append(rules, list(data.frame(
            antecedent = paste(antecedent, collapse = ", "),
            consequent = consequent,
            support = support,
            confidence = confidence
          )))
        }
      }
    }
  }
}

rules_df <- do.call(rbind, rules)

cat("Generated Association Rules:\n")
print(rules_df)