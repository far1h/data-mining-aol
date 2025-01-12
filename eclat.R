itemsets <- eclat(
  list_transaction,
  parameter = list(
    support = 0.05,  # Minimum support increased to 5%
    minlen = 2,      # Minimum length of itemsets
    maxlen = 3       # Maximum length of itemsets
  )
)

# Inspect the top 10 itemsets by support
top_itemsets <- head(sort(itemsets, by = "support"), 10)
inspect(top_itemsets)
