
# Food Sales and Customer Preference Analysis

# Load the dataset
food_sales <- read.csv("food_sales.csv")

# Calculate total revenue for each transaction
food_sales$Revenue <- food_sales$Quantity * food_sales$UnitPrice

# Display the dataset
print(food_sales)

# Show the total number of transactions
total_transactions <- nrow(food_sales)
print(total_transactions)

# Calculate total revenue
total_revenue <- sum(food_sales$Revenue)
print(total_revenue)

# Find the best-selling products
product_sales <- aggregate(
  Quantity ~ Product,
  data = food_sales,
  sum
)

product_sales <- product_sales[
  order(-product_sales$Quantity),
]

print(product_sales)

# Find revenue by food category
category_sales <- aggregate(
  Revenue ~ Category,
  data = food_sales,
  sum
)

category_sales <- category_sales[
  order(-category_sales$Revenue),
]

print(category_sales)

