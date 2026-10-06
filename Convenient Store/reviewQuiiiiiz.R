order <- read.csv("orders.csv")
product <- read.csv("products.csv")

#a.	Show the Product Price for all Product Department.

boxplot(product_price ~ department, data=product, col= rainbow(length(unique(product$department))))

# b.	Show top 5 department based on its product count. 
# Product which is not in the top 5 department will go into “other” categories. 
# For each area, put label to show percentage of the department mentioned along with department name.

depth <- table(product$department)
depth

ordered_depth <- sort(depth , decreasing = TRUE)
print(ordered_depth)

top5 <- ordered_depth[1:5]
others <-  sum(ordered_depth[-(1:5)])

pie_data <- c(top5 , others = others)

labels <- paste0(names(pie_data), "(",round(pie_data/sum(pie_data)*100,1) , "%)" )

pie(
  pie_data , 
  labels = labels , 
  col = rainbow(length(pie_data)),
  main = "top 5 products"
  
)

##c.	Show the lowest 3 aisle based on its product count. Take only data in which the department is frozen.
frozen_department <- product[product$department == "frozen", ]
print(frozen_department)

frozen_department <- table(frozen_department$aisle)
print(frozen_department)

frozen_department <- sort(frozen_department , decreasing = FALSE)
frozen_department

frozen_department <- frozen_department[1:3]
barplot(frozen_department , col = rainbow(length(frozen_department)))


# a.	Data Preprocessing
# In this phase, some data can't be used for further analysis. Do the following task to cleanse the data:
# •	Remove all product which department is not alcohol
merge <- merge(order , product , by = "product_id")
alcohol <-  merge[merge$department == "alcohol",]
alcohol
# •	Remove all product which aisle is Specialty wines champagnes
alcohol <- alcohol[alcohol$aisle != "specialty wines champagnes",]
alcohol

# •	Remove all duplicated data for the analysis
unique(alcohol)

# b.	Data Transformation
# In this phase, you need to change the data, so it is suitable to be used in the Apriori analysis. Prepare the product data in terms of the product’s name.
basket <-  split(alcohol$product_name, alcohol$order_id)
basket
basket <- lapply(basket , unique)
transaction <- as(basket, "transactions")

# c.	Data Mining
# •	Show frequent product using Apriori algorithm with minimum support: 0.04 based on the data that have already pre-processed
library(arules)
rules <- apriori(
  transaction , 
  parameter = list(
    supp = 0.04 , 
    target = "frequent itemsets"
  )
)

inspect(rules)

# •	Show the association rules using minimum confidence: 0.5 based on the frequent product that resulted from step above.
association_rules <- ruleInduction(
  rules,
  transaction,
  conf = 0.50
)
inspect(association_rules)


