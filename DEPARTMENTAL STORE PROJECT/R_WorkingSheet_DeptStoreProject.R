getwd()
setwd("D:\\SHUBHO_Mi_Backup\\Applications\\RStudio\\R\\GuidedProjects\\1.DataScience _For_Business_With_R\\DEPARTMENTAL STORE PROJECT")

### STEP_1: Install the packages
install.packages("dplyr")
install.packages("ggplot2")

### STEP_2: Load the data set(first have to set the file as 
# working directory, followed by loading the file)
store <- read.csv("MY DEPARTMENTAL STORE.csv")
# View the data set
View(store)

### STEP_3: Data Manipulation using 'DPLYR' - FIVE functions on display are
# filter(), select(), arrange(), mutate() and summaries()

## 1.FILTER(): Load the DPLYR package
require(dplyr)

# 1.1: Get product information where product_type is 
# 'foodgrains&spices'
store1 <- filter(store, PRODUCT_TYPE == "foodgrains&spices")
View(store1)

# 1.2: Get product information where it belongs to companies 'S' and 'M'
store1 <- filter(store, COMPANY %in% c("S", "M"))
View(store1)

#--# 1.3: Get information of the product where product_category is 'Dry Fruits'
store1 <- filter(store, PRODUCT_CATEGORY == "Dry Fruits")
View(store1)


## 2.SELECT():
# 2.1: Get the information of columns 'selling price', 'company'
store2 <- select(store, SELLING_PRICE, COMPANY)
View(store2)

# 2.2: Get information of columns 2 and 5
store1 <- select(store, c(2,5))
View(store1)

# 2.2: Get information of columns 2 TO 5
store1 <- select(store, 2:5)
View(store1)

#2.3: Get the information of columns starting with 'P'
store1 <- select(store, starts_with('P'))
View(store1)

# 2.4: Get the information of columns ending with 'E'
store1 <- select(store, ends_with('E'))
View(store1)

#--# 2.5: Get the information of columns containing 'E'
store2 <- select(store, contains('E'))
View(store2)


## 3.MUTATE():
# 3.1: Add a column to show profit
store1 <- mutate(store, PROFIT=SELLING_PRICE-COST_PRICE)
View(store1)

# 3.2: Add a column for Profit Percent
store2 <- mutate(store1, PROFIT_PERCENT = PROFIT/COST_PRICE*100 )
View(store2)

# 3.3: Add a column for Net Profit
store3 <- mutate(store2, NET_PROFIT = PROFIT*QUANTITY_DEMANDED)
View(store3)

# 3.4: Let's save the updated file 
write.table(store3, file = "FINAL DEPARTMENTAL STORE.csv", sep = ",")

store<-read.csv("FINAL DEPARTMENTAL STORE.csv")
View(store)


## 4. ARRANGE()
# 4.1: Arrange the dataset in ascending order of Quantity Demanded
store1 <- arrange(store, QUANTITY_DEMANDED)
View(store1)

# 4.2: Arrange the dataset in descending order of Selling Price
store1 <- arrange(store, desc(SELLING_PRICE)) 
View(store1)

## 5. SUMMARIZE()
# 5.1: Find the Average/Mean
summarise(store, AVERAGE = mean(PROFIT, na.rm = TRUE))

# 5.2: Find the Summation
SUM_NET_PROFIT <- summarise(store1, SUM = sum(NET_PROFIT), na.rm = TRUE)
View(SUM_NET_PROFIT)

# 5.3: Find the Minimum
summarise(store, MINIMUM = min(PROFIT, na.rm = TRUE))

# 5.4: Find the Median
summarise(store1, MEDIAN =median(PROFIT,na.rm = TRUE))

# 5.5: Find the Variance
summarise(store1, VARIANCE = var(PROFIT, na.rm = TRUE))

# 5.6: Find the Standard Deviation
summarise(store1, STANDARD_DEVIATION =sd(PROFIT, na.rm = TRUE))

#--# 5.7: Find the Maximum
summarise(store1, MAXIMUM = max(PROFIT, na.rm = TRUE))


### STEP_4: DATA VISUALIZATION using 'GGPLOT2'
# The types of plots covered here are : 
# Scatter Plot, Line Plot, Column Plot and Histogram

## 6. SCATTER PLOT
require(dplyr)
require(ggplot2)

store <- read.csv("FINAL DEPARTMENTAL STORE.csv")
View(store)

# 6.1: Plot for Net_Profit and Company where Cost Price > 10
store %>% filter(COST_PRICE > 10) %>% ggplot(aes(x = COMPANY, y = NET_PROFIT,
                                                 color = PRODUCT_CATEGORY)) + geom_point()

# 6.2: Plot for Profit and Quantity Demanded 
# where Product Type == "hygiene"
store %>% filter(PRODUCT_TYPE == 'hygiene') %>% ggplot(aes(x = QUANTITY_DEMANDED, y = PROFIT,
                                                          color = PRODUCT_CATEGORY)) + geom_point()

# 6.3: Plot for Profit and Quantity Demanded 
# where Product Type == 'beauty products'
store %>% filter(PRODUCT_TYPE == 'beauty products') %>% ggplot(aes(x = QUANTITY_DEMANDED, y = PROFIT, 
                                                               color = PRODUCT_CATEGORY)) + geom_point()

#--# 6.4: Plot for Selling Price and Quantity Demanded
store %>% filter() %>% ggplot(aes(x = QUANTITY_DEMANDED, y = SELLING_PRICE,
                                  color = PRODUCT_CATEGORY)) + geom_point()


## 7. SCATTER PLOT
require(dplyr)
require(ggplot2)

store <- read.csv("FINAL DEPARTMENTAL STORE.csv") 
View(store)

# 7.1: Plot for Average Quantity and Product Type
store %>% group_by(PRODUCT_TYPE) %>% 
  summarise(AVERAGE_QUANTITY = mean(QUANTITY_DEMANDED)) %>%
  ggplot(aes(x = PRODUCT_TYPE, y = AVERAGE_QUANTITY)) + 
  geom_col(width = 0.6, fill = "light green") +
  theme(text = element_text(size = 9))

# 7.2: Plot for Average Net Profit and Product Type
store %>% group_by(PRODUCT_TYPE) %>%
  summarise(AVERAGE_NET_PROFIT = mean(NET_PROFIT)) %>%
  ggplot(aes(x =PRODUCT_TYPE , y = AVERAGE_NET_PROFIT)) + 
  geom_col(width = 0.6 , fill = "light blue") +
  theme(text = element_text(size = 9))

# 7.3: Plot for Average Net Profit and Company
store %>% group_by(COMPANY) %>%
  summarise(AVERAGE_NET_PROFIT = mean(NET_PROFIT)) %>%
  ggplot(aes(x = COMPANY, y = AVERAGE_NEGT_PROFIT)) +
  geom_col(width = 0.6, fill = "violet") +
  theme(text = element_text(size =9))

#--# 7.4: Plot for Average Profit Percent and Product Type
store %>% group_by(PRODUCT_TYPE) %>%
  summarise(AVERAGE_PROFIT_PERCENT =mean(PROFIT_PERCENT, na.rm = TRUE)) %>%
  ggplot(aes(x = PRODUCT_TYPE, y = AVERAGE_PROFIT_PERCENT)) + 
  geom_col(fill = "blue") +theme(text = element_text(size = 9))


## 8. LINE PLOT
require(dplyr)
require(ggplot2)

store < read.csv("FINAL DEPARTMENTAL STORE.csv")
View(store)

# We use group = 1, when we use only ONE VARIABLE for grouping

# 8.1: Following Price-Demand Relationship (Average_Selling_Price Vs. Quantity_Demanded)
store %>% group_by(QUANTITY_DEMANDED) %>% 
  summarise(AVERAGE_SELLING_PRICE = mean(SELLING_PRICE, na.rm = TRUE)) %>%
  ggplot(aes(x = QUANTITY_DEMANDED, y = AVERAGE_SELLING_PRICE, group = 1)) +
  geom_line(color = "purple")

# 8.2: Plot for Average_Quantity & Product_Category
store %>% group_by(PRODUCT_CATEGORY) %>%
  summarise(AVERAGE_QUANTITY = mean(QUANTITY_DEMANDED, na.rm = TRUE)) %>%
  ggplot(aes(x = PRODUCT_CATEGORY, y = AVERAGE_QUANTITY, group = 1)) +
  geom_line(color = "green") + theme(text = element_text(size = 4.5))

# 8.3: Plot for Average_Profit & Product_Category
store %>% group_by(PRODUCT_CATEGORY) %>%
  summarise(AVERAGE_PROFIT = mean(PROFIT, na.rm = TRUE)) %>%
  ggplot(aes(x = PRODUCT_CATEGORY, y = AVERAGE_PROFIT, group = 1)) +
  geom_line(color = "magenta") + theme(text = element_text(size = 4.5))

# 8.4: Different line for each company, plot for Average_Net_Profit 
# and Company
store %>% group_by(PRODUCT_TYPE, COMPANY) %>%
  summarise(AVERAGE_NET_PROFIT = mean(NET_PROFIT, na.rm = TRUE)) %>%
  ggplot(aes(x = PRODUCT_TYPE, y = AVERAGE_NET_PROFIT, group = COMPANY, 
             color = COMPANY)) + geom_line() 

#--# 8.5: Plot Average_Net_Profit and Product_Type where Net_Profit > 100
store %>% filter(NET_PROFIT > 100) %>% group_by(PRODUCT_TYPE) %>%
  summarise(AVERAGE_NET_PROFIT = mean(NET_PROFIT, na.rm = TRUE)) %>%
  ggplot(aes(x = PRODUCT_TYPE, y = AVERAGE_NET_PROFIT, group = 1)) +
  geom_line(color = "red")

## 9. HISTOGRAM
require(dplyr)
require(ggplot2)

store <- read.csv("FINAL DEPARTMENTAL STORE.csv")
View(store)

# 9.1: Histogram for Profit_Percent of Product_Category
store %>% 
  ggplot(aes(x = PROFIT_PERCENT, fill = PRODUCT_CATEGORY)) +
  geom_histogram(binwidth = 30)


# 9.2: Histogram for Quantity_Demanded of Product_Category where 
# Product_Type is "snacks"
store %>%
  filter(PRODUCT_TYPE == "snacks") %>%
  ggplot(aes(x = QUANTITY_DEMANDED, fill = PRODUCT_CATEGORY)) +
  geom_histogram(binwidth = 30)

# 9.3: Histogram for Selling_Price of Product_Category where Product_Type
# is "foodgrains&spices"
store %>% 
  filter(PRODUCT_TYPE == "foodgrains&spices") %>%
  ggplot(aes(x = SELLING_PRICE, fill = PRODUCT_CATEGORY)) +
  geom_histogram(binwidth = 30)


