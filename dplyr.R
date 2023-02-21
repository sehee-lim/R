# package
library(magrittr)

# load data
data(diamonds, package = "ggplot2")
head(diamonds)

# Example 1
head(diamonds, n = 10)$depth

diamonds %>%
  head(., n = 10) %>% 
  .$depth


# Example2
diamonds$price[1:15]

diamonds %>% 
  .$price %>%
  .[1:15]


# package
library(dplyr)


# slice
diamonds %>% 
  slice(5:10)



# filter
diamonds %>% 
  filter(color == "J")

diamonds %>% 
  filter(color == "J",
         depth < 60)



# select
diamonds %>% 
  select(3)

diamonds %>% 
  select(color)

diamonds %>% 
  select(color, price)

# can use : with column names
diamonds %>%    # useful when we do not know the position
  select(color:price)

# if not
diamonds[, which(colnames(diamonds) == "color"):which(colnames(diamonds) == "price")]


diamonds %>% 
  select(starts_with("c"))   # columns starting with 'c'

diamonds %>% 
  select(ends_with("t"))   # columns ending with 't'

diamonds %>% 
  select(contains("p"))   # columns containing 'p'

# drop column
diamonds %>%
  select(-price)

diamonds %>% 
  select(-color, -price)

diamonds %>% 
  select(-starts_with("c"))   # drop columns starting with 'c'




# mutate
diamonds %>% 
  mutate(x_plus_y = x + y)   # add new column

diamonds %>% 
  mutate(x_minus_y = x - y, y_plus_z = y + z)

head(diamonds)   # not updated



# arrange
diamonds %>% 
  arrange(price)   # order by price

diamonds %>% 
  arrange(price, depth)   # order by price and depth

diamonds %>%
  arrange(desc(price))   # order by price (descending)




# group_by
diamonds %>% 
  group_by(color) %>%    # group by color
  summarise(mean_price = mean(price))   # average price by color

diamonds %>% 
  group_by(color) %>%    # group by color
  summarise(mean_price = mean(price),   # average price by color
            cnt_price = n())   # number of diamonds by color
  
