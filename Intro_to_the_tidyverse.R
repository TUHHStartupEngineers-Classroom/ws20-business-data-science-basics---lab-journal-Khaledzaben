# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes_tbl      <- read_excel("C:/Users/Khale/Desktop/Business Module/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("C:/Users/Khale/Desktop/Business Module/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("C:/Users/Khale/Desktop/Business Module/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")




bikes_tbl <- read_excel("C:/Users/Khale/Desktop/Business Module/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  
  # Separate product category name in main and sub
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # Renaming columns
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 3.0 Examining Data ----
orderlines_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
bike_orderlines_joined_tbl %>% glimpse()
bike_orderlines_joined_tbl

# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  mutate(total.price = price * quantity) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
library(lubridate)

# 6.1 Sales by State ----
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state, total_price) %>%
  
  group_by(state) %>% 
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_state_tbl
sales_by_state_tbl %>%
  
# Step 2 - Visualize

  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "#2DC6D6") + 
  geom_label(aes(label = sales_text)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    subtitle = "Upward Trend",
    x = "",
    y = "Revenue"
  )+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6.2 Sales by Year and State ----

# Step 1 - Manipulate
sales_by_year_loc_1_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state,order_date,total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_loc_1_tbl 


# Step 2 - Visualize
sales_by_year_loc_1_tbl %>%
  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() +
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    subtitle = "Each state has an upward trend",
    fill = "state" 
  ) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

