library(readr)
library(dplyr)
library(lubridate) 
library(ggplot2)
library(zoo)
library(tidyr)

#Read in csv file
data <- read.csv("storedata.csv")

#Q1 Last 3 months of 2017, Region1 and Region9, for Customer_Segment (Corporate and Consumer) 
#TODO show in Table 1
q1 <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(Order_Date >= as.Date('2017-10-01') & Order_Date <= as.Date('2017-12-31')) %>%
  filter(Region == "Region 1" | Region == "Region 9") %>%
  filter(Customer_Segment == "Corporate" | Customer_Segment == "Consumer")
  
#Q2 Make a plot of the monthly total Sales in Region 1 and Region 13 in 2015, 2016 and 2017
q2 <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(Order_Date >= as.Date('2015-01-01') & Order_Date <= as.Date('2017-12-31')) %>%
  filter(Region == "Region 1" | Region == "Region 13") %>%
  select(Order_Date, Sales, Region) %>%
  group_by(Region, as.yearmon(Order_Date)) %>%
  summarise(Total_Sales = sum(Sales)) 

colnames(q2)[2] <- "Month_Year"

#TODO show in Figure 1
ggplot(q2, aes(x=Month_Year, y=Total_Sales, color=Region, fill=Region)) + 
  geom_line() + 
  geom_point(alpha=0.4) +
  xlab("Date") + 
  ylab("Total sales per month") + 
  ggtitle("Figure 1 - Monthly sales")

#Q3 In Figure 1, identify the months where the total Sales in Region 13 is greater than the total Sales in Region 1

#TODO show in Table 2
q3 <- q2 %>% 
  spread(Region, Total_Sales) %>% 
  filter(`Region 13` > `Region 1`) #use `` to make the text not a string, but a evaluatable object
  
#Q4 Find average Profit per Customer_Segment and Product_Category in 2017, for all regions except Region 3, 5 & 8 

#TODO show in Table 3
q4 <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(Order_Date >= as.Date('2017-01-01') & Order_Date <= as.Date('2017-12-31')) %>%
  filter(Region != "Region 3" & Region != "Region 5" & Region != "Region 8") %>% 
  group_by(Customer_Segment, Product_Category) %>% 
  summarise(average_profit = mean(Profit)) %>% 
  arrange(desc(average_profit))

#Q5 You are asked to estimate a SARIMA model on the aggregated monthly Order_Quantity in the: 
# Customer_Segment; Small Business and Product_Category; Office Supplies
#TODO - read more info on the task page + run on here and if it needs 30 minutes, then run on PC's in the uni's lab


