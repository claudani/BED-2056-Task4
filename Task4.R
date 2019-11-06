library(readr)
library(dplyr)
library(lubridate) 
library(ggplot2)

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
  filter(Region == "Region 1" | Region == "Region 13")
#TODO plot by month into Figure 1, different colours for the 3 years? Like gcp's billing?

#Q3 In Figure 1, identify the months where the total Sales in Region 13 is greater than the total Sales in Region 1
#TODO show in Table 2
  #TODO for each month (regardless of the year?) -> 'Sales' in Region 13 > 'Sales' in Region 1

#Q4 Find average Profit per Customer_Segment and Product_Category in 2017, for all regions except Region 3, 5 & 8 
#TODO - What segment produced the highest average profit? 
#TODO show in Table 3
q4 <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(Order_Date >= as.Date('2017-01-01') & Order_Date <= as.Date('2017-12-31')) %>%
  filter(Region != "Region 3" | Region != "Region 5" | Region != "Region 8")
#TODO mean by group_by Customer_Segment and Product_Category -> max (per il secondo todo)

#Q5 You are asked to estimate a SARIMA model on the aggregated monthly Order_Quantity in the: 
# Customer_Segment; Small Business and Product_Category; Office Supplies
#TODO - read more info on the task page + run on here and if it needs 30 minutes, then run on PC's in the uni's lab


