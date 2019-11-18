library(readr)
library(dplyr)
library(lubridate) 
library(ggplot2)
library(zoo)
library(tidyr)
library(purrr)
if (!require("forecast")) install.packages("forecast")


#Read in csv file
data <- read.csv("storedata.csv")

#Q1 Total sales per month of the last 3 months of 2017, Region1 and Region9, for Customer_Segment (Corporate and Consumer) 
q1 <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(Order_Date >= as.Date('2017-10-01') & Order_Date <= as.Date('2017-12-31')) %>%
  filter(Region == "Region 1" | Region == "Region 9") %>%
  filter(Customer_Segment == "Corporate" | Customer_Segment == "Consumer") %>% 
  select(Order_Date, Sales, Region) %>%
  group_by(Region, as.yearmon(Order_Date)) %>%
  summarise(Total_Sales = sum(Sales)) 

colnames(q1)[2] <- "Month_Year"


#Q2 Make a plot of the monthly total Sales in Region 1 and Region 13 in 2015, 2016 and 2017
q2 <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(Order_Date >= as.Date('2015-01-01') & Order_Date <= as.Date('2017-12-31')) %>%
  filter(Region == "Region 1" | Region == "Region 13") %>%
  select(Order_Date, Sales, Region) %>%
  group_by(Region, as.yearmon(Order_Date)) %>%
  summarise(Total_Sales = sum(Sales)) 

colnames(q2)[2] <- "Month_Year"

# show in Figure 1
ggplot(q2, aes(x=Month_Year, y=Total_Sales, color=Region, fill=Region)) + 
  geom_line() + 
  geom_point(alpha=0.4) +
  xlab("Date") + 
  ylab("Total sales per month") + 
  ggtitle("Figure 1 - Monthly sales")

#Q3 In Figure 1, identify the months where the total Sales in Region 13 is greater than the total Sales in Region 1

# show in Table 2
q3 <- q2 %>% 
  spread(Region, Total_Sales) %>% 
  filter(`Region 13` > `Region 1`) #use `` to make the text not a string, but a evaluatable object

#Q4 Find average Profit per Customer_Segment and Product_Category in 2017, for all regions except Region 3, 5 & 8 

# show in Table 3
q4 <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(Order_Date >= as.Date('2017-01-01') & Order_Date <= as.Date('2017-12-31')) %>%
  filter(Region != "Region 3" & Region != "Region 5" & Region != "Region 8") %>% 
  group_by(Customer_Segment, Product_Category) %>% 
  summarise(average_profit = mean(Profit)) %>% 
  arrange(desc(average_profit))

#2. Identify the best SARIMA model on a holdout sample from 2017, based on the smallest RMSE (Root Mean Square Error) 
#3. Produce a plot of the whole time series, and add onto it the 2017 forecast from the best SARIMA model, together with 
#   the actual aggregated monthly Order_Quantity. This output is Figure 2. 
#4. Specify what was the best SARIMA model on the plot

#model(25000)
iterations <- expand.grid(list(
  p = c(0:4), d = c(0:1), q = c(0:4),
  P = c(0:4), D = c(0:1), Q = c(0:4),
  S = 12)) 

q5_data <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(
    Order_Date >= as.Date('2014-01-01') & Order_Date <= as.Date('2016-12-31'),
    Customer_Segment == "Small Business",
    Product_Category == "Office Supplies"
  ) %>% 
  select(Order_Date, Order_Quantity) %>%
  group_by(Order_Date = as.yearmon(Order_Date)) %>%
  summarise(Monthly_Order_Quantity = sum(Order_Quantity)) %>% 
  arrange(Order_Date)

q5_2017 <- data %>%
  mutate(Order_Date = as.Date(data$Order_Date, format = "%Y-%m-%d")) %>%
  filter(
    Order_Date >= as.Date('2017-01-01') & Order_Date <= as.Date('2017-12-31'),
    Customer_Segment == "Small Business",
    Product_Category == "Office Supplies"
  ) %>% 
  select(Order_Date, Order_Quantity) %>%
  group_by(Order_Date = as.yearmon(Order_Date)) %>%
  summarise(Monthly_Order_Quantity = sum(Order_Quantity)) %>% 
  arrange(Order_Date)

min_rmse <- Inf #set to +infinite to start
rmse_func = function(pred, label) sqrt(sum((pred - label) ^ 2)) #RMSE formula

#The loop
for(p in 0:4) for (d in 0:1) for (q in 0:4) for (P in 0:2) for (D in 0:1) for (Q in 0:4) {
  tryCatch({ #work around errors 
    arima_model <- 
      arima(as.numeric(q5_data$Monthly_Order_Quantity), 
            order = c(p, d, q), 
            seasonal = list(order = c(P, D, Q), period = 12))   
    
    #forecast (for 2017)
    variable <- forecast(object = arima_model, h = 12)
    prediction_2017 <- variable$mean
    
    #arrange RMSE (forecast, actual) ⇒ Best model
    rmse <- rmse_func(prediction_2017, q5_2017$Monthly_Order_Quantity)
    
    if(rmse < min_rmse) {
      min_rmse <- rmse
      best_model <- arima_model
      print(min_rmse)
      
    }
    
  }, error = function(e){
    print(paste(p,d,q,P,D,Q))
  })
  
}

TS <- ts(q5_data$Monthly_Order_Quantity, 
         start = c(2014,1), end = c(2016,12), frequency = 12) 
  
p <- TS %>% 
  forecast() %>% 
  autoplot(xlab = "Month-Year", 
           ylab = "Monthly Order Quantity") +
  autolayer(ts(q5_2017$Monthly_Order_Quantity, 
               start=c(2017,1), end=c(2017,12), frequency=12), 
            series="Actual data") 
  
p

best_model


