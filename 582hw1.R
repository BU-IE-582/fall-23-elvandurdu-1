setwd("C:/Users/user/Desktop/all_ticks_wide.csv")
dataw<-read.csv("all_ticks_wide.csv")
library(ggplot2)
library(zoo)
library(quantmod)

#1.
summary(dataw)
plot(dataw)
str(dataw)
head(dataw)
dataw$timestamp <- as.POSIXct(dataw$timestamp, format="%Y-%m-%dT%H:%M:%SZ")
dataw.xts <- xts(dataw[,2:61], order.by = dataw$timestamp)
plot(dataw.xts)
chartSeries(dataw.xts)
missing_count <- sapply(dataw.xts, function(x) sum(is.na(x)))
missing_count
install.packages("imputeTS")
library(imputeTS)
dataw.xts.imputed <- na_interpolation(dataw.xts, option = "linear")
dataw.xts.imputed.scaled<-scale(dataw.xts.imputed)
cor(dataw.xts.imputed)

#2
monthly_stock_prices <- to.period(dataw.xts.imputed, period = "months", OHLC = FALSE)
correlation<- cor(monthly_stock_prices$AKBNK, monthly_stock_prices$GARAN)
correlation
monthly.stock.prices.zoo<-zoo(monthly_stock_prices[,c("AKBNK","GARAN")])
rolling_correlation <- rollapply(monthly.stock.prices.zoo, width = 3, FUN = function(x) cor(x[,1], x[,2]), by.column = FALSE, align = "right") # 3 months moving correlation
rolling_correlation
correlation_data <- data.frame(
  Date = index(rolling_correlation), 
  Correlation = coredata(rolling_correlation)  )
ggplot(correlation_data, aes(x = Date, y = Correlation)) +
  geom_line() +
  labs(title = "Moving Window Correlation Over Time", x = "Date", y = "Correlation") +
  theme_minimal()

#3
stock_matrix <- as.matrix(dataw.xts.imputed)
library(FactoMineR)
pca_result <- PCA(stock_matrix, graph = FALSE)
summary(pca_result)
pca_result$var
plot.PCA(pca_result)

