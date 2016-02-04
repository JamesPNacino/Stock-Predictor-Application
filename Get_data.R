##Install and load required packages
##install.packages("quantmod")
library(quantmod)
library(reshape2)



##Merge the NASDAQ and NYSE csv files. Files found at http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download
##and http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nyse&render=download
##Bind new column to these datasets to include stock type of NASDAQ or NYSE
NASDAQ <- read.csv("NASDAQ.csv")
NASDAQ <- cbind(NASDAQ, "Stock.Exchange"="NASDAQ")
NYSE <- read.csv("NYSE.csv")
NYSE <- cbind(NYSE, "Stock.Exchange"="NYSE")
Stocks_df <- data.frame(rbind(NASDAQ, NYSE))
##write.csv(Stocks_df, "AllStocks.csv", row.names=FALSE)



##Create function to get stock prices/info using a function and bind industry/sector info 
stockprices <- function(symbol){
        TICKERS.DF <- data.frame(getSymbols(symbol, from=Sys.Date()-657, to=Sys.Date(), env=NULL))
        TICKERS.DF <- cbind(TICKERS.DF, "Date"=attr(TICKERS.DF , "row.names"))
        TICKERS.DF <- cbind(TICKERS.DF, "PctChange"=round(((TICKERS.DF[,4]-TICKERS.DF[,1])/TICKERS.DF[,1]*100), digits=4))
        IndustryInfo <- Stocks_df[Stocks_df$Symbol==symbol,]
        TICKERS.DF <- cbind(TICKERS.DF, "Sector"=IndustryInfo$Sector)
        TICKERS.DF <- cbind(TICKERS.DF, "Industry"=IndustryInfo$industry)
        TICKERS.DF <- cbind(TICKERS.DF, "Stock.Exchange"=IndustryInfo$Stock.Exchange)
        TICKERS.DF$Date <- as.Date(TICKERS.DF$Date)
        TICKERS.DF
}


##Create function to get stock ratios, make sure to load 'reshape2' package
stockratios <- function(symbol=""){
        finances <- getFinancials(symbol, env=NULL)
        BalanceSheet <- data.frame(viewFinancials(finances, type='BS', period="Q"))
        BalanceSheet <- cbind(BalanceSheet, "Variables"=attr(BalanceSheet, "row.names"))
        BalanceSheet <- cbind(BalanceSheet, "Statement"="Balance Sheet")
        BalanceSheet <- melt(BalanceSheet)
        
        IncomeStatement <- data.frame(viewFinancials(finances, type='IS', period="Q"))
        IncomeStatement <- cbind(IncomeStatement, "Variables"=attr(IncomeStatement, "row.names"))
        IncomeStatement <- cbind(IncomeStatement, "Statement"="Income Statement")
        IncomeStatement <- melt(IncomeStatement)
        
        finances <- rbind(BalanceSheet, IncomeStatement)
        finances$variable <- sub("X", "", finances$variable)
        finances$variable <- gsub("\\.", "-", finances$variable)
        colnames(finances)[3] <- "Date"
        
        NetIncome <- finances[finances$Variables=="Net Income",]
        TotalAssets <- finances[finances$Variables=="Total Assets",]
        TotalEquity <- finances[finances$Variables=="Total Equity",]
        TotalRevenue <- finances[finances$Variables=="Total Revenue",]
        CurrentAssets <- finances[finances$Variables=="Total Current Assets",]
        CurrentLiabilities <- finances[finances$Variables=="Total Current Liabilities",]
        Inventory <- finances[finances$Variables=="Total Inventory",]
        TotalLiabilities <- finances[finances$Variables=="Total Liabilities",]
        EBIT <- finances[finances$Variables=="Operating Income",]
        IBIT <- finances[finances$Variables=="Income Before Tax",]
        TotalInventory <- finances[finances$Variables=="Total Inventory",]
        
        #Profitability Ratios
        ReturnOnAssets <- data.frame("ReturnOnAssets"=round(NetIncome$value/TotalAssets$value*100, digits=4))
        ReturnOnEquity <- data.frame("ReturnOnEquity"=round(NetIncome$value/TotalEquity$value*100, digits=4))
        ProfitMargin <- data.frame("ProfitMargin"=round(NetIncome$value/TotalRevenue$value*100, digits=4))
        
        #Liquidity Ratios
        CurrentRatio <- data.frame("CurrentRatio"=round(CurrentAssets$value/CurrentLiabilities$value, digits=4))
        QuickRatio <- data.frame("QuickRatio"=round((CurrentAssets$value-Inventory$value)/CurrentLiabilities$value, digits=4))
        
        #Debt Ratios, subtract liabilities from assets to get shareholders equity, Interest Expense is Operating Income(EBIT) minus Income before tax(IBIT)
        DebtToEquityRatio <- data.frame("DebtToEquityRatio"=round(TotalLiabilities$value/(TotalAssets$value-TotalLiabilities$value), digits=4))
        InterestCoverageRatio <- data.frame("InterestCoverageRatio"=round(EBIT$value/(EBIT$value-IBIT$value), digits=4))
        
        #Efficiency Ratios, sales(revenue) divided by Inventory to calculate the Inventory Turnover Ratio
        AssetTurnoverRatio <- data.frame("AssetTurnoverRatio"=round(TotalRevenue$value/TotalAssets$value, digits=4)) 
        InventoryTurnoverRatio <- data.frame("InventoryTurnoverRatio"=round(TotalRevenue$value/TotalInventory$value, digits=4))
        
        #Final ratios and cash flow dataframe
        Ratios_df <- cbind("Date"=NetIncome$Date, ReturnOnAssets)
        Ratios_df <- cbind(Ratios_df, ReturnOnEquity)
        Ratios_df <- cbind(Ratios_df, ProfitMargin)
        Ratios_df <- cbind(Ratios_df, CurrentRatio)
        Ratios_df <- cbind(Ratios_df, QuickRatio)
        Ratios_df <- cbind(Ratios_df, DebtToEquityRatio)
        Ratios_df <- cbind(Ratios_df, InterestCoverageRatio)
        Ratios_df <- cbind(Ratios_df, AssetTurnoverRatio)
        Ratios_df <- cbind(Ratios_df, InventoryTurnoverRatio)
        Ratios_df$Date <- as.Date(Ratios_df$Date)
        Ratios_df
}


##Create the final function to merge both stock prices and financial ratio data
PredictStock <- function(symbol=""){
        ratios <- stockratios(symbol)
        stockprice <- stockprices(symbol)
        stockprice <- stockprice[stockprice$Date>=min(stockratios(symbol)$Date),]
        stockprice <- stockprice[order(stockprice$Date, decreasing=TRUE),]
        for (i in 1:nrow(stockprice)){
                if (i==1){
                        if (stockprice$Date[i]>=ratios$Date[1]){
                                stock_df <- ratios[1,2:10]
                        }
                }
                else if (stockprice$Date[i]>=ratios$Date[1]){
                        stock_df <- rbind(stock_df, ratios[1,2:10])
                }
                else if (stockprice$Date[i]>=ratios$Date[2]){
                        stock_df <- rbind(stock_df, ratios[2,2:10])
                }
                else if (stockprice$Date[i]>=ratios$Date[3]){
                        stock_df <- rbind(stock_df, ratios[3,2:10])
                }
                else if (stockprice$Date[i]>=ratios$Date[4]){
                        stock_df <- rbind(stock_df, ratios[4,2:10])
                }
                else if (stockprice$Date[i]>=ratios$Date[5]){
                        stock_df <- rbind(stock_df, ratios[5,2:10])
                }
        }
        stockprice <- cbind(stockprice, stock_df)
        stockprice
}



##Example how to plot stock prices
plot(PredictStock("AAPL")$Date, PredictStock("AAPL")[,4] , type="l", ylim=c(0,140))
lines(PredictStock("AAPL")$Date, PredictStock("AAPL")$ReturnOnEquity, col="red")
var1 <- PredictStock("AAPL")$AssetTurnoverRatio
var2 <- PredictStock("AAPL")$ReturnOnEquity
var3 <- PredictStock("AAPL")$Date
regress <- lm(PredictStock("AAPL")[,4] ~ var3)
predict(regress, data.frame(var3=as.Date("2016-2-15"), PredictStock("AAPL")$ReturnOnEquity[300]), interval="prediction")


