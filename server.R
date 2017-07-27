##Stock Predictor
library(ggplot2)
library(quantmod)
library(reshape2)
library(googleVis)
library(shiny)


##Create the data for the first tab panel and to be used for other analyses
data <- function(symbol="", MinDays){
        ##Create function to get stock prices/info using a function and bind industry/sector info 
        stockprices <- function(symbol){
                TICKERS.DF <- data.frame(getSymbols(symbol, from=Sys.Date()-657, to=Sys.Date(), env=NULL, src = "google"))
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
        stockratios <- function(symbol){
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
        if (MinDays=="30"){
                PredictStock(symbol)[1:30,]
        }
        else if (MinDays=="120"){
                PredictStock(symbol)[1:120,]
        }
        else if (MinDays=="Max"){
                PredictStock(symbol)
        }
        
}




##Create the regression plot
regress_plot <- function(symbol, MinDays){
        data <- function(symbol="", MinDays){
                ##Create function to get stock prices/info using a function and bind industry/sector info 
                stockprices <- function(symbol){
                        TICKERS.DF <- data.frame(getSymbols(symbol, from=Sys.Date()-657, to=Sys.Date(), env=NULL, src = "google"))
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
                stockratios <- function(symbol){
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
                if (MinDays=="30"){
                        data_df <- PredictStock(symbol)[1:30,]
                }
                else if (MinDays=="120"){
                        data_df <- PredictStock(symbol)[1:120,]
                }
                else if (MinDays=="Max"){
                        data_df <- PredictStock(symbol)
                }
                
        }
        
        data_df <- data(symbol, MinDays)
        x <- data_df$Date
        y <- data_df[,4]
        
        qplot(data=data_df,x=x,y=y,color=y, 
              main=paste("Historic Prices of the Stock"), xlab="Date", 
              ylab="Stock Price", geom="line",
              method="lm")
}






##Create function to plot the various variables
regress_plot2 <- function(symbol="", MinDays){
        ##Create function to get stock prices/info using a function and bind industry/sector info 
        stockprices <- function(symbol){
                TICKERS.DF <- data.frame(getSymbols(symbol, from=Sys.Date()-657, to=Sys.Date(), env=NULL, src = "google"))
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
        stockratios <- function(symbol){
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
        if (MinDays=="30"){
                check <- PredictStock(symbol)[1:30,]
                check <- melt(check, id="Date")
                check <- check[check$variable!=paste(symbol,"Open",sep=".") & check$variable!=paste(symbol,"High",sep=".") & check$variable!=paste(symbol,"Low",sep=".") & check$variable!="Volume" & check$variable!=paste(symbol,"Adjusted",sep=".") & check$variable!="PctChange" & check$variable!="Sector" & check$variable!="Industry" & check$variable!="Stock.Exchange",]
                check$variable <- as.character(check$variable)
                check$value <- as.numeric(check$value)
                check <- data.frame(cbind(check, "symbol"=symbol))
                qplot(data=check[check$variable==paste(check[1,4],"Close", sep="."),],x=check[check$variable==paste(check[1,4],"Close", sep="."),][,1],y=check[check$variable==paste(check[1,4],"Close", sep="."),][,3], color=check[check$variable==paste(check[1,4],"Close", sep="."),][,3],
                      main="Stock Price Regression Analysis", xlab="Date", 
                      ylab="Stock Price", geom=c("point","smooth"), method = "lm") + labs(colour = "Stock Price")
        }
        else if (MinDays=="120"){
                check <- PredictStock(symbol)[1:120,]
                check <- melt(check, id="Date")
                check <- check[check$variable!=paste(symbol,"Open",sep=".") & check$variable!=paste(symbol,"High",sep=".") & check$variable!=paste(symbol,"Low",sep=".") & check$variable!="Volume" & check$variable!=paste(symbol,"Adjusted",sep=".") & check$variable!="PctChange" & check$variable!="Sector" & check$variable!="Industry" & check$variable!="Stock.Exchange",]
                check$variable <- as.character(check$variable)
                check$value <- as.numeric(check$value)
                check <- data.frame(cbind(check, "symbol"=symbol))
                qplot(data=check[check$variable==paste(check[1,4],"Close", sep="."),],x=check[check$variable==paste(check[1,4],"Close", sep="."),][,1],y=check[check$variable==paste(check[1,4],"Close", sep="."),][,3], color=check[check$variable==paste(check[1,4],"Close", sep="."),][,3],
                      main="Stock Price Regression Analysis", xlab="Date", 
                      ylab="Stock Price", geom=c("point","smooth"),
                      method="lm") + labs(colour = "Stock Price")
        }
        else if (MinDays=="Max"){
                check <- PredictStock(symbol)
                check <- melt(check, id="Date")
                check <- check[check$variable!=paste(symbol,"Open",sep=".") & check$variable!=paste(symbol,"High",sep=".") & check$variable!=paste(symbol,"Low",sep=".") & check$variable!="Volume" & check$variable!=paste(symbol,"Adjusted",sep=".") & check$variable!="PctChange" & check$variable!="Sector" & check$variable!="Industry" & check$variable!="Stock.Exchange",]
                check$variable <- as.character(check$variable)
                check$value <- as.numeric(check$value)
                check <- data.frame(cbind(check, "symbol"=symbol))
                qplot(data=check[check$variable==paste(check[1,4],"Close", sep="."),],x=check[check$variable==paste(check[1,4],"Close", sep="."),][,1],y=check[check$variable==paste(check[1,4],"Close", sep="."),][,3], color=check[check$variable==paste(check[1,4],"Close", sep="."),][,3],
                      main="Stock Price Regression Analysis", xlab="Date", 
                      ylab="Stock Price", geom=c("point","smooth"),
                      method="lm") + labs(colour = "Stock Price")
        }

}







##Create the regression summary
summa <- function(symbol, MinDays, MaxDays, predictors){
        ##Create function to get stock prices/info using a function and bind industry/sector info 
        stockprices <- function(symbol){
                TICKERS.DF <- data.frame(getSymbols(symbol, from=Sys.Date()-657, to=Sys.Date(), env=NULL, src = "google"))
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
        stockratios <- function(symbol){
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
        if (MinDays=="30"){
                data <- PredictStock(symbol)[1:30,]
                if (length(predictors)==0){
                        print("Please List the Predictor Variable(s)")
                }
                else if (length(predictors)==1){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1]) 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date)
                                summary(fit)
                        }    
                }
                else if (length(predictors)==2){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1)
                                summary(fit)
                        }
                        
                }
                else if (length(predictors)==3){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2)
                                summary(fit)
                        }
                }
                else if (length(predictors)==4){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3)
                                summary(fit)
                        }
                }
                else if (length(predictors)==5){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4)
                                summary(fit)
                        }
                }
                else if (length(predictors)==6){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5)
                                summary(fit)
                        }
                }
                else if (length(predictors)==7){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6)
                                summary(fit)
                        }
                }
                else if (length(predictors)==8){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var7 <- as.numeric(predictors[8])
                        var7 <- data[,var7]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                summary(fit)
                        }
                }
                else if (length(predictors)==9){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                summary(fit)
                        }
                }
                else if (length(predictors)==10){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        var9 <- as.numeric(predictors[10])
                        var9 <- data[,var9]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                summary(fit)
                        }
                }

        }
        else if (MinDays=="120"){
                data <- PredictStock(symbol)[1:120,]
                if (length(predictors)==0){
                        print("Please List the Predictor Variable(s)")
                }
                else if (length(predictors)==1){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1]) 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date)
                                summary(fit)
                        }    
                }
                else if (length(predictors)==2){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1)
                                summary(fit)
                        }
                        
                }
                else if (length(predictors)==3){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2)
                                summary(fit)
                        }
                }
                else if (length(predictors)==4){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3)
                                summary(fit)
                        }
                }
                else if (length(predictors)==5){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4)
                                summary(fit)
                        }
                }
                else if (length(predictors)==6){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5)
                                summary(fit)
                        }
                }
                else if (length(predictors)==7){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6)
                                summary(fit)
                        }
                }
                else if (length(predictors)==8){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var7 <- as.numeric(predictors[8])
                        var7 <- data[,var7]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                summary(fit)
                        }
                }
                else if (length(predictors)==9){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                summary(fit)
                        }
                }
                else if (length(predictors)==10){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        var9 <- as.numeric(predictors[10])
                        var9 <- data[,var9]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                summary(fit)
                        }
                }
        }
        else if (MinDays=="Max"){
                data <- PredictStock(symbol)
                if (length(predictors)==0){
                        print("Please List the Predictor Variable(s)")
                }
                else if (length(predictors)==1){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1]) 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date)
                                summary(fit)
                        }    
                }
                else if (length(predictors)==2){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1)
                                summary(fit)
                        }
                        
                }
                else if (length(predictors)==3){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2)
                                summary(fit)
                        }
                }
                else if (length(predictors)==4){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3)
                                summary(fit)
                        }
                }
                else if (length(predictors)==5){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4)
                                summary(fit)
                        }
                }
                else if (length(predictors)==6){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5)
                                summary(fit)
                        }
                }
                else if (length(predictors)==7){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6)
                                summary(fit)
                        }
                }
                else if (length(predictors)==8){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var7 <- as.numeric(predictors[8])
                        var7 <- data[,var7]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                summary(fit)
                        }
                }
                else if (length(predictors)==9){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                summary(fit)
                        }
                }
                else if (length(predictors)==10){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        var9 <- as.numeric(predictors[10])
                        var9 <- data[,var9]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                summary(fit)
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                summary(fit)
                        }
                }
        }
}






##Create the prediction interval
prediction <- function(symbol, MinDays, MaxDays, predictors, fp){
        ##Create function to get stock prices/info using a function and bind industry/sector info 
        stockprices <- function(symbol){
                TICKERS.DF <- data.frame(getSymbols(symbol, from=Sys.Date()-657, to=Sys.Date(), env=NULL, src = "google"))
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
        stockratios <- function(symbol){
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
        if (MinDays=="30"){
                data <- PredictStock(symbol)[1:30,]
                if (length(predictors)==0){
                        print("Please List the Predictor Variable(s)")
                }
                else if (length(predictors)==1){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1]) 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays)), interval="prediction")      
                                pred_df
                        }    
                }
                else if (length(predictors)==2){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp), interval="prediction")      
                                pred_df
                        }
                        
                }
                else if (length(predictors)==3){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==4){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==5){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==6){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==7){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==8){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var7 <- as.numeric(predictors[8])
                        var7 <- data[,var7]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==9){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==10){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        var9 <- as.numeric(predictors[10])
                        var9 <- data[,var9]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1], var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
        }
        else if (MinDays=="120"){
                data <- PredictStock(symbol)[1:120,]
                if (length(predictors)==0){
                        print("Please List the Predictor Variable(s)")
                }
                else if (length(predictors)==1){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1]) 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays)), interval="prediction")      
                                pred_df
                        }    
                }
                else if (length(predictors)==2){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp), interval="prediction")      
                                pred_df
                        }
                        
                }
                else if (length(predictors)==3){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==4){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==5){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==6){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==7){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==8){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var7 <- as.numeric(predictors[8])
                        var7 <- data[,var7]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==9){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==10){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        var9 <- as.numeric(predictors[10])
                        var9 <- data[,var9]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1], var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
        }
        else if (MinDays=="Max"){
                data <- PredictStock(symbol)
                
                if (length(predictors)==0){
                        print("Please List the Predictor Variable(s)")
                }
                else if (length(predictors)==1){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1]) 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays)), interval="prediction")      
                                pred_df
                        }    
                }
                else if (length(predictors)==2){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp), interval="prediction")      
                                pred_df
                        }

                }
                else if (length(predictors)==3){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==4){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==5){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==6){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==7){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==8){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var7 <- as.numeric(predictors[8])
                        var7 <- data[,var7]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==9){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                else if (length(predictors)==10){
                        date <- as.numeric(data[,7])
                        var <- as.numeric(predictors[1])
                        var1 <- as.numeric(predictors[2])
                        var1 <- data[,var1] 
                        var2 <- as.numeric(predictors[3])
                        var2 <- data[,var2] 
                        var3 <- as.numeric(predictors[4])
                        var3 <- data[,var3]
                        var4 <- as.numeric(predictors[5])
                        var4 <- data[,var4]
                        var5 <- as.numeric(predictors[6])
                        var5 <- data[,var5]
                        var6 <- as.numeric(predictors[7])
                        var6 <- data[,var6]
                        var8 <- as.numeric(predictors[9])
                        var8 <- data[,var8]
                        var9 <- as.numeric(predictors[10])
                        var9 <- data[,var9]
                        if (var>7){
                                var <- data[,var]
                                fit <- lm(data[,4] ~ var + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var=var[1]*fp, var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1], var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")
                                pred_df
                        }
                        else {
                                fit <- lm(data[,4] ~ date + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9)
                                pred_df <- predict(fit,data.frame(date=(date[1]+MaxDays), var1=var1[1]*fp, var2=var2[1]*fp, var3=var3[1]*fp, var4=var4[1]*fp, var5=var5[1]*fp, var6=var6[1]*fp, var7=var7[1]*fp, var8=var8[1]*fp, var9=var9[1]*fp), interval="prediction")      
                                pred_df
                        }
                }
                }
}





##Create financial statements data table
stockratios <- function(symbol="", statement){
        finances <- getFinancials(symbol, env=NULL)
        BalanceSheet <- data.frame(viewFinancials(finances, type='BS', period="Q"))
        BalanceSheet <- cbind(BalanceSheet, "Variables"=attr(BalanceSheet, "row.names"))
        BalanceSheet <- cbind(BalanceSheet, "Statement"="Balance Sheet")
        
        IncomeStatement <- data.frame(viewFinancials(finances, type='IS', period="Q"))
        IncomeStatement <- cbind(IncomeStatement, "Variables"=attr(IncomeStatement, "row.names"))
        IncomeStatement <- cbind(IncomeStatement, "Statement"="Income Statement")
        
        
        CashFlow <- data.frame(viewFinancials(finances, type='CF', period="Q"))
        CashFlow <- cbind(CashFlow, "Variables"=attr(CashFlow, "row.names"))
        CashFlow <- cbind(CashFlow, "Statement"="Cash Flow")
        
        if (statement=="BS"){
                BalanceSheet
        }
        else if (statement=="IS"){
                IncomeStatement
        }
        else if (statement=="CF"){
                CashFlow
        }
}





##Ratios with ggvis api
ratios <- function(symbol="", MinDays){
        ##Create function to get stock prices/info using a function and bind industry/sector info 
        stockprices <- function(symbol){
                TICKERS.DF <- data.frame(getSymbols(symbol, from=Sys.Date()-657, to=Sys.Date(), env=NULL, src = "google"))
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
        stockratios <- function(symbol){
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
        if (MinDays=="30"){
                data <- PredictStock(symbol)[1:30,]
                data <- data[,-1:-3]
                data <- data[,-2:-3]
                data <- data[,-4:-6]
                data <- melt(data, id=c("Date", paste(symbol, "Close", sep=".")))
                data$variable <- as.character(data$variable)
        }
        else if (MinDays=="120"){
                data <- PredictStock(symbol)[1:120,]
                data <- data[,-1:-3]
                data <- data[,-2:-3]
                data <- data[,-4:-6]
                data <- melt(data, id=c("Date", paste(symbol, "Close", sep=".")))
                data$variable <- as.character(data$variable)

        }
        else if (MinDays=="Max"){
                data <- PredictStock(symbol)
                data <- data[,-1:-3]
                data <- data[,-2:-3]
                data <- data[,-4:-6]
                data <- melt(data, id=c("Date", paste(symbol, "Close", sep=".")))
                data$variable <- as.character(data$variable)
                
        }
        data <- data[data$variable!="InterestCoverageRatio",]
        
        G <- gvisMotionChart(data, idvar="variable", timevar="Date", xvar="value", yvar=paste(symbol, "Close", sep="."),
                             colorvar="value", options=list(width="1250px", height="585px"))
        G
       
}






##Ratios facet ggplots
ratioFacet <- function(symbol="", MinDays){
        ##Create function to get stock prices/info using a function and bind industry/sector info 
        stockprices <- function(symbol){
                TICKERS.DF <- data.frame(getSymbols(symbol, from=Sys.Date()-657, to=Sys.Date(), env=NULL, src = "google"))
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
        stockratios <- function(symbol){
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
        if (MinDays=="30"){
                data <- PredictStock(symbol)[1:30,]
                data <- data[,-1:-3]
                data <- data[,-2:-3]
                data <- data[,-4:-6]
                data <- melt(data, id="Date")
                data$variable <- as.character(data$variable)
        }
        else if (MinDays=="120"){
                data <- PredictStock(symbol)[1:120,]
                data <- data[,-1:-3]
                data <- data[,-2:-3]
                data <- data[,-4:-6]
                data <- melt(data, id="Date")
                data$variable <- as.character(data$variable)
                
        }
        else if (MinDays=="Max"){
                data <- PredictStock(symbol)
                data <- data[,-1:-3]
                data <- data[,-2:-3]
                data <- data[,-4:-6]
                data <- melt(data, id="Date")
                data$variable <- as.character(data$variable)
                
        }
        data <- data[data$variable!="InterestCoverageRatio",]
        data <- data[data$variable!="PctChange",]
        colnames(data) <- c("Date", "Variable", "Value")
        
        p <- ggplot(data, aes(Date, Value)) + geom_point(color="blue") + geom_jitter()
        # With one variable
        p + facet_grid(. ~ Variable) 
        
        
}






shinyServer(
        function(input, output){
                output$data_table <- renderDataTable({data(input$Stock, input$DayBack)})

                output$regression_plot <- renderPlot({regress_plot(input$Stock, input$DayBack)})
                
                output$prediction <- renderPrint({prediction(input$Stock, input$DayBack, input$DayForward, input$Predictors, input$FinancialPredictorMultiplier)})
                
                output$summary <- renderPrint({summa(input$Stock, input$DayBack, input$DayForward, input$Predictors)})
                
                output$regression_plot2 <- renderPlot({regress_plot2(input$Stock, input$DayBack)})
                
                output$finance_statement <- renderDataTable({stockratios(input$Stock, input$FinancialStatement)})
                
                output$plot_table <- renderGvis({ratios(input$Stock, input$DayBack)})
                
                output$facet_plot <- renderPlot({ratioFacet(input$Stock, input$DayBack)})
                
                })