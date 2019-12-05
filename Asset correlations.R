setwd("/home/gabriel/Finance/R - financial analysis/Asset correlations - Major indexes")
list.files(path = "/home/gabriel/Finance/R - financial analysis/Asset correlations - Major indexes")

library(quantmod)
library(tidyverse)
library(Amelia)
library(PerformanceAnalytics)
library(corrplot)
library(RColorBrewer)

##################################### Getting Data #########################################################
USD_Index <- read_csv("USDIndDec19_Daily.csv")
Gold <- read.csv("GOLD_Daily.csv")
USD_BRL <- read.csv("USDBRL_Daily.csv")
IBOV_ <- read.csv("Bra50Dec19_Daily.csv")
US_TB <- read.csv("UsaTBDec19_Daily.csv")
DAX <- read.csv("Ger30Dec19_Daily.csv")
OIL <- read.csv("LCrudeJan20_Daily.csv")
Dow_Jones <- read.csv("UsaIndDec19_Daily.csv")
US_Vix <- read.csv("UsaVixDec19_Daily.csv")

##################################### Transform datas into xts object #######################################

USD_Index$DATE <- USD_Index$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

USD_Index_DATE <- as.POSIXct(USD_Index$DATE, format = "%Y %m %d") 

USD_Index <- as.xts(USD_Index[ ,2:5], order.by = USD_Index_DATE)
########
USD_BRL$DATE <- USD_BRL$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

USD_BRL_DATE <- as.POSIXct(USD_BRL$DATE, format = "%Y %m %d") 

USD_BRL <- as.xts(USD_BRL[ ,2:5], order.by = USD_BRL_DATE)
#######
Gold$DATE <- Gold$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

Gold_DATE <- as.POSIXct(Gold$DATE, format = "%Y %m %d") 

Gold <- as.xts(Gold[ ,2:5], order.by = Gold_DATE)
#######
IBOV_$DATE <- IBOV_$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

IBOV_DATE <- as.POSIXct(IBOV_$DATE, format = "%Y %m %d") 

IBOV <- as.xts(IBOV_[ ,2:5], order.by = IBOV_DATE)
########
Dow_Jones$DATE <- Dow_Jones$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

Dow_Jones_DATE <- as.POSIXct(Dow_Jones$DATE, format = "%Y %m %d") 

Dow_Jones <- as.xts(Dow_Jones[ ,2:5], order.by = Dow_Jones_DATE)
#######
OIL$DATE <- OIL$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

OIL_DATE <- as.POSIXct(OIL$DATE, format = "%Y %m %d") 

OIL <- as.xts(OIL[ ,2:5], order.by = OIL_DATE)
##########
US_Vix$DATE <- US_Vix$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

US_Vix_DATE <- as.POSIXct(US_Vix$DATE, format = "%Y %m %d") 

US_Vix <- as.xts(US_Vix[ ,2:5], order.by = US_Vix_DATE)
###########
US_TB$DATE <- US_TB$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

US_TB_DATE <- as.POSIXct(US_TB$DATE, format = "%Y %m %d") 

US_TB <- as.xts(US_TB[ ,2:5], order.by = US_TB_DATE)
############
DAX$DATE <- DAX$DATE %>%
  as.character() %>%
  gsub(pattern = ".", replacement = "", fixed = TRUE)

DAX_DATE <- as.POSIXct(DAX$DATE, format = "%Y %m %d") 

DAX <- as.xts(DAX[ ,2:5], order.by = DAX_DATE)

#####################################################################################################################
################################# Get all Retuns ######################################################################

DAX_rets <- na.omit(Return.calculate(DAX[,4]))
Dow_Jones_rets <- na.omit(Return.calculate(Dow_Jones[,4]))
Gold_rets <- na.omit(Return.calculate(Gold[,4]))
IBOV_rets <- na.omit(Return.calculate(IBOV[,4]))
OIL_rets <- na.omit(Return.calculate(OIL[,4]))
US_TB_rets <- na.omit(Return.calculate(US_TB[,4]))
US_Vix_rets <- na.omit(Return.calculate(US_Vix[,4]))
USD_BRL_rets <- na.omit(Return.calculate(USD_BRL[,4]))
USD_Index_rets <- na.omit(Return.calculate(USD_Index[,4]))
############################Merge all Returns into xts Data Frame######################################################

Market_df <- na.omit(merge(DAX_rets,  Dow_Jones_rets, Gold_rets, IBOV_rets, OIL_rets, US_TB_rets, US_Vix_rets,
             USD_BRL_rets, USD_Index_rets))

missmap(Market_df)

names(Market_df) <- c("DAX", "Dow_Jones", "Gold", "IBOV", "OIL", "US_Treasury Bonds", "US_Vix", "USD_BRL", "USD_Index")

chart.CumReturns(Market_df, legend.loc = "topleft", main = "Asset Returns")

corr_market <- cor(Market_df)
corrplot(corr_market, method = "circle")

