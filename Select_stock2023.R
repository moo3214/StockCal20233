# install
library(quantmod)
library(tidyquant)
library(purrr)
library(berryFunctions)
library(tidyverse)
library(dplyr) 
library("parallel")


length(stock_list)
length(expect_return_list)
#multicore run

core <- 4
Sys.Date()
Sys.Date()
#Today <- Sys.Date() - 1
Today <- Sys.Date() - 1
#Function to change Date to char type
DateToChr <- function(Date) {
  
  return(as.character.Date(as.Date(Date)))
}

#fuction numdate dummy
num.stock_test <- function(stock,date){
  return(stock_array[num.date(date),2,stock])
}

#fuction return date at number
num.date = function(date){
  return(toString(date))
}  



#return number date of stock
num.stock <- function(stock,date){
  for(i in c(1:1000)){
    if(is.error(num.stock_test(stock,date), tell = FALSE, force = FALSE) == TRUE){
      
      date = as.Date(date) + 1
    }
    else {break}
  }
  return(stock_array[num.date(date),2,stock])
}

num.stock(stock,date)

Cal_return_bydate(stock,start_date,date)

#Cal return of stock by start and end date
Cal_return_bydate = function(stock,date0,date1) {
  
  
  #price <- stock_array[c(num.stock(stock,date0):num.stock(stock,date1)),1,stock]
  
  price0 <- as.numeric(stock_array[num.stock(stock,date0),1,stock])
  price1 <- as.numeric(stock_array[num.stock(stock,date1),1,stock])
  return_value <- (price1 - price0) / price0 *100
  
  return(return_value)
}

start_date <- "2021-01-01"
# option
options("getSymbols.warning4.0"=FALSE)

#create data frame

#stock_list <- c('2S.BK','ADVANC.BK')

stock_list <- c('RCL.BK'
                ,'CHG.BK'
                ,'BCH.BK'
                ,'TQM.BK'
                ,'PSL.BK'
                ,'COM7.BK'
                ,'BH.BK'
                ,'BANPU.BK'
                ,'INTUCH.BK'
                ,'MEGA.BK'
                ,'ESSO.BK'
                ,'SPRC.BK'
                ,'DELTA.BK'
                ,'PTTEP.BK'
                ,'BCP.BK'
                ,'CBG.BK'
                ,'SPALI.BK'
                ,'FORTH.BK'
                ,'HMPRO.BK'
                ,'ORI.BK'
                ,'KCE.BK'
                ,'TOP.BK'
                ,'THG.BK'
                ,'SABUY.BK'
                ,'BDMS.BK'
                ,'RBF.BK'
                ,'JMART.BK'
                
                ,'GLOBAL.BK'
                ,'GUNKUL.BK'
                ,'ONEE.BK'
                ,'SAWAD.BK'
                ,'OSP.BK'
                ,'IVL.BK'
                ,'AP.BK'
                ,'BEC.BK'
                ,'KTC.BK'
                ,'ADVANC.BK'
                ,'AMATA.BK'
                ,'SINGER.BK'
                ,'EA.BK'
                ,'LH.BK'
                ,'ACE.BK'
                ,'JMT.BK'
                ,'OR.BK'
                ,'EPG.BK'
                ,'MTC.BK'
                ,'PTT.BK'
                ,'TIDLOR.BK'
                ,'STA.BK'
                ,'STGT.BK'
                ,'RATCH.BK'
                ,'PLANB.BK'
                ,'BCPG.BK'
                ,'WHA.BK'
                ,'EGCO.BK'
                ,'TCAP.BK'
                ,'THANI.BK'
                ,'QH.BK'
                ,'DOHOME.BK'
                ,'CKP.BK'
                ,'SCGP.BK'
                ,'TU.BK'
                ,'CPN.BK'
                ,'PTG.BK')




pre_data <- tq_get('kbank.bk',
                   from = start_date,
                   to = Sys.Date(),
                   get = "stock.prices")


#length data

L_date <- length(pre_data[[2]])
L_stock_list <- length(stock_list)
L_date
L_stock_list

stockLength <- c(1:length(stock_list))

stock_array <- array(c(0 ),dim = c(L_date,2,L_stock_list))



# #####################################################################3
# 
# funLoadStock <- function(stockName){
#   
#   try({
#     
#     stock_data <- tq_get(stockName,
#                          from = start_date,
#                          to = Sys.Date() ,
#                          get = "stock.prices")
#   
#     
#     
#   }, silent=TRUE
#   )
#   
#   return(stock_data[[8]])
#   
#   
#   
# }
# 
# 
# try({
#   
#   a <- lapply(stock_list,funLoadStock)
#   
#   
#   
# }, silent=TRUE
# )
# 
# a
# 
# for (stock in stock_list){
# 
#   try({
# 
# 
#     funLoadStock(stock)
#     
#     dim <- which(stock_list == stock)
# 
# 
# 
#     stock_array[,1,dim] <- stock_data[[8]]
#     stock_array[,2,dim] <- sapply(pre_data[[2]],DateToChr)
#   }, silent=TRUE
#   )
# 
# }
# stock_array
# 
# stock_arrayDummy2 <- tapply(stock_list,stock_list,funLoadStock)
# x <- stock_arrayDummy2[1]
# x
# class(x)
# 
# x["2S.BK"]

##############################################################


for (i in stockLength){
  
  try({
    
    stock_data <- tq_get(stock_list[i],
                         from = start_date,
                         to = Sys.Date(),
                         get = "stock.prices")
    
    
    
    stock_array[,1,i]<- stock_data[[8]]
    stock_array[,2,i]<- sapply(pre_data[[2]],DateToChr)
  }, silent=TRUE
  )
  
}
tail(stock_data)

colnames(stock_array) <- c("PRICE","DATE")  #to set up col names
dimnames(stock_array)[[3]] <- stock_list
row.names(stock_array) <- sapply(pre_data[[2]],DateToChr)


tail(stock_array)

Cal_expectreturn <- function(stock,date) {
  xDATE <- which(stock_array[,2,stock] == date)
  
  
  # create x
  x <- c(1:xDATE)
  
  # create y = adjust 
  y <- c(stock_array[c(1:xDATE),1,stock])
  
  #find slope and intercept
  equation <- lm(y ~ x) 
  intercept <- coef(equation)[1]
  slope <- coef(equation)[2]
  
  #find expect price
  expect_price <- slope * length(x) + intercept
  
  #actual price
  actual_price <- as.numeric(y[length(y)])
  
  #expect return
  expect_return = (expect_price - actual_price) / actual_price *100
  
  return(expect_return)
  
}

expect_return_list <- c()
i <- 1
for (stock in stock_list){
  try({
    expect_return_list[i] <- Cal_expectreturn(stock,Today)
  }, silent=TRUE
  )
  i <- i+1
}
expect_return_list[i] <- Cal_expectreturn(stock,Today)
expect_return_list <- expect_return_list[1:L_stock_list]
expect_return_list

cbind(stock_list,expect_return_list) 
df_stock_return <- data.frame(stock_list,expect_return_list)
df_stock_return <- df_stock_return[with(df_stock_return,order(-expect_return_list)),] 

df_stock_return[2,1]




# option
options("getSymbols.warning4.0"=FALSE)

# create function to cal return expect price and actual price
#to = Sys.Date( ) ,
Cal_Return <- function(stock_name) {
  
  # dowload stock data
  stock_data <- tq_get(stock_name,
                       from = "2021-01-01",
                       to = Sys.Date(),
                       get = "stock.prices")
  
  # create x
  x <- 1:length(stock_data[[1]])
  
  # create y = adjust price
  y <- stock_data[[8]]
  
  #find slope and intercept
  equation <- lm(y ~ x) 
  intercept <- coef(equation)[1]
  slope <- coef(equation)[2]
  
  #find expect price
  expect_price <- slope * length(stock_data[[1]]) + intercept
  
  #actual price
  actual_price <- y[length(y)]
  
  #expect return
  expect_return = (expect_price - actual_price) / actual_price *100
  expect_return
  
  #return valve
  return_valve <- c(expect_price,actual_price,expect_return)
  return(return_valve)
  
}

# i for loop and list for stote value
i <- 0
new_stock_list <- c()
expect_price_list <- c()
actual_price_list <- c()
expect_return_list <- c()


stock_list <- c(df_stock_return[1,1],df_stock_return[2,1],df_stock_return[3,1],df_stock_return[4,1],df_stock_return[5,1],df_stock_return[6,1])

# cl <- makeCluster(core)
# clusterExport(cl,"tq_get")

for (stock_name in stock_list) {
  # dowload stock data
  stock_data <- tq_get(stock_name,
                       from = "2021-01-01",
                       to = Sys.Date() ,
                       get = "stock.prices")
  
  # stopCluster(cl)
  # create x
  x <- 1:length(stock_data[[1]])
  
  # create y = adjust price
  y <- stock_data[[8]]
  
  
  
  #find slope and intercept
  equation <- lm(y ~ x) 
  intercept <- coef(equation)[1]
  slope <- coef(equation)[2]
  
  #find expect price
  expect_price <- slope * length(stock_data[[1]]) + intercept
  
  #actual price
  actual_price <- y[length(y)]
  
  #expect return
  expect_return = (expect_price - actual_price) / actual_price *100
  expect_return
  
  
  
  #select stock
  if (expect_return >= 0){
    i = i + 1
    new_stock_list[i] <- stock_name
    actual_price_list[i] <- actual_price
    expect_price_list[i] <- expect_price
    expect_return_list[i] <- expect_return
  }
}

# input money for invest

#cold_money <- as.integer(cold_money)
cold_money <- 400000
#find percent invest and return ratio
percent_invest <- c()
return_ratio <- c()
for (i in (1 : length(new_stock_list))){
  percent_invest[i] <- expect_return_list[i] / sum(expect_return_list)
  return_ratio[i] <- percent_invest[i] * expect_return_list[i] /100
  
}

#find return port
return_port <- sum(return_ratio)

#find invest money
if (return_port > 1){
  invest_money = cold_money 
} else {
  invest_money = cold_money * return_port
}
s


#find each stock buy
stock_buy <- c()
volumn_buy <- c()
for (i in (1 : length(new_stock_list))){
  stock_buy[i] <- invest_money * percent_invest[i]
  volumn_buy[i] <- stock_buy[i] / actual_price_list[i]
}
invest_money
return_port
x <- data.frame(new_stock_list,actual_price_list,expect_price_list,expect_return_list,stock_buy,volumn_buy)
x

