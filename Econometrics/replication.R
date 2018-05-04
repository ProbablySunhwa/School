# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Econometrics Replication Paper
# Mark McAvoy
# Spring 2018
# Factor models
# trial
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(DescTools)
library(bbmle)
library(tictoc)

# to run, move to data-EMW folder, and install packages
# inport data, it appears the left values are logs of the actual values to rt
df.Ex <- read.csv("df_exchange_rates.csv")
df.M1 <- read.csv("df_M1.csv")
df.CPI <- read.csv("df_CPI.csv")
df.IP <- read.csv("df_IP.csv")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                   table 2 and figure 1
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# first make the table 2 and figure 1

names <- names(df.M1)
names(df.Ex) <- names

df.Ex <- na.omit(df.Ex)
df.M1 <- na.omit(df.M1)
df.CPI <- na.omit(df.CPI)
df.IP <- na.omit(df.IP)

# drop 2018 Q1 period, and keep only log values (LHS)
df.Ex <- df.Ex[-141,1:19]
df.M1 <- df.M1[-141,1:19]
df.CPI <- df.CPI[-141,1:19]
df.IP <- df.IP[-141,1:19]

# estimate factors using maximum likelihood
# first convert factors to numeric
df.Ex[,2:19] <- lapply(df.Ex[,2:19], function(x) as.numeric(as.character(x)))
df.CPI[,2:19] <- lapply(df.CPI[,2:19], function(x) as.numeric(as.character(x)))
df.M1[,2:19] <- lapply(df.M1[,2:19], function(x) as.numeric(as.character(x)))
df.IP[,2:19] <- lapply(df.IP[,2:19], function(x) as.numeric(as.character(x)))

df <- df.Ex[,2:18]
# old way to demean data
# means = t(as.matrix(apply(df,2,mean)))
# X <- as.matrix(df - kronecker(matrix(1,140,1),means)) # kronecker here is same as repmat
X <- scale(df, scale=FALSE) # much better way!
f <- factanal(X,factors = 3) # actually got pretty close with this for the first two factors

f_hat1 = X%*%f[["loadings"]]/3
time = 1:140
df1 <- as.tibble(cbind(time, f_hat1))

# --- Figure 1 ---
ggplot(df1, aes(time,Factor1)) + geom_line()
ggplot(df1, aes(time,Factor2)) + geom_line()
ggplot(df1, aes(time,Factor3)) + geom_line() # factor3 not rt still

# --- Table 2 ---
f[["loadings"]]



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                 Table 3
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MLE takes a long time, so I put an OLS option as it is quicker (though still rather long)
# can either run the function by itself and check some rows individually, 
# or run the for loop to check all values at once


# sample options are : long, early, late
# model options are  : plain, Taylor, Monetary, PPP
# method options are : OLS, MLE
EMW_forecast(df.Ex, df.M1, df.CPI, df.IP, sample="long", model="plain", method="OLS")

# or run the following loop to produce the entire table in one go
# beware, will take an hr >.<
tic() # OLS takes 750 sec.; MLE takes 2181 sec. and crashed on my computer at the end, though still produced all tables
table3 <- list(); n=1
  for (sample in 1:3) {
    s = c("long", "early", "late")
    s = s[sample]
    
    for (model in 1:4) {
      m = c("plain", "Taylor", "Monetary", "PPP")
      m = m[model]
      
      table3[[n]] <- EMW_forecast(df.Ex, df.M1, df.CPI, df.IP, sample=s, model=m, method="MLE")
      n <- n+1
    }
  }
toc()

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                   try BTC exchange rate factor analysis
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# data from: https://spreadstreet.io

# change wd to btc folder 
# setwd("C:/Users/homur/OneDrive/Memory\ Palace/GarnetStar/cryptocurrency/btc\ data/")
btc.usd <- read.csv("Bitcoin Markets (localbtcUSD).csv") # actually lets not include usd, as pca should be unsupervised learning rt?
btc.cad <- read.csv("Bitcoin Markets (localbtcCAD).csv")
btc.sgd <- read.csv("Bitcoin Markets (localbtcSGD).csv")
btc.hkd <- read.csv("Bitcoin Markets (localbtcHKD).csv")
btc.mxn <- read.csv("Bitcoin Markets (localbtcMXN).csv") 
btc.nxd <- read.csv("Bitcoin Markets (localbtcNZD).csv")
btc.pln <- read.csv("Bitcoin Markets (localbtcPLN).csv")
btc.rub <- read.csv("Bitcoin Markets (localbtcRUB).csv")
btc.thb <- read.csv("Bitcoin Markets (localbtcTHB).csv")
btc.ars <- read.csv("Bitcoin Markets (localbtcARS).csv")
btc.zar <- read.csv("Bitcoin Markets (localbtcZAR).csv")
btc.sek <- read.csv("Bitcoin Markets (localbtcSEK).csv")
# btc.vnd <- read.csv("Bitcoin Markets (localbtcVND).csv") # removed bc too small
btc.dkk <- read.csv("Bitcoin Markets (localbtcDKK).csv")
btc.czk <- read.csv("Bitcoin Markets (localbtcCZK).csv")
btc.inr <- read.csv("Bitcoin Markets (localbtcINR).csv")
btc.ils <- read.csv("Bitcoin Markets (localbtcILS).csv")
btc.aud <- read.csv("Bitcoin Markets (localbtcAUD).csv")
btc.nok <- read.csv("Bitcoin Markets (localbtcNOK).csv")
btc.jpy <- read.csv("Bitcoin Markets (coincheckJPY).csv")
btc.krw <- read.csv("Bitcoin Markets (korbitKRW).csv") 
# chinese data doesnt ends much too soon, so didn't include this either unfortunately
# btc.cny <- read.csv("Bitcoin Markets (btcnCNY).csv") # winner but not all :( 
# btc.cny2 <- read.csv("Bitcoin Markets (okcoinCNY).csv") # close second
btc.gbp <- read.csv("Bitcoin Markets (coinfloorGBP).csv")
btc.eur <- read.csv("Bitcoin Markets (krakenEUR).csv") # winner (most recent)
#btc.eur2 <- read.csv("Bitcoin Markets (btceEUR).csv") # has most
#btc.eur3 <- read.csv("Bitcoin Markets (bitcurexEUR).csv") # alot too

m <- 939 # temporary set based on korean won having smallest dataset
df.btc_0 <- cbind(btc.cad[1:m,"Weighted.Price"], btc.sgd[1:m,"Weighted.Price"],
             btc.hkd[1:m,"Weighted.Price"], btc.mxn[1:m,"Weighted.Price"], btc.nxd[1:m,"Weighted.Price"],
             btc.pln[1:m,"Weighted.Price"], btc.rub[1:m,"Weighted.Price"], btc.thb[1:m,"Weighted.Price"],
             btc.ars[1:m,"Weighted.Price"], btc.zar[1:m,"Weighted.Price"], btc.sek[1:m,"Weighted.Price"], 
             btc.dkk[1:m,"Weighted.Price"], btc.czk[1:m,"Weighted.Price"], btc.inr[1:m,"Weighted.Price"], 
             btc.ils[1:m,"Weighted.Price"], btc.aud[1:m,"Weighted.Price"], btc.nok[1:m,"Weighted.Price"], 
             btc.jpy[1:m,"Weighted.Price"], btc.krw[1:m,"Weighted.Price"],
             btc.gbp[1:m,"Weighted.Price"], btc.eur[1:m,"Weighted.Price"]) # full 22 exchange rates

df.btc1 <- as.data.frame(df.btc)
df.btc1[df.btc1 == 0] <- NA # set 0's to NA (therwise when take logs will be infinity)
df.btc1 <- na.omit(df.btc1) # remove rows with missing data
df.btc1 <- log(df.btc1) # take logs to map data to clearer values
names <- c("cad", "sgd", "hkd", "mxn", "nxd", "pln", "rub", "thb", "ars", "zar", "sek", "dkk", "czk", "inr", "ils", "aud", "nok", "jpy", "krw", "gbp", "eur")
colnames(df.btc1) <- names
m <- dim(df.btc1)[1] # end with 235 / or 340 huh values of 21 different exchange rates

X <- scale(df.btc1, scale=FALSE)

# tidyverse doesn't do well with matricies, alternatives not clear.

f <- factanal(X,factors = 3, rotation = "varimax", lower = 0.01) # try 3 factors at first
f_hat1 = X%*%f[["loadings"]]/3 # im not why equation this way, just saw some code that used this
# I think %*% is matrix multiplication of X (235 x 21) and f (21 x 3) = (235 x 3)
time <- 1:m # for plotting
df_time <- as.tibble(cbind(time, f_hat1))

f[["loadings"]] # lets see loadings
ggplot(df_time, aes(time,Factor1)) + geom_line() # and see factors
ggplot(df_time, aes(time,Factor2)) + geom_line()
ggplot(df_time, aes(time,Factor3)) + geom_line() # they are pretty much the same ... only 1 factor then? hmm odd

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                Forecast
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df.v <- read.csv("bitcoinity_data.csv")
df.v.cad <- read.csv("bitcoinity_data_Cad.csv")

EMW_forecast_btc(df.btc_0)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                Appendix
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
