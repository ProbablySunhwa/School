# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EMW forecast function for replication
# Mark M
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

EMW_forecast <- function(df.Ex, df.M1, df.CPI = NULL, df.IP = NULL, sample = "long", model = "plain", method = "OLS") {

  # model options are  : plain (default), Taylor, Monetary, PPP
  # sample options are : long (default), early, late
  # method options are : OLS (default), MLE
  
  #factor_nominal <- y; factor_M1 <- x1
  #df <- factor_nominal[,2:18]
  
  names_all <- names(df.M1)
  names(df.Ex) <- names_all
  
  df.Ex <- na.omit(df.Ex)
  df.M1 <- na.omit(df.M1)
  df.CPI <- na.omit(df.CPI)
  df.IP <- na.omit(df.IP)
  
  # drop 2018 Q1 period, and keep only log values (LHS), as they don't use it in their analysis
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
  #T <- dim(df)[1]
  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                 Options
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (sample == "long") {
      bound.train = 56; bound.test = 140-bound.train #if have time change this to grep later, as is shown in data as line 59, but had minused 3 lines earlier
      df.train <- df[1:bound.train,]; df.test <- df[(bound.train+1):nrow(df),]
      names = c("Australia", "Canada", "Denmark", "Japan", "Korea", "Norway", 
              "Sweden", "Switzerland", "UK")
    
      T = 140; colsize = 9;
  } else if (sample == "early") {
      bound.train = 56; bound.test = 104-bound.train 
      df.train <- df[1:bound.train,]; df.test <- df[(bound.train+1):nrow(df),]
    
      names = c("Australia", "Canada", "Denmark", "Japan", "Korea", "Norway", 
              "Sweden", "Switzerland", "Austria", "Belgium", "France", "Germany", 
              "Spain", "Italy", "Finland", "Netherlands", "UK")
      T = 104; colsize = 17;
  } else if (sample == "late") {
      bound.train = 105; bound.test = 140-bound.train 
      df.train <- df[1:bound.train,]; df.test <- df[(bound.train+1):nrow(df),]
  
      names = c("Australia", "Canada", "Denmark", "Japan", "Korea", "Norway", 
              "Sweden", "Switzerland", "UK", "Germany") # which 'one' is "euro"? lets say Germany ...
      T = 140; colsize = 10;
  } else
      stop("not a valid sample")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                               Forecast
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    

U.median <- as.data.frame(matrix(nrow=1, ncol=4))
t_values <- as.data.frame(matrix(nrow=1, ncol=4))
U_stats <- as.data.frame(matrix(nrow=4, ncol=colsize)); colnames(U_stats) <- names
t_tests <- as.data.frame(matrix(nrow=4, ncol=colsize)); colnames(U_stats) <- names

for (horizon in 1:4) {
  h = c(1,4,8,12)
  h = h[horizon]
  # h=1; # for trial purposes
  forecasts <- as.data.frame(matrix(nrow=bound.test, ncol=colsize)); colnames(forecasts) <- names # keep in loop, as may need reset each horizon
  SS.P <- as.data.frame(matrix(nrow=bound.test, ncol=colsize)); colnames(SS.P) <- names
  SS.A <- as.data.frame(matrix(nrow=bound.test, ncol=colsize)); colnames(SS.A) <- names
  #compare <- as.data.frame(matrix(nrow=84, ncol=9))
  
  for (j in 1:bound.test) {
    # restimate factors for 1973Q1-1986Q4 (1:56)
    df_fc <- df.Ex[1:(bound.train+j-1),2:18]; # eqn(4.1), expand train data used for factor analysis by 1 each iteration
    X_fc <- scale(df_fc, scale= FALSE); # scale demeans the data, column by column
    
    # means = t(as.matrix(apply(df_fc[[1]],2,mean)))
    f_fc <- factanal(X_fc,factors = 2) # this is the entire factor analysis function, choose r=2
    
    f_hat_fc = X_fc%*%f_fc[["loadings"]]/2

    train <- (bound.train-1)+j-h # bound.test-1 = 55 indicates 1986Q3
    s <- df_fc[, c(names)] # s_t
    
    # initialize some dfs
    SS <- as.data.frame(matrix(nrow=train, ncol=colsize)); colnames(SS) <- names
    inflation <- as.data.frame(matrix(nrow=T-1, ncol=(colsize+1))); colnames(inflation) <- c(names, "United.States")
    
    
    for (t in 1:(train)) {
      SS[t,] <- s[t+h,] - s[t,] # want -0.05023416 for first, the LHS of eqn (4.2)
    }
    SS <- na.omit(SS); train = dim(SS)[1] # 1 less as difference removed one
    
    # only factors
    FS <- f_hat_fc[1:train,] - s[1:train,] # cut last observation to match dimensions with y is RHS of (4.2)
    FS.P <- f_hat_fc[(train+h),] - s[(train+h),] # predict next, is RHS of (4.3)
    
    if (model == "plain") {
        z <- 0*(df.CPI[1:train, c(names)] - df.CPI[1:train, "United.States"])
        zP <- 0*(df.CPI[(train+h), c(names)] - df.CPI[(train+h), "United.States"])
    } else if (model == "Taylor") {
        for (t in 1:(T-1)) {
          inflation[t,] <- df.CPI[t+1, c(names, "United.States")] - df.CPI[t,c(names, "United.States")]
        }
        z <-  1.5*(inflation[1:train, c(names)] - inflation[1:train, "United.States"]) + 0.5*(df.IP[1:train, c(names)] - df.IP[1:train, "United.States"])
        zP <- 1.5*(inflation[(train+h), c(names)] - inflation[(train+h), "United.States"]) + 0.5*(df.IP[(train+h), c(names)] - df.IP[(train+h), "United.States"])
    } else if (model == "Monetary") {
        z <-  (df.M1[1:train, c(names)] - df.M1[1:train, "United.States"]) + (df.IP[1:train, c(names)] - df.IP[1:train, "United.States"])
        zP <- (df.M1[(train+h), c(names)] - df.M1[(train+h), "United.States"]) + (df.IP[(train+h), c(names)] - df.IP[(train+h), "United.States"])
    } else if (model == "PPP") {
        z <- df.CPI[1:train, c(names)] - df.CPI[1:train, "United.States"] # - s[1:train,] # should be just z or z-s??
        zP <- df.CPI[(train+h), c(names)] - df.CPI[(train+h), "United.States"] # - s[(train+h),]
    } else 
        stop("not a valid model")
    
     m <- list();
    
     if (method == "OLS")  {
        for (n in 1:colsize) {
          if (model == "plain") {
            m[[n]] <- lm(SS[,n] ~ FS[,n]) # compare with OLS
            SS.P[j,n] <- m[[n]]$coefficients[1] + m[[n]]$coefficients[2]*FS.P[n] # eqn (4.3)
          } else { # any other model has a z-term
            m[[n]] <- lm(SS[,n] ~ FS[,n] + z[,n]) # compare with OLS
            SS.P[j,n] <- m[[n]]$coefficients[1] + m[[n]]$coefficients[2]*FS.P[n] + m[[n]]$coefficients[3]*zP[n] # eqn (4.3)
          }
        }
     } else if (method == "MLE") { # oh-no! MLE point value is decreasing with horizon, it should be increasing ...
        for (n in 1:colsize) {
          set.seed(10)
          param = runif(4)
          
          if (model == "plain") {
              m[[n]] <- mle2(LL, data = list(y=SS, x=FS, n=n), start = list(alpha = param[1], beta  = param[2], sigma = param[3]), method = "BFGS")
              alpha = coef(m[[n]])[1]
              beta = coef(m[[n]])[2]
              SS.P[j,n] <- alpha + beta*FS.P[n]
          } else if (model != "plain") {
              m[[n]] <- mle2(LL.z, data = list(y=SS, x = FS, z = z, n=n), start = list(alpha = param[1], beta  = param[2], gamma = param[3], sigma = param[4]), method = "BFGS")
              alpha = coef(m[[n]])[1]
              beta = coef(m[[n]])[2]
              gamma = coef(m[[n]])[3]
              SS.P[j,n] <- alpha + beta*FS.P[n] + gamma*zP[n]
          } else
            stop("not a valid method")
        }
      }
    }
    

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                                          Construct Theil's U-statistics
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for (t in 1:bound.test) {
    SS.A[t,] <- df[(bound.train+t+h),names] - df[(bound.train+t),names] # want -0.05023416 for first, the LHS of eqn (4.2)
  }
  
  SS.A <- na.omit(SS.A); SS.P <- SS.P[1:dim(SS.A)[1],] # drop to match size
  U.stats <- as.data.frame(matrix(nrow=1, ncol=colsize)); colnames(U.stats) <- names
  
  for (n in 1:colsize) {
    U.stats[,n] <- TheilU(SS.A[,n],SS.P[,n]) # TheilU is in the DescTools package
  }
  
  # save U-stats
  U_stats[horizon,] <- U.stats
  U.median[horizon] <- median(as.numeric(U.stats))
  
  # t-test
  t_tests <- t.test(U_stats[horizon,], mu=1, alternative="less")
  
  t_values[horizon] <- t_tests$statistic
}
EMW_forecast_list = list("sample" = sample, "model" = model, "U-stats" = U_stats, "U-median" = U.median, "t-values" = t_values)
return(EMW_forecast_list)
}
