library(rugarch)
library(readr)
library(fpp)
library(sandwich)
library(R.utils)

#Set parameters for estimation
#rates: "AUDUSD" "BRLUSD" "CADUSD" "CHFEUR" "CHFUSD" "DKKUSD" "JPYUSD"
#       "MXNUSD" "NOKGBP" "NOKUSD" "SEKUSD" "USDEUR" "USDGBP"
#freqs: "Daily" "Monthly"
#voltypes: "OVX" "sGARCH norm" "eGARCH norm" "gjrGARCH norm" "sGARCH std" "eGARCH std" "gjrGARCH std"
#oilprice: "Brent" "WTI"
#period: "main" or "FRR"


get_params <- function(rate = "CADUSD", freq = "Daily", voltype = "OVX", oilprice = "WTI", period = "main",
                       window = c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 2, 3, 4, 5, 6, 7, 8, 9)) {
  return(list("rate" = rate, "freq" = freq, "voltype" = voltype, "oilprice" = oilprice, "period" = period,
              "window" = window))
}


#Produces data for estimation from params set at the top
loaddata <- function(params) {
  
  data <- read_csv(paste("Data/", params$freq, ".csv", sep = ""),
                   col_types = cols(DATE = col_date(format = "%Y-%m-%d"),
                                    DCOILBRENTEU = col_number(),
                                    DCOILWTICO = col_number(),
                                    DEXUSAL = col_number(),
                                    DEXCAUS = col_number(),
                                    DEXDNUS = col_number(),
                                    DEXUSEU = col_number(),
                                    DEXNOUS = col_number(),
                                    DEXSDUS = col_number(),
                                    DEXUSUK = col_number(),
                                    OVX = col_number(),
                                    DEXNOEU = col_number(),
                                    DEXBZUS = col_number(),
                                    DEXMXUS = col_number(),
                                    CHFEUR = col_number(),
                                    DEXJPUS = col_number(),
                                    DEXSZUS = col_number()))
  colnames(data) <- c("Date", "Brent", "WTI", "AUDUSD", "CADUSD", "DKKUSD", "USDEUR", "NOKUSD",
                      "SEKUSD", "USDGBP", "OVX", "NOKEUR", "BRLUSD", "MXNUSD", "CHFEUR", "JPYUSD", "CHFUSD")
  
  if (params$rate == "AUDUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, AUDUSD))
  } else if (params$rate == "CADUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, CADUSD))
  } else if (params$rate == "DKKDUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, DKKUSD))
  } else if (params$rate == "USDEUR") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, USDEUR))
  } else if (params$rate == "NOKUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, NOKUSD))
  } else if (params$rate == "SEKUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, SEKUSD))
  } else if (params$rate == "USDGBP") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, USDGBP))
  } else if (params$rate == "NOKEUR") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, NOKEUR))
  } else if (params$rate == "BRLUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, BRLUSD))
  } else if (params$rate == "MXNUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, MXNUSD))
  } else if (params$rate == "CHFEUR") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, CHFEUR))
  } else if (params$rate == "JPYUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, JPYUSD))
  } else if (params$rate == "CHFUSD") {
    data <- subset(data, select = c(Date, Brent, WTI, OVX, CHFUSD))
  }
  
  if (params$oilprice == "Brent") {
    data <- subset(data, select = -WTI)
  } else {
    data <- subset(data, select = -Brent)
  }
  
  if (params$period == "main") {
    if (params$freq == "Monthly") {
      data <- data[(which(data$Date == "2007-06-01")):which(data$Date == "2017-06-01"),]
    } else {
      data <- data[(which(data$Date == "2007-05-10")):which(data$Date == "2017-06-01"),]
    }
  } else {
    if (params$freq == "Monthly") {
      data <- data[(which(data$Date == "1986-01-01")):which(data$Date == "2010-09-01"),]
    } else {
      data <- data[(which(data$Date == "1986-01-01")):which(data$Date == "2010-10-05"),]
    }
  }
  
  if (params$period == "FRR") {
    data <- subset(data, select = -OVX)
    colnames(data) <- c("date", "oilprice", "xrate")
  } else {
    colnames(data) <- c("date", "oilprice", "vol", "xrate")
  }
  
  return(na.omit(data))
  
}


# Creates out-of-sample results and optionally writes the results for all windows given in params to
# CSV files in the "Results" subfolder

out_sample <- function(params, write = TRUE) {
  
  #filepaths for writing results if write = TRUE
  if (params$period == "FRR") {
    filepaths <- paste("Results/",
                       paste(params$rate, params$oilprice, params$voltype,
                             params$freq, params$period, params$window),
                       ".csv", sep = "")
  } else {
    filepaths <- paste("Results/",
                     paste(params$rate, params$oilprice, params$voltype, params$freq, params$window),
                     ".csv", sep = "")
  }
  
  
  #function that produces coefficients, robust standard errors, and predictions for given parameters
  rolling_forecast <- function(data, window, voltype = params$voltype) {
    
    xrate <- diff(log(data$xrate))
    oilprice <- diff(log(data$oilprice))
    N <- length(xrate)
    R <- round(N/(window+1))
    
    #function that produces rolling-forecast results for a single date
    oneroll <- function(x) {
      
      tryCatch({
        
        withTimeout(
          {
            # print(paste(window, data$date[x+R+1]))
        
            if (params$voltype == "OVX") {
              y <- xrate[(x+1):(x+R-1)]
              x1 <- oilprice[(x):(x+R-2)]
              x2 <- diff(log(data$vol))[x:(x+R-2)]
              pvol <- diff(log(data$vol))[x+R-1]
            } else {
              y <- xrate[(x+2):(x+R-1)]
              x1 <- oilprice[(x+1):(x+R-2)]
              spec <- ugarchspec(variance.model=list(model=strsplit(params$voltype, split = " ")[[1]][1]),
                                 mean.model=list(armaOrder=c(1,0,0)),
                                 distribution.model=strsplit(params$voltype, split = " ")[[1]][2])
              fit <- ugarchfit(spec, oilprice[x:(x+R-1)], solver="hybrid",
                               solver.control = list(ftol_rel = 1e-6))
              x2 <- diff(log(as.numeric(sigma(fit)^2)))[1:(R-2)]
              pvol <- diff(log(as.numeric(sigma(fit)^2)))[R-1]
            }
            
            aug <- lm(y ~ x1 + x2)
            price <- lm(y ~ x1)
            
            return(cbind(coeftest(aug, vcov. = NeweyWest(aug, prewhite = FALSE))[1,1],
                         coeftest(aug, vcov. = NeweyWest(aug, prewhite = FALSE))[1,2],
                         coeftest(aug, vcov. = NeweyWest(aug, prewhite = FALSE))[2,1],
                         coeftest(aug, vcov. = NeweyWest(aug, prewhite = FALSE))[2,2],
                         coeftest(aug, vcov. = NeweyWest(aug, prewhite = FALSE))[3,1],
                         coeftest(aug, vcov. = NeweyWest(aug, prewhite = FALSE))[3,2],
                         coeftest(price, vcov. = NeweyWest(price, prewhite = FALSE))[1,1],
                         coeftest(price, vcov. = NeweyWest(price, prewhite = FALSE))[1,2],
                         coeftest(price, vcov. = NeweyWest(price, prewhite = FALSE))[2,1],
                         coeftest(price, vcov. = NeweyWest(price, prewhite = FALSE))[2,2],
                         pvol))
          },
        timeout = 300,
        onTimeout = "error")
        
      },
      
      error = function(msg) {
        message(paste(window, data$date[x+R+1], x))
        return(rbind(rep(NA, length = 11)))
      })
      
    }
    
    roll <- Reduce(rbind, lapply(1:(N-R), oneroll))
    result <- cbind(data$date[(R+2):(N+1)], data.frame(cbind(roll[,1:10],
                                                             roll[,1] + roll[,3] * oilprice[R:(N-1)] + roll[,5] * roll[,11],
                                                             roll[,7] + roll[,9] * oilprice[R:(N-1)],
                                                             rep(0, length = (N-R)),
                                                             xrate[(R+1):N])))
    
    colnames(result) <- c("date", "aug_a", "aug_a_se", "aug_b1", "aug_b1_se", "aug_b2", "aug_b2_se",
                          "price_a", "price_a_se", "price_b1", "price_b1_se",
                          "aug_p", "price_p", "random_walk", "realized")
    rownames(result) <- NULL
    return(result)
  }
  
  #function that runs rolling_forecast for a single window, takes integer input for use in lapply()
  one_window <- function(x) {
    rolling_forecast(loaddata(params), params$window[x])
  }
  
  #writes CSV files from results if write = TRUE
  write_from_list <- function(x) {
    write.csv(input[[x]], file = filepaths[x], row.names = FALSE)
  }
  
  input <- lapply(1:length(params$window), one_window)
  
  if (write) {
    written <- lapply(1:length(input), write_from_list)
  }
  
  #the output of the function is a list of dataframes containing data from each window specified in params
  return(input)
}


#Creates in-sample forecasts and optionally writes to CSV in the "Results" subfolder

in_sample <- function(params, write = TRUE) {
  
  data <- loaddata(params)
  
  y <- diff(log(data$xrate))
  x1 <- diff(log(data$oilprice))
  N <- length(y)
  
  if (params$voltype == "OVX") {
    x2 <- diff(log(data$vol))
  } else {
    y <- y[2:N]
    x1 <- x1[2:N]
    N <- length(y)
    spec <- ugarchspec(variance.model=list(model=strsplit(params$voltype, split = " ")[[1]][1]),
                       mean.model=list(armaOrder=c(1,0,0)),
                       distribution.model=strsplit(params$voltype, split = " ")[[1]][2])
    fit <- ugarchfit(spec, diff(log(data$oilprice)), solver="hybrid")
    x2 <- diff(log(as.numeric(sigma(fit)^2)))
  }
  
  aug <- lm(y[2:N] ~ x1[1:(N-1)] + x2[1:(N-1)])
  price <- lm(y[2:N] ~ x1[1:(N-1)])
  
  predictions <- data.frame(cbind(aug$fitted.values,
                                  price$fitted.values,
                                  rep(0, length = (N-1)),
                                  y[2:N]))

  predictions <- cbind(data$date[3:(N+1)], predictions)

  colnames(predictions) <- c("date", "aug_p", "price_p", "random_walk", "realized")
  rownames(predictions) <- NULL

  if (write == TRUE) {
    if (params$period == "FRR") {
      filepath <- paste("Results/",
                        paste(params$rate, params$oilprice, params$voltype,
                              params$freq, params$period, "In-Sample"),
                        ".csv", sep = "")
    } else {
      filepath <- paste("Results/",
                      paste(params$rate, params$oilprice, params$voltype, params$freq, "In-Sample"),
                      ".csv", sep = "")
    }

    write.csv(predictions, file = filepath, row.names = FALSE)
  }

  return(list("Augmented" = aug, "Price" = price, "Predictions" = predictions))
  
}