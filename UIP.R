library(readr)
library(fpp)
library(sandwich)
library(MCS)
library(rugarch)

# Assigns parameters for use in the functions 'outsample_UIP', 'insample_UIP', 'evaluate_UIP',
# and 'print_MCS_UIP'
uip_params <- function(rate = "CADUSD", freq = "Monthly",
                       window = c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 2, 3, 4, 5, 6, 7, 8, 9)) {
  return(list("rate" = rate, "freq" = freq, "window" = window))
  }

# Produces out-of-sample forecasts for UIP under given parameters

outsample_UIP <- function(params, write = TRUE) {
  
  if (params$rate == "CADUSD") {
    home <- "US"
    foreign <- "Canada"
    rate <- "DEXCAUS"
  } else if (params$rate == "NOKEUR") {
    home <- "Euro"
    foreign <- "Norway"
    rate <- "DEXNOEU"
  } else if (params$rate == "NOKUSD") {
    home <- "US"
    foreign <- "Norway"
    rate <- "DEXNOUS"
  } else if (params$rate == "AUDUSD") {
    home <- "US"
    foreign <- "Australia"
    rate <- "DEXUSAL"
  }
  
  if (params$freq == "Monthly") {
    interest <- read_csv("Data/UIP Monthly.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))
    exchangerates <- read_csv("Data/Monthly.csv")
    start <- "2007-06-01"
  } else {
    interest <- read_csv("Data/UIP Daily.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))
    exchangerates <- read_csv("Data/Daily.csv", col_types = cols(DCOILBRENTEU = col_number(),
                                                                 DCOILWTICO = col_number(),
                                                                 DEXUSEU = col_number(),
                                                                 OVX = col_number(),
                                                                 DEXNOEU = col_number(),
                                                                 DEXBZUS = col_number(),
                                                                 DEXMXUS = col_number(),
                                                                 CHFEUR = col_number()))
    start <- "2007-05-10"
  }
  
  df <- merge(subset(interest, select = c("Date", foreign, home)),
                subset(exchangerates, select = c("DATE", rate)), by.x = "Date", by.y = "DATE")
  colnames(df) <- c("Date", "Foreign", "Home", "ExchangeRate")
  
  df <- df[which(df$Date == start):which(df$Date == "2017-06-01"),]
  df <- na.omit(df)
  
  rolling_forecast <- function(data, window) {
    
    xr_return <- diff(log(data$ExchangeRate))
    N <- length(xr_return)
    idiff <- (data$Foreign - data$Home)[1:N]
    R <- round(N/(window+1))
    
    oneroll <- function(x) {
      y <- xr_return[x:(x+R-1)]
      x <- idiff[x:(x+R-1)]
      
      ols <- lm(y ~ x)
      
      return(cbind(coeftest(ols, vcov. = NeweyWest(ols, prewhite = FALSE))[1,1],
                   coeftest(ols, vcov. = NeweyWest(ols, prewhite = FALSE))[1,2],
                   coeftest(ols, vcov. = NeweyWest(ols, prewhite = FALSE))[2,1],
                   coeftest(ols, vcov. = NeweyWest(ols, prewhite = FALSE))[2,2]))
    }
    
    roll <- Reduce(rbind, lapply(1:(N-R), oneroll))
    
    result <- cbind(data$Date[(R+2):(N+1)], data.frame(cbind(roll[,1:4],
                                                             roll[,1] + roll[,3] * idiff[(R+1):N],
                                                             rep(0, length = (N-R)),
                                                             xr_return[(R+1):N])))
    
    colnames(result) <- c("date", "alpha", "alpha_se", "beta", "beta_se", "uip_p", "random_walk", "realized")
    rownames(result) <- NULL
    
    return(result)
    
  }
  
  one_window <- function(x) {
    rolling_forecast(df, params$window[x])
  }
  
  write_from_list <- function(x) {
    write.csv(input[[x]], file = filepaths[x], row.names = FALSE)
  }
  
  input <- lapply(1:length(params$window), one_window)
  
  if (write) {
    filepaths <- paste("Results/", paste(params$rate, "UIP", params$freq, params$window), ".csv", sep = "")
    written <- lapply(1:length(input), write_from_list)
  }
  
  return(input)
  
}


# Produces in-sample forecasts for UIP under given parameters

insample_UIP <- function(params, write = TRUE) {
  
  if (params$rate == "CADUSD") {
    home <- "US"
    foreign <- "Canada"
    rate <- "DEXCAUS"
  } else if (params$rate == "NOKEUR") {
    home <- "Euro"
    foreign <- "Norway"
    rate <- "DEXNOEU"
  } else if (params$rate == "NOKUSD") {
    home <- "US"
    foreign <- "Norway"
    rate <- "DEXNOUS"
  } else if (params$rate == "AUDUSD") {
    home <- "US"
    foreign <- "Australia"
    rate <- "DEXUSAL"
  }
  
  if (params$freq == "Monthly") {
    interest <- read_csv("Data/UIP Monthly.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))
    exchangerates <- read_csv("Data/Monthly.csv")
    start <- "2007-06-01"
  } else {
    interest <- read_csv("Data/UIP Daily.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))
    exchangerates <- read_csv("Data/Daily.csv", col_types = cols(DCOILBRENTEU = col_number(),
                                                                 DCOILWTICO = col_number(),
                                                                 DEXUSEU = col_number(),
                                                                 OVX = col_number(),
                                                                 DEXNOEU = col_number(),
                                                                 DEXBZUS = col_number(),
                                                                 DEXMXUS = col_number(),
                                                                 CHFEUR = col_number()))
    start <- "2007-05-10"
  }
  
  df <- merge(subset(interest, select = c("Date", foreign, home)),
                subset(exchangerates, select = c("DATE", rate)), by.x = "Date", by.y = "DATE")
  colnames(df) <- c("Date", "Foreign", "Home", "ExchangeRate")
  
  df <- df[which(df$Date == start):which(df$Date == "2017-06-01"),]
  df <- na.omit(df)
  
  x <- (df$Foreign - df$Home)[1:(dim(df)[1] - 1)]
  y <- diff(log(df$ExchangeRate))
  ols <- lm(y ~ x)

  predictions <- data.frame(cbind(ols$fitted.values,
                                  rep(0, length = (dim(df)[1] - 1)),
                                      y))
  predictions <- cbind(df$Date[2:dim(df)[1]], predictions)
  colnames(predictions) <- c("date", "uip_p", "random_walk", "realized")
  rownames(predictions) <- NULL

  if (write) {
    write.csv(predictions,
              file = paste("Results/", paste(params$rate, "UIP", params$freq, "In-sample"), ".csv", sep = ""),
              row.names = FALSE)
  }

  return(list("UIP" = ols, "predictions" = predictions))
}



#Produces evaluation for one exchange rate/oil price/volatility type/frequency
#Includes DA, DM (MAE and MSE), and CW

evaluate_UIP <- function(params, write = TRUE, insample = TRUE) {
  
  loadpath <- paste("Results/", paste(params$rate, "UIP", params$freq, params$window), ".csv", sep = "")
  if (insample) {
    loadpath <- append(loadpath, paste("Results/", paste(params$rate, "UIP", params$freq, "In-sample"),
                                       ".csv", sep = ""))
  }
  
  get_row <- function(x) {
    
    input <- read_csv(loadpath[x])
    input <- subset(input, select = c(uip_p, random_walk, realized))
    colnames(input) <- c("UIP", "RW", "Realized")
    
    if ((mean(input$UIP > 0) == 1) | (mean(input$UIP < 0) == 1)) {
      message(paste("DA test failed for UIP model; window", params$window[x]))
      DA <- list("DirAcc" = mean(input$UIP * input$Realized > 0), "p.value" = 1)
    } else {
      DA <- DACTest(input$UIP, input$Realized, test = "PT")
    }
    MAE <- dm.test(input$Realized, (input$Realized - input$UIP), alternative = "greater", power = 1)
    MSE <- dm.test(input$Realized, (input$Realized - input$UIP), alternative = "greater", power = 2)
    CW <- clark_west(input$RW, input$UIP, input$Realized)
    
    row <- cbind(paste(round(100 * DA$DirAcc, 1), "%", sep = ""),
                  round(MAE$statistic, 2), round(MSE$statistic, 2), round(CW$t_stat, 2))
    if (DA$DirAcc < 0.5) {
      pvals <- cbind(1, MAE$p.value, MSE$p.value, CW$p_value)
    } else {
      pvals <- cbind(DA$p.value, MAE$p.value, MSE$p.value, CW$p_value)
    }
    
    return(cbind(row, pvals))
  }
  
  format <- function(df) {
    df.x <- df[,1:(dim(df)[2]/2)]
    df.p <- df[,(1 + dim(df)[2]/2):dim(df)[2]]
    df.star <- matrix(data = NA, nrow = dim(df.p)[1], ncol = dim(df.p)[2])
    
    df.star[df.x > 0 & as.numeric(df.p) < 0.05] <- " *"
    df.star[as.numeric(df.p) < 0.01] <- " **"
    df.star[as.numeric(df.p) < 0.001] <- " ***"
    df.star[df.x < 0] <- ""
    df.star[is.na(df.star)] <- ""
    
    return(matrix(nrow = dim(df)[1], paste(df.x, df.star, sep = "")))
    
  }
  
  clark_west <- function(y1, y2, y) {
    f <- (y-y1)^2 - (y-y2)^2 + (y1-y2)^2
    ols <- lm(f ~ 1)
    a <- summary(ols)$coefficients[1,1]
    b <- summary(ols)$coefficients[1,3]
    c <- summary(ols)$coefficients[1,4]
    abc <- list("coef" = a, "t_stat" = b, "p_value" = c)
    return(abc)
  }
  
  evals <- format(Reduce(rbind, lapply(1:length(loadpath), get_row)))
  colnames(evals) <- c("SR", "DM (MAE)", "DM (MSE)", "CW")
  
  if (insample) {
    rownames(evals) <- c(params$window, "In-Sample")
  } else {
    rownames(evals) <- params$window
  }
  
  if (write) {
    written <- write.csv(evals, file = paste("Results/", paste(params$rate, "UIP", params$freq, "Evaluation"),
                                             ".csv", sep = ""),
              row.names = FALSE)
  }
  
  return(evals)
  
}


#Produces MCS test results

print_MCS_UIP <- function(params, insample = TRUE) {
  
  if (params$rate == "CADUSD") {
    oilprice <- "WTI"
  } else {
    oilprice <- "Brent"
  }
  
  loadpath <- paste("Results/", paste(params$rate, "UIP", params$freq, params$window), ".csv", sep = "")
  if (insample) {
    loadpath <- append(loadpath, paste("Results/", paste(params$rate, "UIP", params$freq, "In-sample"),
                                       ".csv", sep = ""))
  }
  
  loadpath2 <- paste("Results/", paste(params$rate, oilprice, "OVX", params$freq, params$window), ".csv", sep = "")
  if (insample) {
    loadpath2 <- append(loadpath2, paste("Results/", paste(params$rate, oilprice, "OVX", params$freq, "In-sample"),
                                       ".csv", sep = ""))
  }
  
  
  loadpath <- cbind(loadpath, loadpath2)
  
  iteration <- function(x) {
    uip <- read_csv(loadpath[x,1])
    uip <- subset(uip, select = c(date, uip_p, random_walk, realized))
    colnames(uip) <- c("date", "UIP", "RW", "Realized")
    
    main <- read_csv(loadpath[x,2])
    main <- subset(main, select = c(date, aug_p, price_p))
    colnames(main) <- c("date", "Augmented", "Price")
    
    input <- merge(uip, main, by = "date")
    
    loss <- data.frame(LossLevel(input$Realized, input$Augmented, which = "AE"),
                       LossLevel(input$Realized, input$Price, which = "AE"),
                       LossLevel(input$Realized, input$UIP, which = "AE"),
                       LossLevel(input$Realized, input$RW, which = "AE"))
    colnames(loss) <- c("Augmented", "Price", "UIP", "RW")
    MAE <- MCSprocedure(loss, alpha = 0.25)
    
    loss <- data.frame(LossLevel(input$Realized, input$Augmented, which = "SE"),
                       LossLevel(input$Realized, input$Price, which = "SE"),
                       LossLevel(input$Realized, input$UIP, which = "SE"),
                       LossLevel(input$Realized, input$RW, which = "SE"))
    colnames(loss) <- c("Augmented", "Price", "UIP", "RW")
    MSE <- MCSprocedure(loss, alpha = 0.25)
    
    return(list(MAE, MSE))
  }
  
  return(lapply(1:length(loadpath[,1]), iteration))
}