library(MCS)
library(rugarch)
library(fpp)
library(readr)

# "params" takes values that determine which forecasts are evaluated.

# rates: "AUDUSD" "BRZUSD" "CADUSD" "CHFEUR" "CHFUSD" "DKKUSD" "JPYUSD" MXNUSD"
#        "NOKGBP" "NOKUSD" "SEKUSD" "USDEUR" "USDGBP"
# freqs: "Daily" "Monthly"
# voltypes: "OVX" "sGARCH norm" "eGARCH norm" "gjrGARCH norm" "sGARCH std" "eGARCH std" "gjrGARCH std"
# oilprice: "Brent" "WTI"
# period: "main" "FRR"

get_params <- function(rate = "CADUSD", freq = "Daily", voltype = "OVX", oilprice = "WTI",
                       period = "main", window = c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 2, 3, 4, 5, 6, 7, 8, 9)) {
  return(list("rate" = rate, "freq" = freq, "voltype" = voltype, "oilprice" = oilprice,
              "period" = period, "window" = window))
}


#Produces evaluation for one exchange rate/oil price/volatility type/frequency
#Includes DA, DM (MAE and MSE), and CW

evaluate <- function(params, write = TRUE, insample = TRUE) {
  
  if (params$period == "FRR") {
    loadpath <- paste("Results/",
                      paste(params$rate, params$oilprice, params$voltype,
                            params$freq, params$period, params$window),
                      ".csv", sep = "")
  } else {
    loadpath <- paste("Results/",
                    paste(params$rate, params$oilprice, params$voltype, params$freq, params$window),
                    ".csv", sep = "")
  }
  
  
  if (insample) {
    if (params$period == "FRR") {
      loadpath <- append(loadpath,
                         paste("Results/",
                               paste(params$rate, params$oilprice, params$voltype, params$freq,
                                     params$period, "In-Sample"),
                               ".csv", sep = ""))
    } else {
      loadpath <- append(loadpath,
                       paste("Results/",
                             paste(params$rate, params$oilprice, params$voltype, params$freq, "In-Sample"),
                             ".csv", sep = ""))
    }
  }
  
  get_row <- function(x) {
    
    input <- read_csv(loadpath[x], col_types = cols(aug_p = col_number(),
                                                    price_p = col_number(),
                                                    random_walk = col_number(),
                                                    realized = col_number()))
    input <- na.omit(subset(input, select = c(aug_p, price_p, random_walk, realized)))
    colnames(input) <- c("Augmented", "Price", "RW", "Realized")
    
    if ((mean(input$Augmented > 0) == 1) | (mean(input$Augmented < 0) == 1)) {
      message(paste("DA test failed for augmented model; window", params$window[x]))
      DA <- list("DirAcc" = mean(input$Augmented * input$Realized > 0), "p.value" = 1)
    } else {
      DA <- DACTest(input$Augmented, input$Realized, test = "PT")
    }
    MAE <- dm.test(input$Realized, (input$Realized - input$Augmented), alternative = "greater", power = 1)
    MSE <- dm.test(input$Realized, (input$Realized - input$Augmented), alternative = "greater", power = 2)
    CW <- clark_west(input$RW, input$Augmented, input$Realized)
    
    row1 <- cbind(paste(round(100 * DA$DirAcc, 1), "%", sep = ""),
                  round(MAE$statistic, 2), round(MSE$statistic, 2), round(CW$t_stat, 2))
    if (DA$DirAcc < 0.5) {
      pvals1 <- cbind(1, MAE$p.value, MSE$p.value, CW$p_value)
    } else {
      pvals1 <- cbind(DA$p.value, MAE$p.value, MSE$p.value, CW$p_value)
    }
    
    
    if ((mean(input$Price > 0) == 1) | (mean(input$Price < 0) == 1)) {
      message(paste("DA test failed for price model; window", params$window[x]))
      DA <- list("DirAcc" = mean(input$Price * input$Realized > 0), "p.value" = 1)
    } else {
      DA <- DACTest(input$Price, input$Realized, test = "PT")
    }
    MAE <- dm.test(input$Realized, (input$Realized - input$Price), alternative = "greater", power = 1)
    MSE <- dm.test(input$Realized, (input$Realized - input$Price), alternative = "greater", power = 2)
    CW <- clark_west(input$RW, input$Price, input$Realized)
    
    row2 <- cbind(paste(round(100 * DA$DirAcc, 1), "%", sep = ""),
                  round(MAE$statistic, 2), round(MSE$statistic, 2), round(CW$t_stat, 2))
    if (DA$DirAcc < 0.5) {
      pvals2 <- cbind(1, MAE$p.value, MSE$p.value, CW$p_value)
    } else {
      pvals2 <- cbind(DA$p.value, MAE$p.value, MSE$p.value, CW$p_value)
    }
    
    return(cbind(row1, row2, pvals1, pvals2))
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
  colnames(evals) <- c("SR", "DM (MAE)", "DM (MSE)", "CW", "SR", "DM (MAE)", "DM (MSE)", "CW")
  
  if (insample) {
    rownames(evals) <- c(params$window, "In-Sample")
  } else {
    rownames(evals) <- params$window
  }
  
  if (write) {
    if (params$period == "FRR") {
      savepath <- paste("Results/", paste(params$rate, params$oilprice, params$voltype, params$freq,
                                          params$period, "Evaluation"),
                        ".csv", sep = "")
    } else {
      savepath <- paste("Results/", paste(params$rate, params$oilprice, params$voltype, params$freq, "Evaluation"),
                      ".csv", sep = "")
    }
    
    write.csv(evals, file = savepath)
  }
  
  return(evals)
  
}


#Produces MCS test results

print_MCS <- function(params, insample = TRUE) {
  
  if (params$period == "FRR") {
    loadpath <- paste("Results/",
                      paste(params$rate, params$oilprice, params$voltype,
                            params$freq, params$period, params$window),
                      ".csv", sep = "")
  } else {
    loadpath <- paste("Results/",
                      paste(params$rate, params$oilprice, params$voltype, params$freq, params$window),
                      ".csv", sep = "")
  }
  
  
  if (insample) {
    if (params$period == "FRR") {
      loadpath <- append(loadpath,
                         paste("Results/",
                               paste(params$rate, params$oilprice, params$voltype, params$freq,
                                     params$period, "In-Sample"),
                               ".csv", sep = ""))
    } else {
      loadpath <- append(loadpath,
                         paste("Results/",
                               paste(params$rate, params$oilprice, params$voltype, params$freq, "In-Sample"),
                               ".csv", sep = ""))
    }
  }
  
  iteration <- function(x) {
    input <- read_csv(loadpath[x], col_types = cols(aug_p = col_number(),
                                                        price_p = col_number(),
                                                        random_walk = col_number(),
                                                        realized = col_number()))
    input <- na.omit(subset(input, select = c(date, aug_p, price_p, random_walk, realized)))
    
    MAEloss <- data.frame(LossLevel(input$realized, input$aug_p, which = "AE"),
                          LossLevel(input$realized, input$price_p, which = "AE"),
                          LossLevel(input$realized, input$random_walk, which = "AE"))
    colnames(MAEloss) <- c("Augmented", "Price", "Random Walk")
    
    MSEloss <- data.frame(LossLevel(input$realized, input$aug_p, which = "SE"),
                          LossLevel(input$realized, input$price_p, which = "SE"),
                          LossLevel(input$realized, input$random_walk, which = "SE"))
    colnames(MSEloss) <- c("Augmented", "Price", "Random Walk")


    MAE <- MCSprocedure(MAEloss, alpha = 0.25)
    MSE <- MCSprocedure(MSEloss, alpha = 0.25)

    return(list(MAE, MSE))
  }
  
  if (insample) {
    N <- 15
  } else {
    N <- 14
  }
  
  return(lapply(1:N, iteration))
  
}