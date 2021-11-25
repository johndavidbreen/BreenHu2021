library(tidyverse)
library(readr)
library(viridis)
library(grid)
library(gridExtra)

# "params" takes values that determine which forecasts are evaluated.
#rates: "AUDUSD" "BRZUSD" "CADUSD" "CHFEUR" DKKUSD" "JPYUSD" MXNUSD" "NOKGBP" "NOKUSD" "SEKUSD" "USDEUR" "USDGBP"
#freqs: "Daily" "Monthly"
#voltypes: "OVX" "sGARCH norm" "eGARCH norm" "gjrGARCH norm" "sGARCH std" "eGARCH std" "gjrGARCH std"
#oilprice: "Brent" "WTI"

get_params <- function(rate = "CADUSD", freq = "Daily", voltype = "OVX", oilprice = "WTI", period = "main",
                       window = 1) {
  return(list("rate" = rate, "freq" = freq, "voltype" = voltype, "oilprice" = oilprice, "period" = period,
              "window" = window))
}


#CADUSD graphs

params <- get_params()

data <- read_csv(paste("Results/",
                       paste(params$rate, params$oilprice, params$voltype, params$freq, params$window),
                       ".csv", sep = ""))

cadusd <- ggplot(data, aes(x = data$date)) +
  geom_line(aes(y = random_walk), linetype = "dashed") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(legend.title = element_blank())

cadusd_phi <- cadusd +
  geom_ribbon(aes(ymin = aug_b1 - 1.96 * aug_b1_se, ymax = aug_b1 + 1.96 * aug_b1_se), alpha = 0.3) +
  geom_line(aes(y = aug_b1)) +
  ggtitle(expression("CADUSD:" ~ hat(phi[t])))

cadusd_lambda <- cadusd +
  geom_ribbon(aes(ymin = aug_b2 - 1.96 * aug_b2_se, ymax = aug_b2 + 1.96 * aug_b2_se), alpha = 0.3) +
  geom_line(aes(y = aug_b2)) +
  ggtitle(expression("CADUSD:" ~ hat(lambda[t])))

png("Graphs/CADUSD coefs.png", width = 1000)
grid.arrange(cadusd_phi, cadusd_lambda, nrow = 1, ncol = 2)
dev.off()

# ggsave("Graphs/CADUSD coefs.png")

#AUDUSD graphs

params <- get_params(rate = "AUDUSD", oilprice = "Brent")

data <- read_csv(paste("Results/",
                       paste(params$rate, params$oilprice, params$voltype, params$freq, params$window),
                       ".csv", sep = ""))

audusd <- ggplot(data, aes(x = data$date)) +
  geom_line(aes(y = random_walk), linetype = "dashed") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(legend.title = element_blank())

audusd_phi <- audusd +
  geom_ribbon(aes(ymin = aug_b1 - 1.96 * aug_b1_se, ymax = aug_b1 + 1.96 * aug_b1_se), alpha = 0.3) +
  geom_line(aes(y = aug_b1)) +
  ggtitle(expression("AUDUSD:" ~ hat(phi[t])))

audusd_lambda <- audusd +
  geom_ribbon(aes(ymin = aug_b2 - 1.96 * aug_b2_se, ymax = aug_b2 + 1.96 * aug_b2_se), alpha = 0.3) +
  geom_line(aes(y = aug_b2)) +
  ggtitle(expression("AUDUSD:" ~ hat(lambda[t])))

png("Graphs/AUDUSD coefs.png", width = 1000)
grid.arrange(audusd_phi, audusd_lambda, nrow = 1, ncol = 2)
dev.off()

#NOKUSD graphs

params <- get_params(rate = "NOKUSD", oilprice = "Brent")

data <- read_csv(paste("Results/",
                       paste(params$rate, params$oilprice, params$voltype, params$freq, params$window),
                       ".csv", sep = ""))

nokusd <- ggplot(data, aes(x = data$date)) +
  geom_line(aes(y = random_walk), linetype = "dashed") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(legend.title = element_blank())

nokusd_phi <- nokusd +
  geom_ribbon(aes(ymin = aug_b1 - 1.96 * aug_b1_se, ymax = aug_b1 + 1.96 * aug_b1_se), alpha = 0.3) +
  geom_line(aes(y = aug_b1)) +
  ggtitle(expression("NOKUSD:" ~ hat(phi[t])))

nokusd_lambda <- nokusd +
  geom_ribbon(aes(ymin = aug_b2 - 1.96 * aug_b2_se, ymax = aug_b2 + 1.96 * aug_b2_se), alpha = 0.3) +
  geom_line(aes(y = aug_b2)) +
  ggtitle(expression("NOKUSD:" ~ hat(lambda[t])))

png("Graphs/NOKUSD coefs.png", width = 1000)
grid.arrange(nokusd_phi, nokusd_lambda, nrow = 1, ncol = 2)
dev.off()


#NOKEUR graphs

params <- get_params(rate = "NOKEUR", oilprice = "Brent")

data <- read_csv(paste("Results/",
                       paste(params$rate, params$oilprice, params$voltype, params$freq, params$window),
                       ".csv", sep = ""))

nokeur <- ggplot(data, aes(x = data$date)) +
  geom_line(aes(y = random_walk), linetype = "dashed") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(legend.title = element_blank())

nokeur_phi <- nokeur +
  geom_ribbon(aes(ymin = aug_b1 - 1.96 * aug_b1_se, ymax = aug_b1 + 1.96 * aug_b1_se), alpha = 0.3) +
  geom_line(aes(y = aug_b1)) +
  ggtitle(expression("NOKEUR:" ~ hat(phi[t])))

nokeur_lambda <- nokeur +
  geom_ribbon(aes(ymin = aug_b2 - 1.96 * aug_b2_se, ymax = aug_b2 + 1.96 * aug_b2_se), alpha = 0.3) +
  geom_line(aes(y = aug_b2)) +
  ggtitle(expression("NOKEUR:" ~ hat(lambda[t])))

png("Graphs/NOKEUR coefs.png", width = 1000)
grid.arrange(nokeur_phi, nokeur_lambda, nrow = 1, ncol = 2)
dev.off()


#Graphs for NOK, CAD, and Brent/WTI/OVX

data <- read_csv("Data/Daily.csv", col_types = cols(DCOILBRENTEU = col_number(),
                                                    DCOILWTICO = col_number(),
                                                    OVX = col_number(),
                                                    DEXNOEU = col_number()))
data <- subset(data, select = c(DATE, DCOILBRENTEU, DCOILWTICO, DEXUSAL, DEXCAUS, DEXNOUS, DEXNOEU, OVX))
colnames(data) <- c("Date", "Brent", "WTI", "AUDUSD", "CADUSD", "NOKUSD", "NOKEUR", "OVX")
data$AUDUSD <- 1/data$AUDUSD

df <- data[which(data$Date == "2007-05-10"):which(data$Date == "2017-06-01"),]

ggplot(df, aes(x = Date)) +
  labs(caption = "Rates are rescaled so that each rate as of 10 May 2007 is equal to 1.0") +
  geom_line(aes(y = AUDUSD/AUDUSD[1], color = "AUD/USD", linetype = "AUD/USD")) +
  geom_line(aes(y = CADUSD/CADUSD[1], color = "CAD/USD", linetype = "CAD/USD")) +
  geom_line(aes(y = NOKUSD/NOKUSD[1], color = "NOK/USD", linetype = "NOK/USD")) +
  scale_color_manual(name = "Series", values = c("AUD/USD" = "grey50",
                                                 "CAD/USD" = "grey25", "NOK/USD" = "black")) +
  scale_linetype_manual(name = "Series", values = c("AUD/USD" = "solid",
                                                    "CAD/USD" = "dotted", "NOK/USD" = "dashed")) +
  ylab("") +
  xlab("") +
  scale_y_continuous(breaks = c(0.8, 1, 1.2, 1.4)) +
  theme_minimal() +
  theme(legend.title = element_blank())
ggsave("Graphs/USD xrates.png")



ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Brent, color = "Brent", linetype = "Brent")) +
  geom_line(aes(y = WTI, color = "WTI", linetype = "WTI")) +
  geom_line(aes(y = OVX, color = "OVX", linetype = "OVX")) +
  ylab("") +
  xlab("") +
  scale_color_manual(name = "Series", values = c("Brent" = "grey50", "WTI" = "grey25", "OVX" = "black"),
                     limits = c("Brent", "WTI", "OVX")) +
  scale_linetype_manual(name = "Series", values = c("Brent" = "solid", "WTI" = "dotted", "OVX" = "dashed"),
                        limits = c("Brent", "WTI", "OVX")) +
  theme_minimal() +
  theme(legend.title = element_blank())
ggsave("Graphs/Oil Price and Volatility.png")



#Fitted Quadratic Curve of Brent returns against OVX

data <- read_csv("Data/Daily.csv", col_types = cols(DCOILBRENTEU = col_number(),
                                                    OVX = col_number()))
data <- na.omit(subset(data, select = c(DCOILBRENTEU, OVX)))
data <- data.frame(log(data$OVX[2:length(data$OVX)]), diff(log(data$DCOILBRENTEU)))
colnames(data) <- c("logOVX", "returns")

ggplot(data, aes(y = logOVX, x = returns)) +
  geom_point(color = "grey50") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  ylab("log(OVX)") +
  theme_minimal() +
  theme(legend.title = element_blank())
ggsave("Graphs/Fitted Quadratic Curve.png")
