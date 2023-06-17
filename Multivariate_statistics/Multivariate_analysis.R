library(lubridate)
library(dplyr)
library(corrplot)
library(gplots)



# load data
house <- read.csv("kc_house_data.csv")


house$date <- substr(house$date, 1, 8)
house$date <- as.Date(house$date, "%Y%m%d")
house$year <- year(house$date)
house$month <- month(house$date)
house$day <- day(house$date)


house$log_price <- log(house$price)
house$log_sqft_living <- log(house$sqft_living)
house$log_sqft_lot <- log(house$sqft_lot)
house$log_sqft_above <- log(house$sqft_above)
house$log_sqft_living15 <- log(house$sqft_living15)
house$log_sqft_lot15 <- log(house$sqft_lot15)

house$bathrooms <- as.integer(house$bathrooms)

# table(house$sqft_basement)

# outlier
boxplot(house$bedrooms)
house <- house[house$bedrooms <= 30, ]

house$renovated <- ifelse(house$yr_renovated != 0, 1, 0)
house$basement <- ifelse(house$sqft_basement == 0, 0, 1)

house <- house[, -c(2, 3, 6, 7, 13, 14, 16, 20, 21)]
head(house)


house_new <- house %>%
  select("bedrooms", "bathrooms", "floors", "yr_built", "view", "condition",
         "grade", "lat", "long", "log_sqft_living", "log_sqft_lot",
         "log_sqft_above", "month", "log_price")

head(house_new)

# correlation plot
corrplot.mixed(corr = cor(house[, -1], use = "complete.obs"), 
               upper = "ellipse", tl.pos = "lt")



########## FA ##########

# standardization
house_new <- scale(house_new)
house_new_corr <- cor(house_new)

# scree plot
eigenvalues <- eigen(house_new_corr)$values
plot(eigenvalues, type = "b",
     xlab = "", ylab = "Eigenvalues",
     main = "Scree Plot")


# Maximum Likelihood Method
mlm <- factanal(house_new[, -14], factors = 4, rotation = "none", covmat = house_new_corr[-14, -14])
loadings <- mlm$loadings[, 1:4]   # loading matrix
communality <- diag(loadings %*% t(loadings))   # communality
FA <- cbind(loadings, communality)


# rotation
loadings_rotation <- varimax(loadings)
loadings_rotation <- loadings_rotation$loadings[, 1:4]
communality_rotation <- diag(loadings_rotation %*% t(loadings_rotation))
var <- diag(house_new_corr) - communality_rotation
FA_rotation <- cbind(loadings_rotation, communality_rotation, var)
FA_rotation <- round(FA_rotation, 3)
# colnames(FA_rotation) <- c("Factor1", "Factor2", "Factor3", "Communality", "Specific Variances")

par(mfrow = c(1, 2))
# Factor 1 vs Factor 2
plot(loadings_rotation[, 1], loadings_rotation[, 2], type = "n",
     main = "Factor 1 vs Factor 2",
     xlab = "Factor 1", ylab = "Factor 2",
     xlim = c(-1, 1), ylim = c(-1, 1))
# text(-0.1, 0.7, "year")
# text(0.1, -0.9, "month")
text(loadings_rotation[, 1], loadings_rotation[, 2], colnames(house_new[, -14]), cex = 0.8)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 2 vs Factor 3
plot(loadings_rotation[, 3], loadings_rotation[, 4], type = "n",
     xlab = "Factor 3", ylab = "Factor 4",
     main = "Factor 3 vs Factor 4",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 3], loadings_rotation[, 4], colnames(house_new[, -14]), cex = 0.8)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# 3 Factors


# Maximum Likelihood Method
mlm <- factanal(house_new[, -14], factors = 3, rotation = "none", covmat = house_new_corr[-14, -14])
loadings <- mlm$loadings[, 1:3]   # loading matrix
communality <- diag(loadings %*% t(loadings))   # communality
FA <- cbind(loadings, communality)


# rotation
loadings_rotation <- varimax(loadings)
loadings_rotation <- loadings_rotation$loadings[, 1:3]
communality_rotation <- diag(loadings_rotation %*% t(loadings_rotation))
var <- diag(house_new_corr) - communality_rotation
FA_rotation <- cbind(loadings_rotation, communality_rotation, var)
FA_rotation <- round(FA_rotation, 3)
# colnames(FA_rotation) <- c("Factor1", "Factor2", "Factor3", "Communality", "Specific Variances")

par(mfrow = c(1, 2))
# Factor 1 vs Factor 2
plot(loadings_rotation[, 1], loadings_rotation[, 2], type = "n",
     main = "Factor 1 vs Factor 2",
     xlab = "Factor 1", ylab = "Factor 2",
     xlim = c(-1, 1), ylim = c(-1, 1))
# text(-0.1, 0.7, "year")
# text(0.1, -0.9, "month")
text(loadings_rotation[, 1], loadings_rotation[, 2], colnames(house_new[, -14]), cex = 0.8)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 2 vs Factor 3
plot(loadings_rotation[, 2], loadings_rotation[, 3], type = "n",
     xlab = "Factor 2", ylab = "Factor 3",
     main = "Factor 2 vs Factor 3",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 2], loadings_rotation[, 3], colnames(house_new[, -14]), cex = 0.8)
abline(h = 0, v = 0, lty = 3, lwd = 2)



########## LDA ##########
boxplot(house$log_price)

house$high <- ifelse(house$log_price >= mean(house$log_price), 1, 0)

house_high <- house[house$high == 1, ]
house_low <- house[house$high == 0, ]

house_high <- house_high[, -c(1, 10, 16, 24)]
house_low <- house_low[, -c(1, 10, 16, 24)]

head(house_high)
head(house_low)

high_mean <- colMeans(house_high)
low_mean <- colMeans(house_low)
mean <- (high_mean + low_mean) / 2

w <- nrow(house_high) * cov(house_high) + nrow(house_low) * cov(house_low)
a <- solve(w) %*% (high_mean - low_mean)

high_rule <- as.matrix(house_high - matrix(mean, nrow = nrow(house_high), ncol = ncol(house_high), byrow = T)) %*% a
low_rule <- as.matrix(house_low - matrix(mean, nrow = nrow(house_low), ncol = ncol(house_low), byrow = T)) %*% a

misclassification_high <- sum(high_rule < 0)
classification_high <- sum(high_rule > 0)
misclassification_low <- sum(low_rule > 0)
classification_low <- sum(low_rule < 0)
rate <- (misclassification_high + misclassification_low) / nrow(house)

confusion <- matrix(c(classification_high, misclassification_low, misclassification_high, classification_low),
                    ncol = 2,
                    byrow = TRUE)
# colnames(confusion) <- c("Genuine", "Counterfeit")
# rownames(confusion) <- c("Predicted Genuine", "Predicted Counterfeit")

# plot
plot(density(low_rule), lwd = 3, col = "red",
     xlab = "", ylab = "",
     main = "Densities of Projections of KC House Data")
lines(density(high_rule), lwd = 3, col = "blue")
legend("topleft", legend = c("low", "high"),
       col = c("red", "blue"),
       lty = 1,
       lwd = 3)

