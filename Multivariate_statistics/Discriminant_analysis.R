rm(list = ls(all = TRUE))
graphics.off()

#####
# 1 #
#####


# load data
bank <- read.table("bank2.dat")
head(bank)

genuine <- bank[1:100, ]
counterfeit <- bank[101:200, ]


genuine_mean <- colMeans(genuine)
counterfeit_mean <- colMeans(counterfeit)
mean <- (genuine_mean + counterfeit_mean) / 2

w <- 100 * (cov(genuine) + cov(counterfeit))
a <- solve(w) %*% (genuine_mean - counterfeit_mean)

genuine_rule <- as.matrix(genuine - matrix(mean, nrow = 100, ncol = 6, byrow = T)) %*% a
counterfeit_rule <- as.matrix(counterfeit - matrix(mean, nrow = 100, ncol = 6, byrow = T)) %*% a

misclassification_genuine <- sum(genuine_rule < 0)
classification_genuine <- sum(genuine_rule > 0)
misclassification_counterfeit <- sum(counterfeit_rule > 0)
classification_counterfeit <- sum(counterfeit_rule < 0)
rate <- (misclassification_genuine + misclassification_counterfeit) / 200

confusion <- matrix(c(classification_genuine, misclassification_counterfeit, misclassification_genuine, classification_counterfeit),
                    ncol = 2,
                    byrow = TRUE)
colnames(confusion) <- c("Genuine", "Counterfeit")
rownames(confusion) <- c("Predicted Genuine", "Predicted Counterfeit")

# plot
plot(density(counterfeit_rule), lwd = 3, col = "red",
     xlab = "", ylab = "",
     main = "Densities of Projections of Swiss bank notes", 
     xlim = c(-0.2, 0.2), cex.lab = 1.2, cex.axis = 1.2)
lines(density(genuine_rule), lwd = 3, col = "blue")
legend("topright", legend = c("Counterfeit", "Genuine"),
       col = c("red", "blue"),
       lty = 1,
       lwd = 3)


#####
# 2 #
#####


# load data
WAIS <- read.csv("WAIS.csv")
head(WAIS)

group1 <- WAIS[WAIS$group == 1, ]
group1 <- group1[, 2:5]
group2 <- WAIS[WAIS$group == 2, ]
group2 <- group2[, 2:5]

mean_group1 <- colMeans(group1)
mean_group2 <- colMeans(group2)
mean <- (mean_group1 + mean_group2) / 2

w <- 100 * (cov(group1) + cov(group2))
a <- solve(w) %*% (mean_group1 - mean_group2)

group1_rule <- as.matrix(group1 - matrix(mean, nrow = nrow(group1), ncol = ncol(group1), byrow = T)) %*% a
group2_rule <- as.matrix(group2 - matrix(mean, nrow = nrow(group2), ncol = ncol(group2), byrow = T)) %*% a


misclassification_group1 <- sum(group1_rule < 0)
classification_group1 <- sum(group1_rule > 0)
misclassification_group2 <- sum(group2_rule > 0)
classification_group2 <- sum(group2_rule < 0)
(misclassification_group1 + misclassification_group2) / (12 + 37)

confusion <- matrix(c(classification_group1, misclassification_group2, misclassification_group1, classification_group2),
                    ncol = 2,
                    byrow = TRUE)
colnames(confusion) <- c("True Group1", "True Group2")
rownames(confusion) <- c("Predicted Group1", "Predicted Group2")


# plot
plot(density(group1_rule), lwd = 3, col = "red",
     xlab = "", ylab = "",
     main = "Densities of Projections of WAIS", 
     xlim = c(-0.03, 0.02), cex.lab = 1.2, cex.axis = 1.2)
lines(density(group2_rule), lwd = 3, col = "blue")
legend("topright", legend = c("Group1", "Group2"),
       col = c("red", "blue"),
       lty = 1,
       lwd = 3)



cov(genuine)
cov(counterfeit)
