rm(list=ls())
graphics.off()



# load data
uscrime <- read.csv("uscrime.csv")
uscrime <- uscrime[, 1:11]   # drop the last column
head(uscrime)


# correlation plot
library(corrplot)
library(gplots)

corrplot.mixed(corr = cor(uscrime[2:10], use = "complete.obs"), 
               upper = "ellipse", tl.pos = "lt")



# from skewed distribution to symmetric distribution
# uscrime$land.area <- log(uscrime$land.area)
# uscrime$popu.1985 <- log(uscrime$popu.1985)
# uscrime$robbery <- log(uscrime$robbery)



# standardization
uscrime[2:10] <- scale(uscrime[2:10], scale = TRUE, center = TRUE)

# spectral decomposition
eigenvalues <- eigen((nrow(uscrime) - 1) * cov(uscrime[2:10]) / nrow(uscrime))$values
eigenvectors <- eigen((nrow(uscrime) - 1) * cov(uscrime[2:10]) / nrow(uscrime))$vectors
PC <- as.matrix(uscrime[2:10]) %*% eigenvectors



# scree plot
par(mfrow = c(1, 3))
plot(eigenvalues,
     main = "Scree Plot",
     ylab = "eigenvalues",
     xlab = "",
     type = "l",
     ylim = c(0, 5),
     lwd = 2)

barplot(eigenvalues / sum(eigenvalues),
        main = " Proportion of Variance",
        ylim = c(0, 0.5),
        names.arg = 1:9,
        lwd = 2,
        xlab = "", ylab = "variance proportion")

plot(1:9, cumsum(eigenvalues / sum(eigenvalues)),
     main = "Cumulative Proportion of Variance",
     ylim = c(0, 1),
     type = "l", lwd = 2,
     xlab = "", ylab = "cumulative variance proportion")

sum(eigenvalues[1:2] / sum(eigenvalues))




# PC scatter plot
colors <- c("deepskyblue3", "orange", "darkorchid2", "limegreen")
pc12_reg <- cbind(as.data.frame(PC[, 1:3]), uscrime[, 11])

par(mfrow = c(1, 3))
plot(pc12_reg[, 1], pc12_reg[, 2], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "First Principal Component",
     ylab = "Second Principal Component",
     pch = 16, cex = 1.8)
# text(pc12_reg[, 1:2], labels = uscrime$X, cex = 0.9,
#     col = colors[as.factor(pc12_reg[, 4])])
legend("topright", legend = levels(as.factor(pc12_reg[, 4])),
       col = colors, pch = 16)
text(x = pc12_reg[50, 1], y = 2.84, "AL")

plot(pc12_reg[, 2], pc12_reg[, 3], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "Second Principal Component",
     ylab = "Third Principal Component",
     pch = 16, cex = 1.8)
# text(pc12_reg[, 2:3], labels = uscrime$X, cex = 0.9,
#      col = colors[as.factor(pc12_reg[, 4])])
legend("topright", legend = levels(as.factor(pc12_reg[, 4])),
       col = colors, pch = 16)
text(x = pc12_reg[50, 2], y = 1, "AL")
text(x = pc12_reg[49, 2] + 0.1, y = 5, "HI")

plot(pc12_reg[, 1], pc12_reg[, 3], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "First Principal Component",
     ylab = "Third Principal Component",
     pch = 16, cex = 1.8)
# text(pc12_reg[, c(1, 3)], labels = uscrime$X, cex = 0.9,
#      col = colors[as.factor(pc12_reg[, 4])])
legend("topright", legend = levels(as.factor(pc12_reg[, 4])),
       col = colors, pch = 16)
text(x = pc12_reg[49, 1], y = 5, "HI")



# loading matrix
x1 <- as.matrix(uscrime[2:10] - matrix(mean(as.matrix(uscrime[2:10])), nrow(uscrime[2:10]), ncol(uscrime[2:10]), byrow = T))
r1 <- x1 %*% eigenvectors
loading_matrix <- cor(r1[, 1:3], uscrime[2:10])


# correlations between variables and PCs
r <- cor(cbind(r1, uscrime[2:10]))
r12 <- r[(ncol(uscrime[2:10]) + 1):nrow(r), 1:2]
r13 <- cbind(r[(ncol(uscrime[2:10]) + 1):nrow(r), 1], r[(ncol(uscrime[2:10]) + 1):nrow(r), 3])
r32 <- cbind(r[(ncol(uscrime[2:10]) + 1):nrow(r), 3], r[(ncol(uscrime[2:10]) + 1):nrow(r), 2])


# loading plot
par(mfrow = c(1, 3))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC1", ylab = "PC2", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r12, colnames(uscrime[2:10]))

ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC3", ylab = "PC2", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r32, colnames(uscrime[2:10]))

ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC1", ylab = "PC3", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r13, colnames(uscrime[2:10]))

################################################################################

# drop 'land.area' column
uscrime <- uscrime[, -2]
head(uscrime)


# standardization
uscrime[2:9] <- scale(uscrime[2:9])


# spectral decomposition
eigenvalues <- eigen((nrow(uscrime) - 1) * cov(uscrime[2:9]) / nrow(uscrime))$values
eigenvectors <- eigen((nrow(uscrime) - 1) * cov(uscrime[2:9]) / nrow(uscrime))$vectors
PC <- as.matrix(uscrime[2:9]) %*% eigenvectors



# scree plot
par(mfrow = c(1, 3))
plot(eigenvalues,
     main = "Scree Plot",
     ylab = "eigenvalues",
     xlab = "",
     type = "l",
     ylim = c(0, 5),
     lwd = 2)

barplot(eigenvalues / sum(eigenvalues),
        main = " Proportion of Variance",
        ylim = c(0, 0.6),
        names.arg = 1:8,
        lwd = 2,
        xlab = "", ylab = "variance proportion")

plot(1:8, cumsum(eigenvalues / sum(eigenvalues)),
     main = "Cumulative Proportion of Variance",
     ylim = c(0, 1),
     type = "l", lwd = 2,
     xlab = "", ylab = "cumulative variance proportion")

sum(eigenvalues[1:2] / sum(eigenvalues))




# PC scatter plot
colors <- c("deepskyblue3", "orange", "darkorchid2", "limegreen")
pc12_reg <- cbind(as.data.frame(PC[, 1:3]), uscrime[, 10])

par(mfrow = c(1, 3))
plot(pc12_reg[, 1], pc12_reg[, 2], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "First Principal Component",
     ylab = "Second Principal Component",
     pch = 16, cex = 1.8)
# text(pc12_reg[, 1:2], labels = uscrime$X, cex = 0.9,
#     col = colors[as.factor(pc12_reg[, 4])])
legend("topright", legend = levels(as.factor(pc12_reg[, 4])),
       col = colors, pch = 16)

plot(pc12_reg[, 2], pc12_reg[, 3], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "Second Principal Component",
     ylab = "Third Principal Component",
     pch = 16, cex = 1.8)
# text(pc12_reg[, 2:3], labels = uscrime$X, cex = 0.9,
#      col = colors[as.factor(pc12_reg[, 4])])
legend("topright", legend = levels(as.factor(pc12_reg[, 4])),
       col = colors, pch = 16)

plot(pc12_reg[, 1], pc12_reg[, 3], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "First Principal Component",
     ylab = "Third Principal Component",
     pch = 16, cex = 1.8)
# text(pc12_reg[, c(1, 3)], labels = uscrime$X, cex = 0.9,
#      col = colors[as.factor(pc12_reg[, 4])])
legend("topright", legend = levels(as.factor(pc12_reg[, 4])),
       col = colors, pch = 16)




# loading matrix
x1 <- as.matrix(uscrime[2:9] - matrix(mean(as.matrix(uscrime[2:9])), nrow(uscrime[2:9]), ncol(uscrime[2:9]), byrow = T))
r1 <- x1 %*% eigenvectors
loading_matrix <- cor(r1[, 1:3], uscrime[2:9])


# correlations between variables and PCs
r <- cor(cbind(r1, uscrime[2:9]))
r12 <- r[(ncol(uscrime[2:9]) + 1):nrow(r), 1:2]
r13 <- cbind(r[(ncol(uscrime[2:9]) + 1):nrow(r), 1], r[(ncol(uscrime[2:9]) + 1):nrow(r), 3])
r32 <- cbind(r[(ncol(uscrime[2:9]) + 1):nrow(r), 3], r[(ncol(uscrime[2:9]) + 1):nrow(r), 2])


# loading plot
par(mfrow = c(1, 3))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC1", ylab = "PC2", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r12, colnames(uscrime[2:9]))

ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC3", ylab = "PC2", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r32, colnames(uscrime[2:9]))

ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC1", ylab = "PC3", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r13, colnames(uscrime[2:9]))



# load data
ushealth <- read.table("ushealth.txt", header = TRUE)
head(ushealth)

# correlation plot
library(corrplot)
library(gplots)

corrplot.mixed(corr = cor(ushealth[2:12], use = "complete.obs"), 
               upper = "ellipse", tl.pos = "lt")


# standardization
ushealth[2:12] <- scale(ushealth[2:12], scale = TRUE, center = TRUE)


# spectral decomposition
eigenvalues <- eigen((nrow(ushealth) - 1) * cov(ushealth[2:12]) / nrow(ushealth))$values
eigenvectors <- eigen((nrow(ushealth) - 1) * cov(ushealth[2:12]) / nrow(ushealth))$vectors
PC <- as.matrix(ushealth[2:12]) %*% eigenvectors
head(PC)


# scree plot
par(mfrow = c(1, 3))
plot(eigenvalues,
     main = "Scree Plot",
     ylab = "eigenvalues",
     xlab = "",
     type = "l",
     ylim = c(0, 5),
     lwd = 2)

barplot(eigenvalues / sum(eigenvalues),
        main = " Proportion of Variance",
        ylim = c(0, 0.5),
        names.arg = 1:11,
        lwd = 2,
        xlab = "", ylab = "variance proportion")

plot(1:11, cumsum(eigenvalues / sum(eigenvalues)),
     main = "Cumulative Proportion of Variance",
     ylim = c(0, 1),
     type = "l", lwd = 2,
     xlab = "", ylab = "cumulative variance proportion")


sum(eigenvalues[1:3] / sum(eigenvalues))


# PC scatter plot
colors <- c("purple3", "red", "gold3", "mediumblue")
pc12_reg <- cbind(as.data.frame(PC[, 1:3]), ushealth[, 13])

par(mfrow = c(1, 3))
plot(pc12_reg[, 1], pc12_reg[, 2], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "First Principal Component",
     ylab = "Second Principal Component",
     pch = NA, cex = 1.8)
text(pc12_reg[, 1:2], labels = ushealth$state, cex = 0.9,
     col = colors[as.factor(pc12_reg[, 4])])
legend("topleft", legend = c("Northeast", "Midwest", "South", "West"),
       col = colors, pch = 16)


plot(pc12_reg[, 2], pc12_reg[, 3], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "Second Principal Component",
     ylab = "Third Principal Component",
     pch = NA, cex = 1.8)
text(pc12_reg[, 2:3], labels = ushealth$state, cex = 0.9,
     col = colors[as.factor(pc12_reg[, 4])])
legend("topleft", legend = c("Northeast", "Midwest", "South", "West"),
       col = colors, pch = 16)


plot(pc12_reg[, 1], pc12_reg[, 3], col = colors[as.factor(pc12_reg[, 4])],
     xlab = "First Principal Component",
     ylab = "Third Principal Component",
     pch = NA, cex = 1.8)
text(pc12_reg[, c(1, 3)], labels = ushealth$state, cex = 0.9,
     col = colors[as.factor(pc12_reg[, 4])])
legend("topleft", legend = c("Northeast", "Midwest", "South", "West"),
       col = colors, pch = 16)


# loading matrix
x1 <- as.matrix(ushealth[2:12] - matrix(mean(as.matrix(ushealth[2:12])), nrow(ushealth[2:12]), ncol(ushealth[2:12]), byrow = T))
r1 <- x1 %*% eigenvectors
loading_matrix <- cor(r1[, 1:3], ushealth[2:12])


# correlations between variables and PCs
r <- cor(cbind(r1, ushealth[2:12]))
r12 <- r[(ncol(ushealth[2:12]) + 1):nrow(r), 1:2]
r13 <- cbind(r[(ncol(ushealth[2:12]) + 1):nrow(r), 1], r[(ncol(ushealth[2:12]) + 1):nrow(r), 3])
r32 <- cbind(r[(ncol(ushealth[2:12]) + 1):nrow(r), 3], r[(ncol(ushealth[2:12]) + 1):nrow(r), 2])


# loading plot
par(mfrow = c(1, 3))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC1", ylab = "PC2", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r12, colnames(ushealth[2:12]))

ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC3", ylab = "PC2", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r32, colnames(ushealth[2:12]))

ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue",
     xlab = "PC1", ylab = "PC3", 
     main = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0, col = "darkgrey", lty = 3, lwd = 2)
text(r13, colnames(ushealth[2:12]))
