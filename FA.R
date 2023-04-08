rm(list=ls())
graphics.off()


# load data
uscrime <- read.csv("uscrime.csv")

# set an index for a data frame
rownames(uscrime) <- uscrime$X
uscrime <- uscrime[, -1]

# use only (X3-X9) variables
uscrime <- uscrime[, 3:9]
head(uscrime)

# correlation plot
library(corrplot)
library(gplots)

corrplot.mixed(corr = cor(uscrime, use = "complete.obs"), 
               upper = "ellipse", tl.pos = "lt")


# standardization
uscrime <- scale(uscrime)
uscrime_corr <- cor(uscrime)


# scree plot
eigenvalues <- eigen(uscrime_corr)$values
plot(eigenvalues, type = "b",
     xlab = "", ylab = "Eigenvalues",
     main = "Scree Plot")
# grid()
# abline(h = 1, lty = 3, lwd = 2)


# Factor Analysis (Maximum Likelihood Method)
mlm <- factanal(uscrime, factors = 3, rotation = "none", covmat = uscrime_corr)
loadings <- mlm$loadings[, 1:3]   # loading matrix
communality <- diag(loadings %*% t(loadings))   # communality
# var <- diag(uscrime_corr) - communality
FA <- cbind(loadings, communality)


# Factor Loadings plot
barplot(abs(loadings), ylim = c(0, 5),
        main = "Cumulative Factor Loadings")

text(0.7, 0.3, "Murder: 0.701")
text(0.7, 1.1, "Rape: 0.806")
text(0.7, 1.8, "Robbery: 0.676")
text(0.7, 2.7, "Assault: 0.912")
text(0.7, 3.5, "Burglary: 0.779")
text(0.7, 4.1, "Larceny: 0.579")
text(0.7, 4.9, "Autotheft: 0.542")

text(1.9, 0.4, "Murder: 0.511")
# text(1.9, 0.6, "Rape: 0.113")
# text(1.9, 2, "Robbery: 0.182")
text(1.9, 1.2, "Assault: 0.340")
# text(1.9, 1.5, "Burglary: 0.553")
text(1.9, 2, "Larceny: 0.624")
# text(1.9, 2.65, "Autotheft: 0.485")

text(3.1, 0.4, "Robbery: 0.376")
text(3.1, 1.2, "Autotheft: 0.542")


par(mfrow = c(1, 3))

# Factor 1 vs Factor 2
plot(loadings[, 1], loadings[, 2], type = "n",
     xlim = c(-1, 1), ylim = c(-0.7, 0.7),
     main = "Factor 1 vs Factor 2",
     xlab = "Factor 1", ylab = "Factor 2")
text(loadings[, 1], loadings[, 2], colnames(uscrime), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 2 vs Factor 3
plot(loadings[, 2], loadings[, 3], type = "n",
     xlim = c(-1, 1), ylim = c(-0.7, 0.7),
     main = "Factor 2 vs Factor 3",
     xlab = "Factor 2", ylab = "Factor 3")
text(loadings[, 2], loadings[, 3], colnames(uscrime), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 1 vs Factor 3
plot(loadings[, 1], loadings[, 3], type = "n",
     xlim = c(-1, 1), ylim = c(-0.7, 0.7),
     main = "Factor 1 vs Factor 3",
     xlab = "Factor 1", ylab = "Factor 3")
text(loadings[, 1], loadings[, 3], colnames(uscrime), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# rotation
loadings_rotation <- varimax(loadings)
loadings_rotation <- loadings_rotation$loadings[, 1:3]
communality_rotation <- diag(loadings_rotation %*% t(loadings_rotation))
var <- diag(uscrime_corr) - communality_rotation
FA_rotation <- cbind(loadings_rotation, communality_rotation, var)
FA_rotation <- round(FA_rotation, 3)
colnames(FA_rotation) <- c("Factor1", "Factor2", "Factor3", "Communality", "Specific Variances")


# Factor 1 vs Factor 2
plot(loadings_rotation[, 1], loadings_rotation[, 2], type = "n",
     xlab = "Factor 1", ylab = "Factor 2",
     main = "Factor 1 vs Factor 2 (Rotation)",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 1], loadings_rotation[, 2], colnames(uscrime), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 2 vs Factor 3
plot(loadings_rotation[, 2], loadings_rotation[, 3], type = "n",
     xlab = "Factor 2", ylab = "Factor 3",
     main = "Factor 2 vs Factor 3 (Rotation)",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 2], loadings_rotation[, 3], colnames(uscrime), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 1 vs Factor 3
plot(loadings_rotation[, 1], loadings_rotation[, 3], type = "n",
     xlab = "Factor 1", ylab = "Factor 3",
     main = "Factor 1 vs Factor 3 (Rotation)",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 1], loadings_rotation[, 3], colnames(uscrime), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor Loadings plot
barplot(abs(loadings_rotation), ylim = c(0, 3.5),
        main = "Cumulative Factor Loadings")

# text(0.7, 0.3, "Murder: 0.701")
text(0.7, 0.3, "Rape: 0.527")
text(0.7, 0.65, "Robbery: 0.241")
text(0.7, 0.89, "Assault: 0.209")
text(0.7, 1.4, "Burglary: 0.672")
text(0.7, 2.3, "Larceny: 0.910")
text(0.7, 2.8, "Autotheft: 0.332")

text(1.9, 0.7, "Murder: 0.879")
text(1.9, 1.3, "Rape: 0.563")
text(1.9, 1.65, "Robbery: 0.372")
text(1.9, 2.5, "Assault: 0.903")
text(1.9, 2.85, "Burglary: 0.259")
# text(1.9, 2, "Larceny: 0.624")
# text(1.9, 2.65, "Autotheft: 0.485")

text(3.1, 0.24, "Rape: 0.305")
text(3.1, 0.8, "Robbery: 0.646")
text(3.1, 1.15, "Assault: 0.262")
text(3.1, 1.7, "Burglary: 0.571")
text(3.1, 2, "Larceny: 0.310")
text(3.1, 2.8, "Autotheft: 0.802")


###############################################################

# load data
ushealth <- read.table("ushealth.txt", header = TRUE)

# set an index for a data frame
rownames(ushealth) <- ushealth$state
ushealth <- ushealth[, -1]

ushealth <- ushealth[, 3:9]
head(ushealth)


# correlation plot
corrplot.mixed(corr = cor(ushealth, use = "complete.obs"), 
               upper = "ellipse", tl.pos = "lt")

# standardization
ushealth <- scale(ushealth)
ushealth_corr <- cor(ushealth)

# scree plot
eigenvalues <- eigen(ushealth_corr)$values
plot(eigenvalues, type = "b",
     xlab = "", ylab = "Eigenvalues",
     main = "Scree Plot")

# PCA
# eigenvalues <- eigen(ushealth_corr)$values[1:3]
# eigenvectors <- eigen(ushealth_corr)$vectors[, 1:3]
# a <- matrix(eigenvalues, nrow = nrow(ushealth_corr), ncol = 3, byrow = TRUE)
# loadings <- sqrt(a) * eigenvectors   # loading matrix
# loadings_rotation <- varimax(loadings)$loadings[, 1:3]
# communality_rotation <- diag(loadings_rotation %*% t(loadings_rotation))
# FA_rotation <- cbind(loadings_rotation, communality_rotation)
# colnames(FA_rotation) <- c("Factor1", "Factor2", "Factor3", "Communality")
# FA_rotation


# Factor Loadings plot
# barplot(abs(loadings), ylim = c(0, 5),
#         main = "Cumulative Factor Loadings",
#         names.arg= c("Factor1","Factor2", "Factor3"))
# 
# text(0.7, 0.5, "acc: 0.666")
# text(0.7, 1.4, "card: 0.921")
# text(0.7, 2.4, "canc: 0.951")
# text(0.7, 2.9, "pul: 0.558")
# text(0.7, 3.4, "pnue: 0.489")
# text(0.7, 4.1, "diab: 0.739")
# text(0.7, 4.5, "liv: 0.326")
# 
# text(1.9, 0.2, "acc: 0.310")
# text(1.9, 0.8, "pul: 0.417")
# text(1.9, 1.5, "pnue: 0.771")
# text(1.9, 2, "diab: 0.410")
# text(1.9, 2.4, "liv: 0.405")
# 
# text(3.1, 0.2, "acc: 0.332")
# text(3.1, 0.8, "pul: 0.515")
# text(3.1, 1.9, "pul: 0.780")







# Factor Analysis (Maximum Likelihood Method)
mlm <- factanal(ushealth, factors = 3, rotation = "none", covmat = ushealth_corr)
loadings <- mlm$loadings[, 1:3]   # loading matrix
communality <- diag(loadings %*% t(loadings))   # communality
# var <- diag(uscrime_corr) - communality
# FA <- cbind(loadings, communality)


# rotation
loadings_rotation <- varimax(loadings)
loadings_rotation <- loadings_rotation$loadings[, 1:3]
communality_rotation <- diag(loadings_rotation %*% t(loadings_rotation))
var <- diag(ushealth_corr) - communality_rotation
FA_rotation <- cbind(loadings_rotation, communality_rotation, var)
FA_rotation <- round(FA_rotation, 3)
colnames(FA_rotation) <- c("Factor1", "Factor2", "Factor3", "Communality", "Specific Variances")


# Factor Loadings plot
barplot(abs(loadings_rotation), ylim = c(0, 4),
        main = "Cumulative Factor Loadings")

text(0.7, 0.4, "acc: 0.568")
text(0.7, 1.2, "card: 0.798")
text(0.7, 2.1, "canc: 0.893")
text(0.7, 2.38, "pul: 0.241")
# text(0.7, 3.4, "pnue: 0.082")
text(0.7, 3.2, "diab: 0.795")
# text(0.7, 4.5, "liv: 0.125")

# text(1.9, 0.2, "acc: 0.310")
text(1.9, 0.27, "canc: 0.265")
text(1.9, 0.5, "pul: 0.244")
# text(1.9, 2, "diab: 0.410")
text(1.9, 1.6, "liv: 0.945")

text(3.1, 0.55, "card: 0.530")
text(3.1, 0.9, "canc: 0.351")
text(3.1, 1.3, "pul: 0.450")
text(3.1, 2.2, "pneu: 0.900")
# text(3.1, 1.9, "pul: 0.450")

par(mfrow = c(1, 3))

# Factor 1 vs Factor 2
plot(loadings_rotation[, 1], loadings_rotation[, 2], type = "n",
     xlab = "Factor 1", ylab = "Factor 2",
     main = "Factor 1 vs Factor 2 (Rotation)",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 1], loadings_rotation[, 2], colnames(ushealth), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 2 vs Factor 3
plot(loadings_rotation[, 2], loadings_rotation[, 3], type = "n",
     xlab = "Factor 2", ylab = "Factor 3",
     main = "Factor 2 vs Factor 3 (Rotation)",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 2], loadings_rotation[, 3], colnames(ushealth), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 1 vs Factor 3
plot(loadings_rotation[, 1], loadings_rotation[, 3], type = "n",
     xlab = "Factor 1", ylab = "Factor 3",
     main = "Factor 1 vs Factor 3 (Rotation)",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 1], loadings_rotation[, 3], colnames(ushealth), cex = 1.1)
abline(h = 0, v = 0, lty = 3, lwd = 2)



# Factor Scores
mlm = factanal(ushealth, factors = 3,
               scores = "regression")
# loadings <- mlm$loadings[, 1:3]   # loading matrix
# communality <- diag(loadings %*% t(loadings))   # communality
# var <- diag(uscrime_corr) - communality
# FA <- cbind(loadings, communality)


scores <- mlm$scores

ushealth <- read.table("ushealth.txt", header = TRUE)
colors <- c("purple3", "red", "gold3", "mediumblue")

par(mfrow = c(1, 3))
plot(scores[, 1], scores[, 2],
     main = "Factor Scores",
     xlab = "f1", ylab = "f2",
     type = "n")
text(scores[, 1:2], labels = ushealth$state, cex = 0.8,
     col = colors[as.factor(ushealth$r)])
legend("topleft", legend = c("Northeast", "Midwest", "South", "West"),
       col = colors, pch = 16)

plot(scores[, 2], scores[, 3],
     main = "Factor Scores",
     xlab = "f2", ylab = "f3",
     type = "n")
text(scores[, 2:3], labels = ushealth$state, cex = 0.8,
     col = colors[as.factor(ushealth$r)])
legend("topleft", legend = c("Northeast", "Midwest", "South", "West"),
       col = colors, pch = 16)

plot(scores[, 1], scores[, 3],
     main = "Factor Scores",
     xlab = "f1", ylab = "f3",
     type = "n")
text(scores[, c(1, 3)], labels = ushealth$state, cex = 0.8,
     col = colors[as.factor(ushealth$r)])
legend("topleft", legend = c("Northeast", "Midwest", "South", "West"),
       col = colors, pch = 16)
