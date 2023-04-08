rm(list = ls(all = TRUE))
graphics.off()

# load data
car <- read.csv("Car_data.csv")
rownames(car) <- car$Col1   # set an index for a data frame
car <- car[, -1]
head(car)

# to numeric
str(car)
car$Powerful <- as.integer(car$Powerful)
car$Rugged <- as.integer(car$Rugged)
car$Fun <- as.integer(car$Fun)
car$Safe <- as.integer(car$Safe)
car$Performance <- as.integer(car$Performance)
car$Family <- as.integer(car$Family)
car$Versatile <- as.integer(car$Versatile)
car$Sporty <- as.integer(car$Sporty)
car$Status <- as.integer(car$Status)
car$Practical <- as.integer(car$Practical)

# remove NA
sum(is.na(car))
car <- na.omit(car)


###################
# Factor Analysis #
###################


# standardization
car_attributes <- car[, -1]
car_attributes <- scale(car_attributes)
car_corr <- cor(car_attributes)

# scree plot
eigenvalues <- eigen(car_corr)$values
plot(eigenvalues, type = "b",
     xlab = "", ylab = "Eigenvalues",
     main = "Scree Plot")

# Factor Analysis (Maximum Likelihood Method)
mlm <- factanal(car_attributes, factors = 3, rotation = "none", covmat = car_corr)
loadings <- mlm$loadings[, 1:3]   # loading matrix
communality <- diag(loadings %*% t(loadings))   # communality
FA <- cbind(loadings, communality)


# rotation
loadings_rotation <- varimax(loadings)
loadings_rotation <- loadings_rotation$loadings[, 1:3]
communality_rotation <- diag(loadings_rotation %*% t(loadings_rotation))
# var <- diag(uscrime_corr) - communality_rotation
FA_rotation <- cbind(loadings_rotation, communality_rotation)
FA_rotation <- round(FA_rotation, 3)
colnames(FA_rotation) <- c("Factor1", "Factor2", "Factor3", "Commmunality")
FA_rotation


par(mfrow = c(1, 3))
# Factor 1 vs Factor 2
plot(loadings_rotation[, 1], loadings_rotation[, 2], type = "n",
     xlab = "Factor 1", ylab = "Factor 2",
     main = "Factor 1 vs Factor 2",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 1], loadings_rotation[, 2], colnames(car_attributes), cex = 1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 2 vs Factor 3
plot(loadings_rotation[, 2], loadings_rotation[, 3], type = "n",
     xlab = "Factor 2", ylab = "Factor 3",
     main = "Factor 2 vs Factor 3",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 2], loadings_rotation[, 3], colnames(car_attributes), cex = 1)
abline(h = 0, v = 0, lty = 3, lwd = 2)


# Factor 1 vs Factor 3
plot(loadings_rotation[, 1], loadings_rotation[, 3], type = "n",
     xlab = "Factor 1", ylab = "Factor 3",
     main = "Factor 1 vs Factor 3",
     xlim = c(-1, 1), ylim = c(-1, 1))
text(loadings_rotation[, 1], loadings_rotation[, 3], colnames(car_attributes), cex = 1)
abline(h = 0, v = 0, lty = 3, lwd = 2)



##############
# Clustering #
##############

graphics.off()


# PCA for visualization
mean <- as.vector(colMeans(car_attributes))
m <- matrix(mean, nrow(car_attributes), NROW(mean), byrow = TRUE)
x <- car_attributes - m
eigenvalues <- eigen(cov(x))$values
eigenvectors <- eigen(cov(x))$vectors
xm <- as.matrix(x)
y <- xm %*% eigenvectors
ym <- y[, 1:2]


# number of clusters
wss <- sapply(1:15,
              function(k){kmeans(x, k, nstart = 25)$tot.withinss})

plot(1:15, wss, type = "b",
     frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Sum of Squares",
     ylim = c(1000, 5000))



distance <- dist(car_attributes, method = "euclidean", p = 2) ^ 2   # euclidean distance matrix
ward <- hclust(distance, method = "ward.D")   # ward algorithm


# sampling
dim(car)
set.seed(100)
car_sample <- car[sample(1:nrow(car), 40, replace = FALSE), ][, -1]

# Ward's method
distance <- dist(car_sample, method = "euclidean", p = 2) ^ 2   # euclidean distance matrix
ward <- hclust(distance, method = "ward.D")


# Dendrogram
plot(ward, hang = -0.1, frame.plot = TRUE, ann = FALSE)
title(main = "Dendrogram",
      ylab = "Squared Euclidean Distance", 
      xlab = "")
# axis(1, at = 1:30, labels = car[rownames(car_sample), ]$CarID)
abline(h = 143, lty = 3, lwd = 2)



# PCA for visualization
# mean <- as.vector(colMeans(car_sample))
# m <- matrix(mean, nrow(car_sample), NROW(mean), byrow = TRUE)
# x <- car_sample - m
# eigenvalues <- eigen(cov(x))$values
# eigenvectors <- eigen(cov(x))$vectors
# xm <- as.matrix(x)
# y <- xm %*% eigenvectors
# ym <- y[, 1:2]

# PC plot
# plot(ym, type = "n", xlab = "PC1", ylab = "PC2")
# text(ym[, 1], ym[, 2], rownames(car_sample), cex = 0.8)



# scatter plot
# merg <- matrix(c(ym, as.matrix(groups)), nrow = 30, ncol = 3)
# merg1 <- merg[groups == 1, 1:2]
# merg2 <- merg[groups == 2, 1:2]
# plot(ym, type = "n", xlab = "PC1", ylab = "PC2")
# text(ym[, 1], ym[, 2], rownames(car_sample), cex = 0.8)
# dataEllipse(x = merg1[, 1], y = merg1[, 2], center.pch = 0, col = "red", plot.points = F, 
#             add = T, levels = 0.95)
# dataEllipse(x = merg2[, 1], y = merg2[, 2], center.pch = 0, col = "blue", plot.points = F, 
#             add = T, levels = 0.95)
# dataEllipse(x = merg3[, 1], y = merg3[, 2], center.pch = 0, col = "red", plot.points = F, 
#             add = T, levels = 0.95)
# dataEllipse(x = merg4[, 1], y = merg4[, 2], center.pch = 0, col = "blue", plot.points = F, 
#             add = T, levels = 0.95) 


# Ward's method
distance <- dist(car_attributes, method = "euclidean", p = 2) ^ 2   # euclidean distance matrix
ward <- hclust(distance, method = "ward.D")



colors <- c("purple", "red", "chartreuse3", "black")
groups <- cutree(ward, k = 4)
merg <- data.frame(ym)
merg[, 3] <- as.factor(groups)
plot(merg$X1, merg$X2,
     pch = NA,
     col = colors[merg$V3],
     main = "Scatter Plot",
     xlab = "PC1", ylab = "PC2")
text(merg$X1, merg$X2, car[rownames(car_attributes), ]$CarID,
     col = colors[merg$V3], cex = 0.9)
legend("topright",
       legend = c("Group 1", "Group 2", "Group 3", "Group 4"),
       col = colors, pch = 16)

table(car[merg$V3 == 1, ]$CarID)
table(car[merg$V3 == 2, ]$CarID)
table(car[merg$V3 == 3, ]$CarID)
table(car[merg$V3 == 4, ]$CarID)






# kmean clustering 시행

# PCA for visualization
# mean <- as.vector(colMeans(car_attributes))
# m <- matrix(mean, nrow(car_attributes), NROW(mean), byrow = TRUE)
# x <- car_attributes - m
# eigenvalues <- eigen(cov(x))$values
# eigenvectors <- eigen(cov(x))$vectors
# xm <- as.matrix(x)
# y <- xm %*% eigenvectors
# ym <- y[, 1:2]



k2 <- kmeans(x, centers = 3, nstart = 25)
merg <- data.frame(ym)
merg[, 3] <- as.factor(k2$cluster)


colors <- c("purple", "red", "chartreuse3")

plot(ym[, 1], ym[, 2], col = colors[merg$V3],
     pch = NA,
     xlab = "PC1", ylab = "PC2",
     main = "Clustering (k = 3)")
legend("topright",
       legend = c("Group 1", "Group 2", "Group 3"),
       pch = 19,
       col = colors)
text(merg$X1, merg$X2, labels = car[rownames(car_attributes), ]$CarID,
     col = colors[merg$V3],
     cex = 0.8)

table(car[merg$V3 == 1, ]$CarID)
table(car[merg$V3 == 2, ]$CarID)
table(car[merg$V3 == 3, ]$CarID)

par(mfrow = c(1, 3))
k2 <- kmeans(x, centers = 4, nstart = 25)
merg <- data.frame(ym)
merg[, 3] <- as.factor(k2$cluster)
plot(ym[, 1], ym[, 2], col = merg$V3,
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Clustering (k = 4)")
legend("topright",
       legend = c("Group 1", "Group 2", "Group 3", "Group 4"),
       pch = 19,
       col = factor(levels(merg$V3)))

k2 <- kmeans(x, centers = 5, nstart = 25)
merg <- data.frame(ym)
merg[, 3] <- as.factor(k2$cluster)
plot(ym[, 1], ym[, 2], col = merg$V3,
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Clustering (k = 5)")
legend("topright",
       legend = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
       pch = 19,
       col = factor(levels(merg$V3)))

# merg <- matrix(c(ym, as.matrix(k2$cluster)), nrow = nrow(car_attributes), ncol = 3)
# merg1 <- merg[k2$cluster == 1, 1:2]
# merg2 <- merg[k2$cluster == 2, 1:2]
# merg3 <- merg[k2$cluster == 3, 1:2]
# 
# plot(ym, xlab = "PC1", ylab = "PC2")
# text(ym[, 1], ym[, 2], rownames(car_attributes), cex = 0.9)
# dataEllipse(x = merg1[, 1], y = merg1[, 2], center.pch = 0, col = "red", plot.points = F, 
#             add = T, levels = 0.95)
# dataEllipse(x = merg2[, 1], y = merg2[, 2], center.pch = 0, col = "blue", plot.points = F, 
#             add = T, levels = 0.9)
# dataEllipse(x = merg3[, 1], y = merg3[, 2], center.pch = 0, col = "green", plot.points = F, 
#             add = T, levels = 0.9)



# k2 <- kmeans(x, centers = 4, nstart = 25)
# merg <- matrix(c(ym, as.matrix(k2$cluster)), nrow = 30, ncol = 3)
# merg1 <- merg[k2$cluster == 1, 1:2]
# merg2 <- merg[k2$cluster == 2, 1:2]
# merg4 <- merg[k2$cluster == 3, 1:2]
# merg3 <- merg[k2$cluster == 4, 1:2]
# 
# plot(ym, type = "n", xlab = "PC1", ylab = "PC2")
# text(ym[, 1], ym[, 2], rownames(car_sample), cex = 0.9)
# dataEllipse(x = merg1[, 1], y = merg1[, 2], center.pch = 0, col = "red", plot.points = F, 
#             add = T, levels = 0.95)
# dataEllipse(x = merg2[, 1], y = merg2[, 2], center.pch = 0, col = "blue", plot.points = F, 
#             add = T, levels = 0.9)
# dataEllipse(x = merg3[, 1], y = merg3[, 2], center.pch = 0, col = "green", plot.points = F, 
#             add = T, levels = 0.9)
# dataEllipse(x = merg4[, 1], y = merg4[, 2], center.pch = 0, col = "purple", plot.points = F, 
#             add = T, levels = 0.9)



# k2 <- kmeans(x, centers = 5, nstart = 25)
# merg <- matrix(c(ym, as.matrix(k2$cluster)), nrow = 30, ncol = 3)
# merg1 <- merg[k2$cluster == 1, 1:2]
# merg2 <- merg[k2$cluster == 2, 1:2]
# merg3 <- merg[k2$cluster == 3, 1:2]
# merg4 <- merg[k2$cluster == 4, 1:2]
# merg5 <- merg[k2$cluster == 5, 1:2]
# 
# plot(ym, type = "n", xlab = "PC1", ylab = "PC2")
# text(ym[, 1], ym[, 2], rownames(car_sample), cex = 0.9)
# dataEllipse(x = merg1[, 1], y = merg1[, 2], center.pch = 0, col = "red", plot.points = F, 
#             add = T, levels = 0.95)
# dataEllipse(x = merg2[, 1], y = merg2[, 2], center.pch = 0, col = "blue", plot.points = F, 
#             add = T, levels = 0.9)
# dataEllipse(x = merg3[, 1], y = merg3[, 2], center.pch = 0, col = "green", plot.points = F, 
#             add = T, levels = 0.9)
# dataEllipse(x = merg4[, 1], y = merg4[, 2], center.pch = 0, col = "purple", plot.points = F, 
#             add = T, levels = 0.9)
# dataEllipse(x = merg5[, 1], y = merg5[, 2], center.pch = 0, col = "orange", plot.points = F, 
#             add = T, levels = 0.9)



