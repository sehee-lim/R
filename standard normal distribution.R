# base R
x <- seq(-3, 3, length = 1000)
y <- dnorm(x, mean = 0, sd = 1)   # standard normal distribution

plot(x, y, type = "l",
     main = "Standard Normal Distribution",   # title
     lwd = 2,   # line width
     col = "red",   # line color
     xlab = "x", ylab = "f(x)")   # add labels


# ggplot2
library(ggplot2)

ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm,   # standard normal distribution
                size = 1.5,   # line width
                col = "red") +   # line color
  ggtitle("Normal Distribution") +   # title
  theme(plot.title = element_text(hjust = 0.5)) +   # title in the middle
  labs(x = "x", y = "f(x)")


# wikipedia
x <- seq(-4, 4, length = 1000)
y <- dnorm(x, mean = 0, sd = 1)   # standard normal distribution

plot(x, y, type = "l",
     xaxt = "n",   # remove x-axis
     lwd = 3,   # line width
     col = "white",   # line color
     xlab = "", ylab = "",   # add labels
     bty = "n")   # no box

# add x-axis
axis(1, at = -4:4, labels = c("",
                              expression(paste("-3", sigma)),
                              expression(paste("-2", sigma)),
                              expression(paste("-1", sigma)),
                              0,
                              expression(paste("1", sigma)),
                              expression(paste("2", sigma)),
                              expression(paste("3", sigma)),
                              ""))

# divide into sections
polygon(c(seq(-4, -3, length = 30), -3, -4),
        c(dnorm(seq(-4, -3, length = 30)), 0, 0),
        col = "#bdc9e1", border = NA)
polygon(c(seq(-3, -2, length = 30), -2, -3),
        c(dnorm(seq(-3, -2, length = 30)), 0, 0),
        col = "#74a9cf", border = NA)
polygon(c(seq(-2, -1, length = 30), -1, -2),
        c(dnorm(seq(-2, -1, length = 30)), 0, 0),
        col = "#2b8cbe", border = NA)
polygon(c(seq(-1, 0, length = 30), 0, -1),
        c(dnorm(seq(-1, 0, length = 30)), 0, 0),
        col = "#045a8d", border = NA)
polygon(c(seq(0, 1, length = 30), 1, 0),
        c(dnorm(seq(0, 1, length = 30)), 0, 0),
        col = "#045a8d", border = NA)
polygon(c(seq(1, 2, length = 30), 2, 1),
        c(dnorm(seq(1, 2, length = 30)), 0, 0),
        col = "#2b8cbe", border = NA)
polygon(c(seq(2, 3, length = 30), 3, 2),
        c(dnorm(seq(2, 3, length = 30)), 0, 0),
        col = "#74a9cf", border = NA)
polygon(c(seq(3, 4, length = 30), 4, 3),
        c(dnorm(seq(3, 4, length = 30)), 0, 0),
        col = "#bdc9e1", border = NA)

# add lines
lines(c(-3, -3), c(0, dnorm(-3)), col = "white", lwd = 2)
lines(c(-2, -2), c(0, dnorm(-2)), col = "white", lwd = 2)
lines(c(-1, -1), c(0, dnorm(-1)), col = "white", lwd = 2)
lines(c(0, 0), c(0, dnorm(0)), col = "white", lwd = 2)
lines(c(1, 1), c(0, dnorm(1)), col = "white", lwd = 2)
lines(c(2, 2), c(0, dnorm(2)), col = "white", lwd = 2)
lines(c(3, 3), c(0, dnorm(3)), col = "white", lwd = 2)
lines(x, y, lwd = 3.5, col = "black")
lines(rep(-3.5, 20), seq(0.01, 0.03, length = 20), lwd = 2, col = "black")
lines(rep(-2.5, 20), seq(0.05, 0.07, length = 20), lwd = 2, col = "black")
lines(rep(3.5, 20), seq(0.01, 0.03, length = 20), lwd = 2, col = "black")
lines(rep(2.5, 20), seq(0.05, 0.07, length = 20), lwd = 2, col = "black")

# add texts
text(-3.5, 0.05, "0.1%", col = "black")
text(-2.5, 0.09, "2.1%", col = "black")
text(-1.5, 0.05, "13.6%", col = "white")
text(-0.5, 0.18, "34.1%", col = "white")
text(0.5, 0.18, "34.1%", col = "white")
text(1.5, 0.05, "13.6%", col = "white")
text(2.5, 0.09, "2.1%", col = "black")
text(3.5, 0.05, "0.1%", col = "black")

