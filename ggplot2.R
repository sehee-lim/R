# package
library(ggplot2)

# load data
data(msleep)
msleep <- na.omit(msleep)
head(msleep)



# scatter plot

# basic elements
ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point()   # scatter plot

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  labs(title = "Scatter Plot") +   # title
  theme(plot.title = element_text(hjust = 0.5,   # center plot title
                                  size = 15,   # title size
                                  face = 'bold'))   # bold

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  labs(title = "Scatter Plot") +
  theme(plot.title = element_text(angle = 90))   # rotate text


ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  labs(x = "total sleep", y = "rem sleep")   # add text labels

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point(size = 2)   # point size

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point(alpha = 0.5)   # opacity

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point(alpha = 0.5) +
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 6))   # set scale limits

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_line()   # connect points with line


# add text
ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_text(aes(label = name),
            check_overlap = TRUE)   # avoid overlapping

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_label(aes(label = name))


ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem, shape = vore)) +
  geom_point() +
  scale_shape_manual(values = c(1, 3, 5, 6))   # set shape by group

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem, col = vore)) +   # set color by group
  geom_point()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem, col = vore)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green", "black"))   # each color

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem, col = vore)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green", "black"),
                     name = "type",   # legend title
                     labels = c("a", "b", "c", "d"))

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem, col = vore)) +
  geom_point() +
  scale_color_brewer(palette = "Reds")   # palette

RColorBrewer::display.brewer.all()   # display palette

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem, col = vore)) +
  geom_point() +
  scale_color_brewer(palette = "Greens") +
  theme(legend.position = "bottom")   # change the legend position: top, bottom, left, right



ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  facet_grid(. ~ vore)   # faceting groups on the rows dimension

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  facet_grid(vore ~ .)   # faceting groups on the columns dimension

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  facet_grid(. ~ vore + conservation)   # faceting two variables

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  facet_grid(vore ~ conservation)   # faceting groups on the rows and columns dimension

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  facet_grid(vore ~ conservation,
             scales = "free_x")   # different scales on x-axis

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  facet_grid(vore ~ conservation,
             scales = "free_y")   # different scales on y-axis

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  facet_grid(vore ~ conservation,
             scales = "free")   # different scales on x-axis and y-axis





# draw a trend line
ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_smooth()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_smooth(method = lm)   # linear

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_smooth(method = lm,
              size = 2)   # line width

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)   # remove the confidence interval

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_smooth(method = lm, col = "red", fill = "orange")   # change colors



# transform x-axis
ggplot(data = msleep, aes(x = bodywt, y = brainwt)) +
  geom_point() +
  scale_x_log10()   # log

ggplot(data = msleep, aes(x = bodywt, y = brainwt)) +
  geom_point() +
  scale_x_sqrt()   # sqrt



# histogram
ggplot(data = msleep, aes(x = sleep_total)) +
  geom_histogram()

ggplot(data = msleep, aes(x = sleep_total)) +
  geom_histogram(col = "purple", fill = "white")

ggplot(data = msleep, aes(x = sleep_total)) +
  geom_histogram(aes(y=..density..))   # frequency to density

ggplot(data = msleep, aes(x = sleep_total)) +
  geom_histogram(bins = 50)   # number of bins

ggplot(data = msleep, aes(x = sleep_total)) +
  geom_histogram(breaks = c(0, 10, 15:20))   # start and end points of each bin


# bar chart
ggplot(data = msleep, aes(x = vore)) +
  geom_bar(col = "blue", fill = "white")

ggplot(data = msleep, aes(x = vore)) +
  geom_bar(col = "blue", fill = "white") +
  coord_flip()   # vertical x axis

library(forcats)
ggplot(data = msleep, aes(x = fct_infreq(vore))) +   # order by value
  geom_bar(col = "blue", fill = "white")


ggplot(data = msleep, aes(x = vore, fill = vore)) +
  geom_bar() +
  scale_fill_manual(values = c("red", "orange", "pink", "purple"))   # each color

ggplot(data = msleep, aes(x = vore, fill = vore)) +
  geom_bar() +
  scale_fill_manual(values = "orange",
                    limits = "herbi")   # change color of only one bar

ggplot(data = msleep, aes(x = vore, fill = vore)) +
  geom_bar() +
  scale_fill_manual(values = "orange",
                    limits = "herbi",
                    name = "animal type",   # legend title
                    labels = "herbivore")   # legend labels


vore_frequency <- as.data.frame(table(msleep$vore))
colnames(vore_frequency) <- c("vore", "frequency")
ggplot(data = vore_frequency, aes(x = vore, y = frequency)) + 
  geom_col()   # bar chart (two variables)



# add colorbar
data("mpg")
ggplot(data = mpg, aes(x = displ, y = cty, col = cyl)) +
  geom_point() +
  labs(x = "number of cylinders") +
  theme(legend.position = "right") +
  guides(color = "colorbar")

ggplot(data = mpg, aes(x = displ, y = cty, col = cyl)) +
  geom_point() +
  labs(x = "number of cylinders") +
  theme(legend.position = "right") +
  guides(color = "legend")



# density plot
ggplot(data = msleep, aes(x = sleep_total)) +
  geom_density(kernel = "gaussian") +
  geom_rug(sides = "bl")   # add rug plot

# density plot (two-dimensional)
ggplot(data = msleep, aes(x = sleep_total, y = sleep_cycle)) +
  geom_density2d()

# violin plot
ggplot(data = msleep, aes(x = vore, y = sleep_total)) +
  geom_violin()




# box plot
ggplot(data = msleep, aes(x = vore, y = sleep_rem)) +
  geom_boxplot()

ggplot(data = msleep, aes(x = vore, y = sleep_rem)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("carnivore", "herbivore", "insectivore", "omnivore"))   # change x axis breaks




# map plot
df <- data.frame(murder = USArrests$Murder,
                 state = tolower(rownames(USArrests)))   # characters to lower case
head(df)

library(maps)
map <- map_data("state")

ggplot(data = df, aes(fill = murder)) +
  geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat)



# add line
ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_hline(yintercept = 3)   # horizontal line

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_hline(yintercept = 3,
             linetype = "dashed")   # line type: solid, dashed, dotted, dotdash, longdash, twodash

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_vline(xintercept = 10)   # vertical line

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_abline(intercept = 1, slope = 1/10)   # straight line

library(latex2exp)
ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_abline(intercept = 1, slope = 1/10) +
  labs(x = TeX("\\alpha = 1, \\beta = 0.1"))   # latex characters

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_segment(x = 5, xend = 10, y = 2, yend = 3)   # from (5, 2) to (10, 3)

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_segment(x = 5, xend = 10, y = 2, yend = 3,   # from (5, 2) to (10, 3)
               arrow = arrow(length = unit(0.1, "inches")))   # arrow


# draw a rectangle
ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2, ymax = 3, fill = "yellow")   

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2, ymax = 3, fill = "yellow") +
  geom_point()



# theme
ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_bw()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_dark()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_gray()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_classic()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_light()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_linedraw()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_minimal()

ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_void()


# save
graph <- ggplot(data = msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  theme_bw()

ggsave(plot = graph, filename = "graph.jpg")
