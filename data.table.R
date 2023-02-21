# package
library(data.table)
library(magrittr)


# load data
data(mtcars)
head(mtcars)

DT <- as.data.table(mtcars,   # to data.table
                    keep.rownames = TRUE)   # data.table does not have row names
head(DT)


# slice
DT[3, ]   # 3rd row
DT[3]   # 3rd row
DT[c(1, 3)]   # 1st and 3rd rows



# filter
DT[cyl == 6 & mpg > 20, ]
DT[cyl == 6 & mpg > 20]



# select

# by position
DT[, 3]   # 3rd column
DT[, c(3, 4)]   # 3rd and 4th columns

DT[, -c(3, 4)]   # drop 3rd and 4th columns

DT[, 3][[1]]   # vector


# by column name
DT[, .(cyl)]   # 'cyl' column
DT[, .(cyl, hp)]   # 'cyl' and 'hp' columns
DT[, .SD, .SDcols = c(cyl, hp)]   # same
DT[, .(a = cyl, b = hp)]   # with column name

DT[, -c("cyl", "hp")]   # drop 'cyl' and 'hp' columns

DT[, cyl]   # vector


# by variable
col_selected <- c('cyl', 'hp')
DT[, ..col_selected]   # 'cyl' and 'hp' columns
DT[, .SD, .SDcols = col_selected]   # 'cyl' and 'hp' columns

DT[, !..col_selected]   # drop 'cyl' and 'hp' columns
DT[, -..col_selected]   # drop 'cyl' and 'hp' columns
DT[, .SD, .SDcols = -col_selected]

DT[, ..col_selected][[1]]   # vector 

DT[(gear == 4) & (mpg > 20), ..col_selected]   # with conditions




# mutate
DT[, `:=`(hp_qsec, hp * qsec)]   # add 'hp_qsec' column
DT[, hp_qsec := hp * qsec]   # add 'hp_qsec' column
DT   # updated

DT[, `:=`(c("hp", "qsec"), list(hp^2, qsec^2))]   # change 'hp' and 'qsec' columns

DT[, wt := NULL]   # drop 'wt' column



# arrange
DT[order(cyl, -mpg)]
DT[order(cyl, -mpg), ]



# summary
DT[, lapply(.SD, mean), .SDcols = c("mpg", "disp", "drat")]   # compute average for three columns



# group_by
DT[, mean(mpg), cyl]   # group by 'cyl'
DT[, .(mean_mpg = mean(mpg)), cyl]   # column name
DT[, mean(mpg), keyby = cyl]   # order by 'cyl'

DT[gear == 4, mean(mpg), cyl]   # with condition

DT[gear == 4, mean(mpg), .(ifelse(am == 1, "Automatic", "Manual"))]   # group by condition

# group by two variables
DT[, .(mean_mpg = mean(mpg), mean_hp = mean(hp)), .(cyl, am)]   # group by 'cyl' and 'am'
DT[, lapply(.SD, mean), .(cyl, am), .SDcols = c("mpg", "hp")]   # SD: Subset of Data

# group_by and arrange
DT[, .(mpg = mean(mpg), hp = mean(hp)), .(cyl, am)][order(mpg), ]   # order by 'mpg'
DT[, .(mpg = mean(mpg), hp = mean(hp)), .(cyl, am)] %>% 
  .[order(mpg), ]



# key
key(DT)   # key does not exist

setkey(DT, am)
haskey(DT)   # returns TRUE if data.table has a key
key(DT)   # returns key

tables()   # summarizing

setkey(DT, NULL)   # remove key
setkey(DT, am)

# select by key (faster)
DT[J(1)]
DT[am == 1]   # faster after setting key

# multiple keys
setkey(DT, am, cyl)
key(DT)

# select by key
DT[J(1), mult = "first"]   # select the first row
DT[J(1), mult = "last"]   # select the last row
DT[J(1), mult = "all"]   # select all rows (same as DT[J(1)])



# special symbols
DT[, lapply(.SD, mean), .(cyl, am),.SDcols = c("mpg", "hp")]   # SD: Subset of Data
DT[, .N, by = cyl]   # .N: length()
DT[, .I, by = cyl]   # .I: row location
DT[, .GRP, by = cyl]   # .GRP: numbering groups



# merge
dt1 <- as.data.table(data.frame("student" = c("A", "B", "C", "D", "E"),
                                "grade" = c(10, 4, 6, 8, 3)))
dt2 <- as.data.table(data.frame("student" = c("B", "D", "E", "F", "G"),
                                "height" = c(165, 156, 179, 182, 163)))

# inner_join (default)
merge(dt1, dt2, by = "student", all = FALSE)

# full_join
merge(dt1, dt2, by = "student", all = TRUE)

# left_join
merge(dt1, dt2, by = "student", all.x = TRUE)
dt2[dt1, on = "student"]

# right_join
merge(dt1, dt2, by = "student", all.y = TRUE)
dt1[dt2, on = "student"]

# anti_join
dt2[!dt1, on = "student"]   # dt2 - dt1
dt1[!dt2, on = "student"]   # dt1 - dt2

# using key
setkey(dt1, student)
setkey(dt2, student)
dt1[dt2]   # right_join
dt2[dt1]   # left_join
