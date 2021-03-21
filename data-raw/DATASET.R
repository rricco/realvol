library(readr)
library(dplyr)
library(xts)
library(purrr)
library(usethis)

# Setting file names
daily <- list.files("./data-raw", pattern = "[daily]")

code_file <- list.files("./data-raw", pattern = "\\.R$")

sup <- union(daily, code_file)

files <- list.files("./data-raw")

intradaily <- setdiff(files, sup)

# Reading intradaily files

for (i in 1:length(intradaily)) {

  test <- read_delim(paste0("./data-raw/",intradaily[i]), delim = ";")

  names(test) <- c("date", 'hour', 'last', 'min', 'max', 'begin')

  test <- test %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
    mutate(datetime = paste(date,hour)) %>%
    select(datetime, last) %>%
    mutate(datetime = as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S")))

  test <- as.xts(test[,-1], order.by = test$datetime)

  nam <- paste(gsub(".csv","",intradaily[i]))
  assign(nam, test)

  # usethis::use_data(  as.name(gsub(".csv","",intradaily[1]))   , overwrite = TRUE)
  # do.call("use_data", list(as.name(gsub(".csv","",intradaily[i])), overwrite = TRUE)) Using the unquoted names of xts objetcs

}

# Merging intraday files

names <- paste(gsub(".csv","",intradaily))

names_list <- lapply(names, get)

# xts_data <- merge.xts(names_list[[1]],names_list[[2]], names_list[[3]], join = "inner")

xts_data <- do.call("merge.xts", names_list)

names(xts_data) <- paste(gsub(".csv","",intradaily))

xts_data <- xts_data[complete.cases(xts_data), ]

usethis::use_data(xts_data, overwrite = TRUE)

# Reading daily files

# for (i in 1:length(daily)) {
#
#   test <- read_delim(paste0("./data-raw/",daily[i]), delim = ";")
#
#   names(test) <- c("date", 'last', 'min', 'max', 'begin')
#
#   test <- test %>%
#     mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
#     select(date, last, min, max, begin)
#
#   test <- as.xts(test[,-1], order.by = test$date)
#
#   nam <- paste(gsub(".csv","",daily[i]))
#   assign(nam, test)
#
#   # usethis::use_data(  as.name(gsub(".csv","",intradaily[1]))   , overwrite = TRUE)
#   do.call("use_data", list(as.name(gsub(".csv","",daily[i])), overwrite = TRUE))
#
# }

