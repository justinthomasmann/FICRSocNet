library(tidyverse)
library(dplyr)
library(asnipe)

#Read in data
df <- read.csv("GlobalFicrSocNet_abbrev.csv", h=TRUE)
#Replace NAs with 0
df[is.na(df)] <- 0
#Rename date column names with day1-4
df <- rename(df, day1 = X7.24.2015, day2 = X8.1.2015, day3 = X8.22.2015, day4 = X8.30.2015)
colnames(df)





#Create separate dataframes for ids and observations
ids <- df[,1:2]
obs <- df[,3:6]

#group_maker1 uses two separate dataframes: ids (id1, id2) and obs (day1-4)
group_maker1 <- function(x){
  g1 <- ids$id1[obs[,x]> 0]
  g2 <- ids$id2[obs[,x]> 0]
  temp <- c(g1,g2)
  temp <- temp[!duplicated(temp)]
  paste(temp)
}
#it works
group_maker1(4)



#For loop runs but we need to store the output to see if it's working
for(i in 1:nrow(df)) {       # for-loop over columns
  group_maker1 <- function(x){
    g1 <- ids$id1[obs[,x]> 0]
    g2 <- ids$id2[obs[,x]> 0]
    temp <- c(g1,g2)
    temp <- temp[!duplicated(temp)]
    paste(temp)
  }
}



#group_maker2 uses the original df with ids and obs combined
group_maker2 <- function(x){
  g1 <- df$id1[df[,x]> 0]
  g2 <- df$id2[df[,x]> 0]
  day <- c(g1,g2)
  day <- day[!duplicated(day)]
  paste(day)
}
#it also works
group_maker2(3)

#lapply runs but is doing weird shit
lapply(df[3:6], group_maker2)


#Not working
# mapply(group_maker1, df[,3:6])
#
# df2 <- df %>%
#   mutate(across(d, group_maker))
# df2
#
# df %>%
#   mutate(across(c(day1, day2, day3, day4), group_maker))

