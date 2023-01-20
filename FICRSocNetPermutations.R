library(tidyverse)
library(dplyr)
library(asnipe)

#group_maker1 uses two separate dataframes: ids (id1, id2) and obs (day1-4)
group_maker <- function(x){
  g1 <- ids$id1[obs[,x]> 0]
  g2 <- ids$id2[obs[,x]> 0]
  temp <- c(g1,g2)
  temp <- temp[!duplicated(temp)]
  paste(temp)
}
#Read in data
df <- read.csv("GlobalFicrSocNet.csv", h=TRUE)
#Replace NAs with 0
df[is.na(df)] <- 0

#Create separate dataframes for ids and observations
ids <- df[,1:2]
obs <- df[,3:ncol(df)]

# it works
group_maker(4)


# lapply for the WIN!!
gbiList <- lapply(1:ncol(obs), group_maker)
gbiList

gbiList_gbi <- get_group_by_individual(gbiList, data_format = "groups")

network <- get_network(gbiList_gbi, data_format = "GBI")
network


# Rename date column names with day1-4
# df <- rename(df, day1 = X7.24.2015, day2 = X8.1.2015, day3 = X8.22.2015, day4 = X8.30.2015)
# colnames(df)



#
names(gbiList)[1:ncol(obs)] <- paste("day", seq(1:ncol(obs)), sep="")
