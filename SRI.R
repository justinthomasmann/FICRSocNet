library(tidyverse)
library(dplyr)

df <- read.csv("SRI_GlobalFICRSoc_test.csv", h=TRUE)

length(df$ID2)
is.data.frame(df)
sri_function <- function(ID1,ID2){
  x <- print(df$Strength[df$ID1=="02_NET214" & df$ID2=="90_NET214"])
  yax <- sum(c(df$Strength[df$ID1 =="02_NET214"],df$Strength[df$ID2=="02_NET214"]))
  yb <- sum(c(df$Strength[df$ID1 =="90_NET214"],df$Strength[df$ID2=="90_NET214"]))-x
  sri <- x/(yax+yb)
}

sri_function <- function(ID1,ID2){
  x <- df$Strength[df$ID1==ID1 & df$ID2==ID2]
  yax <- sum(c(df$Strength[df$ID1 ==ID1],df$Strength[df$ID2==ID1]))
  yb <- sum(c(df$Strength[df$ID1 ==ID2],df$Strength[df$ID2==ID2]))-x
  sri <- (x/(yax+yb))
  print(sri)
}

sri_function("02_NET214","90_NET214")
sri_function("ZW_NET115","ZM_SAND17")
sri_function("02_NET214","YN_SAND17")

for(row in seq_len(nrow(df))){
  sri_function <- function(ID1,ID2){
    x <- df$Strength[df$ID1==ID1 & df$ID2==ID2]
    yax <- sum(c(df$Strength[df$ID1 ==ID1],df$Strength[df$ID2==ID1]))
    yb <- sum(c(df$Strength[df$ID1 ==ID2],df$Strength[df$ID2==ID2]))-x
    sri <- (x/(yax+yb))
    print(sri)

  }
}

df$sri <- mapply(sri_function, df$ID1, df$ID2)
df[is.na(df)] <- 0
