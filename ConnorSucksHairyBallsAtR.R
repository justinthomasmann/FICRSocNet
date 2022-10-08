library(dplyr)
library(igraph)

#read in data
condata <- read.csv("FicrSocNet.csv", h=T, check.names = F)

#df containing id columns 1-3
ids <- condata[,1:3]



#SUBSET BY YEAR####



#2015####

#df 2015 only 
c2015 <- condata %>% 
  select(ends_with("15"))

c2015$sums <- rowSums(c2015, na.rm=TRUE)

edge2015 <- data.frame(id1 = ids$id1,
           id2 = ids$id2,
           weight = c2015$sums)

graph2015 <- graph.data.frame(edge2015, directed = FALSE)
matrix2015 <- as_adjacency_matrix(graph2015, type = "both", names = TRUE, 
                                  sparse = FALSE, attr = "weight")

#bind id columns with 2015 columns
data2015 <- cbind(ids,c2015)



#2016#### 

#df 2016 only 
c2016 <- condata %>% 
  select(ends_with("16"))

#bind id columns with 2016 columns
data2016 <- cbind(ids,c2016)


#2017#### 

#df 2017 only 
c2017 <- condata %>% 
  select(ends_with("17"))

#bind id columns with 2017 columns
data2017 <- cbind(ids,c2017)


#2018#### 

#df 2018 only 
c2018 <- condata %>% 
  select(ends_with("18"))

#bind id columns with 2018 columns
data2018 <- cbind(ids,c2018)


#2019#### 

#df 2019 only 
c2019 <- condata %>% 
  select(ends_with("19"))

#bind id columns with 2019 columns
data2019 <- cbind(ids,c2019)

#2020#### 

#df 2020 only 
c2020 <- condata %>% 
  select(ends_with("20"))

#bind id columns with 2020 columns
data2020 <- cbind(ids,c2020)



#SUBSET BY MONTH####



#January####

cJan <- condata %>% 
  select(starts_with("1/"))

#bind id columns with January columns
dataJan <- cbind(ids,cJan)


#February####

cFeb <- condata %>% 
  select(starts_with("2"))

#bind id columns with February columns
dataFeb <- cbind(ids,cFeb)


#March####

cMar <- condata %>% 
  select(starts_with("3"))

#bind id columns with March columns
dataMar <- cbind(ids,cMar)


#April####

cApr <- condata %>% 
  select(starts_with("4"))

#bind id columns with April columns
dataApr <- cbind(ids,cApr)


#May####

cMay <- condata %>% 
  select(starts_with("5"))

#bind id columns with May columns
dataMay <- cbind(ids,cMay)


#June####

cJun <- condata %>% 
  select(starts_with("6"))

#bind id columns with June columns
dataJun <- cbind(ids,cJun)


#July####

cJul <- condata %>% 
  select(starts_with("7"))

#bind id columns with July columns
dataJul <- cbind(ids,cJul)


#August####

cAug <- condata %>% 
  select(starts_with("8"))

#bind id columns with August columns
dataAug <- cbind(ids,cAug)


#September####

cSep <- condata %>% 
  select(starts_with("9"))

#bind id columns with September columns
dataSep <- cbind(ids,cSep)


#October####

cOct <- condata %>% 
  select(starts_with("10"))

#bind id columns with October columns
dataOct <- cbind(ids,cOct)


#November####

cNov <- condata %>% 
  select(starts_with("11"))

#bind id columns with November columns
dataNov <- cbind(ids,cNov)


#December####

cDec <- condata %>% 
  select(starts_with("12"))

#bind id columns with December columns
dataDec <- cbind(ids,cDec)



#SUBSET BY SEASON####



#Breeding (Apr-Jul)####

dataBreeding <- cbind(ids,cApr,cMay,cJun,cJul)


#Nonbreeding (Aug-Nov)

dataNonbreeding <- cbind(ids,cAug,cSep,cOct,cNov)


#Winter (Dec-Mar)

dataWinter <- cbind(ids,cDec,cJan,cFeb,cMar)


