#LOAD LIBRARY#
library(dplyr)
library(igraph)
library(ggraph)
library(asnipe)
library(tidygraph)
library(reshape2)
library(threejs)
library(tnet)
library(assocInd)
library(graphlayouts)
library(ggplot2)
library(ggforce)
#read in data####
#Dissertation Files > Social Network > 2022 FICR Soc. Net Data > 2015-2020 Soc. Net Data ALL >
#CSV for R > Global FicrSocNet.csv
condata <- read.csv("GlobalFicrSocNet.csv", h=TRUE, check.names = FALSE)

sex.attr <- read.csv("2015-2020EdgeAttributesSEXficr.csv", h=TRUE)

#df containing id columns 1-3
ids <- condata[,1:3]


#Global Data Refinement####

#remove the first 3 columns from this list so I can calculate sums
Global.SN <- condata %>%
    select(-c(id1, id2, pairs))

ncol(Global.SN)
#375 observations

#create and calculate a "sums" column at the end
Global.SN$sums <- rowSums(Global.SN, na.rm = TRUE)

#bind the IDs to the list
Global.data <- cbind(ids, Global.SN)
#get rid of data in between the sums and the IDs
edge.global <- data.frame(id1 = ids$id1,
                       id2 = ids$id2,
                       weight = Global.SN$sums)

#Make it a dataframe so I can convert it to a matrix/igraph object
Global.graph <- graph.data.frame(edge.global, directed = FALSE)

#Make it a matrix to create plots (see below where graph.adjacency is used for plots/metrics)
Global.matrix <- as_adjacency_matrix(Global.graph, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")

global.adjacency <- graph.adjacency(Global.matrix, mode= "undirected", weighted=TRUE, diag = FALSE)

#This works for the final list but not the edgelist
igraph::V
V(global.adjacency)$Sex <- c("F","M","F", "M", "M", "M","F", "F", "F","M", "F", "M","M", "F", "M",
                        "M", "F", "M", "F","F", "U","M", "F","F", "F", "F", "M", "M","F", "M",
                       "M", "M", "F")
#Need to find out how to get it on the original edgelist as this is only stored in this one
#"global.adjacency object#
summary(global.adjacency)

###REMOVE SUBSETS ONCE METRICS ARE CLEANED/CALCULATED

#SUBSET BY YEAR####
#2015####

#df 2015 only
c2015 <- Global.SN  %>%
  select(ends_with("15"))

ncol(c2015)
#19

#make a new row at the end that sums up all of the rows themselves
c2015$sums <- rowSums(c2015, na.rm=TRUE)

#bind id columns with 2015 columns
data2015 <- cbind(ids,c2015)

#get rid of data in between the sums and the IDs
edge2015 <- data.frame(id1 = ids$id1,
           id2 = ids$id2,
           weight = c2015$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graph2015 <- graph.data.frame(edge2015, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrix2015 <- as_adjacency_matrix(graph2015, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")



#write.csv(matrix2015.adj3, "2015 Adjusted FICR matrix.csv")

#2016####

#df 2016 only
c2016 <- condata %>%
  select(ends_with("16"))

ncol(c2016)
#46

c2016$sums <- rowSums(c2016, na.rm=TRUE)

#bind id columns with 2016 columns
data2016 <- cbind(ids,c2016)

edge2016 <- data.frame(id1 = ids$id1,
                       id2 = ids$id2,
                       weight = c2016$sums)

graph2016 <- graph.data.frame(edge2016, directed = FALSE)

matrix2016 <- as_adjacency_matrix(graph2016, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")



#2017####

#df 2017 only
c2017 <- condata %>%
  select(ends_with("17"))

ncol(c2017)
#126

c2017$sums <- rowSums(c2017, na.rm=TRUE)

#bind id columns with 2017 columns
data2017 <- cbind(ids,c2017)

edge2017 <- data.frame(id1 = ids$id1,
                       id2 = ids$id2,
                       weight = c2017$sums)

graph2017 <- graph.data.frame(edge2017, directed = FALSE)

matrix2017 <- as_adjacency_matrix(graph2017, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")



#2018####

#df 2018 only
c2018 <- condata %>%
  select(ends_with("18"))

ncol(c2018)
#86

c2018$sums <- rowSums(c2018, na.rm=TRUE)

#bind id columns with 2018 columns
data2018 <- cbind(ids,c2018)

edge2018 <- data.frame(id1 = ids$id1,
                       id2 = ids$id2,
                       weight = c2018$sums)

graph2018 <- graph.data.frame(edge2018, directed = FALSE)

matrix2018 <- as_adjacency_matrix(graph2018, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")

#2019####

#df 2019 only
c2019 <- condata %>%
  select(ends_with("19"))

ncol(c2019)
#72

c2019$sums <- rowSums(c2019, na.rm=TRUE)

#bind id columns with 2019 columns
data2019 <- cbind(ids,c2019)

edge2019 <- data.frame(id1 = ids$id1,
                       id2 = ids$id2,
                       weight = c2019$sums)

graph2019 <- graph.data.frame(edge2019, directed = FALSE)

matrix2019 <- as_adjacency_matrix(graph2019, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")

#2020####

#df 2020 only
c2020 <- condata %>%
  select(ends_with("20"))

ncol(c2020)
#26

c2020$sums <- rowSums(c2020, na.rm=TRUE)

#bind id columns with 2020 columns
data2020 <- cbind(ids,c2020)

edge2020 <- data.frame(id1 = ids$id1,
                       id2 = ids$id2,
                       weight = c2020$sums)

graph2020 <- graph.data.frame(edge2020, directed = FALSE)

matrix2020 <- as_adjacency_matrix(graph2020, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")


#SUBSET BY MONTH####

#January####

cJan <- condata %>%
  select(starts_with("1/"))

ncol(cJan)
# 14
cJan$sums <- rowSums(cJan, na.rm=TRUE)

#bind id columns with January columns
dataJan <- cbind(ids,cJan)

edgeJan <- data.frame(id1 = ids$id1,
                       id2 = ids$id2,
                       weight = cJan$sums)

graphJan <- graph.data.frame(edgeJan, directed = FALSE)

matrixJan <- as_adjacency_matrix(graphJan, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")

#February####

cFeb <- condata %>%
  select(starts_with("2"))
ncol(cFeb)
#28
cFeb$sums <- rowSums(cFeb, na.rm=TRUE)


#bind id columns with February columns
dataFeb <- cbind(ids,cFeb)

edgeFeb <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cFeb$sums)

graphFeb <- graph.data.frame(edgeFeb, directed = FALSE)

matrixFeb <- as_adjacency_matrix(graphFeb, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")
#March####

cMar <- condata %>%
  select(starts_with("3"))
ncol(cMar)
#40
cMar$sums <- rowSums(cMar, na.rm=TRUE)

#bind id columns with March columns
dataMar <- cbind(ids,cMar)

edgeMar <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cMar$sums)

graphMar <- graph.data.frame(edgeMar, directed = FALSE)

matrixMar <- as_adjacency_matrix(graphMar, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")
#April####

cApr <- condata %>%
  select(starts_with("4"))
ncol(cApr)
#23
cApr$sums <- rowSums(cApr, na.rm=TRUE)

#bind id columns with April columns
dataApr <- cbind(ids,cApr)

edgeApr <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cApr$sums)

graphApr <- graph.data.frame(edgeApr, directed = FALSE)

matrixApr <- as_adjacency_matrix(graphApr, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#May####

cMay <- condata %>%
  select(starts_with("5"))

ncol(cMay)
#23
cMay$sums <- rowSums(cMay, na.rm=TRUE)

#bind id columns with May columns
dataMay <- cbind(ids,cMay)

edgeMay <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cMay$sums)

graphMay <- graph.data.frame(edgeMay, directed = FALSE)

matrixMay <- as_adjacency_matrix(graphMay, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#June####

cJun <- condata %>%
  select(starts_with("6"))

ncol(cJun)
#9
cJun$sums <- rowSums(cJun, na.rm=TRUE)

#bind id columns with June columns
dataJun <- cbind(ids,cJun)

edgeJun <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cJun$sums)

graphJun <- graph.data.frame(edgeJun, directed = FALSE)

matrixJun <- as_adjacency_matrix(graphJun, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#July####

cJul <- condata %>%
  select(starts_with("7"))
ncol(cJul)
#25
cJul$sums <- rowSums(cJul, na.rm=TRUE)

#bind id columns with July columns
dataJul <- cbind(ids,cJul)

edgeJul <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cJul$sums)

graphJul <- graph.data.frame(edgeJul, directed = FALSE)

matrixJul <- as_adjacency_matrix(graphJul, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#August####

cAug <- condata %>%
  select(starts_with("8"))
ncol(cAug)
#50
cAug$sums <- rowSums(cAug, na.rm=TRUE)

#bind id columns with August columns
dataAug <- cbind(ids,cAug)

edgeAug <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cAug$sums)

graphAug <- graph.data.frame(edgeAug, directed = FALSE)

matrixAug <- as_adjacency_matrix(graphAug, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#September####

cSep <- condata %>%
  select(starts_with("9"))
ncol(cSep)
#43
cSep$sums <- rowSums(cSep, na.rm=TRUE)

#bind id columns with September columns
dataSep <- cbind(ids,cSep)

edgeSep <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cSep$sums)

graphSep <- graph.data.frame(edgeSep, directed = FALSE)

matrixSep <- as_adjacency_matrix(graphSep, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#October####

cOct <- condata %>%
  select(starts_with("10"))
ncol(cOct)
#46
cOct$sums <- rowSums(cOct, na.rm=TRUE)

#bind id columns with October columns
dataOct <- cbind(ids,cOct)

edgeOct <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cOct$sums)

graphOct <- graph.data.frame(edgeOct, directed = FALSE)

matrixOct <- as_adjacency_matrix(graphOct, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#November####

cNov <- condata %>%
  select(starts_with("11"))
ncol(cNov)
#37
cNov$sums <- rowSums(cNov, na.rm=TRUE)

#bind id columns with November columns
dataNov <- cbind(ids,cNov)

edgeNov <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cNov$sums)

graphNov <- graph.data.frame(edgeNov, directed = FALSE)

matrixNov <- as_adjacency_matrix(graphNov, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#December####

cDec <- condata %>%
  select(starts_with("12"))
ncol(cDec)
#37

cDec$sums <- rowSums(cDec, na.rm=TRUE)

#bind id columns with December columns
dataDec <- cbind(ids,cDec)

edgeDec <- data.frame(id1 = ids$id1,
                      id2 = ids$id2,
                      weight = cDec$sums)

graphDec <- graph.data.frame(edgeDec, directed = FALSE)

matrixDec <- as_adjacency_matrix(graphDec, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")


#SUBSET BY SEASON####

#Breeding (Apr-Jul)####

dataBreeding <- condata %>%
  select(starts_with(c("4", "5", "6", "7")))

ncol(dataBreeding)
#80

dataBreeding$sums <- rowSums(dataBreeding, na.rm=TRUE)

dataBreeding <- cbind(ids, dataBreeding)

edgeBreeding <- data.frame(id1 = ids$id1,
                           id2 = ids$id2,
                           weight = dataBreeding$sums)

graphBreeding <- graph.data.frame(edgeBreeding, directed = FALSE)

matrixBreeding <- as_adjacency_matrix(graphBreeding, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")


#Nonbreeding (Aug-Nov)####

dataNonbreeding <- condata %>%
  select(starts_with(c("8", "9", "10", "11")))

ncol(dataNonbreeding)
#176

dataNonbreeding$sums <- rowSums(dataNonbreeding, na.rm = TRUE)

dataNonbreeding <- cbind(ids, dataNonbreeding)

edgeNonbreeding <- data.frame(id1 = ids$id1,
                              id2 = ids$id2,
                              weight = dataNonbreeding$sums)

graphNonbreeding <- graph.data.frame(edgeNonbreeding, directed = FALSE)

matrixNonbreeding <- as_adjacency_matrix(graphNonbreeding, type = "both", names = TRUE,
                                      sparse = FALSE, attr = "weight")

#Winter (Dec-Mar)####
#Do not use below line, it won't work for newer calculations
#dataWinter <- cbind(ids,cDec,cJan,cFeb,cMar)

dataWinter <- condata %>%
  select(starts_with(c("12", "1/", "2", "3")))
ncol(dataWinter)
#119

dataWinter$sums <- rowSums(dataWinter, na.rm = TRUE)

dataWinter <- cbind(ids, dataWinter)

edgeWinter <- data.frame(id1 = ids$id1,
                         id2 = ids$id2,
                         weight = dataWinter$sums)

graphWinter <- graph.data.frame(edgeWinter, directed = FALSE)

matrixWinter <- as_adjacency_matrix(graphWinter, type = "both", names = TRUE,
                                    sparse = FALSE, attr = "weight")







#SEASON BY YEAR (i.e. BREEDING2015, NONBREEDING2015, WINTER2015)####
#REFRESH ENVIRONMENT BEFORE DOING THESE, AS ABOVE CODE ADDS A COLUMN TO THE "c2015" etc. DATAFRAME####
#2015 SUBSETTED BY SEASON#####
#2015 Breeding Season

#df 2015 Breeding season only
c2015 <- condata %>%
  select(ends_with("15"))


Breeding.2015 <- c2015 %>%
  select(starts_with(c("4", "5", "6", "7")))

ncol(Breeding.2015)
#2

#make a new row at the end that sums up all of the rows themselves
Breeding.2015$sums <- rowSums(Breeding.2015, na.rm=TRUE)

#bind id columns with 2015 columns
dataBreed2015 <- cbind(ids,Breeding.2015)

#get rid of data in between the sums and the IDs
edgeBreed2015 <- data.frame(id1 = ids$id1,
                            id2 = ids$id2,
                            weight = Breeding.2015$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphBreed2015 <- graph.data.frame(edgeBreed2015, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixBreed2015 <- as_adjacency_matrix(graphBreed2015, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#2015 Nonbreeding season

Nonbreeding2015 <- c2015 %>%
  select(starts_with(c("8", "9", "10", "11")))

ncol(Nonbreeding2015)
#13

#make a new row at the end that sums up all of the rows themselves
Nonbreeding2015$sums <- rowSums(Nonbreeding2015, na.rm=TRUE)

#bind id columns with 2015 columns
dataNonbreed2015 <- cbind(ids,Nonbreeding2015)

#get rid of data in between the sums and the IDs
edgeNonbreed2015 <- data.frame(id1 = ids$id1,
                               id2 = ids$id2,
                               weight = Nonbreeding2015$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphNonbreed2015 <- graph.data.frame(edgeNonbreed2015, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixNonbreed2015 <- as_adjacency_matrix(graphNonbreed2015, type = "both", names = TRUE,
                                          sparse = FALSE, attr = "weight")

#write.csv(matrix2015.adj3, "2015 Adjusted FICR matrix.csv")

#2015 Winter Season

Winter2015 <- c2015 %>%
  select(starts_with(c("12", "1/", "2", "3")))

ncol(Winter2015)
#4

#make a new row at the end that sums up all of the rows themselves
Winter2015$sums <- rowSums(Winter2015, na.rm=TRUE)

#bind id columns with 2015 columns
dataWinter2015 <- cbind(ids,Winter2015)

#get rid of data in between the sums and the IDs
edgeWinter2015 <- data.frame(id1 = ids$id1,
                             id2 = ids$id2,
                             weight = Winter2015$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphWinter2015 <- graph.data.frame(edgeWinter2015, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixWinter2015 <- as_adjacency_matrix(graphWinter2015, type = "both", names = TRUE,
                                        sparse = FALSE, attr = "weight")







#2016 SUBSETTED BY SEASON####
#df 2016 Breeding season only
c2016 <- condata %>%
  select(ends_with("16"))

#2016 Breeding Season



Breeding.2016 <- c2016 %>%
  select(starts_with(c("4", "5", "6", "7")))

ncol(Breeding.2016)
#12

#make a new row at the end that sums up all of the rows themselves
Breeding.2016$sums <- rowSums(Breeding.2016, na.rm=TRUE)

#bind id columns with 2016 columns
dataBreed2016 <- cbind(ids,Breeding.2016)

#get rid of data in between the sums and the IDs
edgeBreed2016 <- data.frame(id1 = ids$id1,
                            id2 = ids$id2,
                            weight = Breeding.2016$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphBreed2016 <- graph.data.frame(edgeBreed2016, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixBreed2016 <- as_adjacency_matrix(graphBreed2016, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")



#write.csv(matrix2016.adj3, "2016 Adjusted FICR matrix.csv")

#2016 Nonbreeding season

Nonbreeding2016 <- c2016 %>%
  select(starts_with(c("8", "9", "10", "11")))

ncol(Nonbreeding2016)
#18

#make a new row at the end that sums up all of the rows themselves
Nonbreeding2016$sums <- rowSums(Nonbreeding2016, na.rm=TRUE)

#bind id columns with 2016 columns
dataNonbreed2016 <- cbind(ids,Nonbreeding2016)

#get rid of data in between the sums and the IDs
edgeNonbreed2016 <- data.frame(id1 = ids$id1,
                               id2 = ids$id2,
                               weight = Nonbreeding2016$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphNonbreed2016 <- graph.data.frame(edgeNonbreed2016, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixNonbreed2016 <- as_adjacency_matrix(graphNonbreed2016, type = "both", names = TRUE,
                                          sparse = FALSE, attr = "weight")

#write.csv(matrix2016.adj3, "2016 Adjusted FICR matrix.csv")

#2016 Winter Season#

Winter2016 <- c2016 %>%
  select(starts_with(c("12", "1/", "2", "3")))

ncol(Winter2016)
#16

#make a new row at the end that sums up all of the rows themselves
Winter2016$sums <- rowSums(Winter2016, na.rm=TRUE)

#bind id columns with 2016 columns
dataWinter2016 <- cbind(ids,Winter2016)

#get rid of data in between the sums and the IDs
edgeWinter2016 <- data.frame(id1 = ids$id1,
                             id2 = ids$id2,
                             weight = Winter2016$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphWinter2016 <- graph.data.frame(edgeWinter2016, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixWinter2016 <- as_adjacency_matrix(graphWinter2016, type = "both", names = TRUE,
                                        sparse = FALSE, attr = "weight")


#2017 SUBSETTED BY SEASON####
#df 2017 Breeding season only
c2017 <- condata %>%
  select(ends_with("17"))

#2017 Breeding Season



Breeding.2017 <- c2017 %>%
  select(starts_with(c("4", "5", "6", "7")))

ncol(Breeding.2017)
#33

#make a new row at the end that sums up all of the rows themselves
Breeding.2017$sums <- rowSums(Breeding.2017, na.rm=TRUE)

#bind id columns with 2017 columns
dataBreed2017 <- cbind(ids,Breeding.2017)

#get rid of data in between the sums and the IDs
edgeBreed2017 <- data.frame(id1 = ids$id1,
                            id2 = ids$id2,
                            weight = Breeding.2017$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphBreed2017 <- graph.data.frame(edgeBreed2017, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixBreed2017 <- as_adjacency_matrix(graphBreed2017, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#2017 Nonbreeding season

Nonbreeding2017 <- c2017 %>%
  select(starts_with(c("8", "9", "10", "11")))

ncol(Nonbreeding2017)
#62

#make a new row at the end that sums up all of the rows themselves
Nonbreeding2017$sums <- rowSums(Nonbreeding2017, na.rm=TRUE)

#bind id columns with 2017 columns
dataNonbreed2017 <- cbind(ids,Nonbreeding2017)

#get rid of data in between the sums and the IDs
edgeNonbreed2017 <- data.frame(id1 = ids$id1,
                               id2 = ids$id2,
                               weight = Nonbreeding2017$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphNonbreed2017 <- graph.data.frame(edgeNonbreed2017, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixNonbreed2017 <- as_adjacency_matrix(graphNonbreed2017, type = "both", names = TRUE,
                                          sparse = FALSE, attr = "weight")

#2017 Winter Season

Winter2017 <- c2017 %>%
  select(starts_with(c("12", "1/", "2", "3")))

ncol(Winter2017)
#31

#make a new row at the end that sums up all of the rows themselves
Winter2017$sums <- rowSums(Winter2017, na.rm=TRUE)

#bind id columns with 2017 columns
dataWinter2017 <- cbind(ids,Winter2017)

#get rid of data in between the sums and the IDs
edgeWinter2017 <- data.frame(id1 = ids$id1,
                             id2 = ids$id2,
                             weight = Winter2017$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphWinter2017 <- graph.data.frame(edgeWinter2017, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixWinter2017 <- as_adjacency_matrix(graphWinter2017, type = "both", names = TRUE,
                                        sparse = FALSE, attr = "weight")

#2018 SUBSETTED BY SEASON####
#df 2018 Breeding season only
c2018 <- condata %>%
  select(ends_with("18"))

#2018 Breeding Season

Breeding.2018 <- c2018 %>%
  select(starts_with(c("4", "5", "6", "7")))

ncol(Breeding.2018)
#24

#make a new row at the end that sums up all of the rows themselves
Breeding.2018$sums <- rowSums(Breeding.2018, na.rm=TRUE)

#bind id columns with 2018 columns
dataBreed2018 <- cbind(ids,Breeding.2018)

#get rid of data in between the sums and the IDs
edgeBreed2018 <- data.frame(id1 = ids$id1,
                            id2 = ids$id2,
                            weight = Breeding.2018$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphBreed2018 <- graph.data.frame(edgeBreed2018, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixBreed2018 <- as_adjacency_matrix(graphBreed2018, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#2018 Nonbreeding season

Nonbreeding2018 <- c2018 %>%
  select(starts_with(c("8", "9", "10", "11")))

ncol(Nonbreeding2018)
#44

#make a new row at the end that sums up all of the rows themselves
Nonbreeding2018$sums <- rowSums(Nonbreeding2018, na.rm=TRUE)

#bind id columns with 2018 columns
dataNonbreed2018 <- cbind(ids,Nonbreeding2018)

#get rid of data in between the sums and the IDs
edgeNonbreed2018 <- data.frame(id1 = ids$id1,
                               id2 = ids$id2,
                               weight = Nonbreeding2018$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphNonbreed2018 <- graph.data.frame(edgeNonbreed2018, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixNonbreed2018 <- as_adjacency_matrix(graphNonbreed2018, type = "both", names = TRUE,
                                          sparse = FALSE, attr = "weight")

#2018 Winter Season

Winter2018 <- c2018 %>%
  select(starts_with(c("12", "1/", "2", "3")))

ncol(Winter2018)
#18

#make a new row at the end that sums up all of the rows themselves
Winter2018$sums <- rowSums(Winter2018, na.rm=TRUE)

#bind id columns with 2018 columns
dataWinter2018 <- cbind(ids,Winter2018)

#get rid of data in between the sums and the IDs
edgeWinter2018 <- data.frame(id1 = ids$id1,
                             id2 = ids$id2,
                             weight = Winter2018$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphWinter2018 <- graph.data.frame(edgeWinter2018, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixWinter2018 <- as_adjacency_matrix(graphWinter2018, type = "both", names = TRUE,
                                        sparse = FALSE, attr = "weight")

#2019 SUBSETTED BY SEASON####
#df 2019 Breeding season only
c2019 <- condata %>%
  select(ends_with("19"))

#2019 Breeding Season

Breeding.2019 <- c2019 %>%
  select(starts_with(c("4", "5", "6", "7")))

ncol(Breeding.2019)
#7

#make a new row at the end that sums up all of the rows themselves
Breeding.2019$sums <- rowSums(Breeding.2019, na.rm=TRUE)

#bind id columns with 2019 columns
dataBreed2019 <- cbind(ids,Breeding.2019)

#get rid of data in between the sums and the IDs
edgeBreed2019 <- data.frame(id1 = ids$id1,
                            id2 = ids$id2,
                            weight = Breeding.2019$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphBreed2019 <- graph.data.frame(edgeBreed2019, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixBreed2019 <- as_adjacency_matrix(graphBreed2019, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#2019 Nonbreeding season

Nonbreeding2019 <- c2019 %>%
  select(starts_with(c("8", "9", "10", "11")))

ncol(Nonbreeding2019)
#39

#make a new row at the end that sums up all of the rows themselves
Nonbreeding2019$sums <- rowSums(Nonbreeding2019, na.rm=TRUE)

#bind id columns with 2019 columns
dataNonbreed2019 <- cbind(ids,Nonbreeding2019)

#get rid of data in between the sums and the IDs
edgeNonbreed2019 <- data.frame(id1 = ids$id1,
                               id2 = ids$id2,
                               weight = Nonbreeding2019$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphNonbreed2019 <- graph.data.frame(edgeNonbreed2019, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixNonbreed2019 <- as_adjacency_matrix(graphNonbreed2019, type = "both", names = TRUE,
                                          sparse = FALSE, attr = "weight")

#2019 Winter Season

Winter2019 <- c2019 %>%
  select(starts_with(c("12", "1/", "2", "3")))

ncol(Winter2019)
#26

#make a new row at the end that sums up all of the rows themselves
Winter2019$sums <- rowSums(Winter2019, na.rm=TRUE)

#bind id columns with 2019 columns
dataWinter2019 <- cbind(ids,Winter2019)

#get rid of data in between the sums and the IDs
edgeWinter2019 <- data.frame(id1 = ids$id1,
                             id2 = ids$id2,
                             weight = Winter2019$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphWinter2019 <- graph.data.frame(edgeWinter2019, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixWinter2019 <- as_adjacency_matrix(graphWinter2019, type = "both", names = TRUE,
                                        sparse = FALSE, attr = "weight")



#2020 SUBSETTED BY SEASON####
#df 2020 Breeding season only
c2020 <- condata %>%
  select(ends_with("20"))

#2020 Breeding Season

Breeding.2020 <- c2020 %>%
  select(starts_with(c("4", "5", "6", "7")))

ncol(Breeding.2020)
#2

#make a new row at the end that sums up all of the rows themselves
Breeding.2020$sums <- rowSums(Breeding.2020, na.rm=TRUE)

#bind id columns with 2020 columns
dataBreed2020 <- cbind(ids,Breeding.2020)

#get rid of data in between the sums and the IDs
edgeBreed2020 <- data.frame(id1 = ids$id1,
                            id2 = ids$id2,
                            weight = Breeding.2020$sums)

which.max(edgeBreed2020$weight)
#weight of 1 is the highest

#Make it a dataframe so I can convert it to a matrix/igraph object
graphBreed2020 <- graph.data.frame(edgeBreed2020, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixBreed2020 <- as_adjacency_matrix(graphBreed2020, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#2020 Nonbreeding season

Nonbreeding2020 <- c2020 %>%
  select(starts_with(c("8", "9", "10", "11")))

ncol(Nonbreeding2020)
#0

#make a new row at the end that sums up all of the rows themselves
Nonbreeding2020$sums <- rowSums(Nonbreeding2020, na.rm=TRUE)

#bind id columns with 2020 columns
dataNonbreed2020 <- cbind(ids,Nonbreeding2020)

#get rid of data in between the sums and the IDs
edgeNonbreed2020 <- data.frame(id1 = ids$id1,
                               id2 = ids$id2,
                               weight = Nonbreeding2020$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphNonbreed2020 <- graph.data.frame(edgeNonbreed2020, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixNonbreed2020 <- as_adjacency_matrix(graphNonbreed2020, type = "both", names = TRUE,
                                          sparse = FALSE, attr = "weight")

#2020 Winter Season

Winter2020 <- c2020 %>%
  select(starts_with(c("12", "1/", "2", "3")))

ncol(Winter2020)
#24

#make a new row at the end that sums up all of the rows themselves
Winter2020$sums <- rowSums(Winter2020, na.rm=TRUE)

#bind id columns with 2020 columns
dataWinter2020 <- cbind(ids,Winter2020)

#get rid of data in between the sums and the IDs
edgeWinter2020 <- data.frame(id1 = ids$id1,
                             id2 = ids$id2,
                             weight = Winter2020$sums)



#Make it a dataframe so I can convert it to a matrix/igraph object
graphWinter2020 <- graph.data.frame(edgeWinter2020, directed = FALSE)

#Make it a matrix to create plots and calculate metrics from older script
matrixWinter2020 <- as_adjacency_matrix(graphWinter2020, type = "both", names = TRUE,
                                        sparse = FALSE, attr = "weight")


#Network Calculations####
###################################REMOVING ANY ISOLATED EDGES (0) TIES##########################################
#GLOBAL####
#Select only values with "weights" 1 or larger
Global <- edge.global

Global.1 <- Global %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
Globgraph.1 <- graph.data.frame(Global.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
Glob.Matrix1 <- as_adjacency_matrix(Globgraph.1, type = "both", names = TRUE,
                                    sparse = FALSE, attr = "weight")

#make it an igraph object
Glob.Matrix1 <- graph.adjacency(Glob.Matrix1, mode= "undirected", weighted=TRUE, diag = FALSE)
summary(Glob.Matrix1)


#This works for the final list but not the edgelist
V(Glob.Matrix1)$Sex <- c("F","F","M","M","M","F","F","F","M","M","F","M","M","F","M","F","M","F","F",
                             "F","F","M","M","F","M","M","F","U","F","F")
summary(Glob.Matrix1)

##Number of Nodes
gorder(Glob.Matrix1)
#30

##Number of edges (regardless of weight)
gsize(Glob.Matrix1)
# 292

##Degree:the number of nodes at distance 1
degreeGlobal <- degree(Glob.Matrix1)
mean(degreeGlobal)
#19.46667

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(Glob.Matrix1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeGlobal,
                                                                               color = factor(Sex))) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("Glob.Matrix1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Global <- strength(Glob.Matrix1, vids = V(Glob.Matrix1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Global)
#465.4667

#see who has the highest strength
melt(strength.Global)%>%
  arrange(desc(strength.Global))
#HN_NET115  1028

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessGlobal <- closeness(Glob.Matrix1, vids = V(Glob.Matrix1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessGlobal)
#0.007324182

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(Glob.Matrix1)
mean(betweenness(Glob.Matrix1))
#25.97222

##Eigenvector Centrality
#EC a measure of the influence of a node
gGlobal.ec <- eigen_centrality(Glob.Matrix1, directed = FALSE)

###Average E.C. Score
mean(gGlobal.ec$vector)
#0.4540158

which.max(gGlobal.ec$vector)
#HN_NET115
#13


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(Glob.Matrix1, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(Glob.Matrix1, loops = FALSE)
#0.6712644

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(Glob.Matrix1, type = "average")
#0.8651399


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(Glob.Matrix1)
#0.81914

##Average Path Length
distances(Glob.Matrix1)
mean_distance(Glob.Matrix1)
#1.326437


#2015 METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.2015.1 <- edge2015

#Testing join#####
#join has to happen after filtering weights
edge.2015.1 <- edge.2015.1 %>%
  filter(weight >= 1)

edge.2015.1sex <- inner_join(sex.attr, edge.2015.1, by = "id1")
edge.2015.1sex <- edge.2015.1sex[,1:2]




#same for adjusted 3 up-make it a data frame
graph.2015.1 <- graph.data.frame(edge.2015.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.2015.1 <- as_adjacency_matrix(graph.2015.1, type = "both", names = TRUE,
                                sparse = FALSE, attr = "weight")

#make it an igraph object
g.2015.1 <- graph.adjacency(g.2015.1, mode= "undirected", weighted=TRUE, diag = FALSE)
V(g.2015.1)$sex=factor(edge.2015.1sex[match(V(g.2015.1)$id1,edge.2015.1sex$name),"Sex"])

#we need to remove duplicates from the sex df

##Number of Nodes
gorder(g.2015.1)
#15

##Number of edges (regardless of weight)
gsize(g.2015.1)
#89

##Degree:the number of nodes at distance 1
degree2015 <- degree(g.2015.1)
mean(degree2015)
#11.86667

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.2015.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree2015),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.2015.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2015 <- strength(g.2015.1, vids = V(g.2015.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.2015)
#44.26667

#see who has the highest strength
melt(strength.2015)%>%
  arrange(desc(strength.2015))
#78_NET214    86

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness2015 <- closeness(g.2015.1, vids = V(g.2015.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness2015)
#0.0316264

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.2015.1)
mean(betweenness(g.2015.1))
#5.907778

##Eigenvector Centrality
#EC a measure of the influence of a node
g2015.ec <- eigen_centrality(g.2015.1, directed = FALSE)

###Average E.C. Score
mean(g2015.ec$vector)
#0.5511107

which.max(g2015.ec$vector)
#78_NET214
#2


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.2015.1, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.2015.1, loops = FALSE)
#0.847619

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.2015.1, type = "average")
#0.9148074


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.2015.1)
#0.8949455

##Average Path Length
distances(g.2015.1)
mean_distance(g.2015.1)
#1.152381
#2016 METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.2016.1 <- edge2016

edge.2016.1 <- edge.2016.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.2016.1 <- graph.data.frame(edge.2016.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.2016.1 <- as_adjacency_matrix(graph.2016.1, type = "both", names = TRUE,
                                sparse = FALSE, attr = "weight")

#make it an igraph object
g.2016.1 <- graph.adjacency(g.2016.1, mode= "undirected", weighted=TRUE, diag = FALSE)
summary(g.2016.1)


##Number of Nodes
gorder(g.2016.1)
#17

##Number of edges (regardless of weight)
gsize(g.2016.1)
#130

##Degree:the number of nodes at distance 1
degree2016 <- degree(g.2016.1)
mean(degree2016)
#15.29412

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.2016.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree2016),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.2016.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2016 <- strength(g.2016.1, vids = V(g.2016.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.2016)
#82.82353

#see who has the highest strength
melt(strength.2016)%>%
  arrange(desc(strength.2016))
#AU_NET115   161

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness2016 <- closeness(g.2016.1, vids = V(g.2016.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness2016)
#0.01863538

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.2016.1)
mean(betweenness(g.2016.1))
#6.096639

##Eigenvector Centrality
#EC a measure of the influence of a node
g2016.ec <- eigen_centrality(g.2016.1, directed = FALSE)

###Average E.C. Score
mean(g2016.ec$vector)
#0.5410151

which.max(g2016.ec$vector)
#AU_NET115
#4


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.2016.1, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.2016.1, loops = FALSE)
#0.9558824

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.2016.1, type = "average")
#0.9661064


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.2016.1)
#0.9636558

##Average Path Length
distances(g.2016.1)
mean_distance(g.2016.1)
#1.044118



#2017 METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.2017.1 <- edge2017

edge.2017.1 <- edge.2017.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.2017.1 <- graph.data.frame(edge.2017.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.2017.1 <- as_adjacency_matrix(graph.2017.1, type = "both", names = TRUE,
                                sparse = FALSE, attr = "weight")

#make it an igraph object
g.2017.1 <- graph.adjacency(g.2017.1, mode= "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.2017.1)
#22

##Number of edges (regardless of weight)
gsize(g.2017.1)
# 213

##Degree:the number of nodes at distance 1
degree2017 <- degree(g.2017.1)
mean(degree2017)
#19.36364

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.2017.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree2017),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.2017.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2017 <- strength(g.2017.1, vids = V(g.2017.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.2017)
#348

#see who has the highest strength
melt(strength.2017)%>%
  arrange(desc(strength.2017))
#IH_NET116   538

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness2017 <- closeness(g.2017.1, vids = V(g.2017.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness2017)
#0.007980015

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.2017.1)
mean(betweenness(g.2017.1))
#16.24242

##Eigenvector Centrality
#EC a measure of the influence of a node
g2017.ec <- eigen_centrality(g.2017.1, directed = FALSE)

###Average E.C. Score
mean(g2017.ec$vector)
#0.6644328

which.max(g2017.ec$vector)
#IH_NET116
#11


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.2017.1, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.2017.1, loops = FALSE)
#0.9220779

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.2017.1, type = "average")
#0.9489325


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.2017.1)
#0.9433535

##Average Path Length
distances(g.2017.1)
mean_distance(g.2017.1)
#1.077922
#2018 METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.2018.1 <- edge2018

edge.2018.1 <- edge.2018.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.2018.1 <- graph.data.frame(edge.2018.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.2018.1 <- as_adjacency_matrix(graph.2018.1, type = "both", names = TRUE,
                                sparse = FALSE, attr = "weight")

#make it an igraph object
g.2018.1 <- graph.adjacency(g.2018.1, mode= "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.2018.1)
#18

##Number of edges (regardless of weight)
gsize(g.2018.1)
#133

##Degree:the number of nodes at distance 1
degree2018 <- degree(g.2018.1)
mean(degree2018)
# 14.77778

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.2018.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree2018),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.2018.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2018 <- strength(g.2018.1, vids = V(g.2018.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.2018)
#159.3333

#see who has the highest strength
melt(strength.2018)%>%
  arrange(desc(strength.2018))
#ZM_SAND17   322

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness2018 <- closeness(g.2018.1, vids = V(g.2018.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness2018)
#0.02325162

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.2018.1)
mean(betweenness(g.2018.1))
#8.410802

##Eigenvector Centrality
#EC a measure of the influence of a node
g.2018.ec <- eigen_centrality(g.2018.1, directed = FALSE)

###Average E.C. Score
mean(g.2018.ec$vector)
#0.5212233

which.max(g.2018.ec$vector)
#ZM_SAND17
#17

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.2018.1, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.2018.1, loops = FALSE)
# 0.869281

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.2018.1, type = "average")
#0.9292912


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.2018.1)
#0.9153077

##Average Path Length
distances(g.2018.1)
mean_distance(g.2018.1)
#1.130719
#2019 METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.2019.1 <- edge2019

edge.2019.1 <- edge.2019.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.2019.1 <- graph.data.frame(edge.2019.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.2019.1 <- as_adjacency_matrix(graph.2019.1, type = "both", names = TRUE,
                                sparse = FALSE, attr = "weight")

#make it an igraph object
g.2019.1 <- graph.adjacency(g.2019.1, mode= "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.2019.1)
#14

##Number of edges (regardless of weight)
gsize(g.2019.1)
#66

##Degree:the number of nodes at distance 1
degree2019 <- degree(g.2019.1)
mean(degree2019)
#9.428571

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.2019.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree2019),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.2019.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2019 <- strength(g.2019.1, vids = V(g.2019.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.2019)
#79.57143

#see who has the highest strength
melt(strength.2019)%>%
  arrange(desc(strength.2019))
#ZM_SAND17   164

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness2019 <- closeness(g.2019.1, vids = V(g.2019.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness2019)
#0.02210429

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.2019.1)
mean(betweenness(g.2019.1))
#7.452381

##Eigenvector Centrality
#EC a measure of the influence of a node
g.2019.ec <- eigen_centrality(g.2019.1, directed = FALSE)

###Average E.C. Score
mean(g.2019.ec$vector)
#0.5213772

which.max(g.2019.ec$vector)
#ZM_SAND17
#13


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.2019.1, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.2019.1, loops = FALSE)
#0.7252747

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.2019.1, type = "average")
#0.8853452


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.2019.1)
#0.8349515

##Average Path Length
distances(g.2019.1)
mean_distance(g.2019.1)
#1.274725
#2020 METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.2020.1 <- edge2020

edge.2020.1 <- edge.2020.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.2020.1 <- graph.data.frame(edge.2020.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.2020.1 <- as_adjacency_matrix(graph.2020.1, type = "both", names = TRUE,
                                sparse = FALSE, attr = "weight")

#make it an igraph object
g.2020.1 <- graph.adjacency(g.2020.1, mode= "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.2020.1)
#7

##Number of edges (regardless of weight)
gsize(g.2020.1)
#21

##Degree:the number of nodes at distance 1
degree2020 <- degree(g.2020.1)
mean(degree2020)
#6

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.2020.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree2020),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.2020.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2020 <- strength(g.2020.1, vids = V(g.2020.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.2020)
#36.28571

#see who has the highest strength
melt(strength.2020)%>%
  arrange(desc(strength.2020))
#SY_NET217    47

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness2020 <- closeness(g.2020.1, vids = V(g.2020.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness2020)
#0.04744555

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.2020.1)
mean(betweenness(g.2020.1))
#2.142857

##Eigenvector Centrality
#EC a measure of the influence of a node
g.2020.ec <- eigen_centrality(g.2020.1, directed = FALSE)

###Average E.C. Score
mean(g.2020.ec$vector)
#0.8023067

which.max(g.2020.ec$vector)
#SY_NET217
#5


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.2020.1, directed = FALSE, unconnected = TRUE, weights = NA)
#1

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.2020.1, loops = FALSE)
#1

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.2020.1, type = "average")
#1


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.2020.1)
#1

##Average Path Length
distances(g.2020.1)
mean_distance(g.2020.1)
#1




#JANUARY METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.Jan.1 <- edgeJan

edge.Jan.1 <- edge.Jan.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Jan.1 <- graph.data.frame(edge.Jan.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Jan.1 <- as_adjacency_matrix(graph.Jan.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Jan.1 <- graph.adjacency(g.Jan.1, mode= "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Jan.1)
#20

##Number of edges (regardless of weight)
gsize(g.Jan.1)
#117

##Degree:the number of nodes at distance 1
degreeJan <- degree(g.Jan.1)
mean(degreeJan)
#11.7

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Jan.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeJan),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Jan.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Jan <- strength(g.Jan.1, vids = V(g.Jan.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Jan)
#19.4

#see who has the highest strength
melt(strength.Jan)%>%
  arrange(desc(strength.Jan))
#HN_NET115    31

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessJan <- closeness(g.Jan.1, vids = V(g.Jan.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessJan)
#0.02960814

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Jan.1)
mean(betweenness(g.Jan.1))
#6.609482

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Jan.ec <- eigen_centrality(g.Jan.1, directed = FALSE)

###Average E.C. Score
mean(g.Jan.ec$vector)
# 0.5430588

which.max(g.Jan.ec$vector)
#KO_NET215
#12

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Jan.1, directed = FALSE, unconnected = TRUE, weights = NA)
# 3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Jan.1, loops = FALSE)
#0.6157895

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Jan.1, type = "average")
#0.8425716


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Jan.1)
#0.7893602

##Average Path Length
distances(g.Jan.1)
mean_distance(g.Jan.1)
#1.394737

#FEBRUARY METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.Feb.1 <- edgeFeb

edge.Feb.1 <- edge.Feb.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Feb.1 <- graph.data.frame(edge.Feb.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Feb.1 <- as_adjacency_matrix(graph.Feb.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Feb.1 <- graph.adjacency(g.Feb.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Feb.1)
#21

##Number of edges (regardless of weight)
gsize(g.Feb.1)
#135

##Degree:the number of nodes at distance 1
degreeFeb <- degree(g.Feb.1)
mean(degreeFeb)
#12.85714

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Feb.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeFeb),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Feb.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Feb <- strength(g.Feb.1, vids = V(g.Feb.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Feb)
#38.19048

#see who has the highest strength
melt(strength.Feb)%>%
  arrange(desc(strength.Feb))
#HN_NET115    71

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessFeb <- closeness(g.Feb.1, vids = V(g.Feb.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessFeb)
#0.02321681

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Feb.1)
mean(betweenness(g.Feb.1))
#9.611905

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Feb.ec <- eigen_centrality(g.Feb.1, directed = FALSE)

###Average E.C. Score
mean(g.Feb.ec$vector)
#0.517305

which.max(g.Feb.ec$vector)
#KO_NET215
#13


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Feb.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Feb.1, loops = FALSE)
#0.6428571

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Feb.1, type = "average")
#0.8644052


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Feb.1)
#0.8279804

##Average Path Length
distances(g.Feb.1)
mean_distance(g.Feb.1)
#1.409524

#MARCH METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.Mar.1 <- edgeMar

edge.Mar.1 <- edge.Mar.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Mar.1 <- graph.data.frame(edge.Mar.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Mar.1 <- as_adjacency_matrix(graph.Mar.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Mar.1 <- graph.adjacency(g.Mar.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Mar.1)
#24

##Number of edges (regardless of weight)
gsize(g.Mar.1)
#179

##Degree:the number of nodes at distance 1
degreeMar <- degree(g.Mar.1)
mean(degreeMar)
#14.91667

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Mar.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeMar),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Mar.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Mar <- strength(g.Mar.1, vids = V(g.Mar.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Mar)
#44.58333

#see who has the highest strength
melt(strength.Mar)%>%
  arrange(desc(strength.Mar))
#KO_NET215   113

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessMar <- closeness(g.Mar.1, vids = V(g.Mar.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessMar)
#0.01813223

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Mar.1)
mean(betweenness(g.Mar.1))
#9.775083

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Mar.ec <- eigen_centrality(g.Mar.1, directed = FALSE)

###Average E.C. Score
mean(g.Mar.ec$vector)
# 0.4224495

which.max(g.Mar.ec$vector)
#KO_NET215
#15


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Mar.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Mar.1, loops = FALSE)
#0.6485507

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Mar.1, type = "average")
#0.8352109


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Mar.1)
#0.7705925

##Average Path Length
distances(g.Mar.1)
mean_distance(g.Mar.1)
#1.362319

#APRIL METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.Apr.1 <- edgeApr

edge.Apr.1 <- edge.Apr.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Apr.1 <- graph.data.frame(edge.Apr.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Apr.1 <- as_adjacency_matrix(graph.Apr.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Apr.1 <- graph.adjacency(g.Apr.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Apr.1)
#20

##Number of edges (regardless of weight)
gsize(g.Apr.1)
#81

##Degree:the number of nodes at distance 1
degreeApr <- degree(g.Apr.1)
mean(degreeApr)
# 8.1

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Apr.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeApr),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Apr.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Apr <- strength(g.Apr.1, vids = V(g.Apr.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Apr)
#16.5

#see who has the highest strength
melt(strength.Apr)%>%
  arrange(desc(strength.Apr))
#HN_NET115    45

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessApr <- closeness(g.Apr.1, vids = V(g.Apr.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessApr)
#0.02207288

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Apr.1)
mean(betweenness(g.Apr.1))
#9.289773

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Apr.ec <- eigen_centrality(g.Apr.1, directed = FALSE)

###Average E.C. Score
mean(g.Apr.ec$vector)
#0.3953475

which.max(g.Apr.ec$vector)
#HN_NET115
#10


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Apr.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Apr.1, loops = FALSE)
#0.4263158

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Apr.1, type = "average")
#0.8045504


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Apr.1)
#0.6518219

##Average Path Length
distances(g.Apr.1)
mean_distance(g.Apr.1)
#1.626316

#MAY METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.May.1 <- edgeMay

edge.May.1 <- edge.May.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.May.1 <- graph.data.frame(edge.May.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.May.1 <- as_adjacency_matrix(graph.May.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.May.1 <- graph.adjacency(g.May.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.May.1)
#17

##Number of edges (regardless of weight)
gsize(g.May.1)
#47

##Degree:the number of nodes at distance 1
degreeMay <- degree(g.May.1)
mean(degreeMay)
#5.529412

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.May.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeMay),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.May.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.May <- strength(g.May.1, vids = V(g.May.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.May)
#10.94118

#see who has the highest strength
melt(strength.May)%>%
  arrange(desc(strength.May))
#GJ_NET116    24

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessMay <- closeness(g.May.1, vids = V(g.May.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessMay)
#0.02264922

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.May.1)
mean(betweenness(g.May.1))
#9.727451

##Eigenvector Centrality
#EC a measure of the influence of a node
g.May.ec <- eigen_centrality(g.May.1, directed = FALSE)

###Average E.C. Score
mean(g.May.ec$vector)
# 0.4148308

which.max(g.May.ec$vector)
#GJ_NET116
#6


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.May.1, directed = FALSE, unconnected = TRUE, weights = NA)
#4

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.May.1, loops = FALSE)
#0.3455882

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.May.1, type = "average")
#0.7635989


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.May.1)
#0.6486486

##Average Path Length
distances(g.May.1)
mean_distance(g.May.1)
#1.882353

#JUNE METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.Jun.1 <- edgeJun

edge.Jun.1 <- edge.Jun.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Jun.1 <- graph.data.frame(edge.Jun.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Jun.1 <- as_adjacency_matrix(graph.Jun.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Jun.1 <- graph.adjacency(g.Jun.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Jun.1)
#10

##Number of edges (regardless of weight)
gsize(g.Jun.1)
# 19

##Degree:the number of nodes at distance 1
degreeJun <- degree(g.Jun.1)
mean(degreeJun)
#3.8

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Jun.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeJun),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Jun.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Jun <- strength(g.Jun.1, vids = V(g.Jun.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Jun)
#5

#see who has the highest strength
melt(strength.Jun)%>%
  arrange(desc(strength.Jun))
#HN_NET115     9

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessJun <- closeness(g.Jun.1, vids = V(g.Jun.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessJun)
#0.05175974

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Jun.1)
mean(betweenness(g.Jun.1))
#5.216667

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Jun.ec <- eigen_centrality(g.Jun.1, directed = FALSE)

###Average E.C. Score
mean(g.Jun.ec$vector)
#0.5080717

which.max(g.Jun.ec$vector)
#IH_NET116
#6


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Jun.1, directed = FALSE, unconnected = TRUE, weights = NA)
#5

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Jun.1, loops = FALSE)
#0.4222222

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Jun.1, type = "average")
#0.6666667


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Jun.1)
#0.8333333

##Average Path Length
distances(g.Jun.1)
mean_distance(g.Jun.1)
#1.977778

#JULY METRICS NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.Jul.1 <- edgeJul

edge.Jul.1 <- edge.Jul.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Jul.1 <- graph.data.frame(edge.Jul.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Jul.1 <- as_adjacency_matrix(graph.Jul.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Jul.1 <- graph.adjacency(g.Jul.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Jul.1)
#21

##Number of edges (regardless of weight)
gsize(g.Jul.1)
#74

##Degree:the number of nodes at distance 1
degreeJul <- degree(g.Jul.1)
mean(degreeJul)
#7.047619

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Jul.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeJul),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Jul.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Jul <- strength(g.Jul.1, vids = V(g.Jul.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Jul)
#11.71429

#see who has the highest strength
melt(strength.Jul)%>%
  arrange(desc(strength.Jul))
#II_NET214    26

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessJul <- closeness(g.Jul.1, vids = V(g.Jul.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessJul)
#0.02229364

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Jul.1)
mean(betweenness(g.Jul.1))
#10.47523

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Jul.ec <- eigen_centrality(g.Jul.1, directed = FALSE)

###Average E.C. Score
mean(g.Jul.ec$vector)
#0.3708662

which.max(g.Jul.ec$vector)
#SY_NET217
#13


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Jul.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Jul.1, loops = FALSE)
#0.352381

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Jul.1, type = "average")
#0.7961538


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Jul.1)
#0.6232394

##Average Path Length
distances(g.Jul.1)
mean_distance(g.Jul.1)
#1.747619

#AUGUST METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.Aug.1 <- edgeAug

edge.Aug.1 <- edge.Aug.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Aug.1 <- graph.data.frame(edge.Aug.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Aug.1 <- as_adjacency_matrix(graph.Aug.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Aug.1 <- graph.adjacency(g.Aug.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Aug.1)
#26

##Number of edges (regardless of weight)
gsize(g.Aug.1)
#202

##Degree:the number of nodes at distance 1
degreeAug <- degree(g.Aug.1)
mean(degreeAug)
#15.53846

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Aug.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeAug),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Aug.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Aug <- strength(g.Aug.1, vids = V(g.Aug.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Aug)
#71.69231

#see who has the highest strength
melt(strength.Aug)%>%
  arrange(desc(strength.Aug))
#ZM_SAND17   159

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessAug <- closeness(g.Aug.1, vids = V(g.Aug.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessAug)
#0.01684105

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Aug.1)
mean(betweenness(g.Aug.1))
#12.84012

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Aug.ec <- eigen_centrality(g.Aug.1, directed = FALSE)

###Average E.C. Score
mean(g.Aug.ec$vector)
#0.4621206

which.max(g.Aug.ec$vector)
#ZM_SAND17
#22


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Aug.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Aug.1, loops = FALSE)
#0.6215385

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Aug.1, type = "average")
#0.8157671


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Aug.1)
#0.7610837

##Average Path Length
distances(g.Aug.1)
mean_distance(g.Aug.1)
#1.384615

#SEPTEMBER METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.Sep.1 <- edgeSep

edge.Sep.1 <- edge.Sep.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Sep.1 <- graph.data.frame(edge.Sep.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Sep.1 <- as_adjacency_matrix(graph.Sep.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Sep.1 <- graph.adjacency(g.Sep.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Sep.1)
#25

##Number of edges (regardless of weight)
gsize(g.Sep.1)
#192

##Degree:the number of nodes at distance 1
degreeSep <- degree(g.Sep.1)
mean(degreeSep)
#15.36

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Sep.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeSep),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Sep.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Sep <- strength(g.Sep.1, vids = V(g.Sep.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Sep)
#68.4

#see who has the highest strength
melt(strength.Sep)%>%
  arrange(desc(strength.Sep))
#ZM_SAND17   143

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessSep <- closeness(g.Sep.1, vids = V(g.Sep.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessSep)
#0.01493232

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Sep.1)
mean(betweenness(g.Sep.1))
# 12.04112

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Sep.ec <- eigen_centrality(g.Sep.1, directed = FALSE)

###Average E.C. Score
mean(g.Sep.ec$vector)
#0.4650383

which.max(g.Sep.ec$vector)
#ZM_SAND17
#21


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Sep.1, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Sep.1, loops = FALSE)
#0.64

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Sep.1, type = "average")
#0.864119


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Sep.1)
#0.8099709

##Average Path Length
distances(g.Sep.1)
mean_distance(g.Sep.1)
#1.36

#OCTOBER METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.Oct.1 <- edgeOct

edge.Oct.1 <- edge.Oct.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Oct.1 <- graph.data.frame(edge.Oct.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Oct.1 <- as_adjacency_matrix(graph.Oct.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Oct.1 <- graph.adjacency(g.Oct.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Oct.1)
# 26

##Number of edges (regardless of weight)
gsize(g.Oct.1)
#237

##Degree:the number of nodes at distance 1
degreeOct <- degree(g.Oct.1)
mean(degreeOct)
#18.23077

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Oct.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeOct),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Oct.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Oct <- strength(g.Oct.1, vids = V(g.Oct.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Oct)
#113.4615

#see who has the highest strength
melt(strength.Oct)%>%
  arrange(desc(strength.Oct))
#90_NET214   221

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessOct <- closeness(g.Oct.1, vids = V(g.Oct.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessOct)
#0.00907267

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Oct.1)
mean(betweenness(g.Oct.1))
#10.16081

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Oct.ec <- eigen_centrality(g.Oct.1, directed = FALSE)

###Average E.C. Score
mean(g.Oct.ec$vector)
#0.5094624

which.max(g.Oct.ec$vector)
#90_NET214
#4


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Oct.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Oct.1, loops = FALSE)
#0.7292308

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Oct.1, type = "average")
#0.9025268


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Oct.1)
#0.8742081

##Average Path Length
distances(g.Oct.1)
mean_distance(g.Oct.1)
#1.283077

#NOVEMBER METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.Nov.1 <- edgeNov

edge.Nov.1 <- edge.Nov.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Nov.1 <- graph.data.frame(edge.Nov.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Nov.1 <- as_adjacency_matrix(graph.Nov.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Nov.1 <- graph.adjacency(g.Nov.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Nov.1)
#28

##Number of edges (regardless of weight)
gsize(g.Nov.1)
#233

##Degree:the number of nodes at distance 1
degreeNov <- degree(g.Nov.1)
mean(degreeNov)
#16.64286

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Nov.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNov),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Nov.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Nov <- strength(g.Nov.1, vids = V(g.Nov.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Nov)
#94

#see who has the highest strength
melt(strength.Nov)%>%
  arrange(desc(strength.Nov))
#HN_NET115   205

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNov <- closeness(g.Nov.1, vids = V(g.Nov.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNov)
#0.01226034

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Nov.1)
mean(betweenness(g.Nov.1))
#16.94837

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Nov.ec <- eigen_centrality(g.Nov.1, directed = FALSE)

###Average E.C. Score
mean(g.Nov.ec$vector)
#0.4637583

which.max(g.Nov.ec$vector)
#HN_NET115
#13


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Nov.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Nov.1, loops = FALSE)
#0.6164021

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Nov.1, type = "average")
#0.894367


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Nov.1)
#0.8574463

##Average Path Length
distances(g.Nov.1)
mean_distance(g.Nov.1)
#1.412698

#DECEMBER METRICS NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.Dec.1 <- edgeDec

edge.Dec.1 <- edge.Dec.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Dec.1 <- graph.data.frame(edge.Dec.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Dec.1 <- as_adjacency_matrix(graph.Dec.1, type = "both", names = TRUE,
                               sparse = FALSE, attr = "weight")

#make it an igraph object
g.Dec.1 <- graph.adjacency(g.Dec.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Dec.1)
# 25

##Number of edges (regardless of weight)
gsize(g.Dec.1)
#219

##Degree:the number of nodes at distance 1
degreeDec <- degree(g.Dec.1)
mean(degreeDec)
# 17.52

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Dec.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeDec),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Dec.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Dec <- strength(g.Dec.1, vids = V(g.Dec.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Dec)
#69.44

#see who has the highest strength
melt(strength.Dec)%>%
  arrange(desc(strength.Dec))
#90_NET214   141

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessDec <- closeness(g.Dec.1, vids = V(g.Dec.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessDec)
#0.01516281

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Dec.1)
mean(betweenness(g.Dec.1))
#12.95398

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Dec.ec <- eigen_centrality(g.Dec.1, directed = FALSE)

###Average E.C. Score
mean(g.Dec.ec$vector)
#0.5158233

which.max(g.Dec.ec$vector)
#90_NET214
#4


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Dec.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Dec.1, loops = FALSE)
#0.73

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Dec.1, type = "average")
#0.8995316


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Dec.1)
#0.8748417

##Average Path Length
distances(g.Dec.1)
mean_distance(g.Dec.1)
#1.283333




#BREEDING SEASON NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.Breed.1 <- edgeBreeding

edge.Breed.1 <- edge.Breed.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Breed.1 <- graph.data.frame(edge.Breed.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Breed.1 <- as_adjacency_matrix(graph.Breed.1, type = "both", names = TRUE,
                                 sparse = FALSE, attr = "weight")

#make it an igraph object
g.Breed.1 <- graph.adjacency(g.Breed.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Breed.1)
#25

##Number of edges (regardless of weight)
gsize(g.Breed.1)
#133

##Degree:the number of nodes at distance 1
degreeBreed <- degree(g.Breed.1)
mean(degreeBreed)
#10.64

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Breed.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeBreed),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Breed.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Breed <- strength(g.Breed.1, vids = V(g.Breed.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Breed)
#32.48

#see who has the highest strength
melt(strength.Breed)%>%
  arrange(desc(strength.Breed))
#HN_NET115    89

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessBreed <- closeness(g.Breed.1, vids = V(g.Breed.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessBreed)
#0.01684412

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Breed.1)
mean(betweenness(g.Breed.1))
#12.94025

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Breed.ec <- eigen_centrality(g.Breed.1, directed = FALSE)

###Average E.C. Score
mean(g.Breed.ec$vector)
#0.3764487

which.max(g.Breed.ec$vector)
#HN_NET115
#10


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Breed.1, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Breed.1, loops = FALSE)
#0.4433333

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Breed.1, type = "average")
#0.7548338


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Breed.1)
#0.6609195

##Average Path Length
distances(g.Breed.1)
mean_distance(g.Breed.1)
#1.623333

#NON-BREEDING SEASON NO ISOLATES####

#Select only values with "weights" 1 or larger
edge.Nonbreed.1 <- edgeNonbreeding

edge.Nonbreed.1 <- edge.Nonbreed.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Nonbreed.1 <- graph.data.frame(edge.Nonbreed.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Nonbreed.1 <- as_adjacency_matrix(graph.Nonbreed.1, type = "both", names = TRUE,
                                    sparse = FALSE, attr = "weight")

#make it an igraph object
g.Nonbreed.1 <- graph.adjacency(g.Nonbreed.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Nonbreed.1)
#29

##Number of edges (regardless of weight)
gsize(g.Nonbreed.1)
#276

##Degree:the number of nodes at distance 1
degreeNonbreed <- degree(g.Nonbreed.1)
mean(degreeNonbreed)
# 19.03448

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Nonbreed.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNonbreed),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Nonbreed.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Nonbreed <- strength(g.Nonbreed.1, vids = V(g.Nonbreed.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Nonbreed)
#315.7241

#see who has the highest strength
melt(strength.Nonbreed)%>%
  arrange(desc(strength.Nonbreed))
#90_NET214   683

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNonbreed <- closeness(g.Nonbreed.1, vids = V(g.Nonbreed.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNonbreed)
#0.007525205

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Nonbreed.1)
mean(betweenness(g.Nonbreed.1))
#22.56149

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Nonbreed.ec <- eigen_centrality(g.Nonbreed.1, directed = FALSE)

###Average E.C. Score
mean(g.Nonbreed.ec$vector)
#0.4827627

which.max(g.Nonbreed.ec$vector)
#90_NET214
#4


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Nonbreed.1, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Nonbreed.1, loops = FALSE)
#0.679803

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Nonbreed.1, type = "average")
#0.8767207


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Nonbreed.1)
#0.8317264

##Average Path Length
distances(g.Nonbreed.1)
mean_distance(g.Nonbreed.1)
#1.320197

#WINTER NO ISOLATES####
#Select only values with "weights" 1 or larger
edge.Winter.1 <- edgeWinter

edge.Winter.1 <- edge.Winter.1 %>%
  filter(weight >= 1)

#same for adjusted 3 up-make it a data frame
graph.Winter.1 <- graph.data.frame(edge.Winter.1, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
g.Winter.1 <- as_adjacency_matrix(graph.Winter.1, type = "both", names = TRUE,
                                  sparse = FALSE, attr = "weight")

#make it an igraph object
g.Winter.1 <- graph.adjacency(g.Winter.1, mode= "undirected", weighted=TRUE, diag = FALSE)


##Number of Nodes
gorder(g.Winter.1)
#26

##Number of edges (regardless of weight)
gsize(g.Winter.1)
#243

##Degree:the number of nodes at distance 1
degreeWinter <- degree(g.Winter.1)
mean(degreeWinter)
#18.69231

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(g.Winter.1, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeWinter),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Winter.1") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Winter <- strength(g.Winter.1, vids = V(g.Winter.1), loops = FALSE)
#AVERAGE of Strength
mean(strength.Winter)
#153.6923

#see who has the highest strength
melt(strength.Winter)%>%
  arrange(desc(strength.Winter))
#KO_NET215   320

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessWinter <- closeness(g.Winter.1, vids = V(g.Winter.1), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessWinter)
#0.01052125

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Winter.1)
mean(betweenness(g.Winter.1))
# 17.84492

##Eigenvector Centrality
#EC a measure of the influence of a node
g.Winter.ec <- eigen_centrality(g.Winter.1, directed = FALSE)

###Average E.C. Score
mean(g.Winter.ec$vector)
#0.4727996

which.max(g.Winter.ec$vector)
#KO_NET215
#17


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Winter.1, directed = FALSE, unconnected = TRUE, weights = NA)
# 3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Winter.1, loops = FALSE)
#0.7476923

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Winter.1, type = "average")
#0.8909619


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Winter.1)
#0.866508

##Average Path Length
distances(g.Winter.1)
mean_distance(g.Winter.1)
#1.264615

###################################REMOVING ANY EDGE WITH <3 TIES################################################
#GLOBAL METRICS####

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Global.adj3 <- edge.global

Global.adj3 <- Global.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up-make it a data frame
Globgraph.adj3 <- graph.data.frame(Global.adj3, directed = FALSE)

#Same for adjusted 3 up-make it a matrix
Glob.Matrix.adj3 <- as_adjacency_matrix(Globgraph.adj3, type = "both", names = TRUE,
                                        sparse = FALSE, attr = "weight")

#make it an igraph object
Glob.Matrix.adj3 <- graph.adjacency(Glob.Matrix.adj3, mode= "undirected", weighted=TRUE, diag = FALSE)
V(Glob.Matrix.adj3)
summary(Glob.Matrix.adj3)
#This works for the final list but not the edgelist
V(Glob.Matrix.adj3)$Sex <- c("F","F","M","M","M","F","F","F","M","M","F","M","M","F","M","F","M",
                             "F","F","F","F","M","M","M","M","F","F")
summary(Glob.Matrix.adj3)


?geom_node_voronoi
?ggforce
##Number of Nodes
gorder(Glob.Matrix.adj3)
#27

##Number of edges (regardless of weight)
gsize(Glob.Matrix.adj3)
#254

##Degree:the number of nodes at distance 1
degreeGlobal.adj <- degree(Glob.Matrix.adj3)
mean(degreeGlobal.adj)
#18.81481

#plot
#Helpful website
#https://mr.schochastics.net/material/netVizR/
set.seed(666)
ggraph(Glob.Matrix.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeGlobal.adj,
                                                                               color = factor(Sex))) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("Glob.Matrix.adj3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Globaladj <- strength(Glob.Matrix.adj3, vids = V(Glob.Matrix.adj3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Globaladj)
#513.4074

#see who has the highest strength
melt(strength.Globaladj)%>%
  arrange(desc(strength.Globaladj))
#HN_NET115  1026

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessGlobal.adj <- closeness(Glob.Matrix.adj3, vids = V(Glob.Matrix.adj3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessGlobal.adj)
#0.00258051

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(Glob.Matrix.adj3)
mean(betweenness(Glob.Matrix.adj3))
#16.98765

##Eigenvector Centrality
#EC a measure of the influence of a node
gGlobaladj.ec <- eigen_centrality(Glob.Matrix.adj3, directed = FALSE)

###Average E.C. Score
mean(gGlobaladj.ec$vector)
#0.5025833

which.max(gGlobaladj.ec$vector)
#HN_NET115
#13


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(Glob.Matrix.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(Glob.Matrix.adj3, loops = FALSE)
#0.7236467

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(Glob.Matrix.adj3, type = "average")
#0.8882396


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(Glob.Matrix.adj3)
#0.8585426

##Average Path Length
distances(Glob.Matrix.adj3)
mean_distance(Glob.Matrix.adj3)
#2.036269






#YEAR METRICS
#2015 METRICS####
#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)

matrix2015 <- graph.adjacency(matrix2015, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
edge2015.adj3 <- edge2015

edge2015.adj3 <- edge2015.adj3 %>%
  filter(weight >= 3)

#dataframe
#same for adjusted 3 up
graph2015.adj3 <- graph.data.frame(edge2015.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix2015.adj3 <- as_adjacency_matrix(graph2015.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#write.csv(matrix2015.adj3, "2015 Adjusted FICR matrix.csv")

#convert
gmatrix2015.adj3 <- graph.adjacency(matrix2015.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

#number of nodes
gorder(gmatrix2015.adj3)
#13

#Number of edges
gsize(gmatrix2015.adj3)
#56

#Degree:the number of nodes at distance 1 (i.e. how many "one hop" connections each node has to other nodes)
degree15 <- degree(matrix2015)
degree15
melt(degree15) %>%
  arrange(desc(degree15))
hist(degree15, breaks= 30)
which.max(degree15)

degree15.adj <- degree(gmatrix2015.adj3)
mean(degree15.adj)
#8.615385

#Plot
set.seed(32)
ggraph(gmatrix2015.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree15.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("gmatrix2015.adj3") +
  theme(legend.position = "left")


#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2015 <- strength(matrix2015)
melt(strength.2015)%>%
  arrange(desc(strength.2015))

#78_NET214 is the highest with 86
strength2015 <- strength(gmatrix2015.adj3)
mean(strength(gmatrix2015.adj3))

strength.2015adj <- strength(gmatrix2015.adj3, vids = V(gmatrix2015.adj3), loops = FALSE)


melt(strength.2015adj)%>%
  arrange(desc(strength.2015adj))
#78_NET214 is the highest with 84

#mean of strength
mean(strength.2015adj)
#44.15385

#Sum of strengths
sum(strength.2015adj)
#574

#closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
?closeness
closeness15 <- closeness(gmatrix2015.adj3, vids = V(gmatrix2015.adj3), weights = NULL, normalized = FALSE)
mean(closeness15)
#0.01475689

#betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(gmatrix2015.adj3)
mean(betweenness(gmatrix2015.adj3))
#2.307692

# Identify key nodes in the FICR network with Eigenvector centrality:
#EC a measure of the influence of a node, connections to high-scoring nodes contribute more to
#the score of the node in question
#A high EV centrality score means that the node is connected to many nodes who themselves
#have high scores
g2015.ec <- eigen_centrality(gmatrix2015.adj3, directed = FALSE)
g2015.ec
which.max(g2015.ec$vector)
#78_NET214 = 2
mean(g2015.ec$vector)
#0.588774


#Diameter
#the length of the longest path (in number of edges) between 2 nodes
farthest_vertices(matrix2015)
#78_NET214 & ZW_NET115
diameter(matrix2015, directed = FALSE, unconnected = TRUE)
#4
#get_diameter says exactly what the 4 (from above) steps are
get_diameter(matrix2015)
#78 > KK > II > ZW

farthest_vertices(gmatrix2015.adj3)
#10

diameter(gmatrix2015.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#2
get_diameter(gmatrix2015.adj3)
#89 > 90 > II

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(matrix2015, loops=FALSE)
#0.1685606

edge_density(gmatrix2015.adj3, loops = FALSE)
#0.7179487

#Clustering Coefficient also called TRANSITIVITY
#Average Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them
#calculated for each vertex. Local transitivity (CC) of a vertex is the ratio of triangles
#connected to the vertex and the triples centered on it
transitivity(gmatrix2015.adj3, type = "average")
#0.9108503

#Global Clustering Coefficient (transitivity): the ratio of the triangles and the connected triplets of the graph
transitivity(gmatrix2015.adj3)
#0.8739669

#Opsahls Clustering in Weighted Networks#

#https://toreopsahl.com/tnet/weighted-networks/shortest-paths/
#make sure it confirms to tnet data standard for weighted-one mode network
tmatrix2015 <- as.tnet(matrix2015.adj3, type= "weighted one-mode tnet")

#Global clustering coefficient is assumed to be DIRECTED but it is symmatrized so we should be good
clustering_w(tmatrix2015, measure= c("bi", "am", "gm", "ma", "mi"))
#bi        am        gm        ma        mi
#0.8739669 0.8907168 0.8939508 0.8813953 0.9041031
#bi= binary, am= arithmetic mean, gm=geometric mean, mi-minimum measure, ma=maximum
#sosa 2019: "minumum variant should be preferred when trying to understand the mechanisms
#that shape link creation in animal societies as it helps determine the minimum threshold needed for
#closed triplets to occur

#Opsahls Local Clustering Coefficient
clustering_local_w(tmatrix2015, measure = c("bi", "am", "gm", "ma", "mi"))

#Average Path Length
mean_distance(gmatrix2015.adj3, directed = FALSE)
#1.282051


distances(gmatrix2015.adj3)

#2016 METRICS####

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
edge2016.adj3 <- edge2016

edge2016.adj3 <- edge2016.adj3 %>%
  filter(weight >= 3)

#Make it a dataframe so I can convert it to a matrix/igraph object
#same for adjusted 3 up
graph2016.adj3 <- graph.data.frame(edge2016.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix2016.adj3 <- as_adjacency_matrix(graph2016.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#write.csv(matrix2016.adj3, "2016 Adjusted FICR matrix.csv")

gmatrix2016.adj3 <- graph.adjacency(matrix2016.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(gmatrix2016.adj3)
#17

##Number of edges (regardless of weight)
gsize(gmatrix2016.adj3)
#105

##Degree:the number of nodes at distance 1
degree16.adj <- degree(gmatrix2016.adj3)
mean(degree16.adj)
#12.35294

set.seed(32)
ggraph(gmatrix2016.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree16.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("gmatrix2016.adj3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2016adj <- strength(gmatrix2016.adj3, vids = V(gmatrix2016.adj3), loops = FALSE)
#AVERAGE of Strength
mean(strength.2016adj)
#77.88235

#see who has the highest strength
melt(strength.2016adj)%>%
  arrange(desc(strength.2016adj))
#AU_NET115   161

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness16 <- closeness(gmatrix2016.adj3, vids = V(gmatrix2016.adj3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness16)
#0.01127635

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(gmatrix2016.adj3)
mean(betweenness(gmatrix2016.adj3))
#4.170588

##Eigenvector Centrality
#EC a measure of the influence of a node
g2016.ec <- eigen_centrality(gmatrix2016.adj3, directed = FALSE)

###Average E.C. Score
mean(g2016.ec$vector)
#0.5208766

which.max(g2016.ec$vector)
#AU_NET115
#4

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(gmatrix2016.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(gmatrix2016.adj3, loops = FALSE)
#0.7720588


##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(gmatrix2016.adj3, type = "average")
#0.8560737

##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(gmatrix2016.adj3)
#0.8277512

##Average Path Length
distances(gmatrix2016.adj3)
mean_distance(gmatrix2016.adj3)
#1.227941

#2017 METRICS####

matrix2017 <- graph.adjacency(matrix2017, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
edge2017.adj3 <- edge2017

edge2017.adj3 <- edge2017.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph2017.adj3 <- graph.data.frame(edge2017.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix2017.adj3 <- as_adjacency_matrix(graph2017.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#write.csv(matrix2017.adj3, "2017 Adjusted FICR matrix.csv")

gmatrix2017.adj3 <- graph.adjacency(matrix2017.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(gmatrix2017.adj3)
#21

##Number of edges (regardless of weight)
gsize(gmatrix2017.adj3)
#199

##Degree:the number of nodes at distance 1
degree17.adj <- degree(gmatrix2017.adj3)
mean(degree17.adj)
#18.95238

set.seed(32)
ggraph(gmatrix2017.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree17.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("gmatrix2017.adj3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2017adj <- strength(gmatrix2017.adj3, vids = V(gmatrix2017.adj3), loops = FALSE)
#AVERAGE of Strength
mean(strength.2017adj)
#362.7619

#see who has the highest strength
melt(strength.2017adj)%>%
  arrange(desc(strength.2017adj))
#IH_NET116   537

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness17 <- closeness(gmatrix2017.adj3, vids = V(gmatrix2017.adj3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness17)
#0.003650346

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(gmatrix2017.adj3)
mean(betweenness(gmatrix2017.adj3))
#6.507937

##Eigenvector Centrality
#EC a measure of the influence of a node
g2017.ec <- eigen_centrality(gmatrix2017.adj3, directed = FALSE)

###Average E.C. Score
mean(g2017.ec$vector)
#0.6942925

which.max(g2017.ec$vector)
#IH_NET116
#11


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(gmatrix2017.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(gmatrix2017.adj3, loops = FALSE)
#0.947619


##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(gmatrix2017.adj3, type = "average")
#0.9623851

##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(gmatrix2017.adj3)
#0.9592007

##Average Path Length
distances(gmatrix2017.adj3)
mean_distance(gmatrix2017.adj3)
#1.052381

#2018 METRICS####

matrix2018 <- graph.adjacency(matrix2018, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
edge2018.adj3 <- edge2018

edge2018.adj3 <- edge2018.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph2018.adj3 <- graph.data.frame(edge2018.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix2018.adj3 <- as_adjacency_matrix(graph2018.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#write.csv(matrix2018.adj3, "2018 Adjusted FICR matrix.csv")

gmatrix2018.adj3 <- graph.adjacency(matrix2018.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(gmatrix2018.adj3)
#16

##Number of edges (regardless of weight)
gsize(gmatrix2018.adj3)
#94

##Degree:the number of nodes at distance 1
degree18.adj <- degree(gmatrix2018.adj3)
mean(degree18.adj)
#11.75

set.seed(32)
ggraph(gmatrix2018.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree18.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("gmatrix2018.adj3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2018adj <- strength(gmatrix2018.adj3, vids = V(gmatrix2018.adj3), loops = FALSE)
#AVERAGE of Strength
mean(strength.2018adj)
#172.75

#see who has the highest strength
melt(strength.2018adj)%>%
  arrange(desc(strength.2018adj))
#ZM_SAND17   319

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness18 <- closeness(gmatrix2018.adj3, vids = V(gmatrix2018.adj3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness18)
#0.007226699

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(gmatrix2018.adj3)
mean(betweenness(gmatrix2018.adj3))
#5.65625

##Eigenvector Centrality
#EC a measure of the influence of a node
g2018.ec <- eigen_centrality(gmatrix2018.adj3, directed = FALSE)

###Average E.C. Score
mean(g2018.ec$vector)
#0.5765702

which.max(g2018.ec$vector)
#ZM_SAND17
#16

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(gmatrix2018.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(gmatrix2018.adj3, loops = FALSE)
#0.7833333

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(gmatrix2018.adj3, type = "average")
#0.8971109

##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(gmatrix2018.adj3)
#0.8651163

##Average Path Length
distances(gmatrix2018.adj3)
mean_distance(gmatrix2018.adj3)
#1.216667

#2019 METRICS####

matrix2019 <- graph.adjacency(matrix2019, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
edge2019.adj3 <- edge2019

edge2019.adj3 <- edge2019.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph2019.adj3 <- graph.data.frame(edge2019.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix2019.adj3 <- as_adjacency_matrix(graph2019.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#write.csv(matrix2019.adj3, "2019 Adjusted FICR matrix.csv")

gmatrix2019.adj3 <- graph.adjacency(matrix2019.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(gmatrix2019.adj3)
#12

##Number of edges (regardless of weight)
gsize(gmatrix2019.adj3)
#48

##Degree:the number of nodes at distance 1
degree19.adj <- degree(gmatrix2019.adj3)
mean(degree19.adj)
#8

set.seed(32)
ggraph(gmatrix2019.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree19.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("gmatrix2019.adj3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2019adj <- strength(gmatrix2019.adj3, vids = V(gmatrix2019.adj3), loops = FALSE)
#AVERAGE of Strength
mean(strength.2019adj)
#88.66667

#see who has the highest strength
melt(strength.2019adj)%>%
  arrange(desc(strength.2019adj))
#ZM_SAND17   159

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness19 <- closeness(gmatrix2019.adj3, vids = V(gmatrix2019.adj3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness19)
#0.01063355

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(gmatrix2019.adj3)
mean(betweenness(gmatrix2019.adj3))
#3.833333

##Eigenvector Centrality
#EC a measure of the influence of a node
g2019.ec <- eigen_centrality(gmatrix2019.adj3, directed = FALSE)

###Average E.C. Score
mean(g2019.ec$vector)
#0.5948407

which.max(g2019.ec$vector)
#ZM_SAND17
#10

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(gmatrix2019.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(gmatrix2019.adj3, loops = FALSE)
#0.7272727

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(gmatrix2019.adj3, type = "average")
#0.8894399


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(gmatrix2019.adj3)
#0.8469657

##Average Path Length
distances(gmatrix2019.adj3)
mean_distance(gmatrix2019.adj3)
#1.272727

#2020 METRICS####

matrix2020 <- graph.adjacency(matrix2020, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
edge2020.adj3 <- edge2020

edge2020.adj3 <- edge2020.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph2020.adj3 <- graph.data.frame(edge2020.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix2020.adj3 <- as_adjacency_matrix(graph2020.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

#write.csv(matrix2020.adj3, "2020 Adjusted FICR matrix.csv")

gmatrix2020.adj3 <- graph.adjacency(matrix2020.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(gmatrix2020.adj3)
#7

##Number of edges (regardless of weight)
gsize(gmatrix2020.adj3)
#18

##Degree:the number of nodes at distance 1
degree20.adj <- degree(gmatrix2020.adj3)
mean(degree20.adj)
#5.142857

set.seed(32)
ggraph(gmatrix2020.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degree20.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("gmatrix2020.adj3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.2020adj <- strength(gmatrix2020.adj3, vids = V(gmatrix2020.adj3), loops = FALSE)
#AVERAGE of Strength
mean(strength.2020adj)
#35.14286

#see who has the highest strength
melt(strength.2020adj)%>%
  arrange(desc(strength.2020adj))
#SY_NET217    47

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closeness20 <- closeness(gmatrix2020.adj3, vids = V(gmatrix2020.adj3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closeness20)
#0.02485645

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(gmatrix2020.adj3)
mean(betweenness(gmatrix2020.adj3))
#0.8571429

##Eigenvector Centrality
#EC a measure of the influence of a node
g2020.ec <- eigen_centrality(gmatrix2020.adj3, directed = FALSE)

###Average E.C. Score
mean(g2020.ec$vector)
#0.7880182

which.max(g2020.ec$vector)
#SY_NET217
#5

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(gmatrix2020.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(gmatrix2020.adj3, loops = FALSE)
#0.8571429

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(gmatrix2020.adj3, type = "average")
#0.9142857


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(gmatrix2020.adj3)
#0.8846154

##Average Path Length
distances(gmatrix2020.adj3)
mean_distance(gmatrix2020.adj3)
#1.142857


#MONTH METRICS##

#MONTLY METRICS

#JANUARY METRICS####

Jan.mat <- graph.adjacency(matrixJan, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Jan.edge.adj3 <- edgeJan

Jan.edge.adj3 <- Jan.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Jan.adj3 <- graph.data.frame(Jan.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Jan.adj3 <- as_adjacency_matrix(graph.Jan.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Jan.matrix.adj3 <- graph.adjacency(matrix.Jan.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Jan.matrix.adj3)
#12

##Number of edges (regardless of weight)
gsize(g.Jan.matrix.adj3)
#22

##Degree:the number of nodes at distance 1
degreeJan.adj <- degree(g.Jan.matrix.adj3)
mean(degreeJan.adj)
#3.666667

set.seed(32)
ggraph(g.Jan.matrix.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeJan.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Jan.matrix.adj3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Janadj <- strength(g.Jan.matrix.adj3, vids = V(g.Jan.matrix.adj3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Janadj)
#12.83333

#see who has the highest strength
melt(strength.Janadj)%>%
  arrange(desc(strength.Janadj))
#KO_NET215    21

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessJan <- closeness(g.Jan.matrix.adj3, vids = V(g.Jan.matrix.adj3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessJan)
#0.01004648

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Jan.matrix.adj3)
mean(betweenness(g.Jan.matrix.adj3))
#0

##Eigenvector Centrality
#EC a measure of the influence of a node
gJan.ec <- eigen_centrality(g.Jan.matrix.adj3, directed = FALSE)

###Average E.C. Score
mean(gJan.ec$vector)
#0.4525612

which.max(g.Jan.matrix.adj3$vector)
#integrer(0) --KO=1

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Jan.matrix.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#1

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Jan.matrix.adj3, loops = FALSE)
#0.3333333

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Jan.matrix.adj3, type = "average")
#1


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Jan.matrix.adj3)
#1

##Average Path Length
distances(g.Jan.matrix.adj3)
mean_distance(g.Jan.matrix.adj3)
#1
#FEBRUARY METRICS####
Feb.mat <- graph.adjacency(matrixFeb, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Feb.edge.adj3 <- edgeFeb

Feb.edge.adj3 <- Feb.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Feb.adj3 <- graph.data.frame(Feb.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Feb.adj3 <- as_adjacency_matrix(graph.Feb.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Feb.matrix.adj3 <- graph.adjacency(matrix.Feb.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Feb.matrix.adj3)
#15

##Number of edges (regardless of weight)
gsize(g.Feb.matrix.adj3)
#68

##Degree:the number of nodes at distance 1
degreeFeb.adj <- degree(g.Feb.matrix.adj3)
mean(degreeFeb.adj)
#9.066667

set.seed(32)
ggraph(g.Feb.matrix.adj3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeFeb.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Feb.matrix.adj3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Febadj <- strength(g.Feb.matrix.adj3, vids = V(g.Feb.matrix.adj3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Febadj)
#42

#see who has the highest strength
melt(strength.Febadj)%>%
  arrange(desc(strength.Febadj))
#HN_NET115    63

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessFeb <- closeness(g.Feb.matrix.adj3, vids = V(g.Feb.matrix.adj3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessFeb)
#0.01375823

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Feb.matrix.adj3)
mean(betweenness(g.Feb.matrix.adj3))
#3.144444

##Eigenvector Centrality
#EC a measure of the influence of a node
gFeb.ec <- eigen_centrality(g.Feb.matrix.adj3, directed = FALSE)

###Average E.C. Score
mean(gFeb.ec$vector)
#0.6329929

which.max(g.Feb.matrix.adj3$vector)
#integrer(0) --KO=1

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Feb.matrix.adj3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Feb.matrix.adj3, loops = FALSE)
#0.647619

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Feb.matrix.adj3, type = "average")
#0.8473201


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Feb.matrix.adj3)
#0.763285

##Average Path Length
distances(g.Feb.matrix.adj3)
mean_distance(g.Feb.matrix.adj3)
#1.352381

#MARCH METRICS####
Mar.mat <- graph.adjacency(matrixMar, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Mar.edge.adj3 <- edgeMar

Mar.edge.adj3 <- Mar.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Mar.adj3 <- graph.data.frame(Mar.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Mar.adj3 <- as_adjacency_matrix(graph.Mar.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Mar.matrix3 <- graph.adjacency(matrix.Mar.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Mar.matrix3)
#21

##Number of edges (regardless of weight)
gsize(g.Mar.matrix3)
#84

##Degree:the number of nodes at distance 1
degreeMar.adj <- degree(g.Mar.matrix3)
mean(degreeMar.adj)
#8

set.seed(32)
ggraph(g.Mar.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeMar.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Mar.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Maradj3 <- strength(g.Mar.matrix3, vids = V(g.Mar.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Maradj3)
#37.04762

#see who has the highest strength
melt(strength.Maradj3)%>%
  arrange(desc(strength.Maradj3))
#KO_NET215   109

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessMar <- closeness(g.Mar.matrix3, vids = V(g.Mar.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessMar)
#0.008406305

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Mar.matrix3)
mean(betweenness(g.Mar.matrix3))
#7.57619

##Eigenvector Centrality
#EC a measure of the influence of a node
gMar.ec <- eigen_centrality(g.Mar.matrix3, directed = FALSE)

###Average E.C. Score
mean(gMar.ec$vector)
#0.4043085

which.max(g.Mar.matrix3$vector)
#integrer(0) --KO=1

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Mar.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Mar.matrix3, loops = FALSE)
#0.4

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Mar.matrix3, type = "average")
#0.7648549


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Mar.matrix3)
#0.5752896

##Average Path Length
distances(g.Mar.matrix3)
mean_distance(g.Mar.matrix3)
#1.614286


#APRIL METRICS####
Apr.mat <- graph.adjacency(matrixApr, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Apr.edge.adj3 <- edgeApr

Apr.edge.adj3 <- Apr.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Apr.adj3 <- graph.data.frame(Apr.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Apr.adj3 <- as_adjacency_matrix(graph.Apr.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Apr.matrix3 <- graph.adjacency(matrix.Apr.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Apr.matrix3)
#13

##Number of edges (regardless of weight)
gsize(g.Apr.matrix3)
#23

##Degree:the number of nodes at distance 1
degreeApr.adj <- degree(g.Apr.matrix3)
mean(degreeApr.adj)
#3.538462

set.seed(32)
ggraph(g.Apr.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeApr.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Apr.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Apradj3 <- strength(g.Apr.matrix3, vids = V(g.Apr.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Apradj3)
#13.53846

#see who has the highest strength
melt(strength.Apradj3)%>%
  arrange(desc(strength.Apradj3))
#HN_NET115    34

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessApr <- closeness(g.Apr.matrix3, vids = V(g.Apr.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessApr)
#0.01155469

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Apr.matrix3)
mean(betweenness(g.Apr.matrix3))
#6.307692

##Eigenvector Centrality
#EC a measure of the influence of a node
gApr.ec <- eigen_centrality(g.Apr.matrix3, directed = FALSE)

###Average E.C. Score
mean(gApr.ec$vector)
#0.4234371

which.max(g.Apr.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Apr.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#4

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Apr.matrix3, loops = FALSE)
#0.2948718

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Apr.matrix3, type = "average")
#0.7534632


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Apr.matrix3)
#0.5294118

##Average Path Length
distances(g.Apr.matrix3)
mean_distance(g.Apr.matrix3)
#2.038462

#MAY METRICS####
May.mat <- graph.adjacency(matrixMay, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
May.edge.adj3 <- edgeMay

May.edge.adj3 <- May.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.May.adj3 <- graph.data.frame(May.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.May.adj3 <- as_adjacency_matrix(graph.May.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.May.matrix3 <- graph.adjacency(matrix.May.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.May.matrix3)
#9

##Number of edges (regardless of weight)
gsize(g.May.matrix3)
#12

##Degree:the number of nodes at distance 1
degreeMay.adj <- degree(g.May.matrix3)
mean(degreeMay.adj)
#2.666667

set.seed(32)
ggraph(g.May.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeMay.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.May.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Mayadj3 <- strength(g.May.matrix3, vids = V(g.May.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Mayadj3)
#9.111111

#see who has the highest strength
melt(strength.Mayadj3)%>%
  arrange(desc(strength.Mayadj3))
#ZM_SAND17    17

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessMay <- closeness(g.May.matrix3, vids = V(g.May.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessMay)
#0.0188492

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.May.matrix3)
mean(betweenness(g.May.matrix3))
#0.6666667

##Eigenvector Centrality
#EC a measure of the influence of a node
gMay.ec <- eigen_centrality(g.May.matrix3, directed = FALSE)

###Average E.C. Score
mean(gMay.ec$vector)
#0.4741028

which.max(g.May.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.May.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
# 2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.May.matrix3, loops = FALSE)
#0.3333333

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.May.matrix3, type = "average")
#0.8833333


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.May.matrix3)
#0.6923077

##Average Path Length
distances(g.May.matrix3)
mean_distance(g.May.matrix3)
#1.333333

#JUNE METRICS#--Only 1 set of paris >3####

Jun.mat <- graph.adjacency(matrixJun, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Jun.edge.adj3 <- edgeJun

Jun.edge.adj3 <- Jun.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Jun.adj3 <- graph.data.frame(Jun.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Jun.adj3 <- as_adjacency_matrix(graph.Jun.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Jun.matrix3 <- graph.adjacency(matrix.Jun.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Jun.matrix3)
#2

##Number of edges (regardless of weight)
gsize(g.Jun.matrix3)
#1

##Degree:the number of nodes at distance 1
degreeJun.adj <- degree(g.Jun.matrix3)
mean(degreeJun.adj)

set.seed(32)
ggraph(g.Jun.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeJun.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Jun.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Junadj3 <- strength(g.Jun.matrix3, vids = V(g.Jun.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Junadj3)


#see who has the highest strength
melt(strength.Junadj3)%>%
  arrange(desc(strength.Junadj3))


##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessJun <- closeness(g.Jun.matrix3, vids = V(g.Jun.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessJun)


##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Jun.matrix3)
mean(betweenness(g.Jun.matrix3))


##Eigenvector Centrality
#EC a measure of the influence of a node
gJun.ec <- eigen_centrality(g.Jun.matrix3, directed = FALSE)

###Average E.C. Score
mean(gJun.ec$vector)


which.max(g.Jun.matrix3$vector)


##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Jun.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)


#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Jun.matrix3, loops = FALSE)


##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Jun.matrix3, type = "average")



##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Jun.matrix3)


##Average Path Length
distances(g.Jun.matrix3)
mean_distance(g.Jun.matrix3)


#JULY METRICS####

Jul.mat <- graph.adjacency(matrixJul, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Jul.edge.adj3 <- edgeJul

Jul.edge.adj3 <- Jul.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Jul.adj3 <- graph.data.frame(Jul.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Jul.adj3 <- as_adjacency_matrix(graph.Jul.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Jul.matrix3 <- graph.adjacency(matrix.Jul.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Jul.matrix3)
#10

##Number of edges (regardless of weight)
gsize(g.Jul.matrix3)
#10

##Degree:the number of nodes at distance 1
degreeJul.adj <- degree(g.Jul.matrix3)
mean(degreeJul.adj)
#2

set.seed(32)
ggraph(g.Jul.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeJul.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Jul.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Jul <- strength(g.Jul.matrix3, vids = V(g.Jul.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Jul)
#7.4

#see who has the highest strength
melt(strength.Jul)%>%
  arrange(desc(strength.Jul))
#SY_NET217    19

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessJul <- closeness(g.Jul.matrix3, vids = V(g.Jul.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessJul)
#0.01387004

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Jul.matrix3)
mean(betweenness(g.Jul.matrix3))
#1.3

##Eigenvector Centrality
#EC a measure of the influence of a node
gJul.ec <- eigen_centrality(g.Jul.matrix3, directed = FALSE)

###Average E.C. Score
mean(gJul.ec$vector)
#0.3505717

which.max(g.Jul.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Jul.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Jul.matrix3, loops = FALSE)
#0.2222222

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Jul.matrix3, type = "average")
#0.4666667


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Jul.matrix3)
# 0.375

##Average Path Length
distances(g.Jul.matrix3)
mean_distance(g.Jul.matrix3)
#1.619048

#AUGUST METRICS####

Aug.mat <- graph.adjacency(matrixAug, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Aug.edge.adj3 <- edgeAug

Aug.edge.adj3 <- Aug.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Aug.adj3 <- graph.data.frame(Aug.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Aug.adj3 <- as_adjacency_matrix(graph.Aug.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Aug.matrix3 <- graph.adjacency(matrix.Aug.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Aug.matrix3)
#22

##Number of edges (regardless of weight)
gsize(g.Aug.matrix3)
#113

##Degree:the number of nodes at distance 1
degreeAug.adj <- degree(g.Aug.matrix3)
mean(degreeAug.adj)
#10.27273

set.seed(32)
ggraph(g.Aug.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeAug.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Aug.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Aug <- strength(g.Aug.matrix3, vids = V(g.Aug.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Aug)
#73.72727

#see who has the highest strength
melt(strength.Aug)%>%
  arrange(desc(strength.Aug))
#ZM_SAND17   155

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessAug <- closeness(g.Aug.matrix3, vids = V(g.Aug.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessAug)
#0.005295955

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Aug.matrix3)
mean(betweenness(g.Aug.matrix3))
#9.659091

##Eigenvector Centrality
#EC a measure of the influence of a node
gAug.ec <- eigen_centrality(g.Aug.matrix3, directed = FALSE)

###Average E.C. Score
mean(gAug.ec$vector)
#0.5124347

which.max(g.Aug.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Aug.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#4

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Aug.matrix3, loops = FALSE)
# 0.4891775

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Aug.matrix3, type = "average")
#0.8213611


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Aug.matrix3)
# 0.8179825

##Average Path Length
distances(g.Aug.matrix3)
mean_distance(g.Aug.matrix3)
#1.666667

#SEPTEMBER METRICS####
Sep.mat <- graph.adjacency(matrixSep, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Sep.edge.adj3 <- edgeSep

Sep.edge.adj3 <- Sep.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Sep.adj3 <- graph.data.frame(Sep.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Sep.adj3 <- as_adjacency_matrix(graph.Sep.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Sep.matrix3 <- graph.adjacency(matrix.Sep.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Sep.matrix3)
#21

##Number of edges (regardless of weight)
gsize(g.Sep.matrix3)
#123

##Degree:the number of nodes at distance 1
degreeSep.adj <- degree(g.Sep.matrix3)
mean(degreeSep.adj)
#11.71429

set.seed(32)
ggraph(g.Sep.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeSep.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Sep.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Sepadj <- strength(g.Sep.matrix3, vids = V(g.Sep.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Sepadj)
#72.38095

#see who has the highest strength
melt(strength.Sepadj)%>%
  arrange(desc(strength.Sepadj))
#ZM_SAND17   138

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessSep <- closeness(g.Sep.matrix3, vids = V(g.Sep.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessSep)
#0.007738312

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Sep.matrix3)
mean(betweenness(g.Sep.matrix3))
#6.527778

##Eigenvector Centrality
#EC a measure of the influence of a node
gSep.ec <- eigen_centrality(g.Sep.matrix3, directed = FALSE)

###Average E.C. Score
mean(gSep.ec$vector)
#0.5211402

which.max(g.Sep.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Sep.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Sep.matrix3, loops = FALSE)
#0.5857143

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Sep.matrix3, type = "average")
#0.8287004


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Sep.matrix3)
#0.770428

##Average Path Length
distances(g.Sep.matrix3)
mean_distance(g.Sep.matrix3)
#1.419048

#OCTOBER METRICS####
Oct.mat <- graph.adjacency(matrixOct, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Oct.edge.adj3 <- edgeOct

Oct.edge.adj3 <- Oct.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Oct.adj3 <- graph.data.frame(Oct.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Oct.adj3 <- as_adjacency_matrix(graph.Oct.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Oct.matrix3 <- graph.adjacency(matrix.Oct.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Oct.matrix3)
#25

##Number of edges (regardless of weight)
gsize(g.Oct.matrix3)
#192

##Degree:the number of nodes at distance 1
degreeOct.adj <- degree(g.Oct.matrix3)
mean(degreeOct.adj)
#15.36

set.seed(32)
ggraph(g.Oct.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeOct.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Oct.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Octadj <- strength(g.Oct.matrix3, vids = V(g.Oct.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Octadj)
#111.6

#see who has the highest strength
melt(strength.Octadj)%>%
  arrange(desc(strength.Octadj))
#90_NET214   217

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessOct <- closeness(g.Oct.matrix3, vids = V(g.Oct.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessOct)
#0.006035905

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Oct.matrix3)
mean(betweenness(g.Oct.matrix3))
#9.740381

##Eigenvector Centrality
#EC a measure of the influence of a node
gOct.ec <- eigen_centrality(g.Oct.matrix3, directed = FALSE)

###Average E.C. Score
mean(gOct.ec$vector)
#0.5179746

which.max(g.Oct.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Oct.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Oct.matrix3, loops = FALSE)
#0.64

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Oct.matrix3, type = "average")
#0.9136284


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Oct.matrix3)
#0.8868847

##Average Path Length
distances(g.Oct.matrix3)
mean_distance(g.Oct.matrix3)
#1.406667

#NOVEMBER METRICS####
Nov.mat <- graph.adjacency(matrixNov, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Nov.edge.adj3 <- edgeNov

Nov.edge.adj3 <- Nov.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Nov.adj3 <- graph.data.frame(Nov.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Nov.adj3 <- as_adjacency_matrix(graph.Nov.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Nov.matrix3 <- graph.adjacency(matrix.Nov.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Nov.matrix3)
#20

##Number of edges (regardless of weight)
gsize(g.Nov.matrix3)
#166

##Degree:the number of nodes at distance 1
degreeNov.adj <- degree(g.Nov.matrix3)
mean(degreeNov.adj)
# 16.6

set.seed(32)
ggraph(g.Nov.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNov.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Nov.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Novadj <- strength(g.Nov.matrix3, vids = V(g.Nov.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Novadj)
#122.7

#see who has the highest strength
melt(strength.Novadj)%>%
  arrange(desc(strength.Novadj))
#HN_NET115   202

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNov <- closeness(g.Nov.matrix3, vids = V(g.Nov.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNov)
#0.00736232

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Nov.matrix3)
mean(betweenness(g.Nov.matrix3))
#3.545238

##Eigenvector Centrality
#EC a measure of the influence of a node
gNov.ec <- eigen_centrality(g.Nov.matrix3, directed = FALSE)

###Average E.C. Score
mean(gNov.ec$vector)
#0.6289982

which.max(g.Nov.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Nov.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Nov.matrix3, loops = FALSE)
#0.8736842

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Nov.matrix3, type = "average")
#0.9624312


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Nov.matrix3)
#0.9614961

##Average Path Length
distances(g.Nov.matrix3)
mean_distance(g.Nov.matrix3)
#1.131579

#DECEMBER METRICS####

Dec.mat <- graph.adjacency(matrixDec, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Dec.edge.adj3 <- edgeDec

Dec.edge.adj3 <- Dec.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Dec.adj3 <- graph.data.frame(Dec.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Dec.adj3 <- as_adjacency_matrix(graph.Dec.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Dec.matrix3 <- graph.adjacency(matrix.Dec.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Dec.matrix3)
#23

##Number of edges (regardless of weight)
gsize(g.Dec.matrix3)
#153

##Degree:the number of nodes at distance 1
degreeDec.adj <- degree(g.Dec.matrix3)
mean(degreeDec.adj)
#13.30435

set.seed(32)
ggraph(g.Dec.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeDec.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Dec.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Decadj <- strength(g.Dec.matrix3, vids = V(g.Dec.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Decadj)
#12.83333

#see who has the highest strength
melt(strength.Decadj)%>%
  arrange(desc(strength.Decadj))
#90_NET214   138

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessDec <- closeness(g.Dec.matrix3, vids = V(g.Dec.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessDec)
#0.008089986

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Dec.matrix3)
mean(betweenness(g.Dec.matrix3))
#6.502174

##Eigenvector Centrality
#EC a measure of the influence of a node
gDec.ec <- eigen_centrality(g.Dec.matrix3, directed = FALSE)

###Average E.C. Score
mean(gDec.ec$vector)
#0.5292601

which.max(g.Dec.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Dec.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Dec.matrix3, loops = FALSE)
#0.6047431

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Dec.matrix3, type = "average")
#0.8554632


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Dec.matrix3)
#0.8163265

##Average Path Length
distances(g.Dec.matrix3)
mean_distance(g.Dec.matrix3)
#1.407115




#SEASON METRICS

#BREEDING SEASON####
Breeding.mat <- graph.adjacency(matrixBreeding, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Breeding.edge.adj3 <- edgeBreeding

Breeding.edge.adj3 <- Breeding.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Breeding.adj3 <- graph.data.frame(Breeding.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Breeding.adj3 <- as_adjacency_matrix(graph.Breeding.adj3, type = "both", names = TRUE,
                                       sparse = FALSE, attr = "weight")

g.Breeding.matrix3 <- graph.adjacency(matrix.Breeding.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Breeding.matrix3)
#23

##Number of edges (regardless of weight)
gsize(g.Breeding.matrix3)
#64

##Degree:the number of nodes at distance 1
degreeBreeding.adj <- degree(g.Breeding.matrix3)
mean(degreeBreeding.adj)
#5.565217

set.seed(32)
ggraph(g.Breeding.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeBreeding.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Breeding.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Breedingadj <- strength(g.Breeding.matrix3, vids = V(g.Breeding.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Breedingadj)
#27.56522

#see who has the highest strength
melt(strength.Breedingadj)%>%
  arrange(desc(strength.Breedingadj))
#HN_NET115    80

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessBreeding <- closeness(g.Breeding.matrix3, vids = V(g.Breeding.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessBreeding)
#0.00441979

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Breeding.matrix3)
mean(betweenness(g.Breeding.matrix3))
#9.524845

##Eigenvector Centrality
#EC a measure of the influence of a node
gBreeding.ec <- eigen_centrality(g.Breeding.matrix3, directed = FALSE)

###Average E.C. Score
mean(gBreeding.ec$vector)
#0.3571944

which.max(g.Breeding.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Breeding.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#5

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Breeding.matrix3, loops = FALSE)
#0.2529644

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Breeding.matrix3, type = "average")
#0.7709488


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Breeding.matrix3)
#0.6337079

##Average Path Length
distances(g.Breeding.matrix3)
mean_distance(g.Breeding.matrix3)
#2.036269

#NONBREEDING METRICS####
NonBreeding.mat <- graph.adjacency(matrixNonbreeding, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Nonbreeding.edge.adj3 <- edgeNonbreeding

Nonbreeding.edge.adj3 <- Nonbreeding.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Nonbreeding.adj3 <- graph.data.frame(Nonbreeding.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Nonbreeding.adj3 <- as_adjacency_matrix(graph.Nonbreeding.adj3, type = "both", names = TRUE,
                                            sparse = FALSE, attr = "weight")

g.NonBreed.matrix3 <- graph.adjacency(matrix.Nonbreeding.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.NonBreed.matrix3)
#27

##Number of edges (regardless of weight)
gsize(g.NonBreed.matrix3)
#248

##Degree:the number of nodes at distance 1
degreeNonBreed.adj <- degree(g.NonBreed.matrix3)
mean(degreeNonBreed.adj)
#18.37037

set.seed(32)
ggraph(g.NonBreed.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNonBreed.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.NonBreed.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.NonBreedadj <- strength(g.NonBreed.matrix3, vids = V(g.NonBreed.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.NonBreedadj)
#336.3704

#see who has the highest strength
melt(strength.NonBreedadj)%>%
  arrange(desc(strength.NonBreedadj))
#90_NET214   679

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNonBreed <- closeness(g.NonBreed.matrix3, vids = V(g.NonBreed.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNonBreed)
#0.00441979

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.NonBreed.matrix3)
mean(betweenness(g.NonBreed.matrix3))
#12.98148

##Eigenvector Centrality
#EC a measure of the influence of a node
gNonBreed.ec <- eigen_centrality(g.NonBreed.matrix3, directed = FALSE)

###Average E.C. Score
mean(gNonBreed.ec$vector)
#0.5166932

which.max(g.NonBreed.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.NonBreed.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.NonBreed.matrix3, loops = FALSE)
#0.7065527

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.NonBreed.matrix3, type = "average")
#0.8969004


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.NonBreed.matrix3)
#0.8608751

##Average Path Length
distances(g.NonBreed.matrix3)
mean_distance(g.NonBreed.matrix3)
#1.299145

#WINTER METRICS####

Winter.mat <- graph.adjacency(matrixWinter, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Winter.edge.adj3 <- edgeWinter

Winter.edge.adj3 <- Winter.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Winter.adj3 <- graph.data.frame(Winter.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Winter.adj3 <- as_adjacency_matrix(graph.Winter.adj3, type = "both", names = TRUE,
                                            sparse = FALSE, attr = "weight")

g.Winter.matrix3 <- graph.adjacency(matrix.Winter.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Winter.matrix3)
#24

##Number of edges (regardless of weight)
gsize(g.Winter.matrix3)
#210

##Degree:the number of nodes at distance 1
degreeWinter.adj <- degree(g.Winter.matrix3)
mean(degreeWinter.adj)
#17.5

set.seed(32)
ggraph(g.Winter.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeWinter.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Winter.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Winteradj <- strength(g.Winter.matrix3, vids = V(g.Winter.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Winteradj)
#162.75

#see who has the highest strength
melt(strength.Winteradj)%>%
  arrange(desc(strength.Winteradj))
#KO_NET215   318

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessWinter.adj <- closeness(g.Winter.matrix3, vids = V(g.Winter.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessWinter.adj)
#0.005923656

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Winter.matrix3)
mean(betweenness(g.Winter.matrix3))
#7.552083

##Eigenvector Centrality
#EC a measure of the influence of a node
gWinter.ec <- eigen_centrality(g.Winter.matrix3, directed = FALSE)

###Average E.C. Score
mean(gWinter.ec$vector)
#0.5065298

which.max(g.Winter.matrix3$vector)
#integrer(0)

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Winter.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Winter.matrix3, loops = FALSE)
#0.7608696

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Winter.matrix3, type = "average")
#0.9027907


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Winter.matrix3)
#0.8872

##Average Path Length
distances(g.Winter.matrix3)
mean_distance(g.Winter.matrix3)
#1.242754


#BREEDING SEASON METRICS (ADJUSTED) BY YEAR: 2015, 2016, 2017...####
#2015 Breeding Season--NO CONNECTIONS >=3####
Breed2015.mat <- graph.adjacency(matrixBreed2015, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Breed2015.edge.adj3 <- edgeBreed2015

Breed2015.edge.adj3 <- Breed2015.edge.adj3 %>%
  filter(weight >= 3)

#NO DATA AVAILABLE NO EDGES WITH 3 or MORE CONNECTIONS#

#2016 Breeding Season####
Breed2016.mat <- graph.adjacency(matrixBreed2016, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Breed2016.edge.adj3 <- edgeBreed2016

Breed2016.edge.adj3 <- Breed2016.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Breed2016.edge.adj3 <- graph.data.frame(Breed2016.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Breed2016.adj3 <- as_adjacency_matrix(graph.Breed2016.edge.adj3, type = "both", names = TRUE,
                                             sparse = FALSE, attr = "weight")

g.Breed2016.matrix3 <- graph.adjacency(matrix.Breed2016.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Breed2016.matrix3)
#5

##Number of edges (regardless of weight)
gsize(g.Breed2016.matrix3)
#5

##Degree:the number of nodes at distance 1
degreeBreed2016.adj <- degree(g.Breed2016.matrix3)
mean(degreeBreed2016.adj)
#2

set.seed(32)
ggraph(g.Breed2016.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeBreed2016.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Breed2016.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Breed2016adj <- strength(g.Breed2016.matrix3, vids = V(g.Breed2016.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Breed2016adj)
#7.6

#see who has the highest strength
melt(strength.Breed2016adj)%>%
  arrange(desc(strength.Breed2016adj))
#AU_NET115    16

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessBreed2016 <- closeness(g.Breed2016.matrix3, vids = V(g.Breed2016.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessBreed2016)
#0.04543122

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Breed2016.matrix3)
mean(betweenness(g.Breed2016.matrix3))
#1

##Eigenvector Centrality
#EC a measure of the influence of a node
gBreed2016.ec <- eigen_centrality(g.Breed2016.matrix3, directed = FALSE)

###Average E.C. Score
mean(gBreed2016.ec$vector)
# 0.627793

which.max(gBreed2016.ec$vector)
#AU_NET115
#1

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Breed2016.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Breed2016.matrix3, loops = FALSE)
#0.5

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Breed2016.matrix3, type = "average")
#0.7222222


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Breed2016.matrix3)
#0.375

##Average Path Length
distances(g.Breed2016.matrix3)
mean_distance(g.Breed2016.matrix3)
#1.5

#2017 Breeding Season####
Breed2017.mat <- graph.adjacency(matrixBreed2017, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Breed2017.edge.adj3 <- edgeBreed2017

Breed2017.edge.adj3 <- Breed2017.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Breed2017.edge.adj3 <- graph.data.frame(Breed2017.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Breed2017.adj3 <- as_adjacency_matrix(graph.Breed2017.edge.adj3, type = "both", names = TRUE,
                                             sparse = FALSE, attr = "weight")

g.Breed2017.matrix3 <- graph.adjacency(matrix.Breed2017.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Breed2017.matrix3)
#15

##Number of edges (regardless of weight)
gsize(g.Breed2017.matrix3)
#25

##Degree:the number of nodes at distance 1
degreeBreed2017.adj <- degree(g.Breed2017.matrix3)
mean(degreeBreed2017.adj)
#3.333333

set.seed(32)
ggraph(g.Breed2017.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeBreed2017.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Breed2017.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Breed2017adj <- strength(g.Breed2017.matrix3, vids = V(g.Breed2017.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Breed2017adj)
#13.86667

#see who has the highest strength
melt(strength.Breed2017adj)%>%
  arrange(desc(strength.Breed2017adj))
#HN_NET115    35

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessBreed2017 <- closeness(g.Breed2017.matrix3, vids = V(g.Breed2017.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessBreed2017)
#0.00695659

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Breed2017.matrix3)
mean(betweenness(g.Breed2017.matrix3))
#2.4

##Eigenvector Centrality
#EC a measure of the influence of a node
gBreed2017.ec <- eigen_centrality(g.Breed2017.matrix3, directed = FALSE)

###Average E.C. Score
mean(gBreed2017.ec$vector)
#0.3763815

which.max(gBreed2017.ec$vector)
#HN_NET115
#8

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Breed2017.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#3

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Breed2017.matrix3, loops = FALSE)
#0.2380952

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Breed2017.matrix3, type = "average")
#0.7883117


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Breed2017.matrix3)
#0.6741573

##Average Path Length
distances(g.Breed2017.matrix3)
mean_distance(g.Breed2017.matrix3)
#1.653061

#2018 Breeding Season####
Breed2018.mat <- graph.adjacency(matrixBreed2018, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Breed2018.edge.adj3 <- edgeBreed2018

Breed2018.edge.adj3 <- Breed2018.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Breed2018.edge.adj3 <- graph.data.frame(Breed2018.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Breed2018.adj3 <- as_adjacency_matrix(graph.Breed2018.edge.adj3, type = "both", names = TRUE,
                                             sparse = FALSE, attr = "weight")

g.Breed2018.matrix3 <- graph.adjacency(matrix.Breed2018.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Breed2018.matrix3)
#10

##Number of edges (regardless of weight)
gsize(g.Breed2018.matrix3)
#34

##Degree:the number of nodes at distance 1
degreeBreed2018.adj <- degree(g.Breed2018.matrix3)
mean(degreeBreed2018.adj)
#6.8

set.seed(32)
ggraph(g.Breed2018.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeBreed2018.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Breed2018.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Breed2018adj <- strength(g.Breed2018.matrix3, vids = V(g.Breed2018.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Breed2018adj)
#31.4

#see who has the highest strength
melt(strength.Breed2018adj)%>%
  arrange(desc(strength.Breed2018adj))
#WP_SAND17    52

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessBreed2018 <- closeness(g.Breed2018.matrix3, vids = V(g.Breed2018.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessBreed2018)
#0.0214766

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Breed2018.matrix3)
mean(betweenness(g.Breed2018.matrix3))
#1.5

##Eigenvector Centrality
#EC a measure of the influence of a node
gBreed2018.ec <- eigen_centrality(g.Breed2018.matrix3, directed = FALSE)

###Average E.C. Score
mean(gBreed2018.ec$vector)
#0.6595429

which.max(gBreed2018.ec$vector)
#WP_SAND17
# 9

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Breed2018.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Breed2018.matrix3, loops = FALSE)
#0.7555556

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Breed2018.matrix3, type = "average")
#0.8736508


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Breed2018.matrix3)
#0.8309859

##Average Path Length
distances(g.Breed2018.matrix3)
mean_distance(g.Breed2018.matrix3)
#1.244444

#2019 Breeding Season####
Breed2019.mat <- graph.adjacency(matrixBreed2019, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Breed2019.edge.adj3 <- edgeBreed2019

Breed2019.edge.adj3 <- Breed2019.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Breed2019.edge.adj3 <- graph.data.frame(Breed2019.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Breed2019.adj3 <- as_adjacency_matrix(graph.Breed2019.edge.adj3, type = "both", names = TRUE,
                                             sparse = FALSE, attr = "weight")

g.Breed2019.matrix3 <- graph.adjacency(matrix.Breed2019.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Breed2019.matrix3)
#3

##Number of edges (regardless of weight)
gsize(g.Breed2019.matrix3)
#3

##Degree:the number of nodes at distance 1
degreeBreed2019.adj <- degree(g.Breed2019.matrix3)
mean(degreeBreed2019.adj)
#2

set.seed(32)
ggraph(g.Breed2019.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeBreed2019.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Breed2019.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Breed2019adj <- strength(g.Breed2019.matrix3, vids = V(g.Breed2019.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Breed2019adj)
#8

#see who has the highest strength
melt(strength.Breed2019adj)%>%
  arrange(desc(strength.Breed2019adj))
#VZ_NET119     9

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessBreed2019 <- closeness(g.Breed2019.matrix3, vids = V(g.Breed2019.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessBreed2019)
#0.1263228

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Breed2019.matrix3)
mean(betweenness(g.Breed2019.matrix3))
#0

##Eigenvector Centrality
#EC a measure of the influence of a node
gBreed2019.ec <- eigen_centrality(g.Breed2019.matrix3, directed = FALSE)

###Average E.C. Score
mean(gBreed2019.ec$vector)
#0.92671

which.max(gBreed2019.ec$vector)
#VZ_NET119
#3

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Breed2019.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#1

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Breed2019.matrix3, loops = FALSE)
#1

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Breed2019.matrix3, type = "average")
#1


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Breed2019.matrix3)
#1

##Average Path Length
distances(g.Breed2019.matrix3)
mean_distance(g.Breed2019.matrix3)
#1

#2020 Breeding Season--NO CONNECTIONS >=3####
Breed2020.mat <- graph.adjacency(matrixBreed2020, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Breed2020.edge.adj3 <- edgeBreed2020

Breed2020.edge.adj3 <- Breed2020.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Breed2020.edge.adj3 <- graph.data.frame(Breed2020.edge.adj3, directed = FALSE)

#2015 Nonbreeding & Winter Season####
#NONBREEDING METRICS####
NonBreeding2015.mat <- graph.adjacency(matrixNonbreed2015, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Nonbreed2015.edge.adj3 <- edgeNonbreed2015

Nonbreed2015.edge.adj3 <- Nonbreed2015.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Nonbreed16.adj3 <- graph.data.frame(Nonbreed2015.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Nonbreed2015.adj3 <- as_adjacency_matrix(graph.Nonbreed16.adj3, type = "both", names = TRUE,
                                                sparse = FALSE, attr = "weight")

g.NonBreed2015.matrix3 <- graph.adjacency(matrix.Nonbreed2015.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.NonBreed2015.matrix3)
#11

##Number of edges (regardless of weight)
gsize(g.NonBreed2015.matrix3)
#48

##Degree:the number of nodes at distance 1
degreeNonBreed2015.adj <- degree(g.NonBreed2015.matrix3)
mean(degreeNonBreed2015.adj)
#8.727273

set.seed(32)
ggraph(g.NonBreed2015.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNonBreed2015.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.NonBreed2015.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.NonBreed2015adj <- strength(g.NonBreed2015.matrix3, vids = V(g.NonBreed2015.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.NonBreed2015adj)
#39.81818

#see who has the highest strength
melt(strength.NonBreed2015adj)%>%
  arrange(desc(strength.NonBreed2015adj))
#78_NET214    60

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNonBreed2015 <- closeness(g.NonBreed2015.matrix3, vids = V(g.NonBreed2015.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNonBreed2015)
#0.02141434

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.NonBreed2015.matrix3)
mean(betweenness(g.NonBreed2015.matrix3))
#0.9545455

##Eigenvector Centrality
#EC a measure of the influence of a node
gNonBreed2015.ec <- eigen_centrality(g.NonBreed2015.matrix3, directed = FALSE)

###Average E.C. Score
mean(gNonBreed2015.ec$vector)
#0.7049256

which.max(gNonBreed2015.ec$vector)
#78_NET214
#2

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.NonBreed2015.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.NonBreed2015.matrix3, loops = FALSE)
#0.8727273

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.NonBreed2015.matrix3, type = "average")
#0.8987734


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.NonBreed2015.matrix3)
# 0.8865435

##Average Path Length
distances(g.NonBreed2015.matrix3)
mean_distance(g.NonBreed2015.matrix3)
#1.127273

#WINTER METRICS####

Winter2015.mat <- graph.adjacency(matrixWinter2015, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Winter2015.edge.adj3 <- edgeWinter2015

Winter2015.edge.adj3 <- Winter2015.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Winter2015.adj3 <- graph.data.frame(Winter2015.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Winter2015.adj3 <- as_adjacency_matrix(graph.Winter2015.adj3, type = "both", names = TRUE,
                                              sparse = FALSE, attr = "weight")

g.Winter2015.matrix3 <- graph.adjacency(matrix.Winter2015.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Winter2015.matrix3)
#4

##Number of edges (regardless of weight)
gsize(g.Winter2015.matrix3)
#4

##Degree:the number of nodes at distance 1
degreeWinter2015.adj <- degree(g.Winter2015.matrix3)
mean(degreeWinter2015.adj)
#2

set.seed(32)
ggraph(g.Winter2015.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeWinter2015.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Winter2015.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Winter2015adj <- strength(g.Winter2015.matrix3, vids = V(g.Winter2015.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Winter2015adj)
# 6

#see who has the highest strength
melt(strength.Winter2015adj)%>%
  arrange(desc(strength.Winter2015adj))
#78_NET214     9

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessWinter2015.adj <- closeness(g.Winter2015.matrix3, vids = V(g.Winter2015.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessWinter2015.adj)
#0.08611111

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Winter2015.matrix3)
mean(betweenness(g.Winter2015.matrix3))
#0.5

##Eigenvector Centrality
#EC a measure of the influence of a node
gWinter2015.ec <- eigen_centrality(g.Winter2015.matrix3, directed = FALSE)

###Average E.C. Score
mean(gWinter2015.ec$vector)
#0.7925216

which.max(gWinter2015.ec$vector)
#78_NET214
#2

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Winter2015.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Winter2015.matrix3, loops = FALSE)
#0.6666667

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Winter2015.matrix3, type = "average")
#0.7777778


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Winter2015.matrix3)
#0.6

##Average Path Length
distances(g.Winter2015.matrix3)
mean_distance(g.Winter2015.matrix3)
#1.333333

#2016 Nonbreeding & Winter####
#NONBREEDING METRICS####
NonBreeding2016.mat <- graph.adjacency(matrixNonbreed2016, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Nonbreed2016.edge.adj3 <- edgeNonbreed2016

Nonbreed2016.edge.adj3 <- Nonbreed2016.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Nonbreed2016.adj3 <- graph.data.frame(Nonbreed2016.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Nonbreed2016.adj3 <- as_adjacency_matrix(graph.Nonbreed2016.adj3, type = "both", names = TRUE,
                                                sparse = FALSE, attr = "weight")

g.NonBreed2016.matrix3 <- graph.adjacency(matrix.Nonbreed2016.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.NonBreed2016.matrix3)
#16

##Number of edges (regardless of weight)
gsize(g.NonBreed2016.matrix3)
#89

##Degree:the number of nodes at distance 1
degreeNonBreed2016.adj <- degree(g.NonBreed2016.matrix3)
mean(degreeNonBreed2016.adj)
#11.125

set.seed(32)
ggraph(g.NonBreed2016.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNonBreed2016.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.NonBreed2016.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.NonBreed2016adj <- strength(g.NonBreed2016.matrix3, vids = V(g.NonBreed2016.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.NonBreed2016adj)
#52.125

#see who has the highest strength
melt(strength.NonBreed2016adj)%>%
  arrange(desc(strength.NonBreed2016adj))
#AU_NET115    95

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNonBreed2016 <- closeness(g.NonBreed2016.matrix3, vids = V(g.NonBreed2016.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNonBreed2016)
#0.01316716

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.NonBreed2016.matrix3)
mean(betweenness(g.NonBreed2016.matrix3))
#2.645833

##Eigenvector Centrality
#EC a measure of the influence of a node
gNonBreed2016.ec <- eigen_centrality(g.NonBreed2016.matrix3, directed = FALSE)

###Average E.C. Score
mean(gNonBreed2016.ec$vector)
#0.5940128

which.max(gNonBreed2016.ec$vector)
#AU_NET115
#4

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.NonBreed2016.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.NonBreed2016.matrix3, loops = FALSE)
#0.7416667

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.NonBreed2016.matrix3, type = "average")
#0.863234


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.NonBreed2016.matrix3)
#0.8224299

##Average Path Length
distances(g.NonBreed2016.matrix3)
mean_distance(g.NonBreed2016.matrix3)
#1.258333

#WINTER METRICS####

Winter2016.mat <- graph.adjacency(matrixWinter2016, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Winter2016.edge.adj3 <- edgeWinter2016

Winter2016.edge.adj3 <- Winter2016.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Winter2016.adj3 <- graph.data.frame(Winter2016.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Winter2016.adj3 <- as_adjacency_matrix(graph.Winter2016.adj3, type = "both", names = TRUE,
                                              sparse = FALSE, attr = "weight")

g.Winter2016.matrix3 <- graph.adjacency(matrix.Winter2016.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Winter2016.matrix3)
#11

##Number of edges (regardless of weight)
gsize(g.Winter2016.matrix3)
#41

##Degree:the number of nodes at distance 1
degreeWinter2016.adj <- degree(g.Winter2016.matrix3)
mean(degreeWinter2016.adj)
#7.454545

set.seed(32)
ggraph(g.Winter2016.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeWinter2016.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Winter2016.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Winter2016adj <- strength(g.Winter2016.matrix3, vids = V(g.Winter2016.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Winter2016adj)
#28.18182

#see who has the highest strength
melt(strength.Winter2016adj)%>%
  arrange(desc(strength.Winter2016adj))
#AU_NET115    45

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessWinter2016.adj <- closeness(g.Winter2016.matrix3, vids = V(g.Winter2016.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessWinter2016.adj)
#0.02278535

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Winter2016.matrix3)
mean(betweenness(g.Winter2016.matrix3))
#1.633333

##Eigenvector Centrality
#EC a measure of the influence of a node
gWinter2016.ec <- eigen_centrality(g.Winter2016.matrix3, directed = FALSE)

###Average E.C. Score
mean(gWinter2016.ec$vector)
# 0.6765314

which.max(gWinter2016.ec$vector)
#AU_NET115
#3

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Winter2016.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Winter2016.matrix3, loops = FALSE)
#0.7454545

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Winter2016.matrix3, type = "average")
#0.8704185


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Winter2016.matrix3)
#0.8286713

##Average Path Length
distances(g.Winter2016.matrix3)
mean_distance(g.Winter2016.matrix3)
#1.254545

#2017 Nonbreeding & Winter####

#NONBREEDING METRICS####
NonBreeding2017.mat <- graph.adjacency(matrixNonbreed2017, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Nonbreed2017.edge.adj3 <- edgeNonbreed2017

Nonbreed2017.edge.adj3 <- Nonbreed2017.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Nonbreed2017.adj3 <- graph.data.frame(Nonbreed2017.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Nonbreed2017.adj3 <- as_adjacency_matrix(graph.Nonbreed2017.adj3, type = "both", names = TRUE,
                                                sparse = FALSE, attr = "weight")

g.NonBreed2017.matrix3 <- graph.adjacency(matrix.Nonbreed2017.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.NonBreed2017.matrix3)
#20

##Number of edges (regardless of weight)
gsize(g.NonBreed2017.matrix3)
#183

##Degree:the number of nodes at distance 1
degreeNonBreed2017.adj <- degree(g.NonBreed2017.matrix3)
mean(degreeNonBreed2017.adj)
#18.3

set.seed(32)
ggraph(g.NonBreed2017.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNonBreed2017.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.NonBreed2017.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.NonBreed2017adj <- strength(g.NonBreed2017.matrix3, vids = V(g.NonBreed2017.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.NonBreed2017adj)
#260.9

#see who has the highest strength
melt(strength.NonBreed2017adj)%>%
  arrange(desc(strength.NonBreed2017adj))
#IH_NET116   369

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNonBreed2017 <- closeness(g.NonBreed2017.matrix3, vids = V(g.NonBreed2017.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNonBreed2017)
#0.005121644

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.NonBreed2017.matrix3)
mean(betweenness(g.NonBreed2017.matrix3))
#5.6

##Eigenvector Centrality
#EC a measure of the influence of a node
gNonBreed2017.ec <- eigen_centrality(g.NonBreed2017.matrix3, directed = FALSE)

###Average E.C. Score
mean(gNonBreed2017.ec$vector)
#0.7162622

which.max(gNonBreed2017.ec$vector)
#IH_NET116
#10

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.NonBreed2017.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.NonBreed2017.matrix3, loops = FALSE)
#0.9631579

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.NonBreed2017.matrix3, type = "average")
#0.9720846


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.NonBreed2017.matrix3)
# 0.9701727

##Average Path Length
distances(g.NonBreed2017.matrix3)
mean_distance(g.NonBreed2017.matrix3)
#1.036842

#WINTER METRICS####

Winter2017.mat <- graph.adjacency(matrixWinter2017, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Winter2017.edge.adj3 <- edgeWinter2017

Winter2017.edge.adj3 <- Winter2017.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Winter2017.adj3 <- graph.data.frame(Winter2017.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Winter2017.adj3 <- as_adjacency_matrix(graph.Winter2017.adj3, type = "both", names = TRUE,
                                              sparse = FALSE, attr = "weight")

g.Winter2017.matrix3 <- graph.adjacency(matrix.Winter2017.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Winter2017.matrix3)
#21

##Number of edges (regardless of weight)
gsize(g.Winter2017.matrix3)
#178

##Degree:the number of nodes at distance 1
degreeWinter2017.adj <- degree(g.Winter2017.matrix3)
mean(degreeWinter2017.adj)
#16.95238

set.seed(32)
ggraph(g.Winter2017.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeWinter2017.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Winter2017.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Winter2017adj <- strength(g.Winter2017.matrix3, vids = V(g.Winter2017.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Winter2017adj)
#95.2381

#see who has the highest strength
melt(strength.Winter2017adj)%>%
  arrange(desc(strength.Winter2017adj))
#KO_NET215   144

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessWinter2017.adj <- closeness(g.Winter2017.matrix3, vids = V(g.Winter2017.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessWinter2017.adj)
#0.00905058

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Winter2017.matrix3)
mean(betweenness(g.Winter2017.matrix3))
#3.965873

##Eigenvector Centrality
#EC a measure of the influence of a node
gWinter2017.ec <- eigen_centrality(g.Winter2017.matrix3, directed = FALSE)

###Average E.C. Score
mean(gWinter2017.ec$vector)
#0.6871366

which.max(gWinter2017.ec$vector)
#KO_NET215
#14

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Winter2017.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Winter2017.matrix3, loops = FALSE)
#0.847619

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Winter2017.matrix3, type = "average")
#0.9194435


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Winter2017.matrix3)
# 0.9025381

##Average Path Length
distances(g.Winter2017.matrix3)
mean_distance(g.Winter2017.matrix3)
#1.152381

#2018 Nonbreeding & Winter####
#NONBREEDING METRICS####
NonBreeding2018.mat <- graph.adjacency(matrixNonbreed2018, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Nonbreed2018.edge.adj3 <- edgeNonbreed2018

Nonbreed2018.edge.adj3 <- Nonbreed2018.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Nonbreed2018.adj3 <- graph.data.frame(Nonbreed2018.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Nonbreed2018.adj3 <- as_adjacency_matrix(graph.Nonbreed2018.adj3, type = "both", names = TRUE,
                                                sparse = FALSE, attr = "weight")

g.NonBreed2018.matrix3 <- graph.adjacency(matrix.Nonbreed2018.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.NonBreed2018.matrix3)
#12

##Number of edges (regardless of weight)
gsize(g.NonBreed2018.matrix3)
#66

##Degree:the number of nodes at distance 1
degreeNonBreed2018.adj <- degree(g.NonBreed2018.matrix3)
mean(degreeNonBreed2018.adj)
#11

set.seed(32)
ggraph(g.NonBreed2018.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNonBreed2018.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.NonBreed2018.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.NonBreed2018adj <- strength(g.NonBreed2018.matrix3, vids = V(g.NonBreed2018.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.NonBreed2018adj)
#158.3333

#see who has the highest strength
melt(strength.NonBreed2018adj)%>%
  arrange(desc(strength.NonBreed2018adj))
#SY_NET217   220

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNonBreed2018 <- closeness(g.NonBreed2018.matrix3, vids = V(g.NonBreed2018.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNonBreed2018)
#0.006832699

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.NonBreed2018.matrix3)
mean(betweenness(g.NonBreed2018.matrix3))
#1.347222

##Eigenvector Centrality
#EC a measure of the influence of a node
gNonBreed2018.ec <- eigen_centrality(g.NonBreed2018.matrix3, directed = FALSE)

###Average E.C. Score
mean(gNonBreed2018.ec$vector)
#0.744865

which.max(gNonBreed2018.ec$vector)
#SY_NET217
#10

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.NonBreed2018.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#1

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.NonBreed2018.matrix3, loops = FALSE)
#1

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.NonBreed2018.matrix3, type = "average")
#0.8969004


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.NonBreed2018.matrix3)
#0.8608751

##Average Path Length
distances(g.NonBreed2018.matrix3)
mean_distance(g.NonBreed2018.matrix3)
#1.299145

#WINTER METRICS####

Winter2018.mat <- graph.adjacency(matrixWinter2018, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Winter2018.edge.adj3 <- edgeWinter2018

Winter2018.edge.adj3 <- Winter2018.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Winter2018.adj3 <- graph.data.frame(Winter2018.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Winter2018.adj3 <- as_adjacency_matrix(graph.Winter2018.adj3, type = "both", names = TRUE,
                                              sparse = FALSE, attr = "weight")

g.Winter2018.matrix3 <- graph.adjacency(matrix.Winter2018.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Winter2018.matrix3)
#14

##Number of edges (regardless of weight)
gsize(g.Winter2018.matrix3)
#50

##Degree:the number of nodes at distance 1
degreeWinter2018.adj <- degree(g.Winter2018.matrix3)
mean(degreeWinter2018.adj)
#7.142857

set.seed(32)
ggraph(g.Winter2018.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeWinter2018.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Winter2018.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Winter2018adj <- strength(g.Winter2018.matrix3, vids = V(g.Winter2018.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Winter2018adj)
#30.57143

#see who has the highest strength
melt(strength.Winter2018adj)%>%
  arrange(desc(strength.Winter2018adj))
#ZM_SAND17    63

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessWinter2018.adj <- closeness(g.Winter2018.matrix3, vids = V(g.Winter2018.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessWinter2018.adj)
#0.01442778

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Winter2018.matrix3)
mean(betweenness(g.Winter2018.matrix3))
#3.67381

##Eigenvector Centrality
#EC a measure of the influence of a node
gWinter2018.ec <- eigen_centrality(g.Winter2018.matrix3, directed = FALSE)

###Average E.C. Score
mean(gWinter2018.ec$vector)
#0.5634307

which.max(gWinter2018.ec$vector)
#ZM_SAND17
#14

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Winter2018.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Winter2018.matrix3, loops = FALSE)
#0.5494505

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Winter2018.matrix3, type = "average")
#0.8135262


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Winter2018.matrix3)
#0.6929348

##Average Path Length
distances(g.Winter2018.matrix3)
mean_distance(g.Winter2018.matrix3)
# 1.450549

#2019 Nonbreeding & Winter####
#NONBREEDING METRICS####
NonBreeding2019.mat <- graph.adjacency(matrixNonbreed2019, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Nonbreed2019.edge.adj3 <- edgeNonbreed2019

Nonbreed2019.edge.adj3 <- Nonbreed2019.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Nonbreed2019.adj3 <- graph.data.frame(Nonbreed2019.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Nonbreed2019.adj3 <- as_adjacency_matrix(graph.Nonbreed2019.adj3, type = "both", names = TRUE,
                                                sparse = FALSE, attr = "weight")

g.NonBreed2019.matrix3 <- graph.adjacency(matrix.Nonbreed2019.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.NonBreed2019.matrix3)
#8

##Number of edges (regardless of weight)
gsize(g.NonBreed2019.matrix3)
# 28

##Degree:the number of nodes at distance 1
degreeNonBreed2019.adj <- degree(g.NonBreed2019.matrix3)
mean(degreeNonBreed2019.adj)
#7

set.seed(32)
ggraph(g.NonBreed2019.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeNonBreed2019.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.NonBreed2019.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.NonBreed2019adj <- strength(g.NonBreed2019.matrix3, vids = V(g.NonBreed2019.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.NonBreed2019adj)
#66.25

#see who has the highest strength
melt(strength.NonBreed2019adj)%>%
  arrange(desc(strength.NonBreed2019adj))
#UA_NET119    81

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessNonBreed2019 <- closeness(g.NonBreed2019.matrix3, vids = V(g.NonBreed2019.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessNonBreed2019)
#0.016476

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.NonBreed2019.matrix3)
mean(betweenness(g.NonBreed2019.matrix3))
#0.6875

##Eigenvector Centrality
#EC a measure of the influence of a node
gNonBreed2019.ec <- eigen_centrality(g.NonBreed2019.matrix3, directed = FALSE)

###Average E.C. Score
mean(gNonBreed2019.ec$vector)
#0.8385192

which.max(gNonBreed2019.ec$vector)
#UA_NET119
#6

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.NonBreed2019.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#1

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.NonBreed2019.matrix3, loops = FALSE)
#1

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.NonBreed2019.matrix3, type = "average")
#1


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.NonBreed2019.matrix3)
#1

##Average Path Length
distances(g.NonBreed2019.matrix3)
mean_distance(g.NonBreed2019.matrix3)
#1

#WINTER METRICS####

Winter2019.mat <- graph.adjacency(matrixWinter2019, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Winter2019.edge.adj3 <- edgeWinter2019

Winter2019.edge.adj3 <- Winter2019.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Winter2019.adj3 <- graph.data.frame(Winter2019.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Winter2019.adj3 <- as_adjacency_matrix(graph.Winter2019.adj3, type = "both", names = TRUE,
                                              sparse = FALSE, attr = "weight")

g.Winter2019.matrix3 <- graph.adjacency(matrix.Winter2019.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Winter2019.matrix3)
#10

##Number of edges (regardless of weight)
gsize(g.Winter2019.matrix3)
#35

##Degree:the number of nodes at distance 1
degreeWinter2019.adj <- degree(g.Winter2019.matrix3)
mean(degreeWinter2019.adj)
#7

set.seed(32)
ggraph(g.Winter2019.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeWinter2019.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Winter2019.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Winter2019adj <- strength(g.Winter2019.matrix3, vids = V(g.Winter2019.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Winter2019adj)
#45.8

#see who has the highest strength
melt(strength.Winter2019adj)%>%
  arrange(desc(strength.Winter2019adj))
#ZM_SAND17    75

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessWinter2019.adj <- closeness(g.Winter2019.matrix3, vids = V(g.Winter2019.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessWinter2019.adj)
#0.01708403

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Winter2019.matrix3)
mean(betweenness(g.Winter2019.matrix3))
#2.166667

##Eigenvector Centrality
#EC a measure of the influence of a node
gWinter2019.ec <- eigen_centrality(g.Winter2019.matrix3, directed = FALSE)

###Average E.C. Score
mean(gWinter2019.ec$vector)
# 0.6422334

which.max(gWinter2019.ec$vector)
#ZM_SAND17
#10

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Winter2019.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Winter2019.matrix3, loops = FALSE)
#0.7777778

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Winter2019.matrix3, type = "average")
#0.9294533


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Winter2019.matrix3)
#0.9141631

##Average Path Length
distances(g.Winter2019.matrix3)
mean_distance(g.Winter2019.matrix3)
# 1.222222

#2020 Nonbreeding & Winter####
#NONBREEDING METRICS--NO CONNECTIONS >=3####
NonBreeding2020.mat <- graph.adjacency(matrixNonbreed2020, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Nonbreed2020.edge.adj3 <- edgeNonbreed2020

Nonbreed2020.edge.adj3 <- Nonbreed2020.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Nonbreed2020.adj3 <- graph.data.frame(Nonbreed2020.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Nonbreed2020.adj3 <- as_adjacency_matrix(graph.Nonbreed2020.adj3, type = "both", names = TRUE,
                                                sparse = FALSE, attr = "weight")

#WINTER METRICS####

Winter2020.mat <- graph.adjacency(matrixWinter2020, mode= "undirected", weighted=TRUE, diag = FALSE)

#Select only values with "weights" 3 or larger (i.e. only individuals seen together 3 or more times)
Winter2020.edge.adj3 <- edgeWinter2020

Winter2020.edge.adj3 <- Winter2020.edge.adj3 %>%
  filter(weight >= 3)

#same for adjusted 3 up
graph.Winter2020.adj3 <- graph.data.frame(Winter2020.edge.adj3, directed = FALSE)

#Same for adjusted 3 up
matrix.Winter2020.adj3 <- as_adjacency_matrix(graph.Winter2020.adj3, type = "both", names = TRUE,
                                              sparse = FALSE, attr = "weight")

g.Winter2020.matrix3 <- graph.adjacency(matrix.Winter2020.adj3, mode = "undirected", weighted=TRUE, diag = FALSE)

##Number of Nodes
gorder(g.Winter2020.matrix3)
#7

##Number of edges (regardless of weight)
gsize(g.Winter2020.matrix3)
#18

##Degree:the number of nodes at distance 1
degreeWinter2020.adj <- degree(g.Winter2020.matrix3)
mean(degreeWinter2020.adj)
# 5.142857

set.seed(32)
ggraph(g.Winter2020.matrix3, layout = "nicely") +
  geom_edge_link(aes(alpha= weight), color = "darkblue") + geom_node_point(aes(size=degreeWinter2020.adj),
                                                                           fill= "darkslategrey",
                                                                           color = "red", shape =21) +
  geom_node_text(aes(label = name), fontface= "bold", size=4, repel = TRUE) +
  theme_void() + ggtitle("g.Winter2020.matrix3") +
  theme(legend.position = "left")

##Average Weighted Degree (average Strength)
#Strength: Sum of all of its weights
#adds up all the weights from one individual
strength.Winter2020adj <- strength(g.Winter2020.matrix3, vids = V(g.Winter2020.matrix3), loops = FALSE)
#AVERAGE of Strength
mean(strength.Winter2020adj)
#35.14286

#see who has the highest strength
melt(strength.Winter2020adj)%>%
  arrange(desc(strength.Winter2020adj))
#SY_NET217    47

##Closeness: how many steps are required to access every other node from a given node
#how long info takes to arrive
closenessWinter2020.adj <- closeness(g.Winter2020.matrix3, vids = V(g.Winter2020.matrix3), weights = NULL, normalized = FALSE)
#AVERAGE of Closeness
mean(closenessWinter2020.adj)
#0.02485645

##Betweenness: the number of times a node lies on the shortest bath between other nodes
#measure which nodes are "bridges"
betweenness(g.Winter2020.matrix3)
mean(betweenness(g.Winter2020.matrix3))
#0.8571429

##Eigenvector Centrality
#EC a measure of the influence of a node
gWinter2020.ec <- eigen_centrality(g.Winter2020.matrix3, directed = FALSE)

###Average E.C. Score
mean(gWinter2020.ec$vector)
#0.7880182

which.max(gWinter2020.ec$vector)
#SY_NET217
#5

##Network Diameter
#the length of the longest path (in number of edges) between 2 nodes
diameter(g.Winter2020.matrix3, directed = FALSE, unconnected = TRUE, weights = NA)
#2

#Density, how interconnected a network is//
#The proportion of all potential edges between nodes that actually exist in the network
edge_density(g.Winter2020.matrix3, loops = FALSE)
#0.8571429

##Average Local Clustering Coefficient
#Compute the local clustering coefficient of each node and then average all of them

transitivity(g.Winter2020.matrix3, type = "average")
#0.9142857


##Global Clustering Coefficient
#the ratio of the triangles and the connected triplets of the graph
transitivity(g.Winter2020.matrix3)
#0.8846154

##Average Path Length
distances(g.Winter2020.matrix3)
mean_distance(g.Winter2020.matrix3)
#1.142857
