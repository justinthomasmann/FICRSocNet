library(tidyverse)
library(dplyr)
library(asnipe)
library(DescTools)#calculate Entropy
library(sna)#calculate network metrics
theme_set(theme_classic())
?network_swap()

#group_maker1 uses two separate dataframes: ids (id1, id2) and obs (day1-4)

#converts our data in 'asnipe' format data
group_maker <- function(x){
  g1 <- ids$id1[obs[,x]> 0]
  g2 <- ids$id2[obs[,x]> 0]
  temp <- c(g1,g2)
  temp <- temp[!duplicated(temp)]
  paste(temp)
}

#Read in our data data
df <- read.csv("GlobalFicrSocNet.csv", sep = ",", header=TRUE, check.names = F)

#Replace NAs with 0
df[is.na(df)] <- 0

#Create separate dataframes for ids and observations
ids <- df[,1:3]
obs <- df[,4:ncol(df)]

#Number of unique ids in the dataset
df %>% summarise(n_unique = n_distinct(unlist(across(id1:id2)))) #33 birds

# it works
group_maker(4)


# lapply for the WIN!!
gbiList <- lapply(1:ncol(obs), group_maker)
gbiList

# rename date column names with day 1-4 (I don't believe this is necessary any more)
names(gbiList)[1:ncol(obs)] <- paste("day", seq(1:ncol(obs)), sep="")


#Create a "Groub-By-Individual" (GBI) matrix, used in asnipe and is how we permute individuals into different groups

#Generate group-by-individual, this is what we permute
ficr.gbi <- get_group_by_individual(gbiList, data_format = "groups")

#Create an association matrix from the GBI matrix and calculate "SRI" values

ficr.network <- get_network(ficr.gbi, data_format = "GBI", association_index = "SRI")

true.ent <- Entropy(ficr.network)
true.cv <- cv(ficr.network)

#####100000permutations#####

cv<-function(x){return(sd(x)/mean(x))}
cvs <- rep(NA,100)
ent <- rep(NA,100)

network_perm <- list(ficr.network, ficr.gbi)

# make a permutation (1 swap) to the GBI data

for (i in 1:100000) {
  network_perm <- network_swap(network_perm[[2]], swaps = 1, association_matrix = network_perm[[1]])
  cvs[i] <- cv(network_perm[[1]])
  ent[i] <- Entropy(network_perm[[1]], base =2)
}
cvs
ent

cvs.burned <- cvs[10001:length(cvs)]
ent.burned <- ent[10001:length(ent)]
plot.df <- data.frame(rep(NA,90000))
plot.df$cvs <- cvs.burned
plot.df$ent <- ent.burned

#histogram of permuted cv after 10,000 burn-ins
ggplot(data = plot.df)+
  geom_histogram(aes(x=cvs), bins = 300, fill = "white", color = "black")+
  xlim(0.8,1.1)+
  geom_vline(xintercept = true.cv, color = "red")

#histogram of permuted ent after 10,000 burn-ins
ggplot(data = plot.df)+
  geom_histogram(aes(x=ent), bins = 300, fill = "white", color = "black")+
  xlim(8.8,9.3)+
  geom_vline(xintercept = true.ent, color = "red")


# plot the results with the original network as a red dot
plot(cvs,pch=20,cex=0.5)
points(0,cv(ficr.network),cex=1,pch=20,col="red")

plot(ent,pch=20,cex=0.5)
points(0,Entropy(ficr.network),cex=1,pch=20,col="red")

hist(cvs, Bins= 0.0)
length(cvs)
?hist()
