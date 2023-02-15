library(tidyverse)
library(dplyr)
library(asnipe)
library(DescTools)#calculate Entropy
library(sna)#calculate network metrics

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


#######################################PERFORM DATA-STREAM PERMUTATIONS################################################################

#Permute our gbi 10000 times (individual A from Day 1 is swapped with Individual B from Day 2 = 1 permutation...run that 10000 times)


ficr.network_permuted <- network_permutation(ficr.gbi, data_format = "GBI",
                                              association_matrix = ficr.network, permutations = 100000,
                                              returns=1, association_index = "SRI")


#calculate weighted degree (number of times and individual associated with someone) for our REAL Network

ficr.deg_weighted <- degree(ficr.network, gmode = "graph", g = 1, ignore.eval = FALSE)
ficr.deg_weighted

#Calculate the degree for each one of the 10000 permuted networks

ficr.perm.deg_weighted <- degree(ficr.network_permuted, gmode = "graph", g=c(1:100000),
                                 ignore.eval = FALSE)

ficr.perm.deg_weighted

# Plot these results on a nice little histogram

#plot the distribution of weighted degrees generated by the permutation process
hist(colMeans(ficr.perm.deg_weighted), breaks = 100, main = paste("P=", sum(mean(ficr.deg_weighted)
                                       < colMeans(ficr.perm.deg_weighted))/ncol(ficr.perm.deg_weighted)),
                                        xlab = "Weighted Degree", ylab = "Probability")

abline(v=mean(ficr.deg_weighted), col = "red") #add a line for the mean of our real networks weighted degree value


####################################MOVING FORWARD########################################

#We need to calculate either the entropy or the CV of strengths for each of the permuted networks and see where our observed value falls
# Our real network should have a lower entropy than the permuted networks
# The CV of strengths of Associations should be higher in our real network than in the permuted networks

#Calculate the total CV of strengths
cv<-function(x){return(sd(x)/mean(x))}


#CV of strength in our real network
real.net.CV <- cv(ficr.network)
real.net.CV
#[1] 1.091865


#try calculating CV for each individual network in the matrix of permuted networks

perm.cv <- cv(ficr.network_permuted)
perm.cv
#0.849 (this does it to all of them) I think a for.loop is needed to loop it through all of them


#see SNA_functions_code in Gomes et al. 2020 (network structure)


#use 'Entropy()' function in DescTools
#Calculate the Entropy of our real network

real.Ent <- Entropy(ficr.network, base =2)
real.Ent
#[1] 8.862964

# Try on our permuted network

perm.Ent <- Entropy(ficr.network_permuted, base = 2)
perm.Ent
#[1] 25.82692

#Maybe build a for loop, that does 1 permutation, counts the entropy, then does a second, counts entropy...etc. x 10000
#I'm not sure how we can get it to do Entropy individually for each as it seems like the function is treating the permuted network
#as the final product of 100k permutations (as opposed to calculating it after each one)

#Review "simulations_code" Line 110 from Gomes et al. "Network Structure" 2020


#################################################### I HAVE NOT TESTED THIS (CURRENT TIME: 11:39 PM)############################################
######################################### THE FOLLOWING LINES ARE FROM THE ABOVE PAPER AND HOW THEY MEASURED ENT/CV ON PERMS ###################

# permute the data to obtain the expected values of pairwise co-occurrences under the hypothesis of random associations

datastream_randomizations <- network_permutation(ficr.gbi, data_format = "GBI", permutations = 100000, returns = 1, association_index = "SRI",
                                                 association_matrix = ficr.network, identities = colnames(ficr.gbi))

#Justin restricted second argument in seq. Original was datastream_randomizations[seq(100, 100000, by=100),,]
datastream_randomizations<-datastream_randomizations[seq(100, 1000, by=100),,] # select the permutations every 100 swaps

gc() #calls garbage collector: useful after a large object has been removed--gives breakdown of memory usage

# mean of the corresponding pairwise values obtained from randomly generated networks
expected_values<-apply(datastream_randomizations, c(2,3), mean)
colnames(expected_values)<-colnames(ficr.gbi)
rownames(expected_values)<-colnames(ficr.gbi)


# register results in a matrix
options(scipen=999)
true_results<-rbind(real.net.CV, real.Ent)

#Left off on line 140 of "simulations_code.R"

# randomly pick a set of K group observations
# creating random subsets of the gbi_matrix with k social groupings

random_results<-matrix() # create matrix to register results from this replicate


temp_random_results<-data.frame() # create a matrix to register temporary results


  for (y in 1:length(datastream_randomizations)){ # for each replicate of the sub-sampling process

    # select random social groupings
    groups_selected<-sample(1:nrow(ficr.gbi), replace=F) # without replacement
    gbi_random<-ficr.gbi[c(groups_selected),]

    # gbi_random<-gbi_random[, colSums(ficr.gbi) !=0] # remove individuals that were never selected in these selected clusters

    # create the new network according to the randomly selected social groupings
    network_random<-get_network(gbi_random, data_format = "GBI", association_index = "SRI")

    # calculate the new statistics for each randomly selected social grouping
    # cv
    random_cv<-cv(network_random)

    # entropy
    random_entropy<-Entropy(network_random, base=2)

    # register temporary results in the matrix
    options(scipen=999)
    temp_random_results<-rbind(temp_random_results, cbind(random_cv, random_entropy))

  }
