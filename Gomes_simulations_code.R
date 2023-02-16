######################################################################
# SUPPORTING INFORMATION - CODE
#
### These are the scripts for the simulation function and social network analyses in Gomes et al. (DOI: ----), organized in the following parts:
### Notes:
#### No data needed
#### Please read the methods section of our article to guide you through this code.
#### This is simple R code, made for running the simulations in our article, and there are alternative or more condensed ways of coding the same simulations.
#### R version 3.5.3
#### R packages 'asnipe' and 'DescTools'must be installed
#### Created by Ana Cristina R. Gomes
#
######################################################################
#
#
#

sim_net_stat_robustness<-function(N=40, k_groups=c(10000, 5000, 2500, 1200, 600, 300, 150, 70, 35), social_groupings=20000, network_structure="more", replicates=10, I=10, D=10){

  # This function corresponds to the simulations described in the materials and methods section "Simulated networks and robustness of descriptive statistics"
  # To run each type of simulation (more structured or less structured networks), the argument 'network_structure' of the function should be chosen ('more' or 'less', respectively)
  #
  # ARGUMENTS.
  # N, number of individuals in the population, numerical (default = 40)
  # k_groups, number of random set of K group observations that will be selected from the total number of 'social_groupings', numerical (default = c(10000, 5000, 2500, 1200, 600, 300, 150, 70, 35), it accepts numerical vectors)
  # social_groupings, number of observations of social groupings initially generated, numerical (default = 20000)
  # network_structure, select the amount of network structure. Options are "more" or "less" (default = "more")
  # replicates, number of replicates to run, numerical (default = 10)
  # I, mean of the size (i) for each simulated group observation, numerical (default = 10)
  # D, standard deviation of the size (I) for each simulated group observation, numerical (default = 10)
  #
  # VALUES. it returns:
  # true_results, list of results with values for the "true" estatistics for the network calculate
  # random_results, list of results with values for the estatistics calculated from the k group observations selected

  # create lists to register results
  random_results_list<-list()
  true_results_list<-list()

  # create the function to calculate the coefficient of variation (CV)
  cv<-function(x){return(sd(x)/mean(x))}


  for (w in 1:replicates){ # repeat everything 'replicates' times

    # create the generative matrix, initially with all values = 1
    table_T<-matrix(1, nrow = N, ncol = N, dimnames = list(c(paste("I",1:N)), c(paste("I",1:N))))
    table_T<-as.matrix(as.dist(table_T)) # make the matrix symmetrical

    # create the gbi matrix to register individuals present in each social grouping
    gbi_matrix<-matrix(0, nrow = social_groupings, ncol = N, dimnames = list(c(1:social_groupings), c(colnames(table_T))))


    for(z in 1:social_groupings){ # for each social grouping

      repeat{
        x<-round(rnorm(1, mean = I, sd = D), 0) # define the number of individuals in this social grouping

        if (x>=1 && x<=N){break} # repeats the above code until the number of individuals in the social grouping is higher than 1 or smaller than N
      }

      # chose randomly the first individual to enter the social grouping
      first_ind<-sample(1:N, size=1)

      group<-as.vector(paste("I", first_ind)) # begin the social grouping with the first individual


      for (t in 2:x){ # for each number of individuals that it is needed in the social grouping

        inds_group<-vector(mode="numeric") # create a vector to include the possible individuals to enter the social grouping

        for(r in 1:length(group)){ # for each individual already in the social grouping
          inds_group<-c(inds_group, table_T[group[r],]) # join all the "lottery tickets" of all the individuals that can enter the social grouping
        }

        inds_group<-inds_group[!names(inds_group) %in% c(group)] # exclude individuals already in the social grouping

        if(network_structure=="more"){inds_group<-inds_group^2} # define the amount of structure

        sum_group<-sum(inds_group) # sum all the "lottery ticket" values of all the individuals that can enter the social grouping

        random_number<-sample(1:sum_group, size=1) # extract a random number that will represent the "lottery ticket" selected

        inds_cumulative<-cumsum(inds_group) # make the cumulative sum of all the individuals "lottery ticket" values

        ind_entering<-names(inds_cumulative)[inds_cumulative>=random_number] # extract all the individuals that have their cumulative "lottery ticket" sum equal or higher than the "lottery ticket" selected
        ind_entering<-ind_entering[1] # the individual that enters is the first of this list

        # update of the generative matrix, summing 1 to the matrix cells shared between each individual outside the group and each of the individuals already inside the group
        table_T[ind_entering,group]<-table_T[ind_entering,group]+1
        table_T[group, ind_entering]<-table_T[group, ind_entering]+1

        # add the new individual to the social grouping
        group<-c(group, ind_entering)

      }

      # replace in the gbi matrix all the individuals present in each social grouping by 1
      gbi_matrix[z, colnames(gbi_matrix) %in% group]<-1

    }

    print("True social groupings created")

    # create a social network through the gbi matrix obtained
    library(asnipe) # version 1.1.11
    network<-get_network(gbi_matrix, data_format = "GBI", association_index = "SRI")


    # Calculate the "true" estatistics for the network

    # cv
    true_cv<-cv(network)

    # SD
    true_sd<-sd(network)

    # S (Bejder, flercher & Bräger 1998)
    randomizations<-network_permutation(gbi_matrix, data_format = "GBI", permutations = 100000, returns = 1, association_index = "SRI", association_matrix = network, identities = colnames(gbi_matrix)) # permute the data to obtain the expected values of pairwise co-occurrences under the hypothesis of random associations
    randomizations<-randomizations[seq(100, 100000, by=100),,] # select the permutations every 100 swaps
    gc()

    # mean of the corresponding pairwise values obtained from randomly generated networks
    expected_values<-apply(randomizations, c(2,3), mean)
    colnames(expected_values)<-colnames(gbi_matrix)
    rownames(expected_values)<-colnames(gbi_matrix)

    # calculate S
    options(scipen=999)
    a<-network-expected_values
    a<-a^2
    num_dyads<-ncol(network)*(ncol(network)-1)
    a<-a/num_dyads
    true_S<-sum(a)

    # entropy
    library(DescTools) # version 0.99.28
    true_entropy<-Entropy(network, base=2)

    # register results in a matrix
    options(scipen=999)
    true_results<-rbind(true_sd, true_cv, true_S, true_entropy)

    # register results of this replicate in the list of results
    true_results_list[[w]]<-true_results


    # randomly pick a set of K group observations
    # creating random subsets of the gbi_matrix with k social groupings

    random_results<-matrix() # create matrix to register results from this replicate


    for (z in 1:length(k_groups)){ # for each number of K group observations defined in the function argument 'k_groups'

      temp_random_results<-data.frame() # create a matrix to register temporary results

      for (y in 1:100){ # for each replicate of the sub-sampling processs

        # select random social groupings
        groups_selected<-sample(1:social_groupings, size=k_groups[z], replace=F) # without replacement
        gbi_random<-gbi_matrix[c(groups_selected),]

        gbi_random<-gbi_random[, colSums(gbi_random) !=0] # remove individuals that were never selected in these selected clusters

        # create the new network according to the randomly selected social groupings
        network_random<-get_network(gbi_random, data_format = "GBI", association_index = "SRI")

        # calculate the new statistics for each randomly selected social grouping
        # cv
        random_cv<-cv(network_random)

        # SD
        random_sd<-sd(network_random)

        # S (Bejder, flercher & Bräger 1998)
        # reduce expected values matrix to the same individuals present in the gbi random
        expected_values_random<-expected_values[rownames(expected_values) %in% colnames(gbi_random), colnames(expected_values) %in% colnames(gbi_random)]

        options(scipen=999)
        a<-network_random-expected_values_random
        a<-a^2
        num_dyads<-ncol(network_random)*(ncol(network_random)-1)
        a<-a/num_dyads
        random_S<-sum(a)

        # entropy
        random_entropy<-Entropy(network_random, base=2)

        # register temporary results in the matrix
        options(scipen=999)
        temp_random_results<-rbind(temp_random_results, cbind(random_sd, random_cv, random_S, random_entropy))

      }

      # add to column names the reference of K group observations selected
      colnames(temp_random_results)<-c(paste(k_groups[z], colnames(temp_random_results), sep="_"))

      # join the temporary results to the result matrix
      random_results<-cbind(random_results, temp_random_results)

      print(paste(k_groups[z], " group observations ran"))
      gc()
    }

    # remove first column (null column)
    random_results<-random_results[,-1]

    # join result matrix to final list of results
    random_results_list[[w]]<-random_results
    print(paste(w, "_replicate_done"))
  }

  return(list(true_results = true_results_list, random_results=random_results_list))
}
