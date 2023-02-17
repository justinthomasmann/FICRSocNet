######################################################################
# SUPPORTING INFORMATION - R FUNCTIONs FILE (SCRIPT WITH FUNCTION TO RUN SNA OF THE MAIN TEXT)
#
#   These are the R scripts of the functions adapted and developed to run SNA present in
# 'Network structure and the optimisation of proximity-based association criteria'
# (Gomes et al. Methods in Ecology and Evolution), organized in the following parts:
#
### MAKE NETWORK BASED ON INDIVIDUALS' CO-OCCURRENCE -- COUNTS OF CO-OCCURRENCES
### MAKE NETWORK BASED ON INDIVIDUALS' TIME OVERLAP WITH TEMPORAL AND SPATIAL OPTIMIZATION -- TIME OVERLAP 
### STRUCTURE-BASED AND EXTERNAL OPTIMISATION OF ASSOCIATION CRITERIA - DESCRIPTIVE STATISTICS CALCULATION
### NETWORK RANDOMIZATIONS: CHECK SUGNIFICANCE
#
#   Notes:
#    All codes of this script file must be ran in R before runing 'SNA_script' file 
#    Please read the section "Structure-based and external optimisation of association criteria" in
# the methods of our article to guide you through this code.
#    R version 3.6.1
#    'dplyr', 'asnipe', 'data.table', 'intervals', 'DescTools', 'assortnet' packages required
#    Created by Ana Cristina R. Gomes with input from remaining authors
#
######################################################################




##### MAKE NETWORK BASED ON INDIVIDUALS' CO-OCCURRENCE -- COUNTS OF CO-OCCURRENCES #####


# function gmm_adapted made to:
## create a data frame with one line per minute, from the raw data
##
## make FIRST GMM:
#### create files to concatenate group-by-individuals (GBI), events and observations from the first GMM
###### which was ran per day due to computational memory limitations
#### Subset the per minute data, per day
#### Run GMM in each subsetted data
##
## make SECOND GMM: GMM inside each gathering event identified by the first GMM
#### Creates files to concatenate GBI, events and observations from the second GMM
#### Subset data according to each gathering events defined in the first GMM
#### Run GMM in each data subsetted
#### If the data of the subset has only one row, only the same individual or occurs only in the same seconds,
###### GMM does not run, and so copies the files from the first GMM


gmm_adapted <- function (data) {
  #
  #
  # This function uses the GMM function from 'asnipe' R package to apply the method described in Ferreira et al. (2019, DOI: 10.1101/739789)
  ## allows posterious calculation of an association matrix based counts of individuals' co-occurrence
  #
  # ARGUMENTS.
  ### "data" is the raw cleaned data file with the following columns:
  ##### "Date", with date (day-month-year) information
  ##### "Reader", with RFID reader ID
  ##### "Tag", with individuals ID
  ##### "Start", with the time, in seconds, the individual arrived to the RFID reader
  ##### "End", with the time, in seconds, the individual left the RFID reader
  #
  # VALUES. it returns:
  ### data_with_minutes, is the raw cleaned data with two extra columns with the occurrence times, in minutes
  ### data_1min, is the raw cleaned data converted to 1 row per minute of occurrence, that the GMM uses
  ### gbi_1, is the group-by-individuals (GBI) matrix obtained from the first GMM
  ### events_1, is the information about the gathering events, recognized by the first GMM
  ### observations_1, is the information about the number of observations of each individual in each gathering event, obtained from the first GMM
  ### gbi_final, is the GBI matrix obtained from the second GMM
  ### events_final, is the information about the gathering events, recognized by the second GMM
  ### observations_final, is the information about the number of observations of each individual in each gathering event, obtained from the second GMM
  
  
  library(dplyr) # version 0.8.3
  library (asnipe) # version 1.1.11
  
  # register the different individuals ID's present in the data
  tags=unique(as.vector(data$Tag))
  
  # To run GMM we need to calulate the minute each individual occurrence started and ended,
  ### which will be calculated from the seconds information
  data$minutes_Start<-as.integer(data$Start/60)
  data$minutes_End<-as.integer(data$End/60)
  
  # Notes:
  ## do not round minutes, because it will round by default and not considering the true minute where the event occurs
  ## converting to integer, it ignores the decimal part of the number
  
  
  
  ### CONVERT DATA TO 1 ROW PER MINUTE ###
  #
  # takes the raw cleaned data, and converts it to data with 1 row per minute to be used in GMM
  
  data_1min<-data.frame() # create an empty data frame
  
  # for all the rows in data
  for(i in 1:nrow(data)){
    # create a temporary data frame identical to data, but empty, and with as many rows as minutes that each occurrence lasted
    temp<-setNames(data.frame(matrix(ncol = ncol(data), nrow = data$minutes_End[i] - data$minutes_Start[i] + 1)), c(colnames(data)))
    # creates a column with the minute of the occurrence, and makes a sequence of minutes corresponding to the total time of the occurrence
    temp$minutes<-seq(data$minutes_Start[i], data$minutes_End[i], length.out = data$minutes_End[i] - data$minutes_Start[i] + 1)
    temp[,1:ncol(data)]<-data[i, 1:ncol(data)] # copy the remaining information of the data
    data_1min<-rbind(data_1min, temp) # concatenate this temporary data frame to the data frame with 1 minute per row
    print(paste(i, "data_to_min", sep="--"))} # print row number, to check the evolution of the run
  
  # sort data frame by minutes
  data_1min<-data_1min[order(data_1min[,"minutes"]),]
  
  # remove rows with the exactly same information (which is redundant)
  data_1min<-distinct(data_1min, Tag, minutes, Date)
  
  
  
  ### FIRST GMM ###
  #
  # prepare files to register all objects created from the GMM (gbi, events and observations), per day
  gbi_1<-data.frame()
  events_1<-data.frame()
  observations_1<-data.frame()
  
  # define the days present in the data
  days<-unique(data_1min$Date)
  
  
  for(i in 1:length(days)) { # for each day of the data
    
    # subset data according to the day
    temp<-data_1min[data_1min$Date == days[i],]
    
    # GMM run
    gmm_temp<-gmmevents(time=temp$minutes, identity = temp$Tag, location = temp$Date, global_ids = tags)
    
    # concatenate GMM results for all subsetted data
    gbi_1<-rbind(gbi_1, gmm_temp$gbi)
    events_1<-rbind(events_1, gmm_temp$metadata)
    observations_1<-rbind(observations_1, gmm_temp$B)
    print(paste(days[i], "first_GMM", sep="--")) # print day ran, to check the evolution of the run
  }
  
  # define results as matrix, except for the 'event' data frame
  gbi_1<-as.matrix(gbi_1)
  observations_1<-as.matrix(observations_1)
  
  # define correct formats for the information in the object 'event'
  events_1$Start<-as.integer(events_1$Start)
  events_1$End<-as.integer(events_1$End)
  
  
  
  ### SECOND GMM ###
  #
  # prepare files to register all objects created from the GMM (gbi, events and observations), per subsetted gathering event, recognized in the first GMM
  gbi_final<-data.frame()
  events_final<-data.frame()
  observations_final<-data.frame()
  
  for(i in 1:nrow(events_1)) { # for each gathering event recognized in the first GMM
    
    # subset data according to the time period of each gathering event recognized
    temp<-data_1min[data_1min$minutes >= events_1[i,"Start"] & data_1min$minutes <= events_1[i,"End"],]
    
    # if temp data has only one row, only the same individual or occurs only in the same second, GMM does not run, and so it copies the information obtained in the first GMM
    if(nrow(temp)==1 | length(as.vector(distinct(temp, Tag)[,1]))==1 | length(as.vector(distinct(temp, minutes)[,1]))==1){
      gbi_final<-rbind(gbi_final, gbi_1[i,])
      colnames(gbi_final)<-colnames(gbi_1)
      events_final<-rbind(events_final, events_1[i,])
      observations_final<-rbind(observations_final, observations_1[i,])
      colnames(observations_final)<-colnames(observations_1)
      print(paste(i, "second_GMM", sep="--")) # print i, to check the evolution of the run
    }
    else {
      # run GMM in the subsetted data
      gmm_temp<-gmmevents(time=temp$minutes, identity = temp$Tag, location = temp$Date, global_ids = tags)
      
      # concatenate GMM results for all subsetted data
      gbi_final<-rbind(gbi_final, gmm_temp$gbi)
      events_final<-rbind(events_final, gmm_temp$metadata)
      observations_final<-rbind(observations_final, gmm_temp$B)
      print(paste(i, "second_GMM", sep="--")) # print i, to check the evolution of the run
    }}
  
  # format results as matrixes
  gbi_final<-as.matrix(gbi_final)
  events_final<-as.matrix(events_final)
  observations_final<-as.matrix(observations_final)
  
  # return all useful objects and results
  return(list(data_with_minutes=data, data_per_min=data_1min, gbi_1=gbi_1, events_1=events_1, observations_1=observations_1, gbi_final=gbi_final, events_final=events_final, observations_final=observations_final))
}




#####  MAKE NETWORK BASED ON INDIVIDUALS' TIME OVERLAP WITH TEMPORAL AND SPATIAL OPTIMIZATION -- TIME OVERLAP #####

# This function is adapted from the code wrote in Ferreira et al. (2019,  DOI: 10.1101/739789)

overlap_network_spatial_temporal<-function(data, distance_matrixes, spatial_criterion="all", temporal_criterion=0){
  #
  #
  # This function is adapted from the code wrote in Ferreira et al. (2019,  DOI: 10.1101/739789)
  ## calculates an association matrix based on time overlap data between individuals
  ## considers the information on the start and end of an individuals' occurrence, 
  ## and individuals within a spatial criterion and with a lag time in their time of occurrence
  #
  # ARGUMENTS.
  # "data" is the raw cleaned data file with the following columns:
  ### "Date", with date (day-month-year) information
  ### "Reader", with RFID reader ID
  ### "Tag", with individuals ID
  ### "Start", with the time, in seconds, the individual arrived to the RFID reader
  ### "End", with the time, in seconds, the individual left the RFID reader
  ## distance matrix is a list of matrixes reader by reader with distances, in meters, between all the the readers;
  ### names of each matrix in the list corresponding to the initial date (in seconds) of the change in the distances
  ## spatial_criterion, numerical value indicating the distance threshold, in meters, to be included as associations
  ### (maximum value of distance to be included as associations)
  ## temporal_criterion, numerical value indicating in seconds the different lag times that are added to each bird's leave time from a reader,
  ### before computing the time overlap between individuals
  #
  # VALUES. it returns:
  ## dyads_overlap, returns a matrix with five columns: two of them with the ID's of the dyad, the third with the time each individual spent together,
  ### the fourth with the total time (in seconds) each dyad overlaped, and the last with the Simple Ratio index (SRI) value of the dyad occurrence
  ## network_overlap_time, returns an association matrix (network) with the edges for pair-wise individuals as SRI values
  ## Totaltime, returns the total time each individual occurred in data
  
  options(scipen=999) # to avoid scientific notation numbers
  
  # correctly format data
  # Added here the temporal criterion value, to the time of the end of an individuals' occurrence
  Start_end_events<-data.frame(Tag=data$Tag, Start=data$Start, End=data$End+temporal_criterion, Timepresent=(data$End+temporal_criterion)-data$Start+1, reader=data$reader, stringsAsFactors = F)
  
  tagsnames<-unique(Start_end_events$Tag) # define tags of individuals to make the labels of the association matrix, afterwards
  
  
  # calculate the total time each individual spent in all readers
  library(intervals) # version 0.15.1
  
  totaltime_temp<-vector()
  for (i in 1:length(as.vector(tagsnames))){ # for each individual in the data
    # make the union of all the times each individual was present in the data
    timepresent_temp<-as.data.frame(interval_union(Intervals(Start_end_events[Start_end_events$Tag==as.vector(tagsnames)[i],c("Start", "End")])))
    colnames(timepresent_temp)<-c("Start", "End")
    # calculate the total time each individual was present in the data
    timepresent_temp$Timepresent<-timepresent_temp$End-timepresent_temp$Start+1
    totaltime_temp<-c(totaltime_temp, round(sum(timepresent_temp$Timepresent),0))
  }
  Totaltime<-data.frame(Tag=as.vector(tagsnames), Totaltime=totaltime_temp, stringsAsFactors = F)
  
  
  library(data.table) # version 1.12.6
  
  # calculate individuals' pair-wise overlaps of time
  
  # define correctly data frame to calculate overlaps of time
  Start_end_events<-data.table(Start=as.numeric(as.character(Start_end_events$Start)), End=as.numeric(as.character(Start_end_events$End)), Tag=as.character(Start_end_events$Tag), reader=as.character(Start_end_events$reader))
  setkey(Start_end_events, Start, End) # define columns to be considered in overlaps
  
  # function that makes overlaps of two objects with times (in this case is the matrix with the same matrix)
  overlaps<-foverlaps(Start_end_events, Start_end_events, type="any") 
  
  
  # remove self associations (not needed)
  overlaps<-overlaps[which(overlaps$Tag!=overlaps$i.Tag),]
  
  # remove duplicates (also not needed) by placing a ordered stamp with all the information in each row
  stamp<-t(apply(overlaps, 1, sort))
  stamp<-do.call(paste, as.data.frame(stamp, stringsAsFactors=FALSE)) 
  overlaps$stamp<-as.character(stamp)
  overlaps<-overlaps[duplicated(overlaps$stamp), ]
  
  # define column names correctly
  colnames(overlaps)<-c("Start", "End", "Tag", "reader", "i.Start", "i.End", "i.Tag", "i.reader", "stamp")
  
  
  # sort tags in each row alphabeticaly (so that in an interaction A-B or B-A, the individual A is always present in the first column)
  temp<-data.frame(Tag=as.character(overlaps$Tag), i.Tag=as.character(overlaps$i.Tag), stringsAsFactors = FALSE)
  temp<-t(apply(temp, 1, sort)) # alphabetically sorts individuals, by row; therefore in the following code lines, we can replace the values as they refer
  overlaps$Tag<-as.character(temp[,1])
  overlaps$i.Tag<-as.character(temp[,2])
  
  # calculate total time each pair of individuals spent toguether
  temp<-data.frame(Start=overlaps$Start, i.Start=overlaps$i.Start, stringsAsFactors = FALSE) # data subset only with start times for each pair of individuals together
  temp<-t(apply(temp, 1, sort)) # sort, by row, these times, in order that the first time comes first
  overlaps$Start_association<-temp[,2] # the start of the association is when the second individual arrives
  
  temp<-data.frame(End=overlaps$End, i.End=overlaps$i.End, stringsAsFactors = FALSE) # data subset only with end times for each pair of individuals together
  temp<-t(apply(temp, 1, sort)) # sort, by row, these times, in order that the first time comes first
  overlaps$End_association<-temp[,1] # the end of the association is when the first individual leaves
  
  
  # calculate the time individuals spent toguether
  overlaps$Timetogether<-overlaps$End_association-overlaps$Start_association+1
  
  
  # only applies a spatial criterion when the distance is different from "all"
  # if "all" skips this step as no spatial criterion is applied
  if(spatial_criterion!="all"){
    
    # temporary data frame with  information of readers only
    units_dist_temp<-as.data.frame(overlaps[,c("reader", "i.reader")])
    
    
    if(max(as.numeric(names(distance_matrixes)))<= max(overlaps$End_association)){
      # add maximum range of dates, adding a 0 object as the last element of the list, and setting its name according to the maximum date of the data (in seconds)
      distance_matrixes[[length(distance_matrixes)+1]]<-0
      names(distance_matrixes)[[length(distance_matrixes)]]<-overlaps$End_association[nrow(overlaps)]
    }
    
    # calculate overlaps between the time of occurrence of each each row of data and the time of validity of each distance matrix
    
    # matrix with total time of Start and End of each distance matrix present in distance list 
    distance_matrixes_dates<-data.table(start_matrix_date=names(distance_matrixes)[1:(length(distance_matrixes)-1)], end_matrix_date=names(distance_matrixes)[2:(length(distance_matrixes))], matrix_number=1:(length(distance_matrixes)-1))
    # correctly format values in the matrix
    distance_matrixes_dates$end_matrix_date[1:(nrow(distance_matrixes_dates)-1)]<-as.numeric(distance_matrixes_dates$end_matrix_date[1:(nrow(distance_matrixes_dates)-1)])-1
    # correctly format matrix and columns to make overlap
    distance_matrixes_dates<-data.table(start_matrix_date=as.numeric(distance_matrixes_dates$start_matrix_date), end_matrix_date=as.numeric(distance_matrixes_dates$end_matrix_date), matrix_number=distance_matrixes_dates$matrix_number)
    setkey(distance_matrixes_dates, start_matrix_date, end_matrix_date)
    
    # correctly format matrix with reader ID and columns to make overlap
    temp_overlap_to_dist<-data.table(Start_association=overlaps$Start_association, End_association=overlaps$End_association)
    setkey(temp_overlap_to_dist, Start_association, End_association)
    
    # make overlap
    dist_matrix_number_temp<-foverlaps(distance_matrixes_dates, temp_overlap_to_dist, type="any") 
    dist_matrix_number_temp<-na.exclude(dist_matrix_number_temp) # remove NA's originated in the overlaps calculation
    
    # register the number of the distance matrix, in the distance list, in which each dyad occurrence occurred
    units_dist_temp$dist_matrix_number<-dist_matrix_number_temp$matrix_number
    
    
    # exclude individuals which were more than the spatial criterion distance from each other
    
    distances_temp<-vector()
    
    for(i in 1:length(unique(units_dist_temp$dist_matrix_number))){
      overlaps_subset<-overlaps[units_dist_temp$dist_matrix_number==unique(units_dist_temp$dist_matrix_number)[i],]
      units_dist_temp_subset<-units_dist_temp[units_dist_temp$dist_matrix_number==unique(units_dist_temp$dist_matrix_number)[i],]
      
      distances_temp<-c(distances_temp, apply( units_dist_temp_subset, 1, function(x,distance_matrixes) distance_matrixes[ x[1], x[2] ], distance_matrixes=distance_matrixes[[units_dist_temp_subset$dist_matrix_number[1]]]))
    }
    
    # calculates how long each individual is from each other
    overlaps$inds_distance<-distances_temp
    
    
    # note if it is to include as an association or not, depending on the distance value
    overlaps$association_to_include<-ifelse(overlaps$inds_distance >= spatial_criterion, 0, 1)  
    
    # change time together to 0 if the association is not to be considered
    overlaps$Timetogether<-overlaps$Timetogether*overlaps$association_to_include
  }
  
  
  # extract dyads information to check for repeated self-dyads associations in different locations
  # this is done checking the overlaps that will not be counted as associations
  overlaps_counting<-overlaps[overlaps$Timetogether!=0,]
  
  # extract dyads information
  dyads_data<-overlaps_counting[,c("Tag", "i.Tag")]
  # make an ordered stamp for each dyad
  dyads_data$dyad<-as.character(do.call(paste, as.data.frame(t(apply(dyads_data, 1, sort)), stringsAsFactors=FALSE)) )
  
  # include Start and End dyad overlap information, as well as reader ID
  dyads_data<-as.data.frame(dyads_data[,c("dyad")])
  dyads_data<-cbind(dyads_data, overlaps_counting$Start_association, overlaps_counting$End_association)
  colnames(dyads_data)<-c("dyad", "Start_association", "End_association")
  
  
  # calculate the total time each dyad occurred
  dyad_totaltime_temp<-vector()
  # information on the dyads present
  dyadsnames_temp<-as.character(unique(dyads_data$dyad))
  
  # for each dyad
  for (i in 1:length(dyadsnames_temp)){
    # make the union of each dyad's occurrence times
    dyad_timepresent_temp<-as.data.frame(interval_union(Intervals(dyads_data[dyads_data$dyad==dyadsnames_temp[i],c("Start_association", "End_association")])))
    # correctly define column names
    colnames(dyad_timepresent_temp)<-c("Start", "End")
    # calculate the total amount of time each dyad occurred
    dyad_timepresent_temp$Timepresent<-dyad_timepresent_temp$End-dyad_timepresent_temp$Start+1
    # join all the calculated total time each dyad occurred
    dyad_totaltime_temp<-c(dyad_totaltime_temp, round(sum(dyad_timepresent_temp$Timepresent),0))
  }
  
  # correctly format data
  overlaps<-data.frame(Tag=unlist(lapply(strsplit(dyadsnames_temp, " "), '[[', 1)), i.Tag=unlist(lapply(strsplit(dyadsnames_temp, " "), '[[', 2)), Timetogether=dyad_totaltime_temp, stringsAsFactors = F)
  
  
  # divide by the total time both individuals were in the readers, to calculate SRI
  for(i in 1:nrow(overlaps)){
    # SRI
    overlaps$totaltime[i]<-Totaltime[which(Totaltime$Tag==overlaps$Tag[i]),]$Totaltime+Totaltime[which(Totaltime$Tag==overlaps$i.Tag[i]),]$Totaltime
  }
  
  # calculate network edge values (dividing the time individuals spent together by the time both of them were present in the data)
  overlaps$edge<-as.numeric(overlaps$Timetogether/overlaps$totaltime)
  
  # make a data frame only with the edge values for each dyad
  edgelist<-data.frame(Tag1=overlaps$Tag, Tag2=overlaps$i.Tag, edge=as.numeric(as.character(overlaps$edge)), stringsAsFactors = FALSE)
  
  # prepare tag names to make the association matrix
  tagsnames<-tagsnames[order(tagsnames)]
  names<-as.list(tagsnames)
  names<-list(names,names)
  
  # Register edge values to a matrix
  association_matrix<-matrix(NA, nrow=length(tagsnames), ncol=length(tagsnames), dimnames=names)
  
  rownames(association_matrix)<-tagsnames
  colnames(association_matrix)<-tagsnames
  
  # write the correct edge values for each dyad in the correct place of the association matrix
  for(i in 1:nrow(association_matrix)){
    for(x in 1:ncol(association_matrix)){
      if(nrow(edgelist[(rownames(association_matrix)[i]==edgelist$Tag1 & rownames(association_matrix)[x]==edgelist$Tag2),])>0){
        association_matrix[i,x]<-edgelist[(rownames(association_matrix)[i]==edgelist$Tag1 & rownames(association_matrix)[x]==edgelist$Tag2),]$edge
      }
      if(nrow(edgelist[(rownames(association_matrix)[i]==edgelist$Tag2 & rownames(association_matrix)[x]==edgelist$Tag1),])>0){
        association_matrix[i,x]<-edgelist[(rownames(association_matrix)[i]==edgelist$Tag2 & rownames(association_matrix)[x]==edgelist$Tag1),]$edge
      }
      if(nrow(edgelist[(rownames(association_matrix)[i]==edgelist$Tag2 & rownames(association_matrix)[x]==edgelist$Tag1),])==0 & nrow(edgelist[(rownames(association_matrix)[i]==edgelist$Tag1 & rownames(association_matrix)[x]==edgelist$Tag2),])==0){
        association_matrix[i,x]<-0
      }
    }
  }
  
  return(list(network_overlap_time = association_matrix, Totaltime=Totaltime, dyads_overlap=overlaps ))
  
}




##### STRUCTURE-BASED AND EXTERNAL OPTIMISATION OF ASSOCIATION CRITERIA - DESCRIPTIVE STATISTICS CALCULATION #####

stats_calculation<-function(network_overlap, network_cooccurrence, assortment_groups){
  
  #
  #
  # This function calculates the descriptive statitsics (CV, SD, Entropy, and Assortment), described in the main text
  #
  # ARGUMENTS.
  ## network_overlap, is the association matrix computed from time overlap between individuals
  ## network_cooccurrence, is the association matrix computed from individuals' counts of co-occurrence
  ## assortment_groups, is a matrix with two columns: the first column "individual_ID" has the ID of all individuals in the data,
  #### and the second column "group" has the number of the group that each individual belongs
  #
  # VALUES. it returns:
  ## overlap_cv, coefficient of variation of the time overlap network
  ## overlap_sd, standard variation of the time overlap network
  ## overlap_entropy, entropy of the time overlap network
  ## overlap_assortment, assortativity coefficient of the time overlap network
  ## cooccurrence_cv, coefficient of variation of the counts of co-occurrence network
  ## cooccurrence_sd, standard variation of the counts of co-occurrence network
  ## cooccurrence_entropy, entropy of the counts of co-occurrence network
  ## cooccurrence_assortment, assortativity coefficient of the counts of co-occurrence network
  #
  #
  
  # creates objects to register the descriptive statistics calculated
  overlap_cv<-vector()
  overlap_sd<-vector()
  overlap_entropy<-vector()
  overlap_assortment<-vector()
  
  cooccurrence_cv<-vector()
  cooccurrence_sd<-vector()
  cooccurrence_entropy<-vector()
  cooccurrence_assortment<-vector()
  
  
  ## CV calculation
  cv<-function(x){return(sd(x)/mean(x))}
  
  #### time overlap
  overlap_cv<-cv(network_overlap)
  
  #### counts of co-ocurrence
  cooccurrence_cv<-cv(network_cooccurrence)
  
  
  ## SD calculation
  
  #### time overlap
  overlap_sd<-sd(network_overlap)
  
  #### counts of co-ocurrence
  cooccurrence_sd<-sd(network_cooccurrence)
  
  
  ## Entropy (DescTools function) calculation
  library(DescTools) # version 0.99.28
  
  #### time overlap
  overlap_entropy<-Entropy(network_overlap, base=2)
  
  #### counts of co-ocurrence
  cooccurrence_entropy<-Entropy(network_cooccurrence, base=2)
  
  
  ## Assortment
  library(assortnet) # version 0.12
  
  
  #### time overlap
  assortment_groups<-assortment_groups[match(row.names(network_overlap), assortment_groups$individual_ID),]
  overlap_assortment<-assortment.discrete(network_overlap, assortment_groups$group, weighted = T, SE=T)$r
  
  #### counts of co-ocurrence
  assortment_groups<-assortment_groups[match(row.names(network_cooccurrence), assortment_groups$individual_ID),]
  cooccurrence_assortment<-assortment.discrete(network_cooccurrence, assortment_groups$group, weighted = T, SE=T)$r
  
  
  return(list(overlap_cv=overlap_cv, overlap_sd=overlap_sd, overlap_entropy=overlap_entropy, overlap_assortment=overlap_assortment,
              cooccurrence_cv=cooccurrence_cv, cooccurrence_sd=cooccurrence_sd, cooccurrence_entropy=cooccurrence_entropy, 
              cooccurrence_assortment=cooccurrence_assortment))
}




#### NETWORK RANDOMIZATIONS: CHECK SUGNIFICANCE ####


# Answers the question: Is the network significantly more structured when compared to a null random model based on permutations?

###CONNOR MODIFIED THIS SO THAT IT DOESN'T BREAK MY COMPUTER-> TO REVERT TO ORIGINAL DOCUMENT TURN 'permutation_number = 100000' ####

customed_randomization<-function(data, network_cooccurrence, network_overlap, gbi_final, permutation_number = 1000, events_final){
  
  #
  #
  # This function makes permutation on the GBI matrix and on the data to calculate time overlaps
  #
  # ARGUMENTS.
  ## data is the raw cleaned data file with the following columns:
  #### "Date", with date (day-month-year) information
  #### "Reader", with RFID reader ID
  #### "Tag", with individuals ID
  #### "Start", with the time, in seconds, the individual arrived to the RFID reader
  #### "End", with the time, in seconds, the individual left the RFID reader
  ## network_cooccurrence, is the association matrix computed from individuals' counts of co-occurrence
  ## network_overlap, is the association matrix computed from time overlap between individuals
  ## gbi_final, is the GBI matrix obtained from the second GMM (calculated from the 'gmm_adapted' fucntion)
  ## permutation_number, numerical value corresponding to the number of permutations to be made (default = 100000, according to main text)
  ## events_final, is the information about the gathering events, recognized by the second GMM (calculated from the 'gmm_adapted' fucntion)
  #
  # VALUES. it returns:
  ## overlaps_to_rand, data with event ID information for each individual occurrence
  ## gbi_swapped, list of GBI matrixes with cumulative permutations, every 100 swaps
  ## overlap_swapped, list of data to calculate time overlaps after cumulative permutations, every 100 swaps
  #
  #
  
  # prepares data for permutations
  
  library(data.table) # version 1.12.6
  
  events_final<-as.data.frame(events_final)
  # gives an ID to each gathering event recognized
  events_final<-cbind(events_final, 1:nrow(events_final))
  colnames(events_final)[4]<-"Event_number"
  
  
  # register the ID of the event to which each individual occurrence, in the data, belongs
  # for that we make an overlap between the individuals' occurrences, in the data, with each gathering event recognized
  
  # format data to make overlap
  datatable_to_rand<-data.table(Start_data_minutes=as.numeric(as.character(data$minutes_Start)), End_data_minutes=as.numeric(as.character(data$minutes_End)), Tag=as.character(data$Tag), seconds_data_start=as.numeric(as.character(data$Start)), seconds_data_end=as.numeric(as.character(data$End)), reader.number=data$reader)
  datatable_events_to_rand<-data.table(Start_event_minutes=as.numeric(as.character(events_final$Start)), End_event_minutes=as.numeric(as.character(events_final$End)), Event_number=as.character(events_final$Event_number))
  setkey(datatable_to_rand, Start_data_minutes, End_data_minutes)
  setkey(datatable_events_to_rand, Start_event_minutes, End_event_minutes)
  
  # makes overlap between data and gathering events recognized
  overlaps_to_rand<-foverlaps(datatable_to_rand, datatable_events_to_rand, type="any") 
  overlaps_to_rand<-overlaps_to_rand[order(overlaps_to_rand[,"Event_number"]),]
  
  # calculate the time of the gathering events recognized, in seconds
  overlaps_to_rand$seconds_start_events<-overlaps_to_rand$Start_event_minutes*60
  overlaps_to_rand$seconds_end_events<-(overlaps_to_rand$End_event_minutes*60)+60
  
  # ensures that each individual occurrence begins inside the gathering event
  temp<-data.frame(seconds_start_events=overlaps_to_rand$seconds_start_events, seconds_data_start=overlaps_to_rand$seconds_data_start, stringsAsFactors = FALSE) 
  # sort, by row, the gathering event and data start times, in seconds
  temp<-t(apply(temp, 1, sort))
  # the start of each individual occurrence is the maximum time of the two values
  # (when the individual occurrence starts or when the event starts, whichever comes lastly)
  overlaps_to_rand$i.start_seconds<-temp[,2]
  
  # the same for the End times
  temp<-data.frame(seconds_end_events=overlaps_to_rand$seconds_end_events, seconds_data_end=overlaps_to_rand$seconds_data_end, stringsAsFactors = FALSE) 
  temp<-t(apply(temp, 1, sort))
  # the end of the individual occurrence is the minimum time of the two values
  # (when the individual occurrence ends or when the event ends, whichever comes first)
  overlaps_to_rand$i.end_seconds<-temp[,1]
  
  
  
  # run permutations
  
  library(asnipe) # version 1.1.11
  
  
  # generate cumulative permutated networks
  # in each permutation, 1 swap is made to the previous permutated gbi matrix
  # gbi matrixes are saved every 100 cumulative swaps
  # this swaped gbi can be used to recalculate co-occurrence networks
  # and to swap individuals from the data to do time overlaps;
  # from the swapped data to do overlaps, time overlap networks can be recalculated
  
  # create lists to register objects from permutations
  gbi_swapped<-list()
  overlap_swapped<-list()
  
  
  for(i in 1:permutation_number){
    if(i==1){ # for the first permutation, consider the original GBI matrix
      
      # make a permutation (1 swap) to the GBI data
      gbi_swap_temp<-network_swap(association_data = gbi_final, association_matrix = network_cooccurrence, swaps = 1, within_day = T, days=events_final[,"Location"])
      
      # register the individuals swapped
      swap<-which(apply(gbi_final==gbi_swap_temp$Association_data | gbi_swap_temp$Association_data ==gbi_final, 1, all)==F)
      
      # for each individual swapped
      for(t in 1:length(swap)) {
        temp_gbi_swaped<-gbi_swap_temp$Association_data[swap[t],]
        temp_gbi_swaped<-temp_gbi_swaped[temp_gbi_swaped==1]
        inds_gbi_swaped<-names(temp_gbi_swaped)
        
        # detect the events ID between which the swapped occurred
        temp<-overlaps_to_rand[overlaps_to_rand$Event_number == swap[t],]
        inds_in_overlap<-unique(temp$Tag)
        
        # swap the same individuals in the data to calculate time overlap networks
        to_remove<-setdiff(inds_in_overlap, inds_gbi_swaped)
        to_switch<-setdiff(inds_gbi_swaped, inds_in_overlap)
        
        overlaps_to_rand$Tag[overlaps_to_rand$Event_number==swap[t] & overlaps_to_rand$Tag==to_remove]<-to_switch
        
      }
      
    }
    
    else{ # for the remaining permutations, consider always the previous permutated GBI matrix
      # register the GBI data from the previous permutation
      gbi_swap_temp_previous<-gbi_swap_temp$Association_data
      
      # permute again the GBI data
      gbi_swap_temp<-network_swap(association_data = gbi_swap_temp$Association_data, association_matrix = gbi_swap_temp$Association_index, swaps = 1, within_day = T, days=events_final[,"Location"])
      
      # register the GBI data at every 100 cumulative permutations, to allow posteriously, the calculation of a network based on counts of co-occurrence
      if(i/100==floor(i/100)) {
        gbi_swapped[[i/100]]<-gbi_swap_temp$Association_data
      }
      
      # register the individuals swapped
      swap<-which(apply(gbi_swap_temp_previous==gbi_swap_temp$Association_data | gbi_swap_temp$Association_data ==gbi_swap_temp_previous, 1, all)==F)
      
      # for each individual swapped
      for(t in 1:length(swap)) {
        temp_gbi_swaped<-gbi_swap_temp$Association_data[swap[t],]
        temp_gbi_swaped<-temp_gbi_swaped[temp_gbi_swaped==1]
        inds_gbi_swaped<-names(temp_gbi_swaped)
        
        # detect the event ID in which the swapped occurred
        temp<-overlaps_to_rand[overlaps_to_rand$Event_number == swap[t],]
        inds_in_overlap<-unique(temp$Tag)
        
        # swap the same individuals in the data to calculate time overlap networks
        to_remove<-setdiff(inds_in_overlap, inds_gbi_swaped)
        to_switch<-setdiff(inds_gbi_swaped, inds_in_overlap)
        
        overlaps_to_rand$Tag[overlaps_to_rand$Event_number==swap[t] & overlaps_to_rand$Tag==to_remove]<-to_switch
      }
      
      # register the data to calculate time overlaps at every 100 cumulative permutations, to allow posteriously, the calculatiuon of a time overlap network
      if(i/100==floor(i/100)) {
        # correctly define column names to run posteriously function to make time overlap networks
        colnames(overlaps_to_rand)[colnames(overlaps_to_rand)=="i.start_seconds"]<-"Start"
        colnames(overlaps_to_rand)[colnames(overlaps_to_rand)=="i.end_seconds"]<-"End"
        # register data
        overlap_swapped[[i/100]]<-overlaps_to_rand
        
        # correctly define column names to match the original column names of the overlaps_to_rand object
        colnames(overlaps_to_rand)[colnames(overlaps_to_rand)=="Start"]<-"i.start_seconds"
        colnames(overlaps_to_rand)[colnames(overlaps_to_rand)=="End"]<- "i.end_seconds"
        
        gc()
      }
    }
    print(paste(i, "swap", sep="--")) # print permutation number, to check the evolution of the run
  }
  
  
  return(list(overlaps_to_rand=overlaps_to_rand, gbi_swapped=gbi_swapped, overlap_swapped=overlap_swapped))
}
