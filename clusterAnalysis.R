#install.packages('gridExtra')
#install.packages('magrittr')
#install.packages('plot3D')
#install.packages('dplyr')

library("ggplot2")
library("reshape")
library("gridExtra")
library("magrittr")
library("dplyr")
library("reshape2")

# set the working directory
# setwd("C:\\home\\emily\\home")

# function used to read the data input file and returns the dataframe
fn.readCallDetailRecord <- function(CDR_data_subset) {
  
  # read Italia cellular network over the city of Milano
  inputDF <- read.csv(file=CDR_data_subset,sep="\t",header=F)
  
  # check the number of observations
  nrow(inputDF)
  
  # check the structure of the dataset
  str(inputDF)
  
  # rename the dataframe columns
  colnames(inputDF) <- c("square_id","time_interval","country_code","sms_in_activity","sms_out_activity","call_in_activity","call_out_activity","internet_traffic_activity")
  
  # subset the input dataframe for the 500 square_ids
  inputSubsetDF <- subset(inputDF,inputDF$square_id >= 1)
  #inputSubsetDF = inputSubsetDF[ !square_id %in% c(47,48,147,246,247,347)]
  return(inputSubsetDF)
}

# function used to additional fields to the dataframe
fn.deriveAdditionalFields <- function(inputSubsetDF){
  # convert numeric column into factor column
  factorColumns <- c("square_id","country_code")
  inputSubsetDF[factorColumns] <- lapply(inputSubsetDF[factorColumns],as.factor)
  
  # derive activity start date and hour from time interval
  inputSubsetDF$activity_start_time <- fn.findStartTimeInterval(inputSubsetDF$time_interval)
  inputSubsetDF$activity_date <- as.Date(as.POSIXct(inputSubsetDF$activity_start_time,origin="1970-01-01"))
  inputSubsetDF$activity_time <- format(inputSubsetDF$activity_start_time,"%H")
  
  # derive total activity from sms in and out, call in and out and internet traffic activity 
  inputSubsetDF$total_activity <- rowSums(inputSubsetDF[, c(4,5,6,7,8)],na.rm=T)
  return(inputSubsetDF)
}

# function used to calculate the datetime from epoch unix time
fn.findStartTimeInterval <- function(inputTime){
  val <- inputTime/1000
  outputTime <- as.POSIXct(val, origin="1970-01-01",tz="UTC")
  return(outputTime)
}


# function used to plot the activity_time Vs total_activity
fn.findPeekActivityByHours <- function(inputSubsetDF){
  # Sum the total acitivity by hours
  totalActivityDF <- aggregate(total_activity ~ activity_time,inputSubsetDF,FUN=sum)
  
  totalActivityPlot <- ggplot(data=totalActivityDF, aes(x=activity_time, y=total_activity, colour=total_activity)) + geom_bar(stat="identity")+theme_bw()
  
  print(totalActivityPlot)
}

fn.cdrClusterAnalysis <- function(){
  #Data preprocessing
  cdrAcitivityInputDF <- fn.readCallDetailRecord("sms-call-internet-mi-2014-01-01.txt")
  cdrAcitivityInputDF <- fn.deriveAdditionalFields(cdrAcitivityInputDF)
  
  #EDA
  fn.findPeekActivityByHours(cdrAcitivityInputDF)
  fn.findPeekActivityByGrid(cdrAcitivityInputDF)
  fn.findPeekActivityByCountryCode(cdrAcitivityInputDF)
  
}

fn.cdrClusterAnalysis()
# Plot Peak Activity by Hours w/o Function
#Original Dataset 
View(inputDF)

# Subset Original Dataset
View(inputSubsetDF)

# Create total_activity variable
inputSubsetDF$total_activity <- rowSums(inputSubsetDF[, c(4,5,6,7,8)],na.rm=T)
View(inputSubsetDF)

# derive activity start date and hour from time interval
inputSubsetDF$activity_start_time <- fn.findStartTimeInterval(inputSubsetDF$time_interval)
inputSubsetDF$activity_date <- as.Date(as.POSIXct(inputSubsetDF$activity_start_time,origin="1970-01-01"))
inputSubsetDF$activity_time <- format(inputSubsetDF$activity_start_time,"%H")


#save(inputSubsetDF, file="data.Rdata")






###################################################################################################################
##################################### From Here Down R Markdown Documentation ##########################################
###################################################################################################################


# read in r data file
#load("data.Rdata")

# Original data set
cdrActivityDF <- subset(inputSubsetDF,select=c("activity_time","total_activity"))

# Data: sum the total acitivity by hours
#totalActivityDF <- aggregate(total_activity ~ square_id + activity_time,inputSubsetDF, FUN=sum)
totalActivityDF = inputSubsetDF %>% 
  group_by(square_id, activity_time) %>%
  summarize( 
    talk_time = sum(call_in_activity+call_out_activity, na.rm=T),
    sms_time = sum(sms_in_activity+sms_out_activity, na.rm=T),
    total_activity = talk_time + sms_time
  )

# plot: sms_time by talk_time colored by time of day (hour)
totalActivityDF %>% ggplot( aes(x=talk_time, y=sms_time, color=activity_time)) + geom_point()

# plot: volume total cellular activity over time for specific square IDs 
totalActivityDF %>% 
  filter( square_id %in% c(450,5500,7350,9050) ) %>%
  ggplot(aes(x=activity_time, y=total_activity)) + 
  geom_bar(stat="identity") + facet_wrap(~ square_id, nrow = 3)

# Data frame for call activity
talkActivityDF = inputSubsetDF %>%
  group_by(square_id, activity_time) %>%
  summarize(
    talk_time = sum(call_in_activity+call_out_activity, na.rm=T))



# Data frame for SMS activity
smsActivityDF = inputSubsetDF %>%
  group_by(square_id, activity_time) %>%
  summarize(
    sms_time = sum(sms_in_activity+sms_out_activity, na.rm=T))




# Data: stacked talk and sms variables
stackedData <- melt(as.data.frame(totalActivityDF), id=c("square_id","activity_time","total_activity"))

# Plot: Talk vs. SMS activity by hour for specific square IDs 
stackedData %>%
  filter( square_id %in% c(4240,4850, 5560, 6670) ) %>%
  ggplot(aes(x=activity_time, y=value, fill=variable))+
  geom_bar(stat="identity") + facet_wrap(~ square_id, nrow=3)




################

# Data: Broken out by square_id and hour (one row = one hour per square)
totalSquareHourActivityDF = inputSubsetDF %>%
  group_by(square_id, activity_time) %>%
  summarize(
    talk_time = sum(call_in_activity+call_out_activity, na.rm=T),
    sms_time = sum(sms_in_activity+sms_out_activity, na.rm=T)
  )



# Data: Two new data frames, one for SMS activity and one for Talk activity
totalTalkSquareActivityDF <- dcast(as.data.frame(talkActivityDF), square_id ~ activity_time)
totalSmsSquareActivityDF <- dcast(as.data.frame(smsActivityDF), square_id ~ activity_time)

# Data: combine data frames
totalActivityHourDF <- merge(totalTalkSquareActivityDF, totalSmsSquareActivityDF, by = "square_id", suffixes = c(".talk",".sms"))

# Data: Replace missing values with 0
totalActivityHourDF[ is.na(totalActivityHourDF) ] <- 0

# todo: could we do pca or variable reduction first on the hours (columns)
# some times and traffic are very similar bettween hours and we don't need both columns

# Data: applying k-means on acitivity hours and total_activity
n_clusters=4
cdrClusterModel <- kmeans(totalActivityHourDF[,-1],n_clusters,nstart=10)
print(cdrClusterModel)
cdrClusterModel$centers



# Data: Square ID and cluster 
totalActivityHourDF$cluster = as.factor(cdrClusterModel$cluster)
totalActivityHourDF %>% 
  select( square_id, cluster ) %>%
#  filter( cluster == 3 )
#  ggplot(aes(x=talk_time, y=sms_time, color=cluster)) + geom_point()


# # Data: Number of squares in each cluster
# totalActivityHourDF %>%
#   group_by( cluster ) %>% 
#   count


# Plot: Single hour comparison talk and text by square, colored by cluster
# todo: we could find and do a few more views of these...
totalActivityHourDF %>% 
  ggplot( aes(x=`22.talk`, y=`22.sms`, color=cluster)) + 
  geom_point()


# Question: What does the average day look like for each cluster?


# Data: totalActivityDF with cluster
totalActivityDF_cluster = totalActivityDF %>%
  # join a column representing the cluster from model
  left_join( totalActivityHourDF[,c("square_id","cluster")],
             c("square_id"="square_id")) %>%
  group_by( cluster, activity_time ) %>%
  summarize(
    talk_time = mean(talk_time, na.rm=T),
    sms_time = mean(sms_time, na.rm=T),
    total_activity = mean(total_activity, na.rm=T)
  )

# Plot: Total activity volume over total activity time for specific clusters
totalActivityDF_cluster %>%
  #filter( cluster %in% c(1,2, 80, 41) ) %>%
  ggplot(aes(x=activity_time, y=total_activity)) + 
  geom_bar(stat="identity") + facet_wrap(~ cluster, nrow = 2)




# what about differneces in sms vs talk?
# Data: add cluster to stacked data 
 stackedData_cluster <- melt(as.data.frame(totalActivityDF_cluster), 
                             id=c("cluster","activity_time","total_activity"))

# Plot: talk_time and sms_time by hour for each cluster
stackedData_cluster %>%
  ggplot(aes(x=activity_time, y=value, fill=variable))+
  geom_bar(stat="identity") + facet_wrap(~ cluster, nrow=2)


# how many clusters should there be?
resultsDF = data.frame( n_clusters = NULL, tot.withiness= NULL)
# test between 1 and 20 clusters
for( n_clusters in 1:20 ){
  # Applying k-means on acitivity hours and total_activity
  cdrClusterModel_test <- kmeans(totalActivityHourDF[,-1], n_clusters, nstart=n_clusters)
  resultsDF = rbind(resultsDF, c(n_clusters, cdrClusterModel_test$tot.withinss) )
  print( cdrClusterModel_test$tot.withinss )
}
names(resultsDF) = c("n_clusters","tot.withiness")
resultsDF %>%
  ggplot( aes(x=n_clusters,y=tot.withiness)) + geom_line()




#ggplot(data=totalActivityDF, aes(total_activity)) + geom_histogram()

table(cdrClusterModel$cluster, totalActivityDF$square_id)

cdrClusterModel$cluster <- as.factor(cdrClusterModel$cluster)
ggplot(data=totalActivityDF, aes(x=activity_time, y=total_activity, color=cdrClusterModel$cluster)) + geom_point(shape = 19, alpha = 1/4)
ggplot(data=totalActivityDF, aes(x=activity_time, y=total_activity, color=cdrClusterModel$cluster)) + geom_bar(stat="identity") + facet_wrap(~ cdrClusterModel$cluster, nrow = 4)









