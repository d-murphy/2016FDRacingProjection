SimulationModel
================
Dan Murphy
August 17, 2016

Overview
--------

In this document, the runs from the year are first adjusted for track location, and then summarized which is used to simulate future results. Finially, the results of the simulation are summarized to provide the basis for the projection.

### Adjusting for Track Location

The track location can have an impact on times. Because all teams do not have an equal number of appearances at each track, an adjustment is made to times from popular tracks to properly weight the times. The adjustment has been calculated in an earlier project and is uploaded here.

A location column is created and used to join the Track Adjustment CSV file. Then the Adjusted Time column is created.

``` r
TrackAdj  <- read.csv("C:/Users/murph/Desktop/R/2016FDRacingPrediction/RawData/TrackAdj.csv",
                      stringsAsFactors = FALSE)

df2$Location <- ""
df2$Location <- ifelse(df2$TournName=="Central Islip Invitational","Central Islip",df2$Location)
df2$Location <- ifelse(df2$TournName=="Hempstead Invitational Drill","Hempstead",df2$Location)
df2$Location <- ifelse(df2$TournName=="Joe Hunter Memorial Drill","Hempstead",df2$Location)
df2$Location <- ifelse(df2$TournName=="Lindenhurst Invitational Drill","Lindenhurst",df2$Location)
df2$Location <- ifelse(df2$TournName=="Nassau County Charity Drill","Hempstead",df2$Location)
df2$Location <- ifelse(df2$TournName=="Nassau County Motorized Drill","Merrick",df2$Location)
df2$Location <- ifelse(df2$TournName=="Riverhead Invitational Drill","Riverhead",df2$Location)
df2$Location <- ifelse(df2$TournName=="Selden Invitational Drill","Ridge",df2$Location)
df2$Location <- ifelse(df2$TournName=="Suffolk County Drill","Central Islip",df2$Location)
df2$Location <- ifelse(df2$TournName=="Town of Brookhaven","Ridge",df2$Location)

df2$key <- paste(df2$Location, df2$Contest, sep = " ")
TrackAdjJoin <- TrackAdj[6:7]

df2 <-  left_join(df2, TrackAdjJoin, by = "key")

df2$AdjustedTime <- ifelse(is.na(df2$TrackAdj),df2$Time,df2$Time+df2$TrackAdj)
```

### Summarizing Date

Below summary statistics are calculated for each team's run in each contest during the year. The statistics are the number of runs, the number of successful runs (or hits), the average of the successful runs and the standard deviation of the successful runs.

First a dataframe is created to contain the results.

``` r
# Create a dataframe to store summary stats

StateSimParams <- df2 %>% select(TeamName) %>% distinct()
StateSimParamsAll <- data.frame()
for(i in 1:8){
  StateSimParams$key <- i
  StateSimParamsAll <- bind_rows(StateSimParamsAll,StateSimParams)
}
Contests<- df2 %>% select(Contest) %>% distinct()
Contests$key <- c(1,2,3,4,5,6,7,8)

StateSimParams <- left_join(StateSimParamsAll,Contests)
remove(Contests, StateSimParamsAll)
StateSimParams$key <- NULL

colnames(StateSimParams) <- c("Team","TimeContest")
```

Next, appearances, hits, averages, and SD is calculated.

``` r
TimeNumMin <- as.numeric(4)


# Count the number of Hits

TimeNumHitCutoff <- as.numeric(c(6.8,5.6,9.6,13.4,8.6,9.8,6.7,23))
names(TimeNumHitCutoff) <- c("Three Man Ladder", "B Ladder", "C Ladder", "C Hose", 
                             "B Hose","Efficiency", "Motor Pump", "Buckets")
StateSimParams$Hits <- 0
i<-1
for(i in 1:dim(StateSimParams)[1]){
  StateSimParams$Hits[i] <- count(df2 %>% filter(Team==StateSimParams$Team[i],
                                                     Year==StateSimParams$Year[i],
                                                     `Before State?`=="Yes",
                                                     TimeContest==StateSimParams$TimeContest[i],
                                                     AdjustedTime > TimeNumMin,
                                                     AdjustedTime <= TimeNumHitCutoff[StateSimParams$TimeContest[i]]
                                                     ))
}

# Count the number of misses
TimeNumOTCutoff <- as.numeric(c(10,10,15,18,13,14,10,35))
names(TimeNumOTCutoff) <- c("Three Man Ladder", "B Ladder", "C Ladder", "C Hose", 
                             "B Hose","Efficiency", "Motor Pump", "Buckets")
StateSimParams$Misses <- 0
i<-1
for(i in 1:dim(StateSimParams)[1]){
  StateSimParams$Misses[i] <- count(AllData %>% filter(Team==StateSimParams$Team[i],
                                                     Year==StateSimParams$Year[i],
                                                     `Before State?`=="Yes",
                                                     TimeContest==StateSimParams$TimeContest[i],
                                                     AdjustedTime > TimeNumHitCutoff[StateSimParams$TimeContest[i]],
                                                     AdjustedTime <= TimeNumOTCutoff[StateSimParams$TimeContest[i]] 
  ))
}

#Count Number of Appearances 

StateSimParams$Appear <- 0
i<-1
for(i in 1:dim(StateSimParams)[1]){
  StateSimParams$Appear[i] <- count(AllData %>% filter(Team==StateSimParams$Team[i],
                                                       Year==StateSimParams$Year[i],
                                                       `Before State?`=="Yes",
                                                       TimeContest==StateSimParams$TimeContest[i] 
  ))
}

#Count the Avg of Hits

StateSimParams$HitsAvg <- 0
StateSimParams$HitsSD <- 0
i<-1
for(i in 1:dim(StateSimParams)[1]){
      result <-  AllData %>% filter(Team==StateSimParams$Team[i],
                                                     Year==StateSimParams$Year[i],
                                                     `Before State?`=="Yes",
                                                     TimeContest==StateSimParams$TimeContest[i],
                                                     AdjustedTime > TimeNumMin,
                                                     AdjustedTime <= TimeNumHitCutoff[StateSimParams$TimeContest[i]]
                                                  ) %>% 
                                                group_by(Team) %>% summarise(mean(AdjustedTime),sd(AdjustedTime))
      StateSimParams$HitsAvg[i] <- as.numeric(result[2])
      StateSimParams$HitsSD[i]  <- as.numeric(result[3])
}

#Make Adjustments for Model


StateSimParams$Hits <- unlist(StateSimParams$Hits)
StateSimParams$Misses <- unlist(StateSimParams$Misses)
StateSimParams$Appear <- unlist(StateSimParams$Appear)

#Update Appearances so that 1 times are not 100% successful
StateSimParams$Appear <- ifelse(StateSimParams$Appear==1,2,StateSimParams$Appear)
StateSimParams$HitsSD <- ifelse(StateSimParams$HitsSD==0,.075,StateSimParams$HitsSD)
StateSimParams$HitsSD <- ifelse(StateSimParams$Hits>=1 & is.na(StateSimParams$HitsSD),.075,StateSimParams$HitsSD)

#Reduce SD for large variance teams with 3 or less hits

StateSimParams$HitsSD <- ifelse(StateSimParams$HitsSD>.3&StateSimParams$Hits<=3,
                                StateSimParams$HitsSD*.6,
                                ifelse(StateSimParams$HitsSD>.2&StateSimParams$Hits<=4,
                                       StateSimParams$HitsSD*.8,StateSimParams$HitsSD))
```
