Simulation
================
Dan Murphy
August 17, 2016

A new package, triangle, is added to sample from this shape distribution in the simulation.

``` r
library(tidyr)
library(dplyr)
library(triangle)
```

Here, the summary statistics are used to simulate a 300 runs for each team in each contest. To predict a run: \* First, a random variable is pulled to determine if the run takes place. If the random variable is less than the ratio of Hits to Appearances, the run takes place by sampling from a triangle distribution. (This simulates a hit) \* If the random variable was greater than the ration of Hits to Appearances, another random variable is pulled to test if the random variable is less than the ratio of Misses to Appearances. If this is true, a sample is pulled from a uniform distribution. (This simulates a Miss, but a completed run) \* If both tests are failed, NA is the result. (This simulates a NT or OT)

``` r
Simulation <- NULL
Simulation <- data.frame()

i <- 1
  for(i in 1:300){
    Simulation <- bind_rows(Simulation, StateSimParams)
    
    j<- dim(Simulation)[1]-dim(StateSimParams)[1] + 1

        for(j in j:dim(Simulation)[1]){
          
          Simulation$Evol[j] <- i
          Simulation$Time[j] <-    ifelse((runif(1)<(Simulation$Hits[j]/Simulation$Appear[j])),
                                            rtriangle(1,Simulation$HitsAvg[j]-(2*Simulation$HitsSD[j]),
                                             Simulation$HitsAvg[j]+(2*Simulation$HitsSD[j])),
                                      ifelse((runif(1)<(Simulation$Misses[j]/Simulation$Appear[j])),
                                                    runif(1,min=TimeNumHitCutoff[Simulation$TimeContest[j]],
                                                    max=TimeNumOTCutoff[Simulation$TimeContest[j]]),NA))
          }}
Simulation$Time <- as.numeric(Simulation$Time)
```

Next, runs are grouped by Contest and Evolution in order to rank. After ranking, points are identified. Last, points are tallied for each evolution.

``` r
Simulation <- Simulation %>% group_by(Evol,TimeContest,Year) %>% mutate(runrank = min_rank(Time))
Simulation <- ungroup(Simulation)

Points <- c(5,4,3,2,1)
Simulation$Points <- ifelse(Simulation$runrank<=5,Points[Simulation$runrank],0)
Simulation$Points <- ifelse(is.na(Simulation$Points),0,Simulation$Points)

SimTallyPts <- unique(Simulation %>% select(Team,Year,Evol))
```

This next set of code tallies the results. First, each point total is ranked within its evolution. Then after creating a dataframe, the next loop finds the percentage of Tournament Firsts and Top 5s followed by the same for each contest.

``` r
i<-1
for(i in 1:dim(SimTallyPts)[1]){

  temp  <- Simulation %>% 
            filter(Team==SimTallyPts$Team[i]&Year==SimTallyPts$Year[i]&Evol==SimTallyPts$Evol[i]) %>%
            group_by(Team,Year,Evol) %>%
            summarise(PtsTotal=sum(Points))

  SimTallyPts$EvolPts[i] <- temp$PtsTotal[1] 
  }

SimTallyPts <- SimTallyPts %>% group_by(Year,Evol) %>% mutate(EvolRank = min_rank(desc(EvolPts)))

SimTally <- AllData %>% select(Team) %>% distinct()

SimTally$Year <- 2016
SimTally$ThreeManLadderResult<-0
SimTally$BLadderFirstPlace<-0
SimTally$BLadderTop5<-0
SimTally$BLadderResult<-0
SimTally$CLadderFirstPlace<-0
SimTally$CLadderTop5<-0
SimTally$CLadderResult<-0
SimTally$CHoseFirstPlace<-0
SimTally$CHoseTop5<-0
SimTally$CHoseResult<-0
SimTally$BHoseFirstPlace<-0
SimTally$BHoseTop5<-0
SimTally$BHoseResult<-0
SimTally$EfficiencyFirstPlace<-0    
SimTally$EfficiencyTop5<-0  
SimTally$EfficiencyResult<-0    
SimTally$MotorPumpFirstPlace<-0 
SimTally$MotorPumpTop5<-0   
SimTally$MotorPumpResult<-0 
SimTally$BucketsFirstPlace<-0   
SimTally$BucketsTop5<-0 
SimTally$BucketsResult<-0

i<-1
for(i in 1:dim(SimTally)[1]){


  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&Year==SimTally$Year[i]) %>%
    group_by(Team,Year) %>%
    summarise(avg=sum(Points)/max(Evol))
  
  SimTally$AveragePoints[i] <- temp$avg


  temp2 <- Simulation %>% summarise(p=max(Evol))
  
  temp <- SimTallyPts %>% 
              filter(Team==SimTally$Team[i]&
              Year==SimTally$Year[i]&
              EvolRank==1) %>%
              group_by(Team,Year) %>%
                summarise(n=n())

  SimTally$OverallFirstPlace[i] <- temp$n[1]/temp2$p[1]*100

  temp <- SimTallyPts %>% 
            filter(Team==SimTally$Team[i]&
            Year==SimTally$Year[i]&
            (EvolRank<=5)&(EvolRank>=1)) %>%
            group_by(Team,Year) %>%
              summarise(n=n())
  
  
  
  SimTally$OverallTop5[i] <- temp$n[1]/temp2$p[1]*100

  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="Three Man Ladder"&
             Points==5) %>%
    summarise(n())
  
  SimTally$ThreeManLadderFirstPlace[i] <- temp$n[1]/temp2$p[1]*100
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="Three Man Ladder"&
             Points>=1) %>%
    summarise(n=n())
  
  SimTally$ThreeManLadderTop5[i] <- temp$n[1]/temp2$p[1]*100  
  
  # B Ladder
 
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="B Ladder"&
             Points==5) %>%
    summarise(n())
  
  SimTally$BLadderFirstPlace[i] <- temp$n[1]/temp2$p[1]*100

  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="B Ladder"&
             Points>=1) %>%
    summarise(n=n())
  
  SimTally$BLadderTop5[i] <- temp$n[1]/temp2$p[1]*100  
  
# C Ladder  
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="C Ladder"&
             Points==5) %>%
    summarise(n())
  
  SimTally$CLadderFirstPlace[i] <- temp$n[1]/temp2$p[1]*100
  
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="C Ladder"&
             Points>=1) %>%
    summarise(n=n())
  
  SimTally$CLadderTop5[i] <- temp$n[1]/temp2$p[1]*100  
  
# C Hose

  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="C Hose"&
             Points==5) %>%
    summarise(n())
  
  SimTally$CHoseFirstPlace[i] <- temp$n[1]/temp2$p[1]*100
  
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="C Hose"&
             Points>=1) %>%
    summarise(n=n())
  
  SimTally$CHoseTop5[i] <- temp$n[1]/temp2$p[1]*100  
  
# B Hose
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="B Hose"&
             Points==5) %>%
    summarise(n())
  
  SimTally$BHoseFirstPlace[i] <- temp$n[1]/temp2$p[1]*100
  
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="B Hose"&
             Points>=1) %>%
    summarise(n=n())
  
  SimTally$BHoseTop5[i] <- temp$n[1]/temp2$p[1]*100  
  
# Efficiency
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="Efficiency"&
             Points==5) %>%
    summarise(n())
  
  SimTally$EfficiencyFirstPlace[i] <- temp$n[1]/temp2$p[1]*100
  
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="Efficiency"&
             Points>=1) %>%
    summarise(n=n())
  
  SimTally$EfficiencyTop5[i] <- temp$n[1]/temp2$p[1]*100  
 
# Motor Pump
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="Motor Pump"&
             Points==5) %>%
    summarise(n())
  
  SimTally$MotorPumpFirstPlace[i] <- temp$n[1]/temp2$p[1]*100
  
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="Motor Pump"&
             Points>=1) %>%
    summarise(n=n())
  
  SimTally$MotorPumpTop5[i] <- temp$n[1]/temp2$p[1]*100  
 
# Buckets
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="Buckets"&
             Points==5) %>%
    summarise(n())
  
  SimTally$BucketsFirstPlace[i] <- temp$n[1]/temp2$p[1]*100
  
  
  temp <- Simulation %>% 
    filter(Team==SimTally$Team[i]&
             Year==SimTally$Year[i]&
             TimeContest=="Buckets"&
             Points>=1) %>%
    summarise(n=n())
  
  SimTally$BucketsTop5[i] <- temp$n[1]/temp2$p[1]*100  
  
    
    }
```
