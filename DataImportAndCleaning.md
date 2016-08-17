DataImportAndCleaning
================
Dan Murphy
August 16, 2016

The normalized data was exported from [NYSDrillTeams](http://www.nysdrillteams.com). The code below loads the CSV files, updates certain column names in order to complete the joins, and executes the joins.

``` r
df  <- read.csv("C:/Users/murph/Desktop/R/2016FDRacingPrediction/RawData/nysdt_events_results.csv")
df2 <- read.csv("C:/Users/murph/Desktop/R/2016FDRacingPrediction/RawData/nysdt_teams.csv")
df3 <- read.csv("C:/Users/murph/Desktop/R/2016FDRacingPrediction/RawData/nysdt_eventtypes.csv")
df4 <- read.csv("C:/Users/murph/Desktop/R/2016FDRacingPrediction/RawData/nysdt_drills.csv")
df5 <- read.csv("C:/Users/murph/Desktop/R/2016FDRacingPrediction/RawData/nysdt_seasons_drills.csv")
df6 <- read.csv("C:/Users/murph/Desktop/R/2016FDRacingPrediction/RawData/nysdt_seasons_events.csv")

colnames(df2)[1] <- "team_id"
colnames(df6)[1] <- "event_id"
colnames(df5)[1] <- "seasondrill_id"
colnames(df3)[1] <- "type"
colnames(df4)[1] <- "drill_id"

df <- left_join(df, df2, by = "team_id")
df <- left_join(df, df6, by = "event_id")
df <- left_join(df, df5, by = "seasondrill_id")
df <- left_join(df, df3, by = "type")
df <- left_join(df, df4, by = "drill_id")

remove(df2,df3,df4,df5,df6)
```

The dataframe is next trimmed to omit extraneous columns Then, names and data types are corrected. Last, the dataset is filtered to only include runs from motorized teams in 2016.

``` r
df2 <- data.frame(df$id, df$team_name, df$event_name, df$name, df$start_date.y, df$rank,df$performance,    
                  df$division)
colnames(df2) <- c("WebsiteID","TeamName","Contest","TournName","TournDate","Rank","Time","Division")

df2$TournDate <- mdy_hm(df2$TournDate)
df2$Time <- as.numeric(levels(df2$Time))[df2$Time]

df2 <- df2 %>% filter((Division %in% c("Exhbition","Motorized")) & (TournDate > ymd("2016/01/01"))) 
df2 <- df2 %>% filter((TournName != "Nassau County Old-Fashioned") & 
                      (TournName != "Long Island Championship (OF)") & 
                      (TournName != "New York State Old Fashioned Drill")& 
                      (TournName != "Mike Esposito Memorial Drill"))
df2 <- df2 %>% filter((Contest != " *********** LINEUP ***********") & 
                      (Contest != "B Hose - CANCELED (RAIN)") & 
                      (Contest != "Individual Ladder - SCHEDULED"))

df2 <- separate(df2, Contest, c("Contest","discard"), sep = " - ")
df2$discard <- NULL
```