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
