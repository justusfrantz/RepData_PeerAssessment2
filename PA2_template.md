# Reproducible Research: Peer Assessment 2

## Ng Peng Hong


###Title
#### Analysis of severe weather events types which are most harmful to population health and have the greatest economic consequences across the United States

###Synopsis
##### In this report, the analysis is conducted based on the dataset downloaded from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database specifically on the impact of severe weather events types to population health and the economic consequences.

##### Based on the results, the severe weather events types which are most harmful to population health are: 
- Tornadoes and Excessive Heat : Most Fatalities
- Tordanoes and TSTM wind : Most Injuries 

#####The severe weather events types which have the greatest economic consequences are:
- Floods and Hurricanes/Typhoons : Most Property Damages
- Drought and Floods : Most Crop Damages

###Data Processing
####Load
#####Datafile is first checked for existence in the directory. Else, the data file is downloaded and unzipped in the directory.

```r
if (!file.exists("repdata-data-StormData.csv")) {
  if (!"repdata-data-StormData.csv.bz2" %in% dir("./")) {
     download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                   destfile = "repdata-data-StormData.csv.bz2")
     bunzip2("repdata-data-StormData.csv.bz2", overwrite=T, remove=F) 
     }
  else bunzip2("repdata-data-StormData.csv.bz2", overwrite=T, remove=F) }
```
#####Next, the csv file is read into the storm dataframe. If the storm dataframe already exists, then it need not be loaded again

```r
if (!"storm" %in% ls()) {
    storm <- read.csv("repdata-data-StormData.csv",na.strings="",header = T,sep=",")
}
```

###Analysis
####Impact on Population Health
##### The impacts on population health are investigated for FATALITIES and INJURIES

```r
popfatalities <- aggregate(cbind(FATALITIES)~EVTYPE,storm, sum)
popfatalities <- popfatalities[order(popfatalities$FATALITIES,decreasing=T),]
top10fat <- head(popfatalities,10)
popinjuries <- aggregate(cbind(INJURIES)~EVTYPE,storm, sum)
popinjuries <- popinjuries[order(popinjuries$INJURIES,decreasing=T),]
top10inj <- head(popinjuries,10)
```
##### R code chunks for plotting of Fatalities and Injuries

```r
top10fat <- top10fat[order(top10fat$FATALITIES,decreasing=F),]
top10inj <- top10inj[order(top10inj$INJURIES,decreasing=F),]
top10fat$EVTYPE <- factor(top10fat$EVTYPE, levels = top10fat$EVTYPE)
top10inj$EVTYPE <- factor(top10inj$EVTYPE, levels = top10inj$EVTYPE)

fatplot <- ggplot(top10fat, aes(y = FATALITIES, x = EVTYPE)) + 
  geom_bar(stat="identity",fill="blue") +
  theme(axis.text = element_text(colour = "black")) +
  coord_flip() + ggtitle("Top 10 Total Fatalities by Severe Weather Events in the U.S.") + 
  xlab("Severe Weather Events Type") + ylab("Number of Fatalities")

injplot <- ggplot(top10inj, aes(y = INJURIES, x = EVTYPE)) + 
  geom_bar(stat="identity",fill="dark green") + 
  theme(axis.text = element_text(colour = "black")) +
  coord_flip() + ggtitle("Top 10 Total Injuries by Severe Weather Events in the U.S.") + 
  xlab("Severe Weather Events Type") + ylab("Number of Injuries")
```
----

####Impact on Economy
##### Firstly, the property damage and crop damage data need to be converted into comparable numerical forms according to the meaning of units described in the code book (Storm Events). Both PROPDMGEXP and CROPDMGEXP columns represents a multiplier for each observation where we have Hundred (H), Thousand (K), Million (M) and Billion (B). Therefore, a function called act_dollars is created to do the conversion into corresponding exponents.

```r
act_dollars <- function(EXP,DMG){
  EXP <- gsub("H",2,EXP,ignore.case=F)
  EXP <- gsub("K",3,EXP,ignore.case=F)
  EXP <- gsub("M",6,EXP,ignore.case=T)
  EXP <- gsub("B",9,EXP,ignore.case=T)
  EXP <- gsub("\\D",0,EXP,ignore.case=T)
  EXP[is.na(EXP)] <- 0
  value = DMG*10^(as.numeric(EXP))
  return(value)
}
```
##### The impacts on economy are investigated for PROPERTY DAMAGES and CROP DAMAGES

```r
propdmg <- storm[,c("EVTYPE","PROPDMG","PROPDMGEXP")]
propdmg$PDMG <- act_dollars(propdmg$PROPDMGEXP,propdmg$PROPDMG)
propdmg <- aggregate(cbind(PDMG)~EVTYPE,propdmg, sum)
propdmg <- propdmg[order(propdmg$PDMG,decreasing=T),]
top10propdmg <- head(propdmg,10)
cropdmg <- storm[,c("EVTYPE","CROPDMG","CROPDMGEXP")]
cropdmg$CDMG <- act_dollars(cropdmg$CROPDMGEXP, cropdmg$CROPDMG)
cropdmg <- aggregate(cbind(CDMG)~EVTYPE,cropdmg, sum)
cropdmg <- cropdmg[order(cropdmg$CDMG,decreasing=T),]
top10cropdmg <- head(cropdmg,10)
```

##### R code chunks for plotting of Property Damages and Crop Damages

```r
top10propdmg <- top10propdmg[order(top10propdmg$PDMG,decreasing=F),]
top10cropdmg <- top10cropdmg[order(top10cropdmg$CDMG,decreasing=F),]
top10propdmg$EVTYPE <- factor(top10propdmg$EVTYPE, levels = top10propdmg$EVTYPE)
top10cropdmg$EVTYPE <- factor(top10cropdmg$EVTYPE, levels = top10cropdmg$EVTYPE)

propplot <- ggplot(top10propdmg, aes(y = PDMG/1e9, x = EVTYPE)) + 
  geom_bar(stat="identity",fill="Maroon") +
  theme(axis.text = element_text(colour = "black")) +
  coord_flip() + ggtitle("Top 10 Total Property Damages (USD Billion) by Severe Weather Events in the U.S.") + 
  xlab("Severe Weather Events Type") + ylab("Damages (USD Billion)")

cropplot <- ggplot(top10cropdmg, aes(y = CDMG/1e9, x = EVTYPE)) + 
  geom_bar(stat="identity",fill="Gold") + 
  theme(axis.text = element_text(colour = "black")) +
  coord_flip() + ggtitle("Top 10 Total Crop Damages (USD Billion) by Severe Weather Events in the U.S.") + 
  xlab("Severe Weather Events Type") + ylab("Damages (USD Billion)")
```

----

###Results
####Impact on Population Health
##### The Top 10 Total Fatalities by Severe Weather Events in the U.S. is shown in results below

```r
print(top10fat[order(top10fat$FATALITIES,decreasing=T),],row.names=c(1:10))
```

```
##            EVTYPE FATALITIES
## 1         TORNADO       5633
## 2  EXCESSIVE HEAT       1903
## 3     FLASH FLOOD        978
## 4            HEAT        937
## 5       LIGHTNING        816
## 6       TSTM WIND        504
## 7           FLOOD        470
## 8     RIP CURRENT        368
## 9       HIGH WIND        248
## 10      AVALANCHE        224
```
##### The Top 10 Total Injuries by Severe Weather Events in the U.S. is shown in results below

```r
print(top10inj[order(top10inj$INJURIES,decreasing=T),],row.names=c(1:10))
```

```
##               EVTYPE INJURIES
## 1            TORNADO    91346
## 2          TSTM WIND     6957
## 3              FLOOD     6789
## 4     EXCESSIVE HEAT     6525
## 5          LIGHTNING     5230
## 6               HEAT     2100
## 7          ICE STORM     1975
## 8        FLASH FLOOD     1777
## 9  THUNDERSTORM WIND     1488
## 10              HAIL     1361
```
##### Both the results are plotted in the horizontal bar charts as shown below

```r
grid.arrange(fatplot, injplot, ncol = 2)
```

![](PA2_template_files/figure-html/unnamed-chunk-11-1.png) 
----
####Impact on Economy

##### The Top 10 Total Property Damages (USD) by Severe Weather Events in the U.S. is shown in results below

```r
print(top10propdmg[order(top10propdmg$PDMG,decreasing=T),],row.names=c(1:10))
```

```
##               EVTYPE         PDMG
## 1              FLOOD 144657709807
## 2  HURRICANE/TYPHOON  69305840000
## 3            TORNADO  56947380677
## 4        STORM SURGE  43323536000
## 5        FLASH FLOOD  16822673979
## 6               HAIL  15735267513
## 7          HURRICANE  11868319010
## 8     TROPICAL STORM   7703890550
## 9       WINTER STORM   6688497251
## 10         HIGH WIND   5270046295
```
##### The Top 10 Total Crop Damages (USD) by Severe Weather Events in the U.S. is shown in results below

```r
print(top10cropdmg[order(top10cropdmg$CDMG,decreasing=T),],row.names=c(1:10))
```

```
##               EVTYPE        CDMG
## 1            DROUGHT 13972566000
## 2              FLOOD  5661968450
## 3        RIVER FLOOD  5029459000
## 4          ICE STORM  5022113500
## 5               HAIL  3025537890
## 6          HURRICANE  2741910000
## 7  HURRICANE/TYPHOON  2607872800
## 8        FLASH FLOOD  1421317100
## 9       EXTREME COLD  1292973000
## 10      FROST/FREEZE  1094086000
```

##### Both the results are plotted in the horizontal bar charts as shown below

```r
grid.arrange(propplot, cropplot, ncol = 2)
```

![](PA2_template_files/figure-html/unnamed-chunk-14-1.png) 
