Education Across The United States of America
================
Pavel Olariu po3787

### INTRODUCTION

``` r
library(tidyverse)
library(mosaicData)
library(carData)
States
```

    ##    region   pop SATV SATM percent dollars pay
    ## AL    ESC  4041  470  514       8   3.648  27
    ## AK    PAC   550  438  476      42   7.887  43
    ## AZ    MTN  3665  445  497      25   4.231  30
    ## AR    WSC  2351  470  511       6   3.334  23
    ## CA    PAC 29760  419  484      45   4.826  39
    ## CO    MTN  3294  456  513      28   4.809  31
    ## CN     NE  3287  430  471      74   7.914  43
    ## DE     SA   666  433  470      58   6.016  35
    ## DC     SA   607  409  441      68   8.210  39
    ## FL     SA 12938  418  466      44   5.154  30
    ## GA     SA  6478  401  443      57   4.860  29
    ## HI    PAC  1108  404  481      52   5.008  32
    ## ID    MTN  1007  466  502      17   3.200  25
    ## IL    ENC 11431  466  528      16   5.062  34
    ##  [ reached 'max' / getOption("max.print") -- omitted 37 rows ]

``` r
SAT
```

    ##          state expend ratio salary frac verbal math  sat
    ## 1      Alabama  4.405  17.2 31.144    8    491  538 1029
    ## 2       Alaska  8.963  17.6 47.951   47    445  489  934
    ## 3      Arizona  4.778  19.3 32.175   27    448  496  944
    ## 4     Arkansas  4.459  17.1 28.934    6    482  523 1005
    ## 5   California  4.992  24.0 41.078   45    417  485  902
    ## 6     Colorado  5.443  18.4 34.571   29    462  518  980
    ## 7  Connecticut  8.817  14.4 50.045   81    431  477  908
    ## 8     Delaware  7.030  16.6 39.076   68    429  468  897
    ## 9      Florida  5.718  19.1 32.588   48    420  469  889
    ## 10     Georgia  5.193  16.3 32.291   65    406  448  854
    ## 11      Hawaii  6.078  17.9 38.518   57    407  482  889
    ## 12       Idaho  4.210  19.1 29.783   15    468  511  979
    ##  [ reached 'max' / getOption("max.print") -- omitted 38 rows ]

``` r
#install packages that allow library use and opened datasets for viewing and situating
```

##### The datasets in this analysis both pertain to educational statistics of all 50 states present in The United States of America. One dataset uses primary and secondary school information from the 1994-1995 school year, and the other from 1992 (which can be assumed to be from the 1991-1992 school year). This allows for two sets of different variables, classified by year, for the qualities of the datasets that are common, such as the verbal SAT score (verbal\[year\]), math SAT score (math\[year\]), total SAT score (sat\[year\]), salary of teachers in thousands of dollars (salary\[year\]), expenditure per individual pupil in thousands of dollars(pupilexpense\[year\]), and the percentage of graduating students who had taken the SAT (percent\[year\]). The variables that will be considered constant across both years will be the ratio of pupils to teachers (ratio), the U.S. census region of the state (region), and the population of the states in thousands of people (pop). The region abbreviation meanings are: ENC, East North Central; ESC, East South Central; MA, Mid-Atlantic; MTN, Mountain; NE, New England; PAC, Pacific; SA, South Atlantic;WNC, West North Central; WSC, West South Central.These are considered constant as either one or the other dataset does not mirror the variable, and so cannot be compared across year, but can be used as an aid in the general relation of the other variables. These datasets were taken from two packages already present in R, called “States” and “SAT”, which were created with data taken from the “Bureau of the Census” and the “Journal of Statistics Education”. These datasets are interesting to me because I like seeing how states vary in certain features, and which are the most educated and value their education and students the most. I think that there will be a correlation between population/region and the scores/money spent of education. I think that in the northern states there will be higher scores and more money spent on education because of the democratic leanings and culture of those states. I also think that states with lower populations will score higher and spend more money on education because they have less students to worry about and can focus on them more.

### TIDYING

``` r
states1992 <- States[-c(9),]
#deletes row describing Washington D.C. as this is not really a state and not included in the other dataset. 
states1992renamed <- states1992%>%
  rename(
    verbal1992 = SATV,
    math1992 = SATM,
    percent1992 = percent,
    pupilexpense1992 = dollars,
    salary1992 = pay
  )
#renamed variables for first set to represent the associated year in the variable name for easier comparison
states1995renamed <- SAT%>%
  rename(
    pupilexpense1995 = expend,
    salary1995 = salary,
    percent1995 = frac,
    verbal1995 = verbal,
    math1995 = math,
    sat1995 = sat
  )
#renamed variables for other dataset
states1992renamedv2 <- states1992renamed%>%
  mutate(sat1992 = math1992 + verbal1992)
#created a new variable for total sat score by adding verbal and math scores together
states1992renamedv3 <- states1992renamedv2%>%
  mutate(state = states1995renamed$state)
#added a variable of states in 1992 set because the state were being used as the row names instead of its variable, had to do it for easier joining.
states1992renamedv3
```

    ##    region   pop verbal1992 math1992 percent1992 pupilexpense1992 salary1992
    ## AL    ESC  4041        470      514           8            3.648         27
    ## AK    PAC   550        438      476          42            7.887         43
    ## AZ    MTN  3665        445      497          25            4.231         30
    ## AR    WSC  2351        470      511           6            3.334         23
    ## CA    PAC 29760        419      484          45            4.826         39
    ## CO    MTN  3294        456      513          28            4.809         31
    ## CN     NE  3287        430      471          74            7.914         43
    ## DE     SA   666        433      470          58            6.016         35
    ## FL     SA 12938        418      466          44            5.154         30
    ## GA     SA  6478        401      443          57            4.860         29
    ## HI    PAC  1108        404      481          52            5.008         32
    ##    sat1992       state
    ## AL     984     Alabama
    ## AK     914      Alaska
    ## AZ     942     Arizona
    ## AR     981    Arkansas
    ## CA     903  California
    ## CO     969    Colorado
    ## CN     901 Connecticut
    ## DE     903    Delaware
    ## FL     884     Florida
    ## GA     844     Georgia
    ## HI     885      Hawaii
    ##  [ reached 'max' / getOption("max.print") -- omitted 39 rows ]

``` r
states1995renamed
```

    ##          state pupilexpense1995 ratio salary1995 percent1995 verbal1995
    ## 1      Alabama            4.405  17.2     31.144           8        491
    ## 2       Alaska            8.963  17.6     47.951          47        445
    ## 3      Arizona            4.778  19.3     32.175          27        448
    ## 4     Arkansas            4.459  17.1     28.934           6        482
    ## 5   California            4.992  24.0     41.078          45        417
    ## 6     Colorado            5.443  18.4     34.571          29        462
    ## 7  Connecticut            8.817  14.4     50.045          81        431
    ## 8     Delaware            7.030  16.6     39.076          68        429
    ## 9      Florida            5.718  19.1     32.588          48        420
    ## 10     Georgia            5.193  16.3     32.291          65        406
    ## 11      Hawaii            6.078  17.9     38.518          57        407
    ## 12       Idaho            4.210  19.1     29.783          15        468
    ##    math1995 sat1995
    ## 1       538    1029
    ## 2       489     934
    ## 3       496     944
    ## 4       523    1005
    ## 5       485     902
    ## 6       518     980
    ## 7       477     908
    ## 8       468     897
    ## 9       469     889
    ## 10      448     854
    ## 11      482     889
    ## 12      511     979
    ##  [ reached 'max' / getOption("max.print") -- omitted 38 rows ]

##### The datesets were similar in a few variables, so these variables were changed to reflect the year they represent. Row containing Washington D.C. was deleted from one dataset as it was not included in the other. Common variables between the two were renamed to reflect the year they are from, as the educational statistics are from two different years. A total SAT score variable was created in one of the datasets to match the other. This was done by adding the verbal and math scores together. An explicit variable listing the names of the states was added to one of the datasets as they were the row names previously, and this would have prohibited simple joining of the datasets.

### JOINING

``` r
combinedstates <- states1995renamed%>%
  full_join(states1992renamedv3, by = "state")
combinedstates
```

    ##        state pupilexpense1995 ratio salary1995 percent1995 verbal1995 math1995
    ## 1    Alabama            4.405  17.2     31.144           8        491      538
    ## 2     Alaska            8.963  17.6     47.951          47        445      489
    ## 3    Arizona            4.778  19.3     32.175          27        448      496
    ## 4   Arkansas            4.459  17.1     28.934           6        482      523
    ## 5 California            4.992  24.0     41.078          45        417      485
    ## 6   Colorado            5.443  18.4     34.571          29        462      518
    ##   sat1995 region   pop verbal1992 math1992 percent1992 pupilexpense1992
    ## 1    1029    ESC  4041        470      514           8            3.648
    ## 2     934    PAC   550        438      476          42            7.887
    ## 3     944    MTN  3665        445      497          25            4.231
    ## 4    1005    WSC  2351        470      511           6            3.334
    ## 5     902    PAC 29760        419      484          45            4.826
    ## 6     980    MTN  3294        456      513          28            4.809
    ##   salary1992 sat1992
    ## 1         27     984
    ## 2         43     914
    ## 3         30     942
    ## 4         23     981
    ## 5         39     903
    ## 6         31     969
    ##  [ reached 'max' / getOption("max.print") -- omitted 44 rows ]

``` r
#joined both sets using full join to ensure all columns and rows were kept, joined with the key variable designated as 'states' 
```

##### The joined dataset incorporates all the variables present in the two individual datasets, and uses the states as a base for joining. No problems were encountered in joining the two as the states were in identical alphabetical order, and there were exactly 50 observations in each dataset, corresponding to the 50 states.

### SUMMARY STATISTICS

``` r
library(kableExtra)
#a useful package that can help with tables
combinedstates <- combinedstates%>%
  mutate(averagesat = (sat1992 + sat1995)/2)%>%
  mutate(averagepupilexpense = (pupilexpense1992 + pupilexpense1995)/2)%>%
  mutate(averagesalary = (salary1992 + salary1995)/2)%>%
  mutate(averagepercent = (percent1992 + percent1995)/2)%>%
  mutate(averagesatverbal = (verbal1992 + verbal1995)/2)%>%
  mutate(averagesatmath = (math1992 + math1995)/2)
#created averages for all the variables that were common across both years
#combinedstates_withpercents <- combinedstates%>%
#  mutate(pctchange_sat = (sat1992 + sat1995)*100)%>%
#  mutate(pctchange_pupilexpense = (pupilexpense1995/pupilexpense1992 - 1)*100)%>%
#  mutate(pctchange_salary = (salary1995/salary1992 - 1)*100)%>%
#  mutate(pctchange_percent = (percent1995/percent1992 - 1)*100)%>%
#  mutate(pctchange_satverbal = (verbal1995/verbal1992 - 1)*100)%>%
#  mutate(pctchange_satmath = (math1995/math1992 - 1)*100)%>%
#  mutate(pctchange_averagesat = (sat1995/sat1992 - 1)*100)
#created a percent change variable that shows the change from 1992 to 1995 of the variables(talen out caused too many problems)
combinedstates %>%
  group_by(region)%>%
  summarise(mean_ratio = mean(ratio),mean_averagesat = mean(averagesat),mean_averagepupilexpense = mean(averagepupilexpense),mean_averagesalary = mean(averagesalary),mean_averagepercent = mean(averagepercent), mean_averagesatverbal = mean(averagesatverbal),mean_averagesatmath = mean(averagesatmath), mean_averagesat = mean(averagesat), mean_pop = mean(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
region
</th>
<th style="text-align:right;">
mean\_ratio
</th>
<th style="text-align:right;">
mean\_averagesat
</th>
<th style="text-align:right;">
mean\_averagepupilexpense
</th>
<th style="text-align:right;">
mean\_averagesalary
</th>
<th style="text-align:right;">
mean\_averagepercent
</th>
<th style="text-align:right;">
mean\_averagesatverbal
</th>
<th style="text-align:right;">
mean\_averagesatmath
</th>
<th style="text-align:right;">
mean\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ENC
</td>
<td style="text-align:right;">
17.48000
</td>
<td style="text-align:right;">
980.8000
</td>
<td style="text-align:right;">
5.900300
</td>
<td style="text-align:right;">
36.16590
</td>
<td style="text-align:right;">
22.900000
</td>
<td style="text-align:right;">
460.2000
</td>
<td style="text-align:right;">
520.6000
</td>
<td style="text-align:right;">
8401.800
</td>
</tr>
<tr>
<td style="text-align:left;">
ESC
</td>
<td style="text-align:right;">
17.57500
</td>
<td style="text-align:right;">
1010.7500
</td>
<td style="text-align:right;">
4.144625
</td>
<td style="text-align:right;">
28.83700
</td>
<td style="text-align:right;">
8.625000
</td>
<td style="text-align:right;">
483.0000
</td>
<td style="text-align:right;">
527.7500
</td>
<td style="text-align:right;">
3794.000
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
15.36667
</td>
<td style="text-align:right;">
887.6667
</td>
<td style="text-align:right;">
8.449833
</td>
<td style="text-align:right;">
42.36817
</td>
<td style="text-align:right;">
69.500000
</td>
<td style="text-align:right;">
418.0000
</td>
<td style="text-align:right;">
469.6667
</td>
<td style="text-align:right;">
12534.000
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
18.52500
</td>
<td style="text-align:right;">
982.6875
</td>
<td style="text-align:right;">
4.647938
</td>
<td style="text-align:right;">
29.56312
</td>
<td style="text-align:right;">
18.187500
</td>
<td style="text-align:right;">
465.8750
</td>
<td style="text-align:right;">
516.8125
</td>
<td style="text-align:right;">
1707.375
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
14.51667
</td>
<td style="text-align:right;">
902.5000
</td>
<td style="text-align:right;">
6.750167
</td>
<td style="text-align:right;">
36.63892
</td>
<td style="text-align:right;">
69.500000
</td>
<td style="text-align:right;">
430.0833
</td>
<td style="text-align:right;">
472.4167
</td>
<td style="text-align:right;">
2201.000
</td>
</tr>
<tr>
<td style="text-align:left;">
PAC
</td>
<td style="text-align:right;">
19.92000
</td>
<td style="text-align:right;">
915.7000
</td>
<td style="text-align:right;">
6.043200
</td>
<td style="text-align:right;">
38.12530
</td>
<td style="text-align:right;">
48.000000
</td>
<td style="text-align:right;">
429.7000
</td>
<td style="text-align:right;">
486.0000
</td>
<td style="text-align:right;">
7825.400
</td>
</tr>
<tr>
<td style="text-align:left;">
SA
</td>
<td style="text-align:right;">
16.37500
</td>
<td style="text-align:right;">
883.0000
</td>
<td style="text-align:right;">
5.515187
</td>
<td style="text-align:right;">
32.41369
</td>
<td style="text-align:right;">
52.812500
</td>
<td style="text-align:right;">
420.0625
</td>
<td style="text-align:right;">
462.9375
</td>
<td style="text-align:right;">
5369.875
</td>
</tr>
<tr>
<td style="text-align:left;">
WNC
</td>
<td style="text-align:right;">
15.44286
</td>
<td style="text-align:right;">
1058.2857
</td>
<td style="text-align:right;">
4.963357
</td>
<td style="text-align:right;">
28.96736
</td>
<td style="text-align:right;">
8.071429
</td>
<td style="text-align:right;">
498.7143
</td>
<td style="text-align:right;">
559.5714
</td>
<td style="text-align:right;">
2522.857
</td>
</tr>
<tr>
<td style="text-align:left;">
WSC
</td>
<td style="text-align:right;">
16.27500
</td>
<td style="text-align:right;">
974.3750
</td>
<td style="text-align:right;">
4.326625
</td>
<td style="text-align:right;">
26.97375
</td>
<td style="text-align:right;">
17.125000
</td>
<td style="text-align:right;">
464.3750
</td>
<td style="text-align:right;">
510.0000
</td>
<td style="text-align:right;">
6676.000
</td>
</tr>
</tbody>
</table>

``` r
#  calculated the mean for all variables and grouped them by the region the states were in and then put these in a pretty table using kable
combinedstates %>%
  group_by(region)%>%
  summarise(sd_ratio = sd(ratio),sd_averagesat = sd(averagesat),sd_averagepupilexpense = sd(averagepupilexpense),sd_averagesalary = sd(averagesalary),sd_averagepercent = sd(averagepercent), sd_averagesatverbal = sd(averagesatverbal),sd_averagesatmath = sd(averagesatmath), sd_averagesat = sd(averagesat), sd_pop = sd(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
region
</th>
<th style="text-align:right;">
sd\_ratio
</th>
<th style="text-align:right;">
sd\_averagesat
</th>
<th style="text-align:right;">
sd\_averagepupilexpense
</th>
<th style="text-align:right;">
sd\_averagesalary
</th>
<th style="text-align:right;">
sd\_averagepercent
</th>
<th style="text-align:right;">
sd\_averagesatverbal
</th>
<th style="text-align:right;">
sd\_averagesatmath
</th>
<th style="text-align:right;">
sd\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ENC
</td>
<td style="text-align:right;">
1.5943651
</td>
<td style="text-align:right;">
66.910948
</td>
<td style="text-align:right;">
0.4012098
</td>
<td style="text-align:right;">
2.317923
</td>
<td style="text-align:right;">
19.122631
</td>
<td style="text-align:right;">
29.829935
</td>
<td style="text-align:right;">
37.184338
</td>
<td style="text-align:right;">
3018.2188
</td>
</tr>
<tr>
<td style="text-align:left;">
ESC
</td>
<td style="text-align:right;">
0.7135592
</td>
<td style="text-align:right;">
11.891874
</td>
<td style="text-align:right;">
0.4670140
</td>
<td style="text-align:right;">
2.379081
</td>
<td style="text-align:right;">
3.497022
</td>
<td style="text-align:right;">
6.620675
</td>
<td style="text-align:right;">
5.299371
</td>
<td style="text-align:right;">
955.0951
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
1.6563011
</td>
<td style="text-align:right;">
6.525591
</td>
<td style="text-align:right;">
1.4246432
</td>
<td style="text-align:right;">
2.292805
</td>
<td style="text-align:right;">
2.500000
</td>
<td style="text-align:right;">
2.179450
</td>
<td style="text-align:right;">
6.934215
</td>
<td style="text-align:right;">
5160.9813
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
2.7819572
</td>
<td style="text-align:right;">
41.184201
</td>
<td style="text-align:right;">
0.8193933
</td>
<td style="text-align:right;">
2.647333
</td>
<td style="text-align:right;">
8.717542
</td>
<td style="text-align:right;">
20.928023
</td>
<td style="text-align:right;">
20.791374
</td>
<td style="text-align:right;">
1166.7384
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
0.6823977
</td>
<td style="text-align:right;">
15.996875
</td>
<td style="text-align:right;">
0.9578810
</td>
<td style="text-align:right;">
5.934275
</td>
<td style="text-align:right;">
5.830952
</td>
<td style="text-align:right;">
6.909535
</td>
<td style="text-align:right;">
9.265078
</td>
<td style="text-align:right;">
2097.1916
</td>
</tr>
<tr>
<td style="text-align:left;">
PAC
</td>
<td style="text-align:right;">
2.5587106
</td>
<td style="text-align:right;">
20.271902
</td>
<td style="text-align:right;">
1.3751506
</td>
<td style="text-align:right;">
4.653343
</td>
<td style="text-align:right;">
4.227884
</td>
<td style="text-align:right;">
17.016903
</td>
<td style="text-align:right;">
4.500000
</td>
<td style="text-align:right;">
12377.0755
</td>
</tr>
<tr>
<td style="text-align:left;">
SA
</td>
<td style="text-align:right;">
1.3905292
</td>
<td style="text-align:right;">
32.841611
</td>
<td style="text-align:right;">
0.7533109
</td>
<td style="text-align:right;">
3.833027
</td>
<td style="text-align:right;">
15.836074
</td>
<td style="text-align:right;">
16.140981
</td>
<td style="text-align:right;">
16.901897
</td>
<td style="text-align:right;">
3771.5100
</td>
</tr>
<tr>
<td style="text-align:left;">
WNC
</td>
<td style="text-align:right;">
1.0390014
</td>
<td style="text-align:right;">
26.044605
</td>
<td style="text-align:right;">
0.5432636
</td>
<td style="text-align:right;">
3.719172
</td>
<td style="text-align:right;">
2.805182
</td>
<td style="text-align:right;">
11.228153
</td>
<td style="text-align:right;">
15.476941
</td>
<td style="text-align:right;">
1732.3246
</td>
</tr>
<tr>
<td style="text-align:left;">
WSC
</td>
<td style="text-align:right;">
0.7932003
</td>
<td style="text-align:right;">
61.209170
</td>
<td style="text-align:right;">
0.3427347
</td>
<td style="text-align:right;">
1.761797
</td>
<td style="text-align:right;">
18.304713
</td>
<td style="text-align:right;">
32.438082
</td>
<td style="text-align:right;">
28.818397
</td>
<td style="text-align:right;">
6916.5305
</td>
</tr>
</tbody>
</table>

``` r
#created a table with a summary of standard deviations for all variables grouped by region
combinedstates %>% 
  group_by(region)%>%
  summarise(quantile_ratio = quantile(ratio, probs = c(0.5)),quantile_averagesat = quantile(averagesat, probs = c(0.5)),quantile_averagepupilexpense = quantile(averagepupilexpense, probs = c(0.5)),quantile_averagesalary = quantile(averagesalary, probs = c(0.5)),quantile_averagepercent = quantile(averagepercent, probs = c(0.5)), quantile_averagesatverbal = quantile(averagesatverbal, probs = c(0.5)),quantile_averagesatmath = quantile(averagesatmath, probs = c(0.5)), quantile_averagesat = quantile(averagesat, probs = c(0.5)), quantile_pop = quantile(pop, probs = c(0.5)))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
region
</th>
<th style="text-align:right;">
quantile\_ratio
</th>
<th style="text-align:right;">
quantile\_averagesat
</th>
<th style="text-align:right;">
quantile\_averagepupilexpense
</th>
<th style="text-align:right;">
quantile\_averagesalary
</th>
<th style="text-align:right;">
quantile\_averagepercent
</th>
<th style="text-align:right;">
quantile\_averagesatverbal
</th>
<th style="text-align:right;">
quantile\_averagesatmath
</th>
<th style="text-align:right;">
quantile\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ENC
</td>
<td style="text-align:right;">
17.30
</td>
<td style="text-align:right;">
1000.50
</td>
<td style="text-align:right;">
5.90050
</td>
<td style="text-align:right;">
35.37300
</td>
<td style="text-align:right;">
14.50
</td>
<td style="text-align:right;">
469.00
</td>
<td style="text-align:right;">
531.50
</td>
<td style="text-align:right;">
9295.0
</td>
</tr>
<tr>
<td style="text-align:left;">
ESC
</td>
<td style="text-align:right;">
17.35
</td>
<td style="text-align:right;">
1011.25
</td>
<td style="text-align:right;">
4.03700
</td>
<td style="text-align:right;">
29.65525
</td>
<td style="text-align:right;">
9.25
</td>
<td style="text-align:right;">
483.50
</td>
<td style="text-align:right;">
527.75
</td>
<td style="text-align:right;">
3863.0
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
15.20
</td>
<td style="text-align:right;">
887.00
</td>
<td style="text-align:right;">
9.06150
</td>
<td style="text-align:right;">
42.04350
</td>
<td style="text-align:right;">
69.50
</td>
<td style="text-align:right;">
419.00
</td>
<td style="text-align:right;">
471.50
</td>
<td style="text-align:right;">
11882.0
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
18.55
</td>
<td style="text-align:right;">
981.75
</td>
<td style="text-align:right;">
4.68900
</td>
<td style="text-align:right;">
28.76750
</td>
<td style="text-align:right;">
18.25
</td>
<td style="text-align:right;">
467.00
</td>
<td style="text-align:right;">
518.75
</td>
<td style="text-align:right;">
1358.5
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
14.55
</td>
<td style="text-align:right;">
901.25
</td>
<td style="text-align:right;">
6.53200
</td>
<td style="text-align:right;">
35.80025
</td>
<td style="text-align:right;">
67.25
</td>
<td style="text-align:right;">
429.25
</td>
<td style="text-align:right;">
471.50
</td>
<td style="text-align:right;">
1168.5
</td>
</tr>
<tr>
<td style="text-align:left;">
PAC
</td>
<td style="text-align:right;">
19.90
</td>
<td style="text-align:right;">
924.00
</td>
<td style="text-align:right;">
5.54300
</td>
<td style="text-align:right;">
35.27750
</td>
<td style="text-align:right;">
46.00
</td>
<td style="text-align:right;">
440.00
</td>
<td style="text-align:right;">
484.50
</td>
<td style="text-align:right;">
2842.0
</td>
</tr>
<tr>
<td style="text-align:left;">
SA
</td>
<td style="text-align:right;">
16.35
</td>
<td style="text-align:right;">
891.00
</td>
<td style="text-align:right;">
5.38975
</td>
<td style="text-align:right;">
30.96975
</td>
<td style="text-align:right;">
59.25
</td>
<td style="text-align:right;">
422.75
</td>
<td style="text-align:right;">
468.25
</td>
<td style="text-align:right;">
5484.0
</td>
</tr>
<tr>
<td style="text-align:left;">
WNC
</td>
<td style="text-align:right;">
15.30
</td>
<td style="text-align:right;">
1052.00
</td>
<td style="text-align:right;">
5.15800
</td>
<td style="text-align:right;">
29.59450
</td>
<td style="text-align:right;">
9.50
</td>
<td style="text-align:right;">
497.50
</td>
<td style="text-align:right;">
559.00
</td>
<td style="text-align:right;">
2478.0
</td>
</tr>
<tr>
<td style="text-align:left;">
WSC
</td>
<td style="text-align:right;">
16.25
</td>
<td style="text-align:right;">
1000.00
</td>
<td style="text-align:right;">
4.34000
</td>
<td style="text-align:right;">
26.15825
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
478.50
</td>
<td style="text-align:right;">
521.50
</td>
<td style="text-align:right;">
3683.0
</td>
</tr>
</tbody>
</table>

``` r
#created a table with quantile summary which gives value below which 50 percent of the data falls, which is basically the median. grouped by region
combinedstates %>% 
  group_by(region)%>%
  summarise(min_ratio = min(ratio),min_averagesat = min(averagesat),min_averagepupilexpense = min(averagepupilexpense),min_averagesalary = min(averagesalary),min_averagepercent = min(averagepercent), min_averagesatverbal = min(averagesatverbal),min_averagesatmath = min(averagesatmath), min_averagesat = min(averagesat), min_pop = min(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
region
</th>
<th style="text-align:right;">
min\_ratio
</th>
<th style="text-align:right;">
min\_averagesat
</th>
<th style="text-align:right;">
min\_averagepupilexpense
</th>
<th style="text-align:right;">
min\_averagesalary
</th>
<th style="text-align:right;">
min\_averagepercent
</th>
<th style="text-align:right;">
min\_averagesatverbal
</th>
<th style="text-align:right;">
min\_averagesatmath
</th>
<th style="text-align:right;">
min\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ENC
</td>
<td style="text-align:right;">
15.9
</td>
<td style="text-align:right;">
874.5
</td>
<td style="text-align:right;">
5.4385
</td>
<td style="text-align:right;">
34.3925
</td>
<td style="text-align:right;">
10.0
</td>
<td style="text-align:right;">
411.5
</td>
<td style="text-align:right;">
463.0
</td>
<td style="text-align:right;">
4892
</td>
</tr>
<tr>
<td style="text-align:left;">
ESC
</td>
<td style="text-align:right;">
17.0
</td>
<td style="text-align:right;">
996.5
</td>
<td style="text-align:right;">
3.7010
</td>
<td style="text-align:right;">
25.4090
</td>
<td style="text-align:right;">
4.0
</td>
<td style="text-align:right;">
475.0
</td>
<td style="text-align:right;">
521.5
</td>
<td style="text-align:right;">
2573
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
13.8
</td>
<td style="text-align:right;">
881.5
</td>
<td style="text-align:right;">
6.8215
</td>
<td style="text-align:right;">
40.2550
</td>
<td style="text-align:right;">
67.0
</td>
<td style="text-align:right;">
415.5
</td>
<td style="text-align:right;">
462.0
</td>
<td style="text-align:right;">
7730
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
14.9
</td>
<td style="text-align:right;">
919.0
</td>
<td style="text-align:right;">
3.3245
</td>
<td style="text-align:right;">
27.0410
</td>
<td style="text-align:right;">
4.5
</td>
<td style="text-align:right;">
434.0
</td>
<td style="text-align:right;">
485.0
</td>
<td style="text-align:right;">
454
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
13.8
</td>
<td style="text-align:right;">
885.5
</td>
<td style="text-align:right;">
5.6815
</td>
<td style="text-align:right;">
29.9860
</td>
<td style="text-align:right;">
64.0
</td>
<td style="text-align:right;">
423.5
</td>
<td style="text-align:right;">
462.0
</td>
<td style="text-align:right;">
563
</td>
</tr>
<tr>
<td style="text-align:left;">
PAC
</td>
<td style="text-align:right;">
17.6
</td>
<td style="text-align:right;">
887.0
</td>
<td style="text-align:right;">
4.9090
</td>
<td style="text-align:right;">
34.5755
</td>
<td style="text-align:right;">
44.5
</td>
<td style="text-align:right;">
405.5
</td>
<td style="text-align:right;">
481.5
</td>
<td style="text-align:right;">
550
</td>
</tr>
<tr>
<td style="text-align:left;">
SA
</td>
<td style="text-align:right;">
14.6
</td>
<td style="text-align:right;">
839.0
</td>
<td style="text-align:right;">
4.5620
</td>
<td style="text-align:right;">
28.9720
</td>
<td style="text-align:right;">
16.0
</td>
<td style="text-align:right;">
399.0
</td>
<td style="text-align:right;">
440.0
</td>
<td style="text-align:right;">
666
</td>
</tr>
<tr>
<td style="text-align:left;">
WNC
</td>
<td style="text-align:right;">
14.4
</td>
<td style="text-align:right;">
1020.0
</td>
<td style="text-align:right;">
4.2300
</td>
<td style="text-align:right;">
23.9970
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
484.0
</td>
<td style="text-align:right;">
536.0
</td>
<td style="text-align:right;">
639
</td>
</tr>
<tr>
<td style="text-align:left;">
WSC
</td>
<td style="text-align:right;">
15.5
</td>
<td style="text-align:right;">
883.5
</td>
<td style="text-align:right;">
3.8965
</td>
<td style="text-align:right;">
25.9670
</td>
<td style="text-align:right;">
6.0
</td>
<td style="text-align:right;">
416.0
</td>
<td style="text-align:right;">
467.5
</td>
<td style="text-align:right;">
2351
</td>
</tr>
</tbody>
</table>

``` r
#created a table with a summary of the minimum values for each variable, grouped by region
combinedstates %>% 
  group_by(region)%>%
  summarise(max_ratio = max(ratio),max_averagesat = max(averagesat),max_averagepupilexpense = max(averagepupilexpense),max_averagesalary = max(averagesalary),max_averagepercent = max(averagepercent), max_averagesatverbal = max(averagesatverbal),max_averagesatmath = max(averagesatmath), max_averagesat = max(averagesat), max_pop = max(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
region
</th>
<th style="text-align:right;">
max\_ratio
</th>
<th style="text-align:right;">
max\_averagesat
</th>
<th style="text-align:right;">
max\_averagepupilexpense
</th>
<th style="text-align:right;">
max\_averagesalary
</th>
<th style="text-align:right;">
max\_averagepercent
</th>
<th style="text-align:right;">
max\_averagesatverbal
</th>
<th style="text-align:right;">
max\_averagesatmath
</th>
<th style="text-align:right;">
max\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ENC
</td>
<td style="text-align:right;">
20.1
</td>
<td style="text-align:right;">
1046.0
</td>
<td style="text-align:right;">
6.4380
</td>
<td style="text-align:right;">
39.9475
</td>
<td style="text-align:right;">
56.0
</td>
<td style="text-align:right;">
488.5
</td>
<td style="text-align:right;">
557.5
</td>
<td style="text-align:right;">
11431
</td>
</tr>
<tr>
<td style="text-align:left;">
ESC
</td>
<td style="text-align:right;">
18.6
</td>
<td style="text-align:right;">
1024.0
</td>
<td style="text-align:right;">
4.8035
</td>
<td style="text-align:right;">
30.6285
</td>
<td style="text-align:right;">
12.0
</td>
<td style="text-align:right;">
490.0
</td>
<td style="text-align:right;">
534.0
</td>
<td style="text-align:right;">
4877
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
17.1
</td>
<td style="text-align:right;">
894.5
</td>
<td style="text-align:right;">
9.4665
</td>
<td style="text-align:right;">
44.8060
</td>
<td style="text-align:right;">
72.0
</td>
<td style="text-align:right;">
419.5
</td>
<td style="text-align:right;">
475.5
</td>
<td style="text-align:right;">
17990
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
24.3
</td>
<td style="text-align:right;">
1053.5
</td>
<td style="text-align:right;">
5.7075
</td>
<td style="text-align:right;">
33.4180
</td>
<td style="text-align:right;">
28.5
</td>
<td style="text-align:right;">
502.5
</td>
<td style="text-align:right;">
551.0
</td>
<td style="text-align:right;">
3665
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
15.6
</td>
<td style="text-align:right;">
931.5
</td>
<td style="text-align:right;">
8.3655
</td>
<td style="text-align:right;">
46.5225
</td>
<td style="text-align:right;">
77.5
</td>
<td style="text-align:right;">
443.0
</td>
<td style="text-align:right;">
488.5
</td>
<td style="text-align:right;">
6016
</td>
</tr>
<tr>
<td style="text-align:left;">
PAC
</td>
<td style="text-align:right;">
24.0
</td>
<td style="text-align:right;">
935.0
</td>
<td style="text-align:right;">
8.4250
</td>
<td style="text-align:right;">
45.4755
</td>
<td style="text-align:right;">
54.5
</td>
<td style="text-align:right;">
443.5
</td>
<td style="text-align:right;">
491.5
</td>
<td style="text-align:right;">
29760
</td>
</tr>
<tr>
<td style="text-align:left;">
SA
</td>
<td style="text-align:right;">
19.1
</td>
<td style="text-align:right;">
932.5
</td>
<td style="text-align:right;">
6.7145
</td>
<td style="text-align:right;">
39.3305
</td>
<td style="text-align:right;">
63.0
</td>
<td style="text-align:right;">
445.5
</td>
<td style="text-align:right;">
487.0
</td>
<td style="text-align:right;">
12938
</td>
</tr>
<tr>
<td style="text-align:left;">
WNC
</td>
<td style="text-align:right;">
17.5
</td>
<td style="text-align:right;">
1093.5
</td>
<td style="text-align:right;">
5.6300
</td>
<td style="text-align:right;">
34.4740
</td>
<td style="text-align:right;">
11.5
</td>
<td style="text-align:right;">
513.5
</td>
<td style="text-align:right;">
580.0
</td>
<td style="text-align:right;">
5117
</td>
</tr>
<tr>
<td style="text-align:left;">
WSC
</td>
<td style="text-align:right;">
17.1
</td>
<td style="text-align:right;">
1014.0
</td>
<td style="text-align:right;">
4.7300
</td>
<td style="text-align:right;">
29.6115
</td>
<td style="text-align:right;">
44.5
</td>
<td style="text-align:right;">
484.5
</td>
<td style="text-align:right;">
529.5
</td>
<td style="text-align:right;">
16987
</td>
</tr>
</tbody>
</table>

``` r
#created a table with a summary of the maximum values for each variable, grouped by states. 
combinedstates %>% 
  group_by(region)%>%
  summarise(iqr_ratio = IQR(ratio),iqr_averagesat = IQR(averagesat),iqr_averagepupilexpense = IQR(averagepupilexpense),iqr_averagesalary = IQR(averagesalary),iqr_averagepercent = IQR(averagepercent), iqr_averagesatverbal = IQR(averagesatverbal),iqr_averagesatmath = IQR(averagesatmath), iqr_averagesat = IQR(averagesat), iqr_pop = IQR(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
region
</th>
<th style="text-align:right;">
iqr\_ratio
</th>
<th style="text-align:right;">
iqr\_averagesat
</th>
<th style="text-align:right;">
iqr\_averagepupilexpense
</th>
<th style="text-align:right;">
iqr\_averagesalary
</th>
<th style="text-align:right;">
iqr\_averagepercent
</th>
<th style="text-align:right;">
iqr\_averagesatverbal
</th>
<th style="text-align:right;">
iqr\_averagesatmath
</th>
<th style="text-align:right;">
iqr\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ENC
</td>
<td style="text-align:right;">
0.900
</td>
<td style="text-align:right;">
59.000
</td>
<td style="text-align:right;">
0.526500
</td>
<td style="text-align:right;">
2.314500
</td>
<td style="text-align:right;">
11.000
</td>
<td style="text-align:right;">
22.000
</td>
<td style="text-align:right;">
37.00
</td>
<td style="text-align:right;">
5303.00
</td>
</tr>
<tr>
<td style="text-align:left;">
ESC
</td>
<td style="text-align:right;">
0.625
</td>
<td style="text-align:right;">
14.000
</td>
<td style="text-align:right;">
0.291375
</td>
<td style="text-align:right;">
2.179750
</td>
<td style="text-align:right;">
3.875
</td>
<td style="text-align:right;">
8.250
</td>
<td style="text-align:right;">
5.75
</td>
<td style="text-align:right;">
843.00
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
1.650
</td>
<td style="text-align:right;">
6.500
</td>
<td style="text-align:right;">
1.322500
</td>
<td style="text-align:right;">
2.275500
</td>
<td style="text-align:right;">
2.500
</td>
<td style="text-align:right;">
2.000
</td>
<td style="text-align:right;">
6.75
</td>
<td style="text-align:right;">
5130.00
</td>
</tr>
<tr>
<td style="text-align:left;">
MTN
</td>
<td style="text-align:right;">
2.175
</td>
<td style="text-align:right;">
35.375
</td>
<td style="text-align:right;">
0.899375
</td>
<td style="text-align:right;">
4.156750
</td>
<td style="text-align:right;">
14.750
</td>
<td style="text-align:right;">
16.125
</td>
<td style="text-align:right;">
24.75
</td>
<td style="text-align:right;">
1160.75
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
0.825
</td>
<td style="text-align:right;">
11.250
</td>
<td style="text-align:right;">
0.944500
</td>
<td style="text-align:right;">
5.802000
</td>
<td style="text-align:right;">
8.875
</td>
<td style="text-align:right;">
4.500
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
1742.75
</td>
</tr>
<tr>
<td style="text-align:left;">
PAC
</td>
<td style="text-align:right;">
2.300
</td>
<td style="text-align:right;">
27.500
</td>
<td style="text-align:right;">
0.388000
</td>
<td style="text-align:right;">
4.780000
</td>
<td style="text-align:right;">
5.000
</td>
<td style="text-align:right;">
23.500
</td>
<td style="text-align:right;">
7.50
</td>
<td style="text-align:right;">
3759.00
</td>
</tr>
<tr>
<td style="text-align:left;">
SA
</td>
<td style="text-align:right;">
0.850
</td>
<td style="text-align:right;">
50.125
</td>
<td style="text-align:right;">
0.808375
</td>
<td style="text-align:right;">
4.297375
</td>
<td style="text-align:right;">
8.000
</td>
<td style="text-align:right;">
24.875
</td>
<td style="text-align:right;">
24.75
</td>
<td style="text-align:right;">
3452.25
</td>
</tr>
<tr>
<td style="text-align:left;">
WNC
</td>
<td style="text-align:right;">
0.850
</td>
<td style="text-align:right;">
31.250
</td>
<td style="text-align:right;">
0.711250
</td>
<td style="text-align:right;">
4.228500
</td>
<td style="text-align:right;">
4.750
</td>
<td style="text-align:right;">
17.500
</td>
<td style="text-align:right;">
17.50
</td>
<td style="text-align:right;">
2439.00
</td>
</tr>
<tr>
<td style="text-align:left;">
WSC
</td>
<td style="text-align:right;">
1.225
</td>
<td style="text-align:right;">
43.125
</td>
<td style="text-align:right;">
0.278125
</td>
<td style="text-align:right;">
1.019500
</td>
<td style="text-align:right;">
9.625
</td>
<td style="text-align:right;">
20.875
</td>
<td style="text-align:right;">
22.25
</td>
<td style="text-align:right;">
4464.50
</td>
</tr>
</tbody>
</table>

``` r
#created a table with a summary of the interquartile range for each variable, grouped by region
combinedstates %>%
  summarise(mean_ratio = mean(ratio),mean_averagesat = mean(averagesat),mean_averagepupilexpense = mean(averagepupilexpense),mean_averagesalary = mean(averagesalary),mean_averagepercent = mean(averagepercent), mean_averagesatverbal = mean(averagesatverbal),mean_averagesatmath = mean(averagesatmath), mean_averagesat = mean(averagesat), mean_pop = mean(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
mean\_ratio
</th>
<th style="text-align:right;">
mean\_averagesat
</th>
<th style="text-align:right;">
mean\_averagepupilexpense
</th>
<th style="text-align:right;">
mean\_averagesalary
</th>
<th style="text-align:right;">
mean\_averagepercent
</th>
<th style="text-align:right;">
mean\_averagesatverbal
</th>
<th style="text-align:right;">
mean\_averagesatmath
</th>
<th style="text-align:right;">
mean\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
16.858
</td>
<td style="text-align:right;">
956.69
</td>
<td style="text-align:right;">
5.51003
</td>
<td style="text-align:right;">
32.80446
</td>
<td style="text-align:right;">
34.15
</td>
<td style="text-align:right;">
453.04
</td>
<td style="text-align:right;">
503.65
</td>
<td style="text-align:right;">
4962.04
</td>
</tr>
</tbody>
</table>

``` r
combinedstates %>%
  summarise(sd_ratio = sd(ratio),sd_averagesat = sd(averagesat),sd_averagepupilexpense = sd(averagepupilexpense),sd_averagesalary = sd(averagesalary),sd_averagepercent = sd(averagepercent), sd_averagesatverbal = sd(averagesatverbal),sd_averagesatmath = sd(averagesatmath), sd_averagesat = sd(averagesat), sd_pop = sd(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
sd\_ratio
</th>
<th style="text-align:right;">
sd\_averagesat
</th>
<th style="text-align:right;">
sd\_averagepupilexpense
</th>
<th style="text-align:right;">
sd\_averagesalary
</th>
<th style="text-align:right;">
sd\_averagepercent
</th>
<th style="text-align:right;">
sd\_averagesatverbal
</th>
<th style="text-align:right;">
sd\_averagesatmath
</th>
<th style="text-align:right;">
sd\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
2.266355
</td>
<td style="text-align:right;">
69.00672
</td>
<td style="text-align:right;">
1.330486
</td>
<td style="text-align:right;">
5.539907
</td>
<td style="text-align:right;">
25.26699
</td>
<td style="text-align:right;">
32.71738
</td>
<td style="text-align:right;">
36.84181
</td>
<td style="text-align:right;">
5459.782
</td>
</tr>
</tbody>
</table>

``` r
combinedstates %>% 
  summarise(quantile_ratio = quantile(ratio, probs = c(0.5)),quantile_averagesat = quantile(averagesat, probs = c(0.5)),quantile_averagepupilexpense = quantile(averagepupilexpense, probs = c(0.5)),quantile_averagesalary = quantile(averagesalary, probs = c(0.5)),quantile_averagepercent = quantile(averagepercent, probs = c(0.5)), quantile_averagesatverbal = quantile(averagesatverbal, probs = c(0.5)),quantile_averagesatmath = quantile(averagesatmath, probs = c(0.5)), quantile_averagesat = quantile(averagesat, probs = c(0.5)), quantile_pop = quantile(pop, probs = c(0.5)))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
quantile\_ratio
</th>
<th style="text-align:right;">
quantile\_averagesat
</th>
<th style="text-align:right;">
quantile\_averagepupilexpense
</th>
<th style="text-align:right;">
quantile\_averagesalary
</th>
<th style="text-align:right;">
quantile\_averagepercent
</th>
<th style="text-align:right;">
quantile\_averagesatverbal
</th>
<th style="text-align:right;">
quantile\_averagesatmath
</th>
<th style="text-align:right;">
quantile\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
16.6
</td>
<td style="text-align:right;">
939
</td>
<td style="text-align:right;">
5.4245
</td>
<td style="text-align:right;">
31.56
</td>
<td style="text-align:right;">
26.5
</td>
<td style="text-align:right;">
446
</td>
<td style="text-align:right;">
494
</td>
<td style="text-align:right;">
3390.5
</td>
</tr>
</tbody>
</table>

``` r
combinedstates %>% 
  summarise(min_ratio = min(ratio),min_averagesat = min(averagesat),min_averagepupilexpense = min(averagepupilexpense),min_averagesalary = min(averagesalary),min_averagepercent = min(averagepercent), min_averagesatverbal = min(averagesatverbal),min_averagesatmath = min(averagesatmath), min_averagesat = min(averagesat), min_pop = min(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
min\_ratio
</th>
<th style="text-align:right;">
min\_averagesat
</th>
<th style="text-align:right;">
min\_averagepupilexpense
</th>
<th style="text-align:right;">
min\_averagesalary
</th>
<th style="text-align:right;">
min\_averagepercent
</th>
<th style="text-align:right;">
min\_averagesatverbal
</th>
<th style="text-align:right;">
min\_averagesatmath
</th>
<th style="text-align:right;">
min\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
13.8
</td>
<td style="text-align:right;">
839
</td>
<td style="text-align:right;">
3.3245
</td>
<td style="text-align:right;">
23.997
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
399
</td>
<td style="text-align:right;">
440
</td>
<td style="text-align:right;">
454
</td>
</tr>
</tbody>
</table>

``` r
combinedstates %>% 
  summarise(max_ratio = max(ratio),max_averagesat = max(averagesat),max_averagepupilexpense = max(averagepupilexpense),max_averagesalary = max(averagesalary),max_averagepercent = max(averagepercent), max_averagesatverbal = max(averagesatverbal),max_averagesatmath = max(averagesatmath), max_averagesat = max(averagesat), max_pop = max(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
max\_ratio
</th>
<th style="text-align:right;">
max\_averagesat
</th>
<th style="text-align:right;">
max\_averagepupilexpense
</th>
<th style="text-align:right;">
max\_averagesalary
</th>
<th style="text-align:right;">
max\_averagepercent
</th>
<th style="text-align:right;">
max\_averagesatverbal
</th>
<th style="text-align:right;">
max\_averagesatmath
</th>
<th style="text-align:right;">
max\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
24.3
</td>
<td style="text-align:right;">
1093.5
</td>
<td style="text-align:right;">
9.4665
</td>
<td style="text-align:right;">
46.5225
</td>
<td style="text-align:right;">
77.5
</td>
<td style="text-align:right;">
513.5
</td>
<td style="text-align:right;">
580
</td>
<td style="text-align:right;">
29760
</td>
</tr>
</tbody>
</table>

``` r
combinedstates %>% 
  summarise(iqr_ratio = IQR(ratio),iqr_averagesat = IQR(averagesat),iqr_averagepupilexpense = IQR(averagepupilexpense),iqr_averagesalary = IQR(averagesalary),iqr_averagepercent = IQR(averagepercent), iqr_averagesatverbal = IQR(averagesatverbal),iqr_averagesatmath = IQR(averagesatmath), iqr_averagesat = IQR(averagesat), iqr_pop = IQR(pop))%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
iqr\_ratio
</th>
<th style="text-align:right;">
iqr\_averagesat
</th>
<th style="text-align:right;">
iqr\_averagepupilexpense
</th>
<th style="text-align:right;">
iqr\_averagesalary
</th>
<th style="text-align:right;">
iqr\_averagepercent
</th>
<th style="text-align:right;">
iqr\_averagesatverbal
</th>
<th style="text-align:right;">
iqr\_averagesatmath
</th>
<th style="text-align:right;">
iqr\_pop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
116.875
</td>
<td style="text-align:right;">
1.46525
</td>
<td style="text-align:right;">
6.26025
</td>
<td style="text-align:right;">
49.625
</td>
<td style="text-align:right;">
56.75
</td>
<td style="text-align:right;">
57.375
</td>
<td style="text-align:right;">
4598.25
</td>
</tr>
</tbody>
</table>

``` r
#performed same statistical measures on all variables but did not group by region, so now the statistics are calculated for all states. 
combinedstates%>%
  group_by(state)%>%
    filter(pop>10000)%>%
  arrange(desc(averagesat))
```

    ## # A tibble: 7 x 22
    ## # Groups:   state [7]
    ##   state        pupilexpense1995 ratio salary1995 percent1995 verbal1995 math1995
    ##   <fct>                   <dbl> <dbl>      <dbl>       <int>      <int>    <int>
    ## 1 Illinois                 6.14  17.3       39.4          13        488      560
    ## 2 Ohio                     6.16  16.6       36.8          23        460      515
    ## 3 California               4.99  24         41.1          45        417      485
    ## 4 New York                 9.62  15.2       47.6          74        419      473
    ## 5 Florida                  5.72  19.1       32.6          48        420      469
    ## 6 Texas                    5.22  15.7       31.2          47        419      474
    ## 7 Pennsylvania             7.11  17.1       44.5          70        419      461
    ## # ... with 15 more variables: sat1995 <int>, region <fct>, pop <int>,
    ## #   verbal1992 <int>, math1992 <int>, percent1992 <int>,
    ## #   pupilexpense1992 <dbl>, salary1992 <int>, sat1992 <int>, averagesat <dbl>,
    ## #   averagepupilexpense <dbl>, averagesalary <dbl>, averagepercent <dbl>,
    ## #   averagesatverbal <dbl>, averagesatmath <dbl>

``` r
combinedstates %>% 
  filter(averagesat>1000)%>%
  group_by(state)%>%
  arrange(desc(averagepupilexpense))
```

    ## # A tibble: 17 x 22
    ## # Groups:   state [17]
    ##    state       pupilexpense1995 ratio salary1995 percent1995 verbal1995 math1995
    ##    <fct>                  <dbl> <dbl>      <dbl>       <int>      <int>    <int>
    ##  1 Wisconsin               6.93  15.9       37.7           9        501      572
    ##  2 Michigan                6.99  20.1       41.9          11        484      549
    ##  3 Minnesota               6     17.5       35.9           9        506      579
    ##  4 Illinois                6.14  17.3       39.4          13        488      560
    ##  5 Kansas                  5.82  15.1       34.7           9        503      557
    ##  6 Iowa                    5.48  15.8       31.5           5        516      583
    ##  7 Nebraska                5.94  14.5       30.9           9        494      556
    ##  8 Missouri                5.38  15.5       31.2           9        495      550
    ##  9 New Mexico              4.59  17.2       28.5          11        485      530
    ## 10 Louisiana               4.76  16.8       26.5           9        486      535
    ## 11 Oklahoma                4.84  15.5       28.2           9        491      536
    ## 12 South Dako~             4.78  14.4       26.0           5        505      563
    ## 13 North Dako~             4.78  15.3       26.3           5        515      592
    ## 14 Tennessee               4.39  18.6       32.5          12        497      543
    ## 15 Alabama                 4.40  17.2       31.1           8        491      538
    ## 16 Mississippi             4.08  17.5       26.8           4        496      540
    ## 17 Utah                    3.66  24.3       29.1           4        513      563
    ## # ... with 15 more variables: sat1995 <int>, region <fct>, pop <int>,
    ## #   verbal1992 <int>, math1992 <int>, percent1992 <int>,
    ## #   pupilexpense1992 <dbl>, salary1992 <int>, sat1992 <int>, averagesat <dbl>,
    ## #   averagepupilexpense <dbl>, averagesalary <dbl>, averagepercent <dbl>,
    ## #   averagesatverbal <dbl>, averagesatmath <dbl>

``` r
combinedstates %>% 
  group_by(state)%>%
  arrange(desc(averagesalary))%>%
  select(ratio)
```

    ## # A tibble: 50 x 2
    ## # Groups:   state [50]
    ##    state         ratio
    ##    <fct>         <dbl>
    ##  1 Connecticut    14.4
    ##  2 Alaska         17.6
    ##  3 New York       15.2
    ##  4 New Jersey     13.8
    ##  5 Pennsylvania   17.1
    ##  6 California     24  
    ##  7 Michigan       20.1
    ##  8 Maryland       17  
    ##  9 Rhode Island   14.7
    ## 10 Massachusetts  14.8
    ## # ... with 40 more rows

``` r
combinedstates %>% 
  group_by(state)%>%
  arrange(desc(ratio))%>%
  select(pop)
```

    ## # A tibble: 50 x 2
    ## # Groups:   state [50]
    ##    state        pop
    ##    <fct>      <int>
    ##  1 Utah        1723
    ##  2 California 29760
    ##  3 Washington  4867
    ##  4 Michigan    9295
    ##  5 Oregon      2842
    ##  6 Arizona     3665
    ##  7 Florida    12938
    ##  8 Idaho       1007
    ##  9 Nevada      1202
    ## 10 Tennessee   4877
    ## # ... with 40 more rows

``` r
#arranged the data by various variables to see what states fared best (grouped by states), also selecting and filtering various variables to explore dataset
combinedstates_num <- combinedstates %>%
  select_if(is.numeric) 
cor(combinedstates_num, use = "pairwise.complete.obs")%>%
  kbl%>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
pupilexpense1995
</th>
<th style="text-align:right;">
ratio
</th>
<th style="text-align:right;">
salary1995
</th>
<th style="text-align:right;">
percent1995
</th>
<th style="text-align:right;">
verbal1995
</th>
<th style="text-align:right;">
math1995
</th>
<th style="text-align:right;">
sat1995
</th>
<th style="text-align:right;">
pop
</th>
<th style="text-align:right;">
verbal1992
</th>
<th style="text-align:right;">
math1992
</th>
<th style="text-align:right;">
percent1992
</th>
<th style="text-align:right;">
pupilexpense1992
</th>
<th style="text-align:right;">
salary1992
</th>
<th style="text-align:right;">
sat1992
</th>
<th style="text-align:right;">
averagesat
</th>
<th style="text-align:right;">
averagepupilexpense
</th>
<th style="text-align:right;">
averagesalary
</th>
<th style="text-align:right;">
averagepercent
</th>
<th style="text-align:right;">
averagesatverbal
</th>
<th style="text-align:right;">
averagesatmath
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
pupilexpense1995
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.3710254
</td>
<td style="text-align:right;">
0.8698015
</td>
<td style="text-align:right;">
0.5926274
</td>
<td style="text-align:right;">
-0.4100499
</td>
<td style="text-align:right;">
-0.3494141
</td>
<td style="text-align:right;">
-0.3805370
</td>
<td style="text-align:right;">
0.1434879
</td>
<td style="text-align:right;">
-0.4209078
</td>
<td style="text-align:right;">
-0.3581530
</td>
<td style="text-align:right;">
0.6103693
</td>
<td style="text-align:right;">
0.9684576
</td>
<td style="text-align:right;">
0.8242468
</td>
<td style="text-align:right;">
-0.3916596
</td>
<td style="text-align:right;">
-0.3877982
</td>
<td style="text-align:right;">
0.9923383
</td>
<td style="text-align:right;">
0.8558095
</td>
<td style="text-align:right;">
0.6014475
</td>
<td style="text-align:right;">
-0.4173753
</td>
<td style="text-align:right;">
-0.3557169
</td>
</tr>
<tr>
<td style="text-align:left;">
ratio
</td>
<td style="text-align:right;">
-0.3710254
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.0011461
</td>
<td style="text-align:right;">
-0.2130536
</td>
<td style="text-align:right;">
0.0637666
</td>
<td style="text-align:right;">
0.0954217
</td>
<td style="text-align:right;">
0.0812538
</td>
<td style="text-align:right;">
0.3192271
</td>
<td style="text-align:right;">
0.0290800
</td>
<td style="text-align:right;">
0.0633206
</td>
<td style="text-align:right;">
-0.2059106
</td>
<td style="text-align:right;">
-0.3618375
</td>
<td style="text-align:right;">
0.0542548
</td>
<td style="text-align:right;">
0.0475418
</td>
<td style="text-align:right;">
0.0660811
</td>
<td style="text-align:right;">
-0.3694298
</td>
<td style="text-align:right;">
0.0250172
</td>
<td style="text-align:right;">
-0.2098535
</td>
<td style="text-align:right;">
0.0478858
</td>
<td style="text-align:right;">
0.0812486
</td>
</tr>
<tr>
<td style="text-align:left;">
salary1995
</td>
<td style="text-align:right;">
0.8698015
</td>
<td style="text-align:right;">
-0.0011461
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.6167799
</td>
<td style="text-align:right;">
-0.4769636
</td>
<td style="text-align:right;">
-0.4013128
</td>
<td style="text-align:right;">
-0.4398834
</td>
<td style="text-align:right;">
0.3531944
</td>
<td style="text-align:right;">
-0.5003208
</td>
<td style="text-align:right;">
-0.4232865
</td>
<td style="text-align:right;">
0.6365475
</td>
<td style="text-align:right;">
0.8628594
</td>
<td style="text-align:right;">
0.9656808
</td>
<td style="text-align:right;">
-0.4642589
</td>
<td style="text-align:right;">
-0.4536147
</td>
<td style="text-align:right;">
0.8732987
</td>
<td style="text-align:right;">
0.9924431
</td>
<td style="text-align:right;">
0.6265732
</td>
<td style="text-align:right;">
-0.4905038
</td>
<td style="text-align:right;">
-0.4140531
</td>
</tr>
<tr>
<td style="text-align:left;">
percent1995
</td>
<td style="text-align:right;">
0.5926274
</td>
<td style="text-align:right;">
-0.2130536
</td>
<td style="text-align:right;">
0.6167799
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.8932630
</td>
<td style="text-align:right;">
-0.8693839
</td>
<td style="text-align:right;">
-0.8871187
</td>
<td style="text-align:right;">
0.2137348
</td>
<td style="text-align:right;">
-0.8611290
</td>
<td style="text-align:right;">
-0.8631369
</td>
<td style="text-align:right;">
0.9968855
</td>
<td style="text-align:right;">
0.6788224
</td>
<td style="text-align:right;">
0.6297318
</td>
<td style="text-align:right;">
-0.8705263
</td>
<td style="text-align:right;">
-0.8843412
</td>
<td style="text-align:right;">
0.6400936
</td>
<td style="text-align:right;">
0.6282380
</td>
<td style="text-align:right;">
0.9993094
</td>
<td style="text-align:right;">
-0.8831180
</td>
<td style="text-align:right;">
-0.8721661
</td>
</tr>
<tr>
<td style="text-align:left;">
verbal1995
</td>
<td style="text-align:right;">
-0.4100499
</td>
<td style="text-align:right;">
0.0637666
</td>
<td style="text-align:right;">
-0.4769636
</td>
<td style="text-align:right;">
-0.8932630
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.9702560
</td>
<td style="text-align:right;">
0.9915033
</td>
<td style="text-align:right;">
-0.3282947
</td>
<td style="text-align:right;">
0.9781740
</td>
<td style="text-align:right;">
0.9550743
</td>
<td style="text-align:right;">
-0.8844948
</td>
<td style="text-align:right;">
-0.5022311
</td>
<td style="text-align:right;">
-0.5096810
</td>
<td style="text-align:right;">
0.9753724
</td>
<td style="text-align:right;">
0.9895176
</td>
<td style="text-align:right;">
-0.4590276
</td>
<td style="text-align:right;">
-0.4965493
</td>
<td style="text-align:right;">
-0.8898255
</td>
<td style="text-align:right;">
0.9952625
</td>
<td style="text-align:right;">
0.9695773
</td>
</tr>
<tr>
<td style="text-align:left;">
math1995
</td>
<td style="text-align:right;">
-0.3494141
</td>
<td style="text-align:right;">
0.0954217
</td>
<td style="text-align:right;">
-0.4013128
</td>
<td style="text-align:right;">
-0.8693839
</td>
<td style="text-align:right;">
0.9702560
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.9935024
</td>
<td style="text-align:right;">
-0.2249310
</td>
<td style="text-align:right;">
0.9386033
</td>
<td style="text-align:right;">
0.9740209
</td>
<td style="text-align:right;">
-0.8524616
</td>
<td style="text-align:right;">
-0.4478127
</td>
<td style="text-align:right;">
-0.4310896
</td>
<td style="text-align:right;">
0.9664893
</td>
<td style="text-align:right;">
0.9864848
</td>
<td style="text-align:right;">
-0.4009909
</td>
<td style="text-align:right;">
-0.4188543
</td>
<td style="text-align:right;">
-0.8620858
</td>
<td style="text-align:right;">
0.9607577
</td>
<td style="text-align:right;">
0.9945387
</td>
</tr>
<tr>
<td style="text-align:left;">
sat1995
</td>
<td style="text-align:right;">
-0.3805370
</td>
<td style="text-align:right;">
0.0812538
</td>
<td style="text-align:right;">
-0.4398834
</td>
<td style="text-align:right;">
-0.8871187
</td>
<td style="text-align:right;">
0.9915033
</td>
<td style="text-align:right;">
0.9935024
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.2752100
</td>
<td style="text-align:right;">
0.9642335
</td>
<td style="text-align:right;">
0.9724050
</td>
<td style="text-align:right;">
-0.8739032
</td>
<td style="text-align:right;">
-0.4767492
</td>
<td style="text-align:right;">
-0.4712656
</td>
<td style="text-align:right;">
0.9779009
</td>
<td style="text-align:right;">
0.9952956
</td>
<td style="text-align:right;">
-0.4312780
</td>
<td style="text-align:right;">
-0.4585172
</td>
<td style="text-align:right;">
-0.8815810
</td>
<td style="text-align:right;">
0.9841721
</td>
<td style="text-align:right;">
0.9902487
</td>
</tr>
<tr>
<td style="text-align:left;">
pop
</td>
<td style="text-align:right;">
0.1434879
</td>
<td style="text-align:right;">
0.3192271
</td>
<td style="text-align:right;">
0.3531944
</td>
<td style="text-align:right;">
0.2137348
</td>
<td style="text-align:right;">
-0.3282947
</td>
<td style="text-align:right;">
-0.2249310
</td>
<td style="text-align:right;">
-0.2752100
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.3668112
</td>
<td style="text-align:right;">
-0.2650858
</td>
<td style="text-align:right;">
0.2393264
</td>
<td style="text-align:right;">
0.1897784
</td>
<td style="text-align:right;">
0.4041393
</td>
<td style="text-align:right;">
-0.3163477
</td>
<td style="text-align:right;">
-0.2957974
</td>
<td style="text-align:right;">
0.1675850
</td>
<td style="text-align:right;">
0.3803199
</td>
<td style="text-align:right;">
0.2259592
</td>
<td style="text-align:right;">
-0.3481141
</td>
<td style="text-align:right;">
-0.2449019
</td>
</tr>
<tr>
<td style="text-align:left;">
verbal1992
</td>
<td style="text-align:right;">
-0.4209078
</td>
<td style="text-align:right;">
0.0290800
</td>
<td style="text-align:right;">
-0.5003208
</td>
<td style="text-align:right;">
-0.8611290
</td>
<td style="text-align:right;">
0.9781740
</td>
<td style="text-align:right;">
0.9386033
</td>
<td style="text-align:right;">
0.9642335
</td>
<td style="text-align:right;">
-0.3668112
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.9617535
</td>
<td style="text-align:right;">
-0.8577610
</td>
<td style="text-align:right;">
-0.5032199
</td>
<td style="text-align:right;">
-0.5380999
</td>
<td style="text-align:right;">
0.9893672
</td>
<td style="text-align:right;">
0.9812193
</td>
<td style="text-align:right;">
-0.4650787
</td>
<td style="text-align:right;">
-0.5224999
</td>
<td style="text-align:right;">
-0.8602111
</td>
<td style="text-align:right;">
0.9937419
</td>
<td style="text-align:right;">
0.9553845
</td>
</tr>
<tr>
<td style="text-align:left;">
math1992
</td>
<td style="text-align:right;">
-0.3581530
</td>
<td style="text-align:right;">
0.0633206
</td>
<td style="text-align:right;">
-0.4232865
</td>
<td style="text-align:right;">
-0.8631369
</td>
<td style="text-align:right;">
0.9550743
</td>
<td style="text-align:right;">
0.9740209
</td>
<td style="text-align:right;">
0.9724050
</td>
<td style="text-align:right;">
-0.2650858
</td>
<td style="text-align:right;">
0.9617535
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.8515144
</td>
<td style="text-align:right;">
-0.4453652
</td>
<td style="text-align:right;">
-0.4580104
</td>
<td style="text-align:right;">
0.9913655
</td>
<td style="text-align:right;">
0.9865753
</td>
<td style="text-align:right;">
-0.4042529
</td>
<td style="text-align:right;">
-0.4433554
</td>
<td style="text-align:right;">
-0.8583311
</td>
<td style="text-align:right;">
0.9634285
</td>
<td style="text-align:right;">
0.9923365
</td>
</tr>
<tr>
<td style="text-align:left;">
percent1992
</td>
<td style="text-align:right;">
0.6103693
</td>
<td style="text-align:right;">
-0.2059106
</td>
<td style="text-align:right;">
0.6365475
</td>
<td style="text-align:right;">
0.9968855
</td>
<td style="text-align:right;">
-0.8844948
</td>
<td style="text-align:right;">
-0.8524616
</td>
<td style="text-align:right;">
-0.8739032
</td>
<td style="text-align:right;">
0.2393264
</td>
<td style="text-align:right;">
-0.8577610
</td>
<td style="text-align:right;">
-0.8515144
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.6963723
</td>
<td style="text-align:right;">
0.6475399
</td>
<td style="text-align:right;">
-0.8627428
</td>
<td style="text-align:right;">
-0.8735698
</td>
<td style="text-align:right;">
0.6578818
</td>
<td style="text-align:right;">
0.6472510
</td>
<td style="text-align:right;">
0.9991275
</td>
<td style="text-align:right;">
-0.8768285
</td>
<td style="text-align:right;">
-0.8575761
</td>
</tr>
<tr>
<td style="text-align:left;">
pupilexpense1992
</td>
<td style="text-align:right;">
0.9684576
</td>
<td style="text-align:right;">
-0.3618375
</td>
<td style="text-align:right;">
0.8628594
</td>
<td style="text-align:right;">
0.6788224
</td>
<td style="text-align:right;">
-0.5022311
</td>
<td style="text-align:right;">
-0.4478127
</td>
<td style="text-align:right;">
-0.4767492
</td>
<td style="text-align:right;">
0.1897784
</td>
<td style="text-align:right;">
-0.5032199
</td>
<td style="text-align:right;">
-0.4453652
</td>
<td style="text-align:right;">
0.6963723
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.8411843
</td>
<td style="text-align:right;">
-0.4773697
</td>
<td style="text-align:right;">
-0.4796763
</td>
<td style="text-align:right;">
0.9918237
</td>
<td style="text-align:right;">
0.8600888
</td>
<td style="text-align:right;">
0.6876190
</td>
<td style="text-align:right;">
-0.5054434
</td>
<td style="text-align:right;">
-0.4496008
</td>
</tr>
<tr>
<td style="text-align:left;">
salary1992
</td>
<td style="text-align:right;">
0.8242468
</td>
<td style="text-align:right;">
0.0542548
</td>
<td style="text-align:right;">
0.9656808
</td>
<td style="text-align:right;">
0.6297318
</td>
<td style="text-align:right;">
-0.5096810
</td>
<td style="text-align:right;">
-0.4310896
</td>
<td style="text-align:right;">
-0.4712656
</td>
<td style="text-align:right;">
0.4041393
</td>
<td style="text-align:right;">
-0.5380999
</td>
<td style="text-align:right;">
-0.4580104
</td>
<td style="text-align:right;">
0.6475399
</td>
<td style="text-align:right;">
0.8411843
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.5007814
</td>
<td style="text-align:right;">
-0.4875527
</td>
<td style="text-align:right;">
0.8392208
</td>
<td style="text-align:right;">
0.9902538
</td>
<td style="text-align:right;">
0.6386119
</td>
<td style="text-align:right;">
-0.5257687
</td>
<td style="text-align:right;">
-0.4463038
</td>
</tr>
<tr>
<td style="text-align:left;">
sat1992
</td>
<td style="text-align:right;">
-0.3916596
</td>
<td style="text-align:right;">
0.0475418
</td>
<td style="text-align:right;">
-0.4642589
</td>
<td style="text-align:right;">
-0.8705263
</td>
<td style="text-align:right;">
0.9753724
</td>
<td style="text-align:right;">
0.9664893
</td>
<td style="text-align:right;">
0.9779009
</td>
<td style="text-align:right;">
-0.3163477
</td>
<td style="text-align:right;">
0.9893672
</td>
<td style="text-align:right;">
0.9913655
</td>
<td style="text-align:right;">
-0.8627428
</td>
<td style="text-align:right;">
-0.4773697
</td>
<td style="text-align:right;">
-0.5007814
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.9935561
</td>
<td style="text-align:right;">
-0.4372820
</td>
<td style="text-align:right;">
-0.4855322
</td>
<td style="text-align:right;">
-0.8675351
</td>
<td style="text-align:right;">
0.9872607
</td>
<td style="text-align:right;">
0.9842476
</td>
</tr>
<tr>
<td style="text-align:left;">
averagesat
</td>
<td style="text-align:right;">
-0.3877982
</td>
<td style="text-align:right;">
0.0660811
</td>
<td style="text-align:right;">
-0.4536147
</td>
<td style="text-align:right;">
-0.8843412
</td>
<td style="text-align:right;">
0.9895176
</td>
<td style="text-align:right;">
0.9864848
</td>
<td style="text-align:right;">
0.9952956
</td>
<td style="text-align:right;">
-0.2957974
</td>
<td style="text-align:right;">
0.9812193
</td>
<td style="text-align:right;">
0.9865753
</td>
<td style="text-align:right;">
-0.8735698
</td>
<td style="text-align:right;">
-0.4796763
</td>
<td style="text-align:right;">
-0.4875527
</td>
<td style="text-align:right;">
0.9935561
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.4364481
</td>
<td style="text-align:right;">
-0.4735749
</td>
<td style="text-align:right;">
-0.8799529
</td>
<td style="text-align:right;">
0.9910523
</td>
<td style="text-align:right;">
0.9929503
</td>
</tr>
<tr>
<td style="text-align:left;">
averagepupilexpense
</td>
<td style="text-align:right;">
0.9923383
</td>
<td style="text-align:right;">
-0.3694298
</td>
<td style="text-align:right;">
0.8732987
</td>
<td style="text-align:right;">
0.6400936
</td>
<td style="text-align:right;">
-0.4590276
</td>
<td style="text-align:right;">
-0.4009909
</td>
<td style="text-align:right;">
-0.4312780
</td>
<td style="text-align:right;">
0.1675850
</td>
<td style="text-align:right;">
-0.4650787
</td>
<td style="text-align:right;">
-0.4042529
</td>
<td style="text-align:right;">
0.6578818
</td>
<td style="text-align:right;">
0.9918237
</td>
<td style="text-align:right;">
0.8392208
</td>
<td style="text-align:right;">
-0.4372820
</td>
<td style="text-align:right;">
-0.4364481
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.8647590
</td>
<td style="text-align:right;">
0.6489724
</td>
<td style="text-align:right;">
-0.4643721
</td>
<td style="text-align:right;">
-0.4051054
</td>
</tr>
<tr>
<td style="text-align:left;">
averagesalary
</td>
<td style="text-align:right;">
0.8558095
</td>
<td style="text-align:right;">
0.0250172
</td>
<td style="text-align:right;">
0.9924431
</td>
<td style="text-align:right;">
0.6282380
</td>
<td style="text-align:right;">
-0.4965493
</td>
<td style="text-align:right;">
-0.4188543
</td>
<td style="text-align:right;">
-0.4585172
</td>
<td style="text-align:right;">
0.3803199
</td>
<td style="text-align:right;">
-0.5224999
</td>
<td style="text-align:right;">
-0.4433554
</td>
<td style="text-align:right;">
0.6472510
</td>
<td style="text-align:right;">
0.8600888
</td>
<td style="text-align:right;">
0.9902538
</td>
<td style="text-align:right;">
-0.4855322
</td>
<td style="text-align:right;">
-0.4735749
</td>
<td style="text-align:right;">
0.8647590
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.6376847
</td>
<td style="text-align:right;">
-0.5114102
</td>
<td style="text-align:right;">
-0.4328737
</td>
</tr>
<tr>
<td style="text-align:left;">
averagepercent
</td>
<td style="text-align:right;">
0.6014475
</td>
<td style="text-align:right;">
-0.2098535
</td>
<td style="text-align:right;">
0.6265732
</td>
<td style="text-align:right;">
0.9993094
</td>
<td style="text-align:right;">
-0.8898255
</td>
<td style="text-align:right;">
-0.8620858
</td>
<td style="text-align:right;">
-0.8815810
</td>
<td style="text-align:right;">
0.2259592
</td>
<td style="text-align:right;">
-0.8602111
</td>
<td style="text-align:right;">
-0.8583311
</td>
<td style="text-align:right;">
0.9991275
</td>
<td style="text-align:right;">
0.6876190
</td>
<td style="text-align:right;">
0.6386119
</td>
<td style="text-align:right;">
-0.8675351
</td>
<td style="text-align:right;">
-0.8799529
</td>
<td style="text-align:right;">
0.6489724
</td>
<td style="text-align:right;">
0.6376847
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.8808406
</td>
<td style="text-align:right;">
-0.8659691
</td>
</tr>
<tr>
<td style="text-align:left;">
averagesatverbal
</td>
<td style="text-align:right;">
-0.4173753
</td>
<td style="text-align:right;">
0.0478858
</td>
<td style="text-align:right;">
-0.4905038
</td>
<td style="text-align:right;">
-0.8831180
</td>
<td style="text-align:right;">
0.9952625
</td>
<td style="text-align:right;">
0.9607577
</td>
<td style="text-align:right;">
0.9841721
</td>
<td style="text-align:right;">
-0.3481141
</td>
<td style="text-align:right;">
0.9937419
</td>
<td style="text-align:right;">
0.9634285
</td>
<td style="text-align:right;">
-0.8768285
</td>
<td style="text-align:right;">
-0.5054434
</td>
<td style="text-align:right;">
-0.5257687
</td>
<td style="text-align:right;">
0.9872607
</td>
<td style="text-align:right;">
0.9910523
</td>
<td style="text-align:right;">
-0.4643721
</td>
<td style="text-align:right;">
-0.5114102
</td>
<td style="text-align:right;">
-0.8808406
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.9682448
</td>
</tr>
<tr>
<td style="text-align:left;">
averagesatmath
</td>
<td style="text-align:right;">
-0.3557169
</td>
<td style="text-align:right;">
0.0812486
</td>
<td style="text-align:right;">
-0.4140531
</td>
<td style="text-align:right;">
-0.8721661
</td>
<td style="text-align:right;">
0.9695773
</td>
<td style="text-align:right;">
0.9945387
</td>
<td style="text-align:right;">
0.9902487
</td>
<td style="text-align:right;">
-0.2449019
</td>
<td style="text-align:right;">
0.9553845
</td>
<td style="text-align:right;">
0.9923365
</td>
<td style="text-align:right;">
-0.8575761
</td>
<td style="text-align:right;">
-0.4496008
</td>
<td style="text-align:right;">
-0.4463038
</td>
<td style="text-align:right;">
0.9842476
</td>
<td style="text-align:right;">
0.9929503
</td>
<td style="text-align:right;">
-0.4051054
</td>
<td style="text-align:right;">
-0.4328737
</td>
<td style="text-align:right;">
-0.8659691
</td>
<td style="text-align:right;">
0.9682448
</td>
<td style="text-align:right;">
1.0000000
</td>
</tr>
</tbody>
</table>

``` r
#created a correlation matrix of all numeric variables
```

##### The statistics found in this section are quite interesting. There are so many, however, that it would be hard to discuss them all without writing a whole page that would seem quite redundant. One of the main things I noticed across the summaries was that there is quite a bit of disparity across the states. Among individual states, the average SAT score had a range of more than 200 points, the average salary of teachers had a range of more than $20,000, and the average percent of graduating students varied by more than 70 percentange points, to name a few. Another trend that could be seen across the data was that Northern states and regions had better educational statistics, such as lower pupil to teacher ratios, higher expenditures per pupil, and typically higher SAT scores.

### VISUALIZATION

``` r
library(ggplot2)
combinedstates_num2 <- cor(combinedstates_num, use = "pairwise.complete.obs") %>%
  as.data.frame %>%
  rownames_to_column %>%
  pivot_longer(-1, names_to = "other_var", values_to = "correlation")
ggplot(combinedstates_num2, aes(rowname, other_var, fill= correlation)) + 
  geom_tile()+
  scale_fill_gradient(low = "antiquewhite2", high = "darkslateblue")+
  labs(title = "Correlation heatmap for US education statistics")+
  theme(axis.text.x = element_text(angle=45, hjust=1))
```

<img src="Project-1-Comp-Bio_files/figure-gfm/visualization-1.png" style="display: block; margin: auto;" />

``` r
#heatmap created with angled x axis, looks at all variables and their correlation
combinedstates<- combinedstates%>%
  mutate(sd_averagesat = sd(averagesat))
ggplot(data = combinedstates, aes(x = averagesalary, y = averagesat, color = region,stat="summary", fun="mean")) + 
  geom_point(size = 2) + 
  geom_line()+
  labs(title = "Scatterplot of Annual Teacher Salary versus Average SAT score by Region", y = "Average SAT score", x = "Annual Teacher Salary (thousands of US$)")+
  geom_errorbar(aes(ymin=averagesat-sd_averagesat, ymax=averagesat-sd_averagesat), width=.2,)
```

<img src="Project-1-Comp-Bio_files/figure-gfm/visualization-2.png" style="display: block; margin: auto;" />

``` r
#scatterplot of salary versus sat score grouped by region, error bars were attempted but serve only to confuse.
ggplot(data = combinedstates, aes(x = averagepupilexpense, y = averagesat, color = region)) + 
  geom_point(size = 2) + 
  geom_line()+
  labs(title = "Scatterplot of Annual Expenditure per Pupil versus Average SAT score by Region", y = "Average SAT score", x = "Annual Pupil Expenditure (thousands of US$)")
```

<img src="Project-1-Comp-Bio_files/figure-gfm/visualization-3.png" style="display: block; margin: auto;" />

``` r
#scatterplot used again as I only have one categorical variable for the whole dataset, and the only really dependent variable that shows the education level of each state is the average SAT score, so this will go on the y label once more. no error bars as they are cluttering. 
```

##### The correlation heatmap shows a multitude of variables, and there are strong correlations between verbal and math SAT scores, and in turn the average SAT score. However, the biggest take away from this plot is that there are not strong correlations between any of the variables. This trend continues with the following two scatterplots, which relate teacher salaries and pupil expenditures to SAT score by region. Neither plot shows a strong correlation, and if anything an increase in taecher salary seems to either have no substantial effect on SAT scores, or may even have a negative correlation. Again, the data shows no strong correlation with any variables and SAT scores, which are a sign of good education.

### PCA/CLUSTERING

``` r
pca <- combinedstates_num%>%
  scale()%>%
  prcomp()
#standardized and scaled already solely numeric dataset
names(pca)
```

    ## [1] "sdev"     "rotation" "center"   "scale"    "x"

``` r
pca_data <- data.frame(pca$x, region = combinedstates$region, state = combinedstates$state)
#added back in categorical variables
ggplot(pca_data, aes(x = PC1, y = PC2, color = region)) + 
  geom_point()
```

<img src="Project-1-Comp-Bio_files/figure-gfm/pca or clustering-1.png" style="display: block; margin: auto;" />

``` r
#mapped on scatterplot the principal components by region
percent <- 100* (pca$sdev^2 / sum(pca$sdev^2))
percent
```

    ##  [1] 6.829840e+01 1.763876e+01 7.991000e+00 2.940537e+00 1.570842e+00
    ##  [6] 6.192295e-01 3.624902e-01 2.878064e-01 1.839556e-01 8.286377e-02
    ## [11] 1.535115e-02 8.763822e-03 1.060091e-30 4.664511e-31 1.752995e-31
    ## [16] 1.142076e-31 6.346171e-32 4.844422e-32 3.187157e-32 2.770323e-32

``` r
#saw how much effect components had on variance within data
perc_data <- data.frame(percent = percent, PC = 1:length(percent))
ggplot(perc_data, aes(x = PC, y = percent)) + 
  geom_col() + 
  geom_text(aes(label = round(percent, 2)), size = 4, vjust = -0.5) + 
  ylim(0, 80)
```

<img src="Project-1-Comp-Bio_files/figure-gfm/pca or clustering-2.png" style="display: block; margin: auto;" />

``` r
#visualize with a bargraph the variance caused by PCs
```

##### The first principal component, which holds the most information out of all the principal components, accounts for almost 70% of the variance in the dataset (68.3%), and the second accounts for about 18% (17.64%). Because of the many redundant variables in my dataset, such as the different years for variables and the different types of SAT scores, this analysis was useful in better understanding and simplifying the data.

------------------------------------------------------------------------

    ##           sysname           release           version          nodename 
    ##         "Windows"          "10 x64"     "build 19042" "LAPTOP-VDO7L3JC" 
    ##           machine             login              user    effective_user 
    ##          "x86-64"              "HP"              "HP"              "HP"
