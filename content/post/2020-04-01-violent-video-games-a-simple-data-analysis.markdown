---
title: 'Violent Video Games: A Simple Data Analysis'
author: Vincent Le
date: '2020-04-01'
slug: violent-video-games-a-simple-data-analysis
categories:
  - psychology
tags:
  - video games
  - violent
  - data
  - science
  - statistics
  - regression
  - R
subtitle: ''
summary: ''
authors: []
lastmod: '2020-04-01T17:28:17-07:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---
{{% toc %}} 
**Note: All source code and full functions can be found on this [Github repo](https://github.com/vincentle1/Violent-Video-Game-Data-Analysis).**

## Introduction 
Hello everyone! In the last post, I explored the [existing literature](https://vincentle1.netlify.com/post/violent-video-games-literature-review/) on violent video games and violent crime. Well, I decided to run a simple regression with violent video game sales as the independent variable, and violent crime as the dependent variable. In this post, I'll show you what I found.

## Gathering and Tidying the Data

Before I share the results, let me walk you through how I collected and cleaned the data.

First, where did I get the data?

For violent video games, I used https://www.vgchartz.com/, which had tables that listed the 100 top-selling video games in the United States by year and genre. I decided to only include games from the action, shooter and fighting genre, in order to really make sure I was exploring violent video games and not video games in general. 

For violent crime, I used a table from the [Uniform Crime Reporting Program](https://www.fbi.gov/services/cjis/ucr), a website maintained by the FBI. Violent crime is defined by the FBI as murder/manslaughter, rape, robbery and aggravated assault.

Now, here's how I compiled and cleaned everything.

1. First, I wrote a webscraper function that automatically pulled the video game sale tables off of https://www.vgchartz.com/, and then filtered them by genre. It allowed me to pull a table from any year I wanted and put that into an R dataframe. Because I'm really proud of it, here's the whole thing below!



```r
library(rvest)
```

```
## Loading required package: xml2
```

```r
library(stringr)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
## ✓ tibble  2.1.3     ✓ dplyr   0.8.5
## ✓ tidyr   1.0.2     ✓ forcats 0.5.0
## ✓ readr   1.3.1
```

```
## ── Conflicts ────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter()         masks stats::filter()
## x readr::guess_encoding() masks rvest::guess_encoding()
## x dplyr::lag()            masks stats::lag()
## x purrr::pluck()          masks rvest::pluck()
```

```r
vg_scraper <- function(year){
#Scrape the chart
url <- paste("https://www.vgchartz.com/yearly/", year, "/USA/", sep = "") 

# read_html() finds the
# html_nodes finds the full element containing the table.

vg_list <- url %>%
  read_html() %>%
#html_nodes finds the table by CSS selector.
  html_nodes('#chart_body > table') %>%
#html_table puts it into a list of tables.
  html_table(fill = TRUE)

#This extracts the table into a final dataframe we can work with.
vg_dataframe <- vg_list[[1]]

##Clean up the chart

#1. Select columns
vg_dataframe <- vg_dataframe[c(2, 8)]

#2. Rename columns
names(vg_dataframe)[2] <- "Yearly_Sales"

#3. Remove all rows with NA
vg_dataframe <- vg_dataframe %>%
  filter(!is.na(vg_dataframe$Yearly_Sales))

#4. Remove commas from Yearly_Sales using gsub and convert to numeric
vg_dataframe$Yearly_Sales <- as.numeric(gsub(",", "", vg_dataframe$Yearly_Sales))

#5. Filter by genre. Action, shooter and fighting
vg_dataframe <- vg_dataframe %>%
  filter(str_detect(Game, "Action|Shooter|Fighting" ))

#6. Return dataframe
vg_dataframe
}
```

Here's some sample output from this function. Say I wanted the video game sale data from 2015. I could just type vg_scraper(2015) and get:


```r
vg_scraper(2015)
```

```
##                                                                               Game
## 1                               Call of Duty: Black Ops 3 (PS4)Activision, Shooter
## 2                              Call of Duty: Black Ops 3 (XOne)Activision, Shooter
## 3                      Star Wars: Battlefront (2015) (PS4)Electronic Arts, Shooter
## 4                          Halo 5: Guardians (XOne)Microsoft Game Studios, Shooter
## 5  Uncharted: The Nathan Drake Collection (PS4)Sony Computer Entertainment, Action
## 6                     Star Wars: Battlefront (2015) (XOne)Electronic Arts, Shooter
## 7                               Gears of War (XOne)Microsoft Game Studios, Shooter
## 8                                                 Splatoon (WiiU)Nintendo, Shooter
## 9        Batman: Arkham Knight (PS4)Warner Bros. Interactive Entertainment, Action
## 10                            Grand Theft Auto V (PS4)Take-Two Interactive, Action
## 11           Mortal Kombat X (PS4)Warner Bros. Interactive Entertainment, Fighting
## 12                             Call of Duty: Black Ops 3 (X360)Activision, Shooter
## 13                            Super Smash Bros. for Wii U (WiiU)Nintendo, Fighting
## 14                     The Legend of Zelda: Majora's Mask 3D (3DS)Nintendo, Action
## 15                           Grand Theft Auto V (XOne)Take-Two Interactive, Action
## 16  Metal Gear Solid V: The Phantom Pain (PS4)Konami Digital Entertainment, Action
## 17          Mortal Kombat X (XOne)Warner Bros. Interactive Entertainment, Fighting
## 18                             Bloodborne (PS4)Sony Computer Entertainment, Action
## 19                      Super Smash Bros. for Nintendo 3DS (3DS)Nintendo, Fighting
## 20                                   Assassin's Creed: Unity (XOne)Ubisoft, Action
## 21                 Dying Light (PS4)Warner Bros. Interactive Entertainment, Action
## 22      Batman: Arkham Knight (XOne)Warner Bros. Interactive Entertainment, Action
## 23                        Call of Duty: Advanced Warfare (XOne)Activision, Shooter
## 24                         Call of Duty: Advanced Warfare (PS4)Activision, Shooter
## 25                            Battlefield: Hardline (XOne)Electronic Arts, Shooter
## 26                                 Assassin's Creed Syndicate (PS4)Ubisoft, Action
## 27                             Battlefield: Hardline (PS4)Electronic Arts, Shooter
## 28         Halo: The Master Chief Collection (XOne)Microsoft Game Studios, Shooter
## 29                Dying Light (XOne)Warner Bros. Interactive Entertainment, Action
## 30                           Grand Theft Auto V (X360)Take-Two Interactive, Action
## 31                               Destiny: The Taken King (XOne)Activision, Shooter
## 32                                Destiny: The Taken King (PS4)Activision, Shooter
## 33                        The Order 1866 (PS4)Sony Computer Entertainment, Shooter
## 34      The Last of Us Remastered (PS4)Sony Computer Entertainment America, Action
## 35                                Assassin's Creed Syndicate (XOne)Ubisoft, Action
## 36                              Call of Duty: Black Ops 3 (PS3)Activision, Shooter
## 37                                               Destiny (XOne)Activision, Shooter
## 38                        Call of Duty: Advanced Warfare (X360)Activision, Shooter
## 39                                                Destiny (PS4)Activision, Shooter
## 40                            Grand Theft Auto V (PS3)Take-Two Interactive, Action
## 41                          Legend of Zelda: Triforce Heroes (3DS)Nintendo, Action
## 42                                  Call of Duty: Ghosts (X360)Activision, Shooter
## 43        LEGO Jurassic World (X360)Warner Bros. Interactive Entertainment, Action
## 44                                                 Far Cry 4 (PS4)Ubisoft, Shooter
## 45                                    Assassin's Creed: Unity (PS4)Ubisoft, Action
## 46                                       Rainbow Six: Siege (XOne)Ubisoft, Shooter
## 47                                           Just Cause 3 (PS4)Square Enix, Action
## 48                                   Call of Duty: Ghosts (PS3)Activision, Shooter
## 49 Metal Gear Solid V: The Phantom Pain (XOne)Konami Digital Entertainment, Action
## 50                                      Evolve (XOne)Take-Two Interactive, Shooter
##    Yearly_Sales
## 1       3700540
## 2       3255118
## 3       2352056
## 4       1986935
## 5       1486339
## 6       1462792
## 7       1380928
## 8       1359837
## 9       1255687
## 10      1228340
## 11      1215232
## 12       999514
## 13       972358
## 14       969236
## 15       951161
## 16       892091
## 17       839795
## 18       798391
## 19       779206
## 20       741251
## 21       728452
## 22       724832
## 23       662146
## 24       651373
## 25       634328
## 26       632231
## 27       613363
## 28       609817
## 29       605742
## 30       573422
## 31       565348
## 32       563393
## 33       522369
## 34       503449
## 35       482432
## 36       464560
## 37       463267
## 38       458462
## 39       448958
## 40       425554
## 41       415209
## 42       404213
## 43       395445
## 44       390506
## 45       370251
## 46       341348
## 47       334048
## 48       329513
## 49       298388
## 50       298081
```

2. I then wrote another function that used this vg_scraper function to combine all the video game sales from 2005-2018 into one simple dataframe. Here's the output.


```
##    year sum_year_sale
## 1  2005       4590918
## 2  2006      12404813
## 3  2007      18365623
## 4  2008      34872965
## 5  2009      36903451
## 6  2010      43923884
## 7  2011      48619875
## 8  2012      47863548
## 9  2013      51609135
## 10 2014      44006645
## 11 2015      43537307
## 12 2016      37788239
## 13 2017      35206500
## 14 2018      48205951
```

3. After this, I wrote a similar cleaner function to tidy the FBI data.


4. Lastly, I wrote a function that merged the FBI data with the final output of the video game data. Here's the final data frame. The sum_year_sale is video game sales, violent_crime_abs is total violent crime, violent_crime_rate is violent crime per 100,000 people.
  


```
## Joining, by = "year"
```

```
##    year sum_year_sale population violent_crime_abs violent_crime_rate
## 1  2005       4590918  296507061           1390745              469.0
## 2  2006      12404813  299398484           1435123              479.3
## 3  2007      18365623  301621157           1422970              471.8
## 4  2008      34872965  304059724           1394461              458.6
## 5  2009      36903451  307006550           1325896              431.9
## 6  2010      43923884  309330219           1251248              404.5
## 7  2011      48619875  311587816           1206005              387.1
## 8  2012      47863548  313873685           1217057              387.8
## 9  2013      51609135  316497531           1168298              369.1
## 10 2014      44006645  318907401           1153022              361.6
## 11 2015      43537307  320896618           1199310              373.7
## 12 2016      37788239  323405935           1250162              386.6
## 13 2017      35206500  325147121           1247917              383.8
## 14 2018      48205951  327167434           1206836              368.9
```

## Analysis

Okay, on to the fun stuff! 

1. I first plotted the data with violent crime rate (per 100,000 people) on the y-axis and annual violent video game sales (per 100,000 people) on the x-axis.



```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="/post/2020-04-01-violent-video-games-a-simple-data-analysis_files/figure-html/unnamed-chunk-6-1.png" width="672" />

Well well well. Looks like a negative correlation to me! And indeed it was. 


```
## [1] -0.835573
```

So as violent video game sales went up per year, the violent crime rate went down.

2. Now the question remained: Would this regression line be statistically significant? I ran a classic OLS regression model, and here's the output.


```r
model <- lm(violent_crime_rate ~ sum_year_rate, vvg_fbi)
summary(model)
```

```
## 
## Call:
## lm(formula = violent_crime_rate ~ sum_year_rate, data = vvg_fbi)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.266 -20.088   2.984  14.996  48.664 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   501.687520  18.691672  26.840  4.4e-12 ***
## sum_year_rate  -0.008000   0.001518  -5.269 0.000198 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.69 on 12 degrees of freedom
## Multiple R-squared:  0.6982,	Adjusted R-squared:  0.673 
## F-statistic: 27.76 on 1 and 12 DF,  p-value: 0.0001982
```

The adjusted R-Squared was 0.673, meaning the model explained 67.3% of the variation. The F-statistic, a ratio of signal to noise, was large and significant (meaning there was much more signal in the model than noise).

While this is a highly simplistic model, we can probably conclude that violent video games do not significantly contribute to violent crime.

Ta-da!
