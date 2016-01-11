# Principle Component Analysis & Index Construction
Jill Guerra  
January 5, 2016  
###Load packages 

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
library(car) 
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
suppressPackageStartupMessages(library(dplyr))
library(gridExtra)
library("reshape2")
#install.packages("psych")
library(psych) # needed for chronbach's alpha
```

```
## 
## Attaching package: 'psych'
## 
## The following object is masked from 'package:car':
## 
##     logit
## 
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

###Import file - Remember to change depending on what you need. 

```r
rawdf_aei<- read.csv("~/AEI_Index/Census_data_formatted_for_R_01.06.16.csv", quote = '"', sep = ",", na.strings = c(".",""), strip.white = TRUE) # load full dataset
```

###Principle Component Analysis 
ALL FROM HERE: http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/

```r
# omit NAs for this part 
rawdf_aei_omit <- na.omit(rawdf_aei)

# log transform 
log_aei <- log(rawdf_aei_omit [, 3:17]) # columns with the AE components going to be used in the index 
aei_species <- rawdf_aei_omit [, 2] # this calls the categorical variable 
# calls the Municipality names.

#Change all infinite values to NAs 
log_aei<- do.call(data.frame,lapply(log_aei, function(x) replace(x, is.infinite(x),NA))) # this changes all of them! 
# ok but the data.frame doesn't have any other things in there (only the components)

#Drop all observations with NA (thus all the infinite because they were previously changed to NA)
log_aei <- na.omit(log_aei) # shows that it is changed from 262 levels down to 139 levels. 

#Apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
log_aei_pca <- prcomp(log_aei, 
                      na.action = na.exclude, 
                      center = TRUE,               
                      scale. = TRUE) 
print(log_aei_pca)
```

```
## Standard deviations:
##  [1] 1.9537455 1.4801481 1.2401097 1.1747438 1.0279358 0.9948626 0.9047420
##  [8] 0.8476518 0.7342388 0.7247449 0.6906841 0.6214243 0.5151770 0.3990816
## [15] 0.3720510
## 
## Rotation:
##                        PC1         PC2         PC3          PC4
## nochem          0.35414451 -0.22372340  0.01410944 -0.003816553
## usefert        -0.41516868  0.20882271  0.13477719 -0.045697372
## orgcomp         0.06176453 -0.38340657 -0.09206370  0.325365455
## nitchem        -0.22086229  0.34110971 -0.27828297 -0.064633349
## notnitchem     -0.06860530  0.15159029 -0.16469643  0.562294729
## manure         -0.16841420 -0.22284903  0.48984800  0.087932369
## covercrop      -0.14955435  0.22218675  0.10035631  0.420650267
## Innoculents     0.21243526 -0.22575221 -0.12362575  0.389606337
## croprot        -0.33316959 -0.29041529 -0.15156639 -0.117469860
## apm            -0.27509928 -0.06975687  0.14995904  0.377261006
## noconvtill     -0.26224035 -0.43555657  0.09134311 -0.148391544
## directplanting -0.38370618 -0.24514934 -0.19088886 -0.165611067
## lowtill         0.05336230 -0.07813395  0.61899396  0.024608208
## soil           -0.34459853  0.14938401  0.11008472  0.106283490
## drip           -0.15741233 -0.32027625 -0.34318192  0.133176469
##                        PC5         PC6         PC7         PC8         PC9
## nochem          0.23954679 -0.19055258  0.08358938 -0.06253326 -0.42837095
## usefert        -0.13232753 -0.20863766 -0.18608552 -0.13958871  0.01015788
## orgcomp        -0.22466651 -0.20006255 -0.52775031  0.36909179 -0.25473626
## nitchem         0.13367216  0.44393329 -0.17585032  0.01031423 -0.41860761
## notnitchem      0.43079028  0.00173532  0.12666619  0.43865996  0.31986227
## manure         -0.10501080 -0.13426447  0.47303470  0.18035527 -0.19143425
## covercrop      -0.49905006  0.20809665  0.27057844 -0.03426823 -0.32445308
## Innoculents    -0.33529871  0.34998080 -0.04794634 -0.35251138  0.35051777
## croprot        -0.02452250  0.01930832  0.27432240  0.17318743  0.23796069
## apm             0.42568379  0.04546493 -0.08917076 -0.21761105 -0.25564718
## noconvtill     -0.03451228  0.30267968 -0.27666241  0.12453990 -0.03417892
## directplanting  0.02321096  0.20893764  0.11408974  0.09987505  0.01785380
## lowtill         0.27680649  0.34912915 -0.19294144 -0.17501870  0.18077378
## soil           -0.09594300 -0.44999278 -0.29963508 -0.18146294  0.20912931
## drip            0.17121617 -0.19272531  0.18399248 -0.56875853 -0.09643055
##                       PC10         PC11        PC12        PC13
## nochem          0.51729222  0.069779015  0.21806216  0.33063248
## usefert        -0.12070755 -0.022679266  0.10858339 -0.06565626
## orgcomp        -0.13941552 -0.108194925 -0.25431800 -0.02346875
## nitchem        -0.17949216 -0.138074721 -0.05194035  0.52244449
## notnitchem      0.06228871 -0.245642333  0.20354579  0.02897725
## manure         -0.43757609 -0.066554096  0.19504836  0.32708310
## covercrop       0.42301907 -0.149424920 -0.11080370 -0.21922606
## Innoculents    -0.12718048  0.241324799  0.20808080  0.32539369
## croprot         0.22950821  0.180843557 -0.61694015  0.31364002
## apm            -0.09018391  0.610692898 -0.11005286 -0.20044226
## noconvtill      0.11304998 -0.148040746  0.32583822 -0.06823579
## directplanting  0.21692788  0.091834836  0.36451081 -0.20103779
## lowtill         0.17140325 -0.349618782 -0.27119140  0.03380149
## soil            0.32269452 -0.009917387  0.15390519  0.39469496
## drip           -0.14681828 -0.510077588 -0.09610523 -0.10277211
##                       PC14        PC15
## nochem          0.32512208  0.03449623
## usefert         0.77976351  0.09644565
## orgcomp         0.06244014 -0.25384316
## nitchem        -0.03040921 -0.06469340
## notnitchem      0.12659610  0.10066880
## manure         -0.07556868 -0.07523646
## covercrop      -0.04900211  0.07228315
## Innoculents     0.16579701 -0.03759388
## croprot         0.15058282  0.13416599
## apm            -0.11067522  0.07133635
## noconvtill     -0.12808516  0.60266198
## directplanting  0.03912575 -0.65650237
## lowtill         0.09482894 -0.27008545
## soil           -0.40729697 -0.08823823
## drip           -0.04549501  0.02086699
```

```r
# plot method
plot(log_aei_pca, type = "l") # this won't work unless the NAs are omitted a few lines up. 
```

![](PCA_and_Index_Creation_01.06.15_files/figure-html/unnamed-chunk-3-1.png) 

```r
# summary method
summary(log_aei_pca)
```

```
## Importance of components:
##                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
## Standard deviation     1.9537 1.4801 1.2401 1.1747 1.02794 0.99486 0.90474
## Proportion of Variance 0.2545 0.1461 0.1025 0.0920 0.07044 0.06598 0.05457
## Cumulative Proportion  0.2545 0.4005 0.5031 0.5951 0.66550 0.73148 0.78605
##                           PC8     PC9    PC10   PC11    PC12    PC13
## Standard deviation     0.8477 0.73424 0.72474 0.6907 0.62142 0.51518
## Proportion of Variance 0.0479 0.03594 0.03502 0.0318 0.02574 0.01769
## Cumulative Proportion  0.8340 0.86990 0.90491 0.9367 0.96246 0.98015
##                           PC14    PC15
## Standard deviation     0.39908 0.37205
## Proportion of Variance 0.01062 0.00923
## Cumulative Proportion  0.99077 1.00000
```

### Graphing the PCA 

```r
library(devtools)
#install_github("ggbiplot", "vqv") 
 
library(ggbiplot)
```

```
## Loading required package: plyr
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## Loading required package: scales
## 
## Attaching package: 'scales'
## 
## The following objects are masked from 'package:psych':
## 
##     alpha, rescale
## 
## Loading required package: grid
```

```r
plot_pca <- ggbiplot(log_aei_pca, obs.scale = 1, var.scale = 1,
              ellipse = TRUE, 
              circle = TRUE)
# original code for this at group = aei_species - but I had to take it out because there were different number of observations. This is because I had to drop values in the log_aei df as prcomp would not work with NAs. 

plot_pca <- plot_pca + scale_color_discrete(name = '') + 
  theme(legend.direction = 'horizontal', legend.position = 'top')

print(plot_pca)
```

![](PCA_and_Index_Creation_01.06.15_files/figure-html/unnamed-chunk-4-1.png) 

###AEI with brute force 

```r
#### randomly chosen variables 
# this works for the regular data.frame 
ae_index <- ((rawdf_aei$nochem + rawdf_aei$usefert +rawdf_aei$orgcomp + rawdf_aei$manure + rawdf_aei$croprot + rawdf_aei$covercrop + rawdf_aei$apm + rawdf_aei$lowtill + rawdf_aei$drip)/9) # create index! 

add_aei_df <- cbind(rawdf_aei, ae_index) # add the index into a new df 
# NAs were not excluded 
```

Drop NA observations when needed

```r
#Create a new data.frame that only has rows w/o NAs
add_aei_df_dropnas <- na.omit(rawdf_aei) # 262/293 municipalities included 
```


###Check for scale consistency - Chronbach's alpha

```r
#  # create df with just index components (no nochem)
# thisscale <- c(5,8,9,11,12,15,16,17) # calling a selection 
# df_aei_alpha <- rawdf_aei[, thisscale] # calls out just the index variables 
# alpha(df_aei_alpha)
# # is 0.47. Not good. 
# 
# thisscale2 <- c(5,8,9,11,12,15,17) # dropped soil 
# df_aei_alpha2 <- rawdf_aei[, thisscale2] # calls out just the index variables 
# alpha(df_aei_alpha2) # 0.43 
# 
# thisscale3 <- c(5,8,9,11,12,15) # dropped soil and drip
# df_aei_alpha3 <- rawdf_aei[, thisscale3] # calls out just the index variables 
# alpha(df_aei_alpha3) # 0.44 again 
```

Results: 
scale with nochem was baaaaad. 



##Resources & Notes 

####PCA 
overview: http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
cleaning INF values: http://stackoverflow.com/questions/12188509/cleaning-inf-values-from-an-r-dataframe
* can't use is.infinite for a data.frame 

Help with diagnostics: 
http://www.ats.ucla.edu/stat/r/dae/rreg.htm
