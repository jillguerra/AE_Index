# Untitled
Jill Guerra  
January 3, 2016  

Load packages 

```r
library(ggplot2)
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
library(gridExtra)
library("reshape2")
```

Import file

```r
aei<- read.csv("~/jillguerra/AEI/Census_data_formatted_for_R_01.03.16.csv", quote = '"', sep = ",", na.strings = c(".",""), strip.white = TRUE) # load full dataset
```

Reshape data 

```r
# this may be useful down the road but not using at the moment. 
aei_melted <- melt(aei) # new dataframe with melted data 
```

```
## Using X as id variables
```

```r
aes_index_var <- c("nochem", "croprot", "covercrop", "apm", "noconvtill", "directplanting", "lowtill", "drip") # drop of just index variables 
aes_socecon_var <- c("lowincome", "offfarminc", "association", "credit", "regtech", "famlab")# group of just the social and economic variables 

aei_melted2 <- aei_melted %>% 
  filter(variable %in% aes_index_var) # new dataframe with just the index variables 

(ggplot(aei_melted2, aes(x = X, y = value, colour = variable)) +
  geom_point()) # graph works. But now I really know that this graph should be showing the fitted values not the points. 
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-3-1.png) 

```r
# not sure how to use this because the x variable doesn't make sense 
```

Checking distribution 

```r
str(aei)
```

```
## 'data.frame':	293 obs. of  18 variables:
##  $ Municipality.Code: int  4200051 4200101 4200200 4200309 4200408 4200507 4200556 4200606 4200705 4200754 ...
##  $ X                : Factor w/ 293 levels "Abdon Batista",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ nochem           : num  21.1 25.3 17.3 10.2 49.6 ...
##  $ croprot          : num  18.3 24.5 24.8 11.6 33 ...
##  $ covercrop        : num  8.82 3.43 33.49 60.75 3.59 ...
##  $ apm              : num  1.26 0.43 55.99 3.97 40.74 ...
##  $ noconvtill       : num  73.7 73.2 37.1 16.8 82.9 ...
##  $ directplanting   : num  77 87.2 54.6 37 77.4 ...
##  $ lowtill          : num  8.36 3.46 9.01 11.78 10.65 ...
##  $ soil             : num  46.6 75.7 72.8 91.1 79 ...
##  $ drip             : num  57.14 6.67 21.62 7.02 50 ...
##  $ fert             : num  81.7 83.2 89.8 92.7 83 ...
##  $ famlab           : num  85.7 95.9 85.9 81 78.1 ...
##  $ lowincome        : num  51.9 60.8 38.5 24.8 26.4 ...
##  $ associated       : num  46.5 63.4 38.9 49.6 56.1 ...
##  $ regtech          : num  9.62 17.2 22.26 46.74 21.34 ...
##  $ credit           : num  60.9 34 30.6 40.1 47.8 ...
##  $ offfarminc       : num  56.5 12.7 26 15.1 23.6 ...
```

```r
# loop to look at the distributions for all variables. It currently works. Saves all individual graphs in png files 
nm <- names(aei)
for (i in seq_along(nm)) {
  plots <- ggplot(aei, aes_string(x=nm[i])) +
    geom_histogram()
  ggsave(plots,filename=paste("myplot",nm[i],".png",sep=""))
 
}
```

```
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Saving 7 x 5 in image
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

```r
hist(aei$nochem)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-1.png) 

```r
hist(aei$croprot)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-2.png) 

```r
hist(aei$covercrop)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-3.png) 

```r
hist(aei$apm)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-4.png) 

```r
hist(aei$noconvtill)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-5.png) 

```r
hist(aei$directplanting)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-6.png) 

```r
hist(aei$lowtill)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-7.png) 

```r
hist(aei$drip)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-8.png) 

```r
hist(aei$lowincome) # normal 
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-9.png) 

```r
hist(aei$offfarminc)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-10.png) 

```r
hist(aei$credit)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-11.png) 

```r
hist(aei$associated)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-12.png) 

```r
hist(aei$famlab)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-13.png) 

```r
hist(aei$regtech)
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-4-14.png) 
Results
***
- skewed to the right: nochem, croprot, covercrop, apm, low till, drip, offfarminc, regtech
- skewed to the left: soil, assoc (sort of), famlab
- credit, noconvtill and direct planting are both very strange. 

Check variability 

```r
var(aei$nochem)
```

```
## [1] 612.1983
```

```r
var(aei$croprot)
```

```
## [1] 319.1842
```

```r
var(aei$covercrop)
```

```
## [1] 247.9211
```

```r
var(aei$apm)
```

```
## [1] 284.9625
```

```r
var(aei$noconvtill)
```

```
## [1] 858.4171
```

```r
var(aei$directplanting)
```

```
## [1] 1149.033
```

```r
var(aei$lowtill)
```

```
## [1] 302.3407
```

```r
var(aei$drip)
```

```
## [1] NA
```

```r
var(aei$soil)
```

```
## [1] 466.6972
```

```r
var(aei$lowincome) 
```

```
## [1] NA
```

```r
var(aei$offfarminc)
```

```
## [1] 166.7895
```

```r
var(aei$credit)
```

```
## [1] 382.7097
```

```r
var(aei$associated)
```

```
## [1] NA
```

```r
var(aei$famlab)
```

```
## [1] NA
```

```r
var(aei$regtech)
```

```
## [1] 225.7877
```
Results:
***
- noconvtill and directplanting are incredibly varied compared to the others 
- lowinc, assoc, famlab and drip have no variance? Weird 

###### How do you know how much variance is ok?? 

Checking Bivariate relationships of index components 

```r
# NOCHEM
a1 <- ggplot(aei, aes(nochem, croprot)) +
  geom_point()+
  stat_smooth() 
a2 <- ggplot(aei, aes(nochem, covercrop)) +
  geom_point() +
  stat_smooth() 
a3 <- ggplot(aei, aes(nochem, apm)) +
  geom_point() +
  stat_smooth() 
a4 <- ggplot(aei, aes(nochem, noconvtill)) +
  geom_point() +
  stat_smooth() 
a5 <- ggplot(aei, aes(nochem, directplanting)) +
  geom_point() +
  stat_smooth() 
a6 <- ggplot(aei, aes(nochem, lowtill)) +
  geom_point() +
  stat_smooth() 
a7 <- ggplot(aei, aes(nochem, soil)) +
  geom_point() +
  stat_smooth() 
a8 <- ggplot(aei, aes(nochem, drip)) +
  geom_point() +
  stat_smooth()
a9 <- ggplot(aei, aes(nochem, fert)) +
  geom_point() +
  stat_smooth()
grid.arrange(a1,a2,a3,a4,a5,a6,a7,a8,a9, ncol=3)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-6-1.png) 

```r
# CROPROT
b1 <- ggplot(aei, aes(croprot, covercrop)) +
  geom_point() +
  stat_smooth() 
b2 <- ggplot(aei, aes(croprot, apm)) +
  geom_point() +
  stat_smooth() 
b3 <- ggplot(aei, aes(croprot, noconvtill)) +
  geom_point() +
  stat_smooth() 
b4 <- ggplot(aei, aes(croprot, directplanting)) +
  geom_point() +
  stat_smooth() 
b5 <- ggplot(aei, aes(croprot, lowtill)) +
  geom_point() +
  stat_smooth()
b6 <- ggplot(aei, aes(croprot, soil)) +
  geom_point() +
  stat_smooth()
b7 <- ggplot(aei, aes(croprot, drip)) +
  geom_point() +
  stat_smooth()
b8 <- ggplot(aei, aes(croprot, fert)) +
  geom_point() +
  stat_smooth()
grid.arrange(b1,b2,b3,b4,b5,b6,b7,b8, ncol=3) # put all the graphs into one grid 
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-6-2.png) 

```r
# COVERCROP
c1 <- ggplot(aei, aes(covercrop, apm)) +
  geom_point() +
  stat_smooth() 
c2 <- ggplot(aei, aes(covercrop, noconvtill)) +
  geom_point() +
  stat_smooth() 
c3 <- ggplot(aei, aes(covercrop, directplanting)) +
  geom_point() +
  stat_smooth() 
c4 <- ggplot(aei, aes(covercrop, lowtill)) +
  geom_point() +
  stat_smooth()
c5 <- ggplot(aei, aes(covercrop, soil)) +
  geom_point() +
  stat_smooth()
c6 <- ggplot(aei, aes(covercrop, drip)) +
  geom_point() +
  stat_smooth()
c7 <- ggplot(aei, aes(covercrop, fert)) +
  geom_point() +
  stat_smooth()
grid.arrange(c1,c2,c3,c4,c5,c6,c7, ncol=3) # put all the graphs into one grid 
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-6-3.png) 

```r
# APM 
d1 <- ggplot(aei, aes(apm, noconvtill)) +
  geom_point() +
  stat_smooth() 
d2 <- ggplot(aei, aes(apm, directplanting)) +
  geom_point() +
  stat_smooth() 
d3 <- ggplot(aei, aes(apm, lowtill)) +
  geom_point() +
  stat_smooth()
d4 <- ggplot(aei, aes(apm, soil)) +
  geom_point() +
  stat_smooth()
d5 <- ggplot(aei, aes(apm, drip)) +
  geom_point() +
  stat_smooth()
d6 <- ggplot(aei, aes(apm, fert)) +
  geom_point() +
  stat_smooth()
grid.arrange(d1,d2,d3,d4,d5,d6, ncol=3) # put all the graphs into one grid 
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-6-4.png) 

```r
#skipped a few

#DRIP
e1 <- ggplot(aei, aes(drip, noconvtill)) +
  geom_point() +
  stat_smooth() 
e2 <- ggplot(aei, aes(drip, directplanting)) +
  geom_point() +
  stat_smooth() 
e3 <- ggplot(aei, aes(drip, lowtill)) +
  geom_point() +
  stat_smooth()
e4 <- ggplot(aei, aes(drip, soil)) +
  geom_point() +
  stat_smooth()
e5 <- ggplot(aei, aes(drip, fert)) +
  geom_point() +
  stat_smooth()
grid.arrange(e1,e2,e3,e4,e5, ncol=3)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 28 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-6-5.png) 
Results: 
***
- very clear relatioship between nochem and fertilizer. 
- some relationship  between fert and covercrop/croprot/apm


Checking bivariate relationships of SOCECON variables 

```r
# CREDIT
z <- ggplot(aei, aes(credit, associated)) +
  geom_point() +
  stat_smooth() 
z1 <- ggplot(aei, aes(credit, famlab)) +
  geom_point() +
  stat_smooth()
z2 <- ggplot(aei, aes(credit, lowincome)) +
  geom_point() +
  stat_smooth() 
z3 <- ggplot(aei, aes(credit, offfarminc)) +
  geom_point() +
  stat_smooth() 

grid.arrange(z, z1, z2,z3, ncol=3)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 2 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 1 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 1 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-7-1.png) 

```r
# ASSOCIATED
y <- ggplot(aei, aes(associated, famlab)) +
  geom_point() +
  stat_smooth()

y1 <- ggplot(aei, aes(associated, lowincome)) +
  geom_point() +
  stat_smooth() 
y2 <- ggplot(aei, aes(associated, offfarminc)) +
  geom_point() +
  stat_smooth() 

grid.arrange(y, y1, y2, ncol=3)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 3 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 3 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 2 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-7-2.png) 

```r
# FAMLAB
x <- ggplot(aei, aes(famlab, lowincome)) +
  geom_point() +
  stat_smooth() 
x1 <- ggplot(aei, aes(famlab, offfarminc)) +
  geom_point() +
  stat_smooth() 
grid.arrange(x, x1, ncol=2)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 1 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 1 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-7-3.png) 

```r
# lowincome & offfarm
(w <- ggplot(aei, aes(lowincome, offfarminc)) +
  geom_point() +
  stat_smooth()) 
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 1 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-7-4.png) 
Results
***
- credit and associated positively related 
- no other ones are related in any significant way. 

Relationship between nochem and SOCECON variables 

```r
#NOCHEM & SE VARIABLES
n <- ggplot(aei, aes(nochem, associated)) +
  geom_point() +
  stat_smooth() 
n1 <- ggplot(aei, aes(nochem, famlab)) +
  geom_point() +
  stat_smooth()
n2 <- ggplot(aei, aes(nochem, lowincome)) +
  geom_point() +
  stat_smooth() 
n3 <- ggplot(aei, aes(nochem, offfarminc)) +
  geom_point() +
  stat_smooth() 
n4 <- ggplot(aei, aes(nochem, credit)) +
  geom_point() +
  stat_smooth() 
grid.arrange(n, n1, n2,n3,n4, ncol=3)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 2 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 1 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 1 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-8-1.png) 
Results: 
***
- interesting negative relationship between associatedand credit and nochem. Seems to show that more associatedness and access to credit will actually lead to higher levels of agrichem useage. 

Testing to get all on the same graph

```r
(ggplot(aei, aes(x=X)) + 
  geom_line(aes(y=nochem, color="disp")) + 
  geom_line(aes(y=drip, color="hp")) + 
  geom_line(aes(y=lowtill, color="wt")))
```

```
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-9-1.png) 

```r
###### issue right now is that the group aesthetic is off. Need to change stat_bin? 
```

Regression tests with NOCHEM

```r
#REGRESS NO CHEM
nochem_lm <- lm(nochem ~ offfarminc + credit + lowincome + famlab + regtech + associated, aei, , , na.exclude) # regression testing on nochem 
#na.exclude included because otherwise I cannot add the fitted values into the dataframe because there are lost observations 
# the spaces are for two other arguments that can be specified. I don't know how else to just leave them as default. 

aei_fit <- aei %>% 
  mutate(nochemfit = fitted(nochem_lm)) # insert new column with the fitted values for nochem into a new dataframe that will hold the fitted values 

# plot with fitted values 
plot_nochemfit <- ggplot(aei_fit, aes(x=associated, y = nochemfit)) +
  geom_point() +
  stat_smooth()

#plot with regular values 
plot_nochem <- ggplot(aei, aes(associated, nochem)) +
   geom_point() +
   stat_smooth() 

grid.arrange(plot_nochemfit, plot_nochem, ncol = 2)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 3 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 2 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
```

![](Data_checks_bivar_dist_etc_01.05.15_files/figure-html/unnamed-chunk-10-1.png) 

```r
nochem_lm
```

```
## 
## Call:
## lm(formula = nochem ~ offfarminc + credit + lowincome + famlab + 
##     regtech + associated, data = aei, na.action = na.exclude)
## 
## Coefficients:
## (Intercept)   offfarminc       credit    lowincome       famlab  
##     28.8325       0.4196      -0.6527       0.3033       0.2678  
##     regtech   associated  
##     -0.1805      -0.1172
```


Principle Component Analysis 


Creating AES with a function/loop 

```r
# ##### find a way to do the average of each row and store in new column 
# 
# for i in row_number(1:293) { aei %>% 
#   mutate(aes = mean(aei[i,]))}
# 
# aesnm <- c("nochem", "croprot", "covercrop", "apm", "noconvtill", "directplanting", "lowtill", "soil"))
# 
# for (i in seq_along(aesnm) { aei %>% 
#   mutate(aes = mean(aes)) }
```



```r
######## AES with just 6 variables 

aei$aes <- ((aei$nochem + aei$croprot + aei$covercrop + aei$apm + aei$lowtill + aei$drip)/6) # brute force mean 
# note that this creates an NA for any observations that have any NAs in them. 
```


Check for scale consistency - Chronbach's alpha

```r
#alpha(aei$aes)
```

Checking for leverage and outliers 


RESOURCES 
***

Visualizing distribution: 
https://flowingdata.com/2012/05/15/how-to-visualize-and-compare-distributions/

Creating a looping graph 
http://www.r-bloggers.com/ggplot2-graphics-in-a-loop/



