# Scale_Regression_Checks
Jill Guerra  
April 6, 2016  
UPDATED: April 6


###PART 1: LOAD DATA, CREATE SCALE
***
This section loads the packages, imports data and cleans the data of small farms and observations with NA values 
####PACKAGES

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(gridExtra)) # for grid.arrange 
suppressPackageStartupMessages(library("moments")) # for skew & kurtosis
suppressPackageStartupMessages(library(knitr)) # for nice tables 
suppressPackageStartupMessages(library(ggbiplot)) # for PCA analysis 
suppressPackageStartupMessages(library(devtools)) # for PCA analysis as well, I believe 
suppressPackageStartupMessages(library(psych)) # needed for chronbach's alpha
suppressPackageStartupMessages(library(stats)) 
suppressPackageStartupMessages(library(stargazer)) 
```

#### DATA SET UP  - This should be in every file that I am working on)

```r
rawdf_aei<- read.csv("~/AEI_Index/Census_data_formatted_for_R_02.22.16.csv", quote = '"', sep = ",", na.strings = c(".",""), strip.white = TRUE) # load full dataset
# note that this dataset was updated in a feb 22 version to only have variables that I will be working with 
```


```r
small <- c("4202453", "4202008", "4208450", "4202057", "4201950", "4206306") # these are the codes for the municipalities that have fewer than 50 farms. Names in Result_tracking Rmd. 

'%!in%' <- function(x,y)!('%in%'(x,y)) # sweet function for negating an %in% function 

# drop the municipalities that are held in the 'small' value. 
df_aei <- rawdf_aei %>% 
  filter(code %!in% small) %>% 
  droplevels()  # need to drop levels or else the new data.frame will continue to store the other factors even though they aren't being used. See Homework 5 from stats class for more about this. 

str(df_aei) # check the factor levels. It should match the #obs shown in the global environment section. If it does not then the droplevels() didn't work. 
```


```r
na_obs <- df_aei[rowSums(is.na(df_aei)) > 0,] # pulls out new data.frame with just the NA values

na_obs_names <- na_obs[,1]

df_aei <- df_aei %>% 
  filter(code %!in% na_obs_names) %>% 
  droplevels()

nlevels(df_aei$municipality) # should be 273! 
```

```
## [1] 273
```

###PART 2: SCALE CREATION 
***
This section will define the Agscores scale and bind it to the original dataframe, but stored in a new dataframe ('df_aei_score')

####SCALE  

```r
#columns from current dataset:

# current column numbers for orgcomp (3), manure (4), covercrop (5), croprotation (6), apm(7), production diversity (8) straw(9)
ae_scale <- df_aei[,c(4:9)] # this one looks the best. It excludes organic compost because it as negatively related in the alpha of the scale in which it was included 
alpha(ae_scale)
```

```
## 
## Reliability analysis   
## Call: alpha(x = ae_scale)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean sd
##       0.73      0.74    0.76      0.32 2.8 0.039   27 14
## 
##  lower alpha upper     95% confidence boundaries
## 0.65 0.73 0.8 
## 
##  Reliability if an item is dropped:
##           raw_alpha std.alpha G6(smc) average_r S/N alpha se
## manure         0.74      0.74    0.76      0.37 2.9    0.041
## covercrop      0.76      0.79    0.79      0.43 3.7    0.041
## croprot        0.60      0.62    0.63      0.25 1.6    0.053
## apm            0.69      0.71    0.74      0.32 2.4    0.046
## diverse        0.67      0.68    0.70      0.30 2.1    0.048
## straw          0.63      0.65    0.65      0.27 1.8    0.051
## 
##  Item statistics 
##             n raw.r std.r r.cor r.drop mean sd
## manure    273  0.59  0.54  0.38   0.32   44 26
## covercrop 273  0.34  0.39  0.18   0.15   14 16
## croprot   273  0.86  0.85  0.88   0.78   23 18
## apm       273  0.62  0.65  0.52   0.46   18 17
## diverse   273  0.70  0.72  0.68   0.59   30 15
## straw     273  0.83  0.79  0.81   0.64   34 29
```

```r
# chronbach's is 0.73
```


```r
#Creating scores
agscores <- ((df_aei$apm +df_aei$croprot + df_aei$straw + df_aei$manure + df_aei$covercrop + df_aei$diverse)/6) # testing one score set 

str(agscores)

# bind the scores onto the original dataframe but store as a new dataframe
df_aei_scores <- cbind(df_aei, agscores) 
df_aei_scores
```

###PART 3: INITIAL REGRESSIONS
***
#### REGRESSIONS - without any leverage/outliers removed 

Regression with all independent variables 

```r
lm_sep <- lm(agscores ~ lowincome + credit + regtech + associated, df_aei_scores)
summary(lm_sep)
```

```
## 
## Call:
## lm(formula = agscores ~ lowincome + credit + regtech + associated, 
##     data = df_aei_scores)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -27.5673  -6.7611   0.6236   7.0751  27.9267 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.07657    3.54527   0.586   0.5586    
## lowincome    0.17184    0.07416   2.317   0.0213 *  
## credit       0.41591    0.04744   8.767   <2e-16 ***
## regtech      0.06734    0.05270   1.278   0.2024    
## associated   0.03921    0.04273   0.918   0.3597    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.01 on 268 degrees of freedom
## Multiple R-squared:  0.4627,	Adjusted R-squared:  0.4546 
## F-statistic: 57.69 on 4 and 268 DF,  p-value: < 2.2e-16
```


```r
lm_2by2_1 <- lm(agscores ~ lowincome + credit, df_aei_scores)
summary(lm_2by2_1)
```

```
## 
## Call:
## lm(formula = agscores ~ lowincome + credit, data = df_aei_scores)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -26.4999  -6.5544   0.3533   6.9520  27.2587 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.88505    2.60161   2.262   0.0245 *  
## lowincome    0.11877    0.06491   1.830   0.0684 .  
## credit       0.46658    0.03222  14.479   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.02 on 270 degrees of freedom
## Multiple R-squared:  0.4576,	Adjusted R-squared:  0.4536 
## F-statistic: 113.9 on 2 and 270 DF,  p-value: < 2.2e-16
```

```r
lm_2by2_2 <- lm(agscores ~ regtech + associated, df_aei_scores)
summary(lm_2by2_2)
```

```
## 
## Call:
## lm(formula = agscores ~ regtech + associated, data = df_aei_scores)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -32.628  -7.786   0.549   8.946  31.495 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.12696    2.17050   3.744 0.000221 ***
## regtech      0.13044    0.05329   2.448 0.015007 *  
## associated   0.30108    0.03898   7.723 2.22e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.85 on 270 degrees of freedom
## Multiple R-squared:  0.2419,	Adjusted R-squared:  0.2363 
## F-statistic: 43.09 on 2 and 270 DF,  p-value: < 2.2e-16
```

ANOVA 

```r
anova(lm_sep)
```

```
## Analysis of Variance Table
## 
## Response: agscores
##             Df  Sum Sq Mean Sq  F value    Pr(>F)    
## lowincome    1  1823.8  1823.8  18.1970 2.765e-05 ***
## credit       1 21052.1 21052.1 210.0475 < 2.2e-16 ***
## regtech      1   166.7   166.7   1.6634    0.1983    
## associated   1    84.4    84.4   0.8419    0.3597    
## Residuals  268 26860.4   100.2                       
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

PLOT REGRESSION

```r
plot(lm_sep)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-3-1.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-3-2.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-3-3.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-3-4.png) 

#### REGRESSIONS WITH INDIVIDUAL DEPENDENT VARIABLES

```r
#COVER CROP 
lm_covercrop <- lm(covercrop ~ lowincome + credit + regtech + associated, df_aei)
summary(lm_covercrop)
```

```
## 
## Call:
## lm(formula = covercrop ~ lowincome + credit + regtech + associated, 
##     data = df_aei)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.916 -10.611  -4.086   7.812  54.708 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 16.11100    5.45742   2.952  0.00344 **
## lowincome   -0.19195    0.11416  -1.681  0.09386 . 
## credit       0.16327    0.07303   2.236  0.02619 * 
## regtech      0.22049    0.08112   2.718  0.00699 **
## associated  -0.10970    0.06578  -1.668  0.09653 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.41 on 268 degrees of freedom
## Multiple R-squared:  0.09384,	Adjusted R-squared:  0.08032 
## F-statistic: 6.939 on 4 and 268 DF,  p-value: 2.5e-05
```

```r
plot(lm_covercrop)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-1.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-2.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-3.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-4.png) 

```r
#CROP ROTATION 
lm_croprot <- lm(croprot ~ lowincome + credit + regtech + associated, df_aei)
summary(lm_croprot)
```

```
## 
## Call:
## lm(formula = croprot ~ lowincome + credit + regtech + associated, 
##     data = df_aei)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.874  -7.688  -0.005   8.332  32.689 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -13.60719    4.38672  -3.102  0.00213 ** 
## lowincome     0.29268    0.09176   3.189  0.00159 ** 
## credit        0.58702    0.05870  10.000  < 2e-16 ***
## regtech       0.00552    0.06520   0.085  0.93260    
## associated    0.08715    0.05287   1.648  0.10047    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.39 on 268 degrees of freedom
## Multiple R-squared:  0.5336,	Adjusted R-squared:  0.5266 
## F-statistic: 76.65 on 4 and 268 DF,  p-value: < 2.2e-16
```

```r
plot(lm_croprot)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-5.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-6.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-7.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-8.png) 

```r
#MANURE
lm_manure <- lm(manure ~ lowincome + credit + regtech + associated, df_aei)
summary(lm_manure)
```

```
## 
## Call:
## lm(formula = manure ~ lowincome + credit + regtech + associated, 
##     data = df_aei)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -48.419 -18.137  -3.022  20.975  61.058 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 51.57144    9.01643   5.720 2.85e-08 ***
## lowincome   -0.37334    0.18861  -1.979   0.0488 *  
## credit       0.08569    0.12065   0.710   0.4782    
## regtech      0.06058    0.13402   0.452   0.6516    
## associated   0.04284    0.10867   0.394   0.6938    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.46 on 268 degrees of freedom
## Multiple R-squared:  0.03086,	Adjusted R-squared:  0.0164 
## F-statistic: 2.133 on 4 and 268 DF,  p-value: 0.07697
```

```r
plot(lm_manure)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-9.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-10.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-11.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-12.png) 

```r
#APM
lm_apm <- lm(apm ~ lowincome + credit + regtech + associated, df_aei)
summary(lm_apm)
```

```
## 
## Call:
## lm(formula = apm ~ lowincome + credit + regtech + associated, 
##     data = df_aei)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.300 -10.973  -4.134   7.851  70.596 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -5.36690    5.69315  -0.943   0.3467  
## lowincome    0.21408    0.11909   1.798   0.0734 .
## credit       0.13575    0.07618   1.782   0.0759 .
## regtech      0.19116    0.08462   2.259   0.0247 *
## associated   0.10484    0.06862   1.528   0.1277  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.08 on 268 degrees of freedom
## Multiple R-squared:  0.1246,	Adjusted R-squared:  0.1115 
## F-statistic: 9.533 on 4 and 268 DF,  p-value: 3.207e-07
```

```r
plot(lm_apm)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-13.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-14.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-15.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-16.png) 

```r
#DIVERSE
lm_diverse <- lm(diverse ~ lowincome + credit + regtech + associated, df_aei)
summary(lm_diverse)
```

```
## 
## Call:
## lm(formula = diverse ~ lowincome + credit + regtech + associated, 
##     data = df_aei)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.731  -6.882  -1.934   6.636  33.752 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -13.30476    3.73066  -3.566 0.000429 ***
## lowincome     0.70674    0.07804   9.056  < 2e-16 ***
## credit        0.33097    0.04992   6.630 1.84e-10 ***
## regtech      -0.03011    0.05545  -0.543 0.587529    
## associated    0.10193    0.04496   2.267 0.024190 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.53 on 268 degrees of freedom
## Multiple R-squared:  0.5223,	Adjusted R-squared:  0.5151 
## F-statistic: 73.25 on 4 and 268 DF,  p-value: < 2.2e-16
```

```r
plot(lm_diverse)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-17.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-18.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-19.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-20.png) 

```r
#STRAW
lm_straw <- lm(straw ~ lowincome + credit + regtech + associated, df_aei)
summary(lm_straw)
```

```
## 
## Call:
## lm(formula = straw ~ lowincome + credit + regtech + associated, 
##     data = df_aei)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -48.36 -11.93   0.60  10.20  53.41 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -22.944185   6.160878  -3.724 0.000239 ***
## lowincome     0.382806   0.128879   2.970 0.003245 ** 
## credit        1.192727   0.082441  14.468  < 2e-16 ***
## regtech      -0.043589   0.091572  -0.476 0.634458    
## associated    0.008183   0.074255   0.110 0.912337    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.4 on 268 degrees of freedom
## Multiple R-squared:  0.6509,	Adjusted R-squared:  0.6457 
## F-statistic: 124.9 on 4 and 268 DF,  p-value: < 2.2e-16
```

```r
plot(lm_straw)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-21.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-22.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-23.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-4-24.png) 
### PART 4: REGRESSION CHECKS 
***

####RESIDUALS

```r
 st_res <- rstandard(lm_sep) #pulls out the standard residuals from the regression

#Residuals for all independent variables 
stres_a <- ggplot(df_aei_scores, aes(associated, st_res)) +
  geom_point() +
  stat_smooth()

stres_c <- ggplot(df_aei_scores, aes(credit, st_res)) +
  geom_point() +
  stat_smooth()

stres_i <- ggplot(df_aei_scores, aes(lowincome, st_res)) +
  geom_point() +
  stat_smooth()

stres_t <- ggplot(df_aei_scores, aes(regtech, st_res)) +
  geom_point() +
  stat_smooth()

grid.arrange(stres_a, stres_c, stres_i, stres_t, ncol=2)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/residuals-1.png) 

#### RESIDUALS & LEVERAGE

```r
summary(lm_sep)
```

```
## 
## Call:
## lm(formula = agscores ~ lowincome + credit + regtech + associated, 
##     data = df_aei_scores)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -27.5673  -6.7611   0.6236   7.0751  27.9267 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.07657    3.54527   0.586   0.5586    
## lowincome    0.17184    0.07416   2.317   0.0213 *  
## credit       0.41591    0.04744   8.767   <2e-16 ***
## regtech      0.06734    0.05270   1.278   0.2024    
## associated   0.03921    0.04273   0.918   0.3597    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.01 on 268 degrees of freedom
## Multiple R-squared:  0.4627,	Adjusted R-squared:  0.4546 
## F-statistic: 57.69 on 4 and 268 DF,  p-value: < 2.2e-16
```

```r
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0)) # not quite sure what this does but got it from ats.ucla website shown below 
plot(lm_sep, las = 1) # highlights the observations that may be problematic 
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/diagnostics on full-1.png) 

```r
par(opar)
```
###LIST OF PROBLEM CASES 

```r
# Cases identified April 7th with data set excluding small farms and NAs and an ag scale of cover crop, crop rot, apm, manure, straw and diverse
# enter observation numbers and display the 
df_aei_scores[c(10,29,58,105,273), 1:2]
```

```
##        code        municipality
## 10  4200754     Alto Bela Vista
## 29  4202131 Bela Vista do Toldo
## 58  4204178         Cerro Negro
## 105 4207403              Imbuia
## 273 4219853              Zortea
```
###Cook's D and Studentized residuals

```r
cd <- cooks.distance(lm_sep) # run cook's D looking for leverage and residual
sres <- rstandard(lm_sep) #  calculate standardized residuals
df_diag_sep <- cbind(df_aei_scores, cd, sres) # add cooks D and student residuals to the dataframe

#COOK'S D
cooks_d <- df_diag_sep[cd > 4/(268), ] #pull out on the cook's D's that are beyond the hypothetical cutoff. Degrees of freedom for lm_sep is 268 (from the summary output)

#RESIDUALS
rabs <- abs(sres) # absolute value of the residuals
df_diag_sep <- cbind(df_diag_sep, rabs) # bind these residuals with the previous df
df_diag_sep <- df_diag_sep[order(-rabs), ] # reorder according to absolute value residuals
big_residuals <- df_diag_sep[1:10,] # this shows the largest residuals 
```

####RESULTS 

```r
knitr::kable(cooks_d[,c(1, 2, 17)]) 
```

          code  municipality                  cd
----  --------  --------------------  ----------
29     4202131  Bela Vista do Toldo    0.0231351
58     4204178  Cerro Negro            0.0311484
105    4207403  Imbuia                 0.0387800
120    4208500  Ituporanga             0.0236372
138    4209854  Lindoia do Sul         0.0182927
145    4210308  Major Vieira           0.0254087
151    4210803  Meleiro                0.0162622
160    4211405  Nova Erechim           0.0218706
173    4212205  Papanduva              0.0204775
246    4217956  Tigrinhos              0.0221299
248    4218103  Timbe do Sul           0.0151389
258    4218806  Turvo                  0.0203975
273    4219853  Zortea                 0.0284132

```r
knitr::kable(big_residuals[,c(1, 2, 19)])
```

          code  municipality               rabs
----  --------  --------------------  ---------
10     4200754  Alto Bela Vista        2.800655
105    4207403  Imbuia                 2.787769
29     4202131  Bela Vista do Toldo    2.702221
246    4217956  Tigrinhos              2.263490
258    4218806  Turvo                  2.203054
77     4205191  Ermo                   2.192999
21     4201604  Arroio Trinta          2.149234
190    4213807  Praia Grande           2.108258
145    4210308  Major Vieira           2.078959
116    4208005  Ita                    2.064629

####LIST OF PROBLEM CASES 
Municipalities that showed up on more than 1 of the above tables or from the earlier plots 

```r
prob_cases <- df_aei_scores[c(10, 21,29,58,77,145,246,258,273), 1:2]
knitr::kable(prob_cases) # nice table of problem cases 
```

          code  municipality        
----  --------  --------------------
10     4200754  Alto Bela Vista     
21     4201604  Arroio Trinta       
29     4202131  Bela Vista do Toldo 
58     4204178  Cerro Negro         
77     4205191  Ermo                
145    4210308  Major Vieira        
246    4217956  Tigrinhos           
258    4218806  Turvo               
273    4219853  Zortea              

###PART 5: RESPECIFICATION 
***
Rerun the regressions as performed earlier but without the leverage and outliers. Remember that the ag scores are already defined from earlier, so do not need to add again. 

####REMOVE PROBLEM CASES

```r
prob_cases_codes <- prob_cases[,1] # pull out just the code names of the cases identified in the previous chunk 

rerun_df_aei_scores <- df_aei_scores %>% 
  filter(code %!in% prob_cases_codes) %>% # remove all cases identified in previous chunk
  droplevels() # drop factor levels 

str(rerun_df_aei_scores)
```

```
## 'data.frame':	264 obs. of  16 variables:
##  $ code        : int  4200051 4200101 4200200 4200309 4200408 4200507 4200556 4200606 4200705 4200804 ...
##  $ municipality: Factor w/ 264 levels "Abdon Batista",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ orgcomp     : num  7.26 5.45 3.74 3.96 25.59 ...
##  $ manure      : num  18.06 9.65 44.3 10.84 54.95 ...
##  $ covercrop   : num  8.82 3.43 33.49 60.75 3.59 ...
##  $ croprot     : num  18.3 24.5 24.8 11.6 33 ...
##  $ apm         : num  1.26 0.43 55.99 3.97 40.74 ...
##  $ diverse     : num  42.2 47.2 37.6 17.5 21.9 ...
##  $ straw       : num  59.8 75.4 44.9 33.4 58.9 ...
##  $ famlab      : num  85.7 95.9 85.9 81 78.1 ...
##  $ lowincome   : num  51.9 60.8 38.5 24.8 26.4 ...
##  $ associated  : num  46.5 63.4 38.9 49.6 56.1 ...
##  $ regtech     : num  9.62 17.2 22.26 46.74 21.34 ...
##  $ credit      : num  60.9 34 30.6 40.1 47.8 ...
##  $ offfarminc  : num  56.5 12.7 26 15.1 23.6 ...
##  $ agscores    : num  24.7 26.8 40.2 23 35.5 ...
```

#####REGRESSION

```r
lm_sep_rr <- lm(agscores ~ lowincome + credit + regtech + associated, rerun_df_aei_scores)
summary(lm_sep_rr)
```

```
## 
## Call:
## lm(formula = agscores ~ lowincome + credit + regtech + associated, 
##     data = rerun_df_aei_scores)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -28.3098  -6.6587   0.4957   6.6911  20.0372 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.38607    3.41794  -0.113  0.91015    
## lowincome    0.20970    0.07125   2.943  0.00354 ** 
## credit       0.39818    0.04452   8.943  < 2e-16 ***
## regtech      0.13220    0.05004   2.642  0.00874 ** 
## associated   0.04415    0.04051   1.090  0.27682    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.264 on 259 degrees of freedom
## Multiple R-squared:  0.5174,	Adjusted R-squared:  0.5099 
## F-statistic: 69.41 on 4 and 259 DF,  p-value: < 2.2e-16
```


```r
lm_2by2_1_rr <- lm(agscores ~ lowincome + credit, rerun_df_aei_scores)
summary(lm_2by2_1_rr)
```

```
## 
## Call:
## lm(formula = agscores ~ lowincome + credit, data = rerun_df_aei_scores)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -26.8292  -5.6959   0.3879   6.7093  21.9546 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.07015    2.49556   2.432   0.0157 *  
## lowincome    0.10853    0.06252   1.736   0.0838 .  
## credit       0.47363    0.03030  15.629   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.373 on 261 degrees of freedom
## Multiple R-squared:  0.5021,	Adjusted R-squared:  0.4983 
## F-statistic: 131.6 on 2 and 261 DF,  p-value: < 2.2e-16
```

```r
lm_2by2_2_rr <- lm(agscores ~ regtech + associated, rerun_df_aei_scores)
summary(lm_2by2_2_rr)
```

```
## 
## Call:
## lm(formula = agscores ~ regtech + associated, data = rerun_df_aei_scores)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.642  -7.431   0.467   8.214  25.168 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.11780    2.08438   3.415  0.00074 ***
## regtech      0.17787    0.05179   3.435  0.00069 ***
## associated   0.30039    0.03763   7.983 4.55e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.24 on 261 degrees of freedom
## Multiple R-squared:  0.2845,	Adjusted R-squared:  0.2791 
## F-statistic:  51.9 on 2 and 261 DF,  p-value: < 2.2e-16
```

ANOVA 

```r
anova(lm_sep_rr)
```

```
## Analysis of Variance Table
## 
## Response: agscores
##             Df  Sum Sq Mean Sq  F value    Pr(>F)    
## lowincome    1  1659.9  1659.9  19.3437 1.596e-05 ***
## credit       1 21460.0 21460.0 250.0788 < 2.2e-16 ***
## regtech      1   602.1   602.1   7.0160  0.008575 ** 
## associated   1   101.9   101.9   1.1876  0.276821    
## Residuals  259 22225.6    85.8                       
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

PLOT REGRESSION

```r
plot(lm_sep_rr)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-9-1.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-9-2.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-9-3.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-9-4.png) 

#### REGRESSIONS WITH INDIVIDUAL DEPENDENT VARIABLES

```r
#COVER CROP 
lm_covercrop_rr <- lm(covercrop ~ lowincome + credit + regtech + associated, rerun_df_aei_scores)
summary(lm_covercrop_rr)
```

```
## 
## Call:
## lm(formula = covercrop ~ lowincome + credit + regtech + associated, 
##     data = rerun_df_aei_scores)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.361 -10.201  -3.786   8.051  50.486 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 18.60788    5.50476   3.380 0.000836 ***
## lowincome   -0.25055    0.11475  -2.183 0.029908 *  
## credit       0.17015    0.07171   2.373 0.018377 *  
## regtech      0.23844    0.08059   2.959 0.003373 ** 
## associated  -0.12944    0.06525  -1.984 0.048329 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.92 on 259 degrees of freedom
## Multiple R-squared:  0.1175,	Adjusted R-squared:  0.1039 
## F-statistic: 8.623 on 4 and 259 DF,  p-value: 1.51e-06
```

```r
plot(lm_covercrop_rr)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-1.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-2.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-3.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-4.png) 

```r
#CROP ROTATION 
lm_croprot_rr <- lm(croprot ~ lowincome + credit + regtech + associated, rerun_df_aei_scores)
summary(lm_croprot_rr)
```

```
## 
## Call:
## lm(formula = croprot ~ lowincome + credit + regtech + associated, 
##     data = rerun_df_aei_scores)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.295  -6.939   0.264   7.855  32.956 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -15.47119    4.46213  -3.467 0.000616 ***
## lowincome     0.31676    0.09302   3.405 0.000766 ***
## credit        0.57701    0.05812   9.927  < 2e-16 ***
## regtech       0.06101    0.06532   0.934 0.351149    
## associated    0.09130    0.05289   1.726 0.085495 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.09 on 259 degrees of freedom
## Multiple R-squared:  0.557,	Adjusted R-squared:  0.5502 
## F-statistic: 81.43 on 4 and 259 DF,  p-value: < 2.2e-16
```

```r
plot(lm_croprot_rr)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-5.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-6.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-7.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-8.png) 

```r
#MANURE
lm_manure_rr <- lm(manure ~ lowincome + credit + regtech + associated, rerun_df_aei_scores)
summary(lm_manure_rr)
```

```
## 
## Call:
## lm(formula = manure ~ lowincome + credit + regtech + associated, 
##     data = rerun_df_aei_scores)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -49.417 -17.368  -2.736  20.585  59.791 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  47.8835     9.1803   5.216 3.75e-07 ***
## lowincome    -0.3092     0.1914  -1.616    0.107    
## credit        0.0583     0.1196   0.487    0.626    
## regtech       0.1704     0.1344   1.268    0.206    
## associated    0.0411     0.1088   0.378    0.706    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.88 on 259 degrees of freedom
## Multiple R-squared:  0.03918,	Adjusted R-squared:  0.02434 
## F-statistic: 2.641 on 4 and 259 DF,  p-value: 0.03432
```

```r
plot(lm_manure_rr)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-9.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-10.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-11.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-12.png) 

```r
#APM
lm_apm_rr <- lm(apm ~ lowincome + credit + regtech + associated, rerun_df_aei_scores)
summary(lm_apm_rr)
```

```
## 
## Call:
## lm(formula = apm ~ lowincome + credit + regtech + associated, 
##     data = rerun_df_aei_scores)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.992 -10.509  -3.680   7.944  55.741 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -8.11022    5.71066  -1.420   0.1568   
## lowincome    0.26371    0.11905   2.215   0.0276 * 
## credit       0.11319    0.07439   1.522   0.1293   
## regtech      0.26513    0.08360   3.171   0.0017 **
## associated   0.10560    0.06769   1.560   0.1200   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.48 on 259 degrees of freedom
## Multiple R-squared:  0.1487,	Adjusted R-squared:  0.1355 
## F-statistic: 11.31 on 4 and 259 DF,  p-value: 1.795e-08
```

```r
plot(lm_apm_rr)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-13.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-14.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-15.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-16.png) 

```r
#DIVERSE
lm_diverse_rr <- lm(diverse ~ lowincome + credit + regtech + associated, rerun_df_aei_scores)
summary(lm_diverse_rr)
```

```
## 
## Call:
## lm(formula = diverse ~ lowincome + credit + regtech + associated, 
##     data = rerun_df_aei_scores)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -29.359  -6.543  -1.444   6.734  29.666 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -15.57580    3.73788  -4.167 4.21e-05 ***
## lowincome     0.73882    0.07792   9.482  < 2e-16 ***
## credit        0.31951    0.04869   6.562 2.87e-10 ***
## regtech       0.01836    0.05472   0.336   0.7375    
## associated    0.10738    0.04430   2.424   0.0161 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.13 on 259 degrees of freedom
## Multiple R-squared:  0.5465,	Adjusted R-squared:  0.5395 
## F-statistic: 78.04 on 4 and 259 DF,  p-value: < 2.2e-16
```

```r
plot(lm_diverse_rr)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-17.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-18.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-19.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-20.png) 

```r
#STRAW
lm_straw_rr <- lm(straw ~ lowincome + credit + regtech + associated, rerun_df_aei_scores)
summary(lm_straw_rr)
```

```
## 
## Call:
## lm(formula = straw ~ lowincome + credit + regtech + associated, 
##     data = rerun_df_aei_scores)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -49.328 -10.356   0.558  10.341  52.766 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -29.65059    6.10995  -4.853  2.1e-06 ***
## lowincome     0.49865    0.12737   3.915 0.000116 ***
## credit        1.15093    0.07959  14.461  < 2e-16 ***
## regtech       0.03983    0.08944   0.445 0.656470    
## associated    0.04896    0.07242   0.676 0.499626    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.56 on 259 degrees of freedom
## Multiple R-squared:  0.6855,	Adjusted R-squared:  0.6807 
## F-statistic: 141.1 on 4 and 259 DF,  p-value: < 2.2e-16
```

```r
plot(lm_straw_rr)
```

![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-21.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-22.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-23.png) ![](Scale_Creation___Regression_04.06.16_files/figure-html/unnamed-chunk-10-24.png) 


TESTING REGRESSION TABLES

```r
stargazer(lm_sep, lm_2by2_1, lm_2by2_2, type = "text", title="Linear Model - FULL ", digits=1, out="Regression_Tables_Using_Score_04.07.16.txt")

stargazer(lm_covercrop, lm_croprot, lm_manure, lm_straw, lm_diverse, lm_apm, type = "text", title="Linear Model - FULL ", digits=1, out="Regression_Tables_Individ_Indep_04.07.16.txt")

stargazer(lm_sep_rr, lm_2by2_1_rr, lm_2by2_2_rr, type = "text", title="Linear Model - FULL ", digits=1, out="Regression_Tables_Using_Score_RR_04.07.16.txt")

stargazer(lm_covercrop, lm_croprot_rr, lm_manure_rr, lm_straw_rr, lm_diverse_rr, lm_apm_rr, type = "text", title="Linear Model - FULL ", digits=1, out="Regression_Tables_Individ_Indep_04.07.16.txt")
```


Similar results as before: credit still the biggest predictor. 

MAPS! 
link to blog using brazilian municipalities shapefile 
https://dioferrari.wordpress.com/2014/11/27/plotting-maps-using-r-example-with-brazilian-municipal-level-data/



