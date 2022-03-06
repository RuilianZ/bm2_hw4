bm2 hw4
================
Roxy Zhang
3/1/2022

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library (nnet) # multinom
library(MASS) # polr
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

## Question 1

``` r
data <- c(65, 34, 54, 47, 100, 100,
         130, 141, 76, 116, 111, 191,
         67, 130, 48, 105, 62, 104)

table <- array(
  data = data, 
  dim = c(2, 3, 3),
  dimnames = list(Contact = c("Low", "High"),
            Response = c("Low satisfaction", "Medium Satisfaction", "High Satisfaction"),
            Area = c("Tower block", "Apartment", "House")))

ftable(table)
```

    ##                             Area Tower block Apartment House
    ## Contact Response                                            
    ## Low     Low satisfaction                  65       130    67
    ##         Medium Satisfaction               54        76    48
    ##         High Satisfaction                100       111    62
    ## High    Low satisfaction                  34       141   130
    ##         Medium Satisfaction               47       116   105
    ##         High Satisfaction                100       191   104

``` r
# test on levels of satisfaction and contact with other residents
contact <- margin.table(table, margin =c(1, 2))

chisq.test(contact)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  contact
    ## X-squared = 5.1398, df = 2, p-value = 0.07654

``` r
# test on levels of satisfaction and type of housing
house <- margin.table(table, margin =c(2, 3))

chisq.test(house)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  house
    ## X-squared = 34.024, df = 4, p-value = 7.369e-07

## Question 2

``` r
data_full <- data.frame(low = as.vector(table[,1,]),
                       medium = as.vector(table[,2,]),
                       high = as.vector(table[,3,]),
                       contact_dummy = rep(c(1, 2), 3),
                       house_dummy = rep(c(2, 1, 3), each = 2))
```

``` r
# model 1 - without interaction
m1 <- multinom(cbind(data_full$low, data_full$medium, data_full$high)
~ factor(contact_dummy) + factor(house_dummy), data = data_full)
```

    ## # weights:  15 (8 variable)
    ## initial  value 1846.767257 
    ## iter  10 value 1802.866981
    ## final  value 1802.740161 
    ## converged

``` r
summary(m1)
```

    ## Call:
    ## multinom(formula = cbind(data_full$low, data_full$medium, data_full$high) ~ 
    ##     factor(contact_dummy) + factor(house_dummy), data = data_full)
    ## 
    ## Coefficients:
    ##   (Intercept) factor(contact_dummy)2 factor(house_dummy)2 factor(house_dummy)3
    ## 2 -0.51401706              0.2959796            0.4067570           0.06967794
    ## 3 -0.08082309              0.3282256            0.6415915          -0.30401939
    ## 
    ## Std. Errors:
    ##   (Intercept) factor(contact_dummy)2 factor(house_dummy)2 factor(house_dummy)3
    ## 2   0.1207955              0.1301045            0.1713008            0.1437749
    ## 3   0.1079357              0.1181870            0.1500773            0.1351693
    ## 
    ## Residual Deviance: 3605.48 
    ## AIC: 3621.48

``` r
exp(coef(m1))
```

    ##   (Intercept) factor(contact_dummy)2 factor(house_dummy)2 factor(house_dummy)3
    ## 2   0.5980882               1.344443             1.501939            1.0721628
    ## 3   0.9223569               1.388502             1.899502            0.7378466

``` r
exp(confint(m1))
```

    ## , , 2
    ## 
    ##                            2.5 %    97.5 %
    ## (Intercept)            0.4720021 0.7578557
    ## factor(contact_dummy)2 1.0418307 1.7349522
    ## factor(house_dummy)2   1.0735958 2.1011828
    ## factor(house_dummy)3   0.8088711 1.4211574
    ## 
    ## , , 3
    ## 
    ##                            2.5 %    97.5 %
    ## (Intercept)            0.7464899 1.1396566
    ## factor(contact_dummy)2 1.1014013 1.7504412
    ## factor(house_dummy)2   1.4154471 2.5490928
    ## factor(house_dummy)3   0.5661217 0.9616618

``` r
# model 2 - with interaction
m2 <- multinom(cbind(data_full$low, data_full$medium, data_full$high)
~ factor(contact_dummy) +  factor(house_dummy) + factor(contact_dummy)*factor(house_dummy), data = data_full)
```

    ## # weights:  21 (12 variable)
    ## initial  value 1846.767257 
    ## iter  10 value 1804.235447
    ## final  value 1799.293647 
    ## converged

``` r
summary(m2)
```

    ## Call:
    ## multinom(formula = cbind(data_full$low, data_full$medium, data_full$high) ~ 
    ##     factor(contact_dummy) + factor(house_dummy) + factor(contact_dummy) * 
    ##         factor(house_dummy), data = data_full)
    ## 
    ## Coefficients:
    ##   (Intercept) factor(contact_dummy)2 factor(house_dummy)2 factor(house_dummy)3
    ## 2  -0.5368430              0.3416693            0.3514013           0.20337126
    ## 3  -0.1579941              0.4615115            0.5887650           0.08043665
    ##   factor(contact_dummy)2:factor(house_dummy)2
    ## 2                                   0.1674961
    ## 3                                   0.1864948
    ##   factor(contact_dummy)2:factor(house_dummy)3
    ## 2                                  -0.2217822
    ## 3                                  -0.6071135
    ## 
    ## Std. Errors:
    ##   (Intercept) factor(contact_dummy)2 factor(house_dummy)2 factor(house_dummy)3
    ## 2   0.1443974              0.1912160            0.2339956            0.2379267
    ## 3   0.1292328              0.1703787            0.2051474            0.2185307
    ##   factor(contact_dummy)2:factor(house_dummy)2
    ## 2                                   0.3480731
    ## 3                                   0.3063070
    ##   factor(contact_dummy)2:factor(house_dummy)3
    ## 2                                   0.2992292
    ## 3                                   0.2781927
    ## 
    ## Residual Deviance: 3598.587 
    ## AIC: 3622.587

``` r
exp(coef(m2))
```

    ##   (Intercept) factor(contact_dummy)2 factor(house_dummy)2 factor(house_dummy)3
    ## 2   0.5845909               1.407295             1.421058             1.225527
    ## 3   0.8538548               1.586470             1.801762             1.083760
    ##   factor(contact_dummy)2:factor(house_dummy)2
    ## 2                                    1.182341
    ## 3                                    1.205018
    ##   factor(contact_dummy)2:factor(house_dummy)3
    ## 2                                   0.8010898
    ## 3                                   0.5449215

``` r
exp(confint(m2))
```

    ## , , 2
    ## 
    ##                                                 2.5 %    97.5 %
    ## (Intercept)                                 0.4404947 0.7758243
    ## factor(contact_dummy)2                      0.9674349 2.0471441
    ## factor(house_dummy)2                        0.8983265 2.2479628
    ## factor(house_dummy)3                        0.7687753 1.9536492
    ## factor(contact_dummy)2:factor(house_dummy)2 0.5976712 2.3389609
    ## factor(contact_dummy)2:factor(house_dummy)3 0.4456325 1.4400766
    ## 
    ## , , 3
    ## 
    ##                                                 2.5 %   97.5 %
    ## (Intercept)                                 0.6627975 1.099986
    ## factor(contact_dummy)2                      1.1360705 2.215433
    ## factor(house_dummy)2                        1.2052457 2.693514
    ## factor(house_dummy)3                        0.7061866 1.663209
    ## factor(contact_dummy)2:factor(house_dummy)2 0.6610961 2.196457
    ## factor(contact_dummy)2:factor(house_dummy)3 0.3158900 0.940009

``` r
# likelihood ratio test
TS1 = deviance(m1) - deviance(m2)
p1= 1 - pchisq(TS1, 4)


# goodness of fit
pihat <- predict(m1, type = "probs")
m <- rowSums(data_full[,1:3])
res.pearson <- (data_full[,1:3]-pihat*m)/sqrt(pihat*m)
G.stat <- sum(res.pearson^2)
p4 <- 1-pchisq(G.stat, (6-4)*(3-1))
```

## Question 3

``` r
freq <- c(data_full$low, data_full$medium, data_full$high)
res <- c(rep(c("L", "M", "H"), c(6, 6, 6)))
res <- factor(res, levels = c("L", "M", "H"), ordered = T)
data_ord <- data.frame(res = res, int = rep(data_full$contact_dummy, 3),
cate = rep(data_full$house_dummy, 3), freq = freq)

# proportional odds model
m5 <- polr(res ~ factor(int) + factor(cate), data_ord,
weights = freq, method = "logistic")

summary(m5)
```

    ## 
    ## Re-fitting to get Hessian

    ## Call:
    ## polr(formula = res ~ factor(int) + factor(cate), data = data_ord, 
    ##     weights = freq, method = "logistic")
    ## 
    ## Coefficients:
    ##                 Value Std. Error t value
    ## factor(int)2   0.2524    0.09306   2.713
    ## factor(cate)2  0.5010    0.11675   4.291
    ## factor(cate)3 -0.2353    0.10521  -2.236
    ## 
    ## Intercepts:
    ##     Value   Std. Error t value
    ## L|M -0.4964  0.0897    -5.5356
    ## M|H  0.6161  0.0901     6.8381
    ## 
    ## Residual Deviance: 3610.286 
    ## AIC: 3620.286

``` r
exp(coef(m5))
```

    ##  factor(int)2 factor(cate)2 factor(cate)3 
    ##     1.2871623     1.6502924     0.7903354

``` r
m6 <- polr(res ~ factor(cate), data_ord, weights = freq)
TS5 <- deviance(m6) - deviance(m5)
p5<- 1-pchisq(TS5, 1)

m7 <- polr(res ~ factor(int), data_ord, weights = freq)
TS6 <- deviance(m7) - deviance(m5)
p6<- 1-pchisq(TS6, 2)

# Pearson Chi-Square residuals from proportional odds model
pihat <- predict(m5, type = "probs")
m <- rowSums(data_full[,1:3])
res.pearson <- ((data_full[,1:3]-pihat*m)/sqrt(pihat*m)) %>%
  round(.,2) %>%
  as.data.frame()
```
