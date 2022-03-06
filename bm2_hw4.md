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
