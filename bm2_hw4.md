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
