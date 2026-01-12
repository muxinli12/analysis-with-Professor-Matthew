Whezev, whez12, exwhez12,awake12
================
Muxin Li
2026-01-11

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(haven)
```

## replace 9 to NA

``` r
isaac1_6_7=read_sav("spss/spss19/isaac1_6_7_core_ques_data.sav")
analysis1_6_7=
  isaac1_6_7|>
  mutate(across(c(whezev, whez12, awake12, exwhez12),
                ~ na_if(., 9)))
```

## Part 1 analysis between these 4 variables

these 4 variables seem in a logic chain,
whezev(past)–whez12(now)–awake12(severity)–exwhez12(if exercise causes)
\### for whezev, whez12 and exwhez12 1=Yes, 2=No

``` r
prop.table(table(analysis1_6_7$whezev))
```

    ## 
    ##         1         2 
    ## 0.2302226 0.7697774

``` r
prop.table(table(analysis1_6_7$whez12))
```

    ## 
    ##         1         2 
    ## 0.2785114 0.7214886

``` r
prop.table(table(analysis1_6_7$exwhez12))
```

    ## 
    ##          1          2 
    ## 0.06353642 0.93646358

### for awake12 1=no symptoms, 2=mild symptoms, 3=severe symptoms

``` r
prop.table(table(analysis1_6_7$awake12))
```

    ## 
    ##          1          2          3 
    ## 0.74878712 0.18367394 0.06753893

### relationship between the past and the present(whezev and whez12)

``` r
table(analysis1_6_7$whezev, analysis1_6_7$whez12)
```

    ##    
    ##         1     2
    ##   1 37701 36434
    ##   2   434 61654

``` r
prop.table(table(analysis1_6_7$whezev, analysis1_6_7$whez12))
```

    ##    
    ##               1           2
    ##   1 0.276759431 0.267458506
    ##   2 0.003185952 0.452596111

27.68% people have a history of wheezing or whistling and have relapsed
within 12 months \### severity in past 12 month (whez12 and awake12)

``` r
analysis1_6_7|>
  filter(whez12 == 1) |>
  count(awake12)|>
  mutate(prop = n / sum(n))
```

    ## # A tibble: 4 × 3
    ##   awake12                               n   prop
    ##   <dbl+lbl>                         <int>  <dbl>
    ## 1  1 [Never woken with wheezing]    16307 0.425 
    ## 2  2 [Less than one night per week] 14840 0.387 
    ## 3  3 [One or more nights per week]   5389 0.140 
    ## 4 NA                                 1827 0.0476

about 14% have severe symptoms \### under severe sympton (awake12)
analyse past and now (whezev and whez12)

``` r
severe=
  analysis1_6_7|>
  filter(awake12==3)

table(severe$whezev, severe$whez12)
```

    ##    
    ##        1    2
    ##   1 5335  107
    ##   2   34  239

``` r
prop.table(table(severe$whezev, severe$whez12))
```

    ##    
    ##               1           2
    ##   1 0.933508311 0.018722660
    ##   2 0.005949256 0.041819773

93% of the severe cases have a history of wheezing or whistling and have
relapsed within 12 months \### if exerciese-induced (whez12 and
exwhez12)

``` r
table(analysis1_6_7$whez12, analysis1_6_7$exwhez12)
```

    ##    
    ##         1     2
    ##   1 15986 21062
    ##   2  2368 94221

``` r
prop.table(table(analysis1_6_7$whez12, analysis1_6_7$exwhez12))
```

    ##    
    ##              1          2
    ##   1 0.11962256 0.15760605
    ##   2 0.01771964 0.70505174

12.96% is exercise-induced wheeze \### if exercise-induced has
relationship with wheeze history

``` r
exercise=
  analysis1_6_7|>
  filter(exwhez12==1)

table(exercise$whezev, exercise$whez12)
```

    ##    
    ##         1     2
    ##   1 15825  1470
    ##   2   102   872

``` r
prop.table(table(exercise$whezev, exercise$whez12))
```

    ##    
    ##               1           2
    ##   1 0.866221468 0.080464174
    ##   2 0.005583228 0.047731129

86.62% exercise-induced have a history of wheezing or whistling and have
relapsed within 12 months
