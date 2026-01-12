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

## Part 1 basic analysis

these 4 variables seem in a logic chain,
whezev(past)–whez12(now)–awake12(severity)–exwhez12(if exercise causes)

### for whezev, whez12 and exwhez12 1=Yes, 2=No

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

### between the past and the present(whezev and whez12)

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

27.68% overall proportion of people have a history of wheezing or
whistling and have relapsed within 12 months

### severity in past 12 month (whez12 and awake12)

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

about 14% have severe symptoms

### under severe sympton (awake12) analyse past and now (whezev and whez12)

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
relapsed within 12 months

### current wheeze and exerciese-induced (whez12 and exwhez12)

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

## Part 2 relationship exploration

### if exercise-induced has relationship with symptom severity

chi square

``` r
analysis1_6_7 |>
  filter(whez12 == 1) |>
  group_by(awake12) |>
  summarise(
    prop = mean(exwhez12 == 1, na.rm = TRUE),
    n = n()
  )
```

    ## # A tibble: 4 × 3
    ##   awake12                            prop     n
    ##   <dbl+lbl>                         <dbl> <int>
    ## 1  1 [Never woken with wheezing]    0.301 16307
    ## 2  2 [Less than one night per week] 0.510 14840
    ## 3  3 [One or more nights per week]  0.636  5389
    ## 4 NA                                0.362  1827

``` r
table= 
  xtabs(
  ~ awake12 + exwhez12,
  data = analysis1_6_7 |> filter(whez12 == 1)
)

table
```

    ##        exwhez12
    ## awake12     1     2
    ##       1  4807 11161
    ##       2  7326  7037
    ##       3  3301  1892

``` r
chisq.test(table)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table
    ## X-squared = 2346.6, df = 2, p-value < 2.2e-16

There was a statistically significant association between wheeze
severity and exercise-induced wheeze.

``` r
model= glm(
  exwhez12 == 1 ~ awake12,
  data = analysis1_6_7 |> filter(whez12 == 1),
  family = binomial
)

summary(model)
```

    ## 
    ## Call:
    ## glm(formula = exwhez12 == 1 ~ awake12, family = binomial, data = filter(analysis1_6_7, 
    ##     whez12 == 1))
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.53888    0.02956  -52.05   <2e-16 ***
    ## awake12      0.74437    0.01592   46.76   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 48635  on 35523  degrees of freedom
    ## Residual deviance: 46314  on 35522  degrees of freedom
    ##   (2839 observations deleted due to missingness)
    ## AIC: 46318
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
exp(cbind(OR = coef(model), confint(model)))
```

    ## Waiting for profiling to be done...

    ##                    OR     2.5 %    97.5 %
    ## (Intercept) 0.2146204 0.2025166 0.2274009
    ## awake12     2.1051192 2.0405560 2.1719556

use logistic regression get same conclusion

### exercise-induced as a outcome,find the relationship with past wheeze, now wheeze and symptom severity

``` r
df_new=
  analysis1_6_7 |>
  mutate(
    whezev_bin = case_when(
      whezev == 1 ~ 1,
      whezev == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    whez12_bin = case_when(
      whez12 == 1 ~ 1,
      whez12 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
model_all=
  glm(
  exwhez12==1 ~ whezev_bin + whez12_bin + awake12,
  data =df_new,
  family = binomial
)

summary(model_all)
```

    ## 
    ## Call:
    ## glm(formula = exwhez12 == 1 ~ whezev_bin + whez12_bin + awake12, 
    ##     family = binomial, data = df_new)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.99461    0.04450 -112.24   <2e-16 ***
    ## whezev_bin   1.09682    0.05827   18.82   <2e-16 ***
    ## whez12_bin   2.30802    0.04725   48.84   <2e-16 ***
    ## awake12      0.78043    0.01577   49.48   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 82275  on 82462  degrees of freedom
    ## Residual deviance: 55275  on 82459  degrees of freedom
    ##   (259669 observations deleted due to missingness)
    ## AIC: 55283
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
exp(cbind(OR = coef(model_all), confint(model_all)))
```

    ## Waiting for profiling to be done...

    ##                       OR       2.5 %       97.5 %
    ## (Intercept)  0.006774346 0.006202842  0.007385433
    ## whezev_bin   2.994620028 2.671451568  3.357260516
    ## whez12_bin  10.054514689 9.173250513 11.040221099
    ## awake12      2.182412499 2.116066104  2.251044178
