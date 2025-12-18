Dec 16
================
Muxin Li
2025-12-16

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
isaac=read_csv("tab/csv/isaac1_6_7_centre_info.csv")|>
  janitor::clean_names()
```

    ## Rows: 116 Columns: 55
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (39): ISAAC_ID, Country_name, Centre_name, PI_name, Age_group, Included,...
    ## dbl (16): Country_code, Centre_code, Records, Q6, Q25, Q27, Q28, Q29, Q30, Q...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(isaac)
```

    ## Rows: 116
    ## Columns: 55
    ## $ isaac_id     <chr> "038001", "074001", "074002", "002004", "002002", "002003…
    ## $ country_code <dbl> 38, 74, 74, 2, 2, 2, 2, 14, 14, 14, 14, 173, 17, 72, 72, …
    ## $ country_name <chr> "Albania", "Argentina", "Argentina", "Australia", "Austra…
    ## $ centre_code  <dbl> 1, 1, 2, 4, 2, 3, 1, 3, 2, 1, 4, 1, 1, 6, 2, 3, 4, 1, 7, …
    ## $ centre_name  <chr> "Tiran\xeb", "Buenos Aires", "Rosario", "Adelaide", "Melb…
    ## $ pi_name      <chr> "Professor Alfred Priftanji", "Dr Natalio Salmun", "Dr Na…
    ## $ age_group    <chr> "6-7 years", "6-7 years", "6-7 years", "6-7 years", "6-7 …
    ## $ records      <dbl> 2992, 3005, 3007, 3071, 2843, 2269, 2807, 5374, 33999, 36…
    ## $ included     <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "N…
    ## $ q1           <chr> "Geographic area only", "Geographic area and specific sch…
    ## $ q2           <chr> "All schools in the city of Tirana.", "All private and pu…
    ## $ q3           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "…
    ## $ q4           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "No", "No…
    ## $ q5           <chr> "The children in these schools were mentally disabled, bl…
    ## $ q6           <dbl> 0, 16, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, NA, 3, 0, 19, …
    ## $ q7           <chr> NA, "Teaching strike. It was impossible to compliment the…
    ## $ q8           <chr> "No", "No", "No", "Yes", "No", "No", "No", "Yes", "Yes", …
    ## $ q9           <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "N…
    ## $ q10          <chr> "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "No", "No",…
    ## $ q11          <chr> NA, NA, NA, NA, NA, NA, NA, "Complete sample.", "Complete…
    ## $ q12          <chr> "Selection by grade/level/year", "Selection by grade/leve…
    ## $ q13          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ q14          <chr> "All children", "All children", "All children", "All chil…
    ## $ q15          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ q16          <chr> "One", "One", "One", "Two", "One", "Two", "Two", "Other",…
    ## $ q17          <chr> NA, NA, NA, NA, NA, NA, NA, "Pre-school classes, and 1st …
    ## $ q18          <chr> "Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No", "No",…
    ## $ q19          <chr> NA, NA, NA, NA, "A single entry system was used for data …
    ## $ q20          <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "No", "No…
    ## $ q21          <chr> "At the request of the International Data Centre.", "Chan…
    ## $ q22          <chr> "No", "No", "No", "Yes", "No", "No", "No", "No", "No", "N…
    ## $ q23          <chr> NA, NA, NA, "Where available, information on asthma (qu 6…
    ## $ q24          <chr> NA, NA, NA, "Yes", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ q25          <dbl> NA, NA, NA, 10, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ q26          <chr> NA, NA, NA, "Yes", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ q27          <dbl> 45, 5639, 5163, 43, 503, 446, 176, 142, 578, 37, 31, 99, …
    ## $ q28          <dbl> 5390, 248000, 258000, 3587, 21376, 35843, 13963, 5496, 35…
    ## $ q29          <dbl> 0, 16, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, NA, 3, 0, 19, …
    ## $ q30          <dbl> 0, 704, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 100, 0, 2…
    ## $ q31          <dbl> 0, 2, 1, 2, 7, 1, 3, 2, 4, 0, 0, 0, 5, 0, NA, 0, 0, 14, 1…
    ## $ q32          <dbl> 0, 90, 50, 191, 233, 70, 319, 95, 100, 0, 0, 0, 226, 0, N…
    ## $ q33          <dbl> 30, 76, 61, 41, 84, 34, 42, 140, 574, 37, 31, 99, 136, 32…
    ## $ q34          <dbl> 3294, 3344, 3367, 3396, 3157, 3000, 3416, 5401, 35387, 41…
    ## $ q35          <dbl> 2981, 3005, 3007, 3063, 2840, 2192, 2804, 5264, 33068, 36…
    ## $ q36          <dbl> 313, 339, 360, 333, 317, 808, 612, 137, 2319, 505, 101, 7…
    ## $ q37          <chr> "Yes", "Yes", "Yes", "No", "No", "No", "No", "Yes", "Yes"…
    ## $ q38          <dbl> 1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 0, 5, 1, NA, 1, 1, 1…
    ## $ q39          <chr> "Albanian.", "Spanish.", "Spanish.", NA, NA, NA, NA, "Ger…
    ## $ q40          <chr> "Yes", "Yes", "Yes", NA, NA, NA, NA, "Yes", "Yes", "Yes",…
    ## $ q41          <chr> "No", "Yes", "Yes", NA, NA, NA, NA, "Yes", "Yes", "Yes", …
    ## $ q42          <chr> "No", "No", "No", NA, NA, NA, NA, "Yes", "Yes", "Yes", "Y…
    ## $ q43          <chr> NA, NA, NA, NA, NA, NA, NA, "M\xfcnster", "M\xfcnster", "…
    ## $ q44          <chr> "Yes", "Yes", "Yes", NA, NA, NA, NA, "Yes", "Yes", "Yes",…
    ## $ q45          <chr> "Yes", "Yes", "Yes", NA, NA, NA, NA, "Yes", "Yes", "Yes",…
    ## $ q46          <chr> "Did not use video", "Did not use video", "Did not use vi…

## Q1-Q17 about sampling and study design (find the frequently occurring responses)

``` r
study_design_Q1_Q17=
  isaac|>
  select("isaac_id":"q17")

q_lookup_1_17 = tribble(
  ~question, ~question_text,
  "q1",  "Which sampling frame category is most appropriate for the study?",
  "q2",  "Description of the sampling frame.",
  "q3",  "Is a map of the centre available at the ISAAC International Data Centre?",
  "q4",  "Were any schools excluded from the sampling frame before selection?",
  "q5",  "Description of the reason for exclusion of these schools.",
  "q6",  "How many schools were rejected after selection?",
  "q7",  "Description of the reason for rejection of these schools.",
  "q8",  "Were all schools in the sampling frame used?",
  "q9",  "Were the schools selected using a random selection method?",
  "q10", "Was there stratification, followed by random selection of schools?",
  "q11", "Description of sampling method for schools if a random selection method was not used in Q10.",
  "q12", "Which approach was used for selection of children within the schools?",
  "q13", "Description of the approach to child selection within the schools.",
  "q14", "Which children were selected from the grade/level/year or age group?",
  "q15", "Description of which children were selected if \"Some children\" was selected in Q14.",
  "q16", "How many years of age or grade(s)/level(s)/year(s) were selected?",
  "q17", "Description of how many years of age or grade(s)/level(s)/year(s) were selected if \"Other\" in Q16."
)

most_common_q1_q17=
  study_design_Q1_Q17|>
  select("q1":"q17") |>
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response",
    values_transform = list(response = as.character)
  )|>
  filter(!is.na(response), response != "")|>  
  count(question, response, name = "n") |>
  group_by(question) |>
  mutate(qnum = as.integer(stringr::str_remove(question, "^q"))) |>
  arrange(qnum, desc(n), question, response)|>
  mutate(question = paste0("q", qnum)) |>
  select(-qnum)|>
  filter(n>10)|>
  mutate(prop=n/116)|>
  left_join(q_lookup_1_17, by = "question")

most_common_q1_q17
```

    ## # A tibble: 17 × 5
    ## # Groups:   question [10]
    ##    question response                                     n  prop question_text  
    ##    <chr>    <chr>                                    <int> <dbl> <chr>          
    ##  1 q1       Geographic area only                        89 0.767 Which sampling…
    ##  2 q1       Geographic area and specific school type    14 0.121 Which sampling…
    ##  3 q3       Yes                                        107 0.922 Is a map of th…
    ##  4 q4       No                                          74 0.638 Were any schoo…
    ##  5 q4       Yes                                         36 0.310 Were any schoo…
    ##  6 q6       0                                           95 0.819 How many schoo…
    ##  7 q8       No                                          81 0.698 Were all schoo…
    ##  8 q8       Yes                                         30 0.259 Were all schoo…
    ##  9 q9       Yes                                         77 0.664 Were the schoo…
    ## 10 q9       No                                          34 0.293 Were the schoo…
    ## 11 q10      No                                          86 0.741 Was there stra…
    ## 12 q10      Yes                                         24 0.207 Was there stra…
    ## 13 q12      Selection by grade/level/year               81 0.698 Which approach…
    ## 14 q12      Selection by age group                      28 0.241 Which approach…
    ## 15 q14      All children                               103 0.888 Which children…
    ## 16 q16      Two                                         78 0.672 How many years…
    ## 17 q16      One                                         28 0.241 How many years…

## Q18-Q25 about data entry (find the frequently occurring responses)

``` r
q_lookup_18_25 = tribble(
  ~question, ~question_text,
  "q18", "Was the data double entered?",
  "q19", "Description of data entry method if \"No\" to Q18.",
  "q20", "Were changes made to the demographic data after the questionnaire was completed?",
  "q21", "Description of changes to the demographic data if \"Yes\" to Q20.",
  "q22", "Were changes made to the symptoms questionnaire data?",
  "q23", "Description of changes to the symptoms questionnaire data if \"Yes\" to Q22.",
  "q24", "Can the symptoms questionnaire data be returned to its original form if \"Yes\" to Q22?",
  "q25", "What proportion of observations have been changed if \"Yes\" to Q22?"
)

most_common_q18_q25 =
  isaac |>
  select("q18":"q25") |>
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response",
    values_transform = list(response = as.character)
  ) |>
  filter(!is.na(response), response != "") |>
  count(question, response, name = "n") |>
  group_by(question) |>
  mutate(qnum = as.integer(stringr::str_remove(question, "^q"))) |>
  arrange(qnum, desc(n), response) |>
  mutate(question = paste0("q", qnum)) |>
  select(-qnum) |>
  filter(n > 10) |>
  mutate(prop = n / 116) |>
  left_join(q_lookup_18_25, by = "question")

most_common_q18_q25
```

    ## # A tibble: 5 × 5
    ## # Groups:   question [3]
    ##   question response     n  prop question_text                                   
    ##   <chr>    <chr>    <int> <dbl> <chr>                                           
    ## 1 q18      Yes         76 0.655 Was the data double entered?                    
    ## 2 q18      No          35 0.302 Was the data double entered?                    
    ## 3 q20      No          61 0.526 Were changes made to the demographic data after…
    ## 4 q20      Yes         50 0.431 Were changes made to the demographic data after…
    ## 5 q22      No         101 0.871 Were changes made to the symptoms questionnaire…

## Q27-Q36 about sample size and participation situation (analysis participation in child and school in different countries)

``` r
q27_q36=
  isaac |>
  mutate(
    child_participation = q35 / (q35 + q36),
    school_participation = q33 / q27
  )|>
  drop_na(child_participation,school_participation)

country_q27_q36=
  q27_q36|>
  select(country_name,child_participation,school_participation)|>
  group_by(country_name)|>
  summarise(mean_child_participation=mean(child_participation),
            mean_school_participation=mean(school_participation))

country_q27_q36
```

    ## # A tibble: 47 × 3
    ##    country_name mean_child_participation mean_school_participation
    ##    <chr>                           <dbl>                     <dbl>
    ##  1 Albania                         0.905                    0.667 
    ##  2 Argentina                       0.896                    0.0126
    ##  3 Australia                       0.838                    0.359 
    ##  4 Austria                         0.936                    0.995 
    ##  5 Barbados                        0.822                    1     
    ##  6 Belgium                         0.858                    0.965 
    ##  7 Brasil                          0.763                    0.335 
    ##  8 Canada                          0.756                    0.717 
    ##  9 Chile                           0.871                    0.829 
    ## 10 Costa Rica                      0.841                    0.0139
    ## # ℹ 37 more rows

## q37-q46 about questionnaire translation and cultural adaptation (find the frequently occurring responses)

``` r
q_lookup_37_46 = tribble(
  ~question, ~question_text,
  "q37", "Was the English language questionnaire translated to one or more languages?",
  "q38", "How many translations were developed and used in this centre?",
  "q39", "Which languages were used in this centre?",
  "q40", "Were the translators familiar with asthma and allergy terminology?",
  "q41", "Were the local community approached to help with difficult words and concepts?",
  "q42", "Were other centres in the country or region involved in preparation of the translation(s)?",
  "q43", "Which other centres were involved if \"Yes\" to Q42?",
  "q44", "Were the translated questionnaires translated back to English by an independent translator?",
  "q45", "Were the translated questionnaires pilot tested?",
  "q46", "Was the International or European version of the video questionnaire used?"
)

most_common_q37_q46 =
  isaac |>
  select("q37":"q46") |>
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response",
    values_transform = list(response = as.character)
  ) |>
  filter(!is.na(response), response != "") |>
  count(question, response, name = "n") |>
  group_by(question) |>
  mutate(qnum = as.integer(stringr::str_remove(question, "^q"))) |>
  arrange(qnum, desc(n), question, response) |>
  mutate(question = paste0("q", qnum)) |>
  select(-qnum) |>
  filter(n > 10) |>
  mutate(prop = n / 116) |>
  left_join(q_lookup_37_46, by = "question")

most_common_q37_q46
```

    ## # A tibble: 13 × 5
    ## # Groups:   question [9]
    ##    question response              n  prop question_text                         
    ##    <chr>    <chr>             <int> <dbl> <chr>                                 
    ##  1 q37      Yes                  95 0.819 Was the English language questionnair…
    ##  2 q37      No                   15 0.129 Was the English language questionnair…
    ##  3 q38      1                    83 0.716 How many translations were developed …
    ##  4 q39      Spanish.             16 0.138 Which languages were used in this cen…
    ##  5 q40      Yes                  91 0.784 Were the translators familiar with as…
    ##  6 q41      Yes                  61 0.526 Were the local community approached t…
    ##  7 q41      No                   32 0.276 Were the local community approached t…
    ##  8 q42      No                   51 0.440 Were other centres in the country or …
    ##  9 q42      Yes                  42 0.362 Were other centres in the country or …
    ## 10 q44      Yes                  94 0.810 Were the translated questionnaires tr…
    ## 11 q45      Yes                  66 0.569 Were the translated questionnaires pi…
    ## 12 q45      No                   27 0.233 Were the translated questionnaires pi…
    ## 13 q46      Did not use video   111 0.957 Was the International or European ver…
