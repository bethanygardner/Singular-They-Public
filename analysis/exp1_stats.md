Improving memory for and production of singular <i>they</i> pronouns:
Experiment 1
================
Bethany Gardner
04/13/2023

- [Load Data](#load-data)
- [Memory](#memory)
  - [Descriptive Stats](#descriptive-stats)
  - [Model](#model)
- [Production](#production)
  - [Descriptive Stats](#descriptive-stats-1)
  - [Model](#model-1)
- [Memory Predicting Production](#memory-predicting-production)
  - [Descriptive Stats](#descriptive-stats-2)
  - [Model](#model-2)
- [Compare Memory and Production](#compare-memory-and-production)
- [Compare Pet Questions](#compare-pet-questions)

# Load Data

Read data, preprocessed from Qualtrics output. See
`data/exp1a_data_readme` for more details.

``` r
exp1a_d_all <- read.csv("data/exp1a_data.csv", stringsAsFactors = TRUE)
str(exp1a_d_all)
```

    ## 'data.frame':    5202 obs. of  17 variables:
    ##  $ SubjID     : Factor w/ 102 levels "R_0qPfWjp8o4W3Z61",..: 84 84 84 84 84 84 84 84 84 84 ...
    ##  $ SubjAge    : int  18 18 18 18 18 18 18 18 18 18 ...
    ##  $ SubjGender : Factor w/ 3 levels "female","male",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ SubjEnglish: Factor w/ 3 levels "Fully competent in speaking listening reading and writing but not native\n",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ List       : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Task       : Factor w/ 3 levels "introduction",..: 2 2 2 3 2 2 2 3 2 2 ...
    ##  $ Name       : Factor w/ 12 levels "Amanda","Andrew",..: 3 3 3 3 8 8 8 8 4 4 ...
    ##  $ Pronoun    : Factor w/ 3 levels "he/him","she/her",..: 1 1 1 1 1 1 1 1 3 3 ...
    ##  $ Pet        : Factor w/ 3 levels "cat","dog","fish": 1 1 1 1 2 2 2 2 3 3 ...
    ##  $ Job        : Factor w/ 12 levels "accountant","doctor",..: 12 12 12 12 4 4 4 4 9 9 ...
    ##  $ M_Type     : Factor w/ 4 levels "","job","pet",..: 2 3 4 1 2 3 4 1 2 3 ...
    ##  $ M_Response : Factor w/ 19 levels "","accountant",..: 19 3 9 1 8 5 18 1 14 7 ...
    ##  $ M_Acc      : int  0 1 1 NA 1 1 0 NA 1 1 ...
    ##  $ P_Response : Factor w/ 793 levels "","amanda fixed her computer",..: 1 1 1 169 1 1 1 235 1 1 ...
    ##  $ P_Pronoun  : Factor w/ 5 levels "","he/him","none",..: 1 1 1 2 1 1 1 2 1 1 ...
    ##  $ P_Acc      : int  NA NA NA 1 NA NA NA 1 NA NA ...
    ##  $ I_Response : Factor w/ 299 levels ""," I would ask Jessica about food spots ",..: 1 1 1 1 1 1 1 1 1 1 ...

Set up contrast coding. The first contrast compares they to he+she. The
second contrast compares he to she.

``` r
contrasts(exp1a_d_all$Pronoun) <- cbind(
  "they vs he+she" = c(.33, .33, -.66),
  "he vs she"      = c(-.5, .5, 0)
)
contrasts(exp1a_d_all$Pronoun)
```

    ##           they vs he+she he vs she
    ## he/him              0.33      -0.5
    ## she/her             0.33       0.5
    ## they/them          -0.66       0.0

Combine pronoun memory and production trials to make one row for each
character.

``` r
m_temp <- exp1a_d_all %>%
  filter(M_Type == "pronoun") %>%
  select(SubjID, Name, Pronoun, M_Response, M_Acc)
p_temp <- exp1a_d_all %>%
  filter(!is.na(P_Acc)) %>%
  select(SubjID, Name, Pronoun, P_Pronoun, P_Acc)

exp1a_d <- left_join(m_temp, p_temp, by = c("SubjID", "Name", "Pronoun"))
remove(m_temp, p_temp)

str(exp1a_d)
```

    ## 'data.frame':    1224 obs. of  7 variables:
    ##  $ SubjID    : Factor w/ 102 levels "R_0qPfWjp8o4W3Z61",..: 84 84 84 84 84 84 84 84 84 84 ...
    ##  $ Name      : Factor w/ 12 levels "Amanda","Andrew",..: 3 8 4 9 10 12 2 5 1 11 ...
    ##  $ Pronoun   : Factor w/ 3 levels "he/him","she/her",..: 1 1 3 3 2 2 1 2 2 1 ...
    ##   ..- attr(*, "contrasts")= num [1:3, 1:2] 0.33 0.33 -0.66 -0.5 0.5 0
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:3] "he/him" "she/her" "they/them"
    ##   .. .. ..$ : chr [1:2] "they vs he+she" "he vs she"
    ##  $ M_Response: Factor w/ 19 levels "","accountant",..: 9 18 18 18 16 16 9 16 16 9 ...
    ##  $ M_Acc     : int  1 0 1 1 1 1 1 1 1 1 ...
    ##  $ P_Pronoun : Factor w/ 5 levels "","he/him","none",..: 2 2 2 4 4 4 2 4 4 2 ...
    ##  $ P_Acc     : int  1 1 0 0 1 1 1 1 1 1 ...

# Memory

## Descriptive Stats

Mean accuracy for all three memory question types.

``` r
exp1a_d_all %>%
  filter(!is.na(M_Acc)) %>%
  group_by(M_Type) %>%
  summarise(
    mean = mean(M_Acc) %>% round(2),
    sd   = sd(M_Acc)   %>% round(2)
  )
```

    ## # A tibble: 3 × 3
    ##   M_Type   mean    sd
    ##   <fct>   <dbl> <dbl>
    ## 1 job      0.21  0.41
    ## 2 pet      0.41  0.49
    ## 3 pronoun  0.66  0.47

Mean accuracy, split by pronoun type.

``` r
exp1a_d %>%
  mutate(Pronoun_Group = ifelse(Pronoun == "they/them", "They", "He + She")) %>%
  group_by(Pronoun_Group) %>%
  summarise(
    mean = mean(M_Acc) %>% round(2),
    sd   = sd(M_Acc)   %>% round(2)
  )
```

    ## # A tibble: 2 × 3
    ##   Pronoun_Group  mean    sd
    ##   <chr>         <dbl> <dbl>
    ## 1 He + She       0.77  0.42
    ## 2 They           0.44  0.5

94% of participants selected they/them at least once.

``` r
exp1a_r_memory_usedThey <- exp1a_d %>%
  filter(M_Response == "they/them") %>%
  summarise(n = n_distinct(SubjID))

exp1a_r_memory_usedThey / (n_distinct(exp1a_d$SubjID))
```

    ##           n
    ## 1 0.9411765

## Model

Start with model that has random intercepts and slopes for participant
and item. Specifying the direction as “order” in buildmer will identify
the maximal model that will converge. However, it doesn’t continue to
backward stepwise elimination. This results in a model with random
intercepts and slopes by participant, and random intercepts by item.

``` r
exp1a_m_memory <- buildmer(
  formula = M_Acc ~ Pronoun + (1 + Pronoun | SubjID) + (1 + Pronoun | Name),
  data = exp1a_d, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: M_Acc ~ 1

    ## Currently evaluating LRT for: Pronoun

    ## Fitting via glm: M_Acc ~ 1 + Pronoun

    ## Updating formula: M_Acc ~ 1 + Pronoun

    ## Fitting via glm: M_Acc ~ 1 + Pronoun

    ## Currently evaluating LRT for: 1 | SubjID, 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 | SubjID)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 | Name)

    ## Updating formula: M_Acc ~ 1 + Pronoun + (1 | SubjID)

    ## Currently evaluating LRT for: Pronoun | SubjID, 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 + Pronoun |
    ##     SubjID)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 | SubjID) + (1 |
    ##     Name)

    ## Updating formula: M_Acc ~ 1 + Pronoun + (1 + Pronoun | SubjID)

    ## Currently evaluating LRT for: 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 + Pronoun |
    ##     SubjID) + (1 | Name)

    ## Updating formula: M_Acc ~ 1 + Pronoun + (1 + Pronoun | SubjID) + (1 |
    ##     Name)

    ## Currently evaluating LRT for: Pronoun | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 + Pronoun |
    ##     SubjID) + (1 + Pronoun | Name)

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)

``` r
summary(exp1a_m_memory)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ 1 + Pronoun + (1 + Pronoun | SubjID) + (1 | Name)
    ##    Data: exp1a_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1431.7   1482.8   -705.9   1411.7     1214 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4508 -0.6637  0.4549  0.5837  1.9896 
    ## 
    ## Random effects:
    ##  Groups Name                  Variance Std.Dev. Corr       
    ##  SubjID (Intercept)           0.442703 0.66536             
    ##         Pronounthey vs he+she 0.634468 0.79654  -0.42      
    ##         Pronounhe vs she      0.193199 0.43954   0.12  0.41
    ##  Name   (Intercept)           0.008379 0.09153             
    ## Number of obs: 1224, groups:  SubjID, 102; Name, 12
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error z value Pr(>|z|) Pr(>|t|)    
    ## (Intercept)             0.7683     0.1044  7.3592    0.000 1.85e-13 ***
    ## Pronounthey vs he+she   1.6382     0.1717  9.5436    0.000  < 2e-16 ***
    ## Pronounhe vs she        0.1292     0.2167  0.5962    0.551    0.551    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Prnvh+
    ## Prnnthyvsh+ 0.021        
    ## Prononhvssh 0.060  0.084

Convert to odds:

``` r
# intercept (mean)
exp1a_m_memory@model %>% get_intercept() %>% exp()
```

    ## [1] 2.156164

``` r
# they/them vs. he/him + she/her
exp1a_m_memory@model %>%
  get_parameters() %>%
  filter(Parameter == "Pronounthey vs he+she") %>%
  pull(Estimate) %>%
  exp()
```

    ## [1] 5.146116

- The intercept is significant (p\<.001), such that participants are
  2.16 times more likely to answer correctly than incorrectly across all
  pronoun types.

- The contrast between they/them and he/him + she/her is significant
  (p\<.001), such that participants are 5.15 times as likely to get
  he/him and she/her right than they/them.

- The contrast between he/him and she/her is not significant.

# Production

## Descriptive Stats

Mean accuracy, split by pronoun type.

``` r
exp1a_d %>%
  mutate(Pronoun_Group = ifelse(Pronoun == "they/them", "They", "He + She")) %>%
  group_by(Pronoun_Group) %>%
  summarise(
    mean = mean(P_Acc) %>% round(2),
    sd   = sd(P_Acc)   %>% round(2)
  )
```

    ## # A tibble: 2 × 3
    ##   Pronoun_Group  mean    sd
    ##   <chr>         <dbl> <dbl>
    ## 1 He + She       0.84  0.36
    ## 2 They           0.29  0.46

Responses that did not use a pronoun are infrequent and evenly
distributed across pronoun conditions.

``` r
table(exp1a_d$Pronoun, exp1a_d$P_Pronoun)
```

    ##            
    ##                 he/him none she/her they/them
    ##   he/him      0    339    8       1        60
    ##   she/her     0      2    7     350        49
    ##   they/them   0    139    9     140       120

``` r
(exp1a_d %>% filter(P_Pronoun == "none") %>% pull(P_Pronoun) %>% length()) /
(exp1a_d %>% pull(P_Pronoun) %>% length())
```

    ## [1] 0.01960784

``` r
exp1a_d %>%
  filter(P_Pronoun != "none") %>%
  group_by(Pronoun, P_Pronoun) %>%
  summarise(n = n()) %>%
  mutate(prop = (n / (n_distinct(exp1a_d$SubjID) * 4)) %>% round(2))
```

    ## # A tibble: 9 × 4
    ## # Groups:   Pronoun [3]
    ##   Pronoun   P_Pronoun     n  prop
    ##   <fct>     <fct>     <int> <dbl>
    ## 1 he/him    he/him      339  0.83
    ## 2 he/him    she/her       1  0   
    ## 3 he/him    they/them    60  0.15
    ## 4 she/her   he/him        2  0   
    ## 5 she/her   she/her     350  0.86
    ## 6 she/her   they/them    49  0.12
    ## 7 they/them he/him      139  0.34
    ## 8 they/them she/her     140  0.34
    ## 9 they/them they/them   120  0.29

60% of participants produced they/them at least once.

``` r
exp1a_r_prod_usedThey <- exp1a_d %>%
  filter(P_Pronoun == "they/them") %>%
  summarize(n = n_distinct(SubjID))

exp1a_r_prod_usedThey / (n_distinct(exp1a_d$SubjID))
```

    ##           n
    ## 1 0.5980392

## Model

Same model specifications as first model (memory accuracy). Here, the
maximal model has random intercepts and slopes by participant, and no
random effects by item.

``` r
exp1a_m_prod <- buildmer(
  formula = P_Acc ~ Pronoun + (1 + Pronoun | SubjID) + (1 + Pronoun | Name),
  data = exp1a_d, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: P_Acc ~ 1

    ## Currently evaluating LRT for: Pronoun

    ## Fitting via glm: P_Acc ~ 1 + Pronoun

    ## Updating formula: P_Acc ~ 1 + Pronoun

    ## Fitting via glm: P_Acc ~ 1 + Pronoun

    ## Currently evaluating LRT for: 1 | SubjID, 1 | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + (1 | SubjID)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + (1 | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: P_Acc ~ 1 + Pronoun + (1 | SubjID)

    ## Currently evaluating LRT for: Pronoun | SubjID, 1 | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + (1 + Pronoun |
    ##     SubjID)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + (1 | SubjID) + (1 |
    ##     Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: P_Acc ~ 1 + Pronoun + (1 + Pronoun | SubjID)

    ## Currently evaluating LRT for: 1 | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + (1 + Pronoun |
    ##     SubjID) + (1 | Name)

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)

``` r
summary(exp1a_m_prod)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: P_Acc ~ 1 + Pronoun + (1 + Pronoun | SubjID)
    ##    Data: exp1a_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1048.6   1094.6   -515.3   1030.6     1215 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5806 -0.2653  0.1580  0.3570  2.0167 
    ## 
    ## Random effects:
    ##  Groups Name                  Variance Std.Dev. Corr       
    ##  SubjID (Intercept)            1.0056  1.0028              
    ##         Pronounthey vs he+she 12.3364  3.5123    0.66      
    ##         Pronounhe vs she       0.3173  0.5633   -0.94 -0.87
    ## Number of obs: 1224, groups:  SubjID, 102
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error z value Pr(>|z|) Pr(>|t|)    
    ## (Intercept)             1.3299     0.1977  6.7267    0.000 1.74e-11 ***
    ## Pronounthey vs he+she   4.1418     0.4705  8.8027    0.000  < 2e-16 ***
    ## Pronounhe vs she       -0.1569     0.4717 -0.3326    0.739    0.739    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Prnvh+
    ## Prnnthyvsh+  0.527       
    ## Prononhvssh -0.238 -0.192

Convert to odds:

``` r
# intercept (mean)
exp1a_m_prod@model %>% get_intercept() %>% exp()
```

    ## [1] 3.780508

``` r
# they/them vs. he/him + she/her
exp1a_m_prod@model %>%
  get_parameters() %>%
  filter(Parameter == "Pronounthey vs he+she") %>%
  pull(Estimate) %>%
  exp()
```

    ## [1] 62.91574

- The intercept is significant (p\<.001), such that participants are
  3.78 times more likely to answer correctly than incorrectly across all
  pronoun types.

- The contrast between they/them and he/him + she/her is significant
  (p\<.001), such that participants are 62.92 times more likely to get
  he/him and she/her right than they/them.

- The contrast between he/him and she/her is not significant.

# Memory Predicting Production

## Descriptive Stats

Accuracy for producing they/them is lower than accuracy for remembering
they/them. But for he/him and she/her, production accuracy is higher.

``` r
exp1a_d %>%
  pivot_longer(
    cols      = c(M_Acc, P_Acc),
    names_to  = "Task",
    values_to = "Acc"
  ) %>%
  group_by(Pronoun, Task) %>%
  summarise(mean = mean(Acc) %>% round(2))
```

    ## # A tibble: 6 × 3
    ## # Groups:   Pronoun [3]
    ##   Pronoun   Task   mean
    ##   <fct>     <chr> <dbl>
    ## 1 he/him    M_Acc  0.76
    ## 2 he/him    P_Acc  0.83
    ## 3 she/her   M_Acc  0.77
    ## 4 she/her   P_Acc  0.86
    ## 5 they/them M_Acc  0.44
    ## 6 they/them P_Acc  0.29

Combining the two measures, there are 4 possible patterns: getting both
right, getting both wrong, getting just memory right, and getting just
production right.

``` r
exp1a_r_dist <- exp1a_d %>%
  mutate(Combined_Accuracy = case_when(
    M_Acc == 1 & P_Acc == 1 ~ "Both right",
    M_Acc == 0 & P_Acc == 0 ~ "Both wrong",
    M_Acc == 1 & P_Acc == 0 ~ "Memory only",
    M_Acc == 0 & P_Acc == 1 ~ "Production only"
  )) %>%
  group_by(Pronoun, Combined_Accuracy) %>%
  summarise(n = n())

exp1a_r_dist
```

    ## # A tibble: 12 × 3
    ## # Groups:   Pronoun [3]
    ##    Pronoun   Combined_Accuracy     n
    ##    <fct>     <chr>             <int>
    ##  1 he/him    Both right          270
    ##  2 he/him    Both wrong           28
    ##  3 he/him    Memory only          41
    ##  4 he/him    Production only      69
    ##  5 she/her   Both right          281
    ##  6 she/her   Both wrong           23
    ##  7 she/her   Memory only          35
    ##  8 she/her   Production only      69
    ##  9 they/them Both right           86
    ## 10 they/them Both wrong          196
    ## 11 they/them Memory only          92
    ## 12 they/them Production only      34

Production accuracy for they/them when memory was correct vs incorrect.

``` r
exp1a_d %>%
  filter(Pronoun == "they/them") %>%
  group_by(M_Acc, Pronoun) %>%
  summarise(P_Acc = mean(P_Acc) %>% round(2))
```

    ## # A tibble: 2 × 3
    ## # Groups:   M_Acc [2]
    ##   M_Acc Pronoun   P_Acc
    ##   <int> <fct>     <dbl>
    ## 1     0 they/them  0.15
    ## 2     1 they/them  0.48

## Model

``` r
exp1a_d %<>% mutate(M_Acc_Factor = as.factor(M_Acc))
contrasts(exp1a_d$M_Acc_Factor) <- cbind("wrong vs right" = c(-0.5, +0.5))
contrasts(exp1a_d$M_Acc_Factor)
```

    ##   wrong vs right
    ## 0           -0.5
    ## 1            0.5

Model predicting production accuracy with pronoun type and memory
accuracy. Otherwise the same model specifications as the first two. The
maximal model includes random intercepts by participant.

``` r
exp1a_m_mp <- buildmer(
  formula = P_Acc ~ M_Acc_Factor * Pronoun +
    (Pronoun | SubjID) + (Pronoun | Name),
  data = exp1a_d, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: P_Acc ~ 1

    ## Currently evaluating LRT for: M_Acc_Factor, Pronoun

    ## Fitting via glm: P_Acc ~ 1 + M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun

    ## Updating formula: P_Acc ~ 1 + Pronoun

    ## Currently evaluating LRT for: M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor

    ## Currently evaluating LRT for: M_Acc_Factor:Pronoun

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     M_Acc_Factor:Pronoun

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     M_Acc_Factor:Pronoun

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     M_Acc_Factor:Pronoun

    ## Currently evaluating LRT for: 1 | SubjID, 1 | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     Pronoun:M_Acc_Factor + (1 | SubjID)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     Pronoun:M_Acc_Factor + (1 | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     Pronoun:M_Acc_Factor + (1 | SubjID)

    ## Currently evaluating LRT for: Pronoun | SubjID, 1 | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     Pronoun:M_Acc_Factor + (1 + Pronoun | SubjID)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     Pronoun:M_Acc_Factor + (1 | SubjID) + (1 | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)
    ##     Singular fit

``` r
summary(exp1a_m_mp)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + Pronoun:M_Acc_Factor + (1 |  
    ##     SubjID)
    ##    Data: exp1a_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1124.1   1159.9   -555.0   1110.1     1217 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9247 -0.4107  0.3204  0.4044  4.1244 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  SubjID (Intercept) 0.3469   0.589   
    ## Number of obs: 1224, groups:  SubjID, 102
    ## 
    ## Fixed effects:
    ##                                                  Estimate Std. Error  z value
    ## (Intercept)                                       0.75129    0.10249  7.33061
    ## Pronounthey vs he+she                             2.60652    0.17635 14.78056
    ## Pronounhe vs she                                  0.22289    0.21153  1.05371
    ## M_Acc_Factorwrong vs right                        1.24522    0.16816  7.40482
    ## Pronounthey vs he+she:M_Acc_Factorwrong vs right -0.81953    0.33637 -2.43640
    ## Pronounhe vs she:M_Acc_Factorwrong vs right      -0.06404    0.42923 -0.14919
    ##                                                  Pr(>|z|) Pr(>|t|)    
    ## (Intercept)                                         0.000 2.29e-13 ***
    ## Pronounthey vs he+she                               0.000  < 2e-16 ***
    ## Pronounhe vs she                                    0.292   0.2920    
    ## M_Acc_Factorwrong vs right                          0.000 1.31e-13 ***
    ## Pronounthey vs he+she:M_Acc_Factorwrong vs right    0.015   0.0148 *  
    ## Pronounhe vs she:M_Acc_Factorwrong vs right         0.881   0.8814    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Prnvh+ Prnnvs M_A_vr Pvh+vr
    ## Prnnthyvsh+  0.210                            
    ## Prononhvssh  0.055  0.055                     
    ## M_Acc_Fctvr -0.206 -0.073 -0.018              
    ## Pvh+:M_A_vr -0.102 -0.251 -0.020  0.158       
    ## Pvs:M_A_Fvr -0.027 -0.032 -0.312  0.054  0.049

Convert to odds:

``` r
# memory accuracy
exp1a_m_mp@model %>%
  get_parameters() %>%
  filter(Parameter == "M_Acc_Factorwrong vs right") %>%
  pull(Estimate) %>%
  exp()
```

    ## [1] 3.473682

``` r
# they/them vs. he/him + she/her * memory accuracy
exp1a_m_mp@model %>%
  get_parameters() %>%
  filter(Parameter == "Pronounthey vs he+she:M_Acc_Factorwrong vs right") %>%
  pull(Estimate) %>%
  exp()
```

    ## [1] 0.4406391

- The effect of memory accuracy is significant (p\<.001), such that
  participants are 3.47x more likely to get the production right if they
  got the memory right.

- Significant interaction between pronoun type (they/them vs. he/him +
  she/her) and memory accuracy (p\<.05) (odds 0.44). The relative
  difficulty of they/them was attenuated when the participant had
  correctly remembered the character’s pronoun during the memory phase
  of the task.

# Compare Memory and Production

Compare accuracy between memory and production tasks by mean-center
effects coding Task:

``` r
exp1a_d_task <- exp1a_d_all %>%  # memory + prod in long format
  filter(Task != "introduction") %>%
  filter(M_Type == "pronoun" | M_Type == "") %>%
  select(Task, Pronoun, Name, M_Acc, P_Acc, SubjID, Name) %>%
  mutate(.keep = c("unused"), Acc = case_when(
    !is.na(M_Acc) ~ M_Acc,  # group into 1 accuracy variable
    !is.na(P_Acc) ~ P_Acc
  )) %>%
  mutate(  # add pronoun dummy code variables
    Pronoun_They0  = ifelse(Pronoun == "they/them", 0, 1),
    Pronoun_HeShe0 = ifelse(Pronoun != "they/them", 0, 1)
  )

# Mean-center effects code Task
exp1a_d_task$Task %<>% factor()
contrasts(exp1a_d_task$Task) <- cbind("mem vs prod" = c(-.5, +.5))
contrasts(exp1a_d_task$Task)
```

    ##            mem vs prod
    ## memory            -0.5
    ## production         0.5

``` r
exp1a_m_task_all <- buildmer(
  formula = Acc ~ Pronoun * Task + (Pronoun | SubjID) + (Pronoun | Name),
  data = exp1a_d_task, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: Acc ~ 1

    ## Currently evaluating LRT for: Pronoun, Task

    ## Fitting via glm: Acc ~ 1 + Pronoun

    ## Fitting via glm: Acc ~ 1 + Task

    ## Updating formula: Acc ~ 1 + Pronoun

    ## Currently evaluating LRT for: Task

    ## Fitting via glm: Acc ~ 1 + Pronoun + Task

    ## Updating formula: Acc ~ 1 + Pronoun + Task

    ## Currently evaluating LRT for: Pronoun:Task

    ## Fitting via glm: Acc ~ 1 + Pronoun + Task + Pronoun:Task

    ## Updating formula: Acc ~ 1 + Pronoun + Task + Pronoun:Task

    ## Fitting via glm: Acc ~ 1 + Pronoun + Task + Pronoun:Task

    ## Currently evaluating LRT for: 1 | SubjID, 1 | Name

    ## Fitting via glmer, with ML: Acc ~ 1 + Pronoun + Task + Pronoun:Task +
    ##     (1 | SubjID)

    ## Fitting via glmer, with ML: Acc ~ 1 + Pronoun + Task + Pronoun:Task +
    ##     (1 | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: Acc ~ 1 + Pronoun + Task + Pronoun:Task + (1 |
    ##     SubjID)

    ## Currently evaluating LRT for: Pronoun | SubjID, 1 | Name

    ## Fitting via glmer, with ML: Acc ~ 1 + Pronoun + Task + Pronoun:Task +
    ##     (1 + Pronoun | SubjID)

    ## Fitting via glmer, with ML: Acc ~ 1 + Pronoun + Task + Pronoun:Task +
    ##     (1 | SubjID) + (1 | Name)

    ## Updating formula: Acc ~ 1 + Pronoun + Task + Pronoun:Task + (1 |
    ##     SubjID) + (1 | Name)

    ## Currently evaluating LRT for: Pronoun | SubjID, Pronoun | Name

    ## Fitting via glmer, with ML: Acc ~ 1 + Pronoun + Task + Pronoun:Task +
    ##     (1 + Pronoun | SubjID) + (1 | Name)

    ## Fitting via glmer, with ML: Acc ~ 1 + Pronoun + Task + Pronoun:Task +
    ##     (1 | SubjID) + (1 + Pronoun | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)
    ##     Singular fit

``` r
summary(exp1a_m_task_all)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Acc ~ 1 + Pronoun + Task + Pronoun:Task + (1 | SubjID) + (1 |  
    ##     Name)
    ##    Data: exp1a_d_task
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2599.0   2645.4  -1291.5   2583.0     2440 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2606 -0.6240  0.3851  0.5335  2.6929 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  SubjID (Intercept) 0.412130 0.64197 
    ##  Name   (Intercept) 0.002093 0.04575 
    ## Number of obs: 2448, groups:  SubjID, 102; Name, 12
    ## 
    ## Fixed effects:
    ##                                       Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                            0.83988    0.08328 10.08473    0.000
    ## Pronounthey vs he+she                  2.20275    0.10704 20.57897    0.000
    ## Pronounhe vs she                       0.14863    0.13586  1.09398    0.274
    ## Taskmem vs prod                        0.12322    0.10116  1.21803    0.223
    ## Pronounthey vs he+she:Taskmem vs prod  1.21198    0.20492  5.91429    0.000
    ## Pronounhe vs she:Taskmem vs prod       0.14218    0.26127  0.54417    0.586
    ##                                       Pr(>|t|)    
    ## (Intercept)                            < 2e-16 ***
    ## Pronounthey vs he+she                  < 2e-16 ***
    ## Pronounhe vs she                         0.274    
    ## Taskmem vs prod                          0.223    
    ## Pronounthey vs he+she:Taskmem vs prod 3.33e-09 ***
    ## Pronounhe vs she:Taskmem vs prod         0.586    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Prnvh+ Prnnvs Tskmvp Pvh+vp
    ## Prnnthyvsh+ 0.143                             
    ## Prononhvssh 0.030  0.043                      
    ## Taskmmvsprd 0.077  0.046  0.026               
    ## Prvh+sh:Tvp 0.042  0.127  0.023  0.167        
    ## Prnnvsh:Tvp 0.017  0.020  0.139  0.040  0.030

- No main effect of Task (p = .22)
- Significant interaction between Pronoun and Task (beta = 1.21, p \<.
  001)

Dummy code to probe interaction:

First, the Task effect just for they/them characters:

``` r
exp1a_m_task_they0 <- glmer(  # use random effects from main
  formula = Acc ~ Pronoun_They0 * Task + (1 | SubjID) + (1 | Name),
  data = exp1a_d_task, family = binomial
)
summary(exp1a_m_task_they0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Acc ~ Pronoun_They0 * Task + (1 | SubjID) + (1 | Name)
    ##    Data: exp1a_d_task
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2596.4   2631.2  -1292.2   2584.4     2442 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4963 -0.6221  0.4018  0.5377  2.6807 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  SubjID (Intercept) 0.411267 0.6413  
    ##  Name   (Intercept) 0.001109 0.0333  
    ## Number of obs: 2448, groups:  SubjID, 102; Name, 12
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                    -0.6137     0.1008  -6.091 1.13e-09 ***
    ## Pronoun_They0                   2.1780     0.1058  20.577  < 2e-16 ***
    ## Taskmem vs prod                -0.6765     0.1547  -4.372 1.23e-05 ***
    ## Pronoun_They0:Taskmem vs prod   1.1962     0.2027   5.902 3.60e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Prn_T0 Tskmvp
    ## Pronon_Thy0 -0.583              
    ## Taskmmvsprd  0.068 -0.081       
    ## Prnn_T0:Tvp -0.055  0.126 -0.765

- Production is significantly *less* accurate than memory for they/them
  characters (beta = -0.81, p \<. .001)

Second, the Task effect just for he/him and she/her characters:

``` r
exp1a_m_task_heshe0 <- glmer(  # use random effects from main
  formula = Acc ~ Pronoun_HeShe0 * Task + (1 | SubjID) + (1 | Name),
  data = exp1a_d_task, family = binomial
)
summary(exp1a_m_task_heshe0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Acc ~ Pronoun_HeShe0 * Task + (1 | SubjID) + (1 | Name)
    ##    Data: exp1a_d_task
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2596.4   2631.2  -1292.2   2584.4     2442 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4963 -0.6221  0.4018  0.5377  2.6807 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  SubjID (Intercept) 0.411264 0.64130 
    ##  Name   (Intercept) 0.001109 0.03331 
    ## Number of obs: 2448, groups:  SubjID, 102; Name, 12
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     1.56431    0.09441  16.569  < 2e-16 ***
    ## Pronoun_HeShe0                 -2.17803    0.10585 -20.577  < 2e-16 ***
    ## Taskmem vs prod                 0.51974    0.13050   3.983 6.81e-05 ***
    ## Pronoun_HeShe0:Taskmem vs prod -1.19621    0.20269  -5.902 3.60e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Pr_HS0 Tskmvp
    ## Pronon_HSh0 -0.499              
    ## Taskmmvsprd  0.108 -0.099       
    ## Prn_HS0:Tvp -0.083  0.126 -0.646

- Production is significantly *more* accurate than memory for he.him and
  she/her characters (beta = 0.55, p \<. .001)

# Compare Pet Questions

Compare pet accuracy to pronoun accuracy. Pronoun (renamed to Character
Pronoun here for clarity) stays contrast coded as before, and Question
Type (M_Type = pet or pronoun) is mean-center effects coded, comparing
pet questions to pronoun questions.

``` r
exp1a_d_all %>%
  group_by(M_Type, Pronoun) %>%
  filter(!is.na(M_Acc)) %>%  # production rows
  summarise(
    mean = mean(M_Acc) %>% round(2),
    sd   = sd(M_Acc)   %>% round(2)
  )
```

    ## # A tibble: 9 × 4
    ## # Groups:   M_Type [3]
    ##   M_Type  Pronoun    mean    sd
    ##   <fct>   <fct>     <dbl> <dbl>
    ## 1 job     he/him     0.22  0.42
    ## 2 job     she/her    0.22  0.42
    ## 3 job     they/them  0.2   0.4 
    ## 4 pet     he/him     0.43  0.5 
    ## 5 pet     she/her    0.42  0.49
    ## 6 pet     they/them  0.39  0.49
    ## 7 pronoun he/him     0.76  0.43
    ## 8 pronoun she/her    0.77  0.42
    ## 9 pronoun they/them  0.44  0.5

``` r
exp1a_d_pronounsPets <- exp1a_d_all %>%
  filter(M_Type == "pet" | M_Type == "pronoun") %>%
  rename("CharPronoun" = "Pronoun")

exp1a_d_pronounsPets$M_Type %<>% droplevels()
contrasts(exp1a_d_pronounsPets$M_Type) <- cbind("petQ vs pronounQ" = c(-.5, .5))
contrasts(exp1a_d_pronounsPets$M_Type)
```

    ##         petQ vs pronounQ
    ## pet                 -0.5
    ## pronoun              0.5

``` r
exp1a_m_pet <- buildmer(
  formula = M_Acc ~ CharPronoun * M_Type +
    (M_Type * CharPronoun | SubjID) + (M_Type * CharPronoun | Name),
  data = exp1a_d_pronounsPets, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: M_Acc ~ 1

    ## Currently evaluating LRT for: CharPronoun, M_Type

    ## Fitting via glm: M_Acc ~ 1 + CharPronoun

    ## Fitting via glm: M_Acc ~ 1 + M_Type

    ## Updating formula: M_Acc ~ 1 + M_Type

    ## Currently evaluating LRT for: CharPronoun

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun

    ## Currently evaluating LRT for: CharPronoun:M_Type

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + CharPronoun:M_Type

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + CharPronoun:M_Type

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + CharPronoun:M_Type

    ## Currently evaluating LRT for: 1 | SubjID, 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 | SubjID)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + (1 | SubjID)

    ## Currently evaluating LRT for: M_Type | SubjID, CharPronoun | SubjID, 1
    ##     | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + M_Type | SubjID)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + CharPronoun | SubjID)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 | SubjID) + (1 | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + (1 + M_Type | SubjID)

    ## Currently evaluating LRT for: CharPronoun | SubjID, 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + M_Type + CharPronoun | SubjID)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + M_Type | SubjID) + (1 | Name)

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)

``` r
summary(exp1a_m_pet)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun + (1 +  
    ##     M_Type | SubjID)
    ##    Data: exp1a_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3057.7   3109.9  -1519.8   3039.7     2439 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0347 -0.7892  0.3833  0.8154  2.1553 
    ## 
    ## Random effects:
    ##  Groups Name                   Variance Std.Dev. Corr
    ##  SubjID (Intercept)            0.3230   0.5684       
    ##         M_TypepetQ vs pronounQ 0.2433   0.4933   0.23
    ## Number of obs: 2448, groups:  SubjID, 102
    ## 
    ## Fixed effects:
    ##                                                  Estimate Std. Error  z value
    ## (Intercept)                                       0.19894    0.07281  2.73247
    ## M_TypepetQ vs pronounQ                            1.16852    0.10443 11.18934
    ## CharPronounthey vs he+she                         0.88440    0.09597  9.21520
    ## CharPronounhe vs she                              0.01010    0.11290  0.08949
    ## M_TypepetQ vs pronounQ:CharPronounthey vs he+she  1.44895    0.19194  7.54912
    ## M_TypepetQ vs pronounQ:CharPronounhe vs she       0.12794    0.22581  0.56659
    ##                                                  Pr(>|z|) Pr(>|t|)    
    ## (Intercept)                                         0.006  0.00629 ** 
    ## M_TypepetQ vs pronounQ                              0.000  < 2e-16 ***
    ## CharPronounthey vs he+she                           0.000  < 2e-16 ***
    ## CharPronounhe vs she                                0.929  0.92869    
    ## M_TypepetQ vs pronounQ:CharPronounthey vs he+she    0.000 4.38e-14 ***
    ## M_TypepetQ vs pronounQ:CharPronounhe vs she         0.571  0.57099    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) M_TQvp ChPvh+ ChrPvs M_vpvh
    ## M_TypptQvpQ 0.160                             
    ## ChrPrnntvh+ 0.065  0.102                      
    ## ChrPrnnhvss 0.007  0.007  0.008               
    ## M_TQvpQ:Cvh 0.073  0.090  0.088  0.006        
    ## M_TQvpQ:Cvs 0.005  0.010  0.006  0.154  0.008

- Significant main effect of Question Type (p\<.001), with higher
  accuracy for pronouns (this makes sense seeing the high accuracy rates
  for he/him and she/her pronoun questions).
- Significant interaction between Pronoun and Question Type, such that
  the difference between Question Types is larger for he/him + she/her
  questions than for they/them characters.

To check this interaction direction, dummy coded Pronoun with they/them
characters as 0 and he/him and she/her characters as 1:

``` r
exp1a_d_pronounsPets %<>% mutate(CharPronoun_They0 = ifelse(
  CharPronoun == "they/them", 0, 1
))

exp1a_m_pet_they <- glmer(  # use random effects from main
  formula = M_Acc ~ CharPronoun_They0 * M_Type + (M_Type | SubjID),
  data = exp1a_d_pronounsPets, family = binomial
)
summary(exp1a_m_pet_they)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ CharPronoun_They0 * M_Type + (M_Type | SubjID)
    ##    Data: exp1a_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3054.0   3094.6  -1520.0   3040.0     2441 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0905 -0.7929  0.3764  0.8220  2.1550 
    ## 
    ## Random effects:
    ##  Groups Name                   Variance Std.Dev. Corr
    ##  SubjID (Intercept)            0.3229   0.5683       
    ##         M_TypepetQ vs pronounQ 0.2432   0.4931   0.23
    ## Number of obs: 2448, groups:  SubjID, 102
    ## 
    ## Fixed effects:
    ##                                          Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                              -0.38487    0.09336  -4.122 3.75e-05
    ## CharPronoun_They0                         0.87544    0.09500   9.215  < 2e-16
    ## M_TypepetQ vs pronounQ                    0.21219    0.15676   1.354    0.176
    ## CharPronoun_They0:M_TypepetQ vs pronounQ  1.43393    0.19000   7.547 4.46e-14
    ##                                             
    ## (Intercept)                              ***
    ## CharPronoun_They0                        ***
    ## M_TypepetQ vs pronounQ                      
    ## CharPronoun_They0:M_TypepetQ vs pronounQ ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) ChP_T0 M_TQvp
    ## ChrPrnn_Th0 -0.628              
    ## M_TypptQvpQ  0.039 -0.003       
    ## CP_T0:M_Tvp -0.003  0.088 -0.748

- Non-significant main effect of Question Type (M_Type) means that there
  is no difference between pet and pronoun questions when Pronoun is 0
  (= for they/them characters).

Now dummy code to get main effect of Question Type in he/him + she/her
(= 0)

``` r
exp1a_d_pronounsPets %<>% mutate(CharPronoun_HeShe0 = ifelse(
  CharPronoun == "they/them", 1, 0
))

exp1a_m_pet_heshe <- glmer(  # use random effects from main
  formula = M_Acc ~ CharPronoun_HeShe0 * M_Type + (M_Type | SubjID),
  data = exp1a_d_pronounsPets, family = binomial
)
summary(exp1a_m_pet_heshe)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ CharPronoun_HeShe0 * M_Type + (M_Type | SubjID)
    ##    Data: exp1a_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3054.0   3094.6  -1520.0   3040.0     2441 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0905 -0.7929  0.3764  0.8220  2.1550 
    ## 
    ## Random effects:
    ##  Groups Name                   Variance Std.Dev. Corr
    ##  SubjID (Intercept)            0.3229   0.5683       
    ##         M_TypepetQ vs pronounQ 0.2432   0.4931   0.23
    ## Number of obs: 2448, groups:  SubjID, 102
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                                0.49057    0.08123   6.039 1.55e-09
    ## CharPronoun_HeShe0                        -0.87545    0.09500  -9.215  < 2e-16
    ## M_TypepetQ vs pronounQ                     1.64612    0.12688  12.974  < 2e-16
    ## CharPronoun_HeShe0:M_TypepetQ vs pronounQ -1.43393    0.19001  -7.547 4.47e-14
    ##                                              
    ## (Intercept)                               ***
    ## CharPronoun_HeShe0                        ***
    ## M_TypepetQ vs pronounQ                    ***
    ## CharPronoun_HeShe0:M_TypepetQ vs pronounQ ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) CP_HS0 M_TQvp
    ## ChrPrnn_HS0 -0.448              
    ## M_TypptQvpQ  0.200 -0.127       
    ## CP_HS0:M_vp -0.099  0.088 -0.573

- Significant (p \< .001) main effect of Question Type (M_Type) means
  that the difference is in he/him + she/her characters (but not
  they/them)
