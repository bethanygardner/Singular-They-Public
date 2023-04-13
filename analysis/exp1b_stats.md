Improving memory for and production of singular <i>they</i> pronouns:
Experiment 1B
================
Bethany Gardner
03/24/2022, 04/08/2023

- <a href="#load-data" id="toc-load-data">Load Data</a>
- <a href="#memory" id="toc-memory">Memory</a>
  - <a href="#descriptive-stats" id="toc-descriptive-stats">Descriptive
    Stats</a>
  - <a href="#model" id="toc-model">Model</a>
- <a href="#production" id="toc-production">Production</a>
  - <a href="#descriptive-stats-1" id="toc-descriptive-stats-1">Descriptive
    Stats</a>
  - <a href="#model-1" id="toc-model-1">Model</a>
- <a href="#memory-predicting-production"
  id="toc-memory-predicting-production">Memory Predicting Production</a>
  - <a href="#descriptive-stats-2" id="toc-descriptive-stats-2">Descriptive
    Stats</a>
  - <a href="#model-2" id="toc-model-2">Model</a>
- <a href="#jobpet" id="toc-jobpet">Job/Pet</a>
- <a href="#compare-to-main-experiment"
  id="toc-compare-to-main-experiment">Compare to Main Experiment</a>
  - <a href="#load-data-1" id="toc-load-data-1">Load Data</a>
  - <a href="#memory-accuracy" id="toc-memory-accuracy">Memory Accuracy</a>
  - <a href="#production-accuracy" id="toc-production-accuracy">Production
    Accuracy</a>
  - <a href="#memory-predicting-production-1"
    id="toc-memory-predicting-production-1">Memory Predicting Production</a>
  - <a href="#task-difference" id="toc-task-difference">Task Difference</a>
  - <a href="#job-and-pet" id="toc-job-and-pet">Job and pet</a>

This is a replication of the first experiment. Everything is identical,
except that the production task occurs before the memory task.

# Load Data

Read data, preprocessed from Qualtrics output. See
data/exp1b_data_readme for more details.

``` r
exp1b_d_all <- read.csv("data/exp1b_data.csv", stringsAsFactors = TRUE)
str(exp1b_d_all)
```

    ## 'data.frame':    5151 obs. of  17 variables:
    ##  $ SubjID     : Factor w/ 101 levels "R_10uYBrhUOzbUAGV",..: 82 82 82 82 82 82 82 82 82 82 ...
    ##  $ SubjAge    : int  18 18 18 18 18 18 18 18 18 18 ...
    ##  $ SubjGender : Factor w/ 14 levels "female","Female",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ SubjEnglish: Factor w/ 3 levels "Fully competent in speaking listening reading and writing but not native\n",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ List       : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Task       : Factor w/ 3 levels "introduction",..: 2 2 2 3 2 2 2 3 2 2 ...
    ##  $ Name       : Factor w/ 12 levels "Amanda","Andrew",..: 3 3 3 3 8 8 8 8 4 4 ...
    ##  $ Pronoun    : Factor w/ 3 levels "he/him","she/her",..: 1 1 1 1 1 1 1 1 3 3 ...
    ##  $ Pet        : Factor w/ 3 levels "cat","dog","fish": 1 1 1 1 2 2 2 2 3 3 ...
    ##  $ Job        : Factor w/ 12 levels "accountant","doctor",..: 12 12 12 12 4 4 4 4 9 9 ...
    ##  $ M_Type     : Factor w/ 4 levels "","job","pet",..: 2 3 4 1 2 3 4 1 2 3 ...
    ##  $ M_Response : Factor w/ 19 levels "","accountant",..: 19 7 9 1 8 5 9 1 14 7 ...
    ##  $ M_Acc      : int  0 0 1 NA 1 1 1 NA 1 1 ...
    ##  $ P_Response : Factor w/ 755 levels "","After Amanda got home from working as an engineer they took a rest directly.",..: 1 1 1 45 1 1 1 649 1 1 ...
    ##  $ P_Pronoun  : Factor w/ 5 levels "","he/him","none",..: 1 1 1 2 1 1 1 5 1 1 ...
    ##  $ P_Acc      : int  NA NA NA 1 NA NA NA 0 NA NA ...
    ##  $ I_Response : Factor w/ 297 levels "","Amanda goes by she/her. Amanda is a salesman and has a pet cat. ",..: 1 1 1 1 1 1 1 1 1 1 ...

Set up contrast coding. The first contrast compares they to he+she. The
second contrast compares he to she.

``` r
contrasts(exp1b_d_all$Pronoun) <- cbind(
  "they vs he+she" = c(.33, .33, -.66),
  "he vs she"      = c(-.5, .5, 0)
)
contrasts(exp1b_d_all$Pronoun)
```

    ##           they vs he+she he vs she
    ## he/him              0.33      -0.5
    ## she/her             0.33       0.5
    ## they/them          -0.66       0.0

Combine pronoun memory and production trials to make one row for each
character.

``` r
m_temp <- exp1b_d_all %>%
  filter(M_Type == "pronoun") %>%
  select(SubjID, Name, Pronoun, M_Response, M_Acc)
p_temp <- exp1b_d_all %>%
  filter(!is.na(P_Acc)) %>%
  select(SubjID, Name, Pronoun, P_Pronoun, P_Acc)

exp1b_d <- left_join(m_temp, p_temp, by = c("SubjID", "Name", "Pronoun"))
remove(m_temp, p_temp)

str(exp1b_d)
```

    ## 'data.frame':    1212 obs. of  7 variables:
    ##  $ SubjID    : Factor w/ 101 levels "R_10uYBrhUOzbUAGV",..: 82 82 82 82 82 82 82 82 82 82 ...
    ##  $ Name      : Factor w/ 12 levels "Amanda","Andrew",..: 3 8 4 9 10 12 2 5 1 11 ...
    ##  $ Pronoun   : Factor w/ 3 levels "he/him","she/her",..: 1 1 3 3 2 2 1 2 2 1 ...
    ##   ..- attr(*, "contrasts")= num [1:3, 1:2] 0.33 0.33 -0.66 -0.5 0.5 0
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:3] "he/him" "she/her" "they/them"
    ##   .. .. ..$ : chr [1:2] "they vs he+she" "he vs she"
    ##  $ M_Response: Factor w/ 19 levels "","accountant",..: 9 9 18 18 16 16 9 16 18 9 ...
    ##  $ M_Acc     : int  1 1 1 1 1 1 1 1 0 1 ...
    ##  $ P_Pronoun : Factor w/ 5 levels "","he/him","none",..: 2 5 5 5 4 4 2 4 4 2 ...
    ##  $ P_Acc     : int  1 0 1 1 1 1 1 1 1 1 ...

# Memory

### Descriptive Stats

Mean accuracy for all three memory question types.

``` r
exp1b_d_all %>%
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
    ## 1 job      0.29  0.45
    ## 2 pet      0.43  0.5 
    ## 3 pronoun  0.69  0.46

Mean accuracy, split by pronoun type.

``` r
exp1b_d %>%
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
    ## 1 He + She       0.79   0.4
    ## 2 They           0.48   0.5

96% of participants selected they/them at least once.

``` r
exp1b_r_memory_usedThey <- exp1b_d %>%
  filter(M_Response == "they/them") %>%
  summarise(n = n_distinct(SubjID))

exp1b_r_memory_usedThey / (n_distinct(exp1b_d$SubjID))
```

    ##          n
    ## 1 0.960396

### Model

Start with model that has random intercepts and slopes for participant
and item. Maximal model has by-participant random intercepts only.

``` r
exp1b_m_memory <- buildmer(
  formula = M_Acc ~ Pronoun + (1 + Pronoun | SubjID) + (1 + Pronoun | Name),
  data = exp1b_d, family = binomial,
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

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: M_Acc ~ 1 + Pronoun + (1 | SubjID)

    ## Currently evaluating LRT for: Pronoun | SubjID, 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 + Pronoun |
    ##     SubjID)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 | SubjID) + (1 |
    ##     Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: M_Acc ~ 1 + Pronoun + (1 + Pronoun | SubjID)

    ## Currently evaluating LRT for: 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + (1 + Pronoun |
    ##     SubjID) + (1 | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: Singular fit

``` r
summary(exp1b_m_memory)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ 1 + Pronoun + (1 + Pronoun | SubjID)
    ##    Data: exp1b_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1367.8   1413.7   -674.9   1349.8     1203 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5497 -0.8096  0.3707  0.5848  1.3200 
    ## 
    ## Random effects:
    ##  Groups Name                  Variance Std.Dev. Corr     
    ##  SubjID (Intercept)           0.4874   0.6981            
    ##         Pronounthey vs he+she 0.8763   0.9361   0.59     
    ##         Pronounhe vs she      0.1645   0.4055   0.62 1.00
    ## Number of obs: 1212, groups:  SubjID, 101
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error z value Pr(>|z|) Pr(>|t|)    
    ## (Intercept)             1.0379     0.1098  9.4549    0.000   <2e-16 ***
    ## Pronounthey vs he+she   1.6866     0.1831  9.2099    0.000   <2e-16 ***
    ## Pronounhe vs she        0.2568     0.2423  1.0600    0.289    0.289    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Prnvh+
    ## Prnnthyvsh+ 0.464        
    ## Prononhvssh 0.210  0.216

Convert to odds:

``` r
# intercept (mean)
exp1b_m_memory@model %>% get_intercept() %>% exp()
```

    ## [1] 2.823393

``` r
# they/them vs. he/him + she/her
exp1b_m_memory@model %>%
  get_parameters() %>%
  filter(Parameter == "Pronounthey vs he+she") %>%
  pull(Estimate) %>%
  exp()
```

    ## [1] 5.401342

- The intercept is significant (p\<.001), such that participants are
  2.82 times more likely to answer correctly than incorrectly across all
  pronoun types.

- The contrast between they/them and he/him + she/her is significant
  (p\<.001), such that participants are 5.40 times more likely to get
  he/him and she/her right than they/them.

- The contrast between he/him and she/her is not significant.

# Production

### Descriptive Stats

Mean accuracy, split by pronoun type.

``` r
exp1b_d %>%
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
    ## 2 They           0.39  0.49

Responses that did not use a pronoun are infrequent and evenly
distributed across pronoun conditions.

``` r
table(exp1b_d$Pronoun, exp1b_d$P_Pronoun)
```

    ##            
    ##                 he/him none she/her they/them
    ##   he/him      0    340   14       1        49
    ##   she/her     0      1   15     341        47
    ##   they/them   0    124   12     110       158

``` r
(exp1b_d %>% filter(P_Pronoun == "none") %>% pull(P_Pronoun) %>% length()) /
(exp1b_d %>% pull(P_Pronoun) %>% length())
```

    ## [1] 0.03382838

71% of participants produced they/them at least once.

``` r
exp1b_r_prod_usedThey <- exp1b_d %>%
  filter(P_Pronoun == "they/them") %>%
  summarize(n = n_distinct(SubjID))

exp1b_r_prod_usedThey / (n_distinct(exp1b_d$SubjID))
```

    ##           n
    ## 1 0.7128713

### Model

Start with model that has random intercepts and slopes for participant
and item, using same specifications as before. Maximal model has random
intercepts by participant and item, and no random slopes.

``` r
exp1b_m_prod <- buildmer(
  formula = P_Acc ~ Pronoun + (1 + Pronoun | SubjID) + (1 + Pronoun | Name),
  data = exp1b_d, family = binomial,
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

    ## Updating formula: P_Acc ~ 1 + Pronoun + (1 | SubjID) + (1 | Name)

    ## Currently evaluating LRT for: Pronoun | SubjID, Pronoun | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + (1 + Pronoun |
    ##     SubjID) + (1 | Name)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + (1 | SubjID) + (1 +
    ##     Pronoun | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)
    ##     Singular fit

``` r
summary(exp1b_m_prod)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: P_Acc ~ 1 + Pronoun + (1 | SubjID) + (1 | Name)
    ##    Data: exp1b_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1207.4   1232.9   -598.7   1197.4     1207 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.0720 -0.6925  0.3443  0.4193  2.1954 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  SubjID (Intercept) 0.77891  0.88256 
    ##  Name   (Intercept) 0.00775  0.08803 
    ## Number of obs: 1212, groups:  SubjID, 101; Name, 12
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error   z value Pr(>|z|) Pr(>|t|)    
    ## (Intercept)            1.108332   0.123498  8.974463    0.000   <2e-16 ***
    ## Pronounthey vs he+she  2.476427   0.167460 14.788192    0.000   <2e-16 ***
    ## Pronounhe vs she       0.002089   0.223504  0.009344    0.993    0.993    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Prnvh+
    ## Prnnthyvsh+  0.246       
    ## Prononhvssh -0.025 -0.042

Convert to odds:

``` r
# intercept (mean)
exp1b_m_prod@model %>% get_intercept() %>% exp()
```

    ## [1] 3.0293

``` r
# they/them vs. he/him + she/her
exp1b_m_prod@model %>%
  get_parameters() %>%
  filter(Parameter == "Pronounthey vs he+she") %>%
  pull(Estimate) %>%
  exp()
```

    ## [1] 11.89868

- The intercept is significant (p\<.001), such that participants are
  3.03 times more likely to answer correctly than incorrectly across all
  pronoun types.

- The contrast between they/them and he/him + she/her is significant
  (p\<.001), such that participants are 11.90 times more likely to get
  he/him and she/her right than they/them.

- The contrast between he/him and she/her is not significant.

# Memory Predicting Production

### Descriptive Stats

Accuracy for producing they/them is lower than accuracy for remembering
they/them. But for he/him and she/her, production accuracy is higher.

``` r
exp1b_d %>%
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
    ## 1 he/him    M_Acc  0.79
    ## 2 he/him    P_Acc  0.84
    ## 3 she/her   M_Acc  0.8 
    ## 4 she/her   P_Acc  0.84
    ## 5 they/them M_Acc  0.48
    ## 6 they/them P_Acc  0.39

Combining the two measures, there are 4 possible patterns: getting both
right, getting both wrong, getting just memory right, and getting just
production right.

``` r
exp1b_d %<>% mutate(
  Combined_Accuracy = case_when(
    M_Acc == 1 & P_Acc == 1 ~ "Both right",
    M_Acc == 0 & P_Acc == 0 ~ "Both wrong",
    M_Acc == 1 & P_Acc == 0 ~ "Memory only",
    M_Acc == 0 & P_Acc == 1 ~ "Production only"
  )
)

exp1b_d %>%
  group_by(Pronoun, Combined_Accuracy) %>%
  summarise(n = n())
```

    ## # A tibble: 12 × 3
    ## # Groups:   Pronoun [3]
    ##    Pronoun   Combined_Accuracy     n
    ##    <fct>     <chr>             <int>
    ##  1 he/him    Both right          278
    ##  2 he/him    Both wrong           24
    ##  3 he/him    Memory only          40
    ##  4 he/him    Production only      62
    ##  5 she/her   Both right          286
    ##  6 she/her   Both wrong           25
    ##  7 she/her   Memory only          38
    ##  8 she/her   Production only      55
    ##  9 they/them Both right          125
    ## 10 they/them Both wrong          176
    ## 11 they/them Memory only          70
    ## 12 they/them Production only      33

Production accuracy for they/them when memory was correct vs incorrect.

``` r
exp1b_d %>%
  filter(Pronoun == "they/them") %>%
  group_by(M_Acc, Pronoun) %>%
  summarise(P_Acc = mean(P_Acc) %>% round(2))
```

    ## # A tibble: 2 × 3
    ## # Groups:   M_Acc [2]
    ##   M_Acc Pronoun   P_Acc
    ##   <int> <fct>     <dbl>
    ## 1     0 they/them  0.16
    ## 2     1 they/them  0.64

### Model

Create factor for Memory Accuracy that is mean-center effects coded,
comparing incorrect to correct.

``` r
exp1b_d %<>% mutate(M_Acc_Factor = as.factor(M_Acc))
contrasts(exp1b_d$M_Acc_Factor) <- cbind("wrong vs right" = c(-0.5, +0.5))
contrasts(exp1b_d$M_Acc_Factor)
```

    ##   wrong vs right
    ## 0           -0.5
    ## 1            0.5

``` r
exp1b_m_mp_buildmer <- buildmer(
  formula = P_Acc ~ M_Acc_Factor * Pronoun +
    (Pronoun * M_Acc_Factor | SubjID) + (M_Acc_Factor * Pronoun | Name),
  data = exp1b_d, family = binomial,
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

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)
    ##     Singular fit

``` r
summary(exp1b_m_mp_buildmer)
```

    ## 
    ## Call:
    ## stats::glm(formula = P_Acc ~ 1 + Pronoun + M_Acc_Factor + M_Acc_Factor:Pronoun, 
    ##     family = binomial, data = exp1b_d)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0703  -0.5863   0.4995   0.5185   1.9214  
    ## 
    ## Coefficients:
    ##                                                  Estimate Std. Error z value
    ## (Intercept)                                       0.76675    0.08039   9.538
    ## Pronounthey vs he+she                             1.99066    0.16120  12.349
    ## Pronounhe vs she                                 -0.04048    0.20880  -0.194
    ## M_Acc_Factorwrong vs right                        1.49113    0.16078   9.274
    ## Pronounthey vs he+she:M_Acc_Factorwrong vs right -1.15555    0.32239  -3.584
    ## Pronounhe vs she:M_Acc_Factorwrong vs right       0.24029    0.41760   0.575
    ##                                                  Pr(>|z|)    
    ## (Intercept)                                       < 2e-16 ***
    ## Pronounthey vs he+she                             < 2e-16 ***
    ## Pronounhe vs she                                 0.846278    
    ## M_Acc_Factorwrong vs right                        < 2e-16 ***
    ## Pronounthey vs he+she:M_Acc_Factorwrong vs right 0.000338 ***
    ## Pronounhe vs she:M_Acc_Factorwrong vs right      0.565016    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1496.3  on 1211  degrees of freedom
    ## Residual deviance: 1113.0  on 1206  degrees of freedom
    ## AIC: 1125
    ## 
    ## Number of Fisher Scoring iterations: 4

buildmer doesn’t include any random effects, which is odd. Here I’m
starting by adding back the by-subject random intercepts (based on that
having the larger variance in the production model).

``` r
exp1b_m_mp_subjInt <- glmer(
  formula = P_Acc ~ M_Acc_Factor * Pronoun + (1 | SubjID),
  data = exp1b_d, family = binomial
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0425739 (tol = 0.002, component 1)

``` r
summary(exp1b_m_mp_subjInt)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: P_Acc ~ M_Acc_Factor * Pronoun + (1 | SubjID)
    ##    Data: exp1b_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1090.6   1126.3   -538.3   1076.6     1205 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8353 -0.3792  0.3033  0.4348  3.1647 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  SubjID (Intercept) 0.7483   0.865   
    ## Number of obs: 1212, groups:  SubjID, 101
    ## 
    ## Fixed effects:
    ##                                                   Estimate Std. Error z value
    ## (Intercept)                                       0.898566   0.001665  539.66
    ## M_Acc_Factorwrong vs right                        1.569237   0.001663  943.66
    ## Pronounthey vs he+she                             2.306495   0.001663 1387.02
    ## Pronounhe vs she                                 -0.061725   0.001663  -37.12
    ## M_Acc_Factorwrong vs right:Pronounthey vs he+she -1.293479   0.001663 -777.58
    ## M_Acc_Factorwrong vs right:Pronounhe vs she       0.334674   0.001663  201.29
    ##                                                  Pr(>|z|)    
    ## (Intercept)                                        <2e-16 ***
    ## M_Acc_Factorwrong vs right                         <2e-16 ***
    ## Pronounthey vs he+she                              <2e-16 ***
    ## Pronounhe vs she                                   <2e-16 ***
    ## M_Acc_Factorwrong vs right:Pronounthey vs he+she   <2e-16 ***
    ## M_Acc_Factorwrong vs right:Pronounhe vs she        <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) M_A_vr Prnvh+ Prnnvs M_vrvh
    ## M_Acc_Fctvr 0.000                             
    ## Prnnthyvsh+ 0.000  0.000                      
    ## Prononhvssh 0.000  0.000  0.000               
    ## M_A_Fvr:Pvh 0.000  0.000  0.000  0.000        
    ## M_A_Fvr:Pvs 0.000  0.000  0.000  0.000  0.000 
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0425739 (tol = 0.002, component 1)

``` r
exp1b_opt_subjInt <- allFit(exp1b_m_mp_subjInt)
```

    ## Loading required namespace: dfoptim

    ## Loading required namespace: optimx

    ## bobyqa : [OK]
    ## Nelder_Mead :

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0425663 (tol = 0.002, component 1)

    ## [OK]
    ## nlminbwrap :

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0366285 (tol = 0.002, component 1)

    ## [OK]
    ## nmkbw :

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0440317 (tol = 0.002, component 1)

    ## [OK]
    ## optimx.L-BFGS-B :

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0477569 (tol = 0.002, component 1)

    ## [OK]
    ## nloptwrap.NLOPT_LN_NELDERMEAD :

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

    ## [OK]
    ## nloptwrap.NLOPT_LN_BOBYQA :

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

    ## [OK]

``` r
summary(exp1b_opt_subjInt)
```

    ## $which.OK
    ##                        bobyqa                   Nelder_Mead 
    ##                          TRUE                          TRUE 
    ##                    nlminbwrap                         nmkbw 
    ##                          TRUE                          TRUE 
    ##               optimx.L-BFGS-B nloptwrap.NLOPT_LN_NELDERMEAD 
    ##                          TRUE                          TRUE 
    ##     nloptwrap.NLOPT_LN_BOBYQA 
    ##                          TRUE 
    ## 
    ## $msgs
    ## $msgs$bobyqa
    ## NULL
    ## 
    ## $msgs$Nelder_Mead
    ## [1] "Model failed to converge with max|grad| = 0.0425663 (tol = 0.002, component 1)"
    ## 
    ## $msgs$nlminbwrap
    ## [1] "Model failed to converge with max|grad| = 0.0366285 (tol = 0.002, component 1)"
    ## 
    ## $msgs$nmkbw
    ## [1] "Model failed to converge with max|grad| = 0.0440317 (tol = 0.002, component 1)"
    ## 
    ## $msgs$`optimx.L-BFGS-B`
    ## [1] "Model failed to converge with max|grad| = 0.0477569 (tol = 0.002, component 1)"
    ## 
    ## $msgs$nloptwrap.NLOPT_LN_NELDERMEAD
    ## [1] "unable to evaluate scaled gradient"                                       
    ## [2] "Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"
    ## 
    ## $msgs$nloptwrap.NLOPT_LN_BOBYQA
    ## [1] "unable to evaluate scaled gradient"                                       
    ## [2] "Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"
    ## 
    ## 
    ## $fixef
    ##                               (Intercept) M_Acc_Factorwrong vs right
    ## bobyqa                          0.8950352                   1.568797
    ## Nelder_Mead                     0.8985385                   1.570374
    ## nlminbwrap                      0.8983151                   1.570995
    ## nmkbw                           0.8985452                   1.570372
    ## optimx.L-BFGS-B                 0.8976185                   1.571337
    ## nloptwrap.NLOPT_LN_NELDERMEAD   0.8988380                   1.570183
    ## nloptwrap.NLOPT_LN_BOBYQA       0.8988237                   1.570065
    ##                               Pronounthey vs he+she Pronounhe vs she
    ## bobyqa                                     2.303844      -0.06181638
    ## Nelder_Mead                                2.306500      -0.06201012
    ## nlminbwrap                                 2.306317      -0.06219052
    ## nmkbw                                      2.306555      -0.06222682
    ## optimx.L-BFGS-B                            2.305349      -0.06116414
    ## nloptwrap.NLOPT_LN_NELDERMEAD              2.306831      -0.06209217
    ## nloptwrap.NLOPT_LN_BOBYQA                  2.306347      -0.05401377
    ##                               M_Acc_Factorwrong vs right:Pronounthey vs he+she
    ## bobyqa                                                               -1.298448
    ## Nelder_Mead                                                          -1.293261
    ## nlminbwrap                                                           -1.295887
    ## nmkbw                                                                -1.293054
    ## optimx.L-BFGS-B                                                      -1.294828
    ## nloptwrap.NLOPT_LN_NELDERMEAD                                        -1.294981
    ## nloptwrap.NLOPT_LN_BOBYQA                                            -1.289607
    ##                               M_Acc_Factorwrong vs right:Pronounhe vs she
    ## bobyqa                                                          0.3331351
    ## Nelder_Mead                                                     0.3336713
    ## nlminbwrap                                                      0.3342465
    ## nmkbw                                                           0.3343293
    ## optimx.L-BFGS-B                                                 0.3302678
    ## nloptwrap.NLOPT_LN_NELDERMEAD                                   0.3339103
    ## nloptwrap.NLOPT_LN_BOBYQA                                       0.3141189
    ## 
    ## $llik
    ##                        bobyqa                   Nelder_Mead 
    ##                     -538.2934                     -538.2907 
    ##                    nlminbwrap                         nmkbw 
    ##                     -538.2909                     -538.2907 
    ##               optimx.L-BFGS-B nloptwrap.NLOPT_LN_NELDERMEAD 
    ##                     -538.2911                     -538.2908 
    ##     nloptwrap.NLOPT_LN_BOBYQA 
    ##                     -538.2920 
    ## 
    ## $sdcor
    ##                               SubjID.(Intercept)
    ## bobyqa                                 0.8608571
    ## Nelder_Mead                            0.8648093
    ## nlminbwrap                             0.8655682
    ## nmkbw                                  0.8647637
    ## optimx.L-BFGS-B                        0.8665007
    ## nloptwrap.NLOPT_LN_NELDERMEAD          0.8648929
    ## nloptwrap.NLOPT_LN_BOBYQA              0.8641605
    ## 
    ## $theta
    ##                               SubjID.(Intercept)
    ## bobyqa                                 0.8608571
    ## Nelder_Mead                            0.8648093
    ## nlminbwrap                             0.8655682
    ## nmkbw                                  0.8647637
    ## optimx.L-BFGS-B                        0.8665007
    ## nloptwrap.NLOPT_LN_NELDERMEAD          0.8648929
    ## nloptwrap.NLOPT_LN_BOBYQA              0.8641605
    ## 
    ## $times
    ##                               user.self sys.self elapsed user.child sys.child
    ## bobyqa                             0.40        0    0.53         NA        NA
    ## Nelder_Mead                        1.68        0    2.19         NA        NA
    ## nlminbwrap                         0.85        0    1.11         NA        NA
    ## nmkbw                              1.26        0    1.61         NA        NA
    ## optimx.L-BFGS-B                    1.79        0    2.42         NA        NA
    ## nloptwrap.NLOPT_LN_NELDERMEAD      1.39        0    1.99         NA        NA
    ## nloptwrap.NLOPT_LN_BOBYQA          0.28        0    0.37         NA        NA
    ## 
    ## $feval
    ##                        bobyqa                   Nelder_Mead 
    ##                           205                           894 
    ##                    nlminbwrap                         nmkbw 
    ##                            NA                           615 
    ##               optimx.L-BFGS-B nloptwrap.NLOPT_LN_NELDERMEAD 
    ##                            74                           794 
    ##     nloptwrap.NLOPT_LN_BOBYQA 
    ##                            77 
    ## 
    ## attr(,"class")
    ## [1] "summary.allFit"

Most of the optimizers are throwing convergence errors, and all of the
estimates are very consistent across optimizers. But the z values are
weird and you shouldn’t get identical SEs, so the errors aren’t ok to
ignore.

Check by-item intercepts:

``` r
exp1b_m_mp_itemInt <- glmer(
  formula = P_Acc ~ M_Acc_Factor * Pronoun + (1 | Name),
  data = exp1b_d, family = binomial
)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(exp1b_m_mp_itemInt)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: P_Acc ~ M_Acc_Factor * Pronoun + (1 | Name)
    ##    Data: exp1b_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1127.0   1162.7   -556.5   1113.0     1205 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7434 -0.4330  0.3645  0.3793  2.3094 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Name   (Intercept) 0        0       
    ## Number of obs: 1212, groups:  Name, 12
    ## 
    ## Fixed effects:
    ##                                                  Estimate Std. Error z value
    ## (Intercept)                                       0.76675    0.08039   9.538
    ## M_Acc_Factorwrong vs right                        1.49113    0.16078   9.274
    ## Pronounthey vs he+she                             1.99066    0.16119  12.350
    ## Pronounhe vs she                                 -0.04048    0.20880  -0.194
    ## M_Acc_Factorwrong vs right:Pronounthey vs he+she -1.15555    0.32238  -3.584
    ## M_Acc_Factorwrong vs right:Pronounhe vs she       0.24029    0.41762   0.575
    ##                                                  Pr(>|z|)    
    ## (Intercept)                                       < 2e-16 ***
    ## M_Acc_Factorwrong vs right                        < 2e-16 ***
    ## Pronounthey vs he+she                             < 2e-16 ***
    ## Pronounhe vs she                                 0.846282    
    ## M_Acc_Factorwrong vs right:Pronounthey vs he+she 0.000338 ***
    ## M_Acc_Factorwrong vs right:Pronounhe vs she      0.565040    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) M_A_vr Prnvh+ Prnnvs M_vrvh
    ## M_Acc_Fctvr -0.306                            
    ## Prnnthyvsh+  0.188 -0.098                     
    ## Prononhvssh  0.008  0.004  0.006              
    ## M_A_Fvr:Pvh -0.098  0.188 -0.276  0.003       
    ## M_A_Fvr:Pvs  0.004  0.008  0.003 -0.330  0.006
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
exp1b_opt_itemInt <- allFit(exp1b_m_mp_itemInt)
```

    ## bobyqa :

    ## boundary (singular) fit: see help('isSingular')

    ## [OK]
    ## Nelder_Mead :

    ## boundary (singular) fit: see help('isSingular')

    ## [OK]
    ## nlminbwrap :

    ## boundary (singular) fit: see help('isSingular')

    ## [OK]
    ## nmkbw :

    ## boundary (singular) fit: see help('isSingular')

    ## [OK]
    ## optimx.L-BFGS-B :

    ## boundary (singular) fit: see help('isSingular')

    ## [OK]
    ## nloptwrap.NLOPT_LN_NELDERMEAD :

    ## boundary (singular) fit: see help('isSingular')

    ## [OK]
    ## nloptwrap.NLOPT_LN_BOBYQA :

    ## boundary (singular) fit: see help('isSingular')

    ## [OK]

``` r
summary(exp1b_opt_itemInt)
```

    ## $which.OK
    ##                        bobyqa                   Nelder_Mead 
    ##                          TRUE                          TRUE 
    ##                    nlminbwrap                         nmkbw 
    ##                          TRUE                          TRUE 
    ##               optimx.L-BFGS-B nloptwrap.NLOPT_LN_NELDERMEAD 
    ##                          TRUE                          TRUE 
    ##     nloptwrap.NLOPT_LN_BOBYQA 
    ##                          TRUE 
    ## 
    ## $msgs
    ## $msgs$bobyqa
    ## [1] "boundary (singular) fit: see help('isSingular')"
    ## 
    ## $msgs$Nelder_Mead
    ## [1] "boundary (singular) fit: see help('isSingular')"
    ## 
    ## $msgs$nlminbwrap
    ## [1] "boundary (singular) fit: see help('isSingular')"
    ## 
    ## $msgs$nmkbw
    ## [1] "boundary (singular) fit: see help('isSingular')"
    ## 
    ## $msgs$`optimx.L-BFGS-B`
    ## [1] "boundary (singular) fit: see help('isSingular')"
    ## 
    ## $msgs$nloptwrap.NLOPT_LN_NELDERMEAD
    ## [1] "boundary (singular) fit: see help('isSingular')"
    ## 
    ## $msgs$nloptwrap.NLOPT_LN_BOBYQA
    ## [1] "boundary (singular) fit: see help('isSingular')"
    ## 
    ## 
    ## $fixef
    ##                               (Intercept) M_Acc_Factorwrong vs right
    ## bobyqa                          0.7667544                   1.491135
    ## Nelder_Mead                     0.7667545                   1.491135
    ## nlminbwrap                      0.7667545                   1.491135
    ## nmkbw                           0.7667545                   1.491135
    ## optimx.L-BFGS-B                 0.7667546                   1.491135
    ## nloptwrap.NLOPT_LN_NELDERMEAD   0.7667545                   1.491135
    ## nloptwrap.NLOPT_LN_BOBYQA       0.7667545                   1.491135
    ##                               Pronounthey vs he+she Pronounhe vs she
    ## bobyqa                                     1.990657      -0.04047981
    ## Nelder_Mead                                1.990657      -0.04047961
    ## nlminbwrap                                 1.990657      -0.04047946
    ## nmkbw                                      1.990657      -0.04047961
    ## optimx.L-BFGS-B                            1.990657      -0.04047961
    ## nloptwrap.NLOPT_LN_NELDERMEAD              1.990657      -0.04047961
    ## nloptwrap.NLOPT_LN_BOBYQA                  1.990657      -0.04047961
    ##                               M_Acc_Factorwrong vs right:Pronounthey vs he+she
    ## bobyqa                                                               -1.155546
    ## Nelder_Mead                                                          -1.155546
    ## nlminbwrap                                                           -1.155546
    ## nmkbw                                                                -1.155546
    ## optimx.L-BFGS-B                                                      -1.155546
    ## nloptwrap.NLOPT_LN_NELDERMEAD                                        -1.155546
    ## nloptwrap.NLOPT_LN_BOBYQA                                            -1.155546
    ##                               M_Acc_Factorwrong vs right:Pronounhe vs she
    ## bobyqa                                                          0.2402873
    ## Nelder_Mead                                                     0.2402872
    ## nlminbwrap                                                      0.2402873
    ## nmkbw                                                           0.2402872
    ## optimx.L-BFGS-B                                                 0.2402872
    ## nloptwrap.NLOPT_LN_NELDERMEAD                                   0.2402872
    ## nloptwrap.NLOPT_LN_BOBYQA                                       0.2402872
    ## 
    ## $llik
    ##                        bobyqa                   Nelder_Mead 
    ##                     -556.4816                     -556.4816 
    ##                    nlminbwrap                         nmkbw 
    ##                     -556.4816                     -556.4816 
    ##               optimx.L-BFGS-B nloptwrap.NLOPT_LN_NELDERMEAD 
    ##                     -556.4816                     -556.4816 
    ##     nloptwrap.NLOPT_LN_BOBYQA 
    ##                     -556.4816 
    ## 
    ## $sdcor
    ##                               Name.(Intercept)
    ## bobyqa                            0.000000e+00
    ## Nelder_Mead                       0.000000e+00
    ## nlminbwrap                        2.061502e-09
    ## nmkbw                             1.540609e-07
    ## optimx.L-BFGS-B                   0.000000e+00
    ## nloptwrap.NLOPT_LN_NELDERMEAD     0.000000e+00
    ## nloptwrap.NLOPT_LN_BOBYQA         0.000000e+00
    ## 
    ## $theta
    ##                               Name.(Intercept)
    ## bobyqa                            0.000000e+00
    ## Nelder_Mead                       0.000000e+00
    ## nlminbwrap                        2.061502e-09
    ## nmkbw                             1.540609e-07
    ## optimx.L-BFGS-B                   0.000000e+00
    ## nloptwrap.NLOPT_LN_NELDERMEAD     0.000000e+00
    ## nloptwrap.NLOPT_LN_BOBYQA         0.000000e+00
    ## 
    ## $times
    ##                               user.self sys.self elapsed user.child sys.child
    ## bobyqa                             0.25        0    0.27         NA        NA
    ## Nelder_Mead                        0.39        0    0.52         NA        NA
    ## nlminbwrap                         0.25        0    0.27         NA        NA
    ## nmkbw                              0.58        0    0.67         NA        NA
    ## optimx.L-BFGS-B                    0.94        0    1.03         NA        NA
    ## nloptwrap.NLOPT_LN_NELDERMEAD      0.48        0    0.55         NA        NA
    ## nloptwrap.NLOPT_LN_BOBYQA          0.21        0    0.30         NA        NA
    ## 
    ## $feval
    ##                        bobyqa                   Nelder_Mead 
    ##                            33                           201 
    ##                    nlminbwrap                         nmkbw 
    ##                            NA                           270 
    ##               optimx.L-BFGS-B nloptwrap.NLOPT_LN_NELDERMEAD 
    ##                             6                           218 
    ##     nloptwrap.NLOPT_LN_BOBYQA 
    ##                            48 
    ## 
    ## attr(,"class")
    ## [1] "summary.allFit"

The by-item intercept model isn’t giving impossible results, but two of
the optimizers have very different estimates for the random intercepts
than the rest of them, so let’s just stick with the original model:

``` r
summary(exp1b_m_mp_buildmer)
```

    ## 
    ## Call:
    ## stats::glm(formula = P_Acc ~ 1 + Pronoun + M_Acc_Factor + M_Acc_Factor:Pronoun, 
    ##     family = binomial, data = exp1b_d)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0703  -0.5863   0.4995   0.5185   1.9214  
    ## 
    ## Coefficients:
    ##                                                  Estimate Std. Error z value
    ## (Intercept)                                       0.76675    0.08039   9.538
    ## Pronounthey vs he+she                             1.99066    0.16120  12.349
    ## Pronounhe vs she                                 -0.04048    0.20880  -0.194
    ## M_Acc_Factorwrong vs right                        1.49113    0.16078   9.274
    ## Pronounthey vs he+she:M_Acc_Factorwrong vs right -1.15555    0.32239  -3.584
    ## Pronounhe vs she:M_Acc_Factorwrong vs right       0.24029    0.41760   0.575
    ##                                                  Pr(>|z|)    
    ## (Intercept)                                       < 2e-16 ***
    ## Pronounthey vs he+she                             < 2e-16 ***
    ## Pronounhe vs she                                 0.846278    
    ## M_Acc_Factorwrong vs right                        < 2e-16 ***
    ## Pronounthey vs he+she:M_Acc_Factorwrong vs right 0.000338 ***
    ## Pronounhe vs she:M_Acc_Factorwrong vs right      0.565016    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1496.3  on 1211  degrees of freedom
    ## Residual deviance: 1113.0  on 1206  degrees of freedom
    ## AIC: 1125
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# memory accuracy
exp1b_m_mp_buildmer@model %>%
  get_parameters() %>%
  filter(Parameter == "M_Acc_Factorwrong vs right") %>%
  pull(Estimate) %>%
  exp()
```

    ## [1] 4.442133

``` r
# they/them vs. he/him + she/her * memory accuracy
exp1b_m_mp_buildmer@model %>%
  get_parameters() %>%
  filter(Parameter == "Pronounthey vs he+she:M_Acc_Factorwrong vs right") %>%
  pull(Estimate) %>%
  exp()
```

    ## [1] 0.3148857

- The effect of memory accuracy is significant (p\<.001), such that
  participants are 4.44x more likely to get the production right if they
  got the memory right.

- Significant interaction between pronoun type (they/them vs. he/him +
  she/her) and memory accuracy (p\<.05) (odds 0.31). The relative
  difficulty of they/them was attenuated when the participant had
  correctly remembered the character’s pronoun during the memory phase
  of the task.

# Job/Pet

``` r
exp1b_d_all %>%
  filter(M_Type == "job") %>%
  group_by(Pronoun) %>%
  summarise(
    mean = mean(M_Acc) %>% round(2),
    sd   = sd(M_Acc)   %>% round(2)
  )
```

    ## # A tibble: 3 × 3
    ##   Pronoun    mean    sd
    ##   <fct>     <dbl> <dbl>
    ## 1 he/him     0.28  0.45
    ## 2 she/her    0.26  0.44
    ## 3 they/them  0.34  0.47

``` r
exp1b_d_all %>%
  filter(M_Type == "pet") %>%
  group_by(Pronoun) %>%
  summarise(
    mean = mean(M_Acc) %>% round(2),
    sd   = sd(M_Acc)   %>% round(2)
  )
```

    ## # A tibble: 3 × 3
    ##   Pronoun    mean    sd
    ##   <fct>     <dbl> <dbl>
    ## 1 he/him     0.43  0.5 
    ## 2 she/her    0.44  0.5 
    ## 3 they/them  0.42  0.49

Compare pet accuracy to pronoun accuracy. Pronoun (renamed to Character
Pronoun here for clarity) stays contrast coded as before, and Question
Type (M_Type = pet or pronoun) is mean-center effects coded, comparing
pet questions to pronoun questions.

``` r
exp1b_d_pronounsPets <- exp1b_d_all %>%
  filter(M_Type == "pet" | M_Type == "pronoun") %>%
  rename("CharPronoun" = "Pronoun")

exp1b_d_pronounsPets$M_Type %<>% droplevels()
contrasts(exp1b_d_pronounsPets$M_Type) <- cbind(
  "petQ vs pronounQ" = c(-.5, .5))
contrasts(exp1b_d_pronounsPets$M_Type)
```

    ##         petQ vs pronounQ
    ## pet                 -0.5
    ## pronoun              0.5

``` r
exp1b_m_pet <- buildmer(
  formula = M_Acc ~ CharPronoun * M_Type +
            (M_Type * CharPronoun | SubjID) +
            (M_Type * CharPronoun | Name),
  data = exp1b_d_pronounsPets, family = binomial,
  buildmerControl(direction = "order"))
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

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + (1 + M_Type | SubjID)

    ## Currently evaluating LRT for: CharPronoun | SubjID, 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + M_Type + CharPronoun | SubjID)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + M_Type | SubjID) + (1 | Name)

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + (1 + M_Type | SubjID) + (1 | Name)

    ## Currently evaluating LRT for: CharPronoun | SubjID, M_Type | Name,
    ##     CharPronoun | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + M_Type + CharPronoun | SubjID) + (1 |
    ##     Name)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + M_Type | SubjID) + (1 + M_Type | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + (1 + M_Type | SubjID) + (1 + CharPronoun |
    ##     Name)

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)
    ##     Singular fit

``` r
summary(exp1b_m_pet)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun + (1 +  
    ##     M_Type | SubjID) + (1 | Name)
    ##    Data: exp1b_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3013.5   3071.5  -1496.8   2993.5     2414 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0396 -0.8344  0.3886  0.8552  1.6850 
    ## 
    ## Random effects:
    ##  Groups Name                   Variance Std.Dev. Corr
    ##  SubjID (Intercept)            0.256178 0.50614      
    ##         M_TypepetQ vs pronounQ 0.176991 0.42070  0.42
    ##  Name   (Intercept)            0.005328 0.07299      
    ## Number of obs: 2424, groups:  SubjID, 101; Name, 12
    ## 
    ## Fixed effects:
    ##                                                  Estimate Std. Error  z value
    ## (Intercept)                                       0.32426    0.07198  4.50507
    ## M_TypepetQ vs pronounQ                            1.25541    0.10276 12.21664
    ## CharPronounthey vs he+she                         0.81816    0.09589  8.53251
    ## CharPronounhe vs she                              0.07462    0.12437  0.60001
    ## M_TypepetQ vs pronounQ:CharPronounthey vs he+she  1.47364    0.19167  7.68828
    ## M_TypepetQ vs pronounQ:CharPronounhe vs she       0.02192    0.23069  0.09500
    ##                                                  Pr(>|z|) Pr(>|t|)    
    ## (Intercept)                                         0.000 6.64e-06 ***
    ## M_TypepetQ vs pronounQ                              0.000  < 2e-16 ***
    ## CharPronounthey vs he+she                           0.000  < 2e-16 ***
    ## CharPronounhe vs she                                0.549    0.549    
    ## M_TypepetQ vs pronounQ:CharPronounthey vs he+she    0.000 1.49e-14 ***
    ## M_TypepetQ vs pronounQ:CharPronounhe vs she         0.924    0.924    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) M_TQvp ChPvh+ ChrPvs M_vpvh
    ## M_TypptQvpQ 0.231                             
    ## ChrPrnntvh+ 0.088  0.129                      
    ## ChrPrnnhvss 0.005  0.007  0.002               
    ## M_TQvpQ:Cvh 0.091  0.124  0.111  0.007        
    ## M_TQvpQ:Cvs 0.010  0.011  0.011  0.189  0.009

- Like in main experiment:
- Significant main effect of Question Type (p\<.001), with higher
  accuracy for pronouns
- Significant interaction between Pronoun and Question Type, such that
  the difference between Question Types is larger for he/him + she/her
  questions than for they/them characters.

To check this interaction direction, dummy coded Pronoun with they/them
characters as 0 and he/him and she/her characters as 1

``` r
exp1b_d_pronounsPets %<>% mutate(CharPronoun_They0 = ifelse(
  CharPronoun == "they/them", 0, 1))

exp1b_m_pet_they <- glmer(
  formula = M_Acc ~ CharPronoun_They0 * M_Type + (M_Type | SubjID),
  data = exp1b_d_pronounsPets, family = binomial,
)

summary(exp1b_m_pet_they)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ CharPronoun_They0 * M_Type + (M_Type | SubjID)
    ##    Data: exp1b_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3008.3   3048.9  -1497.2   2994.3     2417 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9502 -0.8339  0.3947  0.8578  1.6731 
    ## 
    ## Random effects:
    ##  Groups Name                   Variance Std.Dev. Corr
    ##  SubjID (Intercept)            0.2548   0.5048       
    ##         M_TypepetQ vs pronounQ 0.1771   0.4208   0.42
    ## Number of obs: 2424, groups:  SubjID, 101
    ## 
    ## Fixed effects:
    ##                                          Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                              -0.21495    0.08897  -2.416   0.0157
    ## CharPronoun_They0                         0.80796    0.09479   8.523  < 2e-16
    ## M_TypepetQ vs pronounQ                    0.28248    0.15272   1.850   0.0644
    ## CharPronoun_They0:M_TypepetQ vs pronounQ  1.45625    0.18959   7.681 1.58e-14
    ##                                             
    ## (Intercept)                              *  
    ## CharPronoun_They0                        ***
    ## M_TypepetQ vs pronounQ                   .  
    ## CharPronoun_They0:M_TypepetQ vs pronounQ ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) ChP_T0 M_TQvp
    ## ChrPrnn_Th0 -0.639              
    ## M_TypptQvpQ  0.068 -0.005       
    ## CP_T0:M_Tvp -0.005  0.110 -0.745

- Only trending (p = .064) main effect of Question Type (M_Type) means
  that there is no difference between pet and pronoun questions when
  Pronoun is 0 (= for they/them characters).

Now dummy code to get main effect of Question Type in he/him + she/her
(= 0)

``` r
exp1b_d_pronounsPets %<>% mutate(CharPronoun_HeShe0 = ifelse(
  CharPronoun == "they/them", 1, 0))

exp1b_m_pet_heshe <- glmer(
  formula = M_Acc ~ CharPronoun_HeShe0 * M_Type + (M_Type | SubjID),
  data = exp1b_d_pronounsPets, family = binomial,
)

summary(exp1b_m_pet_heshe)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ CharPronoun_HeShe0 * M_Type + (M_Type | SubjID)
    ##    Data: exp1b_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3008.3   3048.9  -1497.2   2994.3     2417 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9502 -0.8339  0.3947  0.8578  1.6731 
    ## 
    ## Random effects:
    ##  Groups Name                   Variance Std.Dev. Corr
    ##  SubjID (Intercept)            0.2548   0.5048       
    ##         M_TypepetQ vs pronounQ 0.1771   0.4208   0.42
    ## Number of obs: 2424, groups:  SubjID, 101
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                                0.59301    0.07820   7.583 3.38e-14
    ## CharPronoun_HeShe0                        -0.80795    0.09479  -8.523  < 2e-16
    ## M_TypepetQ vs pronounQ                     1.73873    0.12697  13.694  < 2e-16
    ## CharPronoun_HeShe0:M_TypepetQ vs pronounQ -1.45626    0.18958  -7.681 1.57e-14
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
    ## ChrPrnn_HS0 -0.485              
    ## M_TypptQvpQ  0.277 -0.158       
    ## CP_HS0:M_vp -0.128  0.110 -0.597

- Like in main experiment, significant (p \< .001) main effect of
  Question Type (M_Type) means that pronoun questions were more accurate
  than pet questions for he/him + she/her characters

``` r
```


``` r
```



