Improving memory for and production of singular <i>they</i> pronouns:
Experiment 2
================
Bethany Gardner
04/13/2023

- [Load data](#load-data)
- [Memory](#memory)
  - [Descriptive Stats](#descriptive-stats)
  - [Model](#model)
- [Production](#production)
  - [Descriptive Stats](#descriptive-stats-1)
  - [Model](#model-1)
  - [Three-Way Interaction](#three-way-interaction)
  - [Producing they/them at least
    once](#producing-theythem-at-least-once)
- [Memory Predicting Production](#memory-predicting-production)
  - [Descriptive Stats](#descriptive-stats-2)
  - [Model](#model-2)
- [Compare Pet Questions](#compare-pet-questions)
- [Compare to Exp1](#compare-to-exp1)

# Load data

Read data, preprocessed from PCIbex output. See `data/exp2_data_readme`
for more details.

``` r
exp2_d_all <- read.csv("data/exp2_data.csv", stringsAsFactors = TRUE) %>%
  rename("Biographies" = "Story") # rename to match labeling in paper

exp2_d_all$Participant %<>% as.factor()

exp2_d_all$PSA %<>% as.factor() %>%
  recode_factor("0" = "Unrelated", "1" = "Gender")

exp2_d_all$Biographies %<>% as.factor() %>%
  recode_factor("0" = "HeShe", "1" = "They")

exp2_d_all$X <- NULL

str(exp2_d_all)
```

    ## 'data.frame':    11520 obs. of  18 variables:
    ##  $ Participant: Factor w/ 320 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ SubjAge    : int  53 53 53 53 53 53 53 53 53 53 ...
    ##  $ SubjEnglish: Factor w/ 4 levels "Fully competent in speaking, listening, reading, and writing, but not native",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ SubjGender : Factor w/ 2 levels "female","male": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Condition  : Factor w/ 4 levels "both","neither",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ List       : Factor w/ 12 levels "both_1","both_2",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PSA        : Factor w/ 2 levels "Unrelated","Gender": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Biographies: Factor w/ 2 levels "HeShe","They": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Name       : Factor w/ 12 levels "Amanda","Andrew",..: 2 2 7 2 7 1 1 7 6 6 ...
    ##  $ Job        : Factor w/ 12 levels "accountant","doctor",..: 11 11 4 11 4 3 3 4 6 6 ...
    ##  $ Pet        : Factor w/ 3 levels "cat","dog","fish": 2 2 3 2 3 1 1 3 2 2 ...
    ##  $ Pronoun    : Factor w/ 3 levels "he/him","she/her",..: 1 1 2 1 2 2 2 2 1 1 ...
    ##  $ M_Type     : Factor w/ 3 levels "job","pet","pronoun": 2 3 1 1 3 1 2 2 1 2 ...
    ##  $ M_Response : Factor w/ 18 levels "accountant","cat",..: 4 8 11 10 15 9 6 2 5 4 ...
    ##  $ M_Acc      : int  1 1 0 0 1 0 0 0 0 1 ...
    ##  $ P_Text     : Factor w/ 2918 levels " after the work he  play with the kids",..: NA 2625 NA NA 2172 NA NA NA NA NA ...
    ##  $ P_Response : Factor w/ 4 levels "he/him","none",..: NA 4 NA NA 3 NA NA NA NA NA ...
    ##  $ P_Acc      : int  NA 0 NA NA 1 NA NA NA NA NA ...

Set up contrast coding for Pronoun Type. The first contrast compares
*they* to *he*+*she*. The second contrast compares *he* to *she*.

``` r
contrasts(exp2_d_all$Pronoun) <- cbind(
  "_T_HS" = c(.33, .33, -.66),
  "_H_S"  = c(-.5, .5, 0)
)
contrasts(exp2_d_all$Pronoun)
```

    ##           _T_HS _H_S
    ## he/him     0.33 -0.5
    ## she/her    0.33  0.5
    ## they/them -0.66  0.0

Set up contrast coding for PSA and Biographies conditions. .5 are the
conditions related to singular they (gendered language PSA, they/them
biographies); -.5 are the unrelated conditions (unrelated PSA, he/him
and she/her biographies).

``` r
# check labels
exp2_d_all %>% count(Condition, PSA, Biographies)
```

    ##   Condition       PSA Biographies    n
    ## 1      both    Gender        They 2880
    ## 2   neither Unrelated       HeShe 2880
    ## 3       psa    Gender       HeShe 2880
    ## 4     story Unrelated        They 2880

``` r
contrasts(exp2_d_all$PSA) <- cbind("_GenLang" = c(-.5, .5))
contrasts(exp2_d_all$PSA)
```

    ##           _GenLang
    ## Unrelated     -0.5
    ## Gender         0.5

``` r
contrasts(exp2_d_all$Biographies) <- cbind("_They" = c(-.5, .5))
contrasts(exp2_d_all$Biographies)
```

    ##       _They
    ## HeShe  -0.5
    ## They    0.5

Remove pet and job rows, and the columns that aren’t used in the models.

``` r
exp2_d <- exp2_d_all %>%
  filter(M_Type == "pronoun") %>%
  select(
    Participant, Condition, PSA, Biographies, Name, Pronoun,
    M_Acc, M_Response, P_Acc, P_Response
  )

str(exp2_d)
```

    ## 'data.frame':    3840 obs. of  10 variables:
    ##  $ Participant: Factor w/ 320 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Condition  : Factor w/ 4 levels "both","neither",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PSA        : Factor w/ 2 levels "Unrelated","Gender": 2 2 2 2 2 2 2 2 2 2 ...
    ##   ..- attr(*, "contrasts")= num [1:2, 1] -0.5 0.5
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:2] "Unrelated" "Gender"
    ##   .. .. ..$ : chr "_GenLang"
    ##  $ Biographies: Factor w/ 2 levels "HeShe","They": 2 2 2 2 2 2 2 2 2 2 ...
    ##   ..- attr(*, "contrasts")= num [1:2, 1] -0.5 0.5
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:2] "HeShe" "They"
    ##   .. .. ..$ : chr "_They"
    ##  $ Name       : Factor w/ 12 levels "Amanda","Andrew",..: 2 7 6 11 9 5 3 10 4 12 ...
    ##  $ Pronoun    : Factor w/ 3 levels "he/him","she/her",..: 1 2 1 3 2 2 1 3 1 3 ...
    ##   ..- attr(*, "contrasts")= num [1:3, 1:2] 0.33 0.33 -0.66 -0.5 0.5 0
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:3] "he/him" "she/her" "they/them"
    ##   .. .. ..$ : chr [1:2] "_T_HS" "_H_S"
    ##  $ M_Acc      : int  1 1 1 1 1 0 1 1 1 1 ...
    ##  $ M_Response : Factor w/ 18 levels "accountant","cat",..: 8 15 8 17 15 17 8 17 8 17 ...
    ##  $ P_Acc      : int  0 1 1 1 1 0 1 1 1 1 ...
    ##  $ P_Response : Factor w/ 4 levels "he/him","none",..: 4 3 1 4 3 4 1 4 1 4 ...

``` r
summary(exp2_d)
```

    ##   Participant     Condition          PSA       Biographies       Name     
    ##  1      :  12   both   :960   Unrelated:1920   HeShe:1920   Amanda : 320  
    ##  2      :  12   neither:960   Gender   :1920   They :1920   Andrew : 320  
    ##  3      :  12   psa    :960                                 Brian  : 320  
    ##  4      :  12   story  :960                                 Daniel : 320  
    ##  5      :  12                                               Emily  : 320  
    ##  6      :  12                                               James  : 320  
    ##  (Other):3768                                               (Other):1920  
    ##       Pronoun         M_Acc             M_Response       P_Acc      
    ##  he/him   :1280   Min.   :0.0000   she/her   :1366   Min.   :0.000  
    ##  she/her  :1280   1st Qu.:0.0000   he/him    :1334   1st Qu.:0.000  
    ##  they/them:1280   Median :1.0000   they/them :1140   Median :1.000  
    ##                   Mean   :0.7161   accountant:   0   Mean   :0.637  
    ##                   3rd Qu.:1.0000   cat       :   0   3rd Qu.:1.000  
    ##                   Max.   :1.0000   doctor    :   0   Max.   :1.000  
    ##                                    (Other)   :   0                  
    ##      P_Response  
    ##  he/him   :1652  
    ##  none     : 156  
    ##  she/her  :1534  
    ##  they/them: 498  
    ##                  
    ##                  
    ## 

# Memory

## Descriptive Stats

Mean accuracy for all three memory question types.

``` r
exp2_d_all %>%
  group_by(M_Type) %>%
  summarise(
    mean = mean(M_Acc) %>% round(2),
    sd   = sd(M_Acc)   %>% round(2)
  )
```

    ## # A tibble: 3 × 3
    ##   M_Type   mean    sd
    ##   <fct>   <dbl> <dbl>
    ## 1 job      0.37  0.48
    ## 2 pet      0.54  0.5 
    ## 3 pronoun  0.72  0.45

Mean accuracy, split by Pronoun Type, PSA, and Biographies conditions.
\[Both = gendered language PSA + *they* biographies; PSA = gendered
language PSA + *he*/she biographies; Story = unrelated PSA + they
biographies; Neither = unrelated PSA + he/she biographies.\]

``` r
exp2_d %>%
  mutate(Pronoun_Group = ifelse(Pronoun == "they/them", "They", "HeShe")) %>%
  group_by(Pronoun_Group, PSA, Biographies) %>%
  summarise(mean = mean(M_Acc) %>% round(2)) %>%
  pivot_wider(names_from = Pronoun_Group, values_from = mean) %>%
  mutate(Mean_Diff = HeShe - They) %>%
  arrange(Mean_Diff)
```

    ## # A tibble: 4 × 5
    ## # Groups:   PSA [2]
    ##   PSA       Biographies HeShe  They Mean_Diff
    ##   <fct>     <fct>       <dbl> <dbl>     <dbl>
    ## 1 Gender    They         0.77  0.57      0.2 
    ## 2 Gender    HeShe        0.82  0.59      0.23
    ## 3 Unrelated They         0.8   0.51      0.29
    ## 4 Unrelated HeShe        0.82  0.52      0.3

90-95% of participants selected they/them at least once.

``` r
exp2_d %>%
  filter(M_Response == "they/them") %>%
  group_by(Condition) %>%
  summarise(
    n    = n_distinct(Participant),
    prop = n / 80
  )
```

    ## # A tibble: 4 × 3
    ##   Condition     n  prop
    ##   <fct>     <int> <dbl>
    ## 1 both         76 0.95 
    ## 2 neither      72 0.9  
    ## 3 psa          73 0.912
    ## 4 story        74 0.925

## Model

Full model has interactions between Pronoun (2 contrasts), PSA, and
Biographies; random intercepts and slopes by participant and item.
buildmer finds the maximal model that will converge (but doesn’t then go
backward to remove non-significant terms, the default setting). The
final model includes all fixed effects/interactions and random
intercepts by name.

``` r
exp2_m_memory <- buildmer(
  formula = M_Acc ~ Pronoun * PSA * Biographies +
    (Pronoun | Participant) + (Pronoun | Name),
  data = exp2_d, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: M_Acc ~ 1

    ## Currently evaluating LRT for: Pronoun, PSA, Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun

    ## Fitting via glm: M_Acc ~ 1 + PSA

    ## Fitting via glm: M_Acc ~ 1 + Biographies

    ## Updating formula: M_Acc ~ 1 + Pronoun

    ## Currently evaluating LRT for: PSA, Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + PSA

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies

    ## Updating formula: M_Acc ~ 1 + Pronoun + Biographies

    ## Currently evaluating LRT for: PSA, Pronoun:Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies +
    ##     Pronoun:Biographies

    ## Updating formula: M_Acc ~ 1 + Pronoun + Biographies + PSA

    ## Currently evaluating LRT for: Pronoun:PSA, Pronoun:Biographies,
    ##     PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA +
    ##     Pronoun:Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA +
    ##     PSA:Biographies

    ## Updating formula: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA

    ## Currently evaluating LRT for: Pronoun:Biographies, PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + Pronoun:Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + PSA:Biographies

    ## Updating formula: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + Pronoun:Biographies

    ## Currently evaluating LRT for: PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + Pronoun:Biographies + PSA:Biographies

    ## Updating formula: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + Pronoun:Biographies + PSA:Biographies

    ## Currently evaluating LRT for: Pronoun:PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + Pronoun:Biographies + Biographies:PSA + Pronoun:PSA:Biographies

    ## Updating formula: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + Pronoun:Biographies + Biographies:PSA + Pronoun:PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + Pronoun:Biographies + Biographies:PSA + Pronoun:PSA:Biographies

    ## Currently evaluating LRT for: 1 | Participant, 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + Biographies + PSA +
    ##     Pronoun:PSA + Pronoun:Biographies + Biographies:PSA +
    ##     Pronoun:Biographies:PSA + (1 | Participant)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + Biographies + PSA +
    ##     Pronoun:PSA + Pronoun:Biographies + Biographies:PSA +
    ##     Pronoun:Biographies:PSA + (1 | Name)

    ## Updating formula: M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA
    ##     + Pronoun:Biographies + Biographies:PSA + Pronoun:Biographies:PSA +
    ##     (1 | Name)

    ## Currently evaluating LRT for: 1 | Participant, Pronoun | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + Biographies + PSA +
    ##     Pronoun:PSA + Pronoun:Biographies + Biographies:PSA +
    ##     Pronoun:Biographies:PSA + (1 | Name) + (1 | Participant)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + Pronoun + Biographies + PSA +
    ##     Pronoun:PSA + Pronoun:Biographies + Biographies:PSA +
    ##     Pronoun:Biographies:PSA + (1 + Pronoun | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)
    ##     Singular fit

``` r
summary(exp2_m_memory)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## M_Acc ~ 1 + Pronoun + Biographies + PSA + Pronoun:PSA + Pronoun:Biographies +  
    ##     Biographies:PSA + Pronoun:Biographies:PSA + (1 | Name)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4331.6   4412.9  -2152.8   4305.6     3827 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2977 -1.0257  0.4830  0.5489  1.0425 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Name   (Intercept) 0.008349 0.09137 
    ## Number of obs: 3840, groups:  Name, 12
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                                0.99624    0.04641 21.46737    0.000
    ## Pronoun_T_HS                               1.21757    0.07599 16.02350    0.000
    ## Pronoun_H_S                                0.14014    0.11078  1.26496    0.206
    ## Biographies_They                          -0.17570    0.07625 -2.30426    0.021
    ## PSA_GenLang                                0.02205    0.07625  0.28920    0.772
    ## Pronoun_T_HS:PSA_GenLang                  -0.36169    0.15193 -2.38062    0.017
    ## Pronoun_H_S:PSA_GenLang                    0.12121    0.19910  0.60879    0.543
    ## Pronoun_T_HS:Biographies_They             -0.16107    0.15192 -1.06024    0.289
    ## Pronoun_H_S:Biographies_They               0.04874    0.19911  0.24476    0.807
    ## Biographies_They:PSA_GenLang              -0.07341    0.15251 -0.48136    0.630
    ## Pronoun_T_HS:Biographies_They:PSA_GenLang -0.13107    0.30389 -0.43131    0.666
    ## Pronoun_H_S:Biographies_They:PSA_GenLang  -0.08755    0.39833 -0.21980    0.826
    ##                                           Pr(>|t|)    
    ## (Intercept)                                 <2e-16 ***
    ## Pronoun_T_HS                                <2e-16 ***
    ## Pronoun_H_S                                 0.2059    
    ## Biographies_They                            0.0212 *  
    ## PSA_GenLang                                 0.7724    
    ## Pronoun_T_HS:PSA_GenLang                    0.0173 *  
    ## Pronoun_H_S:PSA_GenLang                     0.5427    
    ## Pronoun_T_HS:Biographies_They               0.2890    
    ## Pronoun_H_S:Biographies_They                0.8066    
    ## Biographies_They:PSA_GenLang                0.6303    
    ## Pronoun_T_HS:Biographies_They:PSA_GenLang   0.6662    
    ## Pronoun_H_S:Biographies_They:PSA_GenLang    0.8260    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Pr_T_HS Pr_H_S Bgrp_T PSA_GL P_T_HS:P P_H_S:P Pr_T_HS:B_T
    ## Pronon_T_HS  0.171                                                          
    ## Pronoun_H_S  0.025  0.024                                                   
    ## Bigrphs_Thy -0.043 -0.038   0.001                                           
    ## PSA_GenLang -0.013 -0.019   0.013 -0.011                                    
    ## P_T_HS:PSA_ -0.016 -0.004   0.011 -0.007  0.207                             
    ## P_H_S:PSA_G  0.012  0.011  -0.023 -0.009  0.038  0.029                      
    ## Pr_T_HS:B_T -0.031 -0.031   0.001  0.207 -0.007 -0.007   -0.007             
    ## Prn_H_S:B_T  0.001  0.001  -0.061  0.038 -0.009 -0.007   -0.014   0.029     
    ## Bg_T:PSA_GL -0.009 -0.007  -0.008 -0.016 -0.052 -0.038    0.001  -0.019     
    ## P_T_HS:B_T: -0.007 -0.007  -0.006 -0.019 -0.038 -0.031    0.001  -0.004     
    ## P_H_S:B_T:P -0.007 -0.007  -0.013  0.014  0.001  0.001   -0.067   0.011     
    ##             Pr_H_S:B_T B_T:PS P_T_HS:B_T:
    ## Pronon_T_HS                              
    ## Pronoun_H_S                              
    ## Bigrphs_Thy                              
    ## PSA_GenLang                              
    ## P_T_HS:PSA_                              
    ## P_H_S:PSA_G                              
    ## Pr_T_HS:B_T                              
    ## Prn_H_S:B_T                              
    ## Bg_T:PSA_GL  0.014                       
    ## P_T_HS:B_T:  0.011      0.207            
    ## P_H_S:B_T:P -0.025      0.038  0.029

# Production

## Descriptive Stats

Mean accuracy, split by Pronoun Type, PSA, and Biographies conditions.
\[Both = gendered language PSA + *they* biographies; PSA = gendered
language PSA + *he/she* biographies; Story = unrelated PSA + *they*
biographies; Neither = unrelated PSA + *he/she* biographies.\]

``` r
exp2_d %>%
  mutate(Pronoun_Group = ifelse(Pronoun == "they/them", "They", "HeShe")) %>%
  group_by(Pronoun_Group, PSA, Biographies) %>%
  summarise(mean = mean(P_Acc) %>% round(2)) %>%
  pivot_wider(names_from = Pronoun_Group, values_from = mean) %>%
  mutate(Mean_Diff = HeShe - They) %>%
  arrange(Mean_Diff)
```

    ## # A tibble: 4 × 5
    ## # Groups:   PSA [2]
    ##   PSA       Biographies HeShe  They Mean_Diff
    ##   <fct>     <fct>       <dbl> <dbl>     <dbl>
    ## 1 Gender    HeShe        0.79  0.33      0.46
    ## 2 Gender    They         0.83  0.33      0.5 
    ## 3 Unrelated They         0.85  0.12      0.73
    ## 4 Unrelated HeShe        0.9   0.11      0.79

## Model

Same model specifications as before. The maximal model contains all
fixed effects/interactions and by-item random intercepts.

``` r
exp2_m_prod <- buildmer(
  formula = P_Acc ~ Pronoun * PSA * Biographies +
    (Pronoun | Participant) + (Pronoun | Name),
  data = exp2_d, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: P_Acc ~ 1

    ## Currently evaluating LRT for: Pronoun, PSA, Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun

    ## Fitting via glm: P_Acc ~ 1 + PSA

    ## Fitting via glm: P_Acc ~ 1 + Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun

    ## Currently evaluating LRT for: PSA, Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun + PSA

    ## Currently evaluating LRT for: Pronoun:PSA, Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA + Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA

    ## Currently evaluating LRT for: Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies

    ## Currently evaluating LRT for: Pronoun:Biographies, PSA:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + Pronoun:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + PSA:Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + PSA:Biographies

    ## Currently evaluating LRT for: Pronoun:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + PSA:Biographies + Pronoun:Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + PSA:Biographies + Pronoun:Biographies

    ## Currently evaluating LRT for: Pronoun:PSA:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + PSA:Biographies + Pronoun:Biographies + Pronoun:PSA:Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + PSA:Biographies + Pronoun:Biographies + Pronoun:PSA:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + PSA:Biographies + Pronoun:Biographies + Pronoun:PSA:Biographies

    ## Currently evaluating LRT for: 1 | Participant, 1 | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA +
    ##     Biographies + PSA:Biographies + Pronoun:Biographies +
    ##     Pronoun:PSA:Biographies + (1 | Participant)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA +
    ##     Biographies + PSA:Biographies + Pronoun:Biographies +
    ##     Pronoun:PSA:Biographies + (1 | Name)

    ## Updating formula: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies
    ##     + PSA:Biographies + Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     (1 | Name)

    ## Currently evaluating LRT for: 1 | Participant, Pronoun | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA +
    ##     Biographies + PSA:Biographies + Pronoun:Biographies +
    ##     Pronoun:PSA:Biographies + (1 | Name) + (1 | Participant)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA +
    ##     Biographies + PSA:Biographies + Pronoun:Biographies +
    ##     Pronoun:PSA:Biographies + (1 + Pronoun | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)
    ##     Singular fit

``` r
summary(exp2_m_prod)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## P_Acc ~ 1 + Pronoun + PSA + Pronoun:PSA + Biographies + PSA:Biographies +  
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies + (1 | Name)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3464.0   3545.3  -1719.0   3438.0     3827 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4309 -0.3672  0.3616  0.4550  3.0688 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Name   (Intercept) 0.007172 0.08469 
    ## Number of obs: 3840, groups:  Name, 12
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error  z value Pr(>|z|)
    ## (Intercept)                                0.69508    0.05161 13.46873    0.000
    ## Pronoun_T_HS                               3.15560    0.09577 32.95093    0.000
    ## Pronoun_H_S                               -0.26080    0.12341 -2.11336    0.035
    ## PSA_GenLang                                0.10699    0.09085  1.17767    0.239
    ## Biographies_They                          -0.05933    0.09084 -0.65305    0.514
    ## Pronoun_T_HS:PSA_GenLang                  -1.90734    0.19075 -9.99914    0.000
    ## Pronoun_H_S:PSA_GenLang                    0.26552    0.22695  1.16994    0.242
    ## PSA_GenLang:Biographies_They               0.42601    0.18168  2.34483    0.019
    ## Pronoun_T_HS:Biographies_They             -0.16030    0.19077 -0.84028    0.401
    ## Pronoun_H_S:Biographies_They               0.08454    0.22696  0.37248    0.710
    ## Pronoun_T_HS:PSA_GenLang:Biographies_They  0.88225    0.38148  2.31271    0.021
    ## Pronoun_H_S:PSA_GenLang:Biographies_They   0.13413    0.45382  0.29555    0.768
    ##                                           Pr(>|t|)    
    ## (Intercept)                                 <2e-16 ***
    ## Pronoun_T_HS                                <2e-16 ***
    ## Pronoun_H_S                                 0.0346 *  
    ## PSA_GenLang                                 0.2389    
    ## Biographies_They                            0.5137    
    ## Pronoun_T_HS:PSA_GenLang                    <2e-16 ***
    ## Pronoun_H_S:PSA_GenLang                     0.2420    
    ## PSA_GenLang:Biographies_They                0.0190 *  
    ## Pronoun_T_HS:Biographies_They               0.4007    
    ## Pronoun_H_S:Biographies_They                0.7095    
    ## Pronoun_T_HS:PSA_GenLang:Biographies_They   0.0207 *  
    ## Pronoun_H_S:PSA_GenLang:Biographies_They    0.7676    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) Pr_T_HS Pr_H_S PSA_GnL Bgrp_T Pr_T_HS:PSA_GL
    ## Pronon_T_HS     0.054                                             
    ## Pronoun_H_S    -0.071 -0.066                                      
    ## PSA_GenLang    -0.221  0.071   0.056                              
    ## Bigrphs_Thy    -0.055 -0.024   0.023  0.105                       
    ## Pr_T_HS:PSA_GL  0.062 -0.312   0.042  0.058   0.053               
    ## Pr_H_S:PSA_GL   0.054  0.044  -0.179 -0.084  -0.013 -0.060        
    ## PSA_GnL:B_T     0.093  0.053  -0.012 -0.063  -0.251 -0.024        
    ## Pr_T_HS:B_T    -0.020 -0.045   0.014  0.053   0.058  0.071        
    ## Prn_H_S:B_T     0.022  0.019  -0.071 -0.013  -0.084 -0.010        
    ## P_T_HS:PSA_GL:  0.047  0.073  -0.010 -0.024   0.071 -0.048        
    ## P_H_S:PSA_GL:  -0.011 -0.008   0.124  0.025   0.061  0.018        
    ##                Pr_H_S:PSA_GL PSA_GL: P_T_HS:B P_H_S:B P_T_HS:PSA_GL:
    ## Pronon_T_HS                                                         
    ## Pronoun_H_S                                                         
    ## PSA_GenLang                                                         
    ## Bigrphs_Thy                                                         
    ## Pr_T_HS:PSA_GL                                                      
    ## Pr_H_S:PSA_GL                                                       
    ## PSA_GnL:B_T     0.025                                               
    ## Pr_T_HS:B_T    -0.009         0.071                                 
    ## Prn_H_S:B_T     0.136         0.061  -0.060                         
    ## P_T_HS:PSA_GL:  0.018         0.058  -0.312    0.044                
    ## P_H_S:PSA_GL:  -0.076        -0.084   0.044   -0.194  -0.060

## Three-Way Interaction

The main model has Helmert coding for Pronoun and Effects coding (.5,
-.5) for PSA and Biographies. This means Pronoun (T vs HS) \* PSA \*
Biographies is testing the interaction between Pronoun and PSA across
both Biographies conditions.

Dummy coding Biographies with *they* biographies as 1 and *he/she*
biographies as 0 tests the interaction between Pronoun and PSA for just
the *he/she* Biographies:

``` r
exp2_d %<>% mutate(Biographies_HS0 = Biographies)
contrasts(exp2_d$Biographies_HS0) <- cbind("_T1" = c(0, 1))
contrasts(exp2_d$Biographies_HS0)
```

    ##       _T1
    ## HeShe   0
    ## They    1

``` r
exp2_m_prod_bio_HS0 <- glmer(
  formula = P_Acc ~ Pronoun * PSA * Biographies_HS0 + (1 | Name),
  data = exp2_d, family = binomial
)
summary(exp2_m_prod_bio_HS0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: P_Acc ~ Pronoun * PSA * Biographies_HS0 + (1 | Name)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3464.0   3545.3  -1719.0   3438.0     3827 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4309 -0.3672  0.3616  0.4550  3.0687 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Name   (Intercept) 0.007172 0.08469 
    ## Number of obs: 3840, groups:  Name, 12
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                  0.72475    0.07060  10.265
    ## Pronoun_T_HS                                 3.23575    0.13822  23.411
    ## Pronoun_H_S                                 -0.30307    0.17352  -1.747
    ## PSA_GenLang                                 -0.10602    0.13243  -0.801
    ## Biographies_HS0_T1                          -0.05933    0.09085  -0.653
    ## Pronoun_T_HS:PSA_GenLang                    -2.34842    0.27618  -8.503
    ## Pronoun_H_S:PSA_GenLang                      0.19845    0.33288   0.596
    ## Pronoun_T_HS:Biographies_HS0_T1             -0.16029    0.19079  -0.840
    ## Pronoun_H_S:Biographies_HS0_T1               0.08451    0.22697   0.372
    ## PSA_GenLang:Biographies_HS0_T1               0.42602    0.18169   2.345
    ## Pronoun_T_HS:PSA_GenLang:Biographies_HS0_T1  0.88220    0.38159   2.312
    ## Pronoun_H_S:PSA_GenLang:Biographies_HS0_T1   0.13412    0.45385   0.296
    ##                                             Pr(>|z|)    
    ## (Intercept)                                   <2e-16 ***
    ## Pronoun_T_HS                                  <2e-16 ***
    ## Pronoun_H_S                                   0.0807 .  
    ## PSA_GenLang                                   0.4234    
    ## Biographies_HS0_T1                            0.5137    
    ## Pronoun_T_HS:PSA_GenLang                      <2e-16 ***
    ## Pronoun_H_S:PSA_GenLang                       0.5511    
    ## Pronoun_T_HS:Biographies_HS0_T1               0.4008    
    ## Pronoun_H_S:Biographies_HS0_T1                0.7096    
    ## PSA_GenLang:Biographies_HS0_T1                0.0190 *  
    ## Pronoun_T_HS:PSA_GenLang:Biographies_HS0_T1   0.0208 *  
    ## Pronoun_H_S:PSA_GenLang:Biographies_HS0_T1    0.7676    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) Pr_T_HS Pr_H_S PSA_GnL B_HS0_ Pr_T_HS:PSA_GL
    ## Pronon_T_HS     0.074                                             
    ## Pronoun_H_S    -0.093 -0.075                                      
    ## PSA_GenLang    -0.314  0.017   0.066                              
    ## Bgrp_HS0_T1    -0.684 -0.056   0.071  0.244                       
    ## Pr_T_HS:PSA_GL  0.015 -0.367   0.050  0.078  -0.012               
    ## Pr_H_S:PSA_GL   0.065  0.050  -0.294 -0.102  -0.050 -0.073        
    ## P_T_HS:B_HS    -0.052 -0.722   0.049 -0.012   0.058  0.265        
    ## P_H_S:B_HS0     0.070  0.055  -0.705 -0.051  -0.084 -0.037        
    ## PSA_GL:B_HS     0.229 -0.012  -0.049 -0.729  -0.251 -0.057        
    ## P_T_HS:PSA_GL: -0.011  0.266  -0.036 -0.056   0.071 -0.724        
    ## P_H_S:PSA_GL:  -0.047 -0.036   0.215  0.074   0.061  0.054        
    ##                Pr_H_S:PSA_GL P_T_HS:B P_H_S:B PSA_GL: P_T_HS:PSA_GL:
    ## Pronon_T_HS                                                         
    ## Pronoun_H_S                                                         
    ## PSA_GenLang                                                         
    ## Bgrp_HS0_T1                                                         
    ## Pr_T_HS:PSA_GL                                                      
    ## Pr_H_S:PSA_GL                                                       
    ## P_T_HS:B_HS    -0.036                                               
    ## P_H_S:B_HS0     0.225        -0.060                                 
    ## PSA_GL:B_HS     0.074         0.071    0.061                        
    ## P_T_HS:PSA_GL:  0.053        -0.312    0.044   0.058                
    ## P_H_S:PSA_GL:  -0.733         0.044   -0.194  -0.084  -0.060

Conversely, dummy coding Biographies with *he/she* biographies as 1 and
*they* biographies as 0 tests the interaction between Pronoun and PSA
for just the *they* Biographies.

``` r
exp2_d %<>% mutate(Biographies_T0 = Biographies)
contrasts(exp2_d$Biographies_T0) <- cbind("_HS1" = c(1, 0))
contrasts(exp2_d$Biographies_T0)
```

    ##       _HS1
    ## HeShe    1
    ## They     0

``` r
exp2_m_prod_bio_T0 <- glmer(
  formula = P_Acc ~ Pronoun * PSA * Biographies_T0 + (1 | Name),
  data = exp2_d, family = binomial
)
summary(exp2_m_prod_bio_T0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: P_Acc ~ Pronoun * PSA * Biographies_T0 + (1 | Name)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3464.0   3545.3  -1719.0   3438.0     3827 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4310 -0.3672  0.3616  0.4550  3.0688 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Name   (Intercept) 0.007173 0.08469 
    ## Number of obs: 3840, groups:  Name, 12
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                  0.66541    0.06685   9.954
    ## Pronoun_T_HS                                 3.07546    0.13207  23.286
    ## Pronoun_H_S                                 -0.21855    0.16160  -1.352
    ## PSA_GenLang                                  0.31999    0.12441   2.572
    ## Biographies_T0_HS1                           0.05934    0.09085   0.653
    ## Pronoun_T_HS:PSA_GenLang                    -1.46623    0.26337  -5.567
    ## Pronoun_H_S:PSA_GenLang                      0.33259    0.30870   1.077
    ## Pronoun_T_HS:Biographies_T0_HS1              0.16030    0.19081   0.840
    ## Pronoun_H_S:Biographies_T0_HS1              -0.08453    0.22703  -0.372
    ## PSA_GenLang:Biographies_T0_HS1              -0.42602    0.18172  -2.344
    ## Pronoun_T_HS:PSA_GenLang:Biographies_T0_HS1 -0.88224    0.38179  -2.311
    ## Pronoun_H_S:PSA_GenLang:Biographies_T0_HS1  -0.13417    0.45422  -0.295
    ##                                             Pr(>|z|)    
    ## (Intercept)                                  < 2e-16 ***
    ## Pronoun_T_HS                                 < 2e-16 ***
    ## Pronoun_H_S                                   0.1762    
    ## PSA_GenLang                                   0.0101 *  
    ## Biographies_T0_HS1                            0.5137    
    ## Pronoun_T_HS:PSA_GenLang                    2.59e-08 ***
    ## Pronoun_H_S:PSA_GenLang                       0.2813    
    ## Pronoun_T_HS:Biographies_T0_HS1               0.4008    
    ## Pronoun_H_S:Biographies_T0_HS1                0.7096    
    ## PSA_GenLang:Biographies_T0_HS1                0.0191 *  
    ## Pronoun_T_HS:PSA_GenLang:Biographies_T0_HS1   0.0208 *  
    ## Pronoun_H_S:PSA_GenLang:Biographies_T0_HS1    0.7677    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) Pr_T_HS Pr_H_S PSA_GnL B_T0_H Pr_T_HS:PSA_GL
    ## Pronon_T_HS     0.035                                             
    ## Pronoun_H_S    -0.058 -0.049                                      
    ## PSA_GenLang    -0.145  0.131   0.049                              
    ## Bgrp_T0_HS1    -0.637 -0.025   0.041  0.106                       
    ## Pr_T_HS:PSA_GL  0.122 -0.252   0.035  0.036  -0.090               
    ## Pr_H_S:PSA_GL   0.049  0.038  -0.061 -0.063  -0.035 -0.045        
    ## P_T_HS:B_T0    -0.024 -0.689   0.031 -0.090   0.058  0.174        
    ## P_H_S:B_T0_     0.040  0.029  -0.648 -0.035  -0.084 -0.025        
    ## PSA_GL:B_T0     0.099 -0.090  -0.034 -0.685  -0.251 -0.025        
    ## P_T_HS:PSA_GL: -0.084  0.172  -0.023 -0.025   0.071 -0.690        
    ## P_H_S:PSA_GL:  -0.033 -0.026   0.042  0.043   0.061  0.031        
    ##                Pr_H_S:PSA_GL P_T_HS:B P_H_S:B PSA_GL: P_T_HS:PSA_GL:
    ## Pronon_T_HS                                                         
    ## Pronoun_H_S                                                         
    ## PSA_GenLang                                                         
    ## Bgrp_T0_HS1                                                         
    ## Pr_T_HS:PSA_GL                                                      
    ## Pr_H_S:PSA_GL                                                       
    ## P_T_HS:B_T0    -0.026                                               
    ## P_H_S:B_T0_     0.042        -0.060                                 
    ## PSA_GL:B_T0     0.043         0.071    0.061                        
    ## P_T_HS:PSA_GL:  0.031        -0.312    0.044   0.058                
    ## P_H_S:PSA_GL:  -0.680         0.044   -0.194  -0.084  -0.060

The three models to compare:

``` r
exp2_r_prod_interaction <- bind_rows(
    .id = "model",
    "Across_Bio" = exp2_m_prod@model   %>% tidy(),
    "HeShe_Bio"  = exp2_m_prod_bio_HS0 %>% tidy(),
    "They_Bio"   = exp2_m_prod_bio_T0  %>% tidy()
  ) %>%
  select(model, term, estimate, p.value) %>%
  filter(
    term == "Pronoun_T_HS" |
    term == "PSA_GenLang" |
    term == "Pronoun_T_HS:PSA_GenLang"
  ) %>%
  mutate(
    estimate = round(estimate, 4),
    p.value  = round(p.value, 4)
  ) %>%
  arrange(term, model)

exp2_r_prod_interaction
```

    ## # A tibble: 9 × 4
    ##   model      term                     estimate p.value
    ##   <chr>      <chr>                       <dbl>   <dbl>
    ## 1 Across_Bio PSA_GenLang                 0.107  0.239 
    ## 2 HeShe_Bio  PSA_GenLang                -0.106  0.423 
    ## 3 They_Bio   PSA_GenLang                 0.32   0.0101
    ## 4 Across_Bio Pronoun_T_HS                3.16   0     
    ## 5 HeShe_Bio  Pronoun_T_HS                3.24   0     
    ## 6 They_Bio   Pronoun_T_HS                3.08   0     
    ## 7 Across_Bio Pronoun_T_HS:PSA_GenLang   -1.91   0     
    ## 8 HeShe_Bio  Pronoun_T_HS:PSA_GenLang   -2.35   0     
    ## 9 They_Bio   Pronoun_T_HS:PSA_GenLang   -1.47   0

The estimate for the PSA\*Pronoun interaction is -2.34 for the *he/she*
biographies and -1.47 for the *they* biographies, which means that the
pronoun PSA reduced the relative difficulty of they/them more when
paired with the *he/she* biographies than with *they* biographies.
Connecting to the barplot, the PSA-Neither difference is larger than
Both-Story difference.

## Producing they/them at least once

``` r
exp2_d %>%
  filter(P_Response == "they/them") %>%
  group_by(Condition) %>%
  summarise(
    n    = n_distinct(Participant),
    prop = n / 80
  )
```

    ## # A tibble: 4 × 3
    ##   Condition     n  prop
    ##   <fct>     <int> <dbl>
    ## 1 both         38 0.475
    ## 2 neither      21 0.262
    ## 3 psa          46 0.575
    ## 4 story        28 0.35

Model with whether each participant produced they/them at least once as
the outcome variable. Higher with the gendered language PSA, no effect
of Biographies, vaguely trending interaction.

``` r
exp2_d_they <- exp2_d %>%
  mutate(
    M_IsThey = ifelse(M_Response == "they/them", 1, 0),
    P_IsThey = ifelse(P_Response == "they/them", 1, 0)
  ) %>%
  group_by(Participant, Condition, PSA, Biographies) %>%
  summarise(
    M_Count = sum(M_IsThey),
    P_Count = sum(P_IsThey)
  ) %>%
  mutate(
    M_UseThey = ifelse(M_Count != 0, 1, 0),
    P_UseThey = ifelse(P_Count != 0, 1, 0)
  )

exp2_m_prod_useThey <- glm(
  formula = P_UseThey ~ PSA * Biographies,
  data = exp2_d_they, family = binomial
)
summary(exp2_m_prod_useThey)
```

    ## 
    ## Call:
    ## glm(formula = P_UseThey ~ PSA * Biographies, family = binomial, 
    ##     data = exp2_d_they)
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -0.362464   0.117471  -3.086  0.00203 ** 
    ## PSA_GenLang                   0.927126   0.234941   3.946 7.94e-05 ***
    ## Biographies_They              0.005806   0.234941   0.025  0.98029    
    ## PSA_GenLang:Biographies_They -0.816340   0.469882  -1.737  0.08233 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 434.46  on 319  degrees of freedom
    ## Residual deviance: 415.50  on 316  degrees of freedom
    ## AIC: 423.5
    ## 
    ## Number of Fisher Scoring iterations: 4

# Memory Predicting Production

## Descriptive Stats

``` r
exp2_d %>%
  group_by(Pronoun, PSA, Biographies, M_Acc) %>%
  summarise(m = mean(P_Acc))
```

    ## # A tibble: 24 × 5
    ## # Groups:   Pronoun, PSA, Biographies [12]
    ##    Pronoun PSA       Biographies M_Acc     m
    ##    <fct>   <fct>     <fct>       <int> <dbl>
    ##  1 he/him  Unrelated HeShe           0 0.879
    ##  2 he/him  Unrelated HeShe           1 0.927
    ##  3 he/him  Unrelated They            0 0.812
    ##  4 he/him  Unrelated They            1 0.892
    ##  5 he/him  Gender    HeShe           0 0.781
    ##  6 he/him  Gender    HeShe           1 0.816
    ##  7 he/him  Gender    They            0 0.738
    ##  8 he/him  Gender    They            1 0.867
    ##  9 she/her Unrelated HeShe           0 0.839
    ## 10 she/her Unrelated HeShe           1 0.894
    ## # ℹ 14 more rows

Combining the two measures, there are 4 possible patterns: getting both
right, getting both wrong, getting just memory right, and getting just
production right.

``` r
exp2_r_dist <- exp2_d %>%
  mutate(
    Combined_Accuracy = case_when(
      M_Acc == 1 & P_Acc == 1 ~ "Both right",
      M_Acc == 0 & P_Acc == 0 ~ "Both wrong",
      M_Acc == 1 & P_Acc == 0 ~ "Memory only",
      M_Acc == 0 & P_Acc == 1 ~ "Production only"
    )
  ) %>%
  group_by(Pronoun, PSA, Biographies, Combined_Accuracy) %>%
  summarise(n = n())

exp2_r_dist
```

    ## # A tibble: 48 × 5
    ## # Groups:   Pronoun, PSA, Biographies [12]
    ##    Pronoun PSA       Biographies Combined_Accuracy     n
    ##    <fct>   <fct>     <fct>       <chr>             <int>
    ##  1 he/him  Unrelated HeShe       Both right          243
    ##  2 he/him  Unrelated HeShe       Both wrong            7
    ##  3 he/him  Unrelated HeShe       Memory only          19
    ##  4 he/him  Unrelated HeShe       Production only      51
    ##  5 he/him  Unrelated They        Both right          224
    ##  6 he/him  Unrelated They        Both wrong           13
    ##  7 he/him  Unrelated They        Memory only          27
    ##  8 he/him  Unrelated They        Production only      56
    ##  9 he/him  Gender    HeShe       Both right          209
    ## 10 he/him  Gender    HeShe       Both wrong           14
    ## # ℹ 38 more rows

Production accuracy for they/them when memory was correct vs incorrect.

``` r
exp2_d %>%
  filter(Pronoun == "they/them") %>%
  group_by(Condition, M_Acc) %>%
  summarise(P_Acc = mean(P_Acc))
```

    ## # A tibble: 8 × 3
    ## # Groups:   Condition [4]
    ##   Condition M_Acc  P_Acc
    ##   <fct>     <int>  <dbl>
    ## 1 both          0 0.175 
    ## 2 both          1 0.443 
    ## 3 neither       0 0.0395
    ## 4 neither       1 0.167 
    ## 5 psa           0 0.182 
    ## 6 psa           1 0.441 
    ## 7 story         0 0.108 
    ## 8 story         1 0.130

## Model

``` r
exp2_d %<>% mutate(M_Acc_Factor = as.factor(M_Acc))

contrasts(exp2_d$M_Acc_Factor) <- cbind("_Wrong_Right" = c(-0.5, +0.5))
contrasts(exp2_d$M_Acc_Factor)
```

    ##   _Wrong_Right
    ## 0         -0.5
    ## 1          0.5

Maximal model has interactions between Pronoun (2 contrasts), Memory
Accuracy, PSA, and Biographies, then random intercepts by item.

``` r
exp2_m_mp <- buildmer(
  formula = P_Acc ~ Pronoun * PSA * Biographies * M_Acc_Factor +
    (Pronoun | Participant) + (Pronoun | Name),
  data = exp2_d, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: P_Acc ~ 1

    ## Currently evaluating LRT for: Pronoun, PSA, Biographies, M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun

    ## Fitting via glm: P_Acc ~ 1 + PSA

    ## Fitting via glm: P_Acc ~ 1 + Biographies

    ## Fitting via glm: P_Acc ~ 1 + M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun

    ## Currently evaluating LRT for: PSA, Biographies, M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + PSA

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor

    ## Currently evaluating LRT for: PSA, Biographies, Pronoun:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor +
    ##     Pronoun:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA

    ## Currently evaluating LRT for: Pronoun:PSA, Biographies,
    ##     Pronoun:M_Acc_Factor, PSA:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     PSA:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA

    ## Currently evaluating LRT for: Biographies, Pronoun:M_Acc_Factor,
    ##     PSA:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + PSA:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor

    ## Currently evaluating LRT for: Biographies, PSA:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + PSA:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + PSA:M_Acc_Factor

    ## Currently evaluating LRT for: Biographies, Pronoun:PSA:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA + Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:PSA:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:PSA:M_Acc_Factor

    ## Currently evaluating LRT for: Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies

    ## Currently evaluating LRT for: Pronoun:Biographies, PSA:Biographies,
    ##     Biographies:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + Pronoun:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + Biographies:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies

    ## Currently evaluating LRT for: Pronoun:Biographies,
    ##     Biographies:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     Pronoun:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     Biographies:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     Biographies:M_Acc_Factor

    ## Currently evaluating LRT for: Pronoun:Biographies,
    ##     PSA:Biographies:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + Pronoun:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + PSA:Biographies:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + PSA:Biographies:M_Acc_Factor

    ## Currently evaluating LRT for: Pronoun:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies

    ## Currently evaluating LRT for: Pronoun:PSA:Biographies,
    ##     Pronoun:Biographies:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:Biographies:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies

    ## Currently evaluating LRT for: Pronoun:Biographies:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:Biographies:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:Biographies:M_Acc_Factor

    ## Currently evaluating LRT for: Pronoun:PSA:Biographies:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:M_Acc_Factor:Biographies +
    ##     Pronoun:PSA:Biographies:M_Acc_Factor

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:M_Acc_Factor:Biographies +
    ##     Pronoun:PSA:Biographies:M_Acc_Factor

    ## Fitting via glm: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA
    ##     + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:M_Acc_Factor:Biographies +
    ##     Pronoun:PSA:Biographies:M_Acc_Factor

    ## Currently evaluating LRT for: 1 | Participant, 1 | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:M_Acc_Factor:Biographies +
    ##     Pronoun:M_Acc_Factor:PSA:Biographies + (1 | Participant)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:M_Acc_Factor:Biographies +
    ##     Pronoun:M_Acc_Factor:PSA:Biographies + (1 | Name)

    ## Updating formula: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:M_Acc_Factor:Biographies +
    ##     Pronoun:M_Acc_Factor:PSA:Biographies + (1 | Name)

    ## Currently evaluating LRT for: 1 | Participant, Pronoun | Name

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:M_Acc_Factor:Biographies +
    ##     Pronoun:M_Acc_Factor:PSA:Biographies + (1 | Name) + (1 |
    ##     Participant)

    ## Fitting via glmer, with ML: P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA +
    ##     Pronoun:PSA + Pronoun:M_Acc_Factor + M_Acc_Factor:PSA +
    ##     Pronoun:M_Acc_Factor:PSA + Biographies + PSA:Biographies +
    ##     M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies +
    ##     Pronoun:M_Acc_Factor:Biographies +
    ##     Pronoun:M_Acc_Factor:PSA:Biographies + (1 + Pronoun | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: Optimizer reports not having finished (4)
    ##     Singular fit

``` r
summary(exp2_m_mp)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## P_Acc ~ 1 + Pronoun + M_Acc_Factor + PSA + Pronoun:PSA + Pronoun:M_Acc_Factor +  
    ##     M_Acc_Factor:PSA + Pronoun:M_Acc_Factor:PSA + Biographies +  
    ##     PSA:Biographies + M_Acc_Factor:Biographies + M_Acc_Factor:PSA:Biographies +  
    ##     Pronoun:Biographies + Pronoun:PSA:Biographies + Pronoun:M_Acc_Factor:Biographies +  
    ##     Pronoun:M_Acc_Factor:PSA:Biographies + (1 | Name)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3384.0   3540.3  -1667.0   3334.0     3815 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6071 -0.4348  0.3462  0.4641  4.9870 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Name   (Intercept) 0.003593 0.05995 
    ## Number of obs: 3840, groups:  Name, 12
    ## 
    ## Fixed effects:
    ##                                                                    Estimate
    ## (Intercept)                                                         0.52197
    ## Pronoun_T_HS                                                        3.13031
    ## Pronoun_H_S                                                        -0.34168
    ## M_Acc_Factor_Wrong_Right                                            0.83020
    ## PSA_GenLang                                                         0.07450
    ## Biographies_They                                                   -0.01688
    ## Pronoun_T_HS:PSA_GenLang                                           -1.89636
    ## Pronoun_H_S:PSA_GenLang                                             0.11965
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right                              -0.40185
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right                                0.25505
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang                                0.23978
    ## PSA_GenLang:Biographies_They                                        0.37744
    ## M_Acc_Factor_Wrong_Right:Biographies_They                          -0.11428
    ## Pronoun_T_HS:Biographies_They                                      -0.30400
    ## Pronoun_H_S:Biographies_They                                        0.17425
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang                  -0.23687
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang                    0.48030
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They               0.42522
    ## Pronoun_T_HS:PSA_GenLang:Biographies_They                           1.19830
    ## Pronoun_H_S:PSA_GenLang:Biographies_They                            0.44550
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:Biographies_They              0.83018
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:Biographies_They              -0.39024
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They -1.52736
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They  -1.20271
    ##                                                                    Std. Error
    ## (Intercept)                                                           0.05470
    ## Pronoun_T_HS                                                          0.10973
    ## Pronoun_H_S                                                           0.13348
    ## M_Acc_Factor_Wrong_Right                                              0.10374
    ## PSA_GenLang                                                           0.10367
    ## Biographies_They                                                      0.10366
    ## Pronoun_T_HS:PSA_GenLang                                              0.21855
    ## Pronoun_H_S:PSA_GenLang                                               0.25801
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right                                 0.21858
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right                                  0.25853
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang                                  0.20736
    ## PSA_GenLang:Biographies_They                                          0.20735
    ## M_Acc_Factor_Wrong_Right:Biographies_They                             0.20739
    ## Pronoun_T_HS:Biographies_They                                         0.21861
    ## Pronoun_H_S:Biographies_They                                          0.25806
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang                     0.43706
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang                      0.51601
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They                 0.41475
    ## Pronoun_T_HS:PSA_GenLang:Biographies_They                             0.43732
    ## Pronoun_H_S:PSA_GenLang:Biographies_They                              0.51627
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:Biographies_They                0.43705
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:Biographies_They                 0.51610
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They    0.87560
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They     1.03296
    ##                                                                     z value
    ## (Intercept)                                                         9.54194
    ## Pronoun_T_HS                                                       28.52659
    ## Pronoun_H_S                                                        -2.55973
    ## M_Acc_Factor_Wrong_Right                                            8.00232
    ## PSA_GenLang                                                         0.71867
    ## Biographies_They                                                   -0.16279
    ## Pronoun_T_HS:PSA_GenLang                                           -8.67711
    ## Pronoun_H_S:PSA_GenLang                                             0.46375
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right                              -1.83842
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right                                0.98655
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang                                1.15634
    ## PSA_GenLang:Biographies_They                                        1.82034
    ## M_Acc_Factor_Wrong_Right:Biographies_They                          -0.55103
    ## Pronoun_T_HS:Biographies_They                                      -1.39060
    ## Pronoun_H_S:Biographies_They                                        0.67524
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang                  -0.54195
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang                    0.93079
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They               1.02524
    ## Pronoun_T_HS:PSA_GenLang:Biographies_They                           2.74009
    ## Pronoun_H_S:PSA_GenLang:Biographies_They                            0.86293
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:Biographies_They              1.89948
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:Biographies_They              -0.75614
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They -1.74435
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They  -1.16433
    ##                                                                    Pr(>|z|)
    ## (Intercept)                                                           0.000
    ## Pronoun_T_HS                                                          0.000
    ## Pronoun_H_S                                                           0.010
    ## M_Acc_Factor_Wrong_Right                                              0.000
    ## PSA_GenLang                                                           0.472
    ## Biographies_They                                                      0.871
    ## Pronoun_T_HS:PSA_GenLang                                              0.000
    ## Pronoun_H_S:PSA_GenLang                                               0.643
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right                                 0.066
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right                                  0.324
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang                                  0.248
    ## PSA_GenLang:Biographies_They                                          0.069
    ## M_Acc_Factor_Wrong_Right:Biographies_They                             0.582
    ## Pronoun_T_HS:Biographies_They                                         0.164
    ## Pronoun_H_S:Biographies_They                                          0.500
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang                     0.588
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang                      0.352
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They                 0.305
    ## Pronoun_T_HS:PSA_GenLang:Biographies_They                             0.006
    ## Pronoun_H_S:PSA_GenLang:Biographies_They                              0.388
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:Biographies_They                0.058
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:Biographies_They                 0.450
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They    0.081
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They     0.244
    ##                                                                    Pr(>|t|)    
    ## (Intercept)                                                         < 2e-16 ***
    ## Pronoun_T_HS                                                        < 2e-16 ***
    ## Pronoun_H_S                                                         0.01048 *  
    ## M_Acc_Factor_Wrong_Right                                           1.22e-15 ***
    ## PSA_GenLang                                                         0.47235    
    ## Biographies_They                                                    0.87069    
    ## Pronoun_T_HS:PSA_GenLang                                            < 2e-16 ***
    ## Pronoun_H_S:PSA_GenLang                                             0.64283    
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right                               0.06600 .  
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right                                0.32386    
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang                                0.24754    
    ## PSA_GenLang:Biographies_They                                        0.06871 .  
    ## M_Acc_Factor_Wrong_Right:Biographies_They                           0.58161    
    ## Pronoun_T_HS:Biographies_They                                       0.16435    
    ## Pronoun_H_S:Biographies_They                                        0.49952    
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang                   0.58785    
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang                    0.35196    
    ## M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They               0.30525    
    ## Pronoun_T_HS:PSA_GenLang:Biographies_They                           0.00614 ** 
    ## Pronoun_H_S:PSA_GenLang:Biographies_They                            0.38818    
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:Biographies_They              0.05750 .  
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:Biographies_They               0.44957    
    ## Pronoun_T_HS:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They  0.08110 .  
    ## Pronoun_H_S:M_Acc_Factor_Wrong_Right:PSA_GenLang:Biographies_They   0.24429    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 24 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

# Compare Pet Questions

Compare pet accuracy to pronoun accuracy. Pronoun (renamed to Character
Pronoun here for clarity) stays contrast coded as before and Question
Type (M_Type = pet or pronoun) is mean-center effects coded, comparing
pet questions to pronoun questions.

``` r
# mean and sd for pets
exp2_d_all %>%
  filter(M_Type == "pet") %>%
  pull(M_Acc) %>%
  mean()
```

    ## [1] 0.5395833

``` r
exp2_d_all %>%
  filter(M_Type == "pet") %>%
  pull(M_Acc) %>%
  sd()
```

    ## [1] 0.4984956

``` r
# subset data
exp2_d_pronounsPets <- exp2_d_all %>%
  filter(M_Type == "pet" | M_Type == "pronoun") %>%
  rename("CharPronoun" = "Pronoun")

# check other contrasts are still in df
contrasts(exp2_d_pronounsPets$CharPronoun)
```

    ##           _T_HS _H_S
    ## he/him     0.33 -0.5
    ## she/her    0.33  0.5
    ## they/them -0.66  0.0

``` r
contrasts(exp2_d_pronounsPets$PSA)
```

    ##           _GenLang
    ## Unrelated     -0.5
    ## Gender         0.5

``` r
contrasts(exp2_d_pronounsPets$Biographies)
```

    ##       _They
    ## HeShe  -0.5
    ## They    0.5

``` r
# mean-center effects code question type
exp2_d_pronounsPets$M_Type %<>% droplevels()
contrasts(exp2_d_pronounsPets$M_Type) <- cbind(
  "=Pet_Pronoun" = c(-.5, .5)
)
contrasts(exp2_d_pronounsPets$M_Type)
```

    ##         =Pet_Pronoun
    ## pet             -0.5
    ## pronoun          0.5

``` r
exp2_m_pet <- buildmer(
  formula = M_Acc ~ CharPronoun * PSA * Biographies + # conditions
    M_Type + # add question type
    CharPronoun * M_Type + # but only its interaction with Pronoun
    (M_Type * CharPronoun | Participant) +
    (M_Type * CharPronoun | Name),
  data = exp2_d_pronounsPets, family = binomial,
  buildmerControl(direction = "order")
)
```

    ## Determining predictor order

    ## Fitting via glm: M_Acc ~ 1

    ## Currently evaluating LRT for: CharPronoun, PSA, Biographies, M_Type

    ## Fitting via glm: M_Acc ~ 1 + CharPronoun

    ## Fitting via glm: M_Acc ~ 1 + PSA

    ## Fitting via glm: M_Acc ~ 1 + Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type

    ## Updating formula: M_Acc ~ 1 + M_Type

    ## Currently evaluating LRT for: CharPronoun, PSA, Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun

    ## Fitting via glm: M_Acc ~ 1 + M_Type + PSA

    ## Fitting via glm: M_Acc ~ 1 + M_Type + Biographies

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun

    ## Currently evaluating LRT for: PSA, Biographies, CharPronoun:M_Type

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + PSA

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + CharPronoun:M_Type

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + CharPronoun:M_Type

    ## Currently evaluating LRT for: PSA, Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + Biographies

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA

    ## Currently evaluating LRT for: CharPronoun:PSA, Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + CharPronoun:PSA

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies

    ## Currently evaluating LRT for: CharPronoun:PSA, CharPronoun:Biographies,
    ##     PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + PSA:Biographies

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA

    ## Currently evaluating LRT for: CharPronoun:Biographies, PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + CharPronoun:Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies

    ## Currently evaluating LRT for: CharPronoun:Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies +
    ##     CharPronoun:Biographies

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies +
    ##     CharPronoun:Biographies

    ## Currently evaluating LRT for: CharPronoun:PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies +
    ##     CharPronoun:Biographies + CharPronoun:PSA:Biographies

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies +
    ##     CharPronoun:Biographies + CharPronoun:PSA:Biographies

    ## Fitting via glm: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies +
    ##     CharPronoun:Biographies + CharPronoun:PSA:Biographies

    ## Currently evaluating LRT for: 1 | Participant, 1 | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 | Participant)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 | Name)

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies +
    ##     CharPronoun:Biographies + CharPronoun:PSA:Biographies + (1 | Name)

    ## Currently evaluating LRT for: 1 | Participant, M_Type | Name,
    ##     CharPronoun | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 | Name) + (1 | Participant)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 + M_Type | Name)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 + CharPronoun | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies +
    ##     CharPronoun:Biographies + CharPronoun:PSA:Biographies + (1 + M_Type
    ##     | Name)

    ## Currently evaluating LRT for: 1 | Participant, CharPronoun | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 + M_Type | Name) + (1 |
    ##     Participant)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 + M_Type + CharPronoun | Name)

    ## boundary (singular) fit: see help('isSingular')

    ## Updating formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun
    ##     + PSA + Biographies + CharPronoun:PSA + PSA:Biographies +
    ##     CharPronoun:Biographies + CharPronoun:PSA:Biographies + (1 + M_Type
    ##     | Name) + (1 | Participant)

    ## Currently evaluating LRT for: M_Type | Participant, CharPronoun |
    ##     Participant, CharPronoun | Name

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 + M_Type | Name) + (1 + M_Type |
    ##     Participant)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 + M_Type | Name) + (1 +
    ##     CharPronoun | Participant)

    ## Fitting via glmer, with ML: M_Acc ~ 1 + M_Type + CharPronoun +
    ##     M_Type:CharPronoun + PSA + Biographies + CharPronoun:PSA +
    ##     PSA:Biographies + CharPronoun:Biographies +
    ##     CharPronoun:PSA:Biographies + (1 + M_Type + CharPronoun | Name) +
    ##     (1 | Participant)

    ## boundary (singular) fit: see help('isSingular')

    ## Ending the ordering procedure due to having reached the maximal
    ##     feasible model - all higher models failed to converge. The types of
    ##     convergence failure are: lme4 reports not having converged (-1)
    ##     Singular fit

``` r
summary(exp2_m_pet)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ 1 + M_Type + CharPronoun + M_Type:CharPronoun + PSA +  
    ##     Biographies + CharPronoun:PSA + PSA:Biographies + CharPronoun:Biographies +  
    ##     CharPronoun:PSA:Biographies + (1 + M_Type | Name) + (1 |      Participant)
    ##    Data: exp2_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8533.8   8665.7  -4247.9   8495.8     7661 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.8329 -0.7378  0.2709  0.6234  2.8278 
    ## 
    ## Random effects:
    ##  Groups      Name               Variance Std.Dev. Corr
    ##  Participant (Intercept)        1.88265  1.3721       
    ##  Name        (Intercept)        0.01135  0.1065       
    ##              M_Type=Pet_Pronoun 0.01470  0.1212   0.10
    ## Number of obs: 7680, groups:  Participant, 320; Name, 12
    ## 
    ## Fixed effects:
    ##                                                Estimate Std. Error   z value
    ## (Intercept)                                    0.837832   0.089130  9.400095
    ## M_Type=Pet_Pronoun                             1.033577   0.066289 15.591902
    ## CharPronoun_T_HS                               0.752469   0.058264 12.914730
    ## CharPronoun_H_S                                0.069688   0.086660  0.804163
    ## PSA_GenLang                                    0.142134   0.165163  0.860568
    ## Biographies_They                              -0.123343   0.165169 -0.746764
    ## M_Type=Pet_Pronoun:CharPronoun_T_HS            1.495172   0.116433 12.841460
    ## M_Type=Pet_Pronoun:CharPronoun_H_S             0.189143   0.154982  1.220421
    ## CharPronoun_T_HS:PSA_GenLang                  -0.246769   0.115566 -2.135300
    ## CharPronoun_H_S:PSA_GenLang                   -0.025878   0.137937 -0.187606
    ## PSA_GenLang:Biographies_They                   0.166312   0.330313  0.503498
    ## CharPronoun_T_HS:Biographies_They             -0.010959   0.115657 -0.094753
    ## CharPronoun_H_S:Biographies_They              -0.007882   0.137987 -0.057124
    ## CharPronoun_T_HS:PSA_GenLang:Biographies_They  0.083940   0.231164  0.363121
    ## CharPronoun_H_S:PSA_GenLang:Biographies_They   0.356864   0.275909  1.293414
    ##                                               Pr(>|z|) Pr(>|t|)    
    ## (Intercept)                                      0.000   <2e-16 ***
    ## M_Type=Pet_Pronoun                               0.000   <2e-16 ***
    ## CharPronoun_T_HS                                 0.000   <2e-16 ***
    ## CharPronoun_H_S                                  0.421   0.4213    
    ## PSA_GenLang                                      0.389   0.3895    
    ## Biographies_They                                 0.455   0.4552    
    ## M_Type=Pet_Pronoun:CharPronoun_T_HS              0.000   <2e-16 ***
    ## M_Type=Pet_Pronoun:CharPronoun_H_S               0.222   0.2223    
    ## CharPronoun_T_HS:PSA_GenLang                     0.033   0.0327 *  
    ## CharPronoun_H_S:PSA_GenLang                      0.851   0.8512    
    ## PSA_GenLang:Biographies_They                     0.615   0.6146    
    ## CharPronoun_T_HS:Biographies_They                0.925   0.9245    
    ## CharPronoun_H_S:Biographies_They                 0.954   0.9544    
    ## CharPronoun_T_HS:PSA_GenLang:Biographies_They    0.717   0.7165    
    ## CharPronoun_H_S:PSA_GenLang:Biographies_They     0.196   0.1959    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 15 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
exp2_d_pronounsPets %<>% mutate(CharPronoun_They0 = ifelse(
  CharPronoun == "they/them", 0, 1
))

exp2_m_pet_they <- glmer( # same as buildmer results, just swap CharPronoun
  formula = M_Acc ~ M_Type + CharPronoun_They0 + M_Type:CharPronoun_They0 +
    PSA + Biographies + CharPronoun_They0:PSA + PSA:Biographies +
    CharPronoun_They0:Biographies +
    CharPronoun_They0:PSA:Biographies +
    (1 + M_Type | Name) + (1 | Participant),
  data = exp2_d_pronounsPets, family = binomial
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0192113 (tol = 0.002, component 1)

``` r
summary(exp2_m_pet_they)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ M_Type + CharPronoun_They0 + M_Type:CharPronoun_They0 +  
    ##     PSA + Biographies + CharPronoun_They0:PSA + PSA:Biographies +  
    ##     CharPronoun_They0:Biographies + CharPronoun_They0:PSA:Biographies +  
    ##     (1 + M_Type | Name) + (1 | Participant)
    ##    Data: exp2_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8527.3   8624.5  -4249.6   8499.3     7666 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.9118 -0.7393  0.2711  0.6229  2.7359 
    ## 
    ## Random effects:
    ##  Groups      Name               Variance Std.Dev. Corr
    ##  Participant (Intercept)        1.88004  1.3711       
    ##  Name        (Intercept)        0.01196  0.1094       
    ##              M_Type=Pet_Pronoun 0.02013  0.1419   0.19
    ## Number of obs: 7680, groups:  Participant, 320; Name, 12
    ## 
    ## Fixed effects:
    ##                                                 Estimate Std. Error z value
    ## (Intercept)                                     0.340614   0.095841   3.554
    ## M_Type=Pet_Pronoun                              0.047406   0.099491   0.476
    ## CharPronoun_They0                               0.744860   0.057665  12.917
    ## PSA_GenLang                                     0.304928   0.179862   1.695
    ## Biographies_They                               -0.117291   0.179891  -0.652
    ## M_Type=Pet_Pronoun:CharPronoun_They0            1.477859   0.115242  12.824
    ## CharPronoun_They0:PSA_GenLang                  -0.242827   0.114389  -2.123
    ## PSA_GenLang:Biographies_They                    0.109200   0.359754   0.304
    ## CharPronoun_They0:Biographies_They             -0.009991   0.114483  -0.087
    ## CharPronoun_They0:PSA_GenLang:Biographies_They  0.080498   0.228814   0.352
    ##                                                Pr(>|z|)    
    ## (Intercept)                                    0.000379 ***
    ## M_Type=Pet_Pronoun                             0.633729    
    ## CharPronoun_They0                               < 2e-16 ***
    ## PSA_GenLang                                    0.090011 .  
    ## Biographies_They                               0.514394    
    ## M_Type=Pet_Pronoun:CharPronoun_They0            < 2e-16 ***
    ## CharPronoun_They0:PSA_GenLang                  0.033770 *  
    ## PSA_GenLang:Biographies_They                   0.761478    
    ## CharPronoun_They0:Biographies_They             0.930456    
    ## CharPronoun_They0:PSA_GenLang:Biographies_They 0.724982    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##               (Intr) M_Ty=P_P ChP_T0 PSA_GnL Bgrp_T M_T=P_P: ChP_T0:PSA_GL
    ## M_Typ=Pt_Pr    0.026                                                      
    ## ChrPrnn_Th0   -0.363  0.000                                               
    ## PSA_GenLang    0.007  0.001   -0.004                                      
    ## Bigrphs_Thy   -0.003  0.000   -0.001  0.008                               
    ## M_T=P_P:CP_    0.009 -0.717    0.072  0.002  -0.001                       
    ## ChP_T0:PSA_GL -0.006 -0.001    0.010 -0.398  -0.009 -0.002                
    ## PSA_GnL:B_T    0.009  0.000   -0.008 -0.002   0.006  0.002    0.000       
    ## ChrP_T0:B_T    0.000  0.000   -0.003 -0.009  -0.398 -0.004    0.024       
    ## CP_T0:PSA_GL: -0.008  0.000    0.024  0.000  -0.006 -0.002   -0.003       
    ##               PSA_GL: CP_T0:B
    ## M_Typ=Pt_Pr                  
    ## ChrPrnn_Th0                  
    ## PSA_GenLang                  
    ## Bigrphs_Thy                  
    ## M_T=P_P:CP_                  
    ## ChP_T0:PSA_GL                
    ## PSA_GnL:B_T                  
    ## ChrP_T0:B_T   -0.006         
    ## CP_T0:PSA_GL: -0.398   0.013 
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0192113 (tol = 0.002, component 1)

``` r
exp2_d_pronounsPets %<>% mutate(CharPronoun_HeShe0 = ifelse(
  CharPronoun == "they/them", 1, 0
))

exp2_m_pet_heshe0 <- glmer( # same as buildmer results, just swap CharPronoun
  formula = M_Acc ~ M_Type + CharPronoun_HeShe0 + M_Type:CharPronoun_HeShe0 +
    PSA + Biographies + CharPronoun_HeShe0:PSA + PSA:Biographies +
    CharPronoun_HeShe0:Biographies +
    CharPronoun_HeShe0:PSA:Biographies +
    (1 + M_Type | Name) + (1 | Participant),
  data = exp2_d_pronounsPets, family = binomial
)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00586654 (tol = 0.002, component 1)

``` r
summary(exp2_m_pet_heshe0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: M_Acc ~ M_Type + CharPronoun_HeShe0 + M_Type:CharPronoun_HeShe0 +  
    ##     PSA + Biographies + CharPronoun_HeShe0:PSA + PSA:Biographies +  
    ##     CharPronoun_HeShe0:Biographies + CharPronoun_HeShe0:PSA:Biographies +  
    ##     (1 + M_Type | Name) + (1 | Participant)
    ##    Data: exp2_d_pronounsPets
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8527.3   8624.5  -4249.6   8499.3     7666 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.9129 -0.7393  0.2711  0.6230  2.7362 
    ## 
    ## Random effects:
    ##  Groups      Name               Variance Std.Dev. Corr
    ##  Participant (Intercept)        1.88185  1.3718       
    ##  Name        (Intercept)        0.01198  0.1095       
    ##              M_Type=Pet_Pronoun 0.02023  0.1422   0.19
    ## Number of obs: 7680, groups:  Participant, 320; Name, 12
    ## 
    ## Fixed effects:
    ##                                                 Estimate Std. Error z value
    ## (Intercept)                                      1.08519    0.09220  11.770
    ## M_Type=Pet_Pronoun                               1.52543    0.08215  18.569
    ## CharPronoun_HeShe0                              -0.74420    0.05766 -12.906
    ## PSA_GenLang                                      0.06099    0.17051   0.358
    ## Biographies_They                                -0.12744    0.17053  -0.747
    ## M_Type=Pet_Pronoun:CharPronoun_HeShe0           -1.47885    0.11524 -12.833
    ## CharPronoun_HeShe0:PSA_GenLang                   0.24424    0.11439   2.135
    ## PSA_GenLang:Biographies_They                     0.19100    0.34103   0.560
    ## CharPronoun_HeShe0:Biographies_They              0.01106    0.11448   0.097
    ## CharPronoun_HeShe0:PSA_GenLang:Biographies_They -0.08021    0.22881  -0.351
    ##                                                 Pr(>|z|)    
    ## (Intercept)                                       <2e-16 ***
    ## M_Type=Pet_Pronoun                                <2e-16 ***
    ## CharPronoun_HeShe0                                <2e-16 ***
    ## PSA_GenLang                                       0.7206    
    ## Biographies_They                                  0.4549    
    ## M_Type=Pet_Pronoun:CharPronoun_HeShe0             <2e-16 ***
    ## CharPronoun_HeShe0:PSA_GenLang                    0.0327 *  
    ## PSA_GenLang:Biographies_They                      0.5754    
    ## CharPronoun_HeShe0:Biographies_They               0.9230    
    ## CharPronoun_HeShe0:PSA_GenLang:Biographies_They   0.7259    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) M_Ty=P_P ChP_HS0 PSA_GnL Bgrp_T M_T=P_P: ChP_HS0:PSA_GL
    ## M_Typ=Pt_Pr     0.110                                                        
    ## ChrPrnn_HS0    -0.248 -0.101                                                 
    ## PSA_GenLang     0.005  0.001   -0.002                                        
    ## Bigrphs_Thy    -0.006 -0.005    0.004   0.007                                
    ## M_T=P_P:CP_    -0.054 -0.535    0.072  -0.001   0.004                        
    ## ChP_HS0:PSA_GL  0.000  0.004    0.010  -0.251  -0.006 -0.002                 
    ## PSA_GnL:B_T     0.009  0.001   -0.007  -0.004   0.004 -0.001    0.003        
    ## ChP_HS0:B_T     0.003  0.005   -0.003  -0.006  -0.251 -0.004    0.024        
    ## CP_HS0:PSA_GL: -0.007  0.002    0.024   0.002  -0.002 -0.002   -0.003        
    ##                PSA_GL: CP_HS0:B
    ## M_Typ=Pt_Pr                    
    ## ChrPrnn_HS0                    
    ## PSA_GenLang                    
    ## Bigrphs_Thy                    
    ## M_T=P_P:CP_                    
    ## ChP_HS0:PSA_GL                 
    ## PSA_GnL:B_T                    
    ## ChP_HS0:B_T    -0.002          
    ## CP_HS0:PSA_GL: -0.251   0.013  
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.00586654 (tol = 0.002, component 1)

# Compare to Exp1

``` r
bind_rows(
  read.csv("data/exp1a_data.csv", stringsAsFactors = TRUE) %>%
    filter(M_Type == "pronoun") %>%
    group_by(Pronoun) %>%
    summarise(
      mean = mean(M_Acc) %>% round(2),
      sd   = sd(M_Acc)   %>% round(2),
      exp  = 1
    ),
  exp2_d %>%
    filter(PSA == 0 & Biographies == 0) %>%
    group_by(Pronoun) %>%
    summarise(
      mean = mean(M_Acc) %>% round(2),
      sd   = sd(M_Acc)   %>% round(2),
      exp  = 2
    )
)
```

    ## # A tibble: 3 × 4
    ##   Pronoun    mean    sd   exp
    ##   <fct>     <dbl> <dbl> <dbl>
    ## 1 he/him     0.76  0.43     1
    ## 2 she/her    0.77  0.42     1
    ## 3 they/them  0.44  0.5      1

``` r
bind_rows(
  read.csv("data/exp1a_data.csv", stringsAsFactors = TRUE) %>%
    filter(!is.na(P_Acc)) %>%
    group_by(Pronoun) %>%
    summarise(
      mean = mean(P_Acc) %>% round(2),
      sd   = sd(P_Acc)   %>% round(2),
      exp  = 1
    ),
  exp2_d %>%
    filter(PSA == 0 & Biographies == 0) %>%
    group_by(Pronoun) %>%
    summarise(
      mean = mean(P_Acc) %>% round(2),
      sd   = sd(P_Acc)   %>% round(2),
      exp  = 2
    )
  )
```

    ## # A tibble: 3 × 4
    ##   Pronoun    mean    sd   exp
    ##   <fct>     <dbl> <dbl> <dbl>
    ## 1 he/him     0.83  0.38     1
    ## 2 she/her    0.86  0.35     1
    ## 3 they/them  0.29  0.46     1

Accuracy for unrelated job question:

``` r
bind_rows(
  read.csv("data/exp1a_data.csv", stringsAsFactors = TRUE) %>%
    filter(M_Type == "job") %>%
    summarise(
      mean = mean(M_Acc) %>% round(2),
      sd   = sd(M_Acc)   %>% round(2),
      exp  = 1
    ),
  exp2_d_all %>%
    filter(M_Type == "job") %>%
    filter(PSA == 0 & Biographies == 0) %>%
    summarise(
      mean = mean(M_Acc) %>% round(2),
      sd   = sd(M_Acc)   %>% round(2),
      exp  = 2
    )
  )
```

    ##   mean   sd exp
    ## 1 0.21 0.41   1
    ## 2  NaN   NA   2

And for comparison pet question:

``` r
bind_rows(
  read.csv("data/exp1a_data.csv", stringsAsFactors = TRUE) %>%
    filter(M_Type == "pet") %>%
    summarise(
      mean = mean(M_Acc) %>% round(2),
      sd   = sd(M_Acc) %>% round(2),
      exp  = 1
    ),
  exp2_d_all %>%
    filter(M_Type == "pet") %>%
    filter(PSA == "Gender" & Biographies == "They") %>%
    summarise(
      mean = mean(M_Acc) %>% round(2),
      sd   = sd(M_Acc) %>% round(2),
      exp  = 2
    )
  )
```

    ##   mean   sd exp
    ## 1 0.41 0.49   1
    ## 2 0.56 0.50   2
