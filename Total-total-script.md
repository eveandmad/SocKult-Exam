META ANALYSIS

``` r
#Loading data
meta_matrix <- read_csv("Meta-analysis matrix done.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Specification = col_character(),
    ##   Title = col_character(),
    ##   Authors = col_character(),
    ##   Area = col_character(),
    ##   Article = col_character(),
    ##   GROUP_SPECIFICATION = col_character(),
    ##   EDUCATION_SPECIFICATION = col_character(),
    ##   TASK = col_character(),
    ##   CULTURAL_CONSISTENCY_OF_STIMULI = col_character(),
    ##   DUR_DISTRACTOR_TASK_MIN = col_character(),
    ##   BEHAVIORAL_DATA_ANALYSIS_METHOD = col_character(),
    ##   ANOVA_SD = col_logical(),
    ##   NOTE_FOR_ESSTIMATES = col_character(),
    ##   REG_TO_MEAN_CONTROL = col_character(),
    ##   AUTHOR_CONTACT_DETAILS = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
#Remove empty rows in bottom
meta_matrix <- meta_matrix[1:12,]

#Exclude studies not included: Study 2, 11 and 12
meta_matrix <- meta_matrix[-2,]
meta_matrix <- meta_matrix[-(10:11),]

#Calculating total healthy sample size 
total_sample <- sum(meta_matrix$HC_SAMPLE_SIZE)

#Mean and standard deviation of age
meta_matrix %>% 
  get_summary_stats(AGE_M,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n  mean    sd
    ##   <chr>    <dbl> <dbl> <dbl>
    ## 1 AGE_M        9  23.6  5.72

``` r
#Mean and standard deviation of education
meta_matrix %>% 
  get_summary_stats(EDUCATION_M,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable        n  mean    sd
    ##   <chr>       <dbl> <dbl> <dbl>
    ## 1 EDUCATION_M     4  13.4 0.888

``` r
#Gender distribution 
(100/total_sample)*sum(meta_matrix$FEMALE)
```

    ## [1] 54.7619

``` r
#Median number of stimuli 
median(meta_matrix$NUMBER_OF_STIMULI,na.rm = T) 
```

    ## [1] 153

``` r
#Loading previously cleaned data
simonsen_19 <- read_csv("Simonsen_clean.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   ID = col_double(),
    ##   FaceID = col_double(),
    ##   FirstRating = col_double(),
    ##   GroupRating = col_double(),
    ##   Feedback = col_double(),
    ##   SecondRating = col_double(),
    ##   Change = col_double(),
    ##   Condition = col_character()
    ## )

``` r
#Estimates for meta-analysis (mean and standard deviation of change for low, same and high group)
subset(simonsen_19, Feedback %in% c(-3,-2)) %>% get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n   mean    sd
    ##   <chr>    <dbl>  <dbl> <dbl>
    ## 1 Change    2029 -0.408  1.53

``` r
subset(simonsen_19, Feedback %in% c(0)) %>% get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n   mean    sd
    ##   <chr>    <dbl>  <dbl> <dbl>
    ## 1 Change    2230 -0.081  1.60

``` r
subset(simonsen_19, Feedback %in% c(3,2)) %>% get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n  mean    sd
    ##   <chr>    <dbl> <dbl> <dbl>
    ## 1 Change    1861 0.458  1.53

``` r
#Loading previously cleaned data
cogsci_18 <- read_csv("cogsci_clean.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   ID = col_double(),
    ##   FaceID = col_double(),
    ##   FirstRating = col_double(),
    ##   GroupRating = col_double(),
    ##   SecondRating = col_double(),
    ##   Feedback = col_double(),
    ##   Change = col_double(),
    ##   Condition = col_character()
    ## )

``` r
#Estimates for meta-analysis (mean and standard deviation of change for low, same and high group)
subset(cogsci_18, Feedback %in% c(-3,-2)) %>% get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n   mean    sd
    ##   <chr>    <dbl>  <dbl> <dbl>
    ## 1 Change    1156 -0.548  1.46

``` r
subset(cogsci_18, Feedback %in% c(0)) %>% get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n   mean    sd
    ##   <chr>    <dbl>  <dbl> <dbl>
    ## 1 Change     793 -0.221  1.49

``` r
subset(cogsci_18, Feedback %in% c(3,2)) %>% get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n  mean    sd
    ##   <chr>    <dbl> <dbl> <dbl>
    ## 1 Change    1131  0.08  1.43

``` r
#Loading data with estimates
meta <- read_csv("MA analysis data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Paper = col_double(),
    ##   Article = col_character(),
    ##   Change = col_double(),
    ##   ChangeSD = col_double(),
    ##   Feedback = col_double()
    ## )

``` r
#Exclude article column for imputation and save it separately
article <- meta %>% select(Article)
meta2 <- meta %>% select(-Article)

#Impute missing data 
imp <- mice(meta2, m = 100, print = F)
#Aggreting lists into dataframe
imp1 <- complete(imp,action = "long")
#Aggregating imputed measures into mean
meta <- aggregate(imp1[,3:6] , by = list(imp1$.id),FUN = mean)

#Insert article column again 
meta <- cbind(meta, article)
```

``` r
#Defining model
meta_model <- 
  bf(Change | se(ChangeSD) ~ 1 + Feedback + (1 + Feedback | Article))

#Get priors
get_prior(formula = meta_model,
          data = meta,
          family = gaussian())
```

    ##                 prior     class      coef   group resp dpar nlpar bound
    ## 1                             b                                        
    ## 2                             b  Feedback                              
    ## 3              lkj(1)       cor                                        
    ## 4                           cor           Article                      
    ## 5 student_t(3, 0, 10) Intercept                                        
    ## 6 student_t(3, 0, 10)        sd                                        
    ## 7                            sd           Article                      
    ## 8                            sd  Feedback Article                      
    ## 9                            sd Intercept Article

``` r
#Define priors 
prior = c(
   prior(normal(0,0.25), class = b),
   prior(lkj(5), class = cor),
   prior(normal(0,0.5), class = Intercept),
   prior(cauchy(0,0.1), class = sd))

#Prior model
meta_model_prior <- brm(
  formula = meta_model,
  data = meta,
  family = gaussian(),
  prior = prior,
  sample_prior = "only",
  iter = 6000,
  chains = 4,
  cores = 2,
  control = list(adapt_delta = .99)
  )
```

    ## Compiling the C++ model

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -D_REENTRANT  -DBOOST_DISABLE_ASSERTS -DBOOST_PENDING_INTEGER_LOG2_HPP -include stan/math/prim/mat/fun/Eigen.hpp   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
#Prior predictive check (SM figure 1)
(plot_prior <- pp_check(meta_model_prior,nsamples = 100)+ggtitle("Prior predictive check")+xlab("Change in rating"))
```

![](Total-total-script_files/figure-markdown_github/-%20modelling%20(meta-analysis)-1.png)

``` r
#Posterior model
meta_model_post <- brm(
  formula = meta_model,
  data = meta,
  family = gaussian(),
  prior = prior,
  sample_prior = T,
  iter = 6000,
  chains = 4,
  cores = 2,
  control = list(adapt_delta = .999)
  )
```

    ## Compiling the C++ model
    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -D_REENTRANT  -DBOOST_DISABLE_ASSERTS -DBOOST_PENDING_INTEGER_LOG2_HPP -include stan/math/prim/mat/fun/Eigen.hpp   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
#Posterior predictive check (SM figure 1)
(plot_post <- pp_check(meta_model_post,nsamples = 100)+ggtitle("Posterior predictive check")+xlab("Change in rating"))
```

![](Total-total-script_files/figure-markdown_github/-%20modelling%20(meta-analysis)-2.png)

``` r
#Model summary (SM table 3)
summary(meta_model_post)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: Change | se(ChangeSD) ~ 1 + Feedback + (1 + Feedback | Article) 
    ##    Data: meta (Number of observations: 25) 
    ## Samples: 4 chains, each with iter = 6000; warmup = 3000; thin = 1;
    ##          total post-warmup samples = 12000
    ## 
    ## Group-Level Effects: 
    ## ~Article (Number of levels: 9) 
    ##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               0.05      0.04     0.00     0.16 1.00     6206
    ## sd(Feedback)                0.03      0.03     0.00     0.09 1.00     5884
    ## cor(Intercept,Feedback)    -0.01      0.30    -0.58     0.57 1.00    13064
    ##                         Tail_ESS
    ## sd(Intercept)               5599
    ## sd(Feedback)                5731
    ## cor(Intercept,Feedback)     8310
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.04      0.05    -0.06     0.12 1.00     8216     6510
    ## Feedback      0.17      0.02     0.11     0.21 1.00     7274     5877
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
posterior_summary(meta_model_post, pars = c("^b_", "^sd_"), probs = c(0.025, 0.975))
```

    ##                         Estimate  Est.Error          Q2.5      Q97.5
    ## b_Intercept           0.03600756 0.04567511 -0.0601999331 0.12194495
    ## b_Feedback            0.16536707 0.02359988  0.1146066997 0.20943346
    ## sd_Article__Intercept 0.04679095 0.04273840  0.0015041900 0.15688705
    ## sd_Article__Feedback  0.02683732 0.02550221  0.0008633997 0.09396903

``` r
#Hypothesis testing 
hypothesis(meta_model_post, "Feedback > 0")
```

    ## Hypothesis Tests for class b:
    ##       Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (Feedback) > 0     0.17      0.02     0.13      0.2       2999         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#Subset of unpublished data collection where Feedback is NA
in_class_no_feedback <- subset(cogsci_18, is.na(Feedback))

#Model for regression to the mean estimate
m_in_class <- lmer(SecondRating ~ 1 + FirstRating + (1 | ID), 
          in_class_no_feedback, REML=F)

#Extract estimate of regression to the mean
est_in_class <- as.data.frame(fixef(m_in_class))
est_in_class[2,]
```

    ## [1] 0.6665238

``` r
#Model quality 

#Combo plot (histogram + traceplot) (SM figure 2)
meta_model_post %>% plot(combo = c("hist", "trace"), theme = theme_bw(base_size = 18), main = "Posterior distribution and trace plots" )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20MA-1.png)

``` r
#Overlay plot (SM figure 3)
mcmc_rank <- mcmc_rank_overlay(meta_model_post,
                  pars = c("b_Intercept", "b_Feedback", 
           "sd_Article__Intercept", "sd_Article__Feedback",
           "cor_Article__Intercept__Feedback")) + theme_classic()

#Forest plot (SM figure 4)
#Creating dataframe with beta estimates for each article
#study.draws_1 <- spread_draws(meta_model_post, r_Article[Article,], b_Feedback) %>% 
  #mutate(b_Feedback = r_Article + b_Feedback)
#study.draws_1$Article <- as.factor(study.draws_1$Article) 

# #Creating variable containing the pooled effect across studies
# pooled.effect.draws_1 <- spread_draws(meta_model_post, b_Feedback) %>% 
#   mutate(Article = "Pooled Effect")
# 
# #Binding individual and pooled estimates
# forest.data_1 <- bind_rows(study.draws_1, pooled.effect.draws_1) %>% 
#    ungroup() %>%
#    mutate(Article = reorder(Article, b_Feedback))
# 
# #Summarizing estimates (mean and quantiles)
# forest.data.summary_1 <- group_by(forest.data_1, Article) %>% 
#   mean_qi(b_Feedback)
# 
# #Plot code
# ggplot(aes(b_Feedback, relevel(Article, "Pooled Effect", after = Inf)), 
#        data = forest.data_1) +
#   geom_vline(xintercept = fixef(meta_model_post)[2, 1], color = "grey", size = 1) +
#   geom_vline(xintercept = fixef(meta_model_post)[2, 3:4], color = "grey", linetype = 2) +
#   geom_vline(xintercept = 0, color = "black", size = 1) +
#   geom_density_ridges(fill = "blue", rel_min_height = 0.01, col = NA, scale = 1,
#                       alpha = 0.8) +
#   geom_pointintervalh(data = forest.data.summary_1, size = 1) +
#   geom_text(data = mutate_if(forest.data.summary_1, is.numeric, round, 2),
#     aes(label = glue("{b_Feedback} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
#   labs(x = "Change",
#        y = element_blank()) +
#   theme_minimal()

#Hypothesis plot (figure 2)
(hypothesis_plot <- plot(hypothesis(meta_model_post, "Feedback > 0"))[[1]]+ggtitle("Effect of group opinion on second rating"))
```

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20MA-2.png)![](Total-total-script_files/figure-markdown_github/-%20visualizations%20MA-3.png)

COMPARATIVE ANALYSIS

``` r
#Loading previously cleaned data
sc_df2 <- read_csv("sc_df_clean.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   FaceID = col_double(),
    ##   ID = col_character(),
    ##   Participant = col_double(),
    ##   FirstRating = col_double(),
    ##   GroupRating = col_double(),
    ##   SecondRating = col_double(),
    ##   Feedback = col_double(),
    ##   Change = col_double(),
    ##   Condition = col_character(),
    ##   TimeStamp1 = col_datetime(format = ""),
    ##   TimeStamp2 = col_datetime(format = "")
    ## )

``` r
#Loading data including demographic variables 
demo_data <- read_csv("Demographic responses.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   ID = col_character(),
    ##   Age = col_double(),
    ##   `Country of upbringing` = col_character(),
    ##   `Country of residence` = col_character(),
    ##   Gender = col_character(),
    ##   Education_years = col_double()
    ## )

``` r
#Merge with data included in analysis
demo_data <- merge(demo_data,sc_df2,by = "ID")

#Include only one row per participant 
demo_data <- demo_data[ !duplicated(demo_data$ID), ]

#Mean and standard deviation of age
demo_data %>% 
  get_summary_stats(Age,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n  mean    sd
    ##   <chr>    <dbl> <dbl> <dbl>
    ## 1 Age         38  26.9  7.46

``` r
#Mean and standard deviation of education
demo_data %>% 
  get_summary_stats(Education_years,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable            n  mean    sd
    ##   <chr>           <dbl> <dbl> <dbl>
    ## 1 Education_years    38  16.1  2.22

``` r
#Gender distribution 
females <- sum(demo_data$Gender=="Female")
males <- sum(demo_data$Gender=="Male")
(100/38)*females
```

    ## [1] 63.15789

``` r
(100/38)*males
```

    ## [1] 36.84211

``` r
#Distribution of country of upbringing
denmark <- sum(demo_data$`Country of upbringing`=="Denmark")
not_denmark <- sum(demo_data$`Country of upbringing`!="Denmark")
(100/38)*denmark
```

    ## [1] 89.47368

``` r
(100/38)*not_denmark
```

    ## [1] 10.52632

``` r
#Distribution of country of residence
denmark2 <- sum(demo_data$`Country of residence`=="Denmark")
not_denmark2 <- sum(demo_data$`Country of residence`!="Denmark")
(100/38)*denmark2
```

    ## [1] 84.21053

``` r
(100/38)*not_denmark2
```

    ## [1] 15.78947

``` r
#Getting ready for rbind
sc_df <- sc_df2 %>% select(-c(TimeStamp1, TimeStamp2, Participant))

#Combining data for comparative analysis into one dataframe
all_d <- rbind(simonsen_19, sc_df, cogsci_18)

#Counting ID's for sample size
alldID <- as.data.frame(unique(all_d$ID))
count(alldID)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   121

``` r
#Creating column with feedback according to type
allLow <- subset(all_d, Feedback %in% c(-3,-2))
allLow$Feedback2 <- -2.5
allSame <- subset(all_d, Feedback %in% c(-1, 0, 1))
allSame$Feedback2 <- 0
allHigh <- subset(all_d, Feedback %in% c(3,2))
allHigh$Feedback2 <- 2.5
all_d <- rbind(allLow,allSame,allHigh)
```

``` r
#Defining formula
Combined_f1 <- bf(Change ~ 0 + FirstRating:Condition + Feedback:Condition + 
                         (1 + FirstRating + Feedback | ID) + 
                         (1 + FirstRating + Feedback | FaceID))

#Get priors 
get_prior(Combined_f1, all_d, family = gaussian)
```

    ##                  prior class                      coef  group resp dpar
    ## 1                          b                                           
    ## 2                          b    ConditionPeri:Feedback                 
    ## 3                          b     ConditionPre:Feedback                 
    ## 4                          b FirstRating:ConditionPeri                 
    ## 5                          b  FirstRating:ConditionPre                 
    ## 6               lkj(1)   cor                                           
    ## 7                        cor                           FaceID          
    ## 8                        cor                               ID          
    ## 9  student_t(3, 0, 10)    sd                                           
    ## 10                        sd                           FaceID          
    ## 11                        sd                  Feedback FaceID          
    ## 12                        sd               FirstRating FaceID          
    ## 13                        sd                 Intercept FaceID          
    ## 14                        sd                               ID          
    ## 15                        sd                  Feedback     ID          
    ## 16                        sd               FirstRating     ID          
    ## 17                        sd                 Intercept     ID          
    ## 18 student_t(3, 0, 10) sigma                                           
    ##    nlpar bound
    ## 1             
    ## 2             
    ## 3             
    ## 4             
    ## 5             
    ## 6             
    ## 7             
    ## 8             
    ## 9             
    ## 10            
    ## 11            
    ## 12            
    ## 13            
    ## 14            
    ## 15            
    ## 16            
    ## 17            
    ## 18

``` r
#Define priors
Combined_prior <- c(
  prior(normal(0, .3), class = b, coef = ConditionPeri:Feedback),
  prior(normal(0, .3), class = b, coef = ConditionPre:Feedback),
  prior(normal(0, .5), class = b, coef = FirstRating:ConditionPeri),
  prior(normal(0, .5), class = b, coef = FirstRating:ConditionPre),
  prior(normal(0, .3), class = sd),
  prior(lkj(5), class = cor),
  prior(normal(0,1), class = sigma)
)

#Prior model
Combined_m_prior <- brm(
  Combined_f1,
  all_d,
  family = gaussian,
  prior = Combined_prior,
  sample_prior = "only",
  chains = 2,
  cores = 2,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 20
  )
)
```

    ## Compiling the C++ model

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -D_REENTRANT  -DBOOST_DISABLE_ASSERTS -DBOOST_PENDING_INTEGER_LOG2_HPP -include stan/math/prim/mat/fun/Eigen.hpp   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
#Prior predictive check (SM figure 6)
prior_check <- pp_check(Combined_m_prior, nsamples=100)
(prior_check
  +ggtitle("Prior predictive check"))
```

![](Total-total-script_files/figure-markdown_github/-%20modelling%20comparative-1.png)

``` r
#Posterior model
Combined_m <- brm(
  Combined_f1,
  all_d,
  family = gaussian,
  prior = Combined_prior,
  sample_prior = T,
  chains = 4,
  cores = 2,
  iter = 4000,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 20
  )
)
```

    ## Compiling the C++ model
    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -D_REENTRANT  -DBOOST_DISABLE_ASSERTS -DBOOST_PENDING_INTEGER_LOG2_HPP -include stan/math/prim/mat/fun/Eigen.hpp   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
#Posterior predictive check (SM figure 6)
post_check <- pp_check(Combined_m, nsamples=100)
(post_check
  +ggtitle("Posterior predictive check"))
```

![](Total-total-script_files/figure-markdown_github/-%20modelling%20comparative-2.png)

``` r
#Obtaining estimate summaries of the posterior distribution (SM table 6)
summary(Combined_m)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: Change ~ 0 + FirstRating:Condition + Feedback:Condition + (1 + FirstRating + Feedback | ID) + (1 + FirstRating + Feedback | FaceID) 
    ##    Data: all_d (Number of observations: 11798) 
    ## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
    ##          total post-warmup samples = 8000
    ## 
    ## Group-Level Effects: 
    ## ~FaceID (Number of levels: 153) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                  0.54      0.06     0.43     0.66 1.00
    ## sd(FirstRating)                0.07      0.01     0.05     0.10 1.00
    ## sd(Feedback)                   0.02      0.01     0.00     0.04 1.00
    ## cor(Intercept,FirstRating)    -0.57      0.12    -0.75    -0.29 1.00
    ## cor(Intercept,Feedback)       -0.14      0.26    -0.61     0.42 1.00
    ## cor(FirstRating,Feedback)      0.16      0.27    -0.42     0.64 1.00
    ##                            Bulk_ESS Tail_ESS
    ## sd(Intercept)                  2711     4538
    ## sd(FirstRating)                1314     2135
    ## sd(Feedback)                   2174     3328
    ## cor(Intercept,FirstRating)     2189     3976
    ## cor(Intercept,Feedback)        8160     5470
    ## cor(FirstRating,Feedback)      6415     5857
    ## 
    ## ~ID (Number of levels: 121) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                  2.17      0.11     1.96     2.39 1.00
    ## sd(FirstRating)                0.32      0.03     0.27     0.37 1.00
    ## sd(Feedback)                   0.01      0.01     0.00     0.03 1.00
    ## cor(Intercept,FirstRating)    -0.94      0.01    -0.96    -0.91 1.00
    ## cor(Intercept,Feedback)       -0.14      0.29    -0.67     0.44 1.00
    ## cor(FirstRating,Feedback)      0.18      0.29    -0.41     0.70 1.00
    ##                            Bulk_ESS Tail_ESS
    ## sd(Intercept)                  1478     3207
    ## sd(FirstRating)                 975     2358
    ## sd(Feedback)                   2770     4244
    ## cor(Intercept,FirstRating)     1220     2808
    ## cor(Intercept,Feedback)        8169     6293
    ## cor(FirstRating,Feedback)      7187     6154
    ## 
    ## Population-Level Effects: 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat
    ## FirstRating:ConditionPeri    -0.20      0.03    -0.26    -0.13 1.01
    ## FirstRating:ConditionPre     -0.20      0.03    -0.26    -0.15 1.00
    ## ConditionPeri:Feedback        0.03      0.01     0.00     0.06 1.00
    ## ConditionPre:Feedback         0.03      0.01     0.01     0.05 1.00
    ##                           Bulk_ESS Tail_ESS
    ## FirstRating:ConditionPeri     1146     2056
    ## FirstRating:ConditionPre      1034     2117
    ## ConditionPeri:Feedback        8811     6029
    ## ConditionPre:Feedback         6754     4874
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     1.20      0.01     1.18     1.22 1.00     9968     5427
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
posterior_summary(Combined_m, pars = c("^b_", "^sd_"), probs = c(0.025, 0.975) )
```

    ##                                Estimate   Est.Error          Q2.5
    ## b_FirstRating:ConditionPeri -0.19558517 0.033149584 -0.2649441739
    ## b_FirstRating:ConditionPre  -0.20106086 0.029412212 -0.2623390234
    ## b_ConditionPeri:Feedback     0.03103545 0.014567845  0.0034176844
    ## b_ConditionPre:Feedback      0.02655480 0.009594202  0.0095324130
    ## sd_FaceID__Intercept         0.53837848 0.059332212  0.4251807965
    ## sd_FaceID__FirstRating       0.07172891 0.011959950  0.0476724696
    ## sd_FaceID__Feedback          0.01569446 0.010248254  0.0008509210
    ## sd_ID__Intercept             2.16798603 0.110041076  1.9611329035
    ## sd_ID__FirstRating           0.31951013 0.026951675  0.2678182960
    ## sd_ID__Feedback              0.01303213 0.009067138  0.0005722997
    ##                                   Q97.5
    ## b_FirstRating:ConditionPeri -0.13427127
    ## b_FirstRating:ConditionPre  -0.14610081
    ## b_ConditionPeri:Feedback     0.06098654
    ## b_ConditionPre:Feedback      0.04759936
    ## sd_FaceID__Intercept         0.65717528
    ## sd_FaceID__FirstRating       0.09510551
    ## sd_FaceID__Feedback          0.03741222
    ## sd_ID__Intercept             2.39276406
    ## sd_ID__FirstRating           0.37300856
    ## sd_ID__Feedback              0.03348316

``` r
#Hypothesis testing
hypothesis(Combined_m,"ConditionPeri:Feedback > ConditionPre:Feedback")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (ConditionPeri:Fe... > 0        0      0.01    -0.02     0.03       1.59
    ##   Post.Prob Star
    ## 1      0.61     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
hypothesis(Combined_m,"ConditionPre:Feedback > ConditionPeri:Feedback")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (ConditionPre:Fee... > 0        0      0.01    -0.03     0.02       0.63
    ##   Post.Prob Star
    ## 1      0.39     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
hypothesis(Combined_m,"ConditionPeri:Feedback > 0")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (ConditionPeri:Fe... > 0     0.03      0.01     0.01     0.06      65.12
    ##   Post.Prob Star
    ## 1      0.98    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
hypothesis(Combined_m,"ConditionPre:Feedback > 0")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (ConditionPre:Fee... > 0     0.03      0.01     0.01     0.04     665.67
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#Combo plot (histogram and trace plot) (SM figure 7)
Combined_m %>% plot(combo = c("hist", "trace"), theme = theme_bw(base_size = 18), main = "Posterior distribution and trace plots" )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20comparative-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20comparative-2.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20comparative-3.png)![](Total-total-script_files/figure-markdown_github/-%20visualizations%20comparative-4.png)

``` r
#Overlay plot (SM figure 8)
mcmc_rank_overlay(Combined_m,
                  pars = c("b_FirstRating:ConditionPeri", "b_FirstRating:ConditionPre", 
                           "b_ConditionPeri:Feedback","b_ConditionPre:Feedback",
                            "sd_FaceID__FirstRating", "sd_FaceID__Feedback",
                            "sd_ID__Feedback", "sd_ID__FirstRating")) + theme_classic()
```

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20comparative-5.png)

``` r
#Violin plot of re- and peri-COVID-19 estimates (uncorrected) with feedback -2.5, 0 and 2.5 (figure 3)
all_d$Feedback2 <- as.factor(all_d$Feedback2)
ggplot(all_d, aes(x=Feedback2, y=Change, color=Feedback2)) +
    geom_jitter(color="grey", size=0.2, alpha=0.2, na.rm = T) +
    geom_violin(position="dodge", width=1, fill="transparent") +
    geom_boxplot(width=0.3, alpha=.5, fill = "black") +
    scale_color_manual(values = wes_palette("GrandBudapest1", n = 3))+
    theme_classic() +
    theme(
      legend.position="bottom",
      plot.title = element_text(size=11)
    ) +
    ggtitle(expression(bold("Change in trustworthiness according to feedback")), subtitle = "Grouped by condition") +
    xlab("Feedback type")+
    facet_wrap(~Condition)
```

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20comparative-6.png)

``` r
#Meta-data including own study
MA_d <- read_csv("MA_d.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   Paper = col_double(),
    ##   Article = col_character(),
    ##   Change = col_double(),
    ##   ChangeSD = col_double(),
    ##   Feedback = col_double()
    ## )

``` r
#Updating meta-model to include own study 
meta_model_post_2 <- update(meta_model_post, formula = meta_model, newdata = MA_d)
```

    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '44a8c654f56268125828282c2b443901' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 7.3e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.73 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 6000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  600 / 6000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 6000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 1800 / 6000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 2400 / 6000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 3000 / 6000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 3001 / 6000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3600 / 6000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 4200 / 6000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 4800 / 6000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 5400 / 6000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 6000 / 6000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 3.46106 seconds (Warm-up)
    ## Chain 1:                4.5784 seconds (Sampling)
    ## Chain 1:                8.03946 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '44a8c654f56268125828282c2b443901' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 2.5e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.25 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 6000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  600 / 6000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 6000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 1800 / 6000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 2400 / 6000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 3000 / 6000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 3001 / 6000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3600 / 6000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 4200 / 6000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 4800 / 6000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 5400 / 6000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 6000 / 6000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 3.17643 seconds (Warm-up)
    ## Chain 2:                2.2119 seconds (Sampling)
    ## Chain 2:                5.38833 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '44a8c654f56268125828282c2b443901' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 2.4e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.24 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 6000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  600 / 6000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 6000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 1800 / 6000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 2400 / 6000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 3000 / 6000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 3001 / 6000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3600 / 6000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 4200 / 6000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 4800 / 6000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 5400 / 6000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 6000 / 6000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 3.91309 seconds (Warm-up)
    ## Chain 3:                3.71453 seconds (Sampling)
    ## Chain 3:                7.62763 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '44a8c654f56268125828282c2b443901' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 2.2e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.22 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 6000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  600 / 6000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 6000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 1800 / 6000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 2400 / 6000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 3000 / 6000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 3001 / 6000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3600 / 6000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 4200 / 6000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 4800 / 6000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 5400 / 6000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 6000 / 6000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 3.5073 seconds (Warm-up)
    ## Chain 4:                3.43504 seconds (Sampling)
    ## Chain 4:                6.94234 seconds (Total)
    ## Chain 4:

``` r
#Model summary 
posterior_summary(meta_model_post_2, pars = c("^b_", "^sd_"), probs = c(0.025, 0.975))
```

    ##                         Estimate  Est.Error          Q2.5      Q97.5
    ## b_Intercept           0.02970629 0.04357672 -0.0615436892 0.11336123
    ## b_Feedback            0.16676580 0.02429747  0.1150587742 0.21555818
    ## sd_Article__Intercept 0.04460913 0.04151829  0.0014266708 0.15176268
    ## sd_Article__Feedback  0.02735726 0.02636270  0.0008988945 0.09678998

``` r
summary(meta_model_post_2)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: Change | se(ChangeSD) ~ Feedback + (1 + Feedback | Article) 
    ##    Data: MA_d (Number of observations: 28) 
    ## Samples: 4 chains, each with iter = 6000; warmup = 3000; thin = 1;
    ##          total post-warmup samples = 12000
    ## 
    ## Group-Level Effects: 
    ## ~Article (Number of levels: 10) 
    ##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               0.04      0.04     0.00     0.15 1.00     5618
    ## sd(Feedback)                0.03      0.03     0.00     0.10 1.00     5193
    ## cor(Intercept,Feedback)    -0.01      0.30    -0.59     0.57 1.00    12913
    ##                         Tail_ESS
    ## sd(Intercept)               5255
    ## sd(Feedback)                5502
    ## cor(Intercept,Feedback)     8343
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.03      0.04    -0.06     0.11 1.00     6824     6068
    ## Feedback      0.17      0.02     0.12     0.22 1.00     6187     4571
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#FOREST PLOT 2 (SM figure 8)

# #Creating dataframe with beta estimates for each article
# study.draws_2 <- spread_draws(meta_model_post_2, r_Article[Article,], b_Feedback) %>% 
#   mutate(b_Feedback = r_Article + b_Feedback)
# study.draws_2$Article <- as.factor(study.draws_2$Article) 
# 
# #Creating variable containing the pooled effect across studies
# pooled.effect.draws_2 <- spread_draws(meta_model_post_2, b_Feedback) %>% 
#   mutate(Article = "Pooled Effect")
# 
# #Binding individual and pooled estimates
# forest.data_2 <- bind_rows(study.draws_2, pooled.effect.draws_2) %>% 
#    ungroup() %>%
#    mutate(Article = reorder(Article, b_Feedback))
# 
# #Summarizing estimates (mean and quantiles)
# forest.data.summary_2 <- group_by(forest.data_2, Article) %>% 
#   mean_qi(b_Feedback)
# 
# #Plot code
# ggplot(aes(b_Feedback, relevel(Article, "Pooled Effect", after = Inf)), 
#        data = forest.data_2) +
#   geom_vline(xintercept = fixef(meta_model_post_2)[2, 1], color = "grey", size = 1) +
#   geom_vline(xintercept = fixef(meta_model_post_2)[2, 3:4], color = "grey", linetype = 2) +
#   geom_vline(xintercept = 0, color = "black", size = 1) +
#   geom_density_ridges(fill = "blue", rel_min_height = 0.01, col = NA, scale = 1,
#                       alpha = 0.8) +
#   geom_pointintervalh(data = forest.data.summary_2, size = 1) +
#   geom_text(data = mutate_if(forest.data.summary_2, is.numeric, round, 2),
#     aes(label = glue("{b_Feedback} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
#   labs(x = "Change",
#        y = element_blank()) +
#   theme_minimal()
```

SIMULATION

``` r
#Define simulation function 
SimulateData <- function(samplesize,
                         Regression2Mean,
                         Conformity,
                         MinScore = 1,
                         MaxScore = 8){
  
  FirstRating <- round(runif(samplesize, MinScore, MaxScore), 0)
  Feedback <- round(runif(samplesize, -3, 3), 0)
  SecondRating <- round(Regression2Mean * FirstRating + Conformity * Feedback, 0)
  SecondRating <- ifelse(SecondRating > MaxScore, MaxScore,ifelse(SecondRating < MinScore, MinScore, SecondRating))
  Change <- SecondRating - FirstRating
    
  d1 <- data.frame(FirstRating, Feedback, SecondRating, Change) %>% 
    subset(FirstRating + Feedback < MaxScore & FirstRating + Feedback > MinScore) %>%
    mutate(
      FirstRatingC <- FirstRating - 4.5,
      SecondRatingC <- SecondRating - 4.5,
    )
  return(d1)
}

#Define Calculation Correlation function
CalculateCorrelation <- function(simulations = n, 
                                 samplesize,
                                 Regression2Mean,
                                 Conformity, 
                                 MinScore = 1, 
                                 MaxScore = 8){
  for (i in c(1:n)){
    
    d <- SimulateData(samplesize, Regression2Mean, Conformity, MinScore, MaxScore)
    
    if (i == 1) {
      RegressionEstimate <- cor(d$FirstRating, d$SecondRating)
      ConformityEstimate <- cor(d$Feedback, d$Change)
    } else {
      RegressionEstimate <- c(RegressionEstimate, cor(d$FirstRating, d$SecondRating))
      ConformityEstimate <- c(ConformityEstimate, cor(d$Feedback, d$Change))
    }
  }
  Estimates <- data.frame(
    ConformityEstimate = ifelse(is.na(ConformityEstimate),0,ConformityEstimate), 
    RegressionEstimate = RegressionEstimate, 
    RegressionTrue = Regression2Mean,
    ConformityTrue = Conformity
  )
  return(Estimates)
}

#Define fixed effects for simulations
set.seed <- 1981 
#Samplesize, number of simulations, max rating and min rating
samplesize <- 300 
n <- 1000 
MaxScore <- 8
MinScore <- 1

#Running simulation of fixed level of regression to the mean at 0.66 and different true conformity effects
ConformityEstimateC02 <- CalculateCorrelation(n, samplesize, 
                                              Regression2Mean = 0.66,
                                              Conformity = 0.02)
ConformityEstimateC05 <- CalculateCorrelation(n, samplesize, 
                                              Regression2Mean = 0.66,
                                              Conformity = -0.05)
ConformityEstimateC1 <- CalculateCorrelation(n, samplesize, 
                                              Regression2Mean = 0.66,
                                              Conformity = -0.1)
ConformityEstimateC15 <- CalculateCorrelation(n, samplesize, 
                                              Regression2Mean = 0.66,
                                              Conformity = -0.17)
ConformityEstimateC2 <- CalculateCorrelation(n, samplesize, 
                                              Regression2Mean = 0.66,
                                              Conformity = -0.2)
ConformityEstimateC25 <- CalculateCorrelation(n, samplesize, 
                                              Regression2Mean = 0.66,
                                              Conformity = -0.25)
#Binding estimates to one data-frame
ConformityEstimate <- rbind(ConformityEstimateC02,
                            ConformityEstimateC05,
                            ConformityEstimateC1,
                            ConformityEstimateC15,
                            ConformityEstimateC2,
                            ConformityEstimateC25)

#Obtain a table of means of different levels of conformity according to regression to the mean (first 5 rows of SM table 4)
Reference <- ConformityEstimate %>%
  group_by(RegressionTrue, ConformityTrue) %>%
  summarise(
    TrueRegression = mean(RegressionTrue),
    EstimatedRegression = mean(RegressionTrue),
    TrueConformity = mean(ConformityTrue),
    EstimatedConformity = mean(ConformityEstimate))

#Running simulation of fixed level of regression to the mean at 0.52 and different true conformity effects
C02 <- CalculateCorrelation(n, samplesize, 
                            Regression2Mean = 0.52,
                            Conformity = 0.02)
C05 <- CalculateCorrelation(n, samplesize, 
                            Regression2Mean = 0.52,
                            Conformity = -0.05)
C1 <- CalculateCorrelation(n, samplesize, 
                            Regression2Mean = 0.52,
                            Conformity = -0.1)
C17 <- CalculateCorrelation(n, samplesize, 
                            Regression2Mean = 0.52,
                            Conformity = -0.17)
C2 <- CalculateCorrelation(n, samplesize, 
                            Regression2Mean = 0.52,
                            Conformity = -0.2)
C25 <- CalculateCorrelation(n, samplesize, 
                            Regression2Mean = 0.52,
                            Conformity = -0.25)

#Binding estimates to one data-frame
C <- rbind(C02, C05, C1, C17, C2, C25)

#Obtain a table of means of different levels of conformity according to regression to the mean (last 5 rows of SM table 4)
Reference2 <- C %>%
  group_by(RegressionTrue, ConformityTrue) %>%
  summarise(
    TrueRegression = mean(RegressionTrue),
    EstimatedRegression = mean(RegressionTrue),
    TrueConformity = mean(ConformityTrue),
    EstimatedConformity = mean(ConformityEstimate))
```

INDIVIDUAL ANALYSIS OF NEW STUDY

``` r
#Creating subsets of feedback type
sc_df_low <- subset(sc_df2, Feedback %in% c(-3,-2))
sc_df_low$Feedback2 <- -2.5
sc_df_same <- subset(sc_df2, Feedback %in% c(-1, 0, 1))
sc_df_same$Feedback2 <- 0
sc_df_high <- subset(sc_df2, Feedback %in% c(3,2))
sc_df_high$Feedback2 <- 2.5

#Summary statistics of first and second rating for different types of feedback (SM table 5 - first 2 rows)
sc_df2 %>% 
  get_summary_stats(FirstRating,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable        n  mean    sd
    ##   <chr>       <dbl> <dbl> <dbl>
    ## 1 FirstRating  2971  4.90  1.83

``` r
sc_df2 %>% 
  get_summary_stats(SecondRating,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable         n  mean    sd
    ##   <chr>        <dbl> <dbl> <dbl>
    ## 1 SecondRating  2971  4.89  1.79

``` r
#Summary statistics of change for different types of feedback (SM table 5 - last 3 rows)
sc_df_low %>% 
  get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n   mean    sd
    ##   <chr>    <dbl>  <dbl> <dbl>
    ## 1 Change    1090 -0.412  1.49

``` r
sc_df_same %>% 
  get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n   mean    sd
    ##   <chr>    <dbl>  <dbl> <dbl>
    ## 1 Change     668 -0.076  1.78

``` r
sc_df_high %>% 
  get_summary_stats(Change,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable     n  mean    sd
    ##   <chr>    <dbl> <dbl> <dbl>
    ## 1 Change     840  0.52  1.74

``` r
#Creating dataframe excluding feedback of 0 
sc_df_lh <- rbind(sc_df_low,sc_df_high)

#Getting ditraction duration time 
#Create column with time differences 
sc_df2$time_dif <- sc_df2$TimeStamp2 - sc_df2$TimeStamp1

#Create column with time difference in hours
sc_df2$hours <- sc_df2$time_dif/60
#Mean and SD 
mean(sc_df2$hours)
```

    ## Time difference of 11.3554 mins

``` r
sd(sc_df2$hours)
```

    ## [1] 11.23982

``` r
#Model formula
SocConformity_f1 <- bf(Change ~ 1 + FirstRating + Feedback + 
                         (1 + FirstRating + Feedback | Participant) + 
                         (1 + FirstRating + Feedback | FaceID))

#Get priors 
get_prior(SocConformity_f1, 
          sc_df_lh, 
          family = gaussian)
```

    ##                  prior     class        coef       group resp dpar nlpar
    ## 1                              b                                        
    ## 2                              b    Feedback                            
    ## 3                              b FirstRating                            
    ## 4               lkj(1)       cor                                        
    ## 5                            cor                  FaceID                
    ## 6                            cor             Participant                
    ## 7  student_t(3, 0, 10) Intercept                                        
    ## 8  student_t(3, 0, 10)        sd                                        
    ## 9                             sd                  FaceID                
    ## 10                            sd    Feedback      FaceID                
    ## 11                            sd FirstRating      FaceID                
    ## 12                            sd   Intercept      FaceID                
    ## 13                            sd             Participant                
    ## 14                            sd    Feedback Participant                
    ## 15                            sd FirstRating Participant                
    ## 16                            sd   Intercept Participant                
    ## 17 student_t(3, 0, 10)     sigma                                        
    ##    bound
    ## 1       
    ## 2       
    ## 3       
    ## 4       
    ## 5       
    ## 6       
    ## 7       
    ## 8       
    ## 9       
    ## 10      
    ## 11      
    ## 12      
    ## 13      
    ## 14      
    ## 15      
    ## 16      
    ## 17

``` r
#Define priors
SocConformity_prior <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, .3), class = b),
  prior(normal(0, .3), class = sd),
  prior(lkj(5), class = cor),
  prior(normal(0,1), class = sigma)
)

#Prior model 
SocConformity_m_prior <- brm(
  SocConformity_f1,
  sc_df_lh,
  family = gaussian,
  prior = SocConformity_prior,
  sample_prior = "only",
  chains = 2,
  cores = 2,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 20
  )
)
```

    ## Compiling the C++ model

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -D_REENTRANT  -DBOOST_DISABLE_ASSERTS -DBOOST_PENDING_INTEGER_LOG2_HPP -include stan/math/prim/mat/fun/Eigen.hpp   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
#Prior predictive check (SM figure 9)
prior_check <- pp_check(SocConformity_m_prior, nsamples=100)
(prior_check
  +ggtitle("Prior predictive check"))
```

![](Total-total-script_files/figure-markdown_github/-%20modelling%20own%20study-1.png)

``` r
#Posterior model
SocConformity_m <- brm(
  SocConformity_f1,
  sc_df_lh,
  family = gaussian,
  prior = SocConformity_prior,
  sample_prior = T,
  chains = 4,
  cores = 2,
  iter = 4000,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 20
  )
)
```

    ## Compiling the C++ model
    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -D_REENTRANT  -DBOOST_DISABLE_ASSERTS -DBOOST_PENDING_INTEGER_LOG2_HPP -include stan/math/prim/mat/fun/Eigen.hpp   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
#Posterior predictive check (SM figure 9)
post_check <- pp_check(SocConformity_m, nsamples=100)
(post_check
  +ggtitle("Posterior predictive check"))
```

![](Total-total-script_files/figure-markdown_github/-%20modelling%20own%20study-2.png)

``` r
#Summary of posterior model (SM table 7)
posterior_summary(SocConformity_m, pars = c("^b_", "^sd_"), probs = c(0.025, 0.975) )
```

    ##                                Estimate  Est.Error         Q2.5
    ## b_Intercept                  2.73506917 0.20935504  2.325519079
    ## b_FirstRating               -0.56687766 0.03653603 -0.637482010
    ## b_Feedback                   0.02362473 0.01527962 -0.006004001
    ## sd_FaceID__Intercept         0.35352275 0.08447103  0.164997223
    ## sd_FaceID__FirstRating       0.03755155 0.02078905  0.002188311
    ## sd_FaceID__Feedback          0.03232940 0.01982095  0.001562444
    ## sd_Participant__Intercept    0.95344559 0.11177684  0.746269667
    ## sd_Participant__FirstRating  0.15928427 0.02497822  0.112667779
    ## sd_Participant__Feedback     0.02675949 0.01751410  0.001368540
    ##                                   Q97.5
    ## b_Intercept                  3.14046284
    ## b_FirstRating               -0.49297220
    ## b_Feedback                   0.05409968
    ## sd_FaceID__Intercept         0.49983097
    ## sd_FaceID__FirstRating       0.07895410
    ## sd_FaceID__Feedback          0.07367858
    ## sd_Participant__Intercept    1.18549970
    ## sd_Participant__FirstRating  0.21157120
    ## sd_Participant__Feedback     0.06520237

``` r
summary(SocConformity_m)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: Change ~ 1 + FirstRating + Feedback + (1 + FirstRating + Feedback | Participant) + (1 + FirstRating + Feedback | FaceID) 
    ##    Data: sc_df_lh (Number of observations: 1930) 
    ## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
    ##          total post-warmup samples = 8000
    ## 
    ## Group-Level Effects: 
    ## ~FaceID (Number of levels: 80) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                  0.35      0.08     0.16     0.50 1.00
    ## sd(FirstRating)                0.04      0.02     0.00     0.08 1.00
    ## sd(Feedback)                   0.03      0.02     0.00     0.07 1.00
    ## cor(Intercept,FirstRating)     0.04      0.27    -0.48     0.56 1.00
    ## cor(Intercept,Feedback)        0.07      0.26    -0.45     0.56 1.00
    ## cor(FirstRating,Feedback)      0.09      0.29    -0.48     0.62 1.00
    ##                            Bulk_ESS Tail_ESS
    ## sd(Intercept)                  1047      951
    ## sd(FirstRating)                 533      833
    ## sd(Feedback)                   2125     2919
    ## cor(Intercept,FirstRating)     5404     5469
    ## cor(Intercept,Feedback)        9261     6100
    ## cor(FirstRating,Feedback)      4662     5254
    ## 
    ## ~Participant (Number of levels: 37) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                  0.95      0.11     0.75     1.19 1.00
    ## sd(FirstRating)                0.16      0.02     0.11     0.21 1.00
    ## sd(Feedback)                   0.03      0.02     0.00     0.07 1.00
    ## cor(Intercept,FirstRating)    -0.59      0.12    -0.78    -0.33 1.00
    ## cor(Intercept,Feedback)        0.18      0.26    -0.38     0.64 1.00
    ## cor(FirstRating,Feedback)     -0.13      0.27    -0.62     0.42 1.00
    ##                            Bulk_ESS Tail_ESS
    ## sd(Intercept)                  5216     6036
    ## sd(FirstRating)                2345     3639
    ## sd(Feedback)                   2900     4375
    ## cor(Intercept,FirstRating)     2780     4336
    ## cor(Intercept,Feedback)       11871     6106
    ## cor(FirstRating,Feedback)     12247     5527
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       2.74      0.21     2.33     3.14 1.00     3624     6009
    ## FirstRating    -0.57      0.04    -0.64    -0.49 1.00     3250     4871
    ## Feedback        0.02      0.02    -0.01     0.05 1.00    12557     6324
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     1.19      0.02     1.15     1.23 1.00    12518     5671
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#Hypothesis testing
hypothesis(SocConformity_m,"Feedback > 0")
```

    ## Hypothesis Tests for class b:
    ##       Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (Feedback) > 0     0.02      0.02        0     0.05      16.35      0.94
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#Combo plot (SM figure 10)
SocConformity_m %>% plot(combo = c("hist", "trace"), theme = theme_bw(base_size = 18), main = "Posterior distribution and trace plots" )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20own%20study-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20own%20study-2.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20own%20study-3.png)![](Total-total-script_files/figure-markdown_github/-%20visualizations%20own%20study-4.png)

``` r
#Overlay plot (SM figure 11)
mcmc_rank_overlay(SocConformity_m,
                  pars = c("b_Intercept", "b_Feedback", 
           "sd_FaceID__Intercept", "sd_FaceID__Feedback",
           "sd_Participant__Intercept", "sd_Participant__Feedback")) + theme_classic()
```

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20own%20study-5.png)

``` r
#Spaghetti plot of participant level effects (SM figure 12)
#Predicting estimates for plotting
xx <- predict(SocConformity_m, summary=T)

#Include predicted estimates in dataframe
sc_df_lh <- cbind(sc_df_lh,xx)
sc_df_lh$Participant <- as.factor(sc_df_lh$Participant)

#Plot 
ggplot(sc_df_lh) + 
  geom_point(aes(Feedback,Change, color = ID, group=ID), show.legend=F) + 
  geom_smooth(method=lm, se=F, aes(Feedback,Change, color = ID), show.legend=F) +
  ggtitle("Regression of change on participant level")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20own%20study-6.png)

``` r
#Stacked participant level hypothesis plot (SM figure 12)
#Obtaining coefficients for each participant according to alternative hypothesis
X <- hypothesis(SocConformity_m, "Feedback > 0", group = "Participant", scope = "coef")

#Plot
X$hypothesis %>%
  left_join(distinct(sc_df_lh, Group = Participant)) %>% 
  mutate(Participant = factor(Group), Conformity = Estimate) %>%
  ggplot(aes(Conformity, Participant)) +
  geom_errorbarh(aes(xmin = CI.Lower, xmax = CI.Upper)) +
  geom_point() + theme_classic() + 
  ggtitle("Effect of feedback on participant level", subtitle="Social conformity estimates with 95%CI")
```

    ## Joining, by = "Group"

    ## Warning: Column `Group` joining character vector and factor, coercing into
    ## character vector

![](Total-total-script_files/figure-markdown_github/-%20visualizations%20own%20study-7.png)
