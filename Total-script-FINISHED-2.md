META ANALYSIS

``` r
#Loading data
meta_matrix <- read_csv("~/Desktop/SocKult exam/SocKult-Exam/Meta-analysis matrix done.csv")
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
simonsen_19 <- read_csv("~/Desktop/SocKult exam/SocKult-Exam/Simonsen_clean.csv")
```

    ## Parsed with column specification:
    ## cols(
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
cogsci_18 <- read_csv("~/Desktop/SocKult exam/SocKult-Exam/cogsci_clean.csv")
```

    ## Parsed with column specification:
    ## cols(
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
meta <- read_csv("~/Desktop/SocKult exam/SocKult-Exam/MA analysis data.csv")
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

    ## Start sampling

``` r
#Prior predictive check (SM figure 1)
(plot_prior <- pp_check(meta_model_prior,nsamples = 100)+ggtitle("Prior predictive check")+xlab("Change in rating"))
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20modelling%20(meta-analysis)%201-1.png)

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
    ## Start sampling

``` r
#Posterior predictive check (SM figure 1)
(plot_post <- pp_check(meta_model_post,nsamples = 100)+ggtitle("Posterior predictive check")+xlab("Change in rating"))
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20modelling%20(meta-analysis)%201-2.png)

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
    ## sd(Intercept)               0.05      0.04     0.00     0.16 1.00     6256
    ## sd(Feedback)                0.03      0.03     0.00     0.10 1.00     5573
    ## cor(Intercept,Feedback)    -0.01      0.30    -0.57     0.57 1.00    13700
    ##                         Tail_ESS
    ## sd(Intercept)               5159
    ## sd(Feedback)                5922
    ## cor(Intercept,Feedback)     8651
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.04      0.04    -0.05     0.12 1.00     8183     6530
    ## Feedback      0.17      0.02     0.11     0.21 1.00     6848     5053
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
posterior_summary(meta_model_post, pars = c("^b_", "^sd_"), probs = c(0.025, 0.975))
```

    ##                         Estimate  Est.Error          Q2.5      Q97.5
    ## b_Intercept           0.03695735 0.04390809 -0.0529168613 0.12312561
    ## b_Feedback            0.16515464 0.02466881  0.1128189602 0.21256859
    ## sd_Article__Intercept 0.04597772 0.04290598  0.0014621483 0.15757773
    ## sd_Article__Feedback  0.02740407 0.02610582  0.0008810801 0.09669435

``` r
#Hypothesis testing 
hypothesis(meta_model_post, "Feedback > 0")
```

    ## Hypothesis Tests for class b:
    ##       Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (Feedback) > 0     0.17      0.02     0.13      0.2       1999         1
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

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%201-1.png)

``` r
#Overlay plot (SM figure 3)
mcmc_rank <- mcmc_rank_overlay(meta_model_post,
                  pars = c("b_Intercept", "b_Feedback", 
           "sd_Article__Intercept", "sd_Article__Feedback",
           "cor_Article__Intercept__Feedback")) + theme_classic()

#Forest plot (SM figure 4)
#Creating dataframe with beta estimates for each article
study.draws_1 <- spread_draws(meta_model_post, r_Article[Article,], b_Feedback) %>% 
  mutate(b_Feedback = r_Article + b_Feedback)
study.draws_1$Article <- as.factor(study.draws_1$Article) 

#Creating variable containing the pooled effect across studies
pooled.effect.draws_1 <- spread_draws(meta_model_post, b_Feedback) %>% 
  mutate(Article = "Pooled Effect")

#Binding individual and pooled estimates
forest.data_1 <- bind_rows(study.draws_1, pooled.effect.draws_1) %>% 
   ungroup() %>%
   mutate(Article = reorder(Article, b_Feedback))
```

    ## Warning in bind_rows_(x, .id): binding factor and character vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

``` r
#Summarizing estimates (mean and quantiles)
forest.data.summary_1 <- group_by(forest.data_1, Article) %>% 
  mean_qi(b_Feedback)

#Plot code
ggplot(aes(b_Feedback, relevel(Article, "Pooled Effect", after = Inf)), 
       data = forest.data_1) +
  geom_vline(xintercept = fixef(meta_model_post)[2, 1], color = "grey", size = 1) +
  geom_vline(xintercept = fixef(meta_model_post)[2, 3:4], color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", size = 1) +
  geom_density_ridges(fill = "blue", rel_min_height = 0.01, col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = forest.data.summary_1, size = 1) +
  geom_text(data = mutate_if(forest.data.summary_1, is.numeric, round, 2),
    aes(label = glue("{b_Feedback} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(x = "Change",
       y = element_blank()) +
  theme_minimal()
```

    ## Picking joint bandwidth of 0.00355

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%201-2.png)

``` r
#Hypothesis plot (figure 2)
(hypothesis_plot <- plot(hypothesis(meta_model_post, "Feedback > 0"))[[1]]+ggtitle("Effect of group opinion on second rating"))
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%201-3.png)![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%201-4.png)

COMPARATIVE ANALYSIS

``` r
#Loading data including demographic variables 
demo_data <- read_csv("~/Desktop/SocKult exam/SocKult-Exam/Demographic responses.csv")
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
#Loading previously cleaned data for new study
sc_df <- read_csv("~/Desktop/SocKult exam/SocKult-Exam/sc_df_clean.csv")
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
#Merge with data included in analysis
demo_data <- merge(demo_data,sc_df,by = "ID")

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
#Getting distraction duration time 
#Create column with time differences 
sc_df$TimeStamp1 %>% as.POSIXct(sc_df$TimeStamp1)
```

    ##    [1] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##    [3] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##    [5] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##    [7] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##    [9] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##   [11] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##   [13] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##   [15] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##   [17] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##   [19] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ##   [21] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##   [23] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##   [25] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##   [27] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##   [29] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##   [31] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##   [33] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##   [35] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##   [37] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##   [39] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##   [41] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##   [43] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##   [45] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##   [47] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##   [49] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##   [51] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##   [53] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##   [55] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##   [57] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##   [59] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##   [61] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##   [63] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##   [65] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##   [67] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##   [69] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##   [71] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##   [73] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ##   [75] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##   [77] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##   [79] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##   [81] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##   [83] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##   [85] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##   [87] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##   [89] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##   [91] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##   [93] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ##   [95] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##   [97] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##   [99] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [101] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [103] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [105] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [107] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [109] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ##  [111] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [113] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [115] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [117] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [119] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [121] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [123] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [125] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [127] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [129] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [131] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [133] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [135] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [137] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [139] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [141] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [143] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [145] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [147] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [149] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [151] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [153] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [155] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [157] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [159] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [161] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [163] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [165] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [167] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ##  [169] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [171] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [173] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [175] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [177] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [179] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [181] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [183] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [185] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [187] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [189] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [191] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [193] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [195] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [197] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [199] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [201] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [203] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [205] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [207] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [209] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [211] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [213] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [215] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [217] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [219] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##  [221] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [223] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [225] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [227] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [229] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [231] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [233] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [235] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [237] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [239] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [241] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [243] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [245] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [247] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [249] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [251] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [253] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [255] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [257] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ##  [259] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [261] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [263] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [265] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [267] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [269] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [271] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [273] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [275] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [277] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [279] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [281] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [283] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [285] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [287] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [289] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [291] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [293] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [295] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ##  [297] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [299] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [301] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [303] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [305] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [307] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [309] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [311] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [313] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [315] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [317] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [319] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [321] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [323] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [325] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [327] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [329] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [331] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [333] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [335] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [337] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [339] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [341] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [343] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [345] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [347] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [349] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [351] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [353] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [355] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [357] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [359] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [361] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [363] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [365] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [367] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [369] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##  [371] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [373] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [375] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [377] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [379] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [381] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [383] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [385] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [387] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [389] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [391] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [393] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [395] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [397] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [399] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [401] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [403] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [405] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [407] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [409] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [411] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [413] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [415] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [417] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [419] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [421] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [423] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [425] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [427] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ##  [429] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [431] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [433] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [435] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [437] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [439] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [441] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [443] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [445] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [447] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [449] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [451] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [453] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [455] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [457] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [459] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [461] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [463] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [465] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [467] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [469] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [471] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [473] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [475] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [477] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [479] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##  [481] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ##  [483] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [485] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [487] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [489] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [491] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [493] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [495] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [497] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [499] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [501] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [503] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [505] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [507] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [509] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [511] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [513] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [515] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [517] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##  [519] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [521] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [523] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [525] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [527] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [529] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [531] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [533] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [535] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [537] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [539] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [541] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [543] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [545] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [547] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [549] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [551] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [553] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [555] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [557] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [559] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [561] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [563] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [565] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [567] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [569] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [571] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [573] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [575] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [577] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [579] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [581] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [583] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [585] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [587] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [589] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [591] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##  [593] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [595] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [597] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [599] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [601] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [603] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [605] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [607] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [609] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [611] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [613] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [615] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [617] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [619] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [621] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [623] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [625] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [627] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [629] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ##  [631] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [633] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [635] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [637] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [639] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [641] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [643] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [645] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [647] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [649] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [651] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [653] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [655] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [657] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [659] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [661] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [663] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [665] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [667] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [669] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [671] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [673] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [675] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [677] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [679] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [681] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [683] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [685] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [687] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [689] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [691] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [693] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [695] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [697] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [699] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [701] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [703] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##  [705] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [707] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [709] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [711] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [713] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [715] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [717] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [719] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [721] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [723] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [725] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [727] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [729] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [731] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [733] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [735] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [737] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [739] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [741] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [743] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [745] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [747] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [749] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [751] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [753] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [755] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [757] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [759] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [761] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [763] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [765] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [767] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [769] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [771] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [773] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [775] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [777] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##  [779] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [781] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [783] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [785] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [787] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [789] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [791] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [793] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [795] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [797] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [799] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [801] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [803] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [805] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [807] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [809] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [811] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [813] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [815] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [817] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [819] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [821] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [823] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [825] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [827] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [829] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [831] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [833] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [835] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ##  [837] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [839] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [841] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [843] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [845] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [847] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [849] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [851] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [853] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [855] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [857] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [859] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [861] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [863] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [865] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [867] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [869] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [871] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [873] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [875] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [877] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [879] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [881] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [883] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [885] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [887] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ##  [889] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [891] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [893] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [895] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [897] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [899] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [901] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [903] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [905] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [907] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [909] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [911] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [913] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [915] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [917] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [919] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [921] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [923] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [925] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ##  [927] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ##  [929] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ##  [931] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ##  [933] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ##  [935] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ##  [937] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ##  [939] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ##  [941] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ##  [943] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ##  [945] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ##  [947] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ##  [949] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ##  [951] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ##  [953] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ##  [955] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ##  [957] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ##  [959] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ##  [961] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ##  [963] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ##  [965] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ##  [967] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ##  [969] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ##  [971] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ##  [973] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ##  [975] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ##  [977] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ##  [979] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ##  [981] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ##  [983] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ##  [985] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ##  [987] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ##  [989] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ##  [991] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ##  [993] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ##  [995] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ##  [997] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ##  [999] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1001] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1003] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1005] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1007] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1009] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1011] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1013] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1015] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1017] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1019] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1021] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1023] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1025] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1027] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1029] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1031] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1033] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1035] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1037] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1039] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1041] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1043] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1045] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1047] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1049] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1051] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1053] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1055] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1057] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1059] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1061] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1063] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1065] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1067] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1069] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1071] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1073] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [1075] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1077] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1079] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1081] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1083] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1085] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1087] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1089] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1091] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1093] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1095] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1097] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1099] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1101] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1103] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1105] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1107] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1109] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1111] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [1113] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1115] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1117] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1119] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1121] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1123] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1125] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1127] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1129] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1131] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ## [1133] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1135] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1137] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1139] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1141] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1143] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1145] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1147] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [1149] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1151] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1153] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1155] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1157] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1159] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1161] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1163] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1165] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1167] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1169] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1171] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1173] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1175] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1177] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1179] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1181] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1183] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1185] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1187] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1189] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1191] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1193] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1195] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1197] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1199] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1201] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1203] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1205] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1207] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1209] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1211] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1213] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1215] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1217] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1219] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1221] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [1223] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1225] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1227] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1229] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1231] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1233] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1235] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1237] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1239] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1241] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1243] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1245] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1247] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1249] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1251] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1253] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1255] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1257] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1259] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [1261] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1263] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1265] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1267] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1269] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1271] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1273] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1275] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1277] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1279] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1281] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1283] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1285] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1287] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1289] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1291] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1293] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1295] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1297] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [1299] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1301] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1303] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1305] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1307] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1309] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1311] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1313] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1315] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1317] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ## [1319] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1321] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1323] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1325] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1327] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1329] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1331] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1333] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [1335] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1337] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1339] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1341] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1343] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1345] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1347] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1349] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1351] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1353] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1355] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1357] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1359] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1361] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1363] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1365] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1367] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1369] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1371] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [1373] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1375] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1377] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1379] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1381] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1383] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1385] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1387] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1389] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1391] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1393] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1395] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1397] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1399] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1401] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1403] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1405] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1407] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1409] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1411] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1413] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1415] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1417] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1419] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1421] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1423] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1425] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1427] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1429] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1431] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1433] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1435] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1437] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1439] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1441] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1443] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1445] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [1447] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1449] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1451] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1453] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1455] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1457] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1459] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1461] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1463] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1465] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1467] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1469] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1471] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1473] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1475] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1477] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1479] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1481] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1483] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [1485] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1487] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1489] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1491] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1493] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1495] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1497] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1499] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1501] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1503] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1505] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1507] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1509] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1511] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1513] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1515] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1517] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1519] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1521] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [1523] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1525] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1527] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1529] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1531] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1533] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1535] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1537] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1539] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1541] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1543] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1545] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1547] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1549] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1551] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1553] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1555] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1557] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1559] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1561] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1563] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1565] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1567] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1569] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1571] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1573] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1575] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1577] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1579] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1581] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1583] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1585] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1587] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1589] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1591] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1593] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1595] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [1597] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1599] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1601] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1603] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1605] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1607] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1609] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1611] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1613] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1615] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1617] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1619] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1621] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1623] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1625] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1627] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1629] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1631] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1633] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [1635] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1637] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1639] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1641] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1643] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1645] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1647] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1649] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1651] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1653] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ## [1655] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1657] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1659] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1661] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1663] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1665] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1667] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1669] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [1671] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1673] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1675] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1677] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1679] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1681] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1683] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1685] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1687] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1689] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1691] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1693] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1695] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1697] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1699] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1701] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1703] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1705] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1707] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1709] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1711] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1713] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1715] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1717] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1719] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1721] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1723] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1725] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1727] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1729] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1731] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1733] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1735] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1737] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1739] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1741] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1743] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [1745] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1747] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1749] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1751] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1753] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1755] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1757] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1759] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1761] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1763] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1765] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1767] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1769] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1771] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1773] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1775] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1777] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1779] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1781] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1783] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1785] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1787] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1789] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1791] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1793] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1795] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1797] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1799] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1801] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1803] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1805] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1807] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1809] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1811] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1813] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1815] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1817] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [1819] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1821] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1823] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1825] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1827] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1829] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1831] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1833] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1835] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1837] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1839] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1841] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1843] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1845] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1847] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1849] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1851] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1853] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1855] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1857] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1859] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1861] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1863] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1865] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1867] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1869] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1871] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1873] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1875] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [1877] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [1879] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [1881] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [1883] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [1885] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [1887] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [1889] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [1891] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [1893] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1895] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1897] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1899] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1901] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1903] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1905] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1907] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1909] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1911] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1913] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1915] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1917] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1919] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1921] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1923] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1925] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1927] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1929] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [1931] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [1933] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [1935] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [1937] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [1939] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [1941] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [1943] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [1945] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [1947] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [1949] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1951] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1953] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1955] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1957] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1959] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1961] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [1963] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [1965] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [1967] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [1969] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [1971] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [1973] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [1975] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [1977] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [1979] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [1981] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [1983] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [1985] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [1987] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [1989] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [1991] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [1993] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [1995] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [1997] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [1999] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2001] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2003] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2005] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2007] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2009] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2011] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2013] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2015] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2017] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2019] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2021] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2023] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2025] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2027] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2029] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2031] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2033] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2035] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2037] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2039] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [2041] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2043] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2045] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2047] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2049] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2051] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2053] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2055] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2057] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2059] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2061] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2063] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2065] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2067] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2069] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2071] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2073] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2075] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2077] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [2079] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2081] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2083] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2085] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2087] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2089] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2091] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2093] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2095] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2097] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2099] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2101] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2103] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2105] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2107] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2109] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2111] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2113] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2115] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [2117] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2119] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2121] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2123] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2125] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2127] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2129] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2131] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2133] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2135] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ## [2137] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2139] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2141] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2143] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2145] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2147] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2149] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2151] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [2153] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2155] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2157] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2159] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2161] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2163] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2165] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2167] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2169] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2171] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2173] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2175] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2177] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2179] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2181] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2183] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2185] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2187] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2189] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2191] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2193] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2195] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2197] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2199] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2201] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2203] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2205] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2207] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2209] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2211] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2213] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2215] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2217] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2219] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2221] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2223] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2225] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [2227] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2229] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2231] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2233] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2235] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2237] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2239] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2241] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2243] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2245] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2247] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2249] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2251] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2253] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2255] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2257] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2259] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2261] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2263] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2265] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2267] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2269] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2271] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2273] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2275] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2277] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2279] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2281] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2283] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2285] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2287] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2289] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2291] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2293] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2295] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2297] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2299] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2301] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2303] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2305] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2307] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2309] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2311] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2313] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2315] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2317] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2319] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2321] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2323] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2325] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2327] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2329] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2331] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2333] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2335] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [2337] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2339] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2341] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2343] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2345] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2347] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2349] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2351] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2353] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2355] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2357] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2359] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2361] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2363] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2365] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2367] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2369] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2371] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2373] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [2375] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2377] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2379] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2381] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2383] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2385] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2387] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2389] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2391] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2393] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2395] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2397] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2399] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2401] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2403] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2405] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2407] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2409] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2411] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [2413] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2415] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2417] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2419] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2421] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2423] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2425] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2427] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2429] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2431] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2433] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2435] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2437] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2439] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2441] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2443] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2445] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2447] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2449] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2451] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2453] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2455] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2457] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2459] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2461] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2463] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2465] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2467] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2469] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2471] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2473] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2475] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2477] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2479] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2481] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2483] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2485] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [2487] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2489] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2491] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2493] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2495] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2497] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2499] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2501] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2503] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2505] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2507] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2509] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2511] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2513] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2515] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2517] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2519] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2521] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2523] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [2525] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2527] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2529] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2531] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2533] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2535] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2537] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2539] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2541] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2543] "2020-05-09 14:09:32 UTC" "2020-05-11 13:13:10 UTC"
    ## [2545] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2547] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2549] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2551] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2553] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2555] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2557] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2559] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [2561] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2563] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2565] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2567] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2569] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2571] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2573] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2575] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2577] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2579] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2581] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2583] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2585] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2587] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2589] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2591] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2593] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2595] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2597] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [2599] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2601] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2603] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2605] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2607] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2609] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2611] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2613] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2615] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2617] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2619] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2621] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2623] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2625] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2627] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2629] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2631] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2633] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2635] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [2637] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2639] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2641] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2643] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2645] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2647] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2649] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2651] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2653] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2655] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2657] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2659] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2661] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2663] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2665] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2667] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2669] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2671] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2673] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2675] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2677] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2679] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2681] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2683] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2685] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2687] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2689] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2691] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2693] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2695] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2697] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2699] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2701] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2703] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2705] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2707] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2709] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [2711] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2713] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2715] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2717] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2719] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2721] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2723] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2725] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2727] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2729] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2731] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2733] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2735] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2737] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2739] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2741] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2743] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2745] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2747] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2749] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2751] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2753] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2755] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2757] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2759] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2761] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2763] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2765] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2767] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2769] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2771] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2773] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2775] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2777] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2779] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2781] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2783] "2020-05-11 11:35:22 UTC" "2020-05-11 12:20:26 UTC"
    ## [2785] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2787] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2789] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2791] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2793] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2795] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2797] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2799] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2801] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2803] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2805] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2807] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2809] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2811] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2813] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2815] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2817] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2819] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2821] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [2823] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2825] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2827] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2829] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2831] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2833] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2835] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2837] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2839] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2841] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2843] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2845] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2847] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2849] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2851] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2853] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2855] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2857] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2859] "2020-05-12 15:38:57 UTC" "2020-05-11 14:33:33 UTC"
    ## [2861] "2020-05-12 08:11:44 UTC" "2020-05-08 09:59:25 UTC"
    ## [2863] "2020-05-11 13:59:46 UTC" "2020-05-07 13:26:05 UTC"
    ## [2865] "2020-05-12 14:19:49 UTC" "2020-05-12 16:03:37 UTC"
    ## [2867] "2020-05-10 12:43:53 UTC" "2020-05-11 10:19:49 UTC"
    ## [2869] "2020-05-13 20:10:11 UTC" "2020-05-11 10:49:57 UTC"
    ## [2871] "2020-05-08 12:53:19 UTC" "2020-05-11 11:03:38 UTC"
    ## [2873] "2020-05-08 17:09:27 UTC" "2020-05-07 17:23:10 UTC"
    ## [2875] "2020-05-08 16:45:39 UTC" "2020-05-11 12:35:37 UTC"
    ## [2877] "2020-05-08 12:04:55 UTC" "2020-05-09 14:09:32 UTC"
    ## [2879] "2020-05-11 11:16:46 UTC" "2020-05-11 13:13:10 UTC"
    ## [2881] "2020-05-13 19:27:00 UTC" "2020-05-11 11:26:24 UTC"
    ## [2883] "2020-05-11 11:12:11 UTC" "2020-05-11 11:16:30 UTC"
    ## [2885] "2020-05-12 08:19:30 UTC" "2020-05-12 08:14:46 UTC"
    ## [2887] "2020-05-11 10:31:13 UTC" "2020-05-11 10:37:55 UTC"
    ## [2889] "2020-05-08 17:17:19 UTC" "2020-05-11 11:07:17 UTC"
    ## [2891] "2020-05-13 17:39:40 UTC" "2020-05-10 08:13:59 UTC"
    ## [2893] "2020-05-12 15:25:17 UTC" "2020-05-08 11:11:42 UTC"
    ## [2895] "2020-05-11 11:35:22 UTC" "2020-05-12 15:38:57 UTC"
    ## [2897] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2899] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2901] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2903] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2905] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2907] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2909] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2911] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2913] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2915] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2917] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2919] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2921] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2923] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2925] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2927] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2929] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2931] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2933] "2020-05-11 12:20:26 UTC" "2020-05-12 15:38:57 UTC"
    ## [2935] "2020-05-11 14:33:33 UTC" "2020-05-12 08:11:44 UTC"
    ## [2937] "2020-05-08 09:59:25 UTC" "2020-05-11 13:59:46 UTC"
    ## [2939] "2020-05-07 13:26:05 UTC" "2020-05-12 14:19:49 UTC"
    ## [2941] "2020-05-12 16:03:37 UTC" "2020-05-10 12:43:53 UTC"
    ## [2943] "2020-05-11 10:19:49 UTC" "2020-05-13 20:10:11 UTC"
    ## [2945] "2020-05-11 10:49:57 UTC" "2020-05-08 12:53:19 UTC"
    ## [2947] "2020-05-11 11:03:38 UTC" "2020-05-08 17:09:27 UTC"
    ## [2949] "2020-05-07 17:23:10 UTC" "2020-05-08 16:45:39 UTC"
    ## [2951] "2020-05-11 12:35:37 UTC" "2020-05-08 12:04:55 UTC"
    ## [2953] "2020-05-09 14:09:32 UTC" "2020-05-11 11:16:46 UTC"
    ## [2955] "2020-05-11 13:13:10 UTC" "2020-05-13 19:27:00 UTC"
    ## [2957] "2020-05-11 11:26:24 UTC" "2020-05-11 11:12:11 UTC"
    ## [2959] "2020-05-11 11:16:30 UTC" "2020-05-12 08:19:30 UTC"
    ## [2961] "2020-05-12 08:14:46 UTC" "2020-05-11 10:31:13 UTC"
    ## [2963] "2020-05-11 10:37:55 UTC" "2020-05-08 17:17:19 UTC"
    ## [2965] "2020-05-11 11:07:17 UTC" "2020-05-13 17:39:40 UTC"
    ## [2967] "2020-05-10 08:13:59 UTC" "2020-05-12 15:25:17 UTC"
    ## [2969] "2020-05-08 11:11:42 UTC" "2020-05-11 11:35:22 UTC"
    ## [2971] "2020-05-12 15:38:57 UTC"

``` r
sc_df$TimeStamp2 %>% as.POSIXct(sc_df$TimeStamp2)
```

    ##    [1] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##    [3] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##    [5] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##    [7] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##    [9] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##   [11] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##   [13] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##   [15] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##   [17] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##   [19] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ##   [21] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##   [23] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##   [25] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##   [27] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##   [29] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##   [31] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##   [33] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##   [35] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##   [37] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##   [39] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##   [41] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##   [43] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##   [45] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##   [47] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##   [49] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##   [51] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##   [53] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##   [55] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##   [57] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##   [59] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##   [61] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##   [63] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##   [65] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##   [67] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##   [69] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##   [71] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##   [73] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ##   [75] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##   [77] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##   [79] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##   [81] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##   [83] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##   [85] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##   [87] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##   [89] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##   [91] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##   [93] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ##   [95] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##   [97] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##   [99] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [101] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [103] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [105] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [107] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [109] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ##  [111] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [113] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [115] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [117] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [119] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [121] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [123] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [125] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [127] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [129] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [131] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [133] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [135] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [137] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [139] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [141] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [143] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [145] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [147] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [149] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [151] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [153] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [155] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [157] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [159] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [161] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [163] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [165] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [167] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ##  [169] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [171] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [173] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [175] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [177] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [179] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [181] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [183] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [185] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [187] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [189] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [191] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [193] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [195] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [197] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [199] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [201] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [203] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [205] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [207] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [209] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [211] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [213] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [215] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [217] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [219] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##  [221] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [223] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [225] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [227] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [229] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [231] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [233] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [235] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [237] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [239] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [241] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [243] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [245] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [247] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [249] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [251] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [253] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [255] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [257] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ##  [259] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [261] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [263] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [265] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [267] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [269] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [271] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [273] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [275] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [277] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [279] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [281] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [283] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [285] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [287] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [289] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [291] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [293] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [295] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ##  [297] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [299] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [301] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [303] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [305] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [307] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [309] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [311] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [313] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [315] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [317] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [319] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [321] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [323] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [325] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [327] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [329] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [331] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [333] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [335] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [337] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [339] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [341] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [343] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [345] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [347] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [349] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [351] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [353] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [355] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [357] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [359] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [361] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [363] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [365] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [367] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [369] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##  [371] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [373] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [375] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [377] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [379] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [381] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [383] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [385] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [387] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [389] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [391] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [393] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [395] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [397] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [399] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [401] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [403] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [405] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [407] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [409] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [411] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [413] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [415] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [417] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [419] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [421] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [423] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [425] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [427] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ##  [429] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [431] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [433] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [435] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [437] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [439] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [441] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [443] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [445] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [447] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [449] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [451] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [453] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [455] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [457] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [459] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [461] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [463] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [465] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [467] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [469] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [471] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [473] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [475] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [477] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [479] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##  [481] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ##  [483] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [485] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [487] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [489] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [491] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [493] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [495] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [497] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [499] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [501] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [503] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [505] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [507] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [509] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [511] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [513] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [515] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [517] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##  [519] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [521] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [523] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [525] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [527] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [529] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [531] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [533] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [535] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [537] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [539] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [541] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [543] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [545] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [547] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [549] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [551] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [553] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [555] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [557] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [559] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [561] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [563] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [565] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [567] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [569] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [571] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [573] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [575] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [577] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [579] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [581] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [583] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [585] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [587] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [589] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [591] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##  [593] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [595] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [597] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [599] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [601] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [603] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [605] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [607] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [609] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [611] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [613] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [615] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [617] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [619] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [621] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [623] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [625] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [627] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [629] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ##  [631] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [633] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [635] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [637] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [639] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [641] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [643] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [645] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [647] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [649] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [651] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [653] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [655] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [657] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [659] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [661] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [663] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [665] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [667] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [669] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [671] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [673] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [675] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [677] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [679] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [681] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [683] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [685] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [687] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [689] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [691] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [693] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [695] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [697] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [699] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [701] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [703] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##  [705] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [707] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [709] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [711] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [713] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [715] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [717] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [719] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [721] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [723] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [725] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [727] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [729] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [731] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [733] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [735] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [737] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [739] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [741] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [743] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [745] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [747] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [749] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [751] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [753] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [755] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [757] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [759] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [761] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [763] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [765] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [767] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [769] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [771] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [773] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [775] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [777] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##  [779] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [781] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [783] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [785] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [787] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [789] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [791] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [793] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [795] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [797] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [799] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [801] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [803] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [805] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [807] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [809] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [811] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [813] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [815] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [817] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [819] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [821] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [823] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [825] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [827] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [829] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [831] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [833] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [835] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ##  [837] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [839] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [841] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [843] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [845] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [847] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [849] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [851] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [853] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [855] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [857] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [859] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [861] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [863] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [865] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [867] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [869] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [871] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [873] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [875] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [877] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [879] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [881] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [883] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [885] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [887] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ##  [889] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [891] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [893] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [895] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [897] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [899] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [901] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [903] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [905] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [907] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [909] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [911] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [913] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [915] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [917] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [919] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [921] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [923] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [925] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ##  [927] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ##  [929] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ##  [931] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ##  [933] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ##  [935] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ##  [937] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ##  [939] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ##  [941] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ##  [943] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ##  [945] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ##  [947] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ##  [949] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ##  [951] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ##  [953] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ##  [955] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ##  [957] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ##  [959] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ##  [961] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ##  [963] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ##  [965] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ##  [967] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ##  [969] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ##  [971] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ##  [973] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ##  [975] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ##  [977] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ##  [979] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ##  [981] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ##  [983] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ##  [985] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ##  [987] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ##  [989] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ##  [991] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ##  [993] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ##  [995] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ##  [997] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ##  [999] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1001] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1003] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1005] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1007] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1009] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1011] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1013] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1015] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1017] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1019] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1021] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1023] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1025] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1027] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1029] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1031] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1033] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1035] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1037] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1039] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1041] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1043] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1045] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1047] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1049] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1051] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1053] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1055] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1057] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1059] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1061] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1063] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1065] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1067] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1069] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1071] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1073] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [1075] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1077] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1079] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1081] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1083] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1085] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1087] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1089] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1091] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1093] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1095] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1097] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1099] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1101] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1103] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1105] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1107] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1109] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1111] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [1113] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1115] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1117] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1119] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1121] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1123] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1125] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1127] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1129] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1131] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ## [1133] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1135] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1137] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1139] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1141] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1143] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1145] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1147] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [1149] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1151] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1153] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1155] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1157] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1159] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1161] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1163] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1165] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1167] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1169] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1171] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1173] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1175] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1177] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1179] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1181] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1183] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1185] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1187] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1189] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1191] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1193] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1195] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1197] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1199] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1201] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1203] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1205] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1207] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1209] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1211] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1213] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1215] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1217] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1219] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1221] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [1223] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1225] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1227] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1229] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1231] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1233] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1235] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1237] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1239] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1241] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1243] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1245] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1247] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1249] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1251] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1253] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1255] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1257] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1259] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [1261] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1263] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1265] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1267] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1269] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1271] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1273] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1275] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1277] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1279] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1281] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1283] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1285] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1287] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1289] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1291] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1293] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1295] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1297] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [1299] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1301] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1303] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1305] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1307] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1309] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1311] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1313] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1315] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1317] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ## [1319] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1321] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1323] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1325] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1327] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1329] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1331] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1333] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [1335] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1337] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1339] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1341] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1343] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1345] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1347] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1349] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1351] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1353] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1355] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1357] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1359] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1361] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1363] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1365] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1367] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1369] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1371] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [1373] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1375] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1377] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1379] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1381] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1383] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1385] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1387] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1389] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1391] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1393] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1395] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1397] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1399] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1401] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1403] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1405] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1407] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1409] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1411] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1413] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1415] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1417] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1419] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1421] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1423] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1425] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1427] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1429] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1431] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1433] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1435] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1437] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1439] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1441] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1443] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1445] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [1447] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1449] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1451] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1453] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1455] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1457] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1459] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1461] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1463] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1465] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1467] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1469] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1471] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1473] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1475] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1477] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1479] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1481] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1483] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [1485] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1487] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1489] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1491] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1493] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1495] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1497] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1499] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1501] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1503] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1505] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1507] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1509] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1511] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1513] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1515] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1517] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1519] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1521] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [1523] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1525] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1527] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1529] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1531] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1533] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1535] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1537] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1539] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1541] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1543] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1545] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1547] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1549] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1551] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1553] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1555] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1557] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1559] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1561] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1563] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1565] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1567] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1569] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1571] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1573] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1575] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1577] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1579] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1581] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1583] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1585] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1587] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1589] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1591] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1593] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1595] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [1597] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1599] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1601] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1603] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1605] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1607] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1609] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1611] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1613] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1615] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1617] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1619] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1621] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1623] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1625] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1627] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1629] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1631] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1633] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [1635] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1637] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1639] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1641] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1643] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1645] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1647] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1649] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1651] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1653] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ## [1655] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1657] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1659] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1661] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1663] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1665] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1667] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1669] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [1671] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1673] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1675] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1677] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1679] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1681] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1683] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1685] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1687] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1689] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1691] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1693] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1695] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1697] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1699] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1701] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1703] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1705] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1707] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1709] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1711] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1713] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1715] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1717] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1719] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1721] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1723] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1725] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1727] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1729] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1731] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1733] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1735] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1737] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1739] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1741] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1743] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [1745] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1747] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1749] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1751] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1753] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1755] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1757] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1759] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1761] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1763] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1765] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1767] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1769] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1771] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1773] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1775] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1777] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1779] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1781] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1783] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1785] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1787] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1789] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1791] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1793] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1795] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1797] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1799] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1801] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1803] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1805] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1807] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1809] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1811] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1813] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1815] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1817] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [1819] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1821] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1823] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1825] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1827] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1829] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1831] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1833] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1835] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1837] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1839] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1841] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1843] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1845] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1847] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1849] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1851] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1853] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1855] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1857] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1859] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1861] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1863] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1865] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1867] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1869] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1871] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1873] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1875] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [1877] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [1879] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [1881] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [1883] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [1885] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [1887] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [1889] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [1891] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [1893] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1895] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1897] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1899] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1901] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1903] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1905] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1907] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1909] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1911] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1913] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1915] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1917] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1919] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1921] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1923] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1925] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1927] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1929] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [1931] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [1933] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [1935] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [1937] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [1939] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [1941] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [1943] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [1945] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [1947] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [1949] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1951] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1953] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1955] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1957] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1959] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1961] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [1963] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [1965] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [1967] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [1969] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [1971] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [1973] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [1975] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [1977] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [1979] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [1981] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [1983] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [1985] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [1987] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [1989] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [1991] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [1993] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [1995] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [1997] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [1999] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2001] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2003] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2005] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2007] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2009] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2011] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2013] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2015] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2017] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2019] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2021] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2023] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2025] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2027] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2029] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2031] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2033] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2035] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2037] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2039] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [2041] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2043] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2045] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2047] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2049] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2051] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2053] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2055] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2057] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2059] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2061] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2063] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2065] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2067] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2069] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2071] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2073] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2075] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2077] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [2079] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2081] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2083] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2085] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2087] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2089] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2091] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2093] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2095] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2097] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2099] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2101] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2103] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2105] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2107] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2109] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2111] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2113] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2115] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [2117] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2119] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2121] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2123] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2125] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2127] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2129] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2131] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2133] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2135] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ## [2137] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2139] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2141] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2143] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2145] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2147] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2149] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2151] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [2153] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2155] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2157] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2159] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2161] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2163] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2165] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2167] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2169] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2171] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2173] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2175] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2177] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2179] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2181] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2183] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2185] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2187] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2189] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2191] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2193] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2195] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2197] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2199] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2201] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2203] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2205] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2207] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2209] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2211] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2213] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2215] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2217] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2219] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2221] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2223] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2225] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [2227] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2229] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2231] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2233] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2235] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2237] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2239] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2241] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2243] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2245] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2247] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2249] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2251] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2253] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2255] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2257] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2259] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2261] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2263] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2265] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2267] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2269] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2271] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2273] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2275] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2277] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2279] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2281] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2283] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2285] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2287] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2289] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2291] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2293] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2295] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2297] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2299] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2301] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2303] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2305] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2307] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2309] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2311] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2313] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2315] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2317] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2319] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2321] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2323] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2325] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2327] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2329] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2331] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2333] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2335] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [2337] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2339] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2341] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2343] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2345] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2347] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2349] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2351] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2353] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2355] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2357] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2359] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2361] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2363] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2365] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2367] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2369] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2371] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2373] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [2375] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2377] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2379] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2381] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2383] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2385] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2387] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2389] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2391] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2393] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2395] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2397] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2399] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2401] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2403] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2405] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2407] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2409] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2411] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [2413] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2415] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2417] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2419] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2421] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2423] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2425] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2427] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2429] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2431] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2433] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2435] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2437] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2439] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2441] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2443] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2445] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2447] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2449] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2451] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2453] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2455] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2457] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2459] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2461] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2463] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2465] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2467] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2469] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2471] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2473] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2475] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2477] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2479] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2481] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2483] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2485] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [2487] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2489] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2491] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2493] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2495] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2497] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2499] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2501] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2503] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2505] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2507] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2509] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2511] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2513] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2515] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2517] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2519] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2521] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2523] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [2525] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2527] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2529] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2531] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2533] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2535] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2537] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2539] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2541] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2543] "2020-05-10 09:58:15 UTC" "2020-05-11 15:34:50 UTC"
    ## [2545] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2547] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2549] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2551] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2553] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2555] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2557] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2559] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [2561] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2563] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2565] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2567] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2569] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2571] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2573] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2575] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2577] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2579] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2581] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2583] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2585] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2587] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2589] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2591] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2593] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2595] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2597] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [2599] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2601] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2603] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2605] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2607] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2609] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2611] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2613] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2615] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2617] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2619] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2621] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2623] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2625] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2627] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2629] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2631] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2633] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2635] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [2637] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2639] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2641] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2643] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2645] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2647] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2649] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2651] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2653] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2655] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2657] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2659] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2661] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2663] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2665] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2667] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2669] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2671] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2673] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2675] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2677] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2679] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2681] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2683] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2685] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2687] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2689] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2691] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2693] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2695] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2697] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2699] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2701] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2703] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2705] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2707] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2709] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [2711] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2713] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2715] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2717] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2719] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2721] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2723] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2725] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2727] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2729] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2731] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2733] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2735] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2737] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2739] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2741] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2743] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2745] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2747] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2749] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2751] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2753] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2755] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2757] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2759] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2761] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2763] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2765] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2767] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2769] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2771] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2773] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2775] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2777] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2779] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2781] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2783] "2020-05-12 20:54:58 UTC" "2020-05-12 13:08:57 UTC"
    ## [2785] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2787] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2789] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2791] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2793] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2795] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2797] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2799] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2801] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2803] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2805] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2807] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2809] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2811] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2813] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2815] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2817] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2819] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2821] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [2823] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2825] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2827] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2829] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2831] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2833] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2835] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2837] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2839] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2841] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2843] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2845] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2847] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2849] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2851] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2853] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2855] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2857] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2859] "2020-05-12 18:48:25 UTC" "2020-05-12 06:46:41 UTC"
    ## [2861] "2020-05-12 12:42:31 UTC" "2020-05-09 07:13:50 UTC"
    ## [2863] "2020-05-12 06:45:37 UTC" "2020-05-07 18:11:06 UTC"
    ## [2865] "2020-05-12 17:32:36 UTC" "2020-05-12 17:33:36 UTC"
    ## [2867] "2020-05-10 18:25:33 UTC" "2020-05-12 08:34:56 UTC"
    ## [2869] "2020-05-13 21:28:24 UTC" "2020-05-11 11:54:38 UTC"
    ## [2871] "2020-05-09 13:27:16 UTC" "2020-05-11 12:05:46 UTC"
    ## [2873] "2020-05-10 01:58:32 UTC" "2020-05-07 18:17:29 UTC"
    ## [2875] "2020-05-08 18:12:03 UTC" "2020-05-12 22:27:46 UTC"
    ## [2877] "2020-05-08 21:07:31 UTC" "2020-05-10 09:58:15 UTC"
    ## [2879] "2020-05-11 13:32:30 UTC" "2020-05-11 15:34:50 UTC"
    ## [2881] "2020-05-13 21:01:36 UTC" "2020-05-11 19:28:47 UTC"
    ## [2883] "2020-05-11 13:02:33 UTC" "2020-05-12 10:18:57 UTC"
    ## [2885] "2020-05-13 12:38:49 UTC" "2020-05-12 13:31:52 UTC"
    ## [2887] "2020-05-12 16:28:24 UTC" "2020-05-11 11:59:16 UTC"
    ## [2889] "2020-05-09 12:56:32 UTC" "2020-05-11 13:45:45 UTC"
    ## [2891] "2020-05-13 20:02:00 UTC" "2020-05-10 10:46:01 UTC"
    ## [2893] "2020-05-12 16:45:15 UTC" "2020-05-09 09:54:54 UTC"
    ## [2895] "2020-05-12 20:54:58 UTC" "2020-05-12 18:48:25 UTC"
    ## [2897] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2899] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2901] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2903] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2905] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2907] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2909] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2911] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2913] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2915] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2917] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2919] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2921] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2923] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2925] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2927] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2929] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2931] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2933] "2020-05-12 13:08:57 UTC" "2020-05-12 18:48:25 UTC"
    ## [2935] "2020-05-12 06:46:41 UTC" "2020-05-12 12:42:31 UTC"
    ## [2937] "2020-05-09 07:13:50 UTC" "2020-05-12 06:45:37 UTC"
    ## [2939] "2020-05-07 18:11:06 UTC" "2020-05-12 17:32:36 UTC"
    ## [2941] "2020-05-12 17:33:36 UTC" "2020-05-10 18:25:33 UTC"
    ## [2943] "2020-05-12 08:34:56 UTC" "2020-05-13 21:28:24 UTC"
    ## [2945] "2020-05-11 11:54:38 UTC" "2020-05-09 13:27:16 UTC"
    ## [2947] "2020-05-11 12:05:46 UTC" "2020-05-10 01:58:32 UTC"
    ## [2949] "2020-05-07 18:17:29 UTC" "2020-05-08 18:12:03 UTC"
    ## [2951] "2020-05-12 22:27:46 UTC" "2020-05-08 21:07:31 UTC"
    ## [2953] "2020-05-10 09:58:15 UTC" "2020-05-11 13:32:30 UTC"
    ## [2955] "2020-05-11 15:34:50 UTC" "2020-05-13 21:01:36 UTC"
    ## [2957] "2020-05-11 19:28:47 UTC" "2020-05-11 13:02:33 UTC"
    ## [2959] "2020-05-12 10:18:57 UTC" "2020-05-13 12:38:49 UTC"
    ## [2961] "2020-05-12 13:31:52 UTC" "2020-05-12 16:28:24 UTC"
    ## [2963] "2020-05-11 11:59:16 UTC" "2020-05-09 12:56:32 UTC"
    ## [2965] "2020-05-11 13:45:45 UTC" "2020-05-13 20:02:00 UTC"
    ## [2967] "2020-05-10 10:46:01 UTC" "2020-05-12 16:45:15 UTC"
    ## [2969] "2020-05-09 09:54:54 UTC" "2020-05-12 20:54:58 UTC"
    ## [2971] "2020-05-12 18:48:25 UTC"

``` r
sc_df$time_dif <- sc_df$TimeStamp2 - sc_df$TimeStamp1

#Create column with time difference in hours
sc_df$hours <- sc_df$time_dif/60

#Mean and SD 
mean(sc_df$hours)
```

    ## Time difference of 11.3554 mins

``` r
sd(sc_df$hours)
```

    ## [1] 11.23982

``` r
#Filter out time columns as they are irrelevant for analysis
sc_df <- sc_df %>% select(-c("Participant", "X1","TimeStamp1","TimeStamp2","time_dif","hours"))
```

``` r
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

    ## Start sampling

``` r
#Prior predictive check (SM figure 6)
prior_check <- pp_check(Combined_m_prior, nsamples=100)
(prior_check
  +ggtitle("Prior predictive check"))
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20modelling%202-1.png)

``` r
#Posterior model
Combined_m <- brm(
  Combined_f1,
  all_d,
  family = gaussian,
  prior = Combined_prior,
  sample_prior = T,
  chains = 2, #NB: 4 chains used in the report, reduced in order to make knitting possible
  cores = 2,
  iter = 2000, #NB: 4000 iterations used in the report, reduced in order to make knitting possible
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 20
  )
)
```

    ## Compiling the C++ model
    ## Start sampling

``` r
#Posterior predictive check (SM figure 6)
post_check <- pp_check(Combined_m, nsamples=100)
(post_check
  +ggtitle("Posterior predictive check"))
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20modelling%202-2.png)

``` r
#Obtaining estimate summaries of the posterior distribution (SM table 6)
summary(Combined_m)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: Change ~ 0 + FirstRating:Condition + Feedback:Condition + (1 + FirstRating + Feedback | ID) + (1 + FirstRating + Feedback | FaceID) 
    ##    Data: all_d (Number of observations: 11798) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~FaceID (Number of levels: 153) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                  0.54      0.06     0.43     0.66 1.01
    ## sd(FirstRating)                0.07      0.01     0.05     0.10 1.01
    ## sd(Feedback)                   0.02      0.01     0.00     0.04 1.00
    ## cor(Intercept,FirstRating)    -0.58      0.11    -0.75    -0.32 1.01
    ## cor(Intercept,Feedback)       -0.13      0.26    -0.60     0.40 1.00
    ## cor(FirstRating,Feedback)      0.14      0.27    -0.43     0.61 1.00
    ##                            Bulk_ESS Tail_ESS
    ## sd(Intercept)                   703     1005
    ## sd(FirstRating)                 379      696
    ## sd(Feedback)                    650     1213
    ## cor(Intercept,FirstRating)      595      883
    ## cor(Intercept,Feedback)        2476     1451
    ## cor(FirstRating,Feedback)      1860     1682
    ## 
    ## ~ID (Number of levels: 121) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                  2.16      0.11     1.95     2.38 1.00
    ## sd(FirstRating)                0.32      0.03     0.27     0.37 1.01
    ## sd(Feedback)                   0.01      0.01     0.00     0.03 1.01
    ## cor(Intercept,FirstRating)    -0.94      0.01    -0.96    -0.91 1.00
    ## cor(Intercept,Feedback)       -0.13      0.29    -0.64     0.46 1.00
    ## cor(FirstRating,Feedback)      0.17      0.30    -0.44     0.68 1.00
    ##                            Bulk_ESS Tail_ESS
    ## sd(Intercept)                   350      685
    ## sd(FirstRating)                 213      468
    ## sd(Feedback)                    708     1024
    ## cor(Intercept,FirstRating)      353      602
    ## cor(Intercept,Feedback)        1609     1619
    ## cor(FirstRating,Feedback)      1549     1691
    ## 
    ## Population-Level Effects: 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat
    ## FirstRating:ConditionPeri    -0.19      0.03    -0.26    -0.13 1.01
    ## FirstRating:ConditionPre     -0.20      0.03    -0.26    -0.15 1.01
    ## ConditionPeri:Feedback        0.03      0.01     0.00     0.06 1.00
    ## ConditionPre:Feedback         0.03      0.01     0.01     0.05 1.00
    ##                           Bulk_ESS Tail_ESS
    ## FirstRating:ConditionPeri      270      539
    ## FirstRating:ConditionPre       237      430
    ## ConditionPeri:Feedback        1983     1241
    ## ConditionPre:Feedback         1537     1049
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     1.20      0.01     1.18     1.22 1.00     3148     1569
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
posterior_summary(Combined_m, pars = c("^b_", "^sd_"), probs = c(0.025, 0.975) )
```

    ##                                Estimate   Est.Error          Q2.5
    ## b_FirstRating:ConditionPeri -0.19454520 0.032391915 -0.2640119535
    ## b_FirstRating:ConditionPre  -0.19892563 0.028813914 -0.2575363797
    ## b_ConditionPeri:Feedback     0.03079958 0.014004087  0.0043066676
    ## b_ConditionPre:Feedback      0.02623508 0.009299222  0.0088683902
    ## sd_FaceID__Intercept         0.54062301 0.059327009  0.4301577180
    ## sd_FaceID__FirstRating       0.07279442 0.011892936  0.0502204443
    ## sd_FaceID__Feedback          0.01524278 0.009962678  0.0007632875
    ## sd_ID__Intercept             2.16228074 0.110515244  1.9494517476
    ## sd_ID__FirstRating           0.31972465 0.025928252  0.2699912769
    ## sd_ID__Feedback              0.01271782 0.008954478  0.0005557497
    ##                                   Q97.5
    ## b_FirstRating:ConditionPeri -0.13413236
    ## b_FirstRating:ConditionPre  -0.14593417
    ## b_ConditionPeri:Feedback     0.05829660
    ## b_ConditionPre:Feedback      0.04670719
    ## sd_FaceID__Intercept         0.66479452
    ## sd_FaceID__FirstRating       0.09596170
    ## sd_FaceID__Feedback          0.03652157
    ## sd_ID__Intercept             2.38078867
    ## sd_ID__FirstRating           0.36954275
    ## sd_ID__Feedback              0.03211067

``` r
#Hypothesis testing
hypothesis(Combined_m,"ConditionPeri:Feedback > ConditionPre:Feedback")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (ConditionPeri:Fe... > 0        0      0.01    -0.02     0.03       1.73
    ##   Post.Prob Star
    ## 1      0.63     
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
    ## 1 (ConditionPre:Fee... > 0        0      0.01    -0.03     0.02       0.58
    ##   Post.Prob Star
    ## 1      0.37     
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
    ## 1 (ConditionPeri:Fe... > 0     0.03      0.01     0.01     0.05      73.07
    ##   Post.Prob Star
    ## 1      0.99    *
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
    ## 1 (ConditionPre:Fee... > 0     0.03      0.01     0.01     0.04     332.33
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

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%202-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%202-2.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%202-3.png)![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%202-4.png)

``` r
#Overlay plot (SM figure 8)
mcmc_rank_overlay(Combined_m,
                  pars = c("b_FirstRating:ConditionPeri", "b_FirstRating:ConditionPre", 
                           "b_ConditionPeri:Feedback","b_ConditionPre:Feedback",
                            "sd_FaceID__FirstRating", "sd_FaceID__Feedback",
                            "sd_ID__Feedback", "sd_ID__FirstRating")) + theme_classic()
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%202-5.png)

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

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%202-6.png)

``` r
#Meta-data including own study
MA_d <- read_csv("~/Desktop/SocKult exam/SocKult-Exam/MA_d.csv")
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
    ## Chain 1: Gradient evaluation took 0.000608 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 6.08 seconds.
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
    ## Chain 1:  Elapsed Time: 4.32906 seconds (Warm-up)
    ## Chain 1:                6.08907 seconds (Sampling)
    ## Chain 1:                10.4181 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '44a8c654f56268125828282c2b443901' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 3.6e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.36 seconds.
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
    ## Chain 2:  Elapsed Time: 4.16801 seconds (Warm-up)
    ## Chain 2:                2.83738 seconds (Sampling)
    ## Chain 2:                7.00539 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '44a8c654f56268125828282c2b443901' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 5.4e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.54 seconds.
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
    ## Chain 3:  Elapsed Time: 3.96553 seconds (Warm-up)
    ## Chain 3:                5.17106 seconds (Sampling)
    ## Chain 3:                9.13658 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '44a8c654f56268125828282c2b443901' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 3.1e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.31 seconds.
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
    ## Chain 4:  Elapsed Time: 4.08663 seconds (Warm-up)
    ## Chain 4:                2.50352 seconds (Sampling)
    ## Chain 4:                6.59016 seconds (Total)
    ## Chain 4:

``` r
#Model summary 
posterior_summary(meta_model_post_2, pars = c("^b_", "^sd_"), probs = c(0.025, 0.975))
```

    ##                         Estimate  Est.Error          Q2.5      Q97.5
    ## b_Intercept           0.02814197 0.04381882 -0.0665773633 0.11181748
    ## b_Feedback            0.16688129 0.02338521  0.1177504078 0.21224995
    ## sd_Article__Intercept 0.04675469 0.04331607  0.0016557138 0.15734727
    ## sd_Article__Feedback  0.02685252 0.02605636  0.0009826304 0.09493137

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
    ## sd(Intercept)               0.05      0.04     0.00     0.16 1.00     5563
    ## sd(Feedback)                0.03      0.03     0.00     0.09 1.00     6002
    ## cor(Intercept,Feedback)    -0.01      0.30    -0.58     0.57 1.00    15670
    ##                         Tail_ESS
    ## sd(Intercept)               6701
    ## sd(Feedback)                7028
    ## cor(Intercept,Feedback)     8617
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.03      0.04    -0.07     0.11 1.00     8046     5409
    ## Feedback      0.17      0.02     0.12     0.21 1.00     6685     5432
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#FOREST PLOT 2 (SM figure 8)

#Creating dataframe with beta estimates for each article
study.draws_2 <- spread_draws(meta_model_post_2, r_Article[Article,], b_Feedback) %>% 
  mutate(b_Feedback = r_Article + b_Feedback)
study.draws_2$Article <- as.factor(study.draws_2$Article) 

#Creating variable containing the pooled effect across studies
pooled.effect.draws_2 <- spread_draws(meta_model_post_2, b_Feedback) %>% 
  mutate(Article = "Pooled Effect")

#Binding individual and pooled estimates
forest.data_2 <- bind_rows(study.draws_2, pooled.effect.draws_2) %>% 
   ungroup() %>%
   mutate(Article = reorder(Article, b_Feedback))
```

    ## Warning in bind_rows_(x, .id): binding factor and character vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

``` r
#Summarizing estimates (mean and quantiles)
forest.data.summary_2 <- group_by(forest.data_2, Article) %>% 
  mean_qi(b_Feedback)

#Plot code
ggplot(aes(b_Feedback, relevel(Article, "Pooled Effect", after = Inf)), 
       data = forest.data_2) +
  geom_vline(xintercept = fixef(meta_model_post_2)[2, 1], color = "grey", size = 1) +
  geom_vline(xintercept = fixef(meta_model_post_2)[2, 3:4], color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", size = 1) +
  geom_density_ridges(fill = "blue", rel_min_height = 0.01, col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = forest.data.summary_2, size = 1) +
  geom_text(data = mutate_if(forest.data.summary_2, is.numeric, round, 2),
    aes(label = glue("{b_Feedback} [{.lower}, {.upper}]"), x = Inf), hjust = "inward") +
  labs(x = "Change",
       y = element_blank()) +
  theme_minimal()
```

    ## Picking joint bandwidth of 0.00353

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20meta-analysis%20and%20forest%20plot%20including%20own%20study-1.png)

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
#Re-load sc_df and remove time points 
sc_df <- read_csv("~/Desktop/SocKult exam/SocKult-Exam/sc_df_clean.csv")
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
sc_df_2 <- sc_df %>% select(-c("TimeStamp1","TimeStamp2"))

#Creating subsets of feedback type
sc_df_low <- subset(sc_df_2, Feedback %in% c(-3,-2))
sc_df_low$Feedback2 <- -2.5
sc_df_same <- subset(sc_df_2, Feedback %in% c(-1, 0, 1))
sc_df_same$Feedback2 <- 0
sc_df_high <- subset(sc_df_2, Feedback %in% c(3,2))
sc_df_high$Feedback2 <- 2.5

#Summary statistics of first and second rating for different types of feedback (SM table 5 - first 2 rows)
sc_df_2 %>% 
  get_summary_stats(FirstRating,type = "mean_sd")
```

    ## # A tibble: 1 x 4
    ##   variable        n  mean    sd
    ##   <chr>       <dbl> <dbl> <dbl>
    ## 1 FirstRating  2971  4.90  1.83

``` r
sc_df_2 %>% 
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
```

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

    ## Start sampling

``` r
#Prior predictive check (SM figure 9)
prior_check <- pp_check(SocConformity_m_prior, nsamples=100)
(prior_check
  +ggtitle("Prior predictive check"))
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20modelling%203-1.png)

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
    ## Start sampling

``` r
#Posterior predictive check (SM figure 9)
post_check <- pp_check(SocConformity_m, nsamples=100)
(post_check
  +ggtitle("Posterior predictive check"))
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20modelling%203-2.png)

``` r
#Summary of posterior model (SM table 7)
posterior_summary(SocConformity_m, pars = c("^b_", "^sd_"), probs = c(0.025, 0.975) )
```

    ##                                Estimate  Est.Error         Q2.5
    ## b_Intercept                  2.72570959 0.20967300  2.311618179
    ## b_FirstRating               -0.56548533 0.03743525 -0.639062637
    ## b_Feedback                   0.02390246 0.01514689 -0.005739027
    ## sd_FaceID__Intercept         0.35461738 0.08141601  0.174630281
    ## sd_FaceID__FirstRating       0.03646757 0.02036495  0.002153697
    ## sd_FaceID__Feedback          0.03208608 0.02016405  0.001468697
    ## sd_Participant__Intercept    0.95687385 0.11113918  0.752430531
    ## sd_Participant__FirstRating  0.16030131 0.02509174  0.114102256
    ## sd_Participant__Feedback     0.02585521 0.01720238  0.001213060
    ##                                   Q97.5
    ## b_Intercept                  3.14011522
    ## b_FirstRating               -0.49287603
    ## b_Feedback                   0.05332296
    ## sd_FaceID__Intercept         0.49419791
    ## sd_FaceID__FirstRating       0.07787898
    ## sd_FaceID__Feedback          0.07427430
    ## sd_Participant__Intercept    1.18410620
    ## sd_Participant__FirstRating  0.21292779
    ## sd_Participant__Feedback     0.06354827

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
    ## sd(Intercept)                  0.35      0.08     0.17     0.49 1.00
    ## sd(FirstRating)                0.04      0.02     0.00     0.08 1.00
    ## sd(Feedback)                   0.03      0.02     0.00     0.07 1.00
    ## cor(Intercept,FirstRating)     0.04      0.27    -0.48     0.55 1.00
    ## cor(Intercept,Feedback)        0.07      0.27    -0.45     0.57 1.00
    ## cor(FirstRating,Feedback)      0.08      0.29    -0.49     0.60 1.00
    ##                            Bulk_ESS Tail_ESS
    ## sd(Intercept)                  1311     1299
    ## sd(FirstRating)                 607      909
    ## sd(Feedback)                   2159     3321
    ## cor(Intercept,FirstRating)     6856     4996
    ## cor(Intercept,Feedback)        9223     5788
    ## cor(FirstRating,Feedback)      4739     5178
    ## 
    ## ~Participant (Number of levels: 37) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                  0.96      0.11     0.75     1.18 1.00
    ## sd(FirstRating)                0.16      0.03     0.11     0.21 1.00
    ## sd(Feedback)                   0.03      0.02     0.00     0.06 1.00
    ## cor(Intercept,FirstRating)    -0.60      0.12    -0.79    -0.34 1.00
    ## cor(Intercept,Feedback)        0.18      0.26    -0.38     0.64 1.00
    ## cor(FirstRating,Feedback)     -0.12      0.27    -0.62     0.41 1.00
    ##                            Bulk_ESS Tail_ESS
    ## sd(Intercept)                  5054     5206
    ## sd(FirstRating)                2031     4196
    ## sd(Feedback)                   2388     4216
    ## cor(Intercept,FirstRating)     2498     4877
    ## cor(Intercept,Feedback)        9912     5652
    ## cor(FirstRating,Feedback)     11078     5764
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       2.73      0.21     2.31     3.14 1.00     3292     4731
    ## FirstRating    -0.57      0.04    -0.64    -0.49 1.00     3077     4754
    ## Feedback        0.02      0.02    -0.01     0.05 1.00    10985     6047
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     1.19      0.02     1.15     1.23 1.00    11462     6226
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
    ## 1 (Feedback) > 0     0.02      0.02        0     0.05      15.84      0.94
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

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%203-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%203-2.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%203-3.png)![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%203-4.png)

``` r
#Overlay plot (SM figure 11)
mcmc_rank_overlay(SocConformity_m,
                  pars = c("b_Intercept", "b_Feedback", 
           "sd_FaceID__Intercept", "sd_FaceID__Feedback",
           "sd_Participant__Intercept", "sd_Participant__Feedback")) + theme_classic()
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%203-5.png)

``` r
#Spaghetti plot of participant level effects (SM figure 12)
#Predicting estimates for plotting
xx <- predict(SocConformity_m, summary=T)

#Include predicted estimates in dataframe
d38_lh <- cbind(sc_df_lh,xx)
d38_lh$Participant <- as.factor(d38_lh$Participant)

#Plot 
ggplot(d38_lh) + 
  geom_point(aes(Feedback,Change, color = ID, group=ID), show.legend=F) + 
  geom_smooth(method=lm, se=F, aes(Feedback,Change, color = ID), show.legend=F) +
  ggtitle("Regression of change on participant level")
```

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%203-6.png)

``` r
#Stacked participant level hypothesis plot (SM figure 12)
#Obtaining coefficients for each participant according to alternative hypothesis
X <- hypothesis(SocConformity_m, "Feedback > 0", group = "Participant", scope = "coef")

#Plot
X$hypothesis %>%
  left_join(distinct(d38_lh, Group = Participant)) %>% 
  mutate(Participant = factor(Group), Conformity = Estimate) %>%
  ggplot(aes(Conformity, Participant)) +
  geom_errorbarh(aes(xmin = CI.Lower, xmax = CI.Upper)) +
  geom_point() + theme_classic() + 
  ggtitle("Effect of feedback on participant level", subtitle="Social conformity estimates with 95%CI")
```

    ## Joining, by = "Group"

    ## Warning: Column `Group` joining character vector and factor, coercing into
    ## character vector

![](Total-script-FINISHED-2_files/figure-markdown_github/-%20visualizations%203-7.png)
