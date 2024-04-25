pacman::p_load(tidyverse, haven, lme4, lmerTest, jtools, lfe)

rm(list = ls())

########################################################################
# immigration ----------------------------------------------------------
########################################################################

# loading data
load("working/data/longitudinal_df.RData")

# saving a df for later application of filters prior to robustness checks
df <- immig_df %>% 
  rename(LAD = oslaua_code) %>% 
  mutate(social_housing.affordability_mean = social_housing * affordability_mean,
         homeowner.affordability_mean = homeowner * affordability_mean)

# missing observations -------------------------------

immig_df %>% map_int(~sum(is.na(.)))

missing_las <- function(df, x){
  step1 <- df %>% 
    mutate(nas = is.na({{x}})) %>% 
    filter(nas == T) %>% 
    count(oslaua_code) %>% 
    arrange(desc(n))
  out <- step1$n
  names(out) <- step1$oslaua_code
  return(out)
}

# missing affordability is Scotland and City of London
missing_las(immig_df, affordability)

# missing degree pct is Scotland
missing_las(immig_df, degree_pct)

# missing foreign_per_1000 is Scotland and City of London
missing_las(immig_df, foreign_per_1000)

# dataset for main model
immig_df <- immig_df %>% 
  select(-degree_pct_change, -prices, -prices_mean, -prices_within,
         -uni, -income_full) %>% 
  rename(LAD = oslaua_code) %>% 
  mutate(social_housing.affordability_mean = social_housing * affordability_mean,
         homeowner.affordability_mean = homeowner * affordability_mean) %>% 
  na.omit()

nrow(df) - nrow(immig_df)

# dataset for robustness with prices
immig_df_price <- df %>% 
  select(-degree_pct_change, -affordability, 
         -affordability_log, -affordability_log_mean,
         -affordability_mean, -affordability_within,
         -affordability_log_within, -uni, -income_full) %>% 
  na.omit()

# dataset for robustness with degree change
immig_df_change <- df %>% 
  select(-prices, -prices_mean, -prices_within, -uni, -income_full) %>% 
  na.omit()

# dataset for uni model
immig_df_uni <- df %>% 
  select(-degree_pct_change, -prices, -prices_mean, -prices_within,
         -edu_20plus, -income_full) %>% 
  na.omit()

# dataset for income model
immig_df_inc <- df %>% 
  select(-degree_pct_change, -prices, -prices_mean, -prices_within,
         -uni) %>% 
  na.omit()

## fixed effects ------------------------------------------------------------------

# fixed effects model
immi_fe <- felm(immigSelf ~ affordability +
                  #pop_density + 
                  foreign_per_1000 +
                  over_65_pct + under_15_pct +
                  gdp_capita + 
                  #manuf_pct + 
                  as.factor(year_c) | 
                  id + LAD,
                data = immig_df)
summary(immi_fe)

# within effects model
immi_fe2 <- lmer(immigSelf ~ affordability_within +
                   # pop_density_within + 
                   foreign_per_1000_within +
                   over_65_pct_within + under_15_pct_within +
                   gdp_capita_within + 
                   #manuf_pct_within +
                   as.factor(year_c) +
                   (1|LAD) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_fe2)

# REWB model
immi_fe3 <- lmer(immigSelf ~ affordability_within + affordability_mean +
                   #pop_density_within + pop_density_mean + 
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   gdp_capita_within + gdp_capita_mean +
                   #manuf_pct_within + manuf_pct_mean +
                   as.factor(year_c) + (1|LAD) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_fe3)

# interaction effects ------------------------------------------------

# homeowner interaction only
immi_hom <- lmer(immigSelf ~ affordability_mean + affordability_within +
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   gdp_capita_within + gdp_capita_mean +
                   #manuf_pct_within + manuf_pct_mean +
                   edu_20plus + male + white_british + no_religion + 
                   c1_c2 + d_e + non_uk_born + private_renting +
                   homeowner + social_housing +
                   as.factor(year_c) + degree_pct +
                   homeowner.affordability_mean +
                   (1|LAD) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_hom)

immi_int <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability_mean +
                   social_housing.affordability_mean + # SH interaction
                   homeowner.affordability_mean + 
                   affordability_within + # affordability +
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   gdp_capita_within + gdp_capita_mean +
                   #manuf_pct_within + manuf_pct_mean +
                   edu_20plus + male + white_british + no_religion + 
                   c1_c2 + d_e + non_uk_born +
                   as.factor(year_c) + degree_pct + 
                   (1|LAD) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_int)

# SH interaction improves model fit
anova(immi_hom, immi_int)

saveRDS(immi_int, file = "working/markdown_data/immi_int_long.RDS")

# robustness check - log affordability ---------------------------------

immi_log <- lmer(immigSelf ~ (social_housing * affordability_log_mean) +
                   (homeowner * affordability_log_mean) +
                   affordability_log_within + # affordability +
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   gdp_capita_within + gdp_capita_mean +
                   #manuf_pct_within + manuf_pct_mean +
                   edu_20plus + male + white_british + no_religion + c1_c2 + 
                   d_e + non_uk_born + private_renting +
                   as.factor(year_c) + degree_pct +
                   (1|LAD) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_log)

# robustness check - prices -------------------------------------------

immi_price <- lmer(immigSelf ~ (social_housing * prices_mean) +
                     (homeowner * prices_mean) +
                     prices_within + 
                     #pop_density_within + pop_density_mean +
                     foreign_per_1000_within + foreign_per_1000_mean +
                     over_65_pct_within + over_65_pct_mean +
                     under_15_pct_within + under_15_pct_mean +
                     gdp_capita_within + gdp_capita_mean +
                     #manuf_pct_within + manuf_pct_mean +
                     edu_20plus + male + white_british + no_religion + c1_c2 +
                     d_e + non_uk_born + private_renting +
                     as.factor(year_c) + degree_pct +
                     (1|LAD) + (1|id),
                   data = immig_df_price, REML = FALSE)
summary(immi_price)

# robustness check - degree percent change ---------------------------

immi_int2 <- lmer(immigSelf ~ (social_housing * affordability_mean) +
                    (homeowner * affordability_mean) +
                    affordability_within + 
                    #pop_density_within + pop_density_mean +
                    foreign_per_1000_within + foreign_per_1000_mean +
                    over_65_pct_within + over_65_pct_mean +
                    under_15_pct_within + under_15_pct_mean +
                    gdp_capita_within + gdp_capita_mean +
                    #manuf_pct_within + manuf_pct_mean +
                    edu_20plus + male + white_british + no_religion + 
                    c1_c2 + d_e + non_uk_born + private_renting +
                    as.factor(year_c) + degree_pct + 
                    degree_pct_change + # adding degree pct change
                    (1|LAD) + (1|id),
                  data = immig_df_change, REML = FALSE)
summary(immi_int2)

# robustness check - uni -------------------------------------------

immi_uni <- lmer(immigSelf ~ (social_housing * affordability_mean) +
                   (homeowner * affordability_mean) +
                   affordability_within +
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   gdp_capita_within + gdp_capita_mean +
                   #manuf_pct_within + manuf_pct_mean +
                   male + white_british + no_religion + c1_c2 +
                   d_e + non_uk_born + private_renting +
                   as.factor(year_c) + degree_pct + 
                   uni + # adding uni
                   (1|LAD) + (1|id),
                 data = immig_df_uni, REML = FALSE)
summary(immi_uni)

# robustness check - income -------------------------------------------

immi_inc <- lmer(immigSelf ~ (social_housing * affordability_mean) +
                   (homeowner * affordability_mean) +
                   affordability_within + 
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   gdp_capita_within + gdp_capita_mean +
                   #manuf_pct_within + manuf_pct_mean +
                   edu_20plus + male + white_british + no_religion + 
                   c1_c2 + d_e + non_uk_born + private_renting +
                   income_full + # adding income
                   as.factor(year_c) + degree_pct +
                   (1|LAD) + (1|id),
                 data = immig_df_inc, REML = FALSE)
summary(immi_inc)
