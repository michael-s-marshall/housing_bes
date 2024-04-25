pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance)

rm(list = ls())

# loading dataset ---------------------------------------------

load("working/data/cross_sectional_df.RData")

# predicted values for income -----------------------------------------

lm_income <- readRDS("working/models/lm_income_w22.RDS")

mod_df <- df %>% 
  select(id, income, full_time, education_age, male, log_hh, disabled, unemployed,
         full_time, part_time, white_british, pakistan_bangladesh,
         log_age, non_uk_born, social_housing, private_renting,
         own_mortgage, own_outright, region_fct, e, d, c1, c2, b, cohabiting)

mod_df$income_preds <- predict(lm_income, newdata = mod_df)
min(mod_df$income_preds, na.rm = T)
max(mod_df$income_preds, na.rm = T)
mod_df$income_preds[mod_df$income_preds < 1] <- 1
min(mod_df$income_preds, na.rm = T)
sum(is.na(mod_df$income_preds))

mod_df <- mod_df %>% 
  mutate(
    income_full = ifelse(is.na(income),
                             income_preds,
                             income)
  )

mod_df <- mod_df %>% 
  select(id, income_full)

rm(lm_income)

##############################################################################
# immigself ------------------------------------------------------------------
##############################################################################

# missing observations --------------------------------

df_immi <- df %>% 
  select(-tory_2019, -uni, -income,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -pakistan_bangladesh, -log_age, 
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting) %>% 
  rename(LAD = la_code)

df_immi %>% map_int(~sum(is.na(.)))

missing_las <- function(df, x){
  step1 <- df %>% 
    mutate(nas = is.na({{x}})) %>% 
    filter(nas == T) %>% 
    count(LAD) %>% 
    arrange(desc(n))
  out <- step1$n
  names(out) <- step1$LAD
  return(out)
}

# missing affordability is Scotland and City of London
missing_las(df_immi, affordability)

# missing degree pct is Scotland
missing_las(df_immi, degree_pct)

# missing foreign_per_1000 is Scotland and City of London
missing_las(df_immi, foreign_per_1000)

# missing housing costs measures is Scotland
missing_las(df_immi, cost_ratio)
missing_las(df_immi, price_ratio)
missing_las(df_immi, hcli)

# removing NAs
df_eng_wales <- df_immi %>% drop_na(c(affordability, degree_pct, foreign_per_1000))

df_eng_wales %>% map_int(~sum(is.na(.)))

df_immi <- df_immi %>% 
  na.omit()

1 - (nrow(df_immi) / nrow(df_eng_wales))

rm(df_eng_wales)

# predictions from low income model ------------------------------

df_inc <- df %>% 
  select(-tory_2019, -prices, -prices_raw, -uni,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -pakistan_bangladesh, -log_age, 
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting) %>% 
  rename(LAD = la_code) %>%   
  left_join(mod_df, by = "id")  

these_vars <- df_inc %>% 
  select(-income) %>% 
  names()

df_inc <- df_inc %>% 
  drop_na(all_of(these_vars))

df_inc %>% map_int(~sum(is.na(.)))

# modelling ---------------------------------------------

# ols null model
immi_fit <- lm(immigSelf ~ 1, data = df_immi)

# lmer null model
immi_lmer <- lmer(immigSelf ~ (1|LAD), data = df_immi, REML = F)

summ(immi_lmer)

logLik(immi_fit)
logLik(immi_lmer)
2 * (logLik(immi_lmer) - logLik(immi_fit))

# hypothesis vars only, testing improved fit from SH interaction --------------

# making interaction terms with scaled values
df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_hypot <- lmer(immigSelf ~ homeowner.affordability +
                     homeowner + affordability +
                     (1|LAD),
                   data = df_immi, REML = FALSE)

summary(immi_hypot)

immi_hypot_sh <- lmer(immigSelf ~ social_housing.affordability +
                        homeowner.affordability +
                        social_housing + homeowner + affordability +
                        (1|LAD),
                      data = df_immi, REML = FALSE)

summary(immi_hypot_sh)

# improvement in model fit is significant
anova(immi_hypot, immi_hypot_sh)

# including controls, testing for improvement from SH interaction -------------

immi_hom <- lmer(immigSelf ~ homeowner.affordability +
                   homeowner + affordability +
                   social_housing +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_hom)

immi_int <- lmer(immigSelf ~ social_housing + homeowner + private_renting +  
                   affordability +
                   male + white_british + 
                   no_religion + edu_20plus +
                   age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   social_housing.affordability + 
                   homeowner.affordability +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_int)

# model fit is improved significantly according to chi square
anova(immi_hom, immi_int)

saveRDS(immi_int, file = "working/markdown_data/immi_int.RDS")

# robustness check - log scale -------------------------------------

immi_log <- lmer(immigSelf ~ (social_housing * affordability_log) +
                   (homeowner * affordability_log) +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_log)

# robustness check - with prices -------------------------------------

immi_int_price <- lmer(immigSelf ~ (social_housing * prices) +
                         (homeowner * prices) +
                         male + white_british +
                         no_religion + edu_20plus +
                         private_renting + age +
                         c1_c2 + d_e + non_uk_born +
                         gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                         over_65_pct + under_15_pct +
                         degree_pct + manuf_pct +
                         (1|LAD),
                       data = df_immi, REML = FALSE)
summary(immi_int_price)

# robustness check - dummy for region ----------------------------------

immi_reg <- lmer(immigSelf ~ social_housing.affordability + 
                   homeowner.affordability +
                   social_housing + homeowner + affordability +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct + region_code +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_reg)

anova(immi_int, immi_reg) 

# incl. region makes very little difference to estimates
housing_vars <- c("affordability","homeowner","social_housing",
                  "homeowner.affordability","social_housing.affordability")
tibble(
  var = housing_vars,
  no_region = fixef(immi_int)[housing_vars],
  incl_region = fixef(immi_reg)[housing_vars]
) %>% 
  pivot_longer(cols = no_region:incl_region,
               names_to = "model",
               values_to = "estimate") %>% 
  filter(var != "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = var, colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.5, 
             colour = "lightgrey") +
  geom_point(size = 3, position = position_dodgev(height = 0.5)) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  drop_y_gridlines()

# robustness check - uni > education age ---------------------------

df_uni <- df %>%
  select(-tory_2019, -prices, -prices_raw, -income,
         -full_time, -education_age, -log_hh, -disabled, -unemployed,
         -part_time, -pakistan_bangladesh, -log_age, 
         -region_fct, -e, -d, -c1, -c2, -b, -cohabiting) %>% 
  rename(LAD = la_code)

df_uni %>% select(uni, edu_20plus) %>% map_int(~sum(is.na(.)))

df_uni <- df_uni %>% 
  select(-edu_20plus) %>% 
  na.omit() %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_uni <- lmer(immigSelf ~ social_housing + homeowner + private_renting +  
                   affordability +
                   male + white_british + 
                   no_religion + 
                   age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   social_housing.affordability + 
                   homeowner.affordability +
                   uni + # adding uni
                   (1|LAD),
                 data = df_uni, REML = FALSE)

summary(immi_uni)

# robustness check - including low income predictions -------------------------

df_inc <- df_inc %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_inc <- lmer(immigSelf ~ social_housing + homeowner + private_renting +  
                   affordability +
                   male + white_british + 
                   no_religion + edu_20plus +
                   age +
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   social_housing.affordability + 
                   homeowner.affordability +
                   income_full + # adding income
                   (1|LAD),
                 data = df_inc, REML = FALSE)
summary(immi_inc)

# visualising coefficients ---------------------------------

confint_clean <- function(confint_obj, lmer_obj){
  confint_obj %>% 
    as_tibble() %>% 
    bind_cols(
      tibble(term = c(".sig01",".sigma",names(fixef(lmer_obj))))
    ) %>% 
    bind_cols(tibble(estimate = c(NA, NA, fixef(lmer_obj)))) %>% 
    na.omit() %>% 
    filter(term != "(Intercept)") %>% 
    rename(lower = 1, upper = 2)
}

set.seed(123)
int_confint <- confint(immi_int, method = "profile")
uni_confint <- confint(immi_uni, method = "profile")
inc_confint <- confint(immi_inc, method = "profile")

plot_names <- tibble(
  term = c(housing_vars, "private_renting"),
  var_names = c("Affordability",
                "Homeowner",
                "Social housing",
                "Affordability:Homeowner",
                "Affordability:Social housing",
                "Private renting")
)

coef_plot_immi <- int_confint %>% 
  confint_clean(immi_int) %>% 
  full_join(
    uni_confint %>% 
      confint_clean(immi_uni),
    by = c("term"),
    suffix = c(".int",".uni")
  ) %>% 
  full_join(
    inc_confint %>% 
      confint_clean(immi_inc),
    by = "term"
  ) %>%
  rename(
   lower.inc = lower,
   upper.inc = upper,
   estimate.inc = estimate
  ) %>%
  select(term, everything()) %>% 
  filter(term %in% plot_names$term) %>% 
  pivot_longer(cols = lower.int:estimate.inc,
               names_to = c("parameter", "model"),
               values_to = "value",
               names_sep = "\\.") %>%
  pivot_wider(names_from = "parameter",
              values_from = "value") %>%
  mutate(Model = ifelse(model == "inc", "Income",
                        ifelse(model == "int", "Main", "Uni"))) %>% 
  left_join(plot_names, by = "term") %>% 
  ggplot(aes(x = estimate, y = fct_rev(var_names), colour = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
             linewidth = 1.5, alpha = 0.7) +
  geom_linerangeh(aes(xmin = lower, xmax = upper), 
                  position = position_dodge(width = 0.4),
                size = 1) +
  geom_point(position = position_dodge(width = 0.4),
             shape = 21, fill = "white", size = 3.5) +
  theme_minimal() +
  drop_y_gridlines() +
  labs(x = "Estimate", y = NULL) +
  theme(legend.position = "top") +
  scale_colour_viridis_d()

coef_plot_immi

saveRDS(coef_plot_immi, file = "working/markdown_viz/coef_plot_immi.RDS")

# visualising interaction term ------------------------------

# making interaction terms with raw values
df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

immi_viz <- lmer(immigSelf ~ social_housing.affordability +
                   homeowner.affordability +
                   social_housing + homeowner + affordability_raw +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct + (1|LAD),
                 data = df_immi, REML = FALSE)

# anti immigration among social housing tenants 
x_scale <- seq(min(df_immi$affordability_raw), 
               max(df_immi$affordability_raw), 
               (max(df_immi$affordability_raw) - min(df_immi$affordability_raw))/5)

immi_dummy <- expand.grid(
  male = c(mean(df_immi$male)),
  white_british = c(mean(df_immi$white_british)),
  no_religion = c(mean(df_immi$no_religion)),
  edu_20plus = c(mean(df_immi$edu_20plus)),
  homeowner = c(0,1),
  private_renting = c(mean(df_immi$private_renting)),
  age = mean(df_immi$age),
  c1_c2 = c(mean(df_immi$c1_c2)),
  d_e = c(mean(df_immi$d_e)),
  non_uk_born = c(mean(df_immi$non_uk_born)),
  gdp_capita = c(mean(df_immi$gdp_capita)),
  pop_sqm_2021 = c(mean(df_immi$pop_sqm_2021)),
  foreign_per_1000 = c(mean(df_immi$foreign_per_1000)),
  over_65_pct = c(mean(df_immi$over_65_pct)),
  under_15_pct = c(mean(df_immi$under_15_pct)),
  degree_pct = c(mean(df_immi$degree_pct)),
  manuf_pct = c(mean(df_immi$manuf_pct)),
  social_housing = c(0,1),
  affordability_raw = x_scale
) %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

acov <- vcov(immi_viz)
fixed <- summary(immi_viz)$coefficients[,"Estimate"]
vars_order <- names(fixed)[-1]
xmat <- immi_dummy %>%
  mutate(int = 1, .before = 1) %>%
  select(int, all_of(vars_order)) %>%
  as.matrix()

immi_dummy$fit <- xmat %*% fixed
immi_dummy$SE <- xmat %*% acov %*% t(xmat) %>%
  diag() %>%
  sqrt()

immi_dummy <- immi_dummy %>%
  mutate(LL = fit - qnorm(0.975)*SE,
         UL = fit + qnorm(0.975)*SE)

pacman::p_load(patchwork)

p1 <- immi_dummy %>%
  mutate(
    tenure = ifelse(social_housing == 1 & homeowner == 0, "Social housing",
                    ifelse(homeowner == 1 & social_housing == 0, "Homeowner",
                           ifelse(social_housing == 0 & homeowner == 0, "Other",
                                  "remove")))
  ) %>% 
  filter(tenure != "remove") %>% 
  mutate(tenure = fct_drop(tenure)) %>% 
  ggplot(aes(x = affordability_raw, y = fit,
             colour = tenure)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, group = tenure, fill = tenure,
                  colour = NULL),
              alpha = 0.2) +
  geom_line(linewidth = 2.5) +
  theme_bw() +
  drop_gridlines() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(x = NULL,
       y = "Fewer immigrants",
       colour = "Tenure",
       fill = "Tenure") +
  coord_cartesian(ylim = c(5.5,8.5)) +
  theme(legend.position = "top")

p2 <- df_immi %>% 
  ggplot(aes(x = affordability_raw)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, 
                 colour = "black", fill = "lightgrey") +
  theme_bw() +
  drop_gridlines() +
  labs(x = "Affordability ratio", y = "Density")

int_plot <- p1 / p2

int_plot

saveRDS(int_plot, file = "working/markdown_viz/int_plot.RDS")

# models using housing cost measures ------------------------------------------

# adding regional degree pct
degrees <- read_csv("data/regional_degree_pct.csv")

degrees <- degrees %>% 
  rename(region_code = `Area Codes`,
         degree_pct_region = `Level 4+ (%)`) %>%
  select(region_code, degree_pct_region)

df_immi <- df_immi %>% 
  left_join(degrees, by = "region_code")

df_inc <- df_inc %>% 
  left_join(degrees, by = "region_code")

# df immi -----------------------------------------------------
# sh * costs, owners * affordability
immi_costs <- lmer(immigSelf ~ (social_housing * cost_ratio) +
                     homeowner +
                     degree_pct_region +
                     male + white_british +
                     no_religion + edu_20plus +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born +
                     (1|region_code) + (1|region_code:LAD),
                   data = df_immi, REML = FALSE)

summary(immi_costs)

immi_hcli <- lmer(immigSelf ~ (social_housing * hcli) +
                    homeowner +
                    degree_pct_region +
                    male + white_british + 
                    no_religion + edu_20plus +
                    private_renting + age + 
                    c1_c2 + d_e + non_uk_born + 
                    (1|region_code) + (1|region_code:LAD),
                  data = df_immi, REML = FALSE)

summary(immi_hcli)

immi_price <- lmer(immigSelf ~ (social_housing * price_ratio) +
                     homeowner +
                     degree_pct_region +
                     male + white_british + 
                     no_religion + edu_20plus +
                     private_renting + age + 
                     c1_c2 + d_e + non_uk_born + 
                     (1|region_code) + (1|region_code:LAD),
                   data = df_immi, REML = FALSE)

summary(immi_price)

AIC(immi_costs, immi_hcli, immi_price)

tibble(
  `Cost ratio` = fixef(immi_costs)[c("social_housing","cost_ratio","social_housing:cost_ratio")],
  `30:40` = fixef(immi_hcli)[c("social_housing","hcli","social_housing:hcli")],
  `Price earnings ratio` = fixef(immi_price)[c("social_housing","price_ratio","social_housing:price_ratio")],
  variable = c("Social housing", "Affordability", "Interaction term")
) %>% 
  pivot_longer(
    cols = `Cost ratio`:`Price earnings ratio`,
    names_to = "Affordability measure",
    values_to = "Estimate"
  ) %>% 
  ggplot(aes(x = Estimate, y = variable, colour = `Affordability measure`)) +
  geom_point(position = position_dodge(width = 0.3),
             size = 2) +
  scale_colour_viridis_d() +
  theme_bw() +
  drop_y_gridlines() +
  labs(y = NULL) +
  theme(legend.position = "top")

# df_inc -----------------------------------------------------------

# sh * costs, owners * affordability
immi_cost2 <- lmer(immigSelf ~ (social_housing * cost_ratio) +
                     homeowner +
                     degree_pct_region +
                     male + white_british +
                     no_religion + edu_20plus +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born + income_full +
                     (1|region_code) + (1|region_code:LAD),
                   data = df_inc, REML = FALSE)

summary(immi_cost2)

immi_hcl2 <- lmer(immigSelf ~ (social_housing * hcli) +
                    homeowner +
                    degree_pct_region +
                    male + white_british + 
                    no_religion + edu_20plus +
                    private_renting + age + 
                    c1_c2 + d_e + non_uk_born + income_full + 
                    (1|region_code) + (1|region_code:LAD),
                  data = df_inc, REML = FALSE)

summary(immi_hcl2)

immi_pric2 <- lmer(immigSelf ~ (social_housing * price_ratio) +
                     homeowner +
                     degree_pct_region +
                     male + white_british + 
                     no_religion + edu_20plus +
                     private_renting + age + income_full +
                     c1_c2 + d_e + non_uk_born + 
                     (1|region_code) + (1|region_code:LAD),
                   data = df_inc, REML = FALSE)

summary(immi_pric2)

tibble(
  `Cost ratio` = fixef(immi_cost2)[c("social_housing","cost_ratio","social_housing:cost_ratio")],
  `30:40` = fixef(immi_hcl2)[c("social_housing","hcli","social_housing:hcli")],
  `Price earnings ratio` = fixef(immi_pric2)[c("social_housing","price_ratio","social_housing:price_ratio")],
  variable = c("Social housing", "Affordability", "Interaction term")
) %>% 
  pivot_longer(
    cols = `Cost ratio`:`Price earnings ratio`,
    names_to = "Affordability measure",
    values_to = "Estimate"
  ) %>% 
  ggplot(aes(x = Estimate, y = variable, colour = `Affordability measure`)) +
  geom_point(position = position_dodge(width = 0.3),
             size = 2) +
  scale_colour_viridis_d() +
  theme_bw() +
  drop_y_gridlines() +
  labs(y = NULL) +
  theme(legend.position = "top")
