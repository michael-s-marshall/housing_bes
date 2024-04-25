pacman::p_load(haven, tidyverse, jtools, lme4, lmerTest, margins, ggstance)

rm(list = ls())

# loading data ----------------------------------------------------------------

load("working/data/cross_sectional_df_eu.RData")

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# missing values ---------------------------------------

level_twos <- df %>% select(affordability:manuf_pct) %>% names()

df_immi <- df %>% 
  select(la_code, uni, male, white_british, no_religion, edu_20plus,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner,
         control_immi, all_of(level_twos), contains("raw"), 
         region_code, income, income_preds, income_full) %>% 
  rename(LAD = la_code)

these_vars <- df_immi %>% 
  select(-uni, -income, -income_preds, -income_full, -LAD) %>% 
  names()

df_immi %>% map_int(~sum(is.na(.)))

df_immi <- df_immi %>% 
  select(LAD, all_of(these_vars)) %>% 
  na.omit()

df_immi_uni <- df %>% 
  select(all_of(these_vars), uni, la_code) %>%
  rename(LAD = la_code) %>% 
  na.omit()

nrow(df) - nrow(df_immi)

(nrow(df) - nrow(df_immi)) / nrow(df)

df_inc <- df %>% 
  select(all_of(these_vars), income_full, la_code) %>% 
  rename(LAD = la_code) %>% 
  na.omit()

###############################################################################
# modelling ------------------------------------------------------------------
###############################################################################

# null model test ----------------------------------------------

# null model
immig_fit <- lm(control_immi ~ 1, data = df_immi)

# lmer null model
immig_lmer <- lmer(control_immi ~ (1|LAD), data = df_immi,
                   REML = FALSE)

summ(immig_lmer)

logLik(immig_fit)
logLik(immig_lmer)
2 * (logLik(immig_lmer) - logLik(immig_fit))

# hypothesis vars only, testing improved fit from SH interaction --------------

# making interaction terms with scaled values
df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_hypot <- lmer(control_immi ~ homeowner.affordability +
                     homeowner + affordability +
                     (1|LAD),
                   data = df_immi, REML = FALSE)

summary(immi_hypot)

immi_hypot_sh <- lmer(control_immi ~ homeowner.affordability +
                        social_housing.affordability +
                        homeowner + affordability +
                        social_housing +
                        (1|LAD),
                      data = df_immi, REML = FALSE)

summary(immi_hypot_sh)

# improvement in model fit is significant
anova(immi_hypot, immi_hypot_sh)

# including controls, testing for improvement from SH interaction -------------

immig_hom <- lmer(control_immi ~ social_housing + homeowner + private_renting +
                    affordability +
                    male + white_british + 
                    no_religion + edu_20plus +
                    age + 
                    c1_c2 + d_e + non_uk_born + 
                    gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                    over_65_pct + under_15_pct + 
                    degree_pct + manuf_pct +
                    homeowner.affordability +
                    (1|LAD),
                  data = df_immi, REML = FALSE)
summary(immig_hom)

immig_int <- lmer(control_immi ~ social_housing + homeowner + private_renting +
                    affordability +
                    male + white_british + 
                    no_religion + edu_20plus +
                    age + 
                    c1_c2 + d_e + non_uk_born + 
                    gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                    over_65_pct + under_15_pct + 
                    degree_pct + manuf_pct +
                    social_housing.affordability + # adding SH interaction
                    homeowner.affordability +
                    (1|LAD),
                  data = df_immi, REML = FALSE)
summary(immig_int)

# model fit is improved significantly according to chi square
anova(immig_hom, immig_int)

saveRDS(immig_int, file = "working/markdown_data/immig_eu_int.RDS")

# with uni var ---------------------------------------------------

df_immi_uni <- df_immi_uni %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immig_uni <- lmer(control_immi ~ social_housing.affordability +
                    homeowner.affordability +
                    social_housing + homeowner + affordability +
                    male + white_british + no_religion +
                    private_renting + age +
                    c1_c2 + d_e + non_uk_born +
                    gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                    over_65_pct + under_15_pct + degree_pct + manuf_pct +
                    uni + # adding uni
                    (1|LAD),
                  data = df_immi_uni, REML = FALSE)
summary(immig_uni)

# log affordability ---------------------------------------

immig_log <- lmer(control_immi ~ (social_housing * affordability_log) +
                    (homeowner * affordability_log) + 
                    private_renting +  male + white_british + 
                    no_religion + edu_20plus +
                    age + c1_c2 + d_e + non_uk_born + 
                    gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                    over_65_pct + under_15_pct + 
                    degree_pct + manuf_pct +
                    (1|LAD),
                  data = df_immi, REML = FALSE)
summary(immig_log)

# prices --------------------------------------------------

immig_pri <- lmer(control_immi ~ (social_housing * prices) + 
                     (homeowner * prices) + 
                     private_renting +  male + white_british + 
                     no_religion + edu_20plus +
                     age + c1_c2 + d_e + non_uk_born + 
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + manuf_pct +
                     (1|LAD),
                   data = df_immi, REML = FALSE)
summary(immig_pri)

# region dummies ------------------------------------------

immig_reg <- lmer(control_immi ~ social_housing + homeowner + private_renting +
                    affordability +
                    male + white_british +
                    no_religion + edu_20plus +
                    age +
                    c1_c2 + d_e + non_uk_born +
                    gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                    over_65_pct + under_15_pct +
                    degree_pct + manuf_pct +
                    social_housing.affordability +
                    homeowner.affordability + region_code +
                    (1|LAD),
                  data = df_immi, REML = FALSE)
summary(immig_reg)

# including income_full ------------------------------------------------

df_inc <- df_inc %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_inc <- lmer(control_immi ~ social_housing.affordability +
                   homeowner.affordability +
                   social_housing + homeowner + affordability +
                   male + white_british + no_religion + edu_20plus +
                   private_renting + age +
                   c1_c2 + d_e + non_uk_born +
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                   over_65_pct + under_15_pct + degree_pct + manuf_pct + 
                   income_full + # adding income
                   (1|LAD),
                 data = df_inc, REML = FALSE)
summary(immi_inc)

# coef plot ---------------------------------------------------------

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
int_confint <- confint(immig_int, method = "profile")
uni_confint <- confint(immig_uni, method = "profile")
inc_confint <- confint(immi_inc, method = "profile")

housing_vars <- c("affordability","homeowner","social_housing",
                  "homeowner.affordability","social_housing.affordability")

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
  confint_clean(immig_int) %>% 
  full_join(
    uni_confint %>% 
      confint_clean(immig_uni),
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

saveRDS(coef_plot_immi, file = "working/markdown_viz/coef_plot_eu.RDS")
