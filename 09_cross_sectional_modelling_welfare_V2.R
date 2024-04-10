pacman::p_load(haven, tidyverse, jtools, lme4, lmerTest, margins, ggstance)

rm(list = ls())

# loading data ----------------------------------------------------------------

load("working/data/cross_sectional_df_welfare.RData")

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
         immig_burden, all_of(level_twos), contains("raw"), 
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

# multivariate ------------------------------------------------

immig_multi <- glmer(immig_burden ~ male + white_british +
                       no_religion + edu_20plus +
                       social_housing + private_renting +
                       homeowner + age +
                       c1_c2 + d_e + non_uk_born +
                       (1|LAD),
                     data = df_immi, family = binomial("logit"))

summary(immig_multi)

# including level 2 predictors  ------------------------------

immig_con <- glmer(immig_burden ~ male + white_british +
                     no_religion + edu_20plus +
                     social_housing + private_renting +
                     homeowner + age +
                     c1_c2 + d_e + non_uk_born +
                     affordability + gdp_capita +
                     pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct +
                     degree_pct + manuf_pct +
                     (1|LAD),
                   data = df_immi, family = binomial("logit"))

summary(immig_con)

anova(immig_multi, immig_con)

# cross level interaction ------------------------------------------------------

df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immig_int <- glmer(immig_burden ~ social_housing + homeowner + private_renting +  
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
                   data = df_immi, family = binomial("logit"))
summary(immig_int)

anova(immig_con, immig_int)

saveRDS(immig_int, file = "working/markdown_data/immig_burden_int.RDS")

# marginal effects --------------------------------------------------------

marginals_wel <- margins(immig_int, type = "response")

summary(marginals_wel)

saveRDS(marginals_wel, 
        file = "working/markdown_data/marginals_welfare.RDS")

# with uni var ---------------------------------------------------

df_immi_uni <- df_immi_uni %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immig_uni <- glmer(immig_burden ~ social_housing.affordability +
                     homeowner.affordability +
                     social_housing + homeowner + affordability +
                     male + white_british + no_religion + uni +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born +
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct + degree_pct +
                     manuf_pct + (1|LAD),
                   data = df_immi_uni, family = binomial("logit"))
summary(immig_uni)

marginals_uni <- margins(immig_uni, type = "response")

summary(marginals_uni)

# log affordability ---------------------------------------

immig_log <- glmer(immig_burden ~ (social_housing * affordability_log) + 
                     (homeowner * affordability_log) + 
                     private_renting +  male + white_british + 
                     no_religion + edu_20plus +
                     age + c1_c2 + d_e + non_uk_born + 
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + manuf_pct +
                     (1|LAD),
                   data = df_immi, family = binomial("logit"))
summary(immig_log)

# prices --------------------------------------------------

immig_pri <- glmer(immig_burden ~ (social_housing * prices) + 
                     (homeowner * prices) + 
                     private_renting +  male + white_british + 
                     no_religion + edu_20plus +
                     age + c1_c2 + d_e + non_uk_born + 
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + manuf_pct +
                     (1|LAD),
                   data = df_immi, family = binomial("logit"))
summary(immig_pri)

# region dummies ------------------------------------------

immig_reg <- glmer(immig_burden ~ social_housing + homeowner + private_renting +  
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
                   data = df_immi, family = binomial("logit"))
summary(immig_reg)

# including income_full ------------------------------------------------

df_inc <- df_inc %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_inc <- glmer(immig_burden ~ social_housing.affordability +
                     homeowner.affordability +
                     social_housing + homeowner + affordability +
                     income_full +
                     male + white_british + no_religion + edu_20plus +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born +
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct + degree_pct +
                     manuf_pct + (1|LAD),
                   data = df_inc, family = binomial("logit"))
summary(immi_inc)

marginals_inc <- margins(immi_inc, type = "response")

summary(marginals_inc)

# coef plot ---------------------------------------------------------

marginals_main_tib <- summary(marginals_wel) %>% 
  as_tibble() %>% 
  mutate(Model = "Main")

marginals_uni_tib <- summary(marginals_uni) %>% 
  as_tibble() %>% 
  mutate(Model = "Uni")

marginals_inc_tib <- summary(marginals_inc) %>% 
  as_tibble() %>% 
  mutate(Model = "Income")

plot_names <- tibble(
  term = c("social_housing",
           "affordability",
           "homeowner", 
           "private_renting",
           "social_housing.affordability",
           "homeowner.affordability"),
  var_names = c("Social housing",
                "Affordability",
                "Homeowner",
                "Private renting",
                "Affordability:Social housing",
                "Affordability:Homeowner")
)

coef_plot_welfare <- bind_rows(marginals_main_tib, 
          marginals_uni_tib, 
          marginals_inc_tib) %>%
  filter(factor %in% plot_names$term) %>% 
  left_join(plot_names, by = c("factor" = "term")) %>% 
  ggplot(aes(x = AME, y = fct_rev(var_names), colour = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
             linewidth = 1.5, alpha = 0.7) +
  geom_linerangeh(aes(xmin = lower, xmax = upper), 
                  position = position_dodge(width = 0.4),
                  size = 1) +
  geom_point(position = position_dodge(width = 0.4),
             shape = 21, fill = "white", size = 3.5) +
  theme_minimal() +
  drop_y_gridlines() +
  labs(x = "Average Marginal Effect", y = NULL) +
  theme(legend.position = "top") +
  scale_colour_viridis_d()

coef_plot_welfare

saveRDS(coef_plot_welfare, file = "working/markdown_viz/coef_plot_welfare.RDS")
