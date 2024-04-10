pacman::p_load(tidyverse, haven, jtools, randomForest)

rm(list = ls())

dat <- read_dta("data/panel_data/BES2019_W22_v24.0.dta")

# scaling functions --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# ind vars ------------------------------------------

dat <- dat %>% 
  mutate(
    white_british = fct_lump_n(as.factor(p_ethnicity), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    no_religion = fct_lump_n(as.factor(p_religion), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    non_uk_born = fct_lump_n(as.factor(p_country_birth), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double()
  ) %>% 
  rename(la_code = oslaua_code)

dat$male <- ifelse(dat$gender == 1, 1, 0)
dat$income <- ifelse(dat$p_gross_household %in% c(16, 17), 
                     NA, dat$p_gross_household)
dat$own_outright <- ifelse(dat$p_housing == 1, 1, 0)
dat$private_renting <- ifelse(dat$p_housing == 4, 1, 0)
dat$social_housing <- ifelse(dat$p_housing == 5|dat$p_housing == 6, 1, 0)
dat$own_mortgage <- ifelse(dat$p_housing == 2, 1, 0)
dat$homeowner <- ifelse(dat$own_outright == 1 | dat$own_mortgage == 1, 1, 0)
dat$age_raw <- dat$age
dat$age <- scale_this(dat$age)
dat$edu_20plus <- ifelse(dat$p_education_age == 5, 1, 0)
dat$edu_20plus[is.na(dat$p_education_age)] <- NA
dat$broadsheet <- ifelse(dat$p_paper_read %in% c(6,7,8,9,10), 1, 0)
dat$broadsheet[is.na(dat$p_paper_read)] <- NA
dat$full_time <- ifelse(dat$p_work_stat == 1, 1, 0)
dat <- dat %>% 
  mutate(low_income = ifelse(p_gross_household < 5, 1, 0) %>% as.factor)
dat$low_income[is.na(dat$income)] <- NA
dat$disabled <- ifelse(dat$p_disability %in% c(1, 2), 1, 0)
dat$disabled[is.na(dat$p_disability)] <- NA
dat$p_hh_size <- ifelse(dat$p_hh_size %in% c(9, 10), 
                        NA, dat$p_hh_size)
dat$single_household <- ifelse(dat$p_hh_size == 1, 1, 0)
dat$cohabiting <- ifelse(dat$p_marital %in% c(1, 2, 4), 1, 0)
dat$cohabiting[is.na(dat$p_marital)] <- NA
dat$edu_15 <- ifelse(dat$p_education_age == 1, 1, 0)
dat$edu_15[is.na(dat$p_education_age)] <- NA
dat$edu_16 <- ifelse(dat$p_education_age == 2, 1, 0)
dat$edu_16[is.na(dat$p_education_age)] <- NA
dat$e <- ifelse(dat$p_socgrade == 6, 1, 0)
dat$d <- ifelse(dat$p_socgrade == 5, 1, 0)
dat$c2 <- ifelse(dat$p_socgrade == 4, 1, 0)
dat$c1 <- ifelse(dat$p_socgrade == 3, 1, 0)
dat$b <- ifelse(dat$p_socgrade == 2, 1, 0)
dat$pakistan_bangladesh <- ifelse(dat$p_ethnicity %in% c(8, 9), 1, 0)
dat$black <- ifelse(dat$p_ethnicity %in% c(11, 12, 13), 1, 0)
dat$pri_job <- ifelse(dat$p_job_sector == 1, 1, 0)
dat$pri_job[is.na(dat$p_job_sector)] <- NA
dat$pub_job <- ifelse(dat$p_job_sector == 2, 1, 0)
dat$pub_job[is.na(dat$p_job_sector)] <- NA
dat$unemployed <- ifelse(dat$p_work_stat == 6, 1, 0)
dat$unemployed[is.na(dat$p_work_stat)] <- NA
dat$part_time <- ifelse(dat$p_work_stat %in% c(2, 3), 1, 0)
dat$part_time[is.na(dat$p_work_stat)] <- NA
dat$retired <- ifelse(dat$p_work_stat == 5, 1, 0)
dat$retired[is.na(dat$p_work_stat)] <- NA

dat %>% count(gor)
dat$london <- ifelse(dat$gor == 7, 1, 0)
dat$southeast <- ifelse(dat$gor == 8, 1, 0)
dat$region_fct <- as.factor(dat$gor)

dat %>% 
  count(income, p_gross_household)

dat %>% count(income, p_gross_household, low_income)

dat %>% 
  count(low_income) %>% 
  na.omit() %>% 
  mutate(perc = n / sum(n))

dat %>% 
  count(social_housing, homeowner) %>% 
  mutate(perc = n / sum(n))

dat %>%
  filter(is.na(low_income)) %>% 
  count(social_housing, homeowner) %>% 
  mutate(perc = n/sum(n))

# models ----------------------------------------------------

mod_dat <- dat %>% 
  select(id, income, p_gross_household, full_time, p_education_age, male, p_hh_size,
         single_household, disabled, edu_15, edu_16, edu_20plus,
         london, southeast, pri_job, pub_job,
         unemployed, retired, full_time, part_time, white_british,
         black, pakistan_bangladesh, age_raw, non_uk_born, social_housing,
         private_renting, own_mortgage, own_outright, homeowner,
         region_fct, e, d, c2, c1, b, cohabiting) %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
  )

mod_dat %>% count(income, p_gross_household)

mod_dat <- mod_dat %>% 
  select(id, income, full_time, education_age, male, log_hh, disabled, unemployed,
         full_time, part_time, white_british, pakistan_bangladesh,
         log_age, non_uk_born, social_housing, private_renting,
         own_mortgage, own_outright, region_fct, e, d, c1, c2, b, cohabiting) %>% 
  na.omit()

# train and test ------------------------------------------------
set.seed(123)
train <- sample_frac(mod_dat, 0.9)
test <- mod_dat %>% 
  filter(!id %in% train$id)
train <- train %>% select(-id)
test <- test %>% select(-id)

# lm mod -------------------------------------------------

mincer_mod <- lm(income ~ .,
                 data = train)

summary(mincer_mod)

# lm train preds
lm_preds_train <- predict(mincer_mod)

lm_mse_train <- mean((train$income - lm_preds_train)^2)
lm_mse_train

# lm test preds
lm_preds_test <- predict(mincer_mod, newdata = test)

lm_mse_test <- mean((test$income - lm_preds_test)^2)
lm_mse_test

# rf mod -------------------------------------------------

set.seed(123)
rf_mod <- randomForest(income ~ .,
                       data = train, mtry = 12,
                       ntree = 500)

# rf train preds
rf_preds_train <- predict(rf_mod)

rf_mse_train <- mean((train$income - rf_preds_train)^2)
rf_mse_train

# rf test preds
rf_preds_test <- predict(rf_mod, newdata = test)

rf_mse_test <- mean((test$income - rf_preds_test)^2)
rf_mse_test

# comparison
lm_mse_train
rf_mse_train

lm_mse_test
rf_mse_test

# distribution of predictions
tibble(
  random_forest = rf_preds_train,
  linear_model = lm_preds_train
) %>% 
  pivot_longer(cols = random_forest:linear_model,
               names_to = "model",
               values_to = "predictions") %>% 
  ggplot(aes(x = predictions, fill = model)) +
  geom_density(alpha = 0.3) +
  scale_fill_viridis_d() +
  theme_minimal()

tibble(
  random_forest = rf_preds_test,
  linear_model = lm_preds_test
) %>% 
  pivot_longer(cols = random_forest:linear_model,
               names_to = "model",
               values_to = "predictions") %>% 
  ggplot(aes(x = predictions, fill = model)) +
  geom_density(alpha = 0.3) +
  scale_fill_viridis_d() +
  theme_minimal()

# linear model on full dataset ----------------------------------

mod_dat <- mod_dat %>% select(-id)

mincer_mod_full <- lm(income ~ .,
                      data = mod_dat)

summary(mincer_mod_full)

# saving lm model ------------------------------------------------

saveRDS(mincer_mod_full, file = "working/models/lm_income_w22.RDS")
