#From CoPilot

# Install once if needed:
# install.packages(c("tidyverse","GGally","janitor","vip","tidymodels","glmnet","ranger","pdp"))

library(tidyverse)
library(GGally)
library(janitor)
library(tidymodels)
library(glmnet)   # engine for LASSO/Ridge
library(ranger)   # fast random forest
library(vip)      # variable importance plotting
library(pdp)      # partial dependence

#--- Load the CSV (adjust the path if needed)
dat_raw <- readr::read_csv("data/mbbsTraitsAndCorr.csv", guess_max = 1e6)

# Keep original names so we can match exactly, but you can clean if you like:
# dat_raw <- janitor::clean_names(dat_raw)

# Select outcome + predictors of interest
dat <- dat_raw %>%
  select(corr, starts_with("dif_"))

# Quick check of missingness (your file has very few NAs in dif_*, none in corr)
summary(dat)


#2) Quick EDA to see patterns
#2a. Pairwise scatterplots (sample a subset if the dataset is large)

set.seed(1)
dat_sample <- dat %>% drop_na() %>% slice_sample(n = min(500, nrow(.)))

# A small pairs plot: corr vs a few key dif_ variables
vars_to_plot <- c("dif_mass","dif_tailLen","dif_handWing","dif_wingLen","dif_beakDep")
vars_to_plot <- vars_to_plot[vars_to_plot %in% names(dat_sample)]  # keep only those that exist

if (length(vars_to_plot) > 0) {
  ggpairs(dat_sample, columns = c("corr", vars_to_plot),
          upper = list(continuous = "smooth"),
          lower = list(continuous = "points"),
          diag = list(continuous = "densityDiag"))
}


# 2.b
# Compute Spearman correlation of each dif_* with corr
spearman_tbl <- dat %>%
  pivot_longer(cols = starts_with("dif_"), names_to = "feature", values_to = "x") %>%
  group_by(feature) %>%
  summarize(
    n = sum(!is.na(x) & !is.na(dat$corr)),
    rho = suppressWarnings(cor(x, dat$corr, method = "spearman", use = "pairwise")),
    .groups = "drop"
  ) %>%
  mutate(abs_rho = abs(rho)) %>%
  arrange(desc(abs_rho))

# Bar plot of top features by |Spearman rho|
spearman_tbl %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(feature, abs_rho), y = abs_rho)) +
  geom_col(fill = "#377eb8") +
  coord_flip() +
  labs(x = NULL, y = "|Spearman ρ| with corr",
       title = "Univariate association with species correlation (corr)",
       subtitle = "Higher bars suggest stronger monotonic relationship")

# Faceted scatter
top_k <- 9
top_feats <- spearman_tbl %>% slice_head(n = top_k) %>% pull(feature)

dat %>%
  select(corr, all_of(top_feats)) %>%
  pivot_longer(-corr, names_to = "feature", values_to = "value") %>%
  ggplot(aes(value, corr)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen", linetype = "dashed") +
  facet_wrap(~ feature, scales = "free_x") +
  labs(title = "corr vs top dif_* predictors",
       subtitle = "Red = linear fit; green dashed = LOESS",
       x = "Predictor value", y = "corr")

# Multivariate models

dat <- dat %>%
  mutate(corr_z = atanh(pmax(pmin(corr, 0.999), -0.999)))  # keeps finite values

# model setup
set.seed(123)
dat_model <- dat %>% select(corr, corr_z, starts_with("dif_"))

# Train/test split
split   <- initial_split(dat_model, prop = 0.8, strata = corr)
train   <- training(split)
test    <- testing(split)

# Recipe: impute small number of NAs, standardize, optionally remove highly correlated predictors
rec <- recipe(corr_z ~ ., data = train) %>%
  update_role(corr, new_role = "outcome_raw") %>%  # keep raw corr for later inspection
  step_zv(all_predictors()) %>%
  step_impute_median(starts_with("dif_")) %>%
  step_normalize(starts_with("dif_")) %>%
  step_corr(starts_with("dif_"), threshold = 0.95)   # drop near-duplicate predictors

# LASSO
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lasso_wf <- workflow() %>% add_model(lasso_spec) %>% add_recipe(rec)

set.seed(123)
lasso_res <- tune_grid(
  lasso_wf,
  resamples = vfold_cv(train, v = 4, strata = corr_z),
  grid = 50
)

best_lasso <- select_best(lasso_res, metric = "rmse")
lasso_final <- finalize_workflow(lasso_wf, best_lasso) %>% fit(train)

# Coefficient path / selected variables
lasso_fit <- lasso_final %>% extract_fit_parsnip() %>% pluck("fit")
coef_tbl <- broom::tidy(lasso_fit) %>% arrange(desc(abs(estimate)))

temp <- coef_tbl %>% 
  filter(term != "(Intercept)") %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(term, abs(estimate)), y = estimate)) +
  geom_col(fill = "#984ea3") +
  coord_flip() +
  labs(x = NULL, y = "LASSO coefficient (on corr_z scale)",
       title = "Top coefficients from LASSO (sparse linear model)")

# Random forest
rf_spec <- rand_forest(trees = 1000, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

rf_wf <- workflow() %>% add_model(rf_spec) %>% add_recipe(rec)

set.seed(123)
rf_res <- tune_grid(
  rf_wf,
  resamples = vfold_cv(train, v = 4, strata = corr_z),
  grid = 20
)

best_rf <- select_best(rf_res, metric = "rmse")
rf_final <- finalize_workflow(rf_wf, best_rf) %>% fit(train)

# Variable importance
rf_fit <- rf_final %>% extract_fit_parsnip() %>% pluck("fit")
vip::vip(rf_fit, num_features = 25, aesthetics = list(fill = "#4daf4a")) +
  labs(title = "Random Forest permutation importance",
       subtitle = "Higher = more important for predicting corr_z")

# Test performance
lasso_preds <- predict(lasso_final, test) 
rf_preds <- predict(rf_final, test, type = "raw")$predictions  # Extract predictions as a vector
corr_data <- test %>% select(corr, corr_z) 

# Combine the results into a single data frame
preds <- bind_cols(lasso_preds, 
                   rf_pred_z = rf_preds, 
                   corr_data)

# Rename columns
names(preds)[1:2] <- c("lasso_pred_z", "rf_pred_z")

# Back-transform if desired
preds <- preds %>%
  mutate(
    lasso_pred_corr = tanh(lasso_pred_z),
    rf_pred_corr    = tanh(rf_pred_z)
  )

yardstick::metrics(preds, truth = corr, estimate = lasso_pred_corr)
yardstick::metrics(preds, truth = corr, estimate = rf_pred_corr)

# Partial dependance
# Pick top variables from RF importance (or from LASSO)

# Ensure rec and rf_fit are already defined in your script
top_for_pdp <- vip::vi(rf_fit) %>% 
  arrange(desc(Importance)) %>%
  slice_head(n = 6) %>% 
  pull(Variable)

# Get the correctly baked training data
train_data <- bake(prep(rec), new_data = train)

for (v in top_for_pdp) {
  tryCatch({
    partial_response <- pdp::partial(
      rf_fit, 
      pred.var = v, 
      train = train_data, 
      grid.resolution = 30
    )
    
    p <- autoplot(partial_response, rug = FALSE, alpha = 0.2) +
      labs(title = paste("Partial dependence:", v),
           y = "Predicted corr_z (RF)")
    
    print(p)
  }, error = function(e) {
    message(paste("Error for variable:", v, "->", e$message))
  })
}

#GLM based on highest permutation importance
#Creates formula for glm
sigMain <- c("dif_KippsDist", "dif_wingLen", "dif_tailLen", "dif_mass")
formula_string <- paste("corr", "~", paste(sigMain, collapse = " + "))

glm_model <- glm(formula_string, data = mbbs, family="gaussian")
summary(glm_model)

#devianceResiduals <- residuals(glm_model)
#devianceResiduals
#Residuals vs. Fitted Values Plot
plot(glm_model, which = 1)

plot_model(glm_model, vline.color = "red", show.values = TRUE, value.offset = .3)
