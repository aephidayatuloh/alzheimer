library(tidymodels)
library(readr)

alzheimer <- 
  read_csv("data/alzheimers_disease_patient_data.csv")

alzheimer <- 
  alzheimer |> 
  select(-PatientID, -DoctorInCharge) |> 
  mutate(Diagnosis = factor(Diagnosis, levels = c(1, 0), labels = c("Yes", "No")))

alzheimer |> 
  count(Diagnosis)

alzheimer_split <- 
  alzheimer |> 
  initial_split(prop = 0.7, strata = Diagnosis)

set.seed(1001)
alzheimer_train <- 
  alzheimer_split |> 
  training()

alzheimer_test <- 
  alzheimer_split |> 
  testing()

alzheimer_folds <- 
  alzheimer_train |> 
  vfold_cv(v = 5)

alzheimer_recipe <- 
  alzheimer_train |> 
  # recipe(Diagnosis ~ Age + Gender + Ethnicity + EducationLevel + BMI + 
  #             Smoking + AlcoholConsumption + PhysicalActivity + 
  #             DietQuality + SleepQuality + FamilyHistoryAlzheimers + 
  #             CardiovascularDisease + Diabetes + Depression + 
  #             HeadInjury + Hypertension + SystolicBP + DiastolicBP + 
  #             CholesterolTotal + CholesterolLDL + CholesterolHDL + 
  #             CholesterolTriglycerides + MMSE + FunctionalAssessment + 
  #             MemoryComplaints + BehavioralProblems + ADL + Confusion + 
  #             Disorientation + PersonalityChanges + 
  #             DifficultyCompletingTasks + Forgetfulness) |> 
  recipe(Diagnosis ~ .) |> 
  step_mutate(skip = FALSE, 
      Gender = case_match(
        Gender, 
        0 ~ "Male", 
        .default = "Female"
      ), 
      Ethnicity = case_match(
        Ethnicity, 
        0 ~ "Caucasian", 
        1 ~ "African American", 
        2 ~ "Asian", 
        3 ~ "Other", 
        .default = as.character(Ethnicity)
      ), 
      EducationLevel = case_match(
        EducationLevel, 
        0 ~ "None", 
        1 ~ "High School",
        2 ~ "Bachelor's", 
        3 ~ "Higher", 
        .default = as.character(EducationLevel)
      ), 
      Smoking = case_match(
        Smoking, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      FamilyHistoryAlzheimers = case_match(
        FamilyHistoryAlzheimers, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      CardiovascularDisease = case_match(
        CardiovascularDisease, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      Diabetes = case_match(
        Diabetes, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      Depression = case_match(
        Depression, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      HeadInjury = case_match(
        HeadInjury,
        0 ~ "No", 
        .default = "Yes"
      ), 
      Hypertension = case_match(
        Hypertension, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      MemoryComplaints = case_match(
        MemoryComplaints, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      BehavioralProblems = case_match(
        BehavioralProblems, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      Confusion = case_match(
        Confusion, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      Disorientation = case_match(
        Disorientation, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      PersonalityChanges = case_match(
        PersonalityChanges, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      DifficultyCompletingTasks = case_match(
        DifficultyCompletingTasks, 
        0 ~ "No", 
        .default = "Yes"
      ), 
      Forgetfulness = case_match(
        Forgetfulness, 
        0 ~ "No", 
        .default = "Yes"
      )
    ) |> 
  step_mutate(across(.cols = where(is.character), .fns = factor), skip = FALSE)

alzheimer_dummy <- alzheimer_recipe |> 
  step_dummy(all_factor_predictors())

logreg_spec <- 
  logistic_reg() |> 
  set_engine("glm")

dtree_spec <- 
  decision_tree(cost_complexity = tune(), tree_depth = tune()) |> 
  set_engine("rpart") |> 
  set_mode("classification")

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |> 
  set_engine("ranger") |> 
  set_mode("classification")

xgb_spec <- 
  boost_tree(mtry = tune(), min_n = tune(), learn_rate = tune(), trees = 1000) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

catboost_spec <- boost_tree(min_n = tune(), learn_rate = tune(), tree_depth = tune(), trees = 1000) |> 
  set_engine("catboost") |> 
  set_mode("classification")


model_spec <- 
  workflow_set(
    preproc = list(
      dummy = alzheimer_dummy, 
      base = alzheimer_recipe, 
      base = alzheimer_recipe, 
      dummy = alzheimer_dummy
    ), 
    models = list(
      logreg = logreg_spec, 
      dtree = dtree_spec, 
      rf = rf_spec, 
      xgb = xgb_spec
    ), 
    cross = FALSE
  )

model_fit <- model_spec |> 
  workflow_map(
    fn = "tune_grid", 
    resamples = alzheimer_folds, 
    grid = 50, metrics = metric_set(accuracy, roc_auc, f_meas),
    verbose = TRUE, seed = 1001)

model_fit |> 
  collect_metrics() |> 
  pivot_wider(
    id_cols = c(wflow_id, .config), 
    names_from = .metric, 
    values_from = mean
  ) 
  
 model_fit |>
  rank_results(rank_metric = "roc_auc") |>
  filter(.metric == "roc_auc") 

model_fit |> 
  autoplot(rank_metric = "roc_auc", select_best = TRUE)

model_fit |> 
  autoplot(id = "dummy_xgb")

library(desirability2)

best_params <-
  model_fit |> 
  # extract the tuning results for the boosted tree model
  extract_workflow_set_result("dummy_xgb") |> 
  # collect the metrics associated with it
  collect_metrics() |> 
  # pivot the metrics so that each is in a column
  pivot_wider(
    id_cols = c(mtry, min_n, learn_rate), 
    names_from = .metric, 
    values_from = mean
  ) |> 
  mutate(
    # higher roc values are better; detect max and min from the data
    d_roc     = d_max(roc_auc, use_data = TRUE),
    # lower F1 Score are better; detect max and min from the data
    d_e_odds  = d_max(f_meas, use_data = TRUE),
    # compute overall desirability based on d_roc and d_e_odds
    d_overall = d_overall(across(starts_with("d_")))
  ) |> 
  # pick the model with the highest desirability value
  slice_max(d_overall)
best_params <- 
  best_params |> 
  arrange(min_n) |> 
  head(1)
best_params

final_model_fit <- 
  model_fit |> 
  extract_workflow("dummy_xgb") |> 
  finalize_workflow(best_params) |> 
  last_fit(alzheimer_split, metrics = metric_set(accuracy, roc_auc, f_meas))

final_model_fit |> 
  collect_metrics()

final_wf_fit <- 
  final_model_fit |> 
  extract_workflow()
