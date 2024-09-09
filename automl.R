library(tidymodels)
library(readr)
library(h2o)
library(agua)
h2o.init(max_mem_size = "10G")

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
  step_mutate(across(.cols = where(is.character), .fns = factor), skip = FALSE) |> 
  step_normalize(all_numeric_predictors())

automl_spec <- 
  auto_ml() |> 
  set_engine(
      engine = "h2o", 
      max_runtime_secs_per_model = 60, 
      seed = 1001, stopping_metric = "AUC", 
      keep_cross_validation_predictions = TRUE, 
      keep_cross_validation_models = TRUE, 
      keep_cross_validation_fold_assignment = FALSE, 
      save_data = TRUE, 
      validation = 0.2, 
      nfolds = 5, leaderboard_frame = alzheimer_test, 
      verbosity = "info", 
      project_name = "Alzheimer"
) |> 
  set_mode("classification")

automl_wf <- 
  workflow(
    preprocessor = alzheimer_recipe, 
    spec = automl_spec
  )

automl_fit <- automl_wf |> 
  fit(data = alzheimer_train)

  
automl_fit 

pred_automl_train <- 
  automl_fit |> 
  augment(alzheimer_train) |> 
  mutate(type = "Training")

pred_automl_test <- 
  automl_fit |> 
  augment(alzheimer_test) |> 
  mutate(type = "Testing")

