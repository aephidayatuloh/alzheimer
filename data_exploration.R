library(tidyverse)

alzheimer <- 
  read_csv("data/alzheimers_disease_patient_data.csv")

alzheimer |> 
  glimpse()

alzheimer |> 
  skimr::skim_without_charts()

alzheimer |> 
  nrow()

alzheimer |> 
  count(PatientID)

alzheimer |> 
  count(DoctorInCharge)

alzheimer |> 
  select(PhysicalActivity) |> 
  print(n = 1000)

alzheimer |> 
  select(DietQuality) |> 
  print(n = 1000)

alzheimer |> 
  select(SleepQuality) |> 
  print(n = 1000)

alzheimer |> 
  select(MMSE) |> 
  print(n = 1000)

alzheimer |> 
  select(FunctionalAssessment) |> 
  print(n = 1000)

alzheimer |> 
  select(ADL) |> 
  print(n = 1000)

alzheimer <- 
  alzheimer |> 
    select(-PatientID, -DoctorInCharge) |> 
    mutate(
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
      ), 
      Diagnosis = case_match(
        Diagnosis, 
        0 ~ "No", 
        .default = "Yes"
      )
    )

alzheimer

theme_set(theme_light())

alzheimer |> 
  ggplot(aes(x = Age)) + 
  geom_histogram(binwidth = 2, color = "grey") + 
  scale_x_continuous(breaks = seq(60, 90, by = 4))

alzheimer |> 
  ggplot(aes(x = Age)) + 
  geom_boxplot() + 
  scale_x_continuous(breaks = seq(60, 90, by = 4))

alzheimer |> 
  ggplot(aes(x = BMI)) + 
  geom_histogram(binwidth = 1, color = "grey") + 
  scale_x_continuous(breaks = seq(15, 50, by = 2))

alzheimer |> 
  ggplot(aes(x = AlcoholConsumption)) + 
  geom_histogram(binwidth = 0.25, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 20, by = 0.5))

alzheimer |> 
  select(PhysicalActivity) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = AlcoholConsumption)) + 
  geom_histogram(binwidth = 0.25, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 10, by = 0.5))

alzheimer |> 
  select(DietQuality) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = DietQuality)) + 
  geom_histogram(binwidth = 0.25, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 10, by = 0.5))

alzheimer |> 
  select(SleepQuality) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = SleepQuality)) + 
  geom_histogram(binwidth = 0.25, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 10, by = 0.5))

alzheimer |> 
  select(SystolicBP) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = SystolicBP)) + 
  geom_histogram(binwidth = 2.5, color = "grey") + 
  scale_x_continuous(breaks = seq(90, 180, by = 5))

alzheimer |> 
  select(DiastolicBP) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = DiastolicBP)) + 
  geom_histogram(binwidth = 0.25, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 10, by = 0.5))

alzheimer |> 
  select(CholesterolTotal) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = CholesterolTotal)) + 
  geom_histogram(binwidth = 5, color = "grey") + 
  scale_x_continuous(breaks = seq(150, 300, by = 10))

alzheimer |> 
  select(CholesterolHDL) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = CholesterolHDL)) + 
  geom_histogram(binwidth = 2.5, color = "grey") + 
  scale_x_continuous(breaks = seq(20, 100, by = 5))

alzheimer |> 
  select(CholesterolLDL) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = CholesterolLDL)) + 
  geom_histogram(binwidth = 2.5, color = "grey") + 
  scale_x_continuous(breaks = seq(50, 200, by = 5))

alzheimer |> 
  select(CholesterolTriglycerides) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = CholesterolTriglycerides)) + 
  geom_histogram(binwidth = 10, color = "grey") + 
  scale_x_continuous(breaks = seq(50, 400, by = 20))

alzheimer |> 
  select(MMSE) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = MMSE)) + 
  geom_histogram(binwidth = 1, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 30, by = 2))

alzheimer |> 
  select(FunctionalAssessment) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = FunctionalAssessment)) + 
  geom_histogram(binwidth = 0.5, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))

alzheimer |> 
  select(FunctionalAssessment) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = FunctionalAssessment)) + 
  geom_histogram(binwidth = 0.5, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))

alzheimer |> 
  select(ADL) |> 
  summary()

alzheimer |> 
  ggplot(aes(x = ADL)) + 
  geom_histogram(binwidth = 0.5, color = "grey") + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))

alzheimer |> 
  count(Diagnosis) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Gender) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Gender, Diagnosis) |> 
  group_by(Gender) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Gender, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(Ethnicity) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Ethnicity, Diagnosis) |> 
  group_by(Ethnicity) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Ethnicity, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(EducationLevel) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(EducationLevel, Diagnosis) |> 
  group_by(EducationLevel) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = EducationLevel, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(Smoking) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Smoking, Diagnosis) |> 
  group_by(Smoking) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Smoking, fill = Diagnosis)) + 
  geom_bar(position = position_fill())

alzheimer |> 
  count(FamilyHistoryAlzheimers) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(FamilyHistoryAlzheimers, Diagnosis) |> 
  group_by(FamilyHistoryAlzheimers) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = FamilyHistoryAlzheimers, fill = Diagnosis)) + 
  geom_bar(position = position_fill())

alzheimer |> 
  count(CardiovascularDisease) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(CardiovascularDisease, Diagnosis) |> 
  group_by(FamilyHistoryAlzheimers) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = CardiovascularDisease, fill = Diagnosis)) + 
  geom_bar(position = position_fill())

alzheimer |> 
  count(Diabetes) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Diabetes, Diagnosis) |> 
  group_by(Diabetes) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Diabetes, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(Depression) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Depression, Diagnosis) |> 
  group_by(Depression) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Depression, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(HeadInjury) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(HeadInjury, Diagnosis) |> 
  group_by(HeadInjury) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = HeadInjury, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(Hypertension) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Hypertension, Diagnosis) |> 
  group_by(Hypertension) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Hypertension, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(MemoryComplaints) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(MemoryComplaints, Diagnosis) |> 
  group_by(MemoryComplaints) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = MemoryComplaints, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(BehavioralProblems) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(BehavioralProblems, Diagnosis) |> 
  group_by(BehavioralProblems) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = BehavioralProblems, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(Confusion) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Confusion, Diagnosis) |> 
  group_by(Confusion) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Confusion, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(Disorientation) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Disorientation, Diagnosis) |> 
  group_by(Disorientation) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Confusion, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(PersonalityChanges) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(PersonalityChanges, Diagnosis) |> 
  group_by(PersonalityChanges) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = PersonalityChanges, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(DifficultyCompletingTasks) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(DifficultyCompletingTasks, Diagnosis) |> 
  group_by(DifficultyCompletingTasks) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = DifficultyCompletingTasks, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

alzheimer |> 
  count(Forgetfulness) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  count(Forgetfulness, Diagnosis) |> 
  group_by(Forgetfulness) |> 
  mutate(pct = n/sum(n))

alzheimer |> 
  ggplot(aes(x = Forgetfulness, fill = Diagnosis)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = "Percentage")

library(ggcorrplot)

corr <- 
  alzheimer |> 
  select(where(is.numeric)) |> 
  cor()

pmat <- 
  alzheimer |> 
  select(where(is.numeric)) |> 
  cor() |> 
  cor_pmat()

ggcorrplot(corr, type = "lower", show.diag = FALSE, p.mat = pmat, 
    colors = c("firebrick", "white", "green")
)
