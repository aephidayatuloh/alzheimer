library(tidyverse)
library(tidymodels)

pred_automl_train |> 
  conf_mat(truth = Diagnosis, estimate = .pred_class)
pred_automl_test |> 
  conf_mat(truth = Diagnosis, estimate = .pred_class)

pred_automl_train |> 
  accuracy(truth = Diagnosis, estimate = .pred_class)
pred_automl_test |> 
  accuracy(truth = Diagnosis, estimate = .pred_class)

pred_automl_train |> 
  bal_accuracy(truth = Diagnosis, estimate = .pred_class)
pred_automl_test |> 
  bal_accuracy(truth = Diagnosis, estimate = .pred_class)

pred_automl_train |> 
  sensitivity(truth = Diagnosis, estimate = .pred_class)
pred_automl_test |> 
  sensitivity(truth = Diagnosis, estimate = .pred_class)

pred_automl_train |> 
  specificity(truth = Diagnosis, estimate = .pred_class)
pred_automl_test |> 
  specificity(truth = Diagnosis, estimate = .pred_class)

pred_automl_train |> 
  recall(truth = Diagnosis, estimate = .pred_class)
pred_automl_test |> 
  recall(truth = Diagnosis, estimate = .pred_class)

pred_automl_train |> 
  precision(truth = Diagnosis, estimate = .pred_class)
pred_automl_test |> 
  precision(truth = Diagnosis, estimate = .pred_class)

pred_automl_train |> 
  f_meas(truth = Diagnosis, estimate = .pred_class)
pred_automl_test |> 
  f_meas(truth = Diagnosis, estimate = .pred_class)



pred_automl_train |> 
  roc_auc(truth = Diagnosis, .pred_Yes)
pred_automl_test |> 
  roc_auc(truth = Diagnosis, .pred_Yes)

pred_automl_train |> 
  roc_curve(truth = Diagnosis, .pred_Yes) |> 
  autoplot()
pred_automl_test |> 
  roc_curve(truth = Diagnosis, .pred_Yes) |> 
  autoplot()
