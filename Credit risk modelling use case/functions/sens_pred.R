sens = function(test, train, variable, factor, amount) {
  # apply jitter to a variable 
  test_changed <- as.data.frame(test)
  set.seed(7)
  test_changed[[variable]] <- jitter(test_changed[[variable]], factor = factor, amount = amount)
  train_changed <- as.data.frame(train)
  set.seed(7)
  train_changed[[variable]] <- jitter(train_changed[[variable]], factor = factor, amount = amount)
  test_changed <- as.h2o(test_changed)
  train_changed <- as.h2o(train_changed)
  # re-train the model 
  aml_changed <- h2o.automl(x = x, y = y,
                            training_frame = train_changed,
                            max_models = 20,
                            seed = 7) # Let's also try with 40-50 models
  best_auc_changed <- h2o.get_best_model(aml_changed, criterion = "auc")
  pred_best_auc_changed <- h2o.predict(best_auc_changed, test_changed)
  perf_best_auc_changed <- h2o.performance(best_auc_changed, test_changed)
  
  # extract changes 
  test <- as.data.frame(test)
  test_changed <- as.data.frame(test_changed)
  pred_best_auc <- as.data.frame(pred_best_auc)
  pred_best_auc_changed <- as.data.frame(pred_best_auc_changed)
  comparison = data.frame(
    change_in_var = test_changed[variable] - test[variable],
    change_in_prediction = pred_best_auc_changed$p1 - pred_best_auc$p1)
  colnames(comparison)[1] <- "change_in_var"
  g1 = ggplot(data = comparison,
              mapping = aes(x = change_in_var, 
                            y = change_in_prediction)) +  
    geom_point(size = 1.2, 
               shape = "diamond", 
               colour = "cornflowerblue") +
    geom_smooth()
  
  check_lm <- lm(change_in_prediction ~ change_in_var, data=comparison)
  comparison_full <- comparison
  comparison_full$original_pred <- pred_best_auc$p1
  comparison_full$changed_pred <- pred_best_auc_changed$p1
  
  for (i in 1:length(comparison_full$original_pred)) {
    if (comparison_full$original_pred[i] < best_auc_changed@model[["default_threshold"]]) {
      comparison_full$original_class[i] <- 0
    } else {
      comparison_full$original_class[i] <- 1
    }
  }
  
  for (i in 1:length(comparison_full$changed_pred)) {
    if (comparison_full$changed_pred[i] < best_auc_changed@model[["default_threshold"]]) {
      comparison_full$changed_class[i] <- 0
    } else {
      comparison_full$changed_class[i] <- 1
    }
  }
  
  for (i in 1:length(comparison_full$changed_pred)) {
    if (comparison_full$original_class[i] == comparison_full$changed_class[i]) {
      comparison_full$change_in_class[i] <- 0
    } else {
      comparison_full$change_in_class[i] <- 1
    }
  }
  
  final_table <- rbind(
    difference_in_AUC = h2o.auc(perf_best_auc) - h2o.auc(perf_best_auc_changed),
    difference_in_AUCPR = h2o.aucpr(perf_best_auc) - h2o.aucpr(perf_best_auc_changed),
    mean_PD_change = summary(comparison_full$change_in_prediction)[4],
    mean_change_in_variable = summary(comparison_full$change_in_var)[4],
    min_PD_change = summary(comparison_full$change_in_prediction)[1],
    max_PD_change = summary(comparison_full$change_in_prediction)[6],
    class_change = sum(comparison_full$change_in_class),
    correlation = cor(comparison$change_in_prediction, comparison$change_in_var), 
    coef_lm = check_lm[["coefficients"]][["change_in_var"]],
    p_value = summary(check_lm)$coefficients["change_in_var", "Pr(>|t|)"]
  )
  
  colnames(final_table)[1] <- "Small jitter"
  return(list(final_table, g1))
}
