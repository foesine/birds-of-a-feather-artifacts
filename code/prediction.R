## pred functions

## Calculate evaluation metrics for continuous targets
evaluation_metrics_cont <- function(predictions, true_values) {
  residuals <- predictions - true_values
  MAE       <- mean(abs(residuals))
  RMSE      <- sqrt(mean(residuals^2))
  ss_res    <- sum(residuals^2)
  ss_tot    <- sum((true_values - mean(true_values))^2)
  R2        <- if (ss_tot == 0) NA else 1 - ss_res/ss_tot
  data.frame(MAE = MAE, RMSE = RMSE, R_squared = R2)
}


## Calculate evaluation metrics for factored targets with error handling
evaluation_metrics_factor <- function(predictions, true_values) {
  if (length(predictions) != length(true_values)) {
    stop("Length mismatch: ", length(predictions),
         " predictions vs. ", length(true_values), " true values.")
  }
  
  true_values <- factor(true_values)
  predictions <- factor(predictions, levels = levels(true_values))
  if (nlevels(true_values) < 2) {
    stop("The target must have at least two levels.")
  }
  
  cm  <- caret::confusionMatrix(predictions, true_values, mode = "everything")
  acc <- unname(cm$overall["Accuracy"])
  byc <- cm$byClass
  support <- table(true_values)
  total   <- sum(support)
  
  if (is.null(dim(byc))) {
    ## Binary: byClass is a named vector
    sens <- unname(byc["Sensitivity"])
    spec <- unname(byc["Specificity"])
    f1   <- unname(byc["F1"])
  } else {
    ## Multiclass: byClass is a matrix (rows = classes)
    # Align rows to the levels order in true_values (caret rownames often "Class: <level>")
    row_lvls <- sub("^Class: ", "", rownames(byc))
    ord      <- match(levels(true_values), row_lvls)
    
    sens_vec <- byc[ord, "Sensitivity", drop = TRUE]
    spec_vec <- byc[ord, "Specificity", drop = TRUE]
    f1_vec   <- byc[ord, "F1",          drop = TRUE]
    
    # support aligned to levels(true_values)
    supp_vec <- as.numeric(support[levels(true_values)])
    
    # support-weighted averages
    sens <- sum(sens_vec * supp_vec, na.rm = TRUE) / total
    spec <- sum(spec_vec * supp_vec, na.rm = TRUE) / total
    f1   <- sum(f1_vec   * supp_vec, na.rm = TRUE) / total
  }
  
  data.frame(
    Accuracy    = as.numeric(acc),
    Sensitivity = as.numeric(sens),
    Specificity = as.numeric(spec),
    F1          = as.numeric(f1),
    row.names   = NULL
  )
}

discretize_df = function(df, breaks = 5) {
  for (var in colnames(df)) {
    # Check if the variable is not a factor
    if (is.numeric(df[[var]])) {
      
      # Count the frequency of each unique value
      freq_table <- table(df[[var]])
      
      # Calculate the proportion of zeros, ensuring NA is handled
      zero_proportion <- ifelse(!is.na(freq_table[as.character(0)]),
                                freq_table[as.character(0)] / sum(freq_table),
                                0)
      
      # Determine the number of breaks based on zero proportion
      if (zero_proportion > 4/5) {
        new_breaks = 1
      } else if (zero_proportion > 1/4) {
        new_breaks = breaks - 2
      } else if (zero_proportion > 1/5) {
        new_breaks = breaks - 1
      } else {
        new_breaks = breaks
      }
      
      # Separate zeros and non-zeros
      zero_portion = (df[[var]] == 0)
      non_zero_values = df[[var]][!zero_portion]
      
      # Discretize non-zero values
      if (length(non_zero_values) > 0) {
        # Calculate breaks for non-zero values
        range_values = range(non_zero_values, na.rm = TRUE)
        breaks_values = seq(range_values[1], range_values[2], length.out = new_breaks + 1)
        
        # Ensure correct number of labels are created
        labels = sapply(1:(length(breaks_values)-1), function(i)
          paste("(", breaks_values[i], "-", breaks_values[i+1], "]", sep=""))
        
        
        # Use cut to apply these breaks and labels
        discretized_non_zeros = cut(non_zero_values, breaks = breaks_values, labels = labels, include.lowest = TRUE)
        # Combine zero and discretized non-zeros into the original dataframe
        df[[var]] <- factor(ifelse(zero_portion, "0", as.character(discretized_non_zeros)))
      } else {
        # If all values are zero or the number of breaks is zero or negative
        df[[var]] <- factor("0")
      }
    }
  }
  return(df)
}

# parm_pred
parm_pred <- function(data, target_var,
                                   predictor_vars = NULL,
                                   outer_folds = 5,
                                   inner_folds = 3,
                                   seed = 123) {
  set.seed(seed)
  
  if (!target_var %in% names(data)) {
    stop("Target variable '", target_var, "' not found.")
  }
  
  df <- as.data.frame(data)
  
  if (is.null(predictor_vars)) {
    predictor_vars <- setdiff(names(df), target_var)
  }
  
  df <- df[, unique(c(target_var, predictor_vars)), drop = FALSE]
  
  y <- df[[target_var]]
  if (is.character(y)) y <- factor(y)
  
  if (is.factor(y)) {
    task <- "classification"
    levels(y) <- make.names(levels(y))
    df[[target_var]] <- y
  } else if (is.numeric(y) && length(unique(y)) == 2) {
    task <- "classification"
    df[[target_var]] <- factor(y)
  } else if (is.numeric(y)) {
    task <- "regression"
  } else {
    stop("Target must be numeric or factor.")
  }
  
  if (task == "classification") {
    outer_idx <- caret::createFolds(df[[target_var]], k = outer_folds)
  } else {
    outer_idx <- caret::createFolds(seq_len(nrow(df)), k = outer_folds)
  }
  
  form <- stats::as.formula(
    paste(target_var, "~", paste(predictor_vars, collapse = " + "))
  )
  
  results <- lapply(outer_idx, function(test_idx) {
    train_set <- df[-test_idx, , drop = FALSE]
    test_set  <- df[test_idx,  , drop = FALSE]
    
    if (task == "classification") {
      train_set[[target_var]] <- droplevels(train_set[[target_var]])
      test_set[[target_var]] <- factor(
        test_set[[target_var]],
        levels = levels(train_set[[target_var]])
      )
    }
    
    if (task == "regression") {
      mod <- stats::lm(form, data = train_set)
      preds <- as.numeric(stats::predict(mod, newdata = test_set))
      return(evaluation_metrics_cont(preds, test_set[[target_var]]))
    }
    
    if (nlevels(train_set[[target_var]]) == 2) {
      mod <- stats::glm(
        form,
        data = train_set,
        family = stats::binomial(link = "logit")
      )
      
      probs <- stats::predict(mod, newdata = test_set, type = "response")
      levs <- levels(train_set[[target_var]])
      preds <- ifelse(probs >= 0.5, levs[2], levs[1])
      preds <- factor(preds, levels = levs)
      
      return(evaluation_metrics_factor(preds, test_set[[target_var]]))
    }
    
    mod <- nnet::multinom(
      form,
      data = train_set,
      trace = FALSE
    )
    
    preds <- stats::predict(mod, newdata = test_set, type = "class")
    preds <- factor(preds, levels = levels(train_set[[target_var]]))
    
    evaluation_metrics_factor(preds, test_set[[target_var]])
  })
  
  dplyr::bind_rows(results) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE))
}

cart_pred <- function(data, target_var,
                      predictor_vars = NULL,
                      outer_folds = 5, cp_steps = 10,
                      inner_folds = 3, seed = 123) {
  set.seed(seed)
  
  if (!target_var %in% names(data)) stop("Target not in data.")
  
  if (is.null(predictor_vars)) {
    predictor_vars <- setdiff(names(data), target_var)
  }
  
  if (length(predictor_vars) == 0) {
    stop("No predictors supplied for target ", target_var)
  }
  
  keep_vars <- unique(c(target_var, predictor_vars))
  data <- as.data.frame(data[, keep_vars, drop = FALSE])
  
  # ensure factor/character targets are factors
  if (is.character(data[[target_var]])) {
    data[[target_var]] <- factor(data[[target_var]])
  }
  
  # choose task
  if (is.factor(data[[target_var]])) {
    data[[target_var]] <- factor(make.names(as.character(data[[target_var]])))
    data <- data %>% dplyr::mutate(across(where(is.character), as.factor))
    task <- "class"
    summaryFunc <- caret::defaultSummary
  } else if (is.numeric(data[[target_var]])) {
    task <- "reg"
    summaryFunc <- caret::defaultSummary
  } else {
    stop("Target must be numeric or factor.")
  }
  
  inner_ctrl <- caret::trainControl(
    method = "cv",
    number = inner_folds,
    summaryFunction = summaryFunc,
    verboseIter = FALSE
  )
  
  cp_vals  <- 10^seq(log10(1e-4), log10(1e-2), length.out = cp_steps)
  tunegrid <- expand.grid(cp = cp_vals)
  
  if (task == "class") {
    outer_idx <- caret::createFolds(data[[target_var]], k = outer_folds)
  } else {
    outer_idx <- caret::createFolds(seq_len(nrow(data)), k = outer_folds)
  }
  
  x_formula <- stats::as.formula(
    paste(target_var, "~", paste(predictor_vars, collapse = " + "))
  )
  
  results <- lapply(outer_idx, function(test_idx) {
    train_data <- data[-test_idx, , drop = FALSE]
    test_data  <- data[test_idx,  , drop = FALSE]
    
    # inner CV for best cp
    if (task == "class") {
      inner_idx <- caret::createFolds(train_data[[target_var]], k = inner_folds)
    } else {
      inner_idx <- caret::createFolds(seq_len(nrow(train_data)), k = inner_folds)
    }
    
    best_cps <- sapply(inner_idx, function(idx_i) {
      dti <- train_data[-idx_i, , drop = FALSE]
      
      m <- caret::train(
        x_formula,
        data = dti,
        method = "rpart",
        tuneGrid = tunegrid,
        trControl = inner_ctrl,
        control = rpart::rpart.control(maxsurrogate = 0, maxcompete = 1)
      )
      
      m$bestTune$cp
    })
    
    best_cp <- as.numeric(names(sort(table(best_cps), decreasing = TRUE))[1])
    
    # final fit on outer-train only; no extra outer CV here
    final_m <- rpart::rpart(
      formula = x_formula,
      data = train_data,
      method = if (task == "class") "class" else "anova",
      control = rpart::rpart.control(cp = best_cp, maxsurrogate = 0, maxcompete = 1)
    )
    
    preds <- predict(final_m, newdata = test_data)
    
    if (task == "class") {
      if (is.matrix(preds)) {
        preds <- colnames(preds)[max.col(preds, ties.method = "first")]
      }
      preds <- factor(preds, levels = levels(test_data[[target_var]]))
      evaluation_metrics_factor(preds, test_data[[target_var]])
    } else {
      preds <- as.numeric(preds)
      evaluation_metrics_cont(preds, test_data[[target_var]])
    }
  })
  
  df <- dplyr::bind_rows(results)
  df %>% dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE))
}

bn_pred <- function(data, target_var,
                    predictor_vars = NULL,
                    outer_folds = 5,
                    inner_folds = 3,
                    seed = 123) {
  set.seed(seed)
  
  if (!(target_var %in% colnames(data))) {
    stop("Target variable '", target_var, "' not found in the dataset.")
  }
  
  if (is.null(predictor_vars)) {
    predictor_vars <- setdiff(names(data), target_var)
  }
  
  if (length(predictor_vars) == 0) {
    stop("No predictors supplied for target ", target_var)
  }
  
  keep_vars <- unique(c(target_var, predictor_vars))
  data <- as.data.frame(data[, keep_vars, drop = FALSE])
  
  data <- discretize_df(data)
  
  data <- data %>%
    dplyr::mutate(across(where(is.character), as.factor))
  
  data[[target_var]] <- factor(data[[target_var]], levels = unique(data[[target_var]]))
  
  algorithms <- c("tabu")
  
  outer_cv_folds <- caret::createFolds(data[[target_var]], k = outer_folds)
  outer_results <- list()
  
  for (i in seq_along(outer_cv_folds)) {
    outer_test_index <- outer_cv_folds[[i]]
    outer_testData <- data[outer_test_index, , drop = FALSE]
    outer_trainData <- data[-outer_test_index, , drop = FALSE]
    
    if (length(unique(outer_trainData[[target_var]])) < 2) {
      warning("Outer Fold ", i, ": The target variable has less than two levels in the training set.")
      next
    }
    
    inner_folds_indices <- caret::createFolds(outer_trainData[[target_var]], k = inner_folds)
    
    best_performance <- -Inf
    best_algorithm <- NULL
    
    for (algorithm in algorithms) {
      fold_results <- c()
      
      for (j in seq_along(inner_folds_indices)) {
        inner_test_index <- inner_folds_indices[[j]]
        inner_trainData <- outer_trainData[-inner_test_index, , drop = FALSE]
        inner_testData  <- outer_trainData[inner_test_index, , drop = FALSE]
        
        if (length(unique(inner_trainData[[target_var]])) < 2) {
          warning("Inner Fold ", j, ": The target variable has less than two levels in the training set.")
          fold_results[j] <- NA
          next
        }
        
        bn_model <- do.call(
          get(algorithm, envir = asNamespace("bnlearn")),
          list(inner_trainData)
        )
        
        fitted_bn_model <- bnlearn::bn.fit(bn_model, inner_trainData)
        
        predictions <- predict(
          fitted_bn_model,
          node = target_var,
          data = inner_testData,
          method = "bayes-lw"
        )
        
        predictions <- factor(predictions, levels = levels(inner_trainData[[target_var]]))
        accuracy <- mean(predictions == inner_testData[[target_var]], na.rm = TRUE)
        fold_results[j] <- accuracy
      }
      
      avg_performance <- mean(fold_results, na.rm = TRUE)
      
      if (!is.na(avg_performance) && avg_performance > best_performance) {
        best_performance <- avg_performance
        best_algorithm <- algorithm
      }
    }
    
    if (!is.null(best_algorithm) && length(unique(outer_trainData[[target_var]])) >= 2) {
      best_bn <- do.call(
        get(best_algorithm, envir = asNamespace("bnlearn")),
        list(outer_trainData)
      )
      
      best_fitted_bn <- bnlearn::bn.fit(best_bn, outer_trainData)
      
      predictions <- predict(
        best_fitted_bn,
        node = target_var,
        data = outer_testData,
        method = "bayes-lw"
      )
      
      predictions <- factor(predictions, levels = levels(outer_testData[[target_var]]))
      
      eval <- evaluation_metrics_factor(
        predictions = predictions,
        true_values = outer_testData[[target_var]]
      )
      
      outer_results[[i]] <- eval
    } else {
      cat("Skipping evaluation in outer fold", i, "due to insufficient class diversity.\n")
    }
  }
  
  if (length(outer_results) > 0) {
    eval_avg_outer_folds <- dplyr::bind_rows(outer_results) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE))
    return(eval_avg_outer_folds)
  } else {
    warning("No valid folds to evaluate.")
    return(NULL)
  }
}

svm_pred <- function(data, target_var,
                     predictor_vars = NULL,
                     outer_folds = 5,
                     cost_steps = 10,
                     inner_folds = 3,
                     seed = 123) {
  
  set.seed(seed)
  
  if (!target_var %in% names(data)) {
    stop("Target variable ", target_var, " not found in the dataset")
  }
  
  if (is.null(predictor_vars)) {
    predictor_vars <- setdiff(names(data), target_var)
  }
  
  keep_vars <- unique(c(target_var, predictor_vars))
  data <- as.data.frame(data[, keep_vars, drop = FALSE])
  
  y <- data[[target_var]]
  
  if (is.character(y)) y <- factor(y)
  
  if (is.factor(y)) {
    task <- "classification"
  } else if (is.numeric(y)) {
    task <- "regression"
  } else {
    stop("Target must be numeric or factor.")
  }
  
  summaryFunctionType <- if (task == "regression") caret::defaultSummary else caret::multiClassSummary
  
  inner_control <- caret::trainControl(
    method = "cv",
    number = inner_folds,
    summaryFunction = summaryFunctionType,
    verboseIter = FALSE,
    allowParallel = FALSE
  )
  
  cost_values <- 10^seq(log10(0.001), log10(100), length.out = cost_steps)
  tunegrid <- expand.grid(C = cost_values)
  
  outer_cv_folds <- caret::createFolds(data[[target_var]], k = outer_folds)
  
  x_formula <- stats::as.formula(
    paste(target_var, "~", paste(predictor_vars, collapse = " + "))
  )
  
  all_results <- lapply(outer_cv_folds, function(test_idx) {
    train_set <- data[-test_idx, , drop = FALSE]
    test_set  <- data[test_idx, , drop = FALSE]
    
    model <- caret::train(
      x_formula,
      data = train_set,
      method = "svmRadialCost", # radial kernel, just tuning cost parameter
      tuneGrid = tunegrid,
      trControl = inner_control
    )
    
    preds <- predict(model, newdata = test_set)
    
    if (task == "regression") {
      evaluation_metrics_cont(preds, test_set[[target_var]])
    } else {
      evaluation_metrics_factor(preds, test_set[[target_var]])
    }
  })
  
  df_out <- dplyr::bind_rows(all_results)
  df_out %>% dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE))
}


# Run prediction
run_predictions <- function(data_list,
                            prediction_function,
                            seed = 123,
                            restrict_predictors = FALSE,
                            targets_p6  = c("X1","X2","X3","C1","C2","C3"),
                            targets_p12 = c("X2","X4","X6","C2","C4","C6"),
                            targets_p18 = c("X3","X6","X9","C3","C6","C9")) {
  
  set.seed(seed)
  
  predictions <- purrr::imap(data_list, function(dataset, dataset_name) {
    
    p_val <- as.integer(sub("^p(\\d+)_.*$", "\\1", dataset_name))
    
    if (p_val == 6) {
      selected_targets <- targets_p6
    } else if (p_val == 12) {
      selected_targets <- targets_p12
    } else if (p_val == 18) {
      selected_targets <- targets_p18
    } else {
      stop("Unexpected p = ", p_val, " in dataset name ", dataset_name)
    }
    
    all_vars  <- names(dataset)
    cont_vars <- grep("^X", all_vars, value = TRUE)
    cat_vars  <- grep("^C", all_vars, value = TRUE)
    
    cat("\nDataset:", dataset_name, "\n",
        "  Linear predictor restriction:", restrict_predictors, "\n")
    
    res <- purrr::map(selected_targets, function(tv) {
      
      cat("    -> Predicting with target:", tv, "\n")
      
      if (!tv %in% names(dataset)) {
        warning("      Skipping missing target: ", tv)
        return(NULL)
      }
      
      predictor_vars <- setdiff(all_vars, tv)
      
      if (restrict_predictors) {
        
        if (grepl("^X", tv)) {
          predictor_vars <- setdiff(cont_vars, tv)
          
        } else if (grepl("^C", tv)) {
          tv_num <- as.integer(sub("^C", "", tv))
          
          prev_cats <- if (tv_num > 1) {
            paste0("C", seq_len(tv_num - 1))
          } else {
            character(0)
          }
          
          prev_cats <- intersect(prev_cats, cat_vars)
          predictor_vars <- c(cont_vars, prev_cats)
        }
      }
      
      cat("       Predictors:", paste(predictor_vars, collapse = ", "), "\n")
      
      prediction_function(
        data = dataset,
        target_var = tv,
        predictor_vars = predictor_vars,
        outer_folds = 5,
        inner_folds = 3,
        seed = seed
      )
    })
    
    purrr::set_names(res, selected_targets)
  })
  
  save(predictions, file = "predictions_sim_data.RData")
  predictions
}


run_prediction_grid <- function(
    data_object,
    prediction_function,
    output_file,
    p_values = c(6, 12, 18),
    mc_values = 1:5,
    synmc_values = NULL,
    restrict_predictors = TRUE,
    workers = 2,
    seed = 123,
    output_dir = here::here("data")) {
  
  tasks <- expand.grid(
    p = p_values,
    mc = mc_values,
    synmc = if (is.null(synmc_values)) {
      NA_integer_
    } else {
      synmc_values
    },
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      inner_name = if (is.null(synmc_values)) {
        paste0("p", p, "_mc", mc)
      } else {
        paste0("p", p, "_mc", mc, "_synmc", synmc)
      })
  
  missing_names <- tasks$inner_name[
    !tasks$inner_name %in% names(data_object)]
  
  if (length(missing_names) > 0) {
    stop(
      "These datasets are missing in data_object:\n",
      paste(missing_names, collapse = ", "))
  }
  
  future::plan(
    future::multisession,
    workers = workers)
  
  results <- furrr::future_map(
    seq_len(nrow(tasks)),
    function(i) {
      sim_data <- data_object[[tasks$inner_name[i]]]
      
      res <- run_predictions(
        data_list = stats::setNames(
          list(sim_data),
          tasks$inner_name[i]),
        prediction_function = prediction_function,
        restrict_predictors = restrict_predictors,
        seed = seed)
      
      res[[1]]
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      packages = c(
        "caret", "dplyr", "purrr", "tibble")))
  
  names(results) <- tasks$inner_name
  
  assign(
    output_file,
    results,
    envir = .GlobalEnv)
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE)
  
  output_path <- file.path(
    output_dir,
    paste0(output_file, ".RData"))
  
  save(
    list = output_file,
    file = output_path,
    envir = .GlobalEnv)
  
  results
}