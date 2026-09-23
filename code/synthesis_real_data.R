## synthesis functions

# Function to apply 3rd root transformation to continuous variables
transform_continuous <- function(col) {
  return(sign(col) * abs(col)^(1/3))
}

# Function to retransform continuous variables back to original scale
retransform_continuous <- function(col) {
  return(col^3)
}

discretize_df = function(df, breaks = 5) {
  for (var in colnames(df)) {
    # Check if the variable is not a factor
    if (!is.factor(df[[var]])) {
      cat("Discretizing variable:", var, "\n")
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


synthesize_data_parametric <- function(data, first_var, seed = 123) {
  set.seed(seed)
  if (!first_var %in% names(data)) {
    stop("Column '", first_var, "' not found in the data.")
  }
  n <- nrow(data)
  syn <- data.frame(matrix(NA, nrow = n, ncol = ncol(data)))
  names(syn) <- names(data)
  # sample first variable marginally
  syn[[first_var]] <- sample(data[[first_var]], n, replace = TRUE)
  for (var_j in setdiff(names(data), first_var)) {
    message("Synthesizing ", var_j)
    filled_preds <- names(syn)[!is.na(syn[1, ])]
    preds <- setdiff(filled_preds, var_j)
    train_df <- data[, c(preds, var_j), drop = FALSE]
    syn_preds_df <- syn[, preds, drop = FALSE]
    if (is.factor(data[[var_j]]) || is.character(data[[var_j]])) {
      y_fact <- factor(train_df[[var_j]])
      levs <- levels(y_fact)
      class_counts <- table(y_fact)
      # fallback if any class is too rare
      if (any(class_counts < 2)) {
        warning("Variable ", var_j, 
                " has class(es) with < 2 observations. Using marginal sampling instead.")
        syn[[var_j]] <- factor(
          sample(y_fact, n, replace = TRUE),
          levels = levs
        )
        next
      }
      # use model.matrix instead of as.matrix
      xmat <- model.matrix(~ . - 1, data = train_df[, preds, drop = FALSE])
      newx <- model.matrix(~ . - 1, data = syn_preds_df)
      # align columns in case some factor levels are missing in newx
      missing_cols <- setdiff(colnames(xmat), colnames(newx))
      if (length(missing_cols) > 0) {
        add_mat <- matrix(0, nrow = nrow(newx), ncol = length(missing_cols))
        colnames(add_mat) <- missing_cols
        newx <- cbind(newx, add_mat)
      }
      newx <- newx[, colnames(xmat), drop = FALSE]
      cvfit <- cv.glmnet(
        x = xmat,
        y = y_fact,
        family = "multinomial",
        alpha = 0.5
      )
      prob_array <- predict(
        cvfit,
        newx = newx,
        type = "response",
        s = "lambda.min"
      )
      if (length(dim(prob_array)) == 3) {
        probs <- prob_array[, , 1]
      } else {
        probs <- prob_array
      }
      colnames(probs) <- levs
      syn_cat <- apply(probs, 1, function(pr) {
        pr[is.na(pr)] <- 0
        if (sum(pr) <= 0) {
          sample(levs, 1)
        } else {
          sample(levs, 1, prob = pr / sum(pr))
        }
      })
      syn[[var_j]] <- factor(syn_cat, levels = levs)
    } else {
      # continuous variable
      df_fit <- data.frame(
        train_df[, preds, drop = FALSE],
        y = train_df[[var_j]],
        check.names = FALSE
      )
      lmfit <- lm(y ~ ., data = df_fit)
      sigma_hat <- summary(lmfit)$sigma
      syn_df <- data.frame(syn_preds_df, check.names = FALSE)
      syn_pred <- predict(lmfit, newdata = syn_df)
      syn_cont <- syn_pred + rnorm(n, mean = 0, sd = sigma_hat)
      syn[[var_j]] <- syn_cont
    }
  }
  for (v in names(data)) {
    if (is.factor(data[[v]])) {
      syn[[v]] <- factor(syn[[v]], levels = levels(data[[v]]))
    }
  }
  syn
}

synthesize_data_cart <- function(data, first_var, seed = 123) {    
  syn_obj <- synthpop::syn(data, visit.sequence = c(first_var, setdiff(colnames(data), first_var)), method = "cart", seed = seed)
  return(syn_obj$syn)
}

synthesize_data_bn <- function(data, seed = 123) {
  data <- as.data.frame(data)
  # convert unsupported classes before discretization
  for (var in names(data)) {
    if (is.character(data[[var]]) || is.logical(data[[var]])) {
      data[[var]] <- factor(data[[var]])
    }
  }
  # discretize numeric variables using external function
  data <- discretize_df(data)
  # final safeguard for bnlearn
  for (var in names(data)) {
    if (!is.factor(data[[var]])) {
      data[[var]] <- factor(data[[var]])
    }
  }
  set.seed(seed)
  bn_structure <- tabu(data)
  bn_fitted <- bn.fit(bn_structure, data, method = "bayes", iss = 1)
  syn_data <- rbn(bn_fitted, n = nrow(data))
  for (var in names(data)) {
    syn_data[[var]] <- factor(syn_data[[var]], levels = levels(data[[var]]))
  }
  return(syn_data)
}

synthesize_data_svm <- function(data, first_var,
                                C_class      = 1,
                                C_reg        = 1,
                                epsilon      = 0.1,
                                k_neighbors  = 10,
                                seed         = 123) {
  set.seed(seed)
  
  # Copy original data
  orig <- data.frame(data, stringsAsFactors = FALSE)
  
  # Initialize synthetic data frame
  syn_data <- data.frame(matrix(NA, nrow = nrow(orig), ncol = ncol(orig)))
  names(syn_data) <- names(orig)
  
  # Sample first_var 
  syn_data[[first_var]] <- sample(orig[[first_var]], nrow(orig), replace = TRUE)
  
  # Seq syn
  for (var_j in setdiff(names(orig), first_var)) {
    
    # predictors
    preds_filled <- names(syn_data)[!is.na(syn_data[1, ])]
    predictors   <- setdiff(preds_filled, var_j)
    
    # training data
    train_df <- orig[, c(predictors, var_j), drop = FALSE]
    
    # synpreds for var_j
    syn_preds_df <- syn_data[, predictors, drop = FALSE]
    if (is.factor(orig[[var_j]]) || is.character(orig[[var_j]])) {
      
      # Classification
      train_df[[var_j]] <- factor(train_df[[var_j]])
      levs <- levels(train_df[[var_j]])
      model_cl <- svm(
        as.formula(paste(var_j, "~ .")),
        data        = train_df,
        probability = TRUE,
        kernel      = "radial",
        cost        = C_class
      )
      pred_obj <- predict(model_cl, newdata = syn_preds_df, probability = TRUE)
      prob_mat <- attr(pred_obj, "probabilities")
      
      # sample each row according to its probability vector
      syn_data[[var_j]] <- apply(prob_mat, 1, function(p_row) {
        sample(levs, size = 1, prob = p_row)
      })
      syn_data[[var_j]] <- factor(syn_data[[var_j]], levels = levs)
    } else if (is.numeric(orig[[var_j]])) {
      
      # Regression
      model_rg <- svm(
        as.formula(paste(var_j, "~ .")),
        data    = train_df,
        type    = "eps-regression",
        kernel  = "radial",
        cost    = C_reg,
        epsilon = epsilon
      )
      
      # train predictions & residuals
      preds_train <- predict(model_rg, newdata = train_df)
      resid_train <- train_df[[var_j]] - preds_train
      
      # synthetic predictions
      syn_preds <- predict(model_rg, newdata = syn_preds_df)
      
      # Find kNN among train preds for each syn_pred
      nn <- get.knnx(
        data  = matrix(preds_train, ncol = 1),
        query = matrix(syn_preds,   ncol = 1),
        k     = k_neighbors
      )$nn.index  
      
      # sample one residual each
      sampled_resid <- apply(nn, 1, function(ix) {
        sample(resid_train[ix], 1)
      })
      syn_data[[var_j]] <- syn_preds + sampled_resid
    } else {
      stop("Variable ", var_j, " is neither factor nor numeric.")
    }
  }
  return(syn_data)
}

synth_data <- function(simulated_data, synth_method, mc_reps = 5) {
  # allow user to pass a single data frame directly
  if (is.data.frame(simulated_data)) {
    simulated_data <- list(dataset1 = simulated_data)
  }
  purrr::imap(simulated_data, function(.x, .y) {
    cat("Synthesizing dataset:", .y, "with", synth_method$name, "\n")
    data <- as.data.frame(.x)
    # detect variable types by class, not by name prefix
    categorical_vars <- names(data)[vapply(data, function(z) is.factor(z) || is.character(z), logical(1))]
    continuous_vars  <- names(data)[vapply(data, is.numeric, logical(1))]
    if (length(categorical_vars) > 0) {
      # score categorical vars to pick first_var
      cat_scores <- sapply(categorical_vars, function(var) {
        tab <- table(data[[var]])
        props <- tab / sum(tab)
        k <- length(props)
        # avoid division by zero if only one category
        if (k <= 1) {
          balance <- 0
        } else {
          gini_raw  <- 1 - sum(props^2)
          gini_norm <- gini_raw / (1 - 1 / k)
          balance   <- gini_norm
        }
        cardinality <- length(tab)
        max_cardinality <- max(
          sapply(categorical_vars, function(v) length(table(data[[v]]))),
          na.rm = TRUE
        )
        score <- 0.8 * balance + 0.2 * (1 - (cardinality / max_cardinality))
        score
      })
      first_var <- names(which.max(cat_scores))
      remaining_categorical_vars <- setdiff(categorical_vars, first_var)
      remaining_categorical_vars <- remaining_categorical_vars[
        order(sapply(remaining_categorical_vars, function(var) length(unique(data[[var]]))))
      ]
      ordered_vars <- c(first_var, continuous_vars, remaining_categorical_vars)
    } else if (length(continuous_vars) > 0) {
      first_var <- continuous_vars[1]
      ordered_vars <- continuous_vars
    } else {
      warning("Dataset ", .y, " contains no supported variables. Skipping synthesis.")
      return(NULL)
    }
    cat("  Using first_var:", first_var, "\n")
    data <- data[, ordered_vars, drop = FALSE]
    synthetic_results <- purrr::map(1:mc_reps, function(mc_iter) {
      set.seed(mc_iter)
      cat("  Applying", synth_method$name, "with seed", mc_iter, "on", .y, "\n")
      args <- list(data = data)
      if ("first_var" %in% names(formals(synth_method$func))) {
        args$first_var <- first_var
      }
      if ("seed" %in% names(formals(synth_method$func))) {
        args$seed <- mc_iter
      }
      do.call(synth_method$func, args)
    })
    names(synthetic_results) <- paste0(.y, "_synmc", 1:mc_reps)
    synthetic_results
  }) |> purrr::flatten()
}


run_all_synthesis <- function(data,
                              methods = c("parm", "cart", "bn", "svm"),
                              mc_reps = 5) {
  # use object name as dataset name
  dataset_name <- deparse(substitute(data))
  # hard-coded output directory
  out_path <- here::here("data", "real_data", "data")
  if (!dir.exists(out_path)) {
    dir.create(out_path, recursive = TRUE)
  }
  # available synthesis methods
  synth_methods <- list(
    parm = list(name = "parm", func = synthesize_data_parametric),
    cart = list(name = "cart", func = synthesize_data_cart),
    bn   = list(name = "bn",   func = synthesize_data_bn),
    svm  = list(name = "svm",  func = synthesize_data_svm)
  )
  # check requested methods
  invalid_methods <- setdiff(methods, names(synth_methods))
  if (length(invalid_methods) > 0) {
    stop("Unknown synthesis method(s): ", paste(invalid_methods, collapse = ", "))
  }
  # keep only requested methods
  synth_methods <- synth_methods[methods]
  # initialize results list
  results <- list()
  # run selected methods one by one
  for (method_name in names(synth_methods)) {
    method_obj <- synth_methods[[method_name]]
    cat("Running", method_name, "for", dataset_name, "\n")
    syn_result <- synth_data(
      simulated_data = data,
      synth_method   = method_obj,
      mc_reps        = mc_reps
    )
    # store in results list immediately
    results[[method_name]] <- syn_result
    # save this method result immediately as one RData file
    syn_list <- syn_result
    save(
      syn_list,
      file = file.path(out_path, paste0(dataset_name, "_syn_", method_name, ".RData"))
    )
    cat("Saved", method_name, "for", dataset_name, "\n")
  }
  return(results)
}