## simulation functions

generate_data_linear <- function(n = 1000,
                                       p = 10,
                                       min_obs = 20,
                                       max_levels = 7,
                                       seed = 123,
                                       cont_strength = 1.2,
                                       cat_strength = 0.8,
                                       seq_cat = TRUE,
                                       intercept_range = c(-0.5, 0.5),
                                       max_redraw = 50) {
  set.seed(seed)
  
  # split into continuous and categorical variables
  p_cont <- floor(p / 2)
  p_cat  <- p - p_cont
  
  if (p_cont < 1 && p_cat > 0) {
    stop("Need at least one continuous variable if categorical variables are generated from them.")
  }
  
  # --------------------------------------------------
  # 1. Generate continuous variables
  # --------------------------------------------------
  if (p_cont > 0) {
    A <- matrix(runif(p_cont * p_cont, min = 0.05, max = 0.95), nrow = p_cont)
    Sigma <- crossprod(A)
    mu <- rep(0, p_cont)
    cont_mat <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
    cont_df  <- as.data.frame(cont_mat)
    names(cont_df) <- paste0("X", seq_len(p_cont))
  } else {
    cont_df <- NULL
  }
  
  # --------------------------------------------------
  # 2. Generate categorical variables from multinomial logit
  # --------------------------------------------------
  if (p_cat > 0) {
    cat_levels <- sample(2:max_levels, p_cat, replace = TRUE)
    cat_df <- data.frame(matrix(NA, nrow = n, ncol = p_cat))
    
    for (i in seq_len(p_cat)) {
      num_levels <- cat_levels[i]
      
      success <- FALSE
      tries <- 0
      
      while (!success && tries < max_redraw) {
        tries <- tries + 1
        
        # --- design matrix for linear predictor ---
        X_parts <- list()
        
        if (p_cont > 0) {
          Xc <- as.matrix(cont_df)
          X_parts[[length(X_parts) + 1]] <- scale(Xc)
        }
        
        if (seq_cat && i > 1) {
          # keep this consistent with your original setup:
          # previous categoricals enter as integer-coded predictors
          prev_cat_num <- sapply(cat_df[, seq_len(i - 1), drop = FALSE], as.integer)
          prev_cat_num <- as.matrix(prev_cat_num)
          X_parts[[length(X_parts) + 1]] <- scale(prev_cat_num)
        }
        
        X_pred <- do.call(cbind, X_parts)
        q <- ncol(X_pred)
        
        if (q < 1) {
          stop("No predictors available to generate categorical variables.")
        }
        
        # --- coefficients for non-baseline categories ---
        beta_mat <- matrix(0, nrow = q, ncol = num_levels - 1)
        
        if (p_cont > 0) {
          n_cont_pred <- p_cont
          beta_mat[1:n_cont_pred, ] <- matrix(
            rnorm(n_cont_pred * (num_levels - 1), mean = 0, sd = cont_strength),
            nrow = n_cont_pred,
            ncol = num_levels - 1
          )
        }
        
        if (seq_cat && i > 1) {
          start_idx <- p_cont + 1
          n_cat_pred <- i - 1
          beta_mat[start_idx:(start_idx + n_cat_pred - 1), ] <- matrix(
            rnorm(n_cat_pred * (num_levels - 1), mean = 0, sd = cat_strength),
            nrow = n_cat_pred,
            ncol = num_levels - 1
          )
        }
        
        intercepts <- runif(num_levels - 1,
                            min = intercept_range[1],
                            max = intercept_range[2])
        
        # baseline category has eta = 0
        eta_nonbase <- sweep(X_pred %*% beta_mat, 2, intercepts, "+")
        eta <- cbind(0, eta_nonbase)
        
        # numerically stable softmax
        eta_centered <- eta - apply(eta, 1, max)
        exp_eta <- exp(eta_centered)
        probs <- exp_eta / rowSums(exp_eta)
        
        # draw categories
        y <- apply(probs, 1, function(pr) {
          sample.int(num_levels, size = 1, prob = pr)
        })
        
        # enforce minimum observations per level by redrawing whole variable
        counts <- table(factor(y, levels = seq_len(num_levels)))
        
        if (all(counts >= min_obs)) {
          success <- TRUE
        }
      }
      
      if (!success) {
        stop("Could not generate categorical variable ", i,
             " with at least min_obs observations in all levels after ",
             max_redraw, " attempts. Try smaller min_obs, fewer levels, or weaker intercept/coef settings.")
      }
      
      cat_df[[i]] <- factor(y, levels = seq_len(num_levels))
    }
    
    names(cat_df) <- paste0("C", seq_len(p_cat))
  } else {
    cat_df <- NULL
  }
  
  # --------------------------------------------------
  # 3. Combine and return
  # --------------------------------------------------
  data.frame(cont_df, cat_df, check.names = FALSE)
}


# Helper: sample a truncated normal quantile
sample_quantile <- function(mu = 0.5, sigma = 0.2, lower = 0.1, upper = 0.9) {
  q <- rnorm(1, mean = mu, sd = sigma)
  pmin(pmax(q, lower), upper)
}

# Recursive tree builder: returns a nested list of nodes
build_tree <- function(indices, df_prev, depth = 1,
                       max_depth = 5, min_split = 200, min_bucket = 50) {
  recurse <- function(idx, current_depth) {
    node <- list(indices = idx)
    n <- length(idx)
    # stop if too small or depth limit
    if (n < min_split || current_depth > max_depth) {
      node$is_leaf <- TRUE
      return(node)
    }
    # try up to 5 splits
    for (try in 1:5) {
      split_var <- sample(names(df_prev), 1)
      values    <- df_prev[idx, split_var]
      # if numeric but no variation, make leaf
      if (is.numeric(values) && diff(range(values)) == 0) {
        node$is_leaf <- TRUE
        return(node)
      }
      # if categorical but only one level present, make leaf
      if (!is.numeric(values) && length(unique(values)) < 2) {
        node$is_leaf <- TRUE
        return(node)
      }
      if (is.numeric(values)) {
        q      <- sample_quantile()
        thresh <- quantile(values, q, names = FALSE)
        left_idx  <- idx[values <= thresh]
        right_idx <- idx[values >  thresh]
        split_value <- thresh
      } else {
        levs           <- unique(values)
        k              <- length(levs)
        pick_n         <- sample(1:(k-1), 1)
        subset_levels  <- sample(levs, pick_n)
        left_idx  <- idx[values %in% subset_levels]
        right_idx <- idx[!(values %in% subset_levels)]
        split_value <- subset_levels
      }
      if (length(left_idx) >= min_bucket && length(right_idx) >= min_bucket) {
        node$is_leaf     <- FALSE
        node$split_var   <- split_var
        node$split_value <- split_value
        node$left        <- recurse(left_idx,  current_depth + 1)
        node$right       <- recurse(right_idx, current_depth + 1)
        return(node)
      }
      message(sprintf(
        "  Split attempt %d at depth %d on %s failed (sizes: %d, %d)",
        try, current_depth, split_var, length(left_idx), length(right_idx)
      ))
    }
    warning(sprintf("Could not split node at depth %d; making leaf", current_depth))
    node$is_leaf <- TRUE
    return(node)
  }
  recurse(indices, depth)
}

# Generate continuous variable based on tree leaves

generate_continuous <- function(tree, df_prev, a, b) {
  
  N <- nrow(df_prev)
  X <- numeric(N)
  
  assign_leaf <- function(node, interval) {
    
    l <- interval[1]
    u <- interval[2]
    
    if (node$is_leaf) {
      
      idx <- node$indices
      mu <- (l + u) / 2
      sigma <- (u - l) / 8
      
      X[idx] <<- rnorm(
        length(idx),
        mean = mu,
        sd = sigma
      )
      
    } else {
      
      # Split the current interval between the two child nodes
      mid <- (l + u) / 2
      
      assign_leaf(
        node$left,
        c(l, mid)
      )
      
      assign_leaf(
        node$right,
        c(mid, u)
      )
    }
  }
  
  assign_leaf(tree, c(a, b))
  
  X
}

# Generate categorical variable based on tree leaves
generate_categorical <- function(tree, df_prev) {
  N <- nrow(df_prev)
  # collect leaves
  leaves <- list()
  collect <- function(node) {
    if (node$is_leaf) leaves[[length(leaves) + 1]] <<- node
    else {
      collect(node$left)
      collect(node$right)
    }
  }
  collect(tree)
  L <- length(leaves)
  K <- sample(2:7, 1)
  
  # ensure each category assigned to at least one leaf
  leaf_cats <- integer(L)
  ord       <- sample(L)
  leaf_cats[ord[1:K]]       <- seq_len(K)
  leaf_cats[ord[(K+1):L]]   <- sample(seq_len(K), L - K, replace = TRUE)
  
  # label each observation
  C <- integer(N)
  for (i in seq_along(leaves)) {
    idx        <- leaves[[i]]$indices
    assigned_c <- leaf_cats[i]
    for (j in idx) {
      C[j] <- if (runif(1) < 0.8) assigned_c else sample((1:K)[-assigned_c], 1)
    }
  }
  if (any(table(C) < 20)) {
    stop("Could not assign categories with ≥20 observations each after 5 tries")
  }
  factor(C)  # return as factor
}

# Main simulation function producing numeric Xs and factor Cs
generate_data_hierarchical <- function(
    N = 10000,
    a = 0, b = 10,
    p = 5,
    seed = 123,
    return_trees = FALSE
) {
  set.seed(seed)
  df    <- data.frame(id = seq_len(N))
  trees <- list()
  
  for (j in seq_len(p)) {
    var_name <- if (j %% 2 == 1) paste0("X", (j + 1) %/% 2) else paste0("C", j %/% 2)
    is_cont  <- (j %% 2 == 1)
    message(sprintf("Generating variable %s (%s)…", 
                    var_name, ifelse(is_cont, "continuous", "categorical")))
    if (j == 1 && is_cont) {
      df[[var_name]] <- runif(N, min = a, max = b)
      if (return_trees) trees[[var_name]] <- NULL
    } else {
      df_prev     <- df[, setdiff(names(df), "id"), drop = FALSE]
      tree        <- build_tree(seq_len(N), df_prev)
      if (is_cont) {
        df[[var_name]] <- generate_continuous(tree, df_prev, a, b)
      } else {
        df[[var_name]] <- generate_categorical(tree, df_prev)
      }
      if (return_trees) trees[[var_name]] <- tree
    }
  }
  
  result <- list(data = df[, -1])
  if (return_trees) result$trees <- trees
  invisible(result)
}

