#' Handle Missing Values in Longitudinal Survey Data
#'
#' @title 处理纵向问卷数据的缺失值
#' @description 使用多种方法处理纵向问卷数据中的缺失值，包括FIML和多重插补
#'
#' @param data 数据框，包含纵向问卷数据
#' @param id_col ID列名，默认为"src_subject_id"
#' @param time_col 时间列名，默认为"eventname"
#' @param method 缺失值处理方法，可选"fiml"（完全信息最大似然）或"mi"（多重插补）
#' @param missing_values 缺失值编码向量
#' @param n_imputations 如果使用多重插补，指定插补次数
#' @param missing_threshold 允许的缺失值比例阈值，默认0.2
#'
#' @return 处理后的数据列表，包含处理后的数据和处理报告
#' @export

handle_missing <- function(data,
                           id_col = "src_subject_id",
                           time_col = "eventname",
                           method = c("fiml", "mi"),
                           missing_values = c(NA, 98, 99, 999, -999, -888, "NULL"),
                           n_imputations = 5,
                           missing_threshold = 0.2) {
  # 加载必需的包
  required_packages <- c("lavaan", "mice")
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "needed for this function to work. Please install it."))
    }
  }

  # 1. 检查输入
  method <- match.arg(method)

  # 2. 识别数据列和元数据列
  meta_cols <- c(id_col, time_col, "time")
  data_cols <- setdiff(names(data), meta_cols)

  if(length(data_cols) == 0) {
    stop("No data columns found after removing metadata columns")
  }

  # 3. 分离数据
  meta_data <- data[, meta_cols[meta_cols %in% names(data)], drop = FALSE]
  processed_data <- data[, data_cols, drop = FALSE]

  # 4. 处理缺失值
  for(col in names(processed_data)) {
    if(is.numeric(processed_data[[col]])) {
      processed_data[[col]][processed_data[[col]] %in% missing_values[is.numeric(missing_values)]] <- NA
    }
    if(is.character(processed_data[[col]]) || is.factor(processed_data[[col]])) {
      processed_data[[col]][processed_data[[col]] %in% missing_values[!is.numeric(missing_values)]] <- NA
    }
  }

  # 5. 计算缺失情况
  var_missing <- colMeans(is.na(processed_data))
  high_missing_vars <- names(var_missing[var_missing > missing_threshold])
  obs_missing <- rowMeans(is.na(processed_data))
  high_missing_obs <- which(obs_missing > missing_threshold)
  missing_pattern <- mice::md.pattern(processed_data, plot = FALSE)

  ## 6. 根据方法处理缺失值
  if(method == "fiml") {
    vars <- names(processed_data)

    # 检查零方差变量
    var_zeros <- sapply(processed_data, function(x) var(x, na.rm = TRUE) == 0)
    if(any(var_zeros)) {
      message("Warning: Following variables have zero variance and will be excluded from FIML:")
      message(paste(names(var_zeros)[var_zeros], collapse = ", "))
      vars <- names(var_zeros)[!var_zeros]
    }

    # 只对非零方差变量使用FIML
    if(length(vars) > 0) {
      model <- paste(vars, "~ 1")
      model <- paste(model, collapse = "\n")
      fit <- lavaan::sem(model, data = processed_data, missing = "fiml")
      method_details <- "FIML estimation used (excluding zero-variance variables)"
    } else {
      # 如果没有有效变量，切换到MI方法
      message("No valid variables for FIML, switching to MI method")
      imp <- mice::mice(processed_data, m = n_imputations, printFlag = FALSE)
      processed_data <- mice::complete(imp, 1)
      method_details <- paste("Switched to Multiple imputation with", n_imputations, "imputations")
    }
  } else if(method == "mi") {
    imp <- mice::mice(processed_data, m = n_imputations, printFlag = FALSE)
    processed_data <- mice::complete(imp, 1)
    method_details <- paste("Multiple imputation with", n_imputations, "imputations")
  }


  # 7. 准备报告
  missing_report <- list(
    original_data = list(
      n_missing = colSums(is.na(processed_data)),
      missing_pattern = missing_pattern,
      missing_by_type = sapply(missing_values, function(x)
        sum(processed_data == x, na.rm = TRUE))
    ),
    processed_data = list(
      var_missing = var_missing,
      high_missing_vars = high_missing_vars,
      obs_missing = obs_missing,
      high_missing_cases = length(high_missing_obs)
    ),
    method = list(
      type = method,
      details = method_details
    )
  )

  # 8. 合并回元数据列
  final_data <- cbind(meta_data, processed_data)

  return(list(
    data = final_data,
    report = missing_report
  ))
}
