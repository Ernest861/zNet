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
                           missing_values = c(NA, -999, -888, "NULL"),
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
  if(!all(c(id_col, time_col) %in% names(data))) {
    stop("ID or time column not found in data")
  }

  # 2. 识别和处理缺失值
  processed_data <- data

  # 获取需要检查缺失值的列（除ID和时间列外）
  check_cols <- names(data)[!names(data) %in% c(id_col, time_col)]

  # 将所有指定的缺失值编码转换为NA
  for(col in check_cols) {
    if(is.numeric(data[[col]])) {
      processed_data[[col]][data[[col]] %in% missing_values[is.numeric(missing_values)]] <- NA
    }
    if(is.character(data[[col]]) || is.factor(data[[col]])) {
      processed_data[[col]][data[[col]] %in% missing_values[!is.numeric(missing_values)]] <- NA
    }
  }

  # 3. 计算缺失情况
  missing_summary <- list()

  # 3.1 变量层面的缺失
  var_missing <- colMeans(is.na(processed_data))
  high_missing_vars <- names(var_missing[var_missing > missing_threshold])

  # 3.2 观察层面的缺失
  obs_missing <- rowMeans(is.na(processed_data[, check_cols]))
  high_missing_obs <- which(obs_missing > missing_threshold)

  # 3.3 使用mice生成缺失模式
  missing_pattern <- mice::md.pattern(processed_data[, check_cols], plot = FALSE)

  # 4. 根据方法处理缺失值
  if(method == "fiml") {
    # FIML 方法
    vars <- check_cols
    model <- paste(vars, "~ 1")
    model <- paste(model, collapse = "\n")

    fit <- lavaan::sem(model, data = processed_data, missing = "fiml")

    processed_data <- processed_data  # FIML 不直接修改数据
    method_details <- "FIML estimation used"

  } else if(method == "mi") {
    # 多重插补方法
    imp <- mice::mice(processed_data, m = n_imputations, printFlag = FALSE)
    processed_data <- mice::complete(imp, 1)
    method_details <- paste("Multiple imputation with", n_imputations, "imputations")
  }

  # 5. 准备报告
  missing_report <- list(
    original_data = list(
      n_missing = colSums(is.na(data)),
      missing_pattern = missing_pattern,
      missing_by_type = sapply(missing_values, function(x) sum(data == x, na.rm = TRUE))
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

  # 6. 返回结果
  return(list(
    data = processed_data,
    report = missing_report
  ))
}
