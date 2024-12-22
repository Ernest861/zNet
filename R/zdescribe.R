#' Quick Descriptive Analysis of Data Frame
#' 
#' @description
#' Performs comprehensive descriptive analysis of a data frame, including summary statistics,
#' correlation analysis, frequency distributions, and missing value analysis.
#' 
#' @param data A data frame containing the data to be analyzed
#' @param numeric_cols Character vector of numeric column names to analyze. 
#'        If NULL, all numeric columns will be analyzed.
#' @param categorical_cols Character vector of categorical column names to analyze. 
#'        If NULL, all factor columns will be analyzed.
#' @param correlation_method Method for correlation analysis: "pearson", "spearman", or "kendall"
#' @param digits Number of decimal places for rounding numeric results
#' 
#' @return A list containing:
#'   \item{basic_info}{Basic information about the dataset}
#'   \item{numeric_summary}{Summary statistics for numeric variables}
#'   \item{correlations}{Correlation matrix for numeric variables}
#'   \item{categorical_summary}{Frequency tables for categorical variables}
#'   \item{missing_summary}{Summary of missing values}
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   age = rnorm(100, 30, 5),
#'   income = rnorm(100, 50000, 10000),
#'   education = factor(sample(c("High School", "Bachelor", "Master"), 100, replace = TRUE))
#' )
#' result <- quick_analyze(data)
#' }
#' 
#' @import dplyr
#' @import tidyr
#' @importFrom stats cor median sd
#' @importFrom utils str
#' 
#' @export
zdescribe <- function(data, 
                          numeric_cols = NULL,
                          categorical_cols = NULL,
                          correlation_method = "pearson",
                          digits = 2) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!correlation_method %in% c("pearson", "spearman", "kendall")) {
    stop("correlation_method must be one of: 'pearson', 'spearman', 'kendall'")
  }
  
  # Identify column types if not specified
  if (is.null(numeric_cols)) {
    numeric_cols <- names(dplyr::select_if(data, is.numeric))
  } else {
    if (!all(numeric_cols %in% names(data))) {
      stop("Some specified numeric columns not found in data")
    }
  }
  
  if (is.null(categorical_cols)) {
    categorical_cols <- names(dplyr::select_if(data, is.factor))
  } else {
    if (!all(categorical_cols %in% names(data))) {
      stop("Some specified categorical columns not found in data")
    }
  }
  
  # Initialize results list
  results <- list()
  
  # 1. Basic Information
  results$basic_info <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    n_numeric = length(numeric_cols),
    n_categorical = length(categorical_cols)
  )
  
  # 2. Numeric Analysis
  if (length(numeric_cols) > 0) {
    num_data <- data[numeric_cols]
    
    # Summary statistics
    results$numeric_summary <- lapply(num_data, function(x) {
      c(
        n = length(x),
        missing = sum(is.na(x)),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      )
    }) %>%
      do.call(rbind, .) %>%
      round(digits)
    
    # Correlation analysis
    if (length(numeric_cols) > 1) {
      results$correlations <- cor(num_data,
                                  use = "pairwise.complete.obs",
                                  method = correlation_method) %>%
        round(digits)
    }
  }
  
  # 3. Categorical Analysis
  if (length(categorical_cols) > 0) {
    results$categorical_summary <- lapply(categorical_cols, function(col) {
      freq_table <- table(data[[col]])
      prop_table <- prop.table(freq_table) * 100
      data.frame(
        level = names(freq_table),
        frequency = as.numeric(freq_table),
        percentage = round(as.numeric(prop_table), digits)
      )
    })
    names(results$categorical_summary) <- categorical_cols
  }
  
  # 4. Missing Value Analysis
  missing_data <- sapply(data, function(x) sum(is.na(x)))
  missing_data <- missing_data[missing_data > 0]
  
  if (length(missing_data) > 0) {
    results$missing_summary <- data.frame(
      variable = names(missing_data),
      missing_count = as.numeric(missing_data),
      missing_percent = round(missing_data / nrow(data) * 100, digits)
    )
  } else {
    results$missing_summary <- NULL
  }
  
  # Set class for custom print method
  class(results) <- c("quick_analysis", "list")
  
  return(results)
}

#' Print method for quick_analysis objects
#' 
#' @param x Object of class quick_analysis
#' @param ... Additional arguments passed to print
#' 
#' @export
print.quick_analysis <- function(x, ...) {
  cat("\n=== Dataset Basic Information ===\n")
  print(x$basic_info)
  
  if (!is.null(x$numeric_summary)) {
    cat("\n=== Numeric Variables Summary ===\n")
    print(x$numeric_summary)
  }
  
  if (!is.null(x$correlations)) {
    cat("\n=== Correlation Matrix ===\n")
    print(x$correlations)
  }
  
  if (!is.null(x$categorical_summary)) {
    cat("\n=== Categorical Variables Summary ===\n")
    for (var in names(x$categorical_summary)) {
      cat("\nVariable:", var, "\n")
      print(x$categorical_summary[[var]])
    }
  }
  
  if (!is.null(x$missing_summary)) {
    cat("\n=== Missing Values Summary ===\n")
    print(x$missing_summary)
  }
}