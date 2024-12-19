#' Rank Variables by Their Correlations with Target Variables
#' 
#' @description 
#' This function calculates correlations between specified target variables and all other 
#' variables in a data frame, ranks them by absolute correlation strength, and provides
#' both tabular and visual outputs.
#' 
#' @param data A data frame or matrix containing all variables
#' @param target_vars Character vector specifying the names of target variables
#' @param top_n Integer specifying how many top correlated variables to return (default is all)
#' @param exclude_vars Character vector of variable names to exclude from analysis (optional)
#' @param round_digits Integer specifying number of decimal places for correlations (default = 3)
#' @param plot_type Character specifying the type of plot ("heatmap", "bar", "both", or "none")
#' @param color_palette Character vector of colors for the heatmap (default is viridis)
#' 
#' @return A list containing:
#'   \item{correlations}{Data frame of correlation results}
#'   \item{plots}{List of generated plots (if requested)}
#' 
#' @import ggplot2
#' @import viridis
#' @import reshape2
#' 
#' @export
rank_correlations <- function(data, target_vars, top_n = NULL, 
                              exclude_vars = NULL, round_digits = 3,
                              plot_type = "both", 
                              color_palette = viridis::viridis(100)) {
  # Input validation
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix")
  }
  if (!all(target_vars %in% colnames(data))) {
    stop("Some target variables not found in data")
  }
  
  # Convert matrix to data frame if necessary
  data <- as.data.frame(data)
  
  # Determine variables to analyze
  all_vars <- colnames(data)
  if (!is.null(exclude_vars)) {
    all_vars <- setdiff(all_vars, exclude_vars)
  }
  analysis_vars <- setdiff(all_vars, target_vars)
  
  # Calculate correlations and sample sizes
  correlation_results <- sapply(data[, target_vars, drop = FALSE], function(y) {
    sapply(data[, analysis_vars, drop = FALSE], function(x) {
      cor(x, y, use = "pairwise.complete.obs")
    })
  })
  
  sample_sizes <- sapply(data[, target_vars, drop = FALSE], function(y) {
    sapply(data[, analysis_vars, drop = FALSE], function(x) {
      sum(!is.na(x) & !is.na(y))
    })
  })
  
  # Calculate mean absolute correlation
  mean_abs_cor <- rowMeans(abs(correlation_results))
  
  # Create results data frame
  results <- data.frame(Variable = analysis_vars)
  
  # Add correlations and sample sizes for each target variable
  for (var in target_vars) {
    cor_col_name <- paste0(var, "_cor")
    n_col_name <- paste0(var, "_n")
    results[[cor_col_name]] <- round(correlation_results[, var], round_digits)
    results[[n_col_name]] <- sample_sizes[, var]
  }
  
  # Add mean absolute correlation and sort
  results$Mean_abs_cor <- round(mean_abs_cor, round_digits)
  results <- results[order(-results$Mean_abs_cor), ]
  
  # Subset to top_n if specified
  if (!is.null(top_n) && top_n < nrow(results)) {
    results <- results[1:top_n, ]
  }
  
  # Initialize plots list
  plots <- list()
  
  # Create visualizations if requested
  if (plot_type %in% c("heatmap", "both")) {
    # Prepare data for heatmap
    cor_matrix <- correlation_results[1:min(nrow(results), ifelse(is.null(top_n), nrow(results), top_n)), ]
    cor_long <- reshape2::melt(cor_matrix)
    colnames(cor_long) <- c("Variable", "Target", "Correlation")
    
    # Create heatmap
    plots$heatmap <- ggplot2::ggplot(cor_long, 
                                     aes(x = Target, y = Variable, fill = Correlation)) +
      geom_tile() +
      scale_fill_gradientn(colors = color_palette,
                           limits = c(-1, 1)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Correlation Heatmap",
           x = "Target Variables",
           y = "Input Variables")
  }
  
  if (plot_type %in% c("bar", "both")) {
    # Prepare data for bar plot
    bar_data <- results[, c("Variable", "Mean_abs_cor")]
    
    # Create bar plot
    plots$bar <- ggplot2::ggplot(bar_data, 
                                 aes(x = reorder(Variable, Mean_abs_cor), 
                                     y = Mean_abs_cor)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_classic() +
      labs(title = "Mean Absolute Correlations",
           x = "Variables",
           y = "Mean Absolute Correlation")
  }
  
  # Return results
  return(list(
    correlations = results,
    plots = plots
  ))
}

#' Example usage:
#' 
#' # Load required packages
#' library(ggplot2)
#' library(viridis)
#' library(reshape2)
#' 
#' # Basic usage with both plots
#' results <- rank_correlations(
#'   data = your_data,
#'   target_vars = c("var1", "var2"),
#'   top_n = 10,
#'   plot_type = "both"
#' )
#' 
#' # View results
#' print(results$correlations)
#' 
#' # Display plots
#' print(results$plots$heatmap)
#' print(results$plots$bar)
#' 
#' # Custom color palette
#' results_custom <- rank_correlations(
#'   data = your_data,
#'   target_vars = c("var1", "var2"),
#'   color_palette = c("red", "white", "blue"),
#'   plot_type = "both"
#' )