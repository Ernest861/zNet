% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zdescribe.R
\name{zdescribe}
\alias{zdescribe}
\title{Quick Descriptive Analysis of Data Frame}
\usage{
zdescribe(
  data,
  numeric_cols = NULL,
  categorical_cols = NULL,
  correlation_method = "pearson",
  digits = 2
)
}
\arguments{
\item{data}{A data frame containing the data to be analyzed}

\item{numeric_cols}{Character vector of numeric column names to analyze.
If NULL, all numeric columns will be analyzed.}

\item{categorical_cols}{Character vector of categorical column names to analyze.
If NULL, all factor columns will be analyzed.}

\item{correlation_method}{Method for correlation analysis: "pearson", "spearman", or "kendall"}

\item{digits}{Number of decimal places for rounding numeric results}
}
\value{
A list containing:
\item{basic_info}{Basic information about the dataset}
\item{numeric_summary}{Summary statistics for numeric variables}
\item{correlations}{Correlation matrix for numeric variables}
\item{categorical_summary}{Frequency tables for categorical variables}
\item{missing_summary}{Summary of missing values}
}
\description{
Performs comprehensive descriptive analysis of a data frame, including summary statistics,
correlation analysis, frequency distributions, and missing value analysis.
}
\examples{
\dontrun{
data <- data.frame(
  age = rnorm(100, 30, 5),
  income = rnorm(100, 50000, 10000),
  education = factor(sample(c("High School", "Bachelor", "Master"), 100, replace = TRUE))
)
result <- quick_analyze(data)
}

}
