#' Prepare Longitudinal Survey Data
#'
#' @title Prepare ABCD Longitudinal Survey Data
#' @description Process longitudinal survey data from ABCD study, with improved error handling
#'
#' @param data A data frame containing the survey data
#' @param id_col The name of the ID column (default: "src_subject_id")
#' @param time_col The name of the time column (default: "eventname")
#' @param remove_prefix The prefix to remove from variable names (default: "socialdev_")
#' @param remove_careless Logical, whether to remove careless responses
#' @param debug Logical, whether to print debug information
#'
#' @return A list containing processed data and reports
#' @import ufs
#' @export

prepare_long_data <- function(data,
                              id_col = "src_subject_id",
                              time_col = "eventname",
                              remove_prefix = "socialdev_",
                              remove_careless = TRUE,
                              debug = TRUE) {
  
  # 1. 检查输入
  if(!all(c(id_col, time_col) %in% names(data))) {
    stop("ID or time column not found in data")
  }
  
  # 2. 获取所有唯一的时间点
  unique_waves <- unique(data[[time_col]])
  wave_patterns <- data.frame(
    wave = unique_waves,
    time = sapply(unique_waves, function(x) {
      as.numeric(gsub(".*wave(\\d+).*", "\\1", x))
    })
  )
  wave_patterns <- wave_patterns[order(wave_patterns$time), ]
  
  # 3. 识别所有项目列
  all_columns <- names(data)
  item_cols <- grep(remove_prefix, all_columns, value = TRUE)
  
  # 区分基线和随访列
  base_cols <- item_cols[!grepl("_l$", item_cols)]
  follow_cols <- item_cols[grepl("_l$", item_cols)]
  
  # 创建统一的列名
  unified_cols <- unique(c(
    gsub(remove_prefix, "", base_cols),
    gsub("_l$", "", gsub(remove_prefix, "", follow_cols))
  ))
  
  # 4. 分别处理每个时间点的数据
  data_list <- list()
  wave_details <- list()
  
  for(i in seq_along(wave_patterns$wave)) {
    wave <- wave_patterns$wave[i]
    time <- wave_patterns$time[i]
    
    wave_report <- list()
    message("\n=== Processing wave ", i, " ===")
    
    # 记录原始样本量
    wave_report$original_n <- sum(data[[time_col]] == wave)
    message(paste("Original n:", wave_report$original_n))
    
    # 选择当前wave的数据
    wave_data <- data[data[[time_col]] == wave, ]
    
    # 创建结果数据框，确保所有列都存在
    result_cols <- c(id_col, time_col, unified_cols)
    curr_data <- data.frame(matrix(NA, nrow = nrow(wave_data), 
                                   ncol = length(result_cols)))
    names(curr_data) <- result_cols
    
    # 复制ID和时间列
    curr_data[[id_col]] <- wave_data[[id_col]]
    curr_data[[time_col]] <- wave_data[[time_col]]
    
    # 填充数据
    if(i == 1) {
      # 基线数据
      for(col in base_cols) {
        new_name <- gsub(remove_prefix, "", col)
        if(col %in% names(wave_data)) {
          curr_data[[new_name]] <- wave_data[[col]]
        }
      }
    } else {
      # 随访数据
      for(col in follow_cols) {
        new_name <- gsub("_l$", "", gsub(remove_prefix, "", col))
        if(col %in% names(wave_data)) {
          curr_data[[new_name]] <- wave_data[[col]]
        }
      }
    }
    
    if(debug) {
      message("\nWave ", i, " columns: ", ncol(curr_data))
      message("First few columns: ", 
              paste(head(names(curr_data)), collapse = ", "))
    }
    
    # 处理不认真作答
    if(remove_careless && nrow(curr_data) > 0) {
      numeric_cols <- names(curr_data)[sapply(curr_data, is.numeric)]
      numeric_cols <- numeric_cols[!numeric_cols %in% c(id_col, time_col)]
      
      if(length(numeric_cols) > 0) {
        require(ufs)
        careless_obj <- try(carelessObject(data = curr_data, items = numeric_cols))
        
        if(!inherits(careless_obj, "try-error")) {
          suspect_rows <- try(suspectParticipants(careless_obj))
          
          if(!inherits(suspect_rows, "try-error") && nrow(suspect_rows) < nrow(curr_data)) {
            curr_data <- curr_data[-as.numeric(rownames(suspect_rows)), ]
            wave_report$careless_removed <- length(rownames(suspect_rows))
          } else {
            message("Warning: Careless detection failed or would remove all data. Keeping all responses.")
            wave_report$careless_removed <- 0
          }
        } else {
          message("Warning: Could not create careless object. Keeping all responses.")
          wave_report$careless_removed <- 0
        }
      } else {
        wave_report$careless_removed <- 0
      }
    } else {
      wave_report$careless_removed <- 0
    }
    
    # 记录最终样本量
    wave_report$final_n <- nrow(curr_data)
    
    # 只有在数据框非空时才添加时间指标
    if(nrow(curr_data) > 0) {
      curr_data$time <- time
    } else {
      message("Warning: Wave ", i, " has no data after processing")
      # 创建一个空数据框，但包含所有必需的列
      curr_data <- data.frame(matrix(ncol = ncol(curr_data) + 1, nrow = 0))
      names(curr_data) <- c(names(curr_data)[-ncol(curr_data)], "time")
    }
    
    data_list[[i]] <- curr_data
    wave_details[[wave]] <- wave_report
    
    if(debug) {
      message("Final rows in wave ", i, ": ", nrow(curr_data))
    }
  }
  
  # 检查是否所有wave都有数据
  valid_waves <- which(sapply(data_list, nrow) > 0)
  if(length(valid_waves) == 0) {
    stop("No data remains after processing")
  }
  
  # 5. 只合并有效的wave数据
  final_data <- do.call(rbind, data_list[valid_waves])
  row.names(final_data) <- NULL
  
  # 6. 找出所有时间点的共同ID
  common_ids <- Reduce(intersect, 
                       lapply(data_list[valid_waves], 
                              function(x) unique(x[[id_col]])))
  
  # 7. 准备报告
  report <- list(
    original_n = nrow(data),
    waves = wave_patterns,
    wave_details = wave_details,
    samples_by_wave = sapply(data_list, nrow),
    common_subjects = length(common_ids),
    final_n = nrow(final_data),
    column_info = list(
      unified_columns = unified_cols,
      baseline_columns = base_cols,
      followup_columns = follow_cols
    )
  )
  
  return(list(
    data = final_data,
    common_ids = common_ids,
    report = report
  ))
}