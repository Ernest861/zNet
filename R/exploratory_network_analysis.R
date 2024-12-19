#' Dynamic Panel Network Analysis for Longitudinal Data
#' @param data 输入数据框，列为项目，行为被试
#' ena
#' @param output_dir 图形输出路径
#' @param seed  随机数种子
#' @return 返回探索络分析结果列表bootstrap_ega, stability, dimension_selection
#' @export


# 探索性网络分析函数

#library(EGAnet)
#library(qgraph)
#library(igraph)

exploratory_network_analysis <- function(
    data,                   # 输入数据框，列为项目，行为被试
    max_dimensions = 6,     # 最大探索维度数，实际上没用到
    plot_output = TRUE,     # 是否绘制输出
    output_path = NULL,     # 图形输出路径
    seed = 234              # 随机数种子
) {
  # 执行自助法引导的EGA
  boot_ega <- bootEGA(
    data = data,
    model = "glasso",
    algorithm = "walktrap",
    iter = 5000,
    seed = seed,
    ncores = 6,
    typicalStructure = TRUE
  )

  # 详细分析自助法结果
  analyze_bootstrap_results <- function(boot_ega_result) {
    # 频率表
    frequency_df <- as.data.frame(boot_ega_result$frequency)
    names(frequency_df) <- c("n_factors", "frequency")

    # 自助法中每次抽样的社群划分
    community_matrix <- boot_ega_result$boot.wc

    # 计算每次抽样的社群数量
    community_counts <- apply(community_matrix, 1, function(x) length(unique(x)))
    community_freq <- table(community_counts) / nrow(community_matrix)

    # 打印详细信息
    print("频率表 (按指定维度):")
    print(frequency_df)

    print("\n频率表 (按实际社群数量):")
    print(as.data.frame(community_freq))

    # 找到最高频率的维度数
    optimal_dim_freq <- frequency_df$n_factors[which.max(frequency_df$frequency)]
    optimal_dim_community <- as.numeric(names(community_freq)[which.max(community_freq)])

    return(list(
      frequency_method = optimal_dim_freq,
      community_method = optimal_dim_community,
      frequency_table = frequency_df,
      community_frequency = as.data.frame(community_freq)
    ))
  }

  # 网络稳定性分析
  stability_result <- dimensionStability(boot_ega,structure=boot_ega$typicalGraph$wc)# 注意用典型的，否则可能会不一样

  # 可视化结果
  if (plot_output && !is.null(output_path)) {
    # 创建输出目录
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

    # 绘制EGA网络图
    pdf(file.path(output_path, "EGA_network.pdf"),
        width = 10, height = 8)

    # 使用 qgraph 绘制网络图
    qgraph(
      boot_ega$typicalGraph$graph, # 注意用典型的，否则可能会不一样
      layout = "spring",
      groups = factor(boot_ega$typicalGraph$wc),
      labels = colnames(data),
      title = "EGA Network Structure"
    )
    dev.off()

    # 绘制维度稳定性图
    pdf(file.path(output_path, "dimensionStability.pdf"),
        width = 10, height = 8)
    dimensionStability(boot_ega,structure=boot_ega$typicalGraph$wc)
    dev.off()
  }

  # 执行详细的维度分析
  dimension_selection <- analyze_bootstrap_results(boot_ega)

  # 返回结果
  return(list(
    bootstrap_ega = boot_ega,
    stability = stability_result,
    dimension_selection = dimension_selection
  ))
}

# 使用示例
#results_sample1 <- exploratory_network_analysis(
#  data = data1[,3:38],  # 选择问卷项目列
#  seed = 234,
#  output_path = "./network_analysis_results"
#)
