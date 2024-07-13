################################################################################
################    Complex and Social Networks    #############################
#########################   Lab 4   ############################################
############   Odysseas Kyparissis & Marta Aicart   ###########################
################################################################################
################################################################################


# Setting up paths

# Set as the working directory, the folder where this script is stored.
work_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(work_dir)

# Check the existence of folders/ create the folders

processed_dir <- file.path(work_dir, "data/processed_dependency_tree_metrics/")
if (!file.exists(processed_dir)) {
  dir.create(processed_dir)
}

# Specify the folder name for the results
results_dir <- file.path(work_dir, "results")
if (!file.exists(results_dir)) {
  dir.create(results_dir)
}

# Create a directory for prelim_visualization if it doesn't exist
prelim_dir <- file.path(results_dir, "prelim_visualization")
if (!file.exists(prelim_dir)) {
  dir.create(prelim_dir)
}

# Create a directory for homoscedasticity if it doesn't exist
homosced_dir <- file.path(results_dir, "homoscedasticity")
if (!file.exists(homosced_dir)) {
  dir.create(homosced_dir)
}

# Create a directory for best_models if it doesn't exist
best_models_dir <- file.path(results_dir, "best_models")
if (!file.exists(best_models_dir)) {
  dir.create(best_models_dir)
}


################################################################################
################################################################################

# Loading Libraries and Packages

require(stats4) # for MLE
require(VGAM) # for the Riemann-zeta function
require(ggplot2)
require(gridExtra)
require(grid)
library(dplyr)
library(minpack.lm)

################################################################################
################################################################################

# Defining functions

# Define a function to calculate AIC for models without parameters
calculate_AIC <- function(RSS, n, p=0) {
  n*log(2*pi) + n*log(RSS/n) + n + 2*(p + 1)
}

choose_dataset <- function(model_selection, df, df_mean) {
  if (model_selection == "df") {
    return(df)
  } else if (model_selection == "df_mean") {
    return(df_mean)
  }
}
################################################################################
################################################################################


# 2. Data preparation
table1 <- source(file.path(work_dir, "/summary/summary_table.R"))$value # Table 1

write.csv(table1, file.path(results_dir, "table1.csv"), row.names=FALSE)

# Set up png's width and height
width = 800
height = 600
res = 150
source(file.path(work_dir, "homosced.R"))


# Manually created list of model selections
model_selections <- list(
  Arabic = list(
    model1 = "df_mean",
    model1_plus = "df",
    model2 = "df",
    model2_plus = "df",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  ),
  Basque = list(
    model1 = "df_mean",
    model1_plus = "df_mean",
    model2 = "df_mean",
    model2_plus = "df_mean",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  ),
  Catalan = list(
    model1 = "df_mean",
    model1_plus = "df_mean",
    model2 = "df_mean",
    model2_plus = "df_mean",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  ),
  Chinese = list(
    model1 = "df_mean",
    model1_plus = "df_mean",
    model2 = "df_mean",
    model2_plus = "df_mean",
    model3 = "df_mean"
    # model3_plus = "df_mean",
    # model4 = "df",
    # model4_plus = "df"
  ),
  Czech = list(
    model1 = "df",
    model1_plus = "df",
    model2 = "df",
    model2_plus = "df",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  ),
  English = list(
    model1 = "df_mean",
    model1_plus = "df_mean",
    model2 = "df_mean",
    model2_plus = "df_mean",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  ),
  Greek = list(
    model1 = "df_mean",
    model1_plus = "df_mean",
    model2 = "df_mean",
    model2_plus = "df_mean",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  ),
  Hungarian = list(
    model1 = "df_mean",
    model1_plus = "df",
    model2 = "df",
    model2_plus = "df_mean",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  ),
  Italian = list(
    model1 = "df_mean",
    model1_plus = "df_mean",
    model2 = "df_mean",
    model2_plus = "df_mean",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  ),
  Turkish = list(
    model1 = "df_mean",
    model1_plus = "df_mean",
    model2 = "df_mean",
    model2_plus = "df_mean",
    model3 = "df_mean"
    # model3_plus = "df",
    # model4 = "df",
    # model4_plus = "df"
  )
)


# 3. Data analysis
# Loop through the files and save the plots as PNG files


for (i in seq_along(table1$processed_file)) {
  # headers exist in the new files and they are already sorted (no need to sort again)
  df = read.table(table1$processed_file[i], header = TRUE)
  
  # Initialize variables to store model results
  results <- data.frame(Model = character(0), RSS = numeric(0), AIC = numeric(0), s = numeric(0))
  parameter_results <- data.frame(Model = character(0), Parameter = character(0), Value = numeric(0))
  
  
  # 3.1 Preliminary visualization
  png(file.path(prelim_dir, paste0("plot_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(df[order(df$vertices),]$vertices, df[order(df$vertices),]$degree_2nd_moment, main = table1$Language[i],
       xlab = "vertices", ylab = "degree 2nd moment")
  dev.off()

  png(file.path(prelim_dir, paste0("log_plot_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(log(df$vertices), log(df$degree_2nd_moment), main = table1$Language[i],
       xlab = "log(vertices)", ylab = "log(degree 2nd moment)")
  dev.off()

  mean_df = aggregate(df, list(df$vertices), mean)
  
  png(file.path(prelim_dir, paste0("mean_plot_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(mean_df$vertices, mean_df$degree_2nd_moment, main = table1$Language[i],
       xlab = "aggregated vertices", ylab = "mean degree 2nd moment")
  dev.off()

  png(file.path(prelim_dir, paste0("log_mean_plot_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(log(mean_df$vertices), log(mean_df$degree_2nd_moment), main = table1$Language[i],
       xlab = "log(vertices)", ylab = "log(mean degree 2nd moment)")
  dev.off()

  png(file.path(prelim_dir, paste0("exploration_of_scaling_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(df$vertices, df$degree_2nd_moment, main = table1$Language[i],
         xlab = "vertices", ylab = "degree 2nd moment")
  lines(mean_df$vertices,mean_df$degree_2nd_moment, col = "green")
  lines(df$vertices, (1 - 1/df$vertices)*(5 - 6/df$vertices), col = "red")
  lines(df$vertices,4-6/df$vertices, col = "blue")
  lines(df$vertices,df$vertices-1, col = "blue")
  dev.off()
  
  # 3.2 The ensemble of models
  
  
  # Fit null model
  RSS_null <- sum((df$degree_2nd_moment - (1 - 1/df$vertices) * (5 - 6/df$vertices))^2)
  n <- length(df$degree_2nd_moment)
  p <- 0
  AIC_null <- calculate_AIC(RSS_null, n, p)
  s_null <- sqrt(RSS_null / (n - p))
  
  results <- rbind(results, data.frame(Model = "Null", RSS = RSS_null, AIC = AIC_null, s = s_null))
  
  # Get initial values for a and b parameters
  linear_model = lm(log(degree_2nd_moment)~log(vertices), data=df)
  a_initial = exp(coef(linear_model)[1]); a_initial
  b_initial = coef(linear_model)[2]; b_initial
  c_initial = 0
  d_initial = 0.5
  
  # Fit model1
  
  model1_df = model_selections[[table1$Language[i]]][["model1"]]
  df_to_use = choose_dataset(model1_df, df, mean_df)
  
  model1 <- nls(degree_2nd_moment ~ (vertices/2)^b, data = df_to_use,
                start = list(b = b_initial), trace = TRUE)
  RSS_model1 <- deviance(model1)
  AIC_model1 <- AIC(model1)
  s_model1 <- sqrt(RSS_model1 / df.residual(model1))
  
  model1_b = coef(model1)["b"]
  
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model1", Parameter = "b", Value = model1_b))
  
  results <- rbind(results, data.frame(Model = "Model1", RSS = RSS_model1, AIC = AIC_model1, s = s_model1))
  
  # Fit model1_plus
  
  model1_plus_df = model_selections[[table1$Language[i]]][["model1_plus"]]
  df_to_use = choose_dataset(model1_plus_df, df, mean_df)
  
  model1_plus <- nls(degree_2nd_moment ~ (vertices/2)^b + d, data = df_to_use,
                     start = list(b = b_initial, d = d_initial), trace = TRUE)
  RSS_model1_plus <- deviance(model1_plus)
  AIC_model1_plus <- AIC(model1_plus)
  s_model1_plus <- sqrt(RSS_model1_plus / df.residual(model1_plus))
  
  model1_plus_b = coef(model1_plus)["b"]
  model1_plus_d = coef(model1_plus)["d"]
  
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model1_plus", Parameter = "b", Value = model1_plus_b))
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model1_plus", Parameter = "d", Value = model1_plus_d))
  
  results <- rbind(results, data.frame(Model = "Model1_plus", RSS = RSS_model1_plus, AIC = AIC_model1_plus, s = s_model1_plus))
  
  # Fit model2
  
  model2_df = model_selections[[table1$Language[i]]][["model2"]]
  df_to_use = choose_dataset(model2_df, df, mean_df)
  
  model2 <- nls(degree_2nd_moment ~ a * vertices^b, data = df_to_use,
                start = list(a=a_initial, b = b_initial), trace = TRUE)
  RSS_model2 <- deviance(model2)
  AIC_model2 <- AIC(model2)
  s_model2 <- sqrt(RSS_model2 / df.residual(model2))
  
  model2_a = coef(model2)["a"]
  model2_b = coef(model2)["b"]
  
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model2", Parameter = "a", Value = model2_a))
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model2", Parameter = "b", Value = model2_b))
  
  results <- rbind(results, data.frame(Model = "Model2", RSS = RSS_model2, AIC = AIC_model2, s = s_model2))
  
  # Fit model2_plus
  
  model2_plus_df = model_selections[[table1$Language[i]]][["model2_plus"]]
  df_to_use = choose_dataset(model2_plus_df, df, mean_df)
  
  model2_plus <- nlsLM(degree_2nd_moment ~ a * vertices^b + d, data = df_to_use,
                       start = list(a = a_initial, b = b_initial, d = d_initial),
                       control = nls.control(minFactor = 1e-8, maxiter = 1024),
                       trace = TRUE)
  RSS_model2_plus <- deviance(model2_plus)
  AIC_model2_plus <- AIC(model2_plus)
  s_model2_plus <- sqrt(RSS_model2_plus / df.residual(model2_plus))
  
  model2_plus_a = coef(model2_plus)["a.(Intercept)"]
  model2_plus_b = coef(model2_plus)["b.log(vertices)"]
  model2_plus_d = coef(model2_plus)["d"]
  
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model2_plus", Parameter = "a", Value = model2_plus_a))
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model2_plus", Parameter = "b", Value = model2_plus_b))
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model2_plus", Parameter = "d", Value = model2_plus_d))
  
  results <- rbind(results, data.frame(Model = "Model2_plus", RSS = RSS_model2_plus, AIC = AIC_model2_plus, s = s_model2_plus))
  
  # Fit model3
  
  model3_df = model_selections[[table1$Language[i]]][["model3"]]
  df_to_use = choose_dataset(model3_df, df, mean_df)
  
  model3 <- nls(degree_2nd_moment ~ a * exp(c * vertices), data = df_to_use,
                start = list(a=a_initial, c=c_initial), trace = TRUE)
  RSS_model3 <- deviance(model3)
  AIC_model3 <- AIC(model3)
  s_model3 <- sqrt(RSS_model3 / df.residual(model3))
  
  model3_a = coef(model3)["a"]
  model3_c = coef(model3)["c"]
  
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model3", Parameter = "a", Value = model3_a))
  parameter_results <- rbind(parameter_results, data.frame(Model = "Model3", Parameter = "c", Value = model3_c))
  
  results <- rbind(results, data.frame(Model = "Model3", RSS = RSS_model3, AIC = AIC_model3, s = s_model3))
  
  # Fit model3_plus
  
  # model3_plus_df = model_selections[[table1$Language[i]]][["model3_plus"]]
  # df_to_use = choose_dataset(model3_plus_df, df, mean_df)
  # 
  # model3_plus <- nls(degree_2nd_moment ~ a * exp(c * vertices) + d, data = df_to_use,
  #               start = list(a=a_initial, c=b_initial, d=1), trace = TRUE)
  # RSS_model3_plus <- deviance(model3_plus)
  # AIC_model3_plus <- AIC(model3_plus)
  # s_model3_plus <- sqrt(RSS_model3_plus / df.residual(model3_plus))
  # 
  # model3_plus_a = exp(coef(model3_plus)["a"])
  # model3_plus_c = coef(model3_plus)["c"]
  # model3_plus_d = coef(model3_plus)["d"]
  # 
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model3_plus", Parameter = "a", Value = model3_plus_a))
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model3_plus", Parameter = "c", Value = model3_plus_c))
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model3_plus", Parameter = "d", Value = model3_plus_d))
  # 
  # results <- rbind(results, data.frame(Model = "Model3_plus", RSS = RSS_model3_plus, AIC = AIC_model3_plus, s = s_model3_plus))
  
  # Fit model4
  
  # model4_df = model_selections[[table1$Language[i]]][["model4"]]
  # df_to_use = choose_dataset(model4_df, df, mean_df)
  # 
  # model4 <- nls(degree_2nd_moment ~ a * vertices^b*exp(c*vertices), data = df_to_use,
  #               start = list(a=a_initial, b = b_initial, c = 0), trace = TRUE)
  # RSS_model4 <- deviance(model4)
  # AIC_model4 <- AIC(model4)
  # s_model4 <- sqrt(RSS_model4 / df.residual(model4))
  # 
  # model4_a = exp(coef(model4)["a"])
  # model4_b = coef(model4)["b"]
  # model4_c = 0.01
  # 
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model4", Parameter = "a", Value = model4_a))
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model4", Parameter = "b", Value = model4_b))
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model4", Parameter = "c", Value = model4_c))
  # 
  # Fit model4 plus
  
  # model4_plus_df = model_selections[[table1$Language[i]]][["model4"]]
  # df_to_use = choose_dataset(model4_plus_df, df, mean_df)
  # 
  # model4_plus <- nls(degree_2nd_moment ~ a * vertices^b*exp(c*vertices) + d, data = df_to_use,
  #               start = list(a=a_initial, b = b_initial, c = 0, d = 0), trace = TRUE)
  # RSS_model4 <- deviance(model4_plus)
  # AIC_model4 <- AIC(model4_plus)
  # s_model4_plus <- sqrt(RSS_model4_plus / df.residual(model4_plus))
  # 
  # model4_plus_a = exp(coef(model4_plus)["a"])
  # model4_plus_b = coef(model4_plus)["b"]
  # model4_plus_c = coef(model4_plus)["c"]
  # model4_plus_d = coef(model4_plus)["d"]
  # 
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model4", Parameter = "a", Value = model4_a))
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model4", Parameter = "b", Value = model4_b))
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model4", Parameter = "c", Value = model4_c))
  # parameter_results <- rbind(parameter_results, data.frame(Model = "Model4", Parameter = "d", Value = model4_d))
  # 
  # results <- rbind(results, data.frame(Model = "Model2", RSS = RSS_model2, AIC = AIC_model2, s = s_model2))
  
  # Calculate AIC differences relative to the best model
  best_model <- results[which.min(results$AIC), "Model"]
  results$AIC_difference <- results$AIC - min(results$AIC)
  
  file_name <- paste0("model_results_", gsub(" ", "_", table1$Language[i]), ".csv")
  parameter_file_name <- paste0("model_parameters_", gsub(" ", "_", table1$Language[i]), ".csv")
  write.csv(results, file.path(results_dir, file_name), row.names = FALSE)
  write.csv(parameter_results, file.path(results_dir, parameter_file_name), row.names = FALSE)
  
  # 5.2 Final visualization
  if (best_model == "Model1"){
    df = choose_dataset(model1_df, df, mean_df)
    nonlinear_model = model1
  } else if (best_model == "Model1_plus") {
    df = choose_dataset(model1_plus_df, df, mean_df)
    nonlinear_model = model1_plus
  } else if (best_model == "Model2") {
    df = choose_dataset(model2_df, df, mean_df)
    nonlinear_model = model2
  } else if (best_model == "Model2_plus") {
    df = choose_dataset(model2_plus_df, df, mean_df)
    nonlinear_model = model2_plus
  } else if (best_model == "Model3") {
    df = choose_dataset(model3_df, df, mean_df)
    nonlinear_model = model3
  } else if (best_model == "Model3_plus") {
    df = choose_dataset(model3_plus_df, df, mean_df)
    nonlinear_model = model3_plus
  } else if (best_model == "Model4") {
    df = choose_dataset(model4_df, df, mean_df)
    nonlinear_model = model4
  } else if (best_model == "Model4_plus") {
    df = choose_dataset(model4_plus_df, df, mean_df)
    nonlinear_model = model4_plus
  }

  if (best_model != "Null"){
    png(file.path(best_models_dir, paste0("Best_model_", table1$Language[i], ".png")), res = res, width = width, height = height)
    plot(df$vertices, df$degree_2nd_moment, main=paste0("Best_model_", table1$Language[i]), ylab = "degree_2nd_moment", xlab = "vertices")
    lines(df$vertices, fitted(nonlinear_model), col = "green")
    dev.off()
    
  } else {
    png(file.path(best_models_dir, paste0("Best_model_", table1$Language[i], ".png")), res = res, width = width, height = height)
    plot(df$vertices, df$degree_2nd_moment, main=paste0("Best_model_", table1$Language[i]), ylab = "degree_2nd_moment", xlab = "vertices")
    lines(df$vertices, (1 - 1/df$vertices) * (5 - 6/df$vertices), col = "green")
    dev.off()
  }
  
}



