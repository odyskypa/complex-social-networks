# Define a range of potential initial values for d
# d_initial_candidates <- seq(-5000, 0, by = 0.1)
# 
# # Initialize d_initial to a default value
# d_initial <- NA  # You can choose a sensible default value
# 
# # Loop through candidates and try fitting the model
# for (d_candidate in d_initial_candidates) {
#   tryCatch({
#     model2_plus <- nls(degree_2nd_moment ~ a * vertices^b + d_candidate, data = df,
#                        start = list(a = a_initial, b = b_initial, d = d_candidate), trace = TRUE)
#     
#     if (!is.null(model2_plus$converged)) {
#       # Model converged, use this d_initial
#       d_initial <- d_candidate
#       break
#     }
#   }, error = function(e) {
#     # Non-convergence occurred, continue to the next candidate
#     cat("Non-convergence occurred for d =", d_candidate, "\n")
#   })
# }
# 
# # Check if a suitable d_initial was found
# if (is.na(d_initial)) {
#   cat("No suitable d_initial found.\n")
# } else {
#   cat("Found a suitable d_initial:", d_initial, "\n")
# }





for (i in seq_along(table1$processed_file)) {
  # headers exist in the new files and they are already sorted (no need to sort again)
  df = read.table(table1$processed_file[i], header = TRUE)
  
  mean_df = aggregate(df, list(df$vertices), mean)
  
  # 3.2 The ensemble of models
  
  # Get initial values for a and b parameters
  linear_model = lm(log(degree_2nd_moment)~log(vertices), data=df)
  a_initial = exp(coef(linear_model)[1]); a_initial
  b_initial = coef(linear_model)[2]; b_initial
  c_initial = 0
  d_initial = 0.5
  
  # Fit model1 (not-aggregated)
  model1 <- nls(degree_2nd_moment ~ (vertices/2)^b, data = df,
                start = list(b = b_initial), trace = TRUE)
  
  # Homoscedasticity model 1
  residuals <- residuals(model1)
  
  png(file.path(homosced_dir, paste0("Residuals_model1_not_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(fitted(model1), residuals, main=paste0("Model1 (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_1")
  abline(h = 0, col = "red")
  dev.off()
  
  # Fit model1_agg (aggregated)
  #model1_agg <- nls(degree_2nd_moment ~ (vertices/2)^b, data = mean_df,
  #                  start = list(b = b_initial), trace = TRUE)
  
  # Homoscedasticity model 1 agg
  #residuals <- residuals(model1_agg)
  
  #png(file.path(homosced_dir, paste0("Residuals_model1_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  #plot(fitted(model1_agg), residuals, main=paste0("Model1 (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_1_agg")
  #abline(h = 0, col = "red")
  #dev.off()
  
  # Fit model1_plus
  model1_plus <- nls(degree_2nd_moment ~ (vertices/2)^b + d, data = df,
                     start = list(b = b_initial, d = d_initial), trace = TRUE)
  
  # Homoscedasticity model 1 plus
  residuals <- residuals(model1_plus)
  
  png(file.path(homosced_dir, paste0("Residuals_model1_plus_not_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(fitted(model1_plus), residuals, main=paste0("Model1 + (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_1_plus")
  abline(h = 0, col = "red")
  dev.off()
  
  # Fit model1_plus agg
  #model1_plus_agg <- nls(degree_2nd_moment ~ (vertices/2)^b + d, data = mean_df,
  #                       start = list(b = b_initial, d = d_initial), trace = TRUE)
  
  # Homoscedasticity model 1 plus agg
  #residuals <- residuals(model1_plus_agg)
  
  #png(file.path(homosced_dir, paste0("Residuals_model1_plus_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  #plot(fitted(model1_plus_agg), residuals, main=paste0("Model1 + (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_1_plus_agg")
  #abline(h = 0, col = "red")
  #dev.off()
  
  # Fit model2
  model2 <- nls(degree_2nd_moment ~ a * vertices^b, data = df,
                start = list(a=a_initial, b = b_initial), trace = TRUE)
  
  # Homoscedasticity model 2
  residuals <- residuals(model2)
  
  png(file.path(homosced_dir, paste0("Residuals_model2_not_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(fitted(model2), residuals, main=paste0("Model2 (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_2")
  abline(h = 0, col = "red")
  dev.off()
  
  # Fit model2 agg
  #model2_agg <- nls(degree_2nd_moment ~ a * vertices^b, data = mean_df,
  #                  start = list(a=a_initial, b = b_initial), trace = TRUE)
  
  # Homoscedasticity model 2 agg
  #residuals <- residuals(model2_agg)
  
  #png(file.path(homosced_dir, paste0("Residuals_model2_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  #plot(fitted(model2_agg), residuals, main=paste0("Model2 (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_2_agg")
  #abline(h = 0, col = "red")
  #dev.off()
  
  # Fit model2_plus
  library(minpack.lm)
  model2_plus <- nlsLM(degree_2nd_moment ~ a * vertices^b + d, data = df,
                       start = list(a = a_initial, b = b_initial, d = d_initial),
                       control = nls.control(minFactor = 1e-8, maxiter = 1024),
                       trace = TRUE)
  
  # Homoscedasticity model 2 plus
  residuals <- residuals(model2_plus)
  
  png(file.path(homosced_dir, paste0("Residuals_model2_plus_not_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(fitted(model2_plus), residuals, main=paste0("Model2 + (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_2_plus")
  abline(h = 0, col = "red")
  dev.off()
  
  # Fit model2_plus agg
  # model2_plus_agg <- nlsLM(degree_2nd_moment ~ a * vertices^b + d, data = mean_df,
  #                       start = list(a = a_initial, b = b_initial, d = d_initial),
  #                       control = nls.control(minFactor = 1e-8, maxiter = 1024),
  #                       trace = TRUE)
  # 
  # # Homoscedasticity model 2 plus agg
  # residuals <- residuals(model2_plus_agg)
  # 
  # png(file.path(homosced_dir, paste0("Residuals_model2_plus_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  # plot(fitted(model2_plus_agg), residuals, main=paste0("Model2_plus (aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_2_plus_agg")
  # abline(h = 0, col = "red")
  # dev.off()
  
  # Fit model3
  model3 <- nls(degree_2nd_moment ~ a * exp(c * vertices), data = df,
                start = list(a=a_initial, c=c_initial), trace = TRUE)
  
  # Homoscedasticity model 3
  residuals <- residuals(model3)
  
  png(file.path(homosced_dir, paste0("Residuals_model3_not_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  plot(fitted(model3), residuals, main=paste0("Model3 (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_3")
  abline(h = 0, col = "red")
  dev.off()
  
  # Fit model3 agg
  #model3_agg <- nls(degree_2nd_moment ~ a * exp(c * vertices), data = mean_df,
  #                  start = list(a=a_initial, c=c_initial), trace = TRUE)
  
  # Homoscedasticity model 3 agg
  #residuals <- residuals(model3_agg)
  
  #png(file.path(homosced_dir, paste0("Residuals_model3_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  #plot(fitted(model3_agg), residuals, main=paste0("Model3 (aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model_3_agg")
  #abline(h = 0, col = "red")
  #dev.off()
  
  # Fit model3_plus
  # model3_plus <- nlsLM(degree_2nd_moment ~ a * exp(c * vertices) + d, data = df,
  #                      start = list(a = a_initial,  c=c_initial, d=d_initial),
  #                      control = nls.control(minFactor = 1e-8, maxiter = 1024),
  #                      trace = TRUE)
  
  # Homoscedasticity model 3 plus
  # residuals <- residuals(model3_plus)
  
  # png(file.path(homosced_dir, paste0("Residuals_model3_plus_not_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  # plot(fitted(model3_plus), residuals, main=paste0("Model3_plus (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model3_plus")
  # abline(h = 0, col = "red")
  # dev.off()
  
  # Fit model3_plus agg
  # model3_plus_agg <- nls(degree_2nd_moment ~ a * vertices^b + d, data = mean_df,
  #                        start = list(a=a_initial, b = b_initial, d=d_initial), trace = TRUE)
  # 
  # Homoscedasticity model 3 plus agg
  # residuals <- residuals(model3_plus_agg)
  # 
  # png(file.path(homosced_dir, paste0("Residuals_model3_plus_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  # plot(fitted(model3_plus_agg), residuals, main=paste0("Model3_plus (aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model3_plus_agg")
  # abline(h = 0, col = "red")
  # dev.off()
  
  # Fit model4
  # model4 <- nls(degree_2nd_moment ~ a * n^b * exp(c*n), data = df,
  #               start = list(a=a_initial, b=b_initial, c=c_initial), trace = TRUE)
  
  # Homoscedasticity model 4
  # residuals <- residuals(model4)
  
  # png(file.path(homosced_dir, paste0("Residuals_model4_not_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  # plot(fitted(model4), residuals, main=paste0("Model4 (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model4")
  # abline(h = 0, col = "red")
  # dev.off()
  
  # Fit model4 agg
  # model4_agg <- nls(degree_2nd_moment ~ a * n^b * exp(c*n), data = mean_df,
  #                   start = list(a=a_initial, b=b_initial, c=c_initial), trace = TRUE)
  
  # Homoscedasticity model 4 agg
  # residuals <- residuals(model4_agg)
  
  # png(file.path(homosced_dir, paste0("Residuals_model4_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  # plot(fitted(model4_agg), residuals, main=paste0("Model4 (aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model4_agg")
  # abline(h = 0, col = "red")
  # dev.off()
  
  # Fit model4_plus
  # model4_plus <- nls(degree_2nd_moment ~ a * n^b * exp(c*n) + d, data = df,
  #               start = list(a=a_initial, b=b_initial, c=c_initial, d=d_initial), trace = TRUE)
  
  # Homoscedasticity model 4 plus
  # residuals <- residuals(model4_plus)
  
  # png(file.path(homosced_dir, paste0("Residuals_model4_plus_not_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  # plot(fitted(model4_plus), residuals, main=paste0("Model4_plus (not-aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model4_plus")
  # abline(h = 0, col = "red")
  # dev.off()
  
  # Fit model4_plus agg
  # model4_plus_agg <- nls(degree_2nd_moment ~ a * n^b * exp(c*n) + d, data = mean_df,
  #                   start = list(a=a_initial, b=b_initial, c=c_initial, d=d_initial), trace = TRUE)
  
  # Homoscedasticity model 4 plus agg
  # residuals <- residuals(model4_plus_agg)
  
  # png(file.path(homosced_dir, paste0("Residuals_model4_plus_agg_", table1$Language[i], ".png")), res = res, width = width, height = height)
  # plot(fitted(model4_plus_agg), residuals, main=paste0("Model4_plus (aggregated) - ", table1$Language[i]), ylab = "Residuals", xlab = "Fitted values of Model4_plus_agg")
  # abline(h = 0, col = "red")
  # dev.off()
}

