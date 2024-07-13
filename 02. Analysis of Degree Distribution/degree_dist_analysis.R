################################################################################
################    Complex and Social Networks    #############################
#########################   Lab 2   ############################################
############   Odysseas Kyparissis & Irene Simó Muñoz   ########################
################################################################################
################################################################################


# Setting up paths

# Set as the working directory, the folder where this script is stored.
work_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(work_dir)

# Specify the folder name for the results
results_dir <- file.path(work_dir, "results")

################################################################################
################################################################################

# Installing Libraries

# install.packages("stats4")
# install.packages("VGAM")

# Loading Libraries and Packages

require(stats4) # for MLE
require(VGAM) # for the Riemann-zeta function
require(ggplot2)
require(gridExtra)
require(grid)
source(file.path(work_dir, "likelihoods.R"))
source(file.path(work_dir, "mle.R"))

################################################################################
################################################################################

# Defining utility functions

# Calculation of Akaike Information Criteria
get_AIC <- function(m2logL,K,N) {
  m2logL + 2*K*N/(N-K-1) # AIC with a correction for sample size
}

# Visualization of right truncated zeta distr.
plot_right_truncated_zeta <- function(x, gamma, kmax, language = "English") {
  # Define the range of values for the truncated distribution
  H <- harmonic(kmax, gamma)
  
  # Calculate the PDF values
  zeta_values <- x^(-gamma) / H
  
  # Create a data frame for ggplot
  df <- data.frame(x = x, zeta_values = zeta_values)
  
  # Create a ggplot object with 4 different plots
  p1 <- ggplot(df, aes(x = x, y = zeta_values)) +
    geom_line(color = "#0072B2") +
    geom_point(color = "#D55E00") +  # Add points +
    labs(x = "k", y = "PDF") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
  
  p2 <- p1 + scale_x_log10() + scale_y_log10() +
    labs(x = "log(k)", y = "log(PDF)") +
    theme(legend.position = "none")
  
  p3 <- p1 + scale_x_log10() +
    labs(x = "log(k)", y = "PDF") +
    theme(legend.position = "none")
  
  p4 <- p1 + scale_y_log10() +
    labs(x = "k", y = "log(PDF)") +
    theme(legend.position = "none")
  
  # Combine the title and subtitle
  title_text <- paste("Language: ", language)
  subtitle_text <- paste("Right-Truncated Zeta Distribution (γ =", gamma, ", kmax =", kmax, ")")
  
  # Create a list of plots
  plot_list <- list(p1, p2, p3, p4)
  
  # Create a multi-plot layout
  multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout) + 1, ncol(layout), heights = c(0.1, 0.45,0.45))))
      
      # Add the title and subtitle to the final plot
      grid.text(title_text, x = 0.5, y = 0.98, just = "top", gp = gpar(fontsize = 16))
      grid.text(subtitle_text, x = 0.5, y = 0.94, just = "top", gp = gpar(fontsize = 12))
      
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  # Print the multi-plot
  multiplot(p1, p2, p3, p4, cols = 2)
}

# Visualization of zeta distr.
plot_zeta <- function(x, gamma, language = "English") {
  # Calculate the PDF values
  zeta_values <- x^(-gamma) / zeta(gamma)
  
  # Create a data frame for ggplot
  df <- data.frame(x = x, zeta_values = zeta_values)
  
  # Create a ggplot object with 4 different plots
  p1 <- ggplot(df, aes(x = x, y = zeta_values)) +
    geom_line(color = "#0072B2") +
    geom_point(color = "#D55E00") +
    labs(x = "k", y = "PDF") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
  
  p2 <- p1 + scale_x_log10() + scale_y_log10() +
    labs(x = "log(k)", y = "log(PDF)") +
    theme(legend.position = "none")
  
  p3 <- p1 + scale_x_log10() +
    labs(x = "log(k)", y = "PDF") +
    theme(legend.position = "none")
  
  p4 <- p1 + scale_y_log10() +
    labs(x = "k", y = "log(PDF)") +
    theme(legend.position = "none")
  
  # Combine the title and subtitle
  title_text <- paste("Language: ", language)
  subtitle_text <- paste("Zeta Distribution (γ =", gamma, ")")
  
  # Create a list of plots
  plot_list <- list(p1, p2, p3, p4)
  
  # Create a multi-plot layout
  multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout) + 1, ncol(layout), heights = c(0.1, 0.45,0.45))))
      
      # Add the title and subtitle to the final plot
      grid.text(title_text, x = 0.5, y = 0.98, just = "top", gp = gpar(fontsize = 16))
      grid.text(subtitle_text, x = 0.5, y = 0.94, just = "top", gp = gpar(fontsize = 12))
      
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  # Print the multi-plot
  multiplot(p1, p2, p3, p4, cols = 2)
}


# Visualization of begree barplots
create_degree_barplots <- function(df, results_dir, file_name, n_row, n_col) {
  # Set the PNG file name and resolution
  png(file.path(results_dir, file_name), width = 1920, 
      height = 1080,
      res=150)
  
  # Set up a multi-plot layout
  par(mfrow = c(n_row, n_col))  # 2 rows and 5 columns for 10 plots
  
  for (i in seq(length(df$File))) {
    degree_sequence = read_sequence_table(df$File[i])$V1
    
    # de-normalized version of the empirical degree distribution 
    degree_spectrum = table(degree_sequence)
    
    # Create a barplot for each language
    barplot(degree_spectrum, main = df$Language[i],
            xlab = "degree", ylab = "number of vertices", log = "xy")
    
  }
  
  # Close the PNG device to save the file
  dev.off()
  
  # Reset plot layout
  par(mfrow = c(1, 1))
}

################################################################################
################################################################################


# 1. Introduction
df <- source(file.path(work_dir, "/summary/summary_table.R"))$value # Table 1

write.csv(df, file.path(results_dir, "table1.csv"), row.names=FALSE)

################################################################################
################################################################################

# 2. Visualization - Barplots

create_degree_barplots(df, results_dir, "in-degree-barplots.png", 2, 5)

################################################################################
################################################################################

# 5. Model selection
# + Investigate the consequences of adding a new probability distribution
# that is able to give a better fit than the best model so far

select_model <- function (language, x, N, max_degree, M, M_l, M_N, N_M, C) {
  # Zeta distr.
  mle <- mle_zeta()
  gamma_zeta <- attributes(summary(mle))$coef[1]
  m2logL_zeta <- attributes(summary(mle))$m2logL
  K_zeta <- 1
  AIC_zeta <- get_AIC(m2logL_zeta, K_zeta, N)
  
  # Zeta distr. with gamma=2
  mle <- mle_zeta2()
  gamma_zeta2 <- mle$coef
  m2logL_zeta2 <- mle$m2logL
  K_zeta2 <- 0
  AIC_zeta2 <- mle$AIC_zeta2
  
  # Right truncated zeta distr.
  mle <- mle_right_truncated_zeta()
  kmax_trunc_zeta <- attributes(summary(mle))$coef[1]
  gamma_trunc_zeta <- attributes(summary(mle))$coef[2]
  m2logL_trunc_zeta <- attributes(summary(mle))$m2logL
  K_trunc_zeta <- 2
  AIC_trunc_zeta <- get_AIC(m2logL_trunc_zeta, K_trunc_zeta, N)
  
  # Displaced geometric distribution
  mle <- mle_disp_geom()
  q <- attributes(summary(mle))$coef[1]
  m2logL_geom <- attributes(summary(mle))$m2logL
  K_geom <- 1
  AIC_geom <- get_AIC(m2logL_geom, K_geom, N)
  
  # Displaced Poisson
  mle <- mle_poisson()
  lambda <- attributes(summary(mle))$coef[1]
  m2logL_pois <- attributes(summary(mle))$m2logL
  K_pois <- 1
  AIC_pois <- get_AIC(m2logL_pois, K_pois, N)
  
  # Displaced Altmann
  mle <- mle_altmann()
  gamma_altmann <- attributes(summary(mle))$coef[1]
  delta_altmann <- attributes(summary(mle))$coef[2]
  m2logL_altmann <- attributes(summary(mle))$m2logL
  K_altmann <- 2
  AIC_altmann <- get_AIC(m2logL_altmann, K_altmann, N)
  
  # Create a named vector with your variables and their values
  AIC_values <- c(AIC_pois, AIC_geom, AIC_zeta2, AIC_zeta, AIC_trunc_zeta, 
                  AIC_altmann)
  AIC_names <- c("AIC_pois", "AIC_geom", "AIC_zeta2", "AIC_zeta", 
                 "AIC_trunc_zeta", "AIC_altmann")
  names(AIC_values) <- AIC_names
  
  # Find the minimum AIC value
  min_AIC_value <- min(AIC_values)
  
  # Calculate AIC Difference
  D <- AIC_values - min_AIC_value
  
  # Find the name of the variable with the minimum AIC value
  min_AIC_variable <- names(AIC_values)[which.min(AIC_values)]
  
  
  result <- c(language, lambda, m2logL_pois, K_pois, AIC_pois, q, m2logL_geom,
              K_geom, AIC_geom, gamma_zeta2, m2logL_zeta2, K_zeta2, AIC_zeta2,
              gamma_zeta, m2logL_zeta, K_zeta, AIC_zeta, gamma_trunc_zeta,
              kmax_trunc_zeta, m2logL_trunc_zeta, K_trunc_zeta, AIC_trunc_zeta,
              min_AIC_value, min_AIC_variable, D, gamma_altmann, delta_altmann,
              m2logL_altmann, K_altmann, AIC_altmann)
  return(result)
  
}

model_selection <- data.frame()

for (i in seq(nrow(df))) {
  language <- df$Language[i]
  x <- read_sequence_table(df$File[i])$V1
  N <- as.numeric(df$N[i]) # number of nodes
  max_degree <- as.numeric(df$MaximumDegree[i]) # max_degree
  M <- as.numeric(df$SumDegrees[i]) # sum of degrees
  M_l <- as.numeric(df$SumDegreesLogarithm[i]) # sum of degrees logarithm
  M_N <- as.numeric(df$"M/N"[i]) # mean degree
  N_M <- as.numeric(df$"N/M"[i]) # inverse mean degree
  C <- as.numeric(df$C[i]) # the sum of logarithm of degree factorials
  
  res <- select_model(language, x, N, max_degree, M, M_l, M_N, N_M, C)
  
  model_selection <- rbind(model_selection, res)
  
}

colnames(model_selection) <- c("language", "lambda", "m2logL_pois", "K_pois", 
                               "AIC_pois", "q", "m2logL_geom", "K_geom", 
                               "AIC_geom", "gamma_zeta2", "m2logL_zeta2", 
                               "K_zeta2", "AIC_zeta2", "gamma_zeta", 
                               "m2logL_zeta", "K_zeta", "AIC_zeta", 
                               "gamma_trunc_zeta", "kmax_trunc_zeta", 
                               "m2logL_trunc_zeta", "K_trunc_zeta", 
                               "AIC_trunc_zeta","min_AIC_value", 
                               "min_AIC_variable", "AIC_dIfference_pois",
                               "AIC_dIfference_geom", "AIC_dIfference_zeta2",
                               "AIC_dIfference_zeta", 
                               "AIC_dIfference_trunc_zeta", 
                               "AIC_difference_altmann","gamma_altmann",
                               "delta_altmann",
                               "m2logL_altmann", "K_altmann", "AIC_altmann")

write.csv(model_selection, file.path(results_dir, "models.csv"), 
          row.names=FALSE)

################################################################################
################################################################################

# Plotting the best distributions

# Truncated Zeta as Best model
# The following languages have a zeta truncated distribution as the best model. 

trunc_languages <- c('Arabic', 'Basque', 'Catalan', 'Chinese', 'Czech', 
                     'English', 'Greek', 'Italian')

for (language in trunc_languages[1]) {
  # Extract data
  language_data <- model_selection[model_selection$language == language, 
                                   c('gamma_trunc_zeta', 'kmax_trunc_zeta')]
  language_data <- as.data.frame(lapply(language_data, as.numeric))
  
  # Extract sequence table
  language_seq <- read_sequence_table(df[df$Language == language, c("File")])$V1
  
  # Plot data
  plot_right_truncated_zeta(language_seq, language_data$gamma_trunc_zeta,
                                    language_data$kmax_trunc_zeta, language)
}

# Zeta as Best model
# The following languages have a zeta distribution as the best model.

zeta_languages <- c('Hungarian', 'Turkish')

for (language in zeta_languages[1]) {
  # Extract data
  language_data <- model_selection[model_selection$language == language, 
                                   c('gamma_zeta')]
  g <- as.numeric(language_data)
  
  # Extract sequence table
  language_seq <- read_sequence_table(df[df$Language == language, c("File")])$V1
  
  # Plot data
  plot_zeta(language_seq, g, language)
}

################################################################################
################################################################################

# 6. Checking your methods

################################################################################
################################################################################
# Artificial Zeta distributions.
################################################################################
################################################################################

# Summary of Artificial Zeta distributions.
art_zeta <- source(file.path(work_dir, "/summary/summary_table_zeta.R"))$value

write.csv(art_zeta, file.path(results_dir, "art_zeta.csv"), row.names=FALSE)


# Bar plot of the degree of Artificial Zeta distributions.
create_degree_barplots(art_zeta, results_dir, "art-zeta-barplots.png", 2, 2)

# Estimation of the Degree Distribution for the Artificial Zeta distributions.
model_selection_art_zeta <- data.frame()

for (i in seq(nrow(art_zeta))) {
  language <- art_zeta$Language[i]
  x <- read_sequence_table(art_zeta$File[i])$V1
  N <- as.numeric(art_zeta$N[i]) # number of nodes
  max_degree <- as.numeric(art_zeta$MaximumDegree[i]) # max_degree
  M <- as.numeric(art_zeta$SumDegrees[i]) # sum of degrees
  M_l <- as.numeric(art_zeta$SumDegreesLogarithm[i]) # sum of degrees logarithm
  M_N <- as.numeric(art_zeta$"M/N"[i]) # mean degree
  N_M <- as.numeric(art_zeta$"N/M"[i]) # inverse mean degree
  C <- as.numeric(art_zeta$C[i]) # the sum of logarithm of degree factorials
  real_gamma <- as.numeric(art_zeta$gamma[i])
  
  res <- select_model(language, x, N, max_degree, M, M_l, M_N, N_M, C)
  res["real_gamma"] <- real_gamma
  
  
  model_selection_art_zeta <- rbind(model_selection_art_zeta, res)
  
}

colnames(model_selection_art_zeta) <- c("language", "lambda", "m2logL_pois", "K_pois", 
                               "AIC_pois", "q", "m2logL_geom", "K_geom", 
                               "AIC_geom", "gamma_zeta2", "m2logL_zeta2", 
                               "K_zeta2", "AIC_zeta2", "gamma_zeta", 
                               "m2logL_zeta", "K_zeta", "AIC_zeta", 
                               "gamma_trunc_zeta", "kmax_trunc_zeta", 
                               "m2logL_trunc_zeta", "K_trunc_zeta", 
                               "AIC_trunc_zeta","min_AIC_value", 
                               "min_AIC_variable", "AIC_dIfference_pois",
                               "AIC_dIfference_geom", "AIC_dIfference_zeta2",
                               "AIC_dIfference_zeta", 
                               "AIC_dIfference_trunc_zeta", 
                               "AIC_difference_altmann","gamma_altmann",
                               "delta_altmann",
                               "m2logL_altmann", "K_altmann", "AIC_altmann", "real_gamma")

# Saving information of the estimation procedure.
write.csv(model_selection_art_zeta, file.path(results_dir, 
                                              "models-art-zeta.csv"),
                                              row.names=FALSE)

# Visualization of artificial zeta distr. with the estimation
plot_art_zeta <- function(x, real_gamma, gamma_est, language = "English") {
  # Calculate the PDF values
  real_zeta_values <- x^(-real_gamma) / zeta(real_gamma)
  est_zeta_values <- x^(-gamma_est) / zeta(gamma_est)
  
  # Create a data frame for ggplot
  df <- data.frame(x = x, real_zeta_values = real_zeta_values, est_zeta_values = est_zeta_values)
  
  # Create a ggplot object with 4 different plots
  p1 <- ggplot(df, aes(x = x, y = real_zeta_values)) +
    geom_point(aes(), shape = 19) +
    geom_line(aes(y = real_zeta_values, linetype = "Real PDF", color ="Real PDF")) +
    geom_line(aes(y = est_zeta_values, linetype = "Estimated PDF", color = "Estimated PDF")) +
    labs(x = "k", y = "PDF") +
    scale_color_manual(values = c("Real PDF" = "#0072B2", "Estimated PDF" = "#D55E00")) +
    scale_linetype_manual(values = c("Real PDF" = "solid", "Estimated PDF" = "dashed")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "top",
      #legend.title = element_text(size = 12),
      #legend.text = element_text(size = 10)
    )
  
  p2 <- p1 + scale_x_log10() + scale_y_log10() +
    labs(x = "log(k)", y = "log(PDF)") +
    theme(legend.position = "none")
  
  p3 <- p1 + scale_x_log10() +
    labs(x = "log(k)", y = "PDF") +
    theme(legend.position = "none")
  
  p4 <- p1 + scale_y_log10() +
    labs(x = "k", y = "log(PDF)") +
    theme(legend.position = "none")
  
  # Combine the title and subtitle
  title_text <- paste("Distribution: ", language)
  subtitle_text <- paste("Zeta Distribution (γ =", real_gamma, ", Estimated γ =", gamma_est,")")
  
  # Create a list of plots
  plot_list <- list(p1, p2, p3, p4)
  
  # Create a multi-plot layout
  multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout) + 1, ncol(layout), heights = c(0.1, 0.45,0.45))))
      
      # Add the title and subtitle to the final plot
      grid.text(title_text, x = 0.5, y = 0.98, just = "top", gp = gpar(fontsize = 16))
      grid.text(subtitle_text, x = 0.5, y = 0.94, just = "top", gp = gpar(fontsize = 12))
      
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  
  # Print the multi-plot
  multiplot(p1, p2, p3, p4, cols = 2)
}

# Visualization of artificial zeta distr. with Zeta trunc. estimation
plot_art_zeta_with_trunc_est <- function(x, real_gamma, gamma_est, kmax_est,
                                         language = "English") {
  # Calculate the PDF values
  real_zeta_values <- x^(-real_gamma) / zeta(real_gamma)
  H <- harmonic(kmax_est, gamma_est)
  est_zeta_values <- x^(-gamma_est) / H
  
  # Create a data frame for ggplot
  df <- data.frame(x = x, real_zeta_values = real_zeta_values, 
                   est_zeta_values = est_zeta_values)
  
  # Create a ggplot object with 4 different plots
  p1 <- ggplot(df, aes(x = x, y = real_zeta_values)) +
    geom_point(aes(), shape = 19) +
    geom_line(aes(y = real_zeta_values, linetype = "Real PDF", color ="Real PDF")) +
    geom_line(aes(y = est_zeta_values, linetype = "Estimated Trunc. PDF", color = "Estimated Trunc. PDF")) +
    labs(x = "k", y = "PDF") +
    scale_color_manual(values = c("Real PDF" = "#0072B2", "Estimated Trunc. PDF" = "#D55E00")) +
    scale_linetype_manual(values = c("Real PDF" = "solid", "Estimated Trunc. PDF" = "dashed")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "top",
      #legend.title = element_text(size = 12),
      #legend.text = element_text(size = 10)
    )
  
  p2 <- p1 + scale_x_log10() + scale_y_log10() +
    labs(x = "log(k)", y = "log(PDF)") +
    theme(legend.position = "none")
  
  p3 <- p1 + scale_x_log10() +
    labs(x = "log(k)", y = "PDF") +
    theme(legend.position = "none")
  
  p4 <- p1 + scale_y_log10() +
    labs(x = "k", y = "log(PDF)") +
    theme(legend.position = "none")
  
  # Combine the title and subtitle
  title_text <- paste("Distribution: ", language)
  subtitle_text <- paste("Zeta Distribution (γ =", real_gamma, ", Estimated γ=",
                         gamma_est, ", Estimated kmax =", kmax_est,")")
  
  # Create a list of plots
  plot_list <- list(p1, p2, p3, p4)
  
  # Create a multi-plot layout
  multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout) + 1, ncol(layout), heights = c(0.1, 0.45,0.45))))
      
      # Add the title and subtitle to the final plot
      grid.text(title_text, x = 0.5, y = 0.98, just = "top", gp = gpar(fontsize = 16))
      grid.text(subtitle_text, x = 0.5, y = 0.94, just = "top", gp = gpar(fontsize = 12))
      
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  
  # Print the multi-plot
  multiplot(p1, p2, p3, p4, cols = 2)
}

# Call of visualization functions
for (distribution in model_selection_art_zeta$language) {
  # Extract data
  best_model <- model_selection_art_zeta[model_selection_art_zeta$language
                                         == distribution, 
                                         c('min_AIC_variable')]
  real_gamma <- as.numeric(
    model_selection_art_zeta[model_selection_art_zeta$language == distribution, 
                             c('real_gamma')])
  # Extract sequence table
  distr_seq <- read_sequence_table(art_zeta[art_zeta$Language == distribution,
                                            c("File")])$V1
  
  if (best_model == "AIC_zeta"){
    
    gamma_est <- as.numeric(
      model_selection_art_zeta[model_selection_art_zeta$language == distribution, 
                               c('gamma_zeta')])
    
    # Plot data
    plot_art_zeta(distr_seq, real_gamma, gamma_est, distribution)
    
  } else if (best_model == "AIC_trunc_zeta") {
    
    gamma_est <- as.numeric(
      model_selection_art_zeta[model_selection_art_zeta$language == distribution, 
                               c('gamma_trunc_zeta')])
    kmax_est <- as.numeric(
      model_selection_art_zeta[model_selection_art_zeta$language == distribution, 
                               c('kmax_trunc_zeta')])
    
    plot_art_zeta_with_trunc_est(distr_seq, real_gamma, gamma_est, kmax_est, 
                                 distribution)
  }
  
}


################################################################################
################################################################################
# Artificial Geometric distributions.
################################################################################
################################################################################
art_geom <- source(file.path(work_dir, "/summary/summary_table_geom.R"))$value

write.csv(art_geom, file.path(results_dir, "art_geom.csv"), row.names=FALSE)

create_degree_barplots(art_geom, results_dir, "art-geom-barplots.png", 2, 3)


model_selection_art_geom <- data.frame()

for (i in seq(nrow(art_geom))) {
  language <- art_geom$Language[i]
  x <- read_sequence_table(art_geom$File[i])$V1
  N <- as.numeric(art_geom$N[i]) # number of nodes
  max_degree <- as.numeric(art_geom$MaximumDegree[i]) # max_degree
  M <- as.numeric(art_geom$SumDegrees[i]) # sum of degrees
  M_l <- as.numeric(art_geom$SumDegreesLogarithm[i]) # sum of degrees logarithm
  M_N <- as.numeric(art_geom$"M/N"[i]) # mean degree
  N_M <- as.numeric(art_geom$"N/M"[i]) # inverse mean degree
  C <- as.numeric(art_geom$C[i]) # the sum of logarithm of degree factorials
  real_q <- as.numeric(art_geom$q[i])
  
  res <- select_model(language, x, N, max_degree, M, M_l, M_N, N_M, C)
  res["real_q"] <- real_q
  
  model_selection_art_geom <- rbind(model_selection_art_geom, res)
  
}

colnames(model_selection_art_geom) <- c("language", "lambda", "m2logL_pois", "K_pois", 
                                        "AIC_pois", "q", "m2logL_geom", "K_geom", 
                                        "AIC_geom", "gamma_zeta2", "m2logL_zeta2", 
                                        "K_zeta2", "AIC_zeta2", "gamma_zeta", 
                                        "m2logL_zeta", "K_zeta", "AIC_zeta", 
                                        "gamma_trunc_zeta", "kmax_trunc_zeta", 
                                        "m2logL_trunc_zeta", "K_trunc_zeta", 
                                        "AIC_trunc_zeta","min_AIC_value", 
                                        "min_AIC_variable", "AIC_dIfference_pois",
                                        "AIC_dIfference_geom", "AIC_dIfference_zeta2",
                                        "AIC_dIfference_zeta", 
                                        "AIC_dIfference_trunc_zeta", "AIC_difference_altmann","gamma_altmann",
                                        "delta_altmann",
                                        "m2logL_altmann", "K_altmann", "AIC_altmann", "real_q")

write.csv(model_selection_art_geom, file.path(results_dir, 
                                              "models-art-geom.csv"),
                                              row.names=FALSE)

# Visualization function for geometric distribution estimation
plot_art_geom <- function(x, real_q, q_est, language = "English") {
  # Calculate the PDF values
  real_geom_values <- ((1-real_q)^x)*real_q
  est_geom_values <- ((1-q_est)^x)*q_est
  
  # Create a data frame for ggplot
  df <- data.frame(x = x, real_geom_values = real_geom_values, 
                   est_geom_values = est_geom_values)
  
  # Create a ggplot object with 4 different plots
  p1 <- ggplot(df, aes(x = x, y = real_geom_values)) +
    geom_point(aes(), shape = 19) +
    geom_line(aes(y = real_geom_values, linetype = "Real PDF", color ="Real PDF")) +
    geom_line(aes(y = est_geom_values, linetype = "Estimated PDF", color = "Estimated PDF")) +
    labs(x = "k", y = "PDF") +
    scale_color_manual(values = c("Real PDF" = "#0072B2", "Estimated PDF" = "#D55E00")) +
    scale_linetype_manual(values = c("Real PDF" = "solid", "Estimated PDF" = "dashed")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "top",
      #legend.title = element_text(size = 12),
      #legend.text = element_text(size = 10)
    )
  
  p2 <- p1 + scale_x_log10() + scale_y_log10() +
    labs(x = "log(k)", y = "log(PDF)") +
    theme(legend.position = "none")
  
  p3 <- p1 + scale_x_log10() +
    labs(x = "log(k)", y = "PDF") +
    theme(legend.position = "none")
  
  p4 <- p1 + scale_y_log10() +
    labs(x = "k", y = "log(PDF)") +
    theme(legend.position = "none")
  
  # Combine the title and subtitle
  title_text <- paste("Distribution: ", language)
  subtitle_text <- paste("Displaced Geom. Distribution (q =", real_q, ", Estimated q=",
                         q_est,")")
  
  # Create a list of plots
  plot_list <- list(p1, p2, p3, p4)
  
  # Create a multi-plot layout
  multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout) + 1, ncol(layout), heights = c(0.1, 0.45,0.45))))
      
      # Add the title and subtitle to the final plot
      grid.text(title_text, x = 0.5, y = 0.98, just = "top", gp = gpar(fontsize = 16))
      grid.text(subtitle_text, x = 0.5, y = 0.94, just = "top", gp = gpar(fontsize = 12))
      
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  
  # Print the multi-plot
  multiplot(p1, p2, p3, p4, cols = 2)
}


# Call of visualization function
for (distribution in model_selection_art_geom$language) {
  
  # Extract data
  real_q <- as.numeric(
    model_selection_art_geom[model_selection_art_geom$language == distribution, 
                             c('real_q')])
  
  q_est <- as.numeric(
    model_selection_art_geom[model_selection_art_geom$language == distribution, 
                             c('q')])
  
  # Extract sequence table
  distr_seq <- read_sequence_table(art_geom[art_geom$Language == distribution,
                                            c("File")])$V1
    
  plot_art_geom(distr_seq, real_q, q_est, distribution)
  
}

