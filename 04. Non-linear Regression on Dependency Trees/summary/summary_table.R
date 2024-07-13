valid_filter <- function(df) {
  # Convert 'n' to numeric
  df$vertices <- as.numeric(df$vertices)
  df$degree_2nd_moment <- as.numeric(df$degree_2nd_moment)
  df$mean_length <- as.numeric(df$mean_length)
  
  # Check the validity of k2
  result_df <- df %>%
    filter((4 - (6/vertices)) <= degree_2nd_moment, degree_2nd_moment <= vertices - 1)
  # result_df <- first_filter %>%
  #   filter((vertices*degree_2nd_moment/8*(vertices-1)) + 0.5 <= mean_length, mean_length <= vertices - 1)
  # 
  return(result_df)
}

write_summary <- function(language, file) {
   dependency_tree_metrics = read_metrics_table(file)
   colnames(dependency_tree_metrics) = c("vertices","degree_2nd_moment", "mean_length")
   dependency_tree_metrics = dependency_tree_metrics[order(dependency_tree_metrics$vertices),]
   N_before <- nrow(dependency_tree_metrics)
   
   dependency_tree_metrics <- valid_filter(dependency_tree_metrics)
   substring <- sub(".*/(\\w+\\.txt)", "\\1", file)
   processed_data_path <- file.path(processed_dir, substring)
   processed_file <- gsub("\\s+", "", paste("./data/processed_dependency_tree_metrics/", substring))
   write.table(dependency_tree_metrics, processed_data_path, row.names=FALSE, col.names = TRUE)
   
   N <- nrow(dependency_tree_metrics)
   mean_n <- mean(dependency_tree_metrics$vertices)
   sigma_n <- sd(dependency_tree_metrics$vertices)
   mean_x <- mean(dependency_tree_metrics$degree_2nd_moment)
   sigma_x <- sd(dependency_tree_metrics$degree_2nd_moment)
   sentences_filtered <- N_before - N
   
   lin <- c(language, file, processed_file, N, mean_n, sigma_n, mean_x, sigma_x, sentences_filtered)
}

read_metrics_table <- function (file) {
    dependency_tree_metrics <- read.table(file, header = FALSE)
}

# Generate Table 1  
source = read.table("summary/list.txt", 
         header = TRUE,
         as.is = c("language","file")
        )

table1 <- data.frame()
for (x in 1:nrow(source)) {
  table1 <- rbind(table1, write_summary(source$language[x], source$file[x]))
}
colnames(table1) <- c("Language", "File", "processed_file", "N", "mean_n", "sigma_n", "mean_x", "sigma_x", "sentences_filtered")
rm(x, source)
return(table1)
