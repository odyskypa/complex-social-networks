################################################################################
################    Complex and Social Networks    #############################
#########################   Lab 1   ############################################
############   Odysseas Kyparissis & José Ángel V    ###########################
################################################################################
################################################################################

# Install and load the necessary libraries

install.packages("igraph")
library(igraph)

################################################################################
################################################################################

# Task (a): Plot the clustering coefficient and the average
# shortest-path as a function of the parameter p of the WS model.


# Define parameters
num_nodes <- 500
nei <- 4
p_values <- 10^(seq(-4,0,0.2))
num_simulations <- 100


# Create a function to calculate clustering coefficient and average shortest
# path for a given p
compute_metrics <- function(p, num_nodes, nei, num_simulations) {
  
  # Generate Watts-Strogatz graphs with the specified parameters
  graphs <- replicate(num_simulations, sample_smallworld(1, num_nodes, nei, p))
  
  # Calculate clustering coefficient and average shortest path for all graphs
  clustering_coef <- sapply(graphs, transitivity)
  avg_shortest_path <- sapply(graphs, average.path.length)
  
  # Return the average values
  return(list(
    avg_clustering = mean(clustering_coef),
    avg_shortest_path = mean(avg_shortest_path)
  ))
}

# Run the simulations and collect the results
results <- lapply(p_values, function(p) {
  compute_metrics(p, num_nodes, nei, num_simulations)
})

# Extract the average clustering coefficient and average shortest path values
avg_clustering_values <- sapply(results, function(res) res$avg_clustering)
avg_shortest_path_values <- sapply(results, function(res) res$avg_shortest_path)

# Normalize the values by dividing by their initial values
normalized_avg_clustering <- avg_clustering_values / avg_clustering_values[1]
normalized_avg_shortest_path <- avg_shortest_path_values / avg_shortest_path_values[1]

# Generate the plot
# Create a palette of colors for the lines
line_colors <- c("#0072B2", "#D55E00")

# Plot the first line with customized settings
plot(
  p_values,
  normalized_avg_clustering,
  ylim = c(0, 1),
  type = "o",
  pch = 16,
  log = 'x',
  xlab = "p",
  ylab = "Normalized Coefficient",
  main = "Watts-Strogatz Model",
  col = line_colors[1]
)
lines(
  p_values,
  normalized_avg_shortest_path,
  type = "o",
  pch = 16,
  col = line_colors[2]
)

# Add grid lines
grid()

# Add labels for the lines
text(
  x = p_values[15],
  y = normalized_avg_clustering[15],
  labels = "C(p) / C(0)",
  pos = 4,
  col = line_colors[1]
)
text(
  x = p_values[15],
  y = normalized_avg_shortest_path[15],
  labels = "L(p) / L(0)",
  pos = 1,
  col = line_colors[2]
)

# Add legend with customized settings
legend(
  "topright",
  legend = c("Normalized Clustering Coefficient", "Normalized Shortest Path"),
  col = line_colors,
  lty = 1,
  cex = 0.8
)

################################################################################
################################################################################

# Task (b): Plot the average shortest-path length as a function of the network 
# size of the ER model.

# Define a function to generate a random G(n,p) graph and calculate average path
# length
generate_and_measure_ER <- function(n, p) {
  graph <- sample_gnp(n, p)
  avg_length <- average.path.length(graph)
  return(avg_length)
}


# Function to calculate average shortest path length for a single realization
calculate_avg_shortest_path <- function(n, p, num_simulations) {
  avg_lengths <- numeric(num_simulations)
  
  # Run the simulations using lapply
  avg_lengths <- unlist(lapply(1:num_simulations, function(i) 
    generate_and_measure_ER(n, p)))
  
  return(mean(avg_lengths))
}

# Parameters
epsilon <- 0.01
num_simulations <- 50  # Number of simulations for each network size
max_2_n <- 14  # Maximum power of 2 for the number of nodes

# Generate a range of network sizes (number of nodes)
network_sizes <- 2^seq(4, max_2_n, by = 1)

# Initialize an empty vector to store results
avg_path_lengths <- numeric(length(network_sizes))

# Calculate the average shortest path length for each network size
avg_path_lengths <- lapply(network_sizes, function(n) {
  p <- ((1 + epsilon) * log(n)) / n
  calculate_avg_shortest_path(n, p, num_simulations)
})

# Convert the list to a vector
avg_path_lengths_v <- unlist(avg_path_lengths)

# Plot the results
# Create a palette of colors for the lines
line_colors <- c("#0072B2")

# Plot the results with customized settings
plot(
  network_sizes,
  avg_path_lengths_v,
  type = "o",
  pch = 16,
  xlab = "Network Size (n)",
  ylab = "Average Shortest Path Length",
  main = "Average Shortest Path Length vs. Network Size",
  col = line_colors[1],
  ylim = c(min(avg_path_lengths_v) - 0.01, max(avg_path_lengths_v) + 0.01),
  xlim = c(min(network_sizes) - 1, max(network_sizes) + 1)
)

# Add grid lines
grid()

# Add legend with customized settings
legend(
  "bottomright",
  legend = "ER Model",
  col = line_colors,
  lty = 1,
  cex = 0.8
)

