##############################################################################
################    Complex and Social Networks    ###########################
#########################   Lab 3   ##########################################
############   Leandra XXXX & Odysseas Kyparissis  ###########################
##############################################################################
##############################################################################

##############################################################################
##############################################################################
# 1. Introduction - Setting up Libraries, Functions and Configuration
##############################################################################
##############################################################################

# Import libraries
import os
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt
from multiprocessing import Pool

from utils import * 

# Define paths
data_folder_path = "data"
raw_data_folder_path = os.path.join(data_folder_path, "raw")
processed_folder_path = os.path.join(data_folder_path, "processed")
results_folder_path = os.path.join(data_folder_path, "results")
os.makedirs(processed_folder_path, exist_ok=True)
os.makedirs(results_folder_path, exist_ok=True)
files = os.listdir(raw_data_folder_path)

##############################################################################
##############################################################################
# 2. Data preparation
##############################################################################
##############################################################################

# Create a Dataframe to save general statistics of the analysis
# Define column names as a list
column_names = [
    'original_num_nodes',
    'original_num_edges',
    'original_average_degree',
    'original_density_of_edges',
    'final_num_nodes',
    'final_num_edges',
    'final_average_degree',
    'final_density_of_edges',
    'loops_removed',
    'clustering_coefficient']

# Create an empty DataFrame with the specified column names
df = pd.DataFrame(columns=column_names)

for raw_file_name in files:
    raw_file_path = os.path.join(raw_data_folder_path, raw_file_name)

    # Output
    # Add "_no_loops" before .txt
    output_file_name = raw_file_name.replace('.txt', '_processed.txt')
    output_file_path = os.path.join(processed_folder_path, output_file_name)
    
    # Get language's name from file name
    # Split the filename by underscores and get the first part
    language_name = raw_file_name.split('_')[0]
    
    G, original_num_nodes, original_num_edges, original_average_degree, original_density_of_edges = \
    generate_graph_from_txt_file(raw_file_path)

    
   # Remove loops and multiedges from the random graph
    G = remove_loops(G)
    G = remove_multiedges(G)

    # Calculate the final network properties
    final_num_nodes = len(G.nodes())
    final_num_edges = len(G.edges())
    # Since the graph is undirected
    final_average_degree = 2 * final_num_edges / final_num_nodes
    
    save_processed_graph_to_txt(output_file_path, G, final_num_nodes, final_num_edges)
    print(f"Graph data has been saved to {output_file_path}")
    
    final_density_of_edges = 2 * final_num_edges / (final_num_nodes*(final_num_nodes - 1))
    loops_removed = original_num_edges - final_num_edges
    
    # Compute the clustering coefficient for the entire graph
    clustering_coefficient = nx.average_clustering(G)

    print(f"Clustering Coefficient of {language_name} language's graph  is: ", clustering_coefficient)

    # Append the new row to the DataFrame
    df.loc[language_name] = [original_num_nodes,
                            original_num_edges,
                            original_average_degree,
                            original_density_of_edges,
                            final_num_nodes,
                            final_num_edges,
                            final_average_degree,
                            final_density_of_edges,
                            loops_removed,
                            clustering_coefficient]

# Save to CSV
df.to_csv(os.path.join(results_folder_path, "general_statistics.csv"), index=True)