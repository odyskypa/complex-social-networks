##############################################################################
################    Complex and Social Networks    ###########################
#########################   Lab 3   ##########################################
############   Leandra XXXX & Odysseas Kyparissis  ###########################
##############################################################################
##############################################################################

# Import Libraries
import os
import pandas as pd

from multiprocessing import Pool


from utils import *

def main():
    # Define paths
    data_folder_path = "data"
    processed_folder_path = os.path.join(data_folder_path, "processed")
    results_folder_path = os.path.join(data_folder_path, "results")
    figures_folder_path = os.path.join(results_folder_path, "figures")
    os.makedirs(figures_folder_path, exist_ok=True)
    files = os.listdir(processed_folder_path)

    ##############################################################################
    ##############################################################################
    # 2.1 Test of Significance Erdos-Renyi Model
    ##############################################################################
    ##############################################################################

    for file_name in files:
        # if not file_name.startswith("Basque"):
        #     continue
        file_path = os.path.join(processed_folder_path, file_name)
        
        # Get language's name from file name
        # Split the filename by underscores and get the first part
        language_name = file_name.split('_')[0]
        
        G, num_nodes, num_edges, _, _ = generate_graph_from_txt_file(file_path)
        general_info = pd.read_csv(os.path.join(results_folder_path, "general_statistics.csv"), index_col=[0])

        clustering_coefficient = general_info.loc[language_name, 'clustering_coefficient']

        # 1st null hypothesis

        # A binomial graph (Erdos-Renyi graph) with the same number of vertices
        # and edges as the real network. This null model has no free parameter.

        # Monte Carlo Procedure
        T = 500  # Adjust the number of random graphs as needed
        random_clustering_coefficients = []

        # Generate a large number of Erdos-Renyi random graphs and 
        # calculate the clustering coefficient for each:
        # Use a Pool of workers to parallelize the process
        with Pool() as pool:
            random_clustering_coefficients = pool.starmap(generate_random_graph, [(num_nodes, num_edges)] * T)

        print(f'The random clustering coefficients generated for {language_name} language are: \n {random_clustering_coefficients}')

        p_value = sum(random_clustering_coeff >= clustering_coefficient for random_clustering_coeff in random_clustering_coefficients) / T
        print(f'The p_value of the ER hypothesis test for {language_name} language is: {p_value:.17f}')
        plot_clustering_coefficient_sim(random_clustering_coefficients, clustering_coefficient, p_value, language_name, T, False, figures_folder_path)

        # Second null hypothesis

        # A randomized graph with the same degree sequence of the original graph.
        # The switching model is the randomization to use for this session.
        
        switched_clustering_coefficients = []
        
        # Get the degree sequence of the graph:
        degree_sequence = sorted([d for n, d in G.degree()], reverse=True)
        plot_degree_sequence(degree_sequence, language_name, False, figures_folder_path)
        
        with Pool() as pool:
            switched_clustering_coefficients = pool.starmap(generate_switch_model, [(G, num_edges, 20, degree_sequence)] * T)
            
        print(f'The Switched clustering coefficients generated for {language_name} language are: \n {switched_clustering_coefficients}')
        
        p_value_switched = sum(switched_clustering_coeff >= clustering_coefficient for switched_clustering_coeff in switched_clustering_coefficients) / T
        print(f'The p_value of the Switched hypothesis test for {language_name} language is: {p_value_switched:.17f}')
        plot_clustering_coefficient_sim(switched_clustering_coefficients, clustering_coefficient, p_value_switched, language_name, T, False, figures_folder_path, True)

if __name__ == '__main__':
    main()