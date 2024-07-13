##############################################################################
################    Complex and Social Networks    ###########################
#########################   Lab 3   ##########################################
############   Leandra XXXX & Odysseas Kyparissis  ###########################
##############################################################################
##############################################################################
import os
import networkx as nx
import matplotlib.pyplot as plt

# Define functions

def switching_model(original_graph, Q):
    # Create a copy of the original graph to work with
    randomized_graph = original_graph.copy()
    edges = list(randomized_graph.edges())
    num_edges = len(edges)
    
    # Calculate the number of switch trials required
    randomized_graph = nx.double_edge_swap(original_graph, Q, num_edges)
    # Calculate the clustering coefficient for the random graph
    switched_clustering_coeff = nx.average_clustering(randomized_graph)
    
    
    
    # Initialize a counter for successful switches
    # attempted_switches = 0
    
    # # Create a set to keep track of attempted switches
    # completed_switches = set()
    
    # while attempted_switches < num_switch_trials:
    #     # Randomly and uniformly select two edges
    #     # edge1 : u-v
    #     # edge2: s-t
    #     edge1, edge2 = random.sample(edges, 2)
            
    #     # Check if the selected edges share any vertices
    #     if len(set(edge1).intersection(set(edge2))) == 0:
    #         # Check if the switch has not been attempted before
    #         if (edge1, edge2) not in completed_switches and (edge2, edge1) not in completed_switches:
    #             # Perform the switch and update the graph
    #             randomized_graph.remove_edge(*edge1)
    #             randomized_graph.remove_edge(*edge2)
    #             randomized_graph.add_edge(edge1[0], edge2[1])
    #             randomized_graph.add_edge(edge2[0], edge1[1])
                
    #             # Update counters
    #             attempted_switches += 1
    #             completed_switches.add((edge1, edge2))
    #         else:
    #             attempted_switches +=1
    #     else:
    #         attempted_switches +=1

    return randomized_graph, switched_clustering_coeff

def save_processed_graph_to_txt(output_file_path, G, final_num_nodes, final_num_edges):
    # Save the graph data to a text file
    with open(output_file_path, 'w') as file:
        # Write the header with the number of nodes and edges
        file.write(f"{final_num_nodes} {final_num_edges}\n")

        # Write the edges of the graph
        for edge in G.edges():
            file.write(f"{edge[0]} {edge[1]}\n")

def generate_graph_from_txt_file(raw_file_path):
    # Create an empty graph
    G = nx.Graph()
    
    # Read the file and add edges to the graph
    with open(raw_file_path, 'r', encoding='utf-8') as file:
        # Read the header to get the original number of nodes and edges
        original_num_nodes, original_num_edges = map(int, file.readline().split())
        original_average_degree = 2 * original_num_edges / original_num_nodes
        original_density_of_edges = 2 * original_num_edges / (original_num_nodes*(original_num_nodes - 1))

        # Read the edges and add them to the graph
        for line in file: 
            # Split the line using any whitespace characters
            parts = line.strip().split()
            
            # Check if there are at least two parts (vertices)
            if len(parts) >= 2:
                vertex1, vertex2 = parts[:2]  # Take the first two parts as vertices
                if vertex1 != vertex2:
                    G.add_edge(vertex1, vertex2)
    
    return G, original_num_nodes, original_num_edges, original_average_degree, original_density_of_edges

def plot_graph(G):
    # Define the layout for the graph (optional, you can choose different layouts)
    pos = nx.spring_layout(G)

    # Draw the nodes and edges
    nx.draw(G, pos, with_labels=True, node_size=500, node_color="skyblue", font_size=10, font_color="black")
    nx.draw_networkx_edges(G, pos)

    # Display the graph
    plt.show()

def remove_loops(G):
    # Remove self-loops (edges connecting a node with itself)
    for node in list(G.nodes):
        if G.has_edge(node, node):
            G.remove_edge(node, node)
    return G

def remove_multiedges(G):
    # Create an empty graph
    new_G = nx.Graph()

    # Add edges from your original graph, avoiding multiedges
    for u, v in G.edges():
        if not new_G.has_edge(u, v):
            new_G.add_edge(u, v)
        else:
            print("multiedge found")
    return new_G

def plot_clustering_coefficient_sim(random_clustering_coefficients, real_clustering_coeff, p_value, language_name, T, plot=False, save_dir='', switch = False):
    
    # Visualize the distribution and comparison (step 4)
    plt.figure(figsize=(12, 8), dpi=300)
    plt.hist(random_clustering_coefficients, bins=30, alpha=0.5, label='Random Graphs')
    # Annotate the plot with the value of real_clustering_coeff
    plt.axvline(real_clustering_coeff, color='red', linestyle='dashed', linewidth=2, label='Real Network')
    plt.xlabel('Clustering Coefficient')
    plt.ylabel('Frequency')
    plt.legend()
    plt.suptitle(f'{language_name} language, T = {T}', fontsize=16)
    
    if switch:
        title = f'Switched Clustering Coefficient Comparison (p-value = {p_value:.4f})'
        graph_title = f'{os.path.join(save_dir, language_name)}_switch_sim.png'
    else:
        title = f'Random Clustering Coefficient Comparison (p-value = {p_value:.4f})'
        graph_title = f'{os.path.join(save_dir, language_name)}_er_sim.png'
    plt.title(title, fontsize=12)
    
    if plot:
        plt.show()
    else:
        # Save the graph as an image
        plt.savefig(graph_title, dpi=300)  # You can specify the file format (e.g., .png, .jpg, .pdf)

        # To avoid displaying the graph, you can optionally use plt.close()
        plt.close()

def generate_random_graph(num_nodes, num_edges):
    # Calculate the probability p
    p = num_edges / (num_nodes * (num_nodes - 1) / 2)
    
    # Define a function for generating a random graph and calculating its clustering coefficient
    # Generate a new Erdos-Renyi graph
    random_graph = nx.erdos_renyi_graph(num_nodes, p)
    
    # Remove loops and multiedges from the random graph
    random_graph = remove_loops(random_graph)
    random_graph = remove_multiedges(random_graph)
    
    # Calculate the clustering coefficient for the random graph
    random_clustering_coeff = nx.average_clustering(random_graph)
    
    return random_clustering_coeff


def generate_switch_model(G, num_edges, Q, degree_sequence):
    # Create a copy of the original graph to work with
    randomized_graph = G.copy()
    
    # Calculate the number of switch trials required
    randomized_graph = nx.double_edge_swap(G, Q, num_edges)
    # Calculate the clustering coefficient for the random graph
    random_clustering_coeff = nx.average_clustering(randomized_graph)
    
    # Attempt to try checking if the degree sequences are the same
    #random_degree_sequence = sorted([d for n, d in randomized_graph.degree()], reverse=True)
    #print(random_degree_sequence == degree_sequence)
    
    return random_clustering_coeff

def plot_degree_sequence(degree_sequence, language_name, plot=False, save_dir='', switch=False):
    # Create a figure with subplots for all four combinations of scales
    fig, axes = plt.subplots(2, 2, figsize=(12, 8))

    # Define different marker styles and colors
    marker_style = 'o'
    line_color = '#D55E00'
    marker_edge_color = '#0072B2'
    marker_face_color = 'none'

    # Linear scale, linear scale
    axes[0, 0].plot(degree_sequence, marker_style, markersize=3, color=line_color,
                    markerfacecolor=marker_face_color, markeredgecolor=marker_edge_color)
    axes[0, 0].set_title("Degree Sequence (Linear, Linear)")
    axes[0, 0].set_xlabel("Node")
    axes[0, 0].set_ylabel("Degree")

    # Linear scale, logarithmic scale
    axes[0, 1].plot(degree_sequence, marker_style, markersize=3, color=line_color,
                    markerfacecolor=marker_face_color, markeredgecolor=marker_edge_color)
    axes[0, 1].set_yscale('log')
    axes[0, 1].set_title("Degree Sequence (Linear, Logarithmic)")
    axes[0, 1].set_xlabel("Node")
    axes[0, 1].set_ylabel("Degree (log scale)")

    # Logarithmic scale, linear scale
    axes[1, 0].plot(degree_sequence, marker_style, markersize=3, color=line_color,
                    markerfacecolor=marker_face_color, markeredgecolor=marker_edge_color)
    axes[1, 0].set_xscale('log')
    axes[1, 0].set_title("Degree Sequence (Logarithmic, Linear)")
    axes[1, 0].set_xlabel("Node (log scale)")
    axes[1, 0].set_ylabel("Degree")

    # Logarithmic scale, logarithmic scale
    axes[1, 1].plot(degree_sequence, marker_style, markersize=3, color=line_color,
                    markerfacecolor=marker_face_color, markeredgecolor=marker_edge_color)
    axes[1, 1].set_xscale('log')
    axes[1, 1].set_yscale('log')
    axes[1, 1].set_title("Degree Sequence (Logarithmic, Logarithmic)")
    axes[1, 1].set_xlabel("Node (log scale)")
    axes[1, 1].set_ylabel("Degree (log scale")

    if switch:
        subtitle = f'Degree Sequence of {language_name} language, Switch model'
        graph_title = f'{os.path.join(save_dir, language_name)}_sequence_degree_switched.png'
    else:
        subtitle = f'Degree Sequence of {language_name} language, Original data'
        graph_title = f'{os.path.join(save_dir, language_name)}_sequence_degree.png'
    # Add a title to the entire figure
    fig.suptitle(subtitle, fontsize=16)

    plt.tight_layout()

    if plot:
        plt.show()
    else:
        # Save the graph as an image
        plt.savefig(graph_title, dpi=300)  # You can specify the file format (e.g., .png, .jpg, .pdf)

        # To avoid displaying the graph, you can optionally use plt.close()
        plt.close()