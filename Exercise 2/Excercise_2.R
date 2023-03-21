library(igraph)

# Create Nodes and Edges - This is a non-directional network
network <- graph(edges=c("A","B",
                         "A","C",
                         "A","2",
                         "B","D",
                         "B","3",
                         "B","C",
                         "B","6",
                         "C","D",
                         "C","3",
                         "C","4",
                         "D","5",
                         "D","3",
                         "D","6",
                         "1","2",
                         "3","4",
                         "3","5",
                         "5","6"), 
                 directed = FALSE)

# Calculate Degree Centrality for Each Node
degree_centrality <- degree(network, mode="all")

# Calculate Betweenness Centrality for each node
betweenness_centrality <- betweenness(network, directed = FALSE)

# Calculate closeness centrality for each node
closeness_centrality <- closeness(network, mode = "all")

# Calculate distances for Each Node
distances(network, mode="all")

# Calculate the Articulation Points to see who is holding the network together
articulation.points(network)


# Add centrality scores as node attributes
V(network)$degree_centrality <- degree_centrality
V(network)$closeness_centrality <- closeness_centrality
V(network)$betweenness_centrality <- betweenness_centrality

# Set the vertex label as the combination of node attributes
V(network)$label <- paste(
  "\nSEAT: ", V(network)$name, "\n",
  "Degree: ", V(network)$degree_centrality, "\n",
  "Closeness: ", round(V(network)$closeness_centrality, 2), "\n",
  "Betweenness: ", round(V(network)$betweenness_centrality, 2))

# Plot the Network Graph with node attributes as labels
plot(network, vertex.size=30, vertex.label.cex=0.7, 
     vertex.label.color="black", vertex.label.dist=1,
     vertex.label.family="sans", vertex.label.font=1)

