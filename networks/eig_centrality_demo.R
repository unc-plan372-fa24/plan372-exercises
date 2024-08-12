# Compute eigenvector centrality manually, to answer the question of why one centrality is exactly 1

# load libraries
library(tidyverse)
library(igraph)

# load data and create graph
data = read_csv("networks/sponsors.csv")

sponsors = mutate(data,
                  sponsor1=if_else(sponsor < cosponsor, sponsor, cosponsor),
                  sponsor2=if_else(sponsor < cosponsor, cosponsor, sponsor)
) |> select(-c(sponsor, cosponsor))

# now we just need to create a version of the data with a single entry for each
# sponsor pair
sponsor_pairs = group_by(sponsors, sponsor1, sponsor2) |> summarize()

# Finally, we can create the graph
graph = graph_from_data_frame(sponsor_pairs, directed=F)

# compute eigenvector centrality using igraph
igraph_eig = eigen_centrality(graph)

# compute eigenvector centrality manually
# first, extract the adjacency matrix
adjmat = as_adjacency_matrix(graph)

# now, compute eigenvectors. There are many eigenvectors, but the one with the largest
# eigenvalue is the only one which will have all components with the same sign.
# https://en.wikipedia.org/wiki/Perron%E2%80%93Frobenius_theorem
manual_eig = eigen(adjmat)

# extract the eigenvector with the largest eigenvalue, the first column of the eigenvector matrix
manual_eig_vec = manual_eig$vectors[,1]

manual_eig_vec

# These are all negative. However, the definition of an eigenvector for an nxn matrix
# is a vector (in the geometric sense) in n-dimensional space that changes only by a constant
# when linearly transformed by the transformation defined by treating the adjacency matrix as
# a transformation matrix. Scaling the length of that vector does not change the transformation
# it undergoes. So eigenvectors can be normalized in any arbitrary way. igraph normalizes the
# eigenvectors to have the largest value be 1, whereas R normalizes them to have length 1. We can
# divide our manual eigenvector by the igraph eigenvector and see that it's a constant multiple.
# from the R eigen documentation:
# The vectors are normalized to unit length.

# Recall that the eigenvectors are only defined up to a constant: even when the length is
# specified they are still only defined up to a scalar of modulus one (the sign for real matrices).
manual_eig_vec / igraph_eig$vector
