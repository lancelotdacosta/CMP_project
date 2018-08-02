# The fastcluster package is not necessary.  By loading the
# fastcluster package, the fastcluster::hclust() function 
# automatically replaces the slower stats::hclust() function
# whenever hclust() is called.


m1 <- mapper1D(
  distance_matrix = dist(data.frame( x=2*cos(0.5*(1:100)), y=sin(1:100) )),
  filter_values = 2*cos(0.5*(1:100)),
  num_intervals = 10,
  percent_overlap = 50,
  num_bins_when_clustering = 10)

# The igraph package is necessary to view simplicial complexes #####Important
# (undirected graph) resulting from mapper1D().

g1 <- graph.adjacency(m1$adjacency, mode="undirected")
plot(g1, layout = layout.auto(g1) )

#######################Example 3

# sample points from two intertwined spirals
set.seed("1")
t <- runif(100, min=1, max=6.3) # theta
X <- data.frame( x = c( t*cos(t), -t*cos(t) ), y = c( t*sin(t), -t*sin(t) ) )
d <- dist(X)
plot(X[,1], X[,2])

filter <- X[,2] # height projection
num_intervals <- 10
percent_overlap <- 50
num_bins_when_clustering <- 10

m3 <- mapper1D(
  distance_matrix = d, 
  filter_values = filter 
  # num_intervals = 10, # use default
  # percent_overlap = 50, # use default
  # num_bins_when_clustering = 10 # use default
)

g3 <- graph.adjacency(m3$adjacency, mode="undirected")
plot(g3, layout = layout.auto(g3) )
