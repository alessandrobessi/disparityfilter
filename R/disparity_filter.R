# DISPARITY FILTER
#' Disparity Filter Algorithm of Weighted Network
#'
#' Disparity filter is a network reduction algorithm to extract the backbone structure of both directed and undirected weighted networks. Disparity filter can reduce the network without destroying the multi-scale nature of the network. The algorithm has been developed by M. Angeles Serrano, Marian Boguna, and Alessandro Vespignani in "Extracting the multiscale backbone of complex weighted networks", Proceedings of the national academy of sciences 106 (16): 6483-6488 [doi:10.1073/pnas.0808904106]
#' @param graph igraph graph object. The original weighted graph.
#' @param alpha Statistical significance level. By default is set to 0.05.
#' @param directed Logical, whether the network is directed or undirected. By default is set to FALSE.
#' @keywords backbone extraction disparity filter
#' @export get.backbone
#' @examples
#' G_backbone = get.backbone(graph = G, alpha = 0.05, directed = FALSE)
#' summary(G_backbone)

get.backbone = function(graph, alpha = 0.05, directed = FALSE)
{
  # load igraph package
  if(!require(igraph))
  {
    install.packages("igraph")
    require(igraph)
  }

  G = graph
  # get adjacency matrix
  adj = as.matrix(get.adjacency(G, attr = "weight"))
  N = as.numeric(dim(adj)[1])

  # initialize backbone adjacency matrix
  backbone = matrix(0, ncol = N, nrow = N)
  colnames(backbone) = colnames(adj)
  rownames(backbone) = rownames(adj)

  cat("Disparity Filter\n")
  cat("alpha =", alpha, "\n")
  cat("\nOriginal graph\n")
  print(G)

  # undirected
  if (directed == FALSE)
  {
    for (i in 1:N)
    {
      k_i = length(which(adj[i,] > 0))
      if (k_i > 1)
      {
        integrand_i = function(x){(1-x)^(k_i-2)}

        for (j in 1:N)
        {
          p_ij = adj[i,j] / sum(adj[i,])
          integration = integrate(integrand_i, lower = 0, upper = p_ij)
          alpha_ij = 1 - (k_i - 1) * integration$value
          #alpha_ij = (1 - p_ij)^(k_i - 1)

          if ( alpha_ij < alpha )
          {
            backbone[i,j] = adj[i,j]
          }
        }
      }
    }
    index = which(rowSums(backbone) == 0)
    backbone = backbone[-index,-index]

    G_backbone = graph.adjacency(backbone, weighted = TRUE, mode = "undirected")
    G_backbone
  }

  # directed
  if (directed == TRUE)
  {
    for (i in 1:N)
    {
      k_i = length(which(adj[i,] > 0))
      if (k_i > 1)
      {
        integrand_i = function(x){(1-x)^(k_i-2)}

        for (j in 1:N)
        {
          p_ij = adj[i,j] / sum(adj[i,])
          integration = integrate(integrand_i, lower = 0, upper = p_ij)
          alpha_ij = 1 - (k_i - 1) * integration$value
          #alpha_ij = (1 - p_ij)^(k_i - 1)

          if ( alpha_ij < alpha )
          {
            backbone[i,j] = adj[i,j]
          }
        }
      }
    }
    index = which(rowSums(backbone) == 0)
    backbone = backbone[-index,-index]

    G_backbone = graph.adjacency(backbone, weighted = TRUE,
                                 mode = "directed")
  }

  cat("\nBackbone graph\n")
  print(G_backbone)
  return(G_backbone)
}
