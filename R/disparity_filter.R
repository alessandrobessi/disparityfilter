# DISPARITY FILTER
#' Disparity Filter Algorithm of Weighted Network
#'
#' Disparity filter is a network reduction algorithm to extract the backbone structure of both directed and undirected weighted networks. Disparity filter can reduce the network without destroying the multi-scale nature of the network. The algorithm has been developed by M. Angeles Serrano, Marian Boguna, and Alessandro Vespignani in "Extracting the multiscale backbone of complex weighted networks", Proceedings of the national academy of sciences 106 (16): 6483-6488 [doi:10.1073/pnas.0808904106]
#' @param graph igraph graph object. The original graph.
#' @param alpha Statistical significance level. By default is set to 0.05.
#' @param directed Logical, whether the network is directed or undirected. By default is set to FALSE.
#' @keywords backbone extraction disparity filter
#' @export
#' @examples
#' get.backbone(graph = G, alpha = 0.05, directed = FALSE)
#'
#'
get.backbone = function(graph, alpha = 0.05, directed = FALSE)
{
  require(igraph)
  G = graph
  adj = as.matrix(get.adjacency(G, attr = "weight"))
  N = as.numeric(dim(adj)[1])
  backbone = matrix(0, ncol = N, nrow = N)
  colnames(backbone) = colnames(adj)
  rownames(backbone) = rownames(adj)

  cat("Disparity Filter\n")
  cat("alpha =", alpha, "\n")
  cat("\nOriginal graph\n")
  print(G)

  if (directed == FALSE)
  {
  for (i in 1:N)
  {
      k = length(which(adj[i,] > 0))
      if (k > 1)
      {
        for (j in 1:N)
        {
          pij = adj[i,j] / sum(adj[i,])
          if ( (1 - pij)^(k-1) < alpha )
          {
          backbone[i,j] = adj[i,j]
          }
          else {backbone[i,j] = 0}
        }
      }
    }
  index = which(rowSums(backbone) == 0)
  backbone = backbone[-index,-index]

  G_backbone = graph.adjacency(backbone, weighted = TRUE,
                               mode = "undirected")
  }

  # directed
  if (directed == TRUE)
  {
    for (i in 1:N)
    {
      k = length(which(adj[i,] > 0))
      if (k > 1)
      {
        for (j in 1:N)
        {
          pij = adj[i,j] / sum(adj[i,])
          if ( (1 - pij)^(k-1) < alpha )
          {
            backbone[i,j] = adj[i,j]
          }
          else {backbone[i,j] = 0}
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
