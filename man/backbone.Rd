% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disparity_filter.R
\name{backbone}
\alias{backbone}
\alias{get.backbone}
\title{Extract the backbone of a weighted network using the disparity filter}
\usage{
backbone(graph, weights = E(graph)$weight, directed = is_directed(graph),
  alpha = 0.05)
}
\arguments{
\item{graph}{The input graph.}

\item{weights}{A numeric vector of edge weights, which defaults to
\code{E(graph)$weight}.}

\item{directed}{The directedness of the graph, which defaults to the result
of \code{\link[igraph]{is_directed}}.}

\item{alpha}{The significance level under which to preserve the edges, which
defaults to \code{0.05}.}
}
\value{
An edge list corresponding to the 'backbone' of the graph, i.e. the
edges of the initial graph that were preserved by the null model that the
disparity filter algorithm implements.
}
\description{
Given a weighted graph, \code{backbone} identifies the 'backbone structure'
of the graph, using the disparity filter algorithm by Serrano et al. (2009).
}
\examples{
# undirected network
g <- sample_pa(n = 250, m = 5, directed = FALSE)
E(g)$weight <- sample(1:25, ecount(g), replace = TRUE)
backbone(g)

# directed network
g <- sample_pa(n = 250, m = 5, directed = TRUE)
E(g)$weight <- sample(1:25, ecount(g), replace = TRUE)
backbone(g)
}
\author{
Serrano et al. (2009); R implementation by Alessandro Bessi and
Francois Briatte
}
\references{
Serrano, M.A., Boguna, M. and Vespignani, A. (2009).
Extracting the multiscale backbone of complex weighted networks.
\emph{Proceedings of the National Academy of Sciences} 106, 6483-6488.
}

