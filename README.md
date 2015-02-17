# Disparity Filter Algorithm of Weighted Network

### Description
Disparity filter is a network reduction algorithm to extract the backbone structure of both directed and undirected weighted networks. Disparity filter can reduce the network without destroying the multi-scale nature of the network. The algorithm has been developed by M. Angeles Serrano, Marian Boguna, and Alessandro Vespignani in *"Extracting the multiscale backbone of complex weighted networks"*, **Proceedings of the national academy of sciences 106 (16): 6483–6488** [doi:10.1073/pnas.0808904106]


### Usage
```
get.backbone(graph = G, alpha = 0.05, directed = FALSE)
```

### Arguments
* `graph`	iGraph graph object.
* `alpha`	Statistical significance level. By default is set to 0.05.
* `directed`	Logical, whether the network is directed or undirected. By default is set to FALSE.

### How to quickly install the `disparityfilter` package in R
```
install.packages("devtools")
library(devtools)
devtools::install_github('disparityfilter','alessandrobessi')
library(disparityfilter)
?get.backbone
```
