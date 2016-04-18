The **disparity filter algorithm** is a network reduction technique to identify the 'backbone' structure of a weighted network without destroying its multi-scale nature. The algorithm was developed by M. Angeles Serrano, Marian Boguna and Alessandro Vespignani in "[Extracting the multiscale backbone of complex weighted networks](http://arxiv.org/abs/0904.2389)", _Proceedings of the National Academy of Sciences_ **106** (16), 2009, [doi:10.1073/pnas.0808904106](http://dx.doi.org/10.1073/pnas.0808904106). This implementation of the algorithm supports both directed and undirected networks.

# Installation #

Install the `disparityfilter` package from CRAN:

```R
install.packages("disparityfilter")
```

Or install it from GitHub with the `devtools` package:

```R
devtools::install_github("alessandrobessi/disparityfilter")
```

# Usage #

The algorithm is implemented by the `backbone` function:

```R
library(disparityfilter)
?backbone
```
