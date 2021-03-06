
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Last-changedate](https://img.shields.io/badge/last%20change-2018--12--02-brightgreen.svg)](https://github.com/jmcascalheira/ScaledPiecesVB/commits/master)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.2.4-brightgreen.svg)](https://cran.r-project.org/)
[![Build
Status](https://travis-ci.org/jmcascalheira/ScaledPiecesVB.svg?branch=master)](https://travis-ci.org/jmcascalheira/ScaledPiecesVB)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![ORCiD](https://img.shields.io/badge/ORCiD-/0000--0003--0321--8892-green.svg)](http://orcid.org/0000-0003-0321-8892)

# ScaledPiecesVB

This repository contains the data and code for our paper:

> Horta, P., Cascalheira, J., Bicho, N. (2018). *The role of lithic
> bipolar technology in Western Iberia’s Upper Paleolithic: the case of
> Vale Boi (southern Portugal)*. Journal of Paleolithic Archaeology
> <https://doi.org/xxx/xxx>

Our pre-print is online here:

> Horta, P., Cascalheira, J., Bicho, N. (2018). *The role of lithic
> bipolar technology in Western Iberia’s Upper Paleolithic: the case of
> Vale Boi (southern Portugal)*. SocArXiv , Accessed 02 Dec 2018. Online
> at <https://doi.org/xxx/xxx>

### How to cite

Please cite this compendium as:

> Cascalheira, J., Horta, P., Bicho, N. (2018). *Compendium of R code
> and data for ‘The role of lithic bipolar technology in Western
> Iberia’s Upper Paleolithic: the case of Vale Boi (southern
> Portugal)’*. Accessed 02 Dec 2018. Online at
> <https://doi.org/10.17605/OSF.IO/WPXGH>

### How to download or install

You can download the compendium as a zip from from this URL:

<https://github.com/jmcascalheira/ScaledPiecesVB/archive/master.zip>

This repository is organized as an R package using rrtools by [Ben
Marwick](https://github.com/benmarwick) which can be installed from
github with:

``` r

git clone https://github.com/benmarwick/rrtools.git
```

The functions are provided as a package because this makes it simpler to
reuse the functions many times in the paper. It also makes it easier for
others to use and adapt these functions on their own data. Nevertheless,
this package has been written explicitly for this project and may not
yet be suitable for more general purpose use.

To download the package source as you see it on GitHub, for offline
browsing, use this line at the shell prompt (assuming you have Git
installed on your computer):

``` r

git clone https://github.com/jmcascalheira/ScaledPiecesVB.git
```

Once the download is complete, open the ScaledPieces.Rproj in RStudio to
begin working with the package and compendium files.

If you want to re-run all the analyses in R, you can start by installing
the compendium package with this line at the R prompt:

``` r

devtools::install_github("jmcascalheira/ScaledPiecesVB")
```

The package has a number of dependencies on other R packages, and
programs outside of R. Installing these can be time-consuming and
complicated, so we’ve included a packrat directory, which contains the
source code for all the packages we depend on. If all works well, these
will be installed on your computer when you open ScaledPiecesVB.Rproj in
RStudio.

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.

### Dependencies:

See the colophon section of the docx file in `analysis/paper` for a full
list of the packages that this project depends on.

### Contact:

João Cascalheira, Post-Doc Researcher, ICArEHB University of Algarve,
Campus de Gambelas 8005-139 Faro PORTUGAL e. <jmcascalheira@ualg.pt> w.
<http://www.icarehb.com/cascalheira>
