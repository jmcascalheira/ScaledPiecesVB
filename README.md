
<!-- README.md is generated from README.Rmd. Please edit that file -->
ScaledPiecesVB
==============

This repository contains the data and code for our paper:

> Horta, P., Cascalheira, J., Bicho, N. (2018). *The role of lithic bipolar technology in Western Iberia’s Upper Paleolithic: the case of Vale Boi (southern Portugal)*. Name of journal/book <https://doi.org/xxx/xxx>

Our pre-print is online here:

> Horta, P., Cascalheira, J., Bicho, N. (2018). *The role of lithic bipolar technology in Western Iberia’s Upper Paleolithic: the case of Vale Boi (southern Portugal)*. Name of journal/book, Accessed 05 May 2018. Online at <https://doi.org/xxx/xxx>

### How to cite

Please cite this compendium as:

> Cascalheira, J., Horta, P., Bicho, N. (2018). *Compendium of R code and data for 'The role of lithic bipolar technology in Western Iberia’s Upper Paleolithic: the case of Vale Boi (southern Portugal)'*. Accessed 05 May 2018. Online at <https://doi.org/xxx/xxx>

### How to download or install

You can download the compendium as a zip from from this URL:

<https://github.com/jmcascalheira/ScaledPiecesVB/archive/master.zip>

This repository is organized as an R package using rrtools by Ben Marwick which can be installed from github with:

There are no actual R functions in this package - all the R code is in the Rmd file. I simply used the R package structure to help manage dependencies, to take advantage of continuous integration for automated code testing, and so I didn't have to think too much about how to organise the files.

To download the package source as you see it on GitHub, for offline browsing, use this line at the shell prompt (assuming you have Git installed on your computer):

``` r

git clone https://github.com/jmcascalheira/ScaledPiecesVB.git
```

Once the download is complete, open the ShortTermOccupations.Rproj in RStudio to begin working with the package and compendium files.

The package has a number of dependencies on other R packages, and programs outside of R. Installing these can be time-consuming and complicated, so we've included a packrat directory, which contains the source code for all the packages we depend on. If all works well, these will be installed on your computer when you open ShortTermOccupations.Rproj in RStudio.

### Licenses

**Text and figures :** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please see our [contributor guidelines](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

### Dependencies:

See the colophon section of the docx file in `analysis/paper` for a full list of the packages that this project depends on.

### Contact:

João Cascalheira, Post-Doc Researcher, ICArEHB University of Algarve, Campus de Gambelas 8005-139 Faro PORTUGAL e. <jmcascalheira@ualg.pt> w. <http://www.icarehb.com/cascalheira>
