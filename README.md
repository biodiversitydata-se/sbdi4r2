# sbdi4r2

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

### R functionality for the SBDI data portal

The Swedish Biodiversity Data Infrastructure (SBDI) provides tools to enable users of biodiversity information to find, access, combine and visualize data on Swedish plants and animals; available through [SBDI](https://biodiversitydata.se/). The R package sbdi4r2 provides a subset of the tools, and some extension tools (found previously in Analysportalen.se), to be directly used within R.

sbdi4r2 enables the R community to directly access data and resources hosted by SBDI. Our goal is to enable observations of species to be queried and output in a range of standard formats. This tool is built on the Atlas of Living Australia platform wrapping the `galah` package which provides similar services for the ALA.

Use-examples are available in the package [vignette here](https://biodiversitydata-se.github.io/sbdi4r2/articles/intro.html), or via (in R): `vignette("intro")`. If you have any questions please get in contact with us via the [support center](https://docs.biodiversitydata.se/support/).

## Installing sbdi4r2

The package is not yet available on CRAN therefore it should be installed from its GitHub repository.

In R:

Or the development version from GitHub:

```{r}
install.packages("remotes") 
remotes::install_github("biodiversitydata-se/sbdi4r2")
```

### Windows

If you see an error about "ERROR: lazy loading failed for package 'sbdi4r2'", this may be due to you trying to install on a network location. Try instead to install on a local location: first create the local location you want to use, and then specify this location for installing, and later loading the package:

```{r windows}
install_github("biodiversitydata-se/sbdi4r2", lib = "C:/pathname/MyLibrary")
library(sbdi4r2, lib.loc = "C:/pathname/MyLibrary")
```

### Mac

If you see an error about a failure to set default locale, you will need to manually set this:

```{r mac}
system('defaults write org.R-project.R force.LANG en_US.UTF-8')
```

and restart R.

More information can be found on the [CRAN R for Mac page](https://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html#Internationalization-of-the-R_002eapp).

### Linux

First, ensure that `libcurl` is installed on your system --- e.g. on Ubuntu, open a terminal and do:

`sudo apt-get install libcurl4-openssl-dev`

or install `libcurl4-openssl-dev` via the Software Centre.

Then, in R as shown above.

## Using sbdi4r2

The sbdi4r2 package must be loaded for each new R session with `library(sbdi4r2)`, or specifying your local location with `library(sbdi4r2, lib.loc = "C:/pathname/MyLibrary")`.

## Customizing sbdi4r2

Various aspects of the sbdi4r2 package can be customized.

### E-mail address

Each download request to SBDI servers is also accompanied by an "e-mail address" string that identifies the user making the request. You will need to provide an email address registered with the SBDI. You can create an account [here](https://auth.biodiversitydata.se/cas/login). Once an email is registered with the SBDI, it should be stored in the config:

```{r config1}
sbdi_config(email = "your.valid@emailaddress.com")
```

### Debugging

If things aren't working as expected, more detail (particularly about web requests and caching behaviour) can be obtained by setting the verbose configuration option:

```{r config2}
sbdi_config(verbose = TRUE)
```

### Setting the download reason

SBDI requires that you provide a reason when downloading occurrence data (via the sbdi4r2 `occurrences()` function). You can provide this as a parameter directly to each call of `occurrences()`, or you can set it once per session using:

```{r config3}
sbdi_config(download_reason_id = "your_reason_id")
```

(See `sbdi_reasons()` for valid download reasons, e.g. download_reason_id=10 for "testing", or 7 for "ecological research", 8 for "systematic research/taxonomy", 3 for "education")

### See examples on how to use the package in the next [vignette](https://biodiversitydata-se.github.io/sbdi4r2/articles/sbdi4r.html)
