[![DOI](https://zenodo.org/badge/541789294.svg)](https://zenodo.org/badge/latestdoi/541789294)

# cropSpreadR: Exploring Spread Processes and Rates of Maize Agriculture

`cropSpreadR` is an *R* package implementing functions to perform
analysis on the spread processes and movement rates of maize agriculture.

This is the official R package for [cropSpreadR](https://github.com/Archaeo-Programmer/cropSpreadR), 
which contains all code associated with the analyses described and presented, including figures and tables, in Gillreath-Brown et al. 2022 (submitted): 

Gillreath-Brown, A., D. Bird, and T. A. Kohler (2022). Rate Variability on the Maize Northern Frontier in the Prehispanic Southwestern United States: Implications for Earliest Maize. Submitted to *Journal of Archaeological Science: Reports* for review.
    
All code for analysis, figures, and tables is in [Maize_Spread_Processes.Rmd](vignettes/Maize_Spread_Processes.Rmd).

## Installation

You can install `cropSpreadR` from GitHub with these lines of R code (Windows users are recommended to install a separate program, Rtools, before proceeding with this step):

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Archaeo-Programmer/cropSpreadR")
```

## Repository Contents

The [:file\_folder: vignettes](vignettes) directory contains:

  - [:page\_facing\_up: Maize_Spread_Processes](vignettes/Maize_Spread_Processes.Rmd): R
    Markdown document with all analysis and code to reproduce the figures and tables for the submitted paper (Gillreath-Brown et al. 2022).
  - [:file\_folder: figures](vignettes/figures): Plots, figures, and illustrations in the paper, including supplementary materials.
  - [:file\_folder: tables](vignettes/tables): Tables in the paper, including supplementary materials.
  
## How to Run the Code?

To reproduce the analysis, output, figures, and tables, you will need to clone the repository. To clone the repository, you can do the following from your Terminal:

```bash
git clone https://github.com/Archaeo-Programmer/cropSpreadR.git
cd cropSpreadR
```

After installing the `cropSpreadR` package (via `install_github` as shown above or by using `devtools::install()`), then you can render the analysis, visualizations, and tables.
You can compile the `cropSpreadR` analysis within R by entering the following in the console:

``` r
rmarkdown::render(here::here('vignettes/Maize_Analysis.Rmd'), output_dir = here::here('vignettes'))
```

Another option for reproducing the results is to use the package itself and follow along with the vignette, [cropSpreadR](vignettes/cropSpreadR.Rmd). Data and functions are already loaded into the package. 

## Licenses

**Code:** [GNU GPLv3](LICENSE.md)

**Data:** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

## Acknowledgements

This material is based upon work supported by the National Science Foundation under Grants [SMA-1637171](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1637171) 
and [SMA-1620462](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1620462), and by the Office of the Chancellor, [Washington State University-Pullman](https://wsu.edu/).

