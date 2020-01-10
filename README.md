## Overview
This Shiny app was built for the stochastic Collision Risk Model (CRM) developed by Masden (2015a,b) – itself based on the deterministic models of Band (2012).

Masden’s program was presented as R code, which users would interact with via standard R command-line. This code has been ported to Shiny to offer a user-friendly GUI accessible via standard web-browsers. 

The core model calculations are largely the same as for Masden’s code, so associated documentation applies here. The core deterministic calculations underpinning the Masden code (i.e. without stochasticity) follow that of Band (2012), so the associated documentation there is also relevant. 

Some core alterations and additions have been made for the GUI, in particular to address deficiencies identified in the review by Trinder (2017). In addition, there are correction of some bugs and general improvements in structure, speed and efficiency.

The user manual is available to download [here](https://github.com/dmpstats/stochCRM/blob/master/Stochastic%20CRM%20Shiny%20Users%20Manual%20V1.0_submitted.pdf).


## How to run

Users have the option to use an online version of the app or download a copy of the code and run it locally. 

### Run Online

Using the [online version](https://dmpstats.shinyapps.io/avian_stochcrm/) is recommended, as the advantages are manifold:
* Users are not required to use any R code.
* Users are not required to install or maintain R – their web-browser will communicate with remote servers that carry the necessary software and conduct the calculations.
* Updates are made directly to the server, so are immediately propagated to users.


### Run Locally

To run the app locally, the correct package versions for the app are managed through an R-Studio R-project file, so must run via R-studio. Follow the next steps to set up and run sCRM locally:
  1. Download and install the latest version of [R](https://cran.r-project.org/) and [R-Studio](https://rstudio.com/).
  1. To enable R package building from source (required for packrat), go to [Package Development Prerequisites](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) and install the tools needed for your operating system.
  1. Download the ZIP containing the app files (green button "Clone or download") and unzip it to a convenient location.
  1. Open the R project in R-Studio, either by double-clicking the `*.Rproj` file (named along the lines of: `shiny-stochCRM vXXX.Rproj`), or __File -> Open Project__ and locating the same. This will ensure that the correct versions of the libraries are used, rather than versions you may have installed (via `packrat`). __It is important that the R-Studio project file be used for this reason.__
  1. Run `packrat::restore()`. Wait till the function finishes its job (i.e. install the required packages).
  1. Restart the R session (e.g. press _Ctrl+Shift+F10_).
  1. Open the `global.r` file.
  1. Launch the app - say via the __Run App__ button at the top-right of the code window. 


----------------

#### References
Band, B. (2012) Using a collision risk model to assess bird collision risks for offshore windfarms. SOSS report, The Crown Estate.

Masden, E (2015a) Developing an avian collision risk model to incorporate variability and uncertainty. Scottish Marine and Freshwater Science Report Vol 6 No 14. Marine Scotland Science. ISSN: 2043-7722 DOI: 10.7489/1659-1 

Masden, E. (2015b) Developing an avian collision risk model to incorporate variability and uncertainty. [R computer code]

Trinder, M (2017) Incorporating Uncertainty in Collision Risk Models: a test of Masden. Final Draft dated 28/04/2017. McArthur Green.


---------------------
#### Note - Band comparisons

There is facility for comparing outputs from the Band spreadsheet (2012). If working from a local copy, the server.r file can be modified at __step 5: run simulation function__ to set parameters equivalent to those in the pre-prepared spreadsheet (`band_comparison_inputs/Band_Kittiwake_comparison.xlsm`) and suppress stochasticity. In effect, all standard deviations are made zero (or very small) and bootstrap FHD are set to be the same for each draw. All other parameters and input data are set to be the same - although rounding differs between the platforms.

Toggling the `if(1)` and `if(0)` will switch between these modes. Turning this comparison on will make sCRM iterate only twice for Kittiwakes and produce comparison tables for the option 1 to option 3 outputs. Option 1 and 2 (at 16/12/2019) are within 0.05% of the Band estimates and option 3 is within approximately 0.4%.

