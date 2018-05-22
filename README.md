## Overview
This Shiny app was built for the stochastic Collision Risk Model (CRM) developed by Masden (2015a,b) – itself based on the deterministic models of Band (2012).

Masden’s program was presented as R code, which users would interact with via standard R command-line. This code has been ported to Shiny to offer a user-friendly GUI accessible via standard web-browsers. The advantages are manifold:
  - Users are not required to use any R code.
   - Users are not required to install or maintain R – their web-browser will communicate with remote servers that carry the necessary software and conduct the calculations.
  - Updates are made directly to the server, so are immediately propagated to users.
  
The core calculations are largely the same as for Masden’s code, so associated documentation applies here. The core deterministic calculations underpinning the Masden code (i.e. without stochasticity) follow that of Band (2012), so the associated documentation there is also relevant. 

Some core alterations and additions have been made for the GUI, in particular to address deficiencies identified in the review by Trinder (2017). In addition, there are correction of some bugs and general improvements in structure, speed and efficiency.

The app is available [here](https://dmpstats.shinyapps.io/avian_stochcrm/).

The user manual is available to download [here](https://github.com/dmpstats/stochCRM/blob/master/Stochastic%20CRM%20Shiny%20Users%20Manual%20V1.0_submitted.pdf).


#### References
Band, B. (2012) Using a collision risk model to assess bird collision risks for offshore windfarms. SOSS report, The Crown Estate.

Masden, E (2015a) Developing an avian collision risk model to incorporate variability and uncertainty. Scottish Marine and Freshwater Science Report Vol 6 No 14. Marine Scotland Science. ISSN: 2043-7722 DOI: 10.7489/1659-1 

Masden, E. (2015b) Developing an avian collision risk model to incorporate variability and uncertainty. [R computer code]

Trinder, M (2017) Incorporating Uncertainty in Collision Risk Models: a test of Masden. Final Draft dated 28/04/2017. McArthur Green.
