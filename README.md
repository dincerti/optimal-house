# optimal-house
This repository contains the code for my paper [The Optimal Allocation of Campaign Funds
in House Elections](http://devinincerti.com/papers/optimal_house.pdf). To run the analysis for the entire paper, download
the repository as a zip file or clone the repository to your local machine. Then run the script `main.R` 
and be sure to install the required packages and change the working directory. However, be forewarned
that it will take over 15 hours because four separate Bayesian models must be implemented. 

The scripts containing the Bayesian models are `5-hlm.R`, `7-do_dlmnat`, `8b-do_dlmcd_run`, and 
`8c-do_dlmcd_nprun` (`7-do_dlmnat` is much faster than the others).
Scripts `1-load.R` and `2-clean.R` create the Rdata file `output/ohdata.Rdata`, which is used for the analysis, 
by loading/cleaning data from a MySQL database. 

