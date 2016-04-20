# optimal-house
This repository contains the code for my paper [The Optimal Allocation of Campaign Funds
in House Elections](http://devinincerti.com/papers/optimal_house.pdf). The script main.R will run the
the code for the entire paper but, be forewarned, it will take over 15 hours because 4 separate 
Bayesian models must be implemented. The scripts containing the Bayesian models are `5-hlm.R`, 
`7-do_dlmnat`, `8b-do_dlmcd_run`, and `8c-do_dlmcd_nprun` (`7-do_dlmnat` is much faster than the others).
Scripts `1-load.R` and `2-clean.R` create the Rdata file output/ohdata.Rdata, which is used for the analysis, 
by loading/cleaning data from a MySQL database. 

