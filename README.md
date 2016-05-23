# optimal-house
This repository contains the code for my paper [The Optimal Allocation of Campaign Funds
in House Elections](http://devinincerti.com/papers/optimal_house.pdf). To run the analysis for the entire paper,
first clone the directory,

```
git clone https://github.com/dincerti/optimal-house.git

```
Then, from the command line (terminal) run the script `main.R`.

````
Rscript main.R
````

Alternatively, download the repository as a zip folder to your local machine and run the script `main.R` from within R.
Make sure that all of the required packages have been installed. The script takes around 17 hours to run on 
my local machine. The code is slow because there are 20 separate Bayesian models that need to be simulated.
The scripts containing these models are `5-hlm.R`, `7-do_dlmnat`, `8b-do_dlmcd_run`, and 
`8c-do_dlmcd_nprun` (`7-do_dlmnat` is much faster than the others). Your machine must have at least four cores because 
the Bayesian models in `8b` and `8c` are run in parallel.

Scripts `1-load.R` and `2-clean.R` (sourced within `main.R') create the Rdata file `output/ohdata.Rdata`, which is used for the analysis, 
by loading/cleaning data from a MySQL database. 

