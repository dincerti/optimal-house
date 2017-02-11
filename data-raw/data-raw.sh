# files/directories needed: states.csv, jacobson.zip
Rscript download_rcpgb.R
Rscript download_nytimes.R
python download_voteview.py
Rscript clean_dwnom.R
Rscript clean_hr4612.R
# Rscript download_presaprv.R  # URL doesn't work anymore
python download_gdp.py
Rscript save-ohdata.R