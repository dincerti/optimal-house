# -*- coding: utf-8 -*-
"""
Created on Wed Jan 28 14:06:46 2015

@author: Devin
"""

import urllib, xlrd, csv, shutil
import pandas as pd


# download quarterly bls gdp and convert excel file to csv
urllib.urlretrieve('http://www.bea.gov/national/xls/gdpchg.xls', 'gdpchg.xls')
wb = xlrd.open_workbook('gdpchg.xls')
sh = wb.sheet_by_index(0)
with open('gdpchg.csv', 'wb') as f:
  c = csv.writer(f)
  c.writerow(['date', 'gdp_current', 'gdp_chained'])
  for r in range(9, sh.nrows):
    c.writerow(sh.row_values(r)[4:7])

