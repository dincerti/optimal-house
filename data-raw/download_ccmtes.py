# -*- coding: utf-8 -*-
"""
Created on Wed Jan 28 14:06:46 2015

@author: Devin
"""

import os, urllib, zipfile, xlrd, csv

# functions
def myunzip(folder, name):
  fh = open(folder, 'rb')
  z = zipfile.ZipFile(fh)
  z.extract(name)
  fh.close()
  
def remove_quotes(s):
    return ''.join(c for c in s if c not in ('"'))  
  
def comcsv(file):
  wb = xlrd.open_workbook(file)
  sh = wb.sheet_by_index(0)
  with open(os.path.splitext(file)[0] + '.csv', 'wb') as f:
    c = csv.writer(f)
    for r in range(1, sh.nrows):
      row = sh.row_values(r)
      row[3] = remove_quotes(row[3])
      if r == 3094 and file == 'house_assignments.xls': 
        row[13] = row[13].replace('?', '')
      c.writerow(row)
  os.remove(file)

# download data
curl = 'http://web.mit.edu/cstewart/www/data/'
urllib.urlretrieve(curl + 'house_assignments_103-112-1.xls', 'house_assignments.xls')
urllib.urlretrieve(curl + 'house_members_103-112-1.xls', 'house_members.xls')   
comcsv('house_assignments.xls')
comcsv('house_members.xls')
os.chdir('..')
urllib.urlretrieve(curl + 'codebook.txt', 'committee_codebook.txt')  

