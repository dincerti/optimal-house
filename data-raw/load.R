
# CONNECT TO MYSQL -------------------------------------------------------------
cfile <-"C:/Program Files/MySQL/MySQL Server 5.5/my.ini"
con <- dbConnect(MySQL(), group = "client", dbname = "hr", default.file = cfile)
con2 <- dbConnect(MySQL(), group = "client", dbname= "pres", default.file = cfile)
con3 <- dbConnect(MySQL(), group = "client", dbname= "crp", default.file = cfile)
con4 <- dbConnect(MySQL(), group = "client", dbname= "macroecon", default.file = cfile)

# LOAD DATA --------------------------------------------------------------------
# presidential election data
presel <- fread("data/cq_presel.csv")[, .(year, st_usps, st_presdv)]

# dw-nominate
dwnom <- data.table(dbGetQuery(con, 'SELECT stcd, year, idno, dwnom1 FROM dwnom'))
dwnom.pres <- fread("data/dwnom_pres.csv")[, .(year, dwnom_dem, dwnom_rep)]

# gary jacobson data
hr <- data.table(dbReadTable(con, "hr4612"))

# incumbent id
incid <- fread("data/incid.csv")

# generic ballot
gb <- data.table(dbReadTable(con, "generic_ballot"))
ropergb <- fread("data/ropergb.csv")

# election year file
elyear <- data.table(dbReadTable(con, "elecyear"))

# presidential approval
approval <- data.table(dbReadTable(con2, "approval"))

# gdp data
gdp <- data.table(dbGetQuery(con4, 'SELECT CONVERT(substring(date, 1, 4), UNSIGNED INTEGER) as year,
                             substring(date, 5) as quarter, gdp_chained as gdp FROM gdpq'))

# bls cpi data
cpi <- fread("data/bls_cpi_urban.csv")

# congressional committee data
ccmtes <- data.table(dbGetQuery(con, 'SELECT * FROM ccmtes'))

# district polls
polls <- data.table(dbReadTable(con, "polls"))

# crp data
pacs <- data.table(dbGetQuery(con3, 'SELECT * FROM pacs WHERE Year(Date) % 2 = 0 
                              AND RealCode NOT LIKE "Z9%"'))
cands <- data.table(dbGetQuery(con3, 'SELECT * FROM cands WHERE DistIDRunFor != "PRES" and
                               SUBSTRING(DistIDRunFor, 3, 2) != "S1" and
                               SUBSTRING(DistIDRunFor, 3, 2) != "S2"'))
indivs <- data.table(dbGetQuery(con3, 'SELECT * FROM indivs WHERE MONTH(Date) >= 9 and Year(Date) % 2 = 0
                                and SUBSTRING(RecipID,1,1) = "N" 
                                and RecipID LIKE "N%"
                                and RealCode NOT LIKE "Z9%"'))
setnames(indivs, c("RecipID"), c("CID"))
industry <- dbReadTable(con3, "industry")
setnames(industry, c("Catcode"), c("RealCode"))

