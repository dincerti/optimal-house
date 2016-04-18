# (1) NATIONWIDE VARIABLES  ---------------------------------------------------
# election year
elyear[, eldate := as.Date(eldate, format = "%m/%d/%Y")]
elyear[, presyear := 1 * (year %% 4 == 0)]

# august generic ballot by year
ropergb <- ropergb[, .(gbdv_aug = mean(dv)), by = year]

# quarterly gdp by year
gdp <- reshape(gdp, timevar = "quarter", idvar = "year", direction = "wide")

# august presidential approval by year
approval[, net_approve := approve - disapprove]
approval <- approval[month == 8]
approval <- approval[, .(net_approve_aug = mean(net_approve, na.rm = TRUE)), by =  year]

# presidential dw-nominate scores
dwnom.pres[, dwnom_dem := ifelse(is.na(dwnom_dem), # impute score for michael dukakis
                                       mean(unique(dwnom.pres$dwnom_dem), na.rm = TRUE) , 
                                       dwnom_dem)]

# merge data and create new variables
nat <- merge(elyear, ropergb, by = "year", all.x = TRUE)
nat <- merge(nat, gdp, by = "year")
nat <- merge(nat, approval, by = "year")
nat <- merge(nat, dwnom.pres, by = "year", all.x = TRUE)

# (2) DISTRICT VARIABLES  -----------------------------------------------------
# dw-nominate score
setnames(dwnom, c("idno"), c("dwnomid"))
dwnom.mean <- dwnom[, .(dwnom1.mean = mean(dwnom1)), by = c("stcd", "year")]

# congressional committees
ccmtes[, ':=' (value = 1, ccode = paste0("cc", ccode))]
ccmtes[, chair := ifelse(leadership %in% c(11, 12, 13, 14, 16), 1, 0)]
ccmtes[, finchair := ifelse(leadership %in% c(11, 12, 13, 14, 16, 21, 22, 23, 24) & ccode == "cc113", 1, 0)]
ccmtes.id <- ccmtesID(ccmtes) # dataframe by congressman and congressional session
setnames(ccmtes.id, c("id"), c("ccmtesid"))
NAToZero(ccmtes.id, names(ccmtes.id)[-c(1:2)])

## merging
hr <- merge(hr, incid[, .(year, stcd, dwnomid, ccmtesid, inc_name)], 
            by = c("year", "stcd"), all.x = TRUE)
hr <- merge(hr, dwnom, by = c("year", "stcd", "dwnomid"), all.x = TRUE)
setnames(hr, "dwnom1", "dwnom1_inc")
hr <- merge(hr, dwnom.mean, by = c("year", "stcd"), all.x = TRUE)
hr <- merge(hr, ccmtes.id, by = c("year", "ccmtesid"), all.x = TRUE)
#hr[cong >= 103 &  year!=2002 & year!=2012 & i_inc == 1 & is.na(numcc)] # incumbents not matched
NAToZero(hr, names(ccmtes.id)[-c(1:2)])

# (3) CAMPAIGN CONTRIBUTIONS --------------------------------------------------
# unique candidate id's by cycle
ucands <- cands[, .N, by = c("CID", "Cycle", "DistIDRunFor", "Party", "CRPICO")]
ucands <- ucands[, .N, by = c("CID", "Cycle", "DistIDRunFor", "Party", "CRPICO")]
setnames(ucands, c("DistIDRunFor"), c("stcd"))
ucands[, N := NULL]

# create variables
pacs <- CRPVars(pacs)
indivs <- CRPVars(indivs)

# party
pacs[Party == "D" | Party == "R", party := Party]
pacs[(Party == "D" | Party == "R") & DI == "I" & (Type == "24A" | Type == "24N"), 
     party := ifelse(Party == "D", "R", "D")]

# party spending
tmp <- pacs[, .(rAmount = sum(rAmount)), by = c("year", "PACID", "party")]
tmp <- dcast.data.table(tmp, PACID + year ~ party, sum, na.rm = TRUE, value.var = "rAmount")
tmp$tot <- rowSums(tmp[, -c(1:2), with = FALSE])
tmp[, ':=' (fdem = D/tot, frep = R /tot)]
pacs <- merge(pacs, tmp[, .(year, PACID, fdem, frep)], by = c("year", "PACID"))

# classify pacs/party orgs
pacs[, pactype := "other"]
pacs[frep > 0.9, pactype := "repally"]
pacs[fdem > 0.9, pactype := "demally"]
pacs[PACID %in% c("C00075820", "C00003418"), pactype := "reporg"]
pacs[PACID %in% c("C00000935", "C00010603"), pactype := "demorg"]
pacs[RealCode %in% c("Z1100", "J2200", "J2400") | (RealCode == "Z5100" & pactype != "reporg") , pactype := "repcon"]
pacs[RealCode %in% c("Z1200", "J2100", "J2300") | (RealCode == "Z5200" & pactype != "demorg") , pactype := "demcon"]

# post august expenditures by pac party type
exp.dem <- PartyExp("D")
exp.rep <- PartyExp("R")
exp.party <- merge(exp.dem, exp.rep, by = c("year", "stcd"), all.x = TRUE)
exp.party[, ':=' (expD_party = expD_demorg + expD_demcon + expD_demally,
                  expR_party = expR_reporg + expR_repcon + expR_repally)]
exp.party[, ':=' (exp_party = expD_party + expR_party,
                  exp_natorg = expD_demorg + expR_reporg,
                  exp_con = expD_demcon + expR_repcon,
                  exp_ally = expD_demally + expR_repally,
                  exp_other = expD_other + expR_other)]

# other post august expenditures
exp <- list()
exp[[1]] <- PostAugExp(pacs, "exp_pacs")
exp[[2]] <- PostAugExp(indivs[Party == "D"], "exp_indivs_dem")
exp[[3]] <- PostAugExp(indivs[Party == "R"], "exp_indivs_rep")
exp[[4]] <- PostAugExp(pacs[PACID == "C00000935"], "exp_dccc")
exp[[5]] <- PostAugExp(pacs[PACID == "C00075820"], "exp_nrcc")
exp[[6]] <- PostAugExp(pacs[party == "D"], "exp_pacs_dem")
exp[[7]] <- PostAugExp(pacs[party == "R"], "exp_pacs_rep")
exp[[8]] <- PostAugExp(pacs[Sector == "Finance/Insur/RealEst" & CRPICO == "I"], "exp_pacs_fin_inc")
exp[[9]] <- PostAugExp(indivs[Sector == "Finance/Insur/RealEst" & CRPICO == "I"], "exp_indivs_fin_inc")
exp[[10]] <- PostAugExp(pacs[Sector == "Finance/Insur/RealEst" & party == "D"], "exp_pacs_fin_dem")
exp[[11]] <- PostAugExp(pacs[Sector == "Finance/Insur/RealEst" & party == "R"], "exp_pacs_fin_rep")
exp <- Reduce(function (x, y) merge(x, y, by = c("year", "stcd"), all = TRUE), exp)
exp[, ':=' (exp_fin_inc = exp_pacs_fin_inc + exp_indivs_fin_inc,
            exp_indivs = exp_indivs_dem + exp_indivs_rep,
            exp_dem = exp_pacs_dem + exp_indivs_dem,
            exp_rep = exp_pacs_rep + exp_indivs_rep)]
exp[, exp := exp_pacs + exp_indivs]

# CONGRESSIONAL DISTRICT YEARLY DATASET ---------------------------------------
cd <- merge(hr, nat, by = c("year"))
cd <- merge(cd, presel, by = c("year", "st_usps"), all.x = TRUE)
cd <- merge(cd, exp, by = c("year", "stcd"), all.x = TRUE)
cd <- merge(cd, exp.party, by = c("year", "stcd"), all.x = TRUE)
for (j in names(exp.party)[-c(1:2)]){
  set(cd, which(is.na(cd[[j]]) & cd[["year"]] >= 2000 & cd[["year"]] <= 2010), j, 0)
}
for (j in names(exp)[-c(1:2)]){
  set(cd, which(is.na(cd[[j]]) & cd[["year"]] >= 2000 & cd[["year"]] <= 2010), j, 0)
}

cd[, ':=' (int = 1, prescontrolXq2gdp = prescontrol * gdp.q2,
              prescontrolXaugapr = prescontrol * net_approve_aug/100)]
cd[, c_l_presdv := l_presdv - mean(l_presdv), by = year]
cd[, dwnom1 := ifelse(i_inc == 1, dwnom1_inc, dwnom1.mean)]
cd[, cdideo := l_presdv * dwnom_dem + (1 - l_presdv) * dwnom_rep]
cd[, rdwnom1 := dwnom1 - cdideo]
cd[, absdwnom1Xhouinc := abs(dwnom1) * houinc]
cd[, absrdwnom1Xhouinc := abs(rdwnom1) * houinc]
cd[, dwnom1Xi_open := dwnom1 * (1 - i_inc)]
cd[, rdwnom1Xi_open := rdwnom1 * (1 - i_inc)]

# POLL DATASETS ---------------------------------------------------------------
# combine generic ballot and district polls for editing
gb[, stcd := "NAT"]
polls <- data.table(rbind_all(list(polls, gb)))
polls[, year := 2010] # this is the election year (not poll year)
polls <- merge(polls, cd[, .(year, stcd, houdv, houdvimp)],
               by = c("stcd","year"), all.x = TRUE)
polls <- merge(polls, nat[, .(year, eldate)], by = c("year"), all.x = TRUE)
polls[, date_mid := as.Date(date_mid, format = "%Y-%m-%d")]
polls[, ':=' (dte = difftime(eldate, date_mid, units = "days"))] # days to election
polls[, nmis := ifelse(is.na(poll), 0, 1)]

# split back into national vs district
gb <- polls[stcd == "NAT"]
gb <- GallupEdit(gb) # average gallup low and high turnout versions from same poll
polls <- polls[stcd != "NAT"]

# CLEAN UP --------------------------------------------------------------------
cd <- subset(cd, year >= 1980 & houwin != 9 & reapportion == 0) # might also set redist = 0
rm(list= ls()[!(ls() %in% c("cd", "pacs", "indivs", "gb", "polls", lsf.str()))])
save(list = ls(), file = "output/ohdata.RData")
