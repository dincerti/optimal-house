packages <- c("XML", "data.table", "stringr", "zoo")
lapply(packages, library, character.only = TRUE)

# FUNCTIONS --------------------------------------------------------------------
StrPos <- function(x, char, n = 1){
  pos <- gregexpr(char, x)
  pos <- sapply(pos, function(x) x[n])
  pos[is.na(pos)] <- -1
  return(pos)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# DOWNLOAD POLLS ---------------------------------------------------------------
url <- "http://www.realclearpolitics.com/epolls/other/2010_generic_congressional_vote-2171.html"
gb <- data.table(readHTMLTable(url, stringsAsFactors = FALSE)[[1]])
                 
# CLEANING ---------------------------------------------------------------------
gb <- gb[-c(1:2)]
gb[, dashpos := StrPos(Date, "-")]
gb[, ':=' (date_beg = substr(Date, 1, dashpos -1), 
           date_end = substr(Date, dashpos + 1, nchar(Date)),
           sample_size = as.numeric(gsub("\\D", "", Sample)))]
gb[, Sample := gsub("\\d", "", Sample)]
gb[,  ':=' (Sample = trim(Sample), date_beg = trim(date_beg), date_end = trim(date_end))]

# assign years correctly
gb[, N := seq(1, nrow(gb))]
gb[, ':=' (tmp_beg = as.Date(date_beg, "%m/%d"), tmp_end = as.Date(date_end, "%m/%d"))]
gb[, ':=' (month_beg = month(tmp_beg), month_end = month(tmp_end))]
gb[, ':=' (lmonth_beg = c(NA, month_beg[-.N]), lmonth_end = c(NA, month_end[-.N]))]
newyrbeg <- gb[month_beg == 12 & lmonth_beg == 1, .(N, tmp_beg)]
newyrbeg[, year_beg := seq(2010 - 1, by = -1, length.out = nrow(newyrbeg))]
newyrend <-gb[month_end == 12 & lmonth_end == 1, .(N, tmp_end)]
newyrend[, year_end := seq(2010 -1 , by = -1, length.out = nrow(newyrend))]
gb <- merge(gb, newyrbeg, by = c("N", "tmp_beg"), all.x = TRUE)
gb <- merge(gb, newyrend, by = c("N", "tmp_end"), all.x = TRUE)
gb[1, ':=' (year_beg = 2010, year_end = 2010)]
gb[, ':=' (year_beg = na.locf(year_beg), year_end = na.locf(year_end))]

# create date variable
gb[, ':=' (date_beg = paste(date_beg, year_beg, sep = "/"),
           date_end = paste(date_end, year_end, sep = "/"))]
gb[, ':=' (date_beg = as.Date(date_beg, "%m/%d/%Y"),
           date_end = as.Date(date_end, "%m/%d/%Y"))]
gb[, date_mid := date_beg + floor((date_end - date_beg)/2)]

# cleaning
setnames(gb, names(gb), tolower(names(gb)))
setnames(gb, names(gb), trim(names(gb)))
setnames(gb, c("democrats", "republicans"), c("dem", "rep"))
gb <- gb[, .(poll, date, sample_size, sample, rep, dem, date_beg, date_end, date_mid)]
gb[, ':=' (dem = as.numeric(dem), rep = as.numeric(rep))]
gb[, dv := dem/(dem + rep)]

# fix polling names
gb[poll == "Ipsos???McClatchy", poll := "Ipsos/McClatchy"]

# SAVE -------------------------------------------------------------------------
write.csv(gb, "rcpgb.csv", row.names = FALSE)
