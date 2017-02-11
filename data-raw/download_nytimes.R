rm(list = ls())
library("XML")
library("data.table")
library("stringr")

# FUNCTIONS --------------------------------------------------------------------
GetTable <- function(district){
  state <- gsub("\\d", "", district)
  cd <- gsub("\\D", "", district)
  url <- paste("http://www.nytimes.com/elections/2010/forecasts/house", state, cd, sep = "/")
  url <- paste0(url, ".html")
  tables <- readHTMLTable(url, stringsAsFactors = FALSE)
  if (length(tables) > 1){
    table <- tables[[1]][, 1:6]
    colnames(table) <- c("date", "poll", "sample_size", "weight", "dem", "rep")
    table$state <- state
    table$cd <- cd
    return(data.table(table))
  } else {
    df <- data.table(date = NA, poll = NA, sample_size = NA, weight = NA, 
                     dem = NA, rep = NA)
    df$state <- state
    df$cd <- cd
    return(df)
  }
}

StrPos <- function(x, char, n = 1){
  pos <- gregexpr(char, x)
  pos <- sapply(pos, function(x) x[n])
  pos[is.na(pos)] <- -1
  return(pos)
}

SimpleCap <- function(x) {
  s <- strsplit(x, " ")
  l <- lapply(s, function (y) 
    paste(toupper(substring(y, 1,1)), substring(y, 2),
          sep="", collapse=" "))
  return(do.call(rbind, l))
}

# LIST OF DISTRICTS ------------------------------------------------------------
districts <- c(paste0("alabama",seq(1, 7)),
               paste0("alaska", seq(1,1)),
               paste0("arizona", seq(1,8)),
               paste0("arkansas", seq(1,4)),
               paste0("california", seq(1, 53)),
               paste0("colorado", seq(1, 7)),
               paste0("connecticut", seq(1, 5)),
               paste0("delaware", seq(1, 1)),
               paste0("florida", seq(1, 25)),
               paste0("georgia", seq(1, 13)),
               paste0("hawaii", seq(1, 2)),
               paste0("idaho", seq(1, 2)),
               paste0("illinois", seq(1, 19)),
               paste0("indiana", seq(1, 9)),
               paste0("iowa", seq(1, 5)),
               paste0("kansas", seq(1, 4)),
               paste0("kentucky", seq(1, 6)),
               paste0("louisiana", seq(1, 7)),
               paste0("maine", seq(1, 2)),
               paste0("maryland", seq(1, 8)),
               paste0("massachusetts", seq(1, 10)),
               paste0("michigan", seq(1, 15)),
               paste0("minnesota", seq(1, 8)),
               paste0("mississippi", seq(1, 4)),
               paste0("missouri", seq(1, 9)),
               paste0("montana", seq(1, 1)),
               paste0("nebraska", seq(1, 3)),
               paste0("nevada", seq(1, 3)),
               paste0("new-hampshire", seq(1, 2)),
               paste0("new-jersey", seq(1, 13)),
               paste0("new-mexico", seq(1, 3)),
               paste0("new-york", seq(1, 29)),
               paste0("north-carolina", seq(1, 13)),
               paste0("north-dakota", seq(1, 1)),
               paste0("ohio", seq(1, 18)),
               paste0("oklahoma", seq(1, 5)),
               paste0("oregon", seq(1, 5)),
               paste0("pennsylvania", seq(1, 19)),
               paste0("rhode-island", seq(1, 2)),
               paste0("south-carolina", seq(1, 6)),
               paste0("south-dakota", seq(1, 1)),
               paste0("tennessee", seq(1, 9)),
               paste0("texas", seq(1, 32)),
               paste0("utah", seq(1, 3)),
               paste0("vermont", seq(1, 1)),
               paste0("virginia", seq(1, 11)),
               paste0("washington", seq(1, 9)),
               paste0("west-virginia", seq(1, 3)),
               paste0("wisconsin", seq(1, 8)),
               paste0("wyoming", seq(1, 1))
)
               
# DOWNLOAD POLLS ---------------------------------------------------------------
tables <- list()
for (i in 1:length(districts)){
  tables[[i]] <- GetTable(districts[[i]])
  print(i)
}
polls <- rbindlist(tables)

# CLEANING ---------------------------------------------------------------------
# polling organization
polls[, poll := gsub("\n", "", poll)]
polls[, ':=' (poll =  gsub("\\s+", " ", poll), date = gsub("\\s+", " ", date))]
polls[, partisan := ifelse(word(poll, -1) == "DEM", "dem", 
                           ifelse(word(poll, -1) == "REP", "rep",
                                  ifelse(!is.na(poll), "neutral", NA)))]
polls[, poll := gsub(" DEM| REP", "", poll)]

# month abbreviations
polls[, date := gsub("Sept.", "Sep", date)]

# polls from 2009
polls[, comma := StrPos(date, ",", 1)]
polls[comma >=0, year := as.numeric(substr(date, comma + 1, nchar(date)))]
polls[!is.na(date) & comma <0, year := 2010]
polls[comma >=0, date := substr(date, 1, comma - 1)]

# start and end months/days
polls[, dashpos := StrPos(date, "-", 1)]
polls[!is.na(date) & dashpos >= 0, ':=' (month_beg = gsub("[^A-z]", "", substr(date, 1, dashpos)),
      day_beg = gsub("[^0-9]", "", substr(date, 1, dashpos)))]
polls[!is.na(date) & dashpos == -1, ':=' (month_beg = gsub("[^A-z]", "", date),
                                          day_beg = gsub("[^0-9]", "", date))]
polls[, month_end:= ifelse(!is.na(date) & dashpos >= 0 & grepl("[A-z]", substr(date, dashpos, nchar(date))),
      gsub("[^A-z]", "", substr(date, dashpos, nchar(date))), month_beg)]
polls[, day_end:= ifelse(!is.na(date) & dashpos >= 0,
                           gsub("[^0-9]", "", substr(date, dashpos, nchar(date))), day_beg)]

# start and end dates
polls[, date_beg := as.Date(paste(month_beg, day_beg, year, sep = " "), format = "%B %d %Y")]
polls[, date_end := as.Date(paste(month_end, day_end, year, sep = " "), format = "%B %d %Y")]
polls[, date_mid := date_beg + floor((date_end - date_beg)/2)]

# sample types voters
polls[, sample := word(sample_size, -1)]
polls[, sample_size := word(sample_size, 1)]
polls[, sample_size := as.numeric(gsub(",", "", sample_size))]

# state names and abbreviations
polls[, state := SimpleCap(gsub("-", " ", state))]
polls[, st_usps := state.abb[match(state, state.name)]]
polls[, stcd := paste0(st_usps, formatC(as.numeric(cd), width = 2, format = "d", flag = "0"))]

# fixing errors
polls[stcd == "AR03" & date == "Oct. 14", rep := "59"]

# cleaning up
polls[, c("comma", "dashpos", "month_beg", "day_beg", "month_end", "day_end", 
          "state", "cd", "st_usps") := NULL]
setcolorder(polls, c("stcd", names(polls)[-which(names(polls)=="stcd")]))
polls[is.na(year), year := 2010]
polls[, ':=' (dem = as.numeric(dem), rep = as.numeric(rep))]
polls[, dv := dem/(dem + rep)]
polls <- polls[!is.na(date)]
setnames(polls, "year", "pollyear")
polls[, year := 2010]
polls <- polls[order(-date_mid)]

# TEMPORARY SAVE TO CSV --------------------------------------------------------
write.csv(polls, "nytimes2010.csv", row.names = FALSE)