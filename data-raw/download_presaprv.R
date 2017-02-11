rm(list=ls()) 
library(XML)
library(stringr)
library(data.table)

# FUNCTIONS --------------------------------------------------------------------
GetTable <- function(pres){
  browser()
  url <- paste0("http://www.ropercenter.uconn.edu/CFIDE/roper/presidential/webroot/",
                "presidential_rating_detail.cfm?allRate=True&presidentName=")  
  tables <- readHTMLTable(paste0(url, pres), stringsAsFactors = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  return(tables[[which.max(n.rows)]])
}

SubstrRight <- function(x, n) {
  substr(x, nchar(x)-n+1, nchar(x))
}

StrPos <- function(x, char, n = 1){
  pos <- gregexpr(char, x)
  pos <- sapply(pos, function(x) x[n])
  pos[is.na(pos)] <- -1
  return(pos)
}

# DOWNLOAD DATA ----------------------------------------------------------------
presidents <- c("Obama", "Bush", "Clinton", "Bush%20(G.H.W.)", "Reagan",
                "Carter", "Ford", "Nixon", "Johnson", "Kennedy", "Eisenhower",
                "Truman", "Roosevelt")
df <- list()
for (i in 1:length(presidents)){
  df[[i]] <- GetTable(presidents[i])
  df[[i]]$pres <- presidents[i]
}
df <- do.call(rbind, df)

# CLEAN ------------------------------------------------------------------------
# column names
names(df) <- gsub("\r\n|\\*", "", names(df))
names(df) <- gsub(" ", "", names(df)) # remove white space between words)
names(df) <- tolower(names(df))

# character to numeric
df$approve <- as.numeric(df$approve)
df$disapprove <- as.numeric(df$disapprove)
df$noopinion <- as.numeric(df$noopinion)
df$samplesize <- as.numeric(df$samplesize)

# remove white space in date variable
df$date <- gsub(" ", "", df$date) 

# a bit of error correction
df$date[df$date == "11/10/-13/01"] <- "11/10-13/01"
df$date[df$date == "2/5-10/0"] <- "2/5-10/10"
df$date[df$date == "23-9/10"] <- "2/3-9/10"
df$date[df$date == "10-30-11/1/08"] <- "10/30-11/1/08"
df$date[df$date == "428-30/08"] <- "4/28-30/08"
df$date[df$date == "3/12-1509"] <- "3/12-15/09"
df$date[df$date == "4/28-3-/06"] <- "4/28-30/06"
df$date[df$date == "12/2-3-/09"] <- "12/2-3/09"
df$date[df$date == "3/31/-4/1/09"] <- "3/31/09-4/1/09"

# create year variables
df$year2d <- SubstrRight(df$date, 2)
df$year <- as.integer(ifelse(as.integer(df$year2d) >= 0 & as.integer(df$year2d) <= 15,
                  paste0("20", df$year2d), paste0("19", df$year2d)))
df$year2d <- NULL

# create temporary variables for beginning and ending date variables
df$slashes <- str_count(df$date, "/")
df$dashes <- str_count(df$date, "-")
df$posslash1 <- StrPos(df$date, "/", 1)
df$posslash2 <- StrPos(df$date, "/", 2)
df$posslash3 <- StrPos(df$date, "/", 3)
df$posslash4 <- StrPos(df$date, "/", 4)
df$posand <- StrPos(df$date, "&", 1)
df$posstrand <- StrPos(df$date, "and", 1)
df$posdash1 <- StrPos(df$date, "-", 1)
df$posdash2 <- StrPos(df$date, "-", 2)
df$month_beg <- substr(df$date, 1, df$posslash1 -1) 

# create beggining date variable
df$date_beg <- ifelse(df$slashes < 4,
                      substr(df$date, 1, df$posdash1 - 1),
                      substr(df$date, 1, df$posslash2 - 1))
df$date_beg <- ifelse(df$slashes == 4 & grepl("and", df$date),
                      substr(df$date, 1, df$posdash1 - 1), 
                      df$date_beg) # update for dates with "and" in them
df$date_beg <- ifelse(!grepl("-", df$date), 
                      substr(df$date, 1, df$posslash2 - 1), 
                             df$date_beg) # polls from single day

# create ending date variable
df$date_end <- ""
df$date_end <- ifelse(df$slashes == 2 & df$dashes == 1, 
                      paste0(df$month_beg, "/",
                             substr(df$date, df$posdash1 + 1, df$posslash2 -1)),
                             df$date_end)
df$date_end <-ifelse(df$slashes == 3 & df$dashes == 1,
                     substr(df$date, df$posdash1 + 1, df$posslash3 -1),
                     df$date_end)
df$date_end <- ifelse(df$slashes == 2 & df$dashes == 2,
                      paste0(df$month_beg, "/",
                             substr(df$date, df$posdash2 + 1, df$posslash2 - 1)),
                             df$date_end) 
df$date_end <- ifelse(df$slashes == 3 & df$dashes == 2,   
                      paste0(substr(df$date, df$posand + 1, df$posslash2 ),
                             substr(df$date, df$posdash2 + 1, df$posslash3 -1)),
                      df$date_end)
df$date_end <- ifelse(df$slashes == 4 & !grepl("and", df$date),
                      substr(df$date, df$posdash1 + 1, df$posslash4 -1),
                             df$date_end)
df$date_end <- ifelse(df$slashes == 4 & grepl("and", df$date),
                      paste0(substr(df$date, df$posstrand + 3, df$posslash3),
                      substr(df$date, df$posdash2 + 1, df$posslash4 - 1)),
                      df$date_end)
df$date_end <- ifelse(!grepl("-", df$date), 
                      substr(df$date, 1, df$posslash2 - 1), 
                      df$date_end) # polls from single day


# full date_beg and date_end
df$date_beg <- paste0(df$date_beg, "/", df$year)
df$date_beg <- ifelse(df$slashes == 1, "", df$date_beg)
df$date_end <- paste0(df$date_end, "/", df$year)
df$date_end <- ifelse(df$slashes == 1, "", df$date_end)

# mid-point date
df$date_mid <- as.Date(df$date_beg, "%m/%d/%Y") +
  floor((as.Date(df$date_end, "%m/%d/%Y") - as.Date(df$date_beg, "%m/%d/%Y"))/2)

# create month variable
df$month <- month(df$date_mid)
df$month <- ifelse(df$slashes == 1, as.integer(substr(df$date, 1, df$posslash1 - 1)),
                   df$month) # dates with only months and not days

# format date
df$date_mid <- strftime(df$date_mid ,"%m/%d/%Y")
df$date_mid <- as.character(df$date_mid)

# delete variables
df <- subset(df, -c(month_beg, slashes, dashes, posdash1, posdash2,
                      posslash1, posslash2, posslash3, posslash4,
                      posand, posstrand))

# SAVE -------------------------------------------------------------------------
saveRDS(df, "pres-approval.rds")
