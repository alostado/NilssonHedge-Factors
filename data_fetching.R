library(tidyverse)
library(readxl)

# Data Downloading ----

# url_database <- '<INSERT URL HERE>'
# url_classification <- '<INSERT URL HERE>'

# CTA database

download.file(
  url_database,
  "factordata/CTADatabase.csv"
)
download.file(
  url_classification,
  "factordata/CTADatabaseClassification.csv"
)

# FF5 factors

temp <- tempfile()
download.file(
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip",
  temp
)
unzip(temp, exdir='factordata')

dat <- readLines('factordata/F-F_Research_Data_5_Factors_2x3.CSV')

keep <- as.numeric(substr(dat, 1, 6))
keep[is.na(keep)] <- 0
keep <- keep>10000
dat <- dat[keep]
dat <- read.csv(textConnection(dat), header = F)
colnames(dat) <- c('yearmon', 'MKT', 'SMB', 'HML', 'RMW', 'CMA', 'RF')
write.table(dat, 'factordata/ff5.csv', sep = ',', row.names = F, col.names = T)

# FF Momentum

temp <- tempfile()
download.file(
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip",
  temp
)
unzip(temp, exdir='factordata')

dat <- readLines('factordata/F-F_Momentum_Factor.CSV')

keep <- as.numeric(substr(dat, 1, 6))
keep[is.na(keep)] <- 0
keep <- keep>10000
dat <- dat[keep]
dat <- read.csv(textConnection(dat), header = F)
colnames(dat) <- c('yearmon', 'MOM')
write.table(dat, 'factordata/mom.csv', sep = ',', row.names = F, col.names = T)

# Data Cleaning ----

CTAdb <- tibble(read.csv('factordata/CTADatabase.csv', header = F))
colnames(CTAdb) <- c('Manager', 'Program', 'Date', 'Return', 'AUM')

CTAclass <- tibble(read.csv('factordata/CTADatabaseClassification.csv', 
                            header = F))
colnames(CTAclass) <- c('Manager', 'Program', 'Type', 'Style', 'Strategy', 'Sector')

CTAdb$Date <- as.Date(CTAdb$Date)
CTAdb$yearmon <- format(CTAdb$Date, format = '%b %Y')

CTAdb$Manager[CTAdb$Manager == 'Rcube Asset Management'] = 'RCube Asset Management'

CTAclass = CTAclass %>% mutate(ID = row_number(Program))

CTAdb = left_join(CTAdb, CTAclass, by=c('Program','Manager'))

# FF5 factors

ff5 <- tibble(read.csv('factordata/ff5.csv'))

ff5$yearmon <- 
  format(as.Date(paste0(as.character(ff5$yearmon),'01'), 
                 '%Y%m%d'), 
         format='%b %Y')

ff5 = ff5 %>% 
  mutate(across(-yearmon, function(x){x/100}))

# FF MOM Factor
mom <- tibble(read.csv('factordata/mom.csv'))

mom$yearmon <- 
  format(as.Date(paste0(as.character(mom$yearmon),'01'), 
                 '%Y%m%d'), 
         format='%b %Y')

mom = mom %>% 
  mutate(across(-yearmon, function(x){x/100}))


# Join data with factors

df <- left_join(CTAdb, ff5, by='yearmon')

df <- left_join(df, mom, by='yearmon')

# Get limit dates for factor models

capm_logic = !is.na(df$RF) & !is.na(df$MKT)
ff3_logic = capm_logic & !is.na(df$SMB) & !is.na(df$HML)
ff5_logic = ff3_logic & !is.na(df$RMW) & !is.na(df$CMA)
ff5_mom_logic = ff5_logic & !is.na(df$MOM)

capm_mindate = min(df$Date[capm_logic])
capm_maxdate = max(df$Date[capm_logic])
ff3_mindate = min(df$Date[ff3_logic])
ff3_maxdate = max(df$Date[ff3_logic])
ff5_mindate = min(df$Date[ff5_logic])
ff5_maxdate = max(df$Date[ff5_logic])
ff5_mom_mindate = min(df$Date[ff5_mom_logic])
ff5_mom_maxdate = max(df$Date[ff5_mom_logic])

rm(list=setdiff(ls(), c('df', 'capm_mindate','capm_maxdate','ff3_mindate',
                        'ff3_maxdate','ff5_mindate','ff5_maxdate','ff5_mom_mindate',
                        'ff5_mom_maxdate')))

save.image('CTAdb.RData')

