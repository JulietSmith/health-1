# http://www.hscic.gov.uk/searchcatalogue?q=title%3a%22presentation+level+data%22&sort=Relevance&size=10&page=1#top
setwd("~/Documents/mpini/github/health/prescribing")
library(sqldf)

getStatinsData <- function(filename) {
  print(paste('Loading in', filename))
  data <- read.csv(paste('data', filename, sep='/'))
  print(paste('Extracting statins entries from', filename))
  return(sqldf('SELECT * FROM data WHERE LOWER(BNF_NAME) LIKE "%statin%"'))
}

statins <- data.frame()
for(filename in list.files(path='data', pattern='*.CSV')) {
  statins <- rbind(statins, getStatinsData(filename))
  rm(filename)
}

head()

for(month in c('08', '09', '10', '11')) {
  data <- sqldf(paste('select * from statins where PERIOD = "2013', month, '"', sep=''))
  print(nrow(data))
  data <- sqldf('select BNF_NAME, count(*) count from data group by BNF_NAME')
  assign(paste('d2013', month, sep=''), )
}

merged <- merge(d201308,d201309, by.x='BNF_NAME', by.y='BNF_NAME', all.x=TRUE, all.y=TRUE)
colnames(merged) <- c('BNF_NAME', 'aug', 'sep')
merged <- merge(merged,d201310, by.x='BNF_NAME', by.y='BNF_NAME', all.x=TRUE, all.y=TRUE)
colnames(merged) <- c('BNF_NAME', 'aug', 'sep', 'oct')
merged <- merge(merged,d201311, by.x='BNF_NAME', by.y='BNF_NAME', all.x=TRUE, all.y=TRUE)
colnames(merged) <- c('BNF_NAME', 'aug', 'sep', 'oct', 'nov')

# ones with null in any one of the months
nullentries <- merged[is.na(merged$aug) | is.na(merged$sep) | is.na(merged$oct) | is.na(merged$nov),]

unique(statins$PERIOD)

head(statins)

practice_data <- read.csv('practice_address_list.csv', header=FALSE)
head(practice_data)

sqldf('SELECT * FROM practice_data WHERE V2 IN(select PRACTICE from statins where BNF_NAME like "%Ecostatin_Crm 1%")')

nhs.prescribing <- read.csv(path='data', 'prescribing.csv')
colnames(nhs.prescribing) <- c(gsub('\\.', '_', colnames(nhs.prescribing)))

nhs_prescribing <- nhs.prescribing
statins <- sqldf('SELECT *
                      FROM nhs_prescribing
                      WHERE LOWER(BNF_NAME) LIKE "%statin%"')
names(prescribing)
head(statins)
unique(statins$BNF_NAME)

unique_names <- sqldf('SELECT DISTINCT BNF_NAME
                        FROM statins
                        ORDER BY BNF_NAME ASC')
unique_names
# get counts for each statin
monthly_statin_count <- sqldf('SELECT BNF_NAME, count(*) 
                              FROM statins
                              GROUP BY BNF_NAME
                              ORDER BY count(*) DESC')


View(monthly_statin_count)


rans5 <- within(rans, {
  rans.j <- (rans.x + 2) * rans.y 
  m <- mean(rans.j)
  centered.rans.j <- rans.j - m
  rm(m, rans.j)
})

hist.with.normal <- function(x, xlab=deparse(substitute(x)),...) {
   h <- hist(x, plot=F, ...)
   s <- sd(x)
   m <- mean(x)
   ylim <- range(0,h$density,dnorm(0,sd=s))
   hist(x, freq=F, ylim=ylim, xlab=xlab, ...)
   curve(dnorm(x,m,s), add=T) }
hist.with.normal(rnorm(100))


testfunc <- function (x) {
  return((max(x) - min(x)) / mean(x))
}
merged$diff <- apply(merged[,2:5], 1, testfunc)
