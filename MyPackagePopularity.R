
options(repos = c(CRAN = "http://cran.rstudio.com"))
  # Here's an easy way to get all the URLs in R
  start <- as.Date('2013-01-01'); #start <- as.Date('2013-01-01')
  today <- as.Date('2018-07-16')

  all_days <- seq(start, today, by = 'day')

  year <- as.POSIXlt(all_days)$year + 1900
  files <- paste0(all_days, '.csv.gz')
  urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')

    # If you only want to download the files you don't have, try:
  missing_days <- setdiff(all_days, tools::file_path_sans_ext(dir(), TRUE))


m <- length(all_days)
for(i in 1:m) {
  if(file.exists(files[i])) {
   message("skipping ",files[i])
   next
  }
  try(download.file(urls[i], files[i]))
}
NAs <- rep(NA, m)
AP <- data.frame(date=NAs, lmomco_rnk=NAs,
                 lmomco_pct=NAs,     lmomco_cnt=NAs,   lmomco_countries=NAs,
                           copBasic_rnk=NAs,
                 copBasic_pct=NAs, copBasic_cnt=NAs, copBasic_countries=NAs,
                 total_cnt=NAs)
for(i in 1:m) {
   if(!  file.exists(files[i])) next
   system(paste0("gzcat ",files[i]," > tmp.txt"))
   df <- read.table("tmp.txt", sep=",", header=TRUE)
   df <- df[! is.na(df$package),]
   P <- aggregate(df$package, by=list(df$package), length)
   names(P) <- c("package", "count")
   P <- P[order(P$count, decreasing=FALSE),]; n <- length(P$count)
   P$rank <- 1:n
   dflm <- df[df$package == "lmomco",   ]
   dfcb <- df[df$package == "copBasic", ]
   Cl <- length(unique(dflm$country))
   Cb <- length(unique(dfcb$country))
   lmomco   <- P[P$package == "lmomco",   ]
   copBasic <- P[P$package == "copBasic", ]
   if(length(lmomco$count) == 0) {
     lmomco <- list(count=0,   rank=NA, countries=NA)
   } else {
     lmomcoBarRank <- mean(P[P$count == lmomco$count,]$rank)
     lmomco$rank <- lmomcoBarRank
     lmomco$countries <- Cl
   }
   if(length(copBasic$count) == 0) {
     copBasic <- list(count=0, rank=NA, countries=NA)
   } else {
     copBarRank <- mean(P[P$count == copBasic$count,]$rank)
     copBasic$rank <- copBarRank
     copBasic$countries <- Cb
   }
   message("processing ",i," ",files[i], " ",   lmomco$count, " and ",
                                              copBasic$count, " with ",
                         n, " total all CRAN downloads")
   AP[i,1]    <- as.character(all_days[i])
   AP[i,2:10]  <- c(lmomco$rank,
         100*round(lmomco$rank/n, digits=4),
                   lmomco$count,     lmomco$countries,
                   copBasic$rank,
         100*round(copBasic$rank/n, digits=4),
                   copBasic$count, copBasic$countries, n)
   unlink("tmp.txt")
}
AP[,1] <- as.Date(AP[,1])
Packages <- AP
save(Packages, file="Packages.RData"); rm(Packages)
load("Packages_20130101_20180614.RData") # Packages is coming back
AP <- merge(Packages, AP, all=TRUE)

#Packages <- AP
#save(Packages, file="Packages_20130101_20180614.RData")

library(kernlab)
tmp <- AP[complete.cases(AP),]
svmy <- ksvm(tmp$lmomco_pct~I(as.numeric(tmp$date)/(60*60*24*365)))
y <- predict(svmy)
scy <- slot(svmy, "scaling")
y <- scy$y.scale$`scaled:scale`*y + scy$y.scale$`scaled:center`

svmz <- ksvm(tmp$copBasic_pct~I(as.numeric(tmp$date)/(60*60*24*365)))
z <- predict(svmz)
scz <- slot(svmz, "scaling")
z <- scz$y.scale$`scaled:scale`*z + scz$y.scale$`scaled:center`


COP <- read.table("timeline_copBasic.txt", header=TRUE, stringsAsFactors=FALSE)
LMR <- read.table("timeline_lmomco.txt",   header=TRUE, stringsAsFactors=FALSE)
COP$time <- as.Date(COP$time)
LMR$time <- as.Date(LMR$time)

pdf("MyPackagePopularity.pdf", useDingbats=FALSE, width=7, height=6)
par(las=1, lend=1, mgp=c(3,0.5,0))
plot(AP$date, AP$copBasic_pct, type="n",
     xlab="Date (daily download data from cran-logs.rstudio.com)", tcl=0.5,
     ylab="Rank as percentile against other package downloads (100 is best)", ylim=c(0,100),
     xaxs="i", yaxs="i")
for(i in seq(20,95, by=5)) {
   lines(par()$usr[1:2], rep(i,2), lty=2, lwd=0.6)
}
for(i in LMR$time) {
  lines(rep(i,2), c(0,50), col=4, lty=2)
}
for(i in COP$time) {
  lines(rep(i,2), c(50,200), col=2, lty=2)
}
for(i in LMR$time) {
  lines(rep(i,2), c(50,100), col=4)
}
for(i in COP$time) {
  lines(rep(i,2), c(0,50), col=2)
}
jnkcb <- data.frame(date=AP$date,
                    copBasic_pct=AP$copBasic_pct,
                    copBasic_countries=AP$copBasic_countries,
                    lmomco_pct=NA,
                    lmomco_countries=NA,
                    isCB=1)
jnklm <- data.frame(date=AP$date,
                    copBasic_pct=NA,
                    copBasic_countries=NA,
                    lmomco_pct=AP$lmomco_pct,
                    lmomco_countries=AP$lmomco_countries,
                    isCB=0)
ap <- rbind(jnkcb, jnklm); ap <- ap[! is.na(ap$date),]; ix <- 1:length(ap$date)
ix <- sample(ix, size=length(ix), replace=FALSE)
ap <- ap[ix,]; ap$isCB <- as.logical(ap$isCB)
ap$col <- rgb(1,.4,0,.3); ap$col[! ap$isCB] <- rgb(0,.4,1,.3)
ap$cex <- ap$copBasic_countries/10
ap$cex[is.na(ap$cex)] <- ap$lmomco_countries[is.na(ap$cex)]/10
ap$pct <- ap$copBasic_pct
ap$pct[is.na(ap$pct)] <- ap$lmomco_pct[is.na(ap$pct)]

points(ap$date, ap$pct, cex=ap$cex, col=ap$col, pch=16, lwd=0.4)
#points(ap$date[ap$isCB], ap$copBasic_pct[ap$isCB], cex=ap$copBasic_countries[ap$isCB]/10, lwd=0.4, pch=16, col=rgb(1,.4,0,.3))
#points(ap$date[! ap$isCB], ap$lmomco_pct[! ap$isCB], cex=ap$lmomco_countries[! ap$isCB]/10, lwd=0.4, pch=16, col=rgb(0,.4,1,.3))
lines(tmp$date, y, col=4, lwd=4)
lines(tmp$date, z, col=2, lwd=4)
legend(as.Date("2013-02-01"), 21,
       c("Trend line for lmomco package by kernlab::ksvm(<defaults>)",
         "Trend line for copBasic package by kernlab::ksvm(<defaults>)",
         "lmomco package (L-moments and many distributions) [size {cex}=no. countries/10]",
         "copBasic package (copulas, utilities, and theory) [size {cex}=no. countries/10]"),
       pch=c(NA,NA,16,16), lwd=c(4,4,NA,NA), bty="o", box.col=NA, bg=grey(1,.8),
       col=c(4,2,rgb(0,.4,1),rgb(1,.4,0)), cex=0.85,
      )
mtext("TREND IN R PACKAGE POPULARITY (lmomco, copBasic)")
dev.off()

m <- length(AP$lmomco_pct)
if(m > 180) {
  message("Mean last 180 days lmomco: ",
           mean(AP$lmomco_pct[(m-180):m], na.rm=TRUE))
  message("Mean last 180 days copBasic: ",
           mean(AP$copBasic_pct[(m-180):m], na.rm=TRUE))
}

library(lmomco)
plot(pp(AP$lmomco_cnt, sort=FALSE),pp(AP$copBasic_cnt, sort=FALSE))
plot(qnorm(pp(AP$lmomco_cnt, sort=FALSE)),
     qnorm(pp(AP$copBasic_cnt, sort=FALSE)))

# if (!require('devtools')) install.packages('devtools'); require('devtools')
# make sure you have Rtools installed first! if not, then run:
#install_Rtools()
#install_github('installr', 'talgalili') # get the latest installr R package
# or run the code from here:
# https://github.com/talgalili/installr/blob/master/R/RStudio_CRAN_data.r


##### start
#if(packageVersion("installr") %in% c("0.8","0.9","0.9.2")) install.packages('installr') #If you have one of the older installr versions, install the latest one....

#require(installr)

# The first two functions might take a good deal of time to run (depending on the date range)
#RStudio_CRAN_data_folder <- download_RStudio_CRAN_data(START = '2013-04-02', END = '2013-04-05') # around the time R 3.0.0 was released
#my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder)

 # barplots: (more functions can easily be added in the future)
#barplot_package_users_per_day("plyr", my_RStudio_CRAN_data)
#barplot_package_users_per_day("installr", my_RStudio_CRAN_data)



#https://www.r-bloggers.com/analyzing-package-dependencies-and-download-logs-from-rstudio-and-a-start-towards-building-an-r-recommendation-engine/
