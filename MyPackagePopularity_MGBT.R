the.pkg <- "MGBT"
the.origin <- "2019-10-31"

options(repos = c(CRAN = "http://cran.rstudio.com"))
  # Here's an easy way to get all the URLs in R
  start <- as.Date('2021-02-01'); #start <- as.Date(the.origin)
  today <- as.Date('2021-02-24')

  all_days <- seq(start, today, by='day')

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
AP <- data.frame(date=NAs, PKG_rnk=NAs,
                 PKG_pct=NAs,     PKG_cnt=NAs,   PKG_countries=NAs,
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
   dflm <- df[df$package == the.pkg,  ]
   Cl <- length(unique(dflm$country))
   PKG   <- P[P$package == the.pkg,   ]
   if(length(PKG$count) == 0) {
     PKG <- list(count=0,   rank=NA, countries=NA)
   } else {
     PKGBarRank <- mean(P[P$count == PKG$count,]$rank)
     PKG$rank <- PKGBarRank
     PKG$countries <- Cl
   }
   message("processing ",i," ",files[i], " ",   PKG$count, " with ",
                         n, " total all CRAN downloads")
   AP[i,1]    <- as.character(all_days[i])
   AP[i,2:6] <- c(PKG$rank,
         100*round(PKG$rank/n, digits=4),
                   PKG$count,     PKG$countries, n)
   unlink("tmp.txt")
}
AP[,1] <- as.Date(AP[,1])
Package <- AP
save(Package, file=paste0(the.pkg,".RData")); rm(Package)
file <- paste0(the.pkg,"_","20191001_20201231.RData")
if(file.exists(file)) load(file) # Packages is coming back
AP <- merge(Package, AP, all=TRUE)

#Package <- AP
#save(Package, file=paste0(the.pkg,"_","20191001_20201231.RData"))

library(kernlab)
yearize <- 365
tmp <- AP[complete.cases(AP),]
svmy <- ksvm(tmp$PKG_pct~I(as.numeric(tmp$date)/yearize), cross=0, C=.1)
y <- predict(svmy, tmp)

RT  <- read.table("timeline_R.txt",               header=TRUE, stringsAsFactors=FALSE)
ME <- read.table(paste0("timeline_",the.pkg,".txt"), header=TRUE, stringsAsFactors=FALSE)
RT$time <- as.Date(RT$time)
ME$time <- as.Date(ME$time)

#pdf(paste0(the.pkg,"_PackagePopularity.pdf"), useDingbats=FALSE, width=7, height=6)
par(las=1, lend=1, mgp=c(3,0.5,0))
plot(AP$date, AP$PKG_pct, type="n",
     xlab="Date (daily download data from cran-logs.rstudio.com)", tcl=0.5,
     ylab="Rank as percentile against other package downloads (100 is best)", ylim=c(0,100),
     xaxs="i", yaxs="i")
suppressWarnings(rug(RT$time, tcl=-0.5, col="#22a524", lwd=3))
for(i in seq(05,95, by=5)) {
   lines(par()$usr[1:2], rep(i,2), lty=2, lwd=0.6)
}
for(i in ME$time) {
  lines(rep(i,2), c(0,100), col=4, lty=1)
}
ap <- data.frame(date=AP$date, PKG_pct=AP$PKG_pct, PKG_countries=AP$PKG_countries)
ap <- ap[! is.na(ap$date),]; ix <- 1:length(ap$date)
ix <- sample(ix, size=length(ix), replace=FALSE)
ap <- ap[ix,]
ap$col <- rgb(1,.4,0,.5)
ap$cex <- ap$PKG_countries/10

points(ap$date, ap$PKG_pct, cex=ap$cex, col=ap$col, pch=16, lwd=0.4)
lines(tmp$date, y, col=4, lwd=4)
legend(as.Date(the.origin), 100,
       c(paste0("Trend line for ",the.pkg," package by kernlab::ksvm(<defaults>)"),
         "Release date of R (see outside 'rug' ticks on horizontal axis)",
         paste0("Release date of ",the.pkg," (solid and dashed aids viewing when overplotting)"),
         paste0(the.pkg," package (L-moments and many distributions) [size {cex}=no. countries/10]")),
       pch=c(NA,NA,NA,16), lwd=c(4,3,1,NA), bty="o", box.col=NA, bg=grey(1,.8),
       col=c(4,"#22a524",4,rgb(1,.4,0)), cex=0.85,
      )
mtext(paste0("TRENDS IN 'GLOBAL' R PACKAGE POPULARITY (",the.pkg,")"))
#dev.off()

m <- length(AP$PKG_pct)
if(m > 180) {
  message("Mean last 180 days PKG: ",
           round(mean(AP$PKG_pct[  (m-180):m], na.rm=TRUE), digits=1))
}

message("Total package count:   ", sum(AP$PKG_cnt,   na.rm=TRUE))
