#Load the 'meta' package
library(meta)
#Load the 'rmeta' package
library(rmeta)
#Load 'gdata' package
library(gdata)
#Load the 'metafor' package
library(metafor)
#Invoke help manual for metafor
library(help=metafor)
#Loading data from Excel
require(gdata)
#Link the Excel Databook at your file path

datfile = "~/Google Drive/Dissertation/Chapters/Data Extraction/Data set.xlsx"
#Call "read.xls" to read the specific Excel data sheet
dat <- read.xls(datfile, sheet="Random", perl="/usr/bin/perl")
#Print the data
print(dat)
View(dat)

#Effect sizes == LogHR
#Calculate log HR
dat$yi <- with(dat, log(HR))

#back transform 95% CIs to find standard error
#SE = [Log(U)-Log(HR)]/1.96
dat$sei <- ((log(dat$Upper.CI)-dat$yi)/1.96)

#Effect sizes == LogHR
#Calculate log HR
dat$yi <- with(dat, log(HR))

#back transform 95% CIs to find standard error
#SE = [Log(U)-Log(HR)]/1.96
dat$sei <- ((log(dat$Upper.CI)-dat$yi)/1.96)

#calculating p values
dat$z <- (dat$yi/dat$sei)
dat$pval <- (1 - pnorm(abs(dat$z))) * 2

#is p value significant at 0.05?
dat$sig_0.05 <- dat$pval < 0.05
table(dat$sig_0.05)

#is p value significant at 0.0001?
dat$sig_0.0001 <- dat$pval < 0.001
table(dat$sig_0.001)

#is p value significant at 0.00001?
dat$sig_0.00001 <- dat$pval < 0.00001
table(dat$sig_0.00001)


#is largest study sig at 0.05?
#Effect sizes == LogHR
#Calculate log HR
dat$Lyi <- with(dat, log(Largest.HR))

#back transform 95% CIs to find standard error
#SE = [Log(U)-Log(HR)]/1.96
dat$Lsei <- ((log(dat$L.up.CI)-dat$Lyi)/1.96)

#calculating p values
dat$Lz <- (dat$Lyi/dat$Lsei)
dat$Lpval <- (1 - pnorm(abs(dat$Lz))) * 2

dat$sigL <- dat$Lpval < 0.05


