## Neutrophil counts and Cancer Prognosis:
## an Umbrella Review and Meta-analysis

# Assessment of gastric cancers: 


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
dat <- read.xls(datfile, sheet="Rectal OS", perl="/usr/bin/perl")
#Print the data
print(dat)
View(dat)

#Effect sizes == LogHR
#Calculate log HR
dat$yi <- with(dat, log(HR))

#back transform 95% CIs to find standard error
#SE = [Log(U)-Log(HR)]/1.96
dat$sei <- ((log(dat$Upper.CI)-dat$yi)/1.96)


#Use generic inverse variance meta-analysis to calculate pooled HR from
#log(HR) and standard error
## inverse variance weighting is used for pooling results
recOS <- metagen(TE = dat$yi, seTE = dat$sei, data = NULL, studlab = dat$Author, 
                 subset=(dat$Outcome =="OS" & dat$Site == "Rectal"), sm ="HR", 
                 prediction = TRUE, comb.random = TRUE)

forest.meta(recOS, studlab = TRUE, 
            comb.random = recOS$comb.random, 
            comb.fixed = recOS$comb.fixed,
            text.random = "Pooled HR by Random Effects",
            text.fixed = "Pooled HR by Fixed Effects", 
            lyt.random = TRUE, 
            lyt.fixed = TRUE, 
            type.study="square", 
            type.fixed="diamond", 
            type.random="diamond", 
            col.study="black", 
            col.square="lightskyblue3", 
            col.inside="white", 
            col.diamond="lightskyblue3", 
            col.diamond.lines="dodgerblue4", 
            col.predict="dodgerblue4", 
            col.predict.lines="dodgerblue4", 
            col.by="dodgerblue4", 
            col.label.right="black",
            col.label.left="black", 
            hetlab = "Heterogeneity: ", 
            print.I2 = recOS$comb.fixed | recOS$comb.random, 
            print.I2.ci = TRUE, 
            print.tau2 = FALSE, 
            print.Q = TRUE, 
            print.Rb = FALSE, 
            print.Rb.ci = FALSE)

#print meta data on recOS
print(recOS)

#funnel plot
funnel.meta(recOS, pch=1, 
            col.fixed = "dodgerblue4",
            col.random = "lightskyblue3",
            backtransf = FALSE)

#test for asymmetry in funnel plot
metabias(recOS, method.bias = "linreg", 
         plotit = TRUE, correct = FALSE,
         k.min=2)



