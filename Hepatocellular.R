## Neutrophil counts and Cancer Prognosis:
## an Umbrella Review and Meta-analysis

#Load the tidyverse
library(tidyverse)
#Load the 'meta' package
library(meta)
#Load the 'rmeta' package
library(rmeta)
#Load 'gdata' package
library(gdata)
#Load the 'metafor' package
library(metafor)
#Loading data from Excel
require(gdata)
#Link the Excel Databook at your file path

##########################################################
#       Meta-analysis of Hepatocellular Cancers          #
##########################################################

#Clean work space
remove(list = ls())

datfile = "Neutrophils.xlsx"
#Call "read.xls" to read the specific Excel data sheet
Hepatocellular <- read.xls(datfile, sheet="Liver", perl="/usr/bin/perl")

#Effect sizes == LogHR
#Calculate log HR
Hepatocellular$yi <- with(Hepatocellular, log(HR))

#back transform 95% CIs to find standard error
#SE = [Log(U)-Log(HR)]/1.96
Hepatocellular$sei <- ((log(Hepatocellular$Upper.CI)-Hepatocellular$yi)/1.96)

#Use generic inverse variance meta-analysis to calculate pooled HR from
#log(HR) and standard error
## inverse variance weighting is used for pooling results
  
#create a function which conducts a meta-analysis and constructs forest plot when
#given meta-analysis ID number as function(ID)

meta.write <- function(ID) {
  Hep <- metagen(TE = Hepatocellular$yi, seTE = Hepatocellular$sei, data = NULL, studlab = Hepatocellular$Author, 
                 subset=(Hepatocellular$ID==ID), sm ="HR", hakn=gs("hakn"), method.tau = "REML",
                 prediction = TRUE, comb.random = TRUE)

forest.meta(Hep, studlab = TRUE, 
            comb.random = Hep$comb.random, 
            comb.fixed = Hep$comb.fixed,
            hakn=gs("hakn"),       #create a forest plot using hakn=gs("hakn") to call the
                                   #Hartung and Knapp method to adjust confidence intervals
            method.tau="REML",     #utilise method.tau="REML" to call the Restricted
                                   #maximum-likelihood estimator to estimate between study variance
                                   #as tau^2
            text.random = "Pooled HR by Random Effects",
            text.fixed = "Pooled HR by Fixed Effects", 
            lyt.random = TRUE, 
            lyt.fixed = TRUE, 
            type.study="square", 
            type.fixed="diamond", 
            type.random="diamond", 
            col.study="black", 
            col.square="lightskyblue3", 
            col.inside="black", 
            col.diamond="lightskyblue3", 
            col.diamond.lines="dodgerblue4", 
            col.predict="dodgerblue4", 
            col.predict.lines="dodgerblue4", 
            col.by="dodgerblue4", 
            col.label.right="black",
            col.label.left="black", 
            hetlab = "Heterogeneity: ", 
            print.I2 = Hep$comb.fixed | Hep$comb.random, 
            print.I2.ci = TRUE, 
            print.tau2 = FALSE, 
            print.Q = TRUE, 
            print.Rb = FALSE, 
            print.Rb.ci = FALSE)
return(Hep)
}

#create a function which creates a funnel plot when given meta-analysis constructed
#by function meta.write

auto.funnel <- function(Hep){
  funnel <- funnel.meta(Hep, pch=1,
              col.fixed = "dodgerblue4",
              col.random = "lightskyblue3",
              backtransf = FALSE)
  return(funnel)
}

#for arcsin test described in Rücker et al 2008, set sm from "HR" to "ASD"
meta.arc <- function(ID) {
  HepASD <- metagen(TE = Hepatocellular$yi, seTE = Hepatocellular$sei, data = NULL, studlab = Hepatocellular$Author, 
                    subset=(Hepatocellular$id==ID), sm ="ASD", hakn=gs("hakn"), method.tau = "REML",
                    prediction = TRUE, comb.random = TRUE, label.left="ES")
  return(HepASD)
}

#create a function which conducts metabias when given meta-analysis constructed
#by function meta.write
#set method.bias to mm for the Arc-Thompson test, which performed best
#in simulations at takes account of heterogeneity
auto.asymmetry <- function(HepASD){
  bias <- metabias(HepASD, method.bias = "mm", 
                   plotit = TRUE, correct = FALSE,
                   k.min=2)
  return(bias)
}

pdf(file = "Hepatocellular/HepForest%03d.pdf", width=9, height=11, onefile=FALSE)
forest.plots <- lapply(1:12, meta.write)
dev.off()

pdf(file = "Hepatocellular/HepFunnel%03d.pdf", width=7, height=, onefile=FALSE)
funnel.plots <- lapply(forest.plots, auto.funnel)
dev.off()

pdf(file = "Hepatocellular/HepBias%03d.pdf", width=7, height=6, onefile=FALSE)
test.bias <- lapply(forest.plots, auto.asymmetry)
dev.off()


#write to excel
library(openxlsx)
path_template <- file.path("All_results Template.xlsx")
wb <- loadWorkbook(path_template)


for (i in 1:12) {
  outputs <- c(forest.plots[[i]][["TE.fixed"]], forest.plots[[i]][["lower.fixed"]], 
               forest.plots[[i]][["upper.fixed"]], forest.plots[[i]][["pval.fixed"]], 
               forest.plots[[i]][["TE.random"]], forest.plots[[i]][["lower.random"]], 
               forest.plots[[i]][["upper.random"]], forest.plots[[i]][["pval.random"]], 
               forest.plots[[i]][["lower.predict"]], forest.plots[[i]][["upper.predict"]],
               forest.plots[[i]][["Q"]], forest.plots[[i]][["pval.Q"]], forest.plots[[i]][["tau"]], 
               forest.plots[[i]][["se.tau2"]], forest.plots[[i]][["I2"]], forest.plots[[i]][["lower.I2"]],
               forest.plots[[i]][["upper.I2"]])
  outputs <- t(outputs)
  writeData(wb, outputs, sheet = "1", startRow = (i+1), startCol = 2, colNames = FALSE)
}

for (i in 1:12) {
  sig.bias <- test.bias[[i]][["p.value"]]
  writeData(wb, sig.bias, sheet = "1", startRow = (i+1), startCol = 19, colNames = FALSE)
}

path_output <- file.path("Hep_results.xlsx")
saveWorkbook(wb, file = path_output, overwrite = FALSE)

##########################################################
#     Figures for data visualisation and exploration     #
##########################################################

#load ggplot2
library(ggplot2)

#make Site categorical to bin by site on boxplot x axis
Hepatocellular$Site <- as.character(Hepatocellular$Site)
typeof(Hepatocellular$Site)
typeof(Hepatocellular$HR)

#Boxplots
#create boxplot in ggplot2
box <- ggplot(data=Hepatocellular, aes(x=Site, y=HR), 
              xlab="Site", ylab="Random Effects Hazard Ratio Estimate", 
              main="Boxplot of Hazard Ratio by Cancer Diagnosis") + 
  geom_boxplot(color="dodgerblue4", fill="lightskyblue3", outlier.color = NA,  
               alpha=0.5 )  +  coord_cartesian(ylim = c(0, 10)) + geom_jitter(alpha = 0.5, 
                                                                              color = "lightskyblue3") + theme_bw()
box
ggsave("Hepatocellular/HepBoxSite.pdf")

#plot by site on Y
box_out <- ggplot(data=Hepatocellular, aes(x=Outcome, y=HR), 
                  xlab="Outcome", ylab="Random Effects Hazard Ratio Estimate", 
                  main="Boxplot of Hazard Ratio by Cancer Diagnosis") + 
  geom_boxplot(color="dodgerblue4", fill="lightskyblue3", outlier.color = NA,  
               alpha=0.5 ) + coord_cartesian(ylim = c(0, 10)) + geom_jitter(alpha = 0.5, 
                                         color = "lightskyblue3") + theme_bw()
box_out
ggsave("Hepatocellular/HepBoxOutcome.pdf")

#Clean work space
remove(list = ls())
