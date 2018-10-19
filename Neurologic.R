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
#         Meta-analysis of Neurological Cancers           #
##########################################################

#Clean work space
remove(list = ls())

datfile = "Neutrophils.xlsx"
#Call "read.xls" to read the specific Excel data sheet
Neurological <- read.xls(datfile, sheet="Neurological", perl="/usr/bin/perl")

#Effect sizes == LogHR
#Calculate log HR
Neurological$yi <- with(Neurological, log(HR))

#back transform 95% CIs to find standard error
#SE = [Log(U)-Log(HR)]/1.96
Neurological$sei <- ((log(Neurological$Upper.CI)-Neurological$yi)/1.96)

#Use generic inverse variance meta-analysis to calculate pooled HR from
#log(HR) and standard error
## inverse variance weighting is used for pooling Neuults
  
#create a function which conducts a meta-analysis and constructs foNeut plot when
#given meta-analysis ID number as function(ID)

meta.write <- function(ID) {
  Neu <- metagen(TE = Neurological$yi, seTE = Neurological$sei, data = NULL, studlab = Neurological$Author, 
                 subset=(Neurological$ID==ID), sm ="HR", hakn=gs("hakn"), method.tau = "REML",
                 prediction = TRUE, comb.random = TRUE)

forest.meta(Neu, studlab = TRUE, 
            comb.random = Neu$comb.random, 
            comb.fixed = Neu$comb.fixed,
            hakn=gs("hakn"),       #create a foNeut plot using hakn=gs("hakn") to call the
                                   #Hartung and Knapp method to adjust confidence intervals
            method.tau="REML",     #utilise method.tau="REML" to call the Neutricted
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
            print.I2 = Neu$comb.fixed | Neu$comb.random, 
            print.I2.ci = TRUE, 
            print.tau2 = FALSE, 
            print.Q = TRUE, 
            print.Rb = FALSE, 
            print.Rb.ci = FALSE)
return(Neu)
}

#create a function which creates a funnel plot when given meta-analysis constructed
#by function meta.write

auto.funnel <- function(Neu){
  funnel <- funnel.meta(Neu, pch=1,
              col.fixed = "dodgerblue4",
              col.random = "lightskyblue3",
              backtransf = FALSE)
  return(funnel)
}

#create a function which conducts metabias when given meta-analysis constructed
#by function meta.write

auto.asymmetry <- function(Neu){
  bias <- metabias(Neu, method.bias = "linreg", 
                   plotit = TRUE, correct = FALSE,
                   k.min=2)
  return(bias)
}

pdf(file = "Neurological/NeuForest%03d.pdf", width=9, height=11, onefile=FALSE)
foNeut.plots <- lapply(1:5, meta.write)
dev.off()

pdf(file = "Neurological/NeuFunnel%03d.pdf", width=7, height=, onefile=FALSE)
funnel.plots <- lapply(foNeut.plots, auto.funnel)
dev.off()

pdf(file = "Neurological/NeuBias%03d.pdf", width=7, height=6, onefile=FALSE)
test.bias <- lapply(foNeut.plots, auto.asymmetry)
dev.off()


##########################################################
#     FiguNeu for data visualisation and exploration     #
##########################################################

#load ggplot2
library(ggplot2)

#make Site categorical to bin by site on boxplot x axis
Neurological$Site <- as.character(Neurological$Site)
typeof(Neurological$Site)
typeof(Neurological$HR)

#Boxplots
#create boxplot in ggplot2
box <- ggplot(data=Neurological, aes(x=Site, y=HR), 
              xlab="Site", ylab="Random Effects Hazard Ratio Estimate", 
              main="Boxplot of Hazard Ratio by Cancer Diagnosis") + 
  geom_boxplot(color="dodgerblue4", fill="lightskyblue3", outlier.color = NA,  
               alpha=0.5 )  +  coord_cartesian(ylim = c(0, 10)) + geom_jitter(alpha = 0.5, 
                                                                              color = "lightskyblue3") + theme_bw()
box
ggsave("Neurological/NeuBoxSite.pdf")

#plot by site on Y
box_out <- ggplot(data=Neurological, aes(x=Outcome, y=HR), 
                  xlab="Outcome", ylab="Random Effects Hazard Ratio Estimate", 
                  main="Boxplot of Hazard Ratio by Cancer Diagnosis") + 
  geom_boxplot(color="dodgerblue4", fill="lightskyblue3", outlier.color = NA,  
               alpha=0.5 ) + coord_cartesian(ylim = c(0, 10)) + geom_jitter(alpha = 0.5, 
                                         color = "lightskyblue3") + theme_bw()
box_out
ggsave("Neurological/NeuBoxOutcome.pdf")

#Clean work space
remove(list = ls())
