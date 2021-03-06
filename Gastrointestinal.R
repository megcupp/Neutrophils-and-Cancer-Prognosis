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
#         Meta-analysis of Gastrointestinal cancers         #
##########################################################

#Clean work space
remove(list = ls())

datfile = "Neutrophils.xlsx"
#Call "read.xls" to read the specific Excel data sheet
Gastrointestinal <- read.xls(datfile, sheet="Gastrointestinal", perl="/usr/bin/perl")

#Effect sizes == LogHR
#Calculate log HR
Gastrointestinal$yi <- with(Gastrointestinal, log(HR))


Gastrointestinal$Upper.CI <- as.numeric(as.character(Gastrointestinal$Upper.CI))


#back transform 95% CIs to find standard error
#SE = [Log(U)-Log(HR)]/1.96
Gastrointestinal$sei <- ((log(Gastrointestinal$Upper.CI)-Gastrointestinal$yi)/1.96)

#Use generic inverse variance meta-analysis to calculate pooled HR from
#log(HR) and standard error
## inverse variance weighting is used for pooling results
  
#create a function which conducts a meta-analysis and constructs forest plot when
#given meta-analysis ID number as function(ID)

meta.write <- function(ID) {
  Gas <- metagen(TE = Gastrointestinal$yi, seTE = Gastrointestinal$sei, data = NULL, studlab = Gastrointestinal$Author, 
                 subset=(Gastrointestinal$ID==ID), sm ="HR", hakn=gs("hakn"), method.tau = "REML",
                 prediction = TRUE, comb.random = TRUE)

forest.meta(Gas, studlab = TRUE, 
            comb.random = Gas$comb.random, 
            comb.fixed = Gas$comb.fixed,
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
            print.I2 = Gas$comb.fixed | Gas$comb.random, 
            print.I2.ci = TRUE, 
            print.tau2 = FALSE, 
            print.Q = TRUE, 
            print.Rb = FALSE, 
            print.Rb.ci = FALSE)
return(Gas)
}

#create a function which creates a funnel plot when given meta-analysis constructed
#by function meta.write

auto.funnel <- function(Gas){
  funnel <- funnel.meta(Gas, pch=1,
              col.fixed = "dodgerblue4",
              col.random = "lightskyblue3",
              backtransf = FALSE)
  return(funnel)
}

#create a function which conducts metabias when given meta-analysis constructed
#by function meta.write

auto.asymmetry <- function(Gas){
  bias <- metabias(Gas, method.bias = "linreg", 
                   plotit = TRUE, correct = FALSE,
                   k.min=2)
  return(bias)
}

pdf(file = "Gastrointestinal/GasForest%03d.pdf", width=9, height=11, onefile=FALSE)
forest.plots <- lapply(1:8, meta.write)
dev.off()

pdf(file = "Gastrointestinal/GasFunnel%03d.pdf", width=7, height=, onefile=FALSE)
funnel.plots <- lapply(forest.plots, auto.funnel)
dev.off()

pdf(file = "Gastrointestinal/GasBias%03d.pdf", width=7, height=6, onefile=FALSE)
test.bias <- lapply(forest.plots, auto.asymmetry)
dev.off()


##########################################################
#     Figures for data visualisation and exploration     #
##########################################################

#load ggplot2
library(ggplot2)

#make Site categorical to bin by site on boxplot x axis
Gastrointestinal$Site <- as.character(Gastrointestinal$Site)
typeof(Gastrointestinal$Site)
typeof(Gastrointestinal$HR)

#Boxplots
#create boxplot in ggplot2
box <- ggplot(data=Gastrointestinal, aes(x=Site, y=HR), 
              xlab="Site", ylab="Random Effects Hazard Ratio Estimate", 
              main="Boxplot of Hazard Ratio by Cancer Diagnosis") + 
  geom_boxplot(color="dodgerblue4", fill="lightskyblue3", outlier.color = NA,  
               alpha=0.5 )  +  coord_cartesian(ylim = c(0, 60)) + geom_jitter(alpha = 0.5, 
                                                                              color = "lightskyblue3") + theme_bw()
box
ggsave("Gastrointestinal/GasBoxSite.pdf")

#plot by site on Y
box_out <- ggplot(data=Gastrointestinal, aes(x=Outcome, y=HR), 
                  xlab="Outcome", ylab="Random Effects Hazard Ratio Estimate", 
                  main="Boxplot of Hazard Ratio by Cancer Diagnosis") + 
  geom_boxplot(color="dodgerblue4", fill="lightskyblue3", outlier.color = NA,  
               alpha=0.5 ) + coord_cartesian(ylim = c(0, 7.5)) + geom_jitter(alpha = 0.5, 
                                         color = "lightskyblue3") + theme_bw()
box_out
ggsave("Gastrointestinal/GasBoxOutcome.pdf")

#Clean work space
remove(list = ls())
