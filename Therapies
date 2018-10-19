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
#       Meta-analysis of Therapies Cancers          #
##########################################################

#Clean work space
remove(list = ls())

datfile = "Neutrophils.xlsx"
#Call "read.xls" to read the specific Excel data sheet
Therapies <- read.xls(datfile, sheet="Therapies", perl="/usr/bin/perl")

#Effect sizes == LogHR
#Calculate log HR
Therapies$yi <- with(Therapies, log(HR))

#back transform 95% CIs to find standard error
#SE = [Log(U)-Log(HR)]/1.96
Therapies$sei <- ((log(Therapies$Upper.CI)-Therapies$yi)/1.96)

#Use generic inverse variance meta-analysis to calculate pooled HR from
#log(HR) and standard error
## inverse variance weighting is used for pooling results
  
#create a function which conducts a meta-analysis and constructs forest plot when
#given meta-analysis ID number as function(ID)

meta.write <- function(ID) {
  Tx <- metagen(TE = Therapies$yi, seTE = Therapies$sei, data = NULL, studlab = Therapies$Author, 
                 subset=(Therapies$ID==ID), sm ="HR", hakn=gs("hakn"), method.tau = "REML",
                 prediction = TRUE, comb.random = TRUE)

forest.meta(Tx, studlab = TRUE, 
            comb.random = Tx$comb.random, 
            comb.fixed = Tx$comb.fixed,
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
            print.I2 = Tx$comb.fixed | Tx$comb.random, 
            print.I2.ci = TRUE, 
            print.tau2 = FALSE, 
            print.Q = TRUE, 
            print.Rb = FALSE, 
            print.Rb.ci = FALSE)
return(Tx)
}

#create a function which creates a funnel plot when given meta-analysis constructed
#by function meta.write

auto.funnel <- function(Tx){
  funnel <- funnel.meta(Tx, pch=1,
              col.fixed = "dodgerblue4",
              col.random = "lightskyblue3",
              backtransf = FALSE)
  return(funnel)
}

#create a function which conducts metabias when given meta-analysis constructed
#by function meta.write

auto.asymmetry <- function(Tx){
  bias <- metabias(Tx, method.bias = "linreg", 
                   plotit = TRUE, correct = FALSE,
                   k.min=2)
  return(bias)
}

pdf(file = "Therapy/TxForest%03d.pdf", width=9, height=11, onefile=FALSE)
forest.plots <- lapply(1:11, meta.write)
dev.off()

pdf(file = "Therapy/TxFunnel%03d.pdf", width=7, height=, onefile=FALSE)
funnel.plots <- lapply(forest.plots, auto.funnel)
dev.off()

pdf(file = "Therapy/TxBias%03d.pdf", width=7, height=6, onefile=FALSE)
test.bias <- lapply(forest.plots, auto.asymmetry)
dev.off()


##########################################################
#     Figures for data visualisation and exploration     #
##########################################################

#load ggplot2
library(ggplot2)

#make Site categorical to bin by site on boxplot x axis
Therapies$Site <- as.character(Therapies$Site)
typeof(Therapies$Site)
typeof(Therapies$HR)

#Boxplots
#create boxplot in ggplot2
box <- ggplot(data=Therapies, aes(x=Site, y=HR), 
              xlab="Site", ylab="Random Effects Hazard Ratio Estimate", 
              main="Boxplot of Hazard Ratio by Cancer Diagnosis") + 
  geom_boxplot(color="dodgerblue4", fill="lightskyblue3", outlier.color = NA,  
               alpha=0.5 )  +  coord_cartesian(ylim = c(0, 10)) + geom_jitter(alpha = 0.5, 
                                                                              color = "lightskyblue3") + theme_bw()
box
ggsave("Therapy/TxBoxSite.pdf")

#plot by site on Y
box_out <- ggplot(data=Therapies, aes(x=Outcome, y=HR), 
                  xlab="Outcome", ylab="Random Effects Hazard Ratio Estimate", 
                  main="Boxplot of Hazard Ratio by Cancer Diagnosis") + 
  geom_boxplot(color="dodgerblue4", fill="lightskyblue3", outlier.color = NA,  
               alpha=0.5 ) + coord_cartesian(ylim = c(0, 10)) + geom_jitter(alpha = 0.5, 
                                         color = "lightskyblue3") + theme_bw()
box_out
ggsave("Therapy/TxBoxOutcome.pdf")

#Clean work space
remove(list = ls())
