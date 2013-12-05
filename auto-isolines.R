# ----------------------------------------------------------------
# autoisolines: Code for automatically plotting isolines of
# effectiveness landscapes.
# Based on code for plotting effectiveness landscapes by Pedro 
# Jordano and code for automatic calculation of isolines 
# by Bernardo Santos.
# 3 December 2013. UNESP, Rio Claro, Brazil. Pedro Jordano.
# ----------------------------------------------------------------
## First version 12 Jan 2009. Revised 3 December 2013
# ----------------------------------------------------------------
# DESCRIPTION:
# The script plots effectiveness landscapes as described in
# Schupp, E. W., Jordano, P. and Gómez, J.M. 2010. Seed dispersal
# effectiveness revisited: a conceptual review. New Phytologist
# 188: 333-353.
# ----------------------------------------------------------------
# Notes to do
# - Adding a function for the isolines code.
# - Implementing an option for setting the number of desired 
# isolines.
# ----------------------------------------------------------------
# NOT-Log scaled axes. Data example for Archontophoenix 
# cunninghamiani.
#
# Input data here.
sde <- read.table("sde.txt", header=T, sep="\t", dec=".", na.strings="NA")

plot(sde$vish,sde$frutot,    # Empty plot
	 xlab="Visit rate",
     ylab="No. fruits/visit (total handled)", 
     main="Quantitative component",
	 ylim=c(0,max(sde$frutot)), 
	 xlim=c(0,max(sde$vish)),type="n")

points(sde$vish,sde$frutot,  # Adds the points
       pch=c(1,2,4,6,7)[sde$fam])
#
# Code for legend. REVISE
#legend("topleft", title="Family",
# c("Mimidae","Psittacidae","Thraupidae","Turdidae","Tyrannidae"), pch=c(1,2,4,6,7),horiz=F,ncol=1)
# 
# This plots the isolines (code prototype by Bernardo Santos.)
#
nlines <- 10 # number of isolines wanted
alfa <- max(sde$frutot)/max(sde$vish) # slope of a straight line linking (left,bottom) to (right,above) corners of the graphic
xval <- seq(0, max(sde$vish), length.out=(nlines+1))[2:(nlines+1)] # sequence of (nlines) regular spaced x values for the isoclines
isoc <- (xval*xval*alfa) # values of the isoclines

vis1<-seq(0,2,length.out=1000)
for(i in 1:nlines)
{
  lines(vis1, isoc[i]/vis1)
  text(0.9*max(sde$vish), isoc[i]/(0.9*max(sde$vish)), paste("QC = ", round(isoc[i], digits=1)), col="red")
}

