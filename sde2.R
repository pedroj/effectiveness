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
# NOT-Log scaled axes. Data example for Prunus mahaleb.
#
require(calibrate)
# Input data here.
sde <- read.table("./effectiveness/data.txt", header=T, sep="\t", dec=".", na.strings="NA")

# Variables in dataset:
# dataset	plant	animal	visits	prop_visits	eff_per_vis	eff_total	
# prop_disp_service	frugivore_species
plot(sde$visits,sde$eff_per_vis,    # Empty plot
	 xlab="Visit rate",
     ylab="No. fruits/visit (total handled)", 
     main="Quantitative component",
	 ylim=c(0,max(sde$eff_per_vis)), 
	 xlim=c(0,max(sde$visits)),type="n")

points(sde$visits,sde$eff_per_vis,  # Adds the points
       pch=c(17,17,2,2,2,2,2,6,6,15,15,17,5,5,5,5,5,5,5,5)[sde$group])

#
# Code for legend. REVISE
legend("topright", title="Functional group",
       c("Large birds","Thrushes","Warblers","Small muscicapids","Others"),
       pch=c(17,15,6,2,5),horiz=F,ncol=1)
# 
# This plots the isolines (code prototype by Bernardo Santos.)
#
nlines <- 10 # number of isolines wanted
alfa <- max(sde$eff_per_vis)/max(sde$visits) # slope of a straight line linking (left,bottom) to (right,above) corners of the graphic
xval <- seq(0, max(sde$visits), length.out=(nlines+1))[2:(nlines+1)] # sequence of (nlines) regular spaced x values for the isoclines
isoc <- (xval*xval*alfa) # values of the isoclines

vis1<-seq(0,2,length.out=1000)
for(i in 1:nlines)
{
  lines(vis1, isoc[i]/vis1)
  text(0.9*max(sde$visits), isoc[i]/(0.9*max(sde$visits)), paste("QC = ", round(isoc[i], digits=1)), col="red")
}

#----------------
# Isolines of QC
# Vary the numeric values in the lines statements to adjust the location of 
# the isolines
qnc1<-seq(0, max(sde$visits),length.out=100)
qlc1<-seq(0, max(sde$eff_per_vis),length.out=100)
#lines(qnc1,50/qnc1,col="gray")
lines(qnc1,100/qnc1,col="gray")
lines(qnc1,500/qnc1,col="gray")
lines(qnc1,1000/qnc1,col="gray")
lines(qnc1,2000/qnc1,col="gray")
lines(qnc1,5000/qnc1,col="gray")
# This adds the labels for the isolines
#text(650,0.076, "PE= 50", cex=0.8, pos=4, col="red")
text(650,0.15385, "PE= 100", cex=0.8, pos=4, col="red")
text(650,0.76923, "PE= 500", cex=0.8, pos=4, col="red")
text(650,1.5385, "PE= 1000", cex=0.8, pos=4, col="red")
text(650,3.0769, "PE= 2000", cex=0.8, pos=4, col="red")
text(650,7.6923, "PE= 5000", cex=0.8, pos=4, col="red")
#--------------------------------------------------------------------------


