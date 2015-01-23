#---------------------------------------------------------------------------
# autoisolines: Code for automatically plotting isolines of
# effectiveness landscapes.
# Based on code for plotting effectiveness landscapes by Pedro 
# Jordano and code for automatic calculation of isolines 
# by Bernardo Santos.
# 3 December 2013. UNESP, Rio Claro, Brazil. Pedro Jordano.
#---------------------------------------------------------------------------
## First version 12 Jan 2009. Revised 3 December 2013
#---------------------------------------------------------------------------
# DESCRIPTION:
# The script plots effectiveness landscapes as described in
# Schupp, E. W., Jordano, P. and Gómez, J.M. 2010. Seed dispersal
# effectiveness revisited: a conceptual review. New Phytologist
# 188: 333-353.
#---------------------------------------------------------------------------
# Notes to do
# - Adding a function for the isolines code.
# - Implementing an option for setting the number of desired 
# isolines.
# Implementing ggplot2 graph.
# - Checking with other datasets.
#---------------------------------------------------------------------------
# NOT-Log scaled axes. Data example for Prunus mahaleb.
#
# Input data here.
sde <- read.table("data.txt", header=T, sep="\t", dec=".", na.strings="NA")

# Variables in dataset:
# dataset	plant	animal	visits	prop_visits	eff_per_vis	eff_total	
# prop_disp_service	frugivore_species

# 
# This plots the isolines (code prototype by Bernardo Santos.)
#
nlines <- 10 # number of isolines wanted
# slope of a straight line linking (left,bottom) to (right,above) 
# corners of the graphic
alfa <- max(sde$eff_per_vis)/max(sde$visits)

# sequence of (nlines) regular spaced x values for the isoclines
xval <- seq(0, max(sde$visits), 
            length.out=(nlines+1))[2:(nlines+1)] 
isoc <- (xval*xval*alfa) # values of the isoclines
vis1<-seq(0,max(sde$visits),length.out=1000)

pp<- as.data.frame(vis1)
for(i in 1:nlines)
{
    pp<- cbind(pp, isoc[i]/vis1)
}

# The plot
#p1<- ggplot(sde, aes(visits, eff_per_vis)) + 
p1<- ggplot(sde, aes(visits, eff_per_vis)) +
            geom_point(shape=sde$group, size=5)+ 
            geom_text(label=sde$animal, size=4, hjust=0.5, vjust=2) +
            ylim(0, max(sde$eff_per_vis)) +
            xlab("Visit rate (vis/10h)") + 
            ylab("No. fruits/visit (total handled)")
p1
for(i in 2:nlines){
p1<-    p1 +   geom_line(data=pp, aes(vis1, pp[,i]), col="blue")
}
print(p1)


#---------------------------------------------------------------------------
# Code for legend. Ok.
# legend("topright", title="Functional group",
#     c("Large birds","Thrushes","Warblers","Small muscicapids","Others"),
#     pch=c(17,15,6,2,5), horiz=F, ncol=1)
# text(0.6*max(sde$visits), isoc[i]/(0.6*max(sde$visits)), 
#     paste("QC = ", round(isoc[i], digits=1)), 
#     col="red", cex= 0.75)

#---------------------------------------------------------------------------


