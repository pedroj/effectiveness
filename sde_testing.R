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
plot(sde$visits,sde$eff_per_vis,    # Empty plot
    xlab="Visit rate (vis/10h)",
    ylab="No. fruits/visit (total handled)", 
    main="Quantitative component",
    ylim=c(0,max(sde$eff_per_vis)), 
    xlim=c(0,max(sde$visits)),type="n")

points(sde$visits,sde$eff_per_vis,  # Adds the points
       pch=c(17,17,2,2,2,2,2,6,6,15,15,17,5,5,5,5,5,5,5,5)[sde$group])

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
for(i in 1:nlines)
{
    lines(vis1, isoc[i]/vis1)
    text(0.6*max(sde$visits), isoc[i]/(0.6*max(sde$visits)), 
         paste("QC = ", round(isoc[i], digits=1)), 
         col="red", cex= 0.75)
}
#
# Code for legend. Ok.
legend("topright", title="Functional group",
    c("Large birds","Thrushes","Warblers","Small muscicapids","Others"),
    pch=c(17,15,6,2,5), horiz=F, ncol=1)
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Code with ggplot2
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

#---------------------------------------------------------------------------
pp<- as.data.frame(vis1) # Build dataset for within loop plot.
for(i in 1:nlines)
{
    pp<- cbind(pp, isoc[i]/vis1)
}    

# Main plot ----------------------------------------------------------------
require(devtools)
require(ggplot2)
# mytheme_bw.R
devtools::source_gist("https://gist.github.com/b843fbafa3af8f408972")
#
p1<- ggplot(sde, aes(x=visits, y=eff_per_vis)) + 
    geom_point(shape=sde$group, size=5) +
    geom_text(size=4, label=sde$animal,hjust=0.5, vjust=1.9) +
    mytheme_bw()

# Adding isolines
labelx<- rep(0.8*max(sde$visits), nlines)
labely<- as.vector(t(pp[800,2:11]))

for(i in 1:nlines+1){ 
    #labely<- isoc[i]/(0.8*max(sde$eff_per_vis)
#    labely<- pp[,i][800]
    p1= p1 + geom_line(aes(x, y), 
        data= data.frame(x= pp$vis1, y= pp[,i]), 
        col="blue", size = 0.25, alpha= 0.6) + 
        ylim(0, max(sde$eff_per_vis)) +
        xlab("Visit rate (/10h)") + 
        ylab("Effectiveness/visit (No. fruits handled/vis)")  # +
#        geom_text(aes(), data= NULL, x= labelx, y= labely, 
#            label = paste("QC = ", round(isoc[i], digits=1)),
#            size = 4, colour = "red")
}
p1 + annotate("text", x= labelx, y= labely,
        label=paste("QC= ", round(isoc,1)), 
        size=4, colour="red", hjust=0) 
#---------------------------------------------------------------------------


