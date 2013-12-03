#-------------------------------------------------------------------------- 
# Plots of Effectiveness Landscapes. 
# See:
# Schupp, E.W., Jordano, P. & Gomez, J.M. (2010). Seed dispersal 
# effectiveness revisited: a conceptual review. New Phytol, 188, 333–353.
<<<<<<< HEAD
# From New Phytologist MS code. Nov 2009. Pedro Jordano. Sevilla. 
#--------------------------------------------------------------------------
# Quant. component as product of two variables: visits, flowers/vis
# We use our experimental data, from field work
qnc<-PE.5000$visit*PE.5000$nflow 
=======
# From New Phytologist MS code. Nov 2009. Pedro Jordano. Sevilla.
#--------------------------------------------------------------------------
# Quant. component as product of two variables: visits, flowers/vis
# We use our experimental data, from field work
qnc<-PE.5000$visit*PE.5000$nflow
>>>>>>> f690ca85b5e98e7c858a79f707b5ace8a800585b
tapply(qnc,Pollinator,mean) # Below we just copy the summary data
Gallotia.qnc<-0.027771945
GAdult.qnc<-0.007938871
GJuv.qnc<-0.019856327
Phy.qnc<-0.345377267

# Qual. components as product of two variables: % pollen removal and 
# % viable seed set
qlc<-(PE.5000$pprem/100)*(PE.5000$pvseed/100)
tapply(qlc,Pollinator,mean)
Gallotia.qlc<-0.07018447
GAdult.qlc<-0.15816336
GJuv.qlc<-0.01618308
Phy.qlc<-0.10957918
 # PE estimate
pe<-qnc*qlc
tapply(pe,Pollinator,mean)
Gallotia.pe<-0.0019468677
GAdult.pe<-0.0012539678
GJuv.pe<-0.0003211828
Phy.pe<-0.0378527826

# Estimate the SD for qnc and qlc per pollinator type:
tapply(qnc,Pollinator,sd)
Gallotia.qnc.sd<-0.010355862
GAdult.qnc.sd<-0.003269856
GJuv.qnc.sd<-0.008188137
Phy.qnc.sd<-0.043216248
tapply(qlc,Pollinator,sd)
Gallotia.qlc.sd<-0.02211062
GAdult.qlc.sd<-0.05102973
GJuv.qlc.sd<-0.01005910
Phy.qlc.sd<-0.02559917

# NOT-Log scaled axes. I keep plotting the different species
# This is the actual plot of the PE (or SDE) landscape.
plot(PE.5000$qnc,PE.5000$qlc,type="n",xlab="Quantitative component",
     ylab="Qualitative component", main="Pollinator effectiveness",
     xlim=c(-0.001,1),ylim=c(-0.001,0.3),
     cex.lab=1.3,cex.main=1.5)

# Now we add the data points
#Gallotia galloti pooled:
points(Gallotia.qnc,Gallotia.qlc,pch=c(17),cex=2)
arrows(Gallotia.qnc,Gallotia.qlc,Gallotia.qnc-Gallotia.qnc.sd,Gallotia.qlc,
       length=.05,angle=90,code=3)
arrows(Gallotia.qnc,Gallotia.qlc,Gallotia.qnc+Gallotia.qnc.sd,Gallotia.qlc,
       length=.05,angle=90,code=3)
arrows(Gallotia.qnc,Gallotia.qlc,Gallotia.qnc,Gallotia.qlc-Gallotia.qlc.sd,
       length=.05,angle=90,code=3)
arrows(Gallotia.qnc,Gallotia.qlc,Gallotia.qnc,Gallotia.qlc+Gallotia.qlc.sd,
       length=.05,angle=90,code=3)
#---
#Gallotia adults:
points(GAdult.qnc,GAdult.qlc,pch=c(2),cex=2)
arrows(GAdult.qnc,GAdult.qlc,GAdult.qnc-GAdult.qnc.sd,GAdult.qlc,
       length=.05,angle=90,code=3)
arrows(GAdult.qnc,GAdult.qlc,GAdult.qnc+GAdult.qnc.sd,GAdult.qlc,
       length=.05,angle=90,code=3)
arrows(GAdult.qnc,GAdult.qlc,GAdult.qnc,GAdult.qlc-GAdult.qlc.sd,
       length=.05,angle=90,code=3)
arrows(GAdult.qnc,GAdult.qlc,GAdult.qnc,GAdult.qlc+GAdult.qlc.sd,
       length=.05,angle=90,code=3)
#---
#Gallotia juveniles:
points(GJuv.qnc,GJuv.qlc,pch=c(6),cex=2)
arrows(GJuv.qnc,GJuv.qlc,GJuv.qnc-GJuv.qnc.sd,GJuv.qlc,
       length=.05,angle=90,code=3)
arrows(GJuv.qnc,GJuv.qlc,GJuv.qnc+GJuv.qnc.sd,GJuv.qlc,
       length=.05,angle=90,code=3)
arrows(GJuv.qnc,GJuv.qlc,GJuv.qnc,GJuv.qlc-GJuv.qlc.sd,
       length=.05,angle=90,code=3)
arrows(GJuv.qnc,GJuv.qlc,GJuv.qnc,GJuv.qlc+GJuv.qlc.sd,
       length=.05,angle=90,code=3)
#---
#Phylloscopus canariensis:
points(Phy.qnc,Phy.qlc,pch=c(15),cex=2)
arrows(Phy.qnc,Phy.qlc,Phy.qnc-Phy.qnc.sd,Phy.qlc,
       length=.05,angle=90,code=3)
arrows(Phy.qnc,Phy.qlc,Phy.qnc+Phy.qnc.sd,Phy.qlc,
       length=.05,angle=90,code=3)
arrows(Phy.qnc,Phy.qlc,Phy.qnc,Phy.qlc-Phy.qlc.sd,
       length=.05,angle=90,code=3)
arrows(Phy.qnc,Phy.qlc,Phy.qnc,Phy.qlc+Phy.qlc.sd,
       length=.05,angle=90,code=3)
#--------------------------------------------------------------------------
# Legend added
legend("topright", inset=.02, title="Pollinator species",
       c("Gallotia galloti","G. galloti adults",
         "G. galloti Juveniles","Phylloscopus canariensis"), 
          pch=c(17,2,6,15),ncol=1)
#----------------
# This is the code chunk for plotting the isolines. Needs tweaking of the 
# numeric values for each particular case. We are working to improve this…
#----------------
# Isolines of QC
# Vary the numeric values in the lines statements to adjust the location of 
# the isolines
qnc1<-seq(0,1.5,length.out=100)
qlc1<-seq(0,0.081,length.out=100)
        lines(qnc1,0.07/qnc1,col="gray")
        lines(qnc1,0.0335/qnc1,col="gray")
        lines(qnc1,0.0167/qnc1,col="gray")
        lines(qnc1,0.0034/qnc1,col="gray")
        lines(qnc1,0.0007/qnc1,col="gray")
# This adds the labels for the isolines
text(0.8,0.08, "PE= 0.07", cex=0.8, pos=4, col="red")
text(0.8,0.036, "PE= 0.03", cex=0.8, pos=4, col="red")
text(0.8,0.018, "PE= 0.01", cex=0.8, pos=4, col="red")
text(0.8,0.007, "PE= 0.005", cex=0.8, pos=4, col="red")
text(0.8,0.00, "PE< 0.001", cex=0.8, pos=4, col="red")
#--------------------------------------------------------------------------
detach(PE.5000)
rm(list=ls())
#--------------------------------------------------------------------------
