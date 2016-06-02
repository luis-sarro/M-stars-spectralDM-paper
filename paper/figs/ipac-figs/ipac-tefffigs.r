
load("LSB_Tdata_plot_IPAC.RData")

lc2 <- 15

#############################################
# Teffs Validation

pdf("../ipac-teff.pdf",width=8,height=5)
par(cex.axis=1.0)
nrows = 2
ncols = 2
nplots = nrows*ncols
windim = c(60,30) 
mardim=7
layoutmat = matrix(c(1,2,3,4),nrows,ncols, byrow=T)
nf = layout(layoutmat,respect=T,
            widths=c(windim[1],windim[1]),
            heights=c(windim[2],windim[2]+mardim)
)

par(mar = c(0,5,1,0))
plot(log10(df_T_50$Chi2_50),df_T_50$T_teo,pch=lc2,cex=0.5,col="red",
     axes=F,xlab="",ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(3.9,3.25))
box()
text(3.4,0,paste("GA-",expression(chi^2),"-50",sep=""),pos=4,cex=0.7)
axis(2)

par(mar = c(0,0,1,4))
plot(log10(df_T_50$PLS),df_T_50$T_teo,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(3.9,3.25))
box()
axis(4)
text(3.4,0,"GA-PLS-50",pos=4,cex=0.7)

par(mar = c(4,5,0,0))
plot(df_T_50$T_teo,log10(df_T_50$RF),pch=lc2,cex=0.5,col="blue",axes=F,
     xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), c(3.9,3.25))
box()
axis(1)
axis(2)
text(3.4,0,"GA-RF-50",pos=4,cex=0.7)
box()
#
par(mar = c(4,0,0,4))
#plot(df_T_50$T_teo,log10(df_T_10$RF_Ces),pch=lc2,cex=0.5,col="blue",axes=F,
plot(df_T_50$T_teo,log10(df_T_10$GB_Ces),pch=lc2,cex=0.5,col="blue",axes=F,
          xlab=expression(log(T[eff-spt])),
     ylab=expression(log(T[eff-GBM])),cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(3.9,3.25))
box()
axis(1)
axis(4)
text(3.4,0,"ICA-10",pos=4,cex=0.7)
box()
axis(1)

dev.off()

#############################################
# Teffs vs logg

load("../irtf-figs/irtf.Rdata")

teffirtf <- df_T_inf$KNN
loggirtf <- df_G$NNR_50
load("LSB_Tdata_plot_IPAC.RData")
load("~/Escritorio/M-stars-spectralDM-paper/paper/figs/ipac-figs/Gdata_plot.RData")

lc2 <- 16
lc <- 15

pdf("../ipac-teff-logg.pdf",width=8,height=5)
par(cex.axis=1.0)
nrows = 2
ncols = 2
nplots = nrows*ncols
windim = c(60,30) 
mardim=7
layoutmat = matrix(c(1,2,3,4),nrows,ncols, byrow=T)
nf = layout(layoutmat,respect=T,
            widths=c(windim[1],windim[1]),
            heights=c(windim[2],windim[2]+mardim)
)

par(mar = c(0,5,1,0))
plot(log10(df_T_10$PLS),df_G$RF_10,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
text(3.4,0,"GA-RF-10",pos=4,cex=0.7)
axis(2)

par(mar = c(0,0,1,4))
plot(log10(df_T_10$PLS),df_G$GB_10,pch=lc2,cex=0.5,col="red",axes=F,xlab="",
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(4)
text(3.4,0,"GA-GB-10",pos=4,cex=0.7)

par(mar = c(4,5,0,0))
plot(log10(df_T_10$PLS),df_G$Chi2_50,pch=lc2,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(1)
axis(2)
text(3.4,0,"GA-NNR-50",pos=4,cex=0.7)
box()
#
par(mar = c(4,0,0,4))
plot(log10(df_T_10$PLS),df_G$Chi2_inf,pch=lc2,cex=0.5,col="blue",axes=F,xlab=expression(log(T[eff])),
     ylab="log(g)",cex.lab=1.5, xlim=c(3.9,3.25), ylim=c(6,-1))
points(log10(teffirtf),loggirtf,cex=.5,pch=lc)
box()
axis(1)
axis(4)
text(3.4,0,"ICA-10",pos=4,cex=0.7)
box()
axis(1)

dev.off()

