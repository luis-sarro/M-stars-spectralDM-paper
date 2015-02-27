load("BT-Settl-2011-IRTF.RData")

n <- length(bf_clean)
names <- rep("name",n)

for (i in 1:n) names[i] <- bf_clean[[i]]$starname

k <- length(bp_clean)

plt <- function(name){
color <- 1
color_transparent <- adjustcolor(color, alpha.f = 0.1)
plot(bf_clean[[1]]$data[[1]],ty="l",ylim=c(0,2e-4),col=color_transparent,main=name)

for (i in 2:n) lines(bf_clean[[i]]$data[[1]],col=color_transparent)
}

grd <- NULL

for (i in 1:length(bp_clean))
{
  grd <- rbind(grd,c(bp_clean[[i]]$stellarp))
}

load("../sptype-teff/Teffs.RData")
#HD35601
star <- which(grepl("HD35601",names))

for (i in 1:length(star)) print(bf_clean[[star[1]]]$name)
#Input
new <- data.frame(x = 11.5)
teff <- predict.lm(m,new,se.fit=TRUE)
print(teff[1])
#Input
idcs <- which(grd[,1]==3600 & grd[,3]==0)

plt("HD35601")
for (i in 1:length(star)) lines(bf_clean[[star[i]]]$data[[1]],col=i+2)
for (i in 1:length(idcs)) lines(bp_clean[[idcs[i]]]$data[[1]],col=2)

# HD339034

star <- which(grepl("HD339034",names))

for (i in 1:length(star)) print(bf_clean[[star[1]]]$name)
#Input
new <- data.frame(x = 11)
teff <- predict.lm(m,new,se.fit=TRUE)
print(teff[1])
#Input
idcs <- which(grd[,1]==3700 & grd[,3]==0)

plt("HD339034")
for (i in 1:length(star)) lines(bf_clean[[star[i]]]$data[[1]],col=i+2)
for (i in 1:length(idcs)) lines(bp_clean[[idcs[i]]]$data[[1]],col=2)

# HD69243

star <- which(grepl("HD69243",names))

for (i in 1:length(star)) print(bf_clean[[star[1]]]$name)
#Input
new <- data.frame(x = 19)
teff <- predict.lm(m,new,se.fit=TRUE)
print(teff[1])
#Input
idcs <- which(grd[,1]==2000 & grd[,3]==0)

plt("HD69243")
for (i in 1:length(star)) lines(bf_clean[[star[i]]]$data[[1]],col=i+2)
for (i in 1:length(idcs)) lines(bp_clean[[idcs[i]]]$data[[1]],col=2)

# IRAS01037+1219

star <- which(grepl("IRAS01037",names))

for (i in 1:length(star)) print(bf_clean[[star[1]]]$name)
#Input
new <- data.frame(x = 18)
teff <- predict.lm(m,new,se.fit=TRUE)
print(teff[1])
#Input
idcs <- which(grd[,1]==2000 & grd[,3]==0)

plt("IRAS01037+1219")
for (i in 1:length(star)) lines(bf_clean[[star[i]]]$data[[1]],col=i+2)
for (i in 1:length(idcs)) lines(bp_clean[[idcs[i]]]$data[[1]],col=2)

# HD14386

star <- which(grepl("HD14386",names))

for (i in 1:length(star)) print(bf_clean[[star[1]]]$name)
#Input
new <- data.frame(x = 15)
teff <- predict.lm(m,new,se.fit=TRUE)
print(teff[1])
#Input
idcs <- which(grd[,1]==2500 & grd[,3]==0)

plt("HD14386")
for (i in 1:length(star)) lines(bf_clean[[star[i]]]$data[[1]],col=i+2)
for (i in 1:length(idcs)) lines(bp_clean[[idcs[i]]]$data[[1]],col=2)

# Gl752B

star <- which(grepl("Gl752B",names))

for (i in 1:length(star)) print(bf_clean[[star[1]]]$name)
#Input
new <- data.frame(x = 18)
teff <- predict.lm(m,new,se.fit=TRUE)
print(teff[1])
#Input
idcs <- which(grd[,1]==2700 & grd[,3]==0)

plt("Gl752B-2700")
for (i in 1:length(star)) lines(bf_clean[[star[i]]]$data[[1]],col=i+2)
for (i in 1:length(idcs)) lines(bp_clean[[idcs[i]]]$data[[1]],col=2)

# LP412-31

star <- which(grepl("LP412-31",names))

for (i in 1:length(star)) print(bf_clean[[star[1]]]$name)
#Input
new <- data.frame(x = 18)
teff <- predict.lm(m,new,se.fit=TRUE)
print(teff[1])
#Input
idcs <- which(grd[,1]==2300 & grd[,3]==0)

plt("LP412-31")
for (i in 1:length(star)) lines(bf_clean[[star[i]]]$data[[1]],col=i+2)
for (i in 1:length(idcs)) lines(bp_clean[[idcs[i]]]$data[[1]],col=2)

# BRIB0021-0214

star <- which(grepl("BRIB0021",names))

for (i in 1:length(star)) print(bf_clean[[star[i]]]$name)
#Input
new <- data.frame(x = 19.5)
teff <- predict.lm(m,new,se.fit=TRUE)
print(teff[1])
#Input
idcs <- which(grd[,1]==2500 & grd[,3]==0)

plt("BRIB0021-0214")
for (i in 1:length(star)) lines(bf_clean[[star[i]]]$data[[1]],col=i+2)
for (i in 1:length(idcs)) lines(bp_clean[[idcs[i]]]$data[[1]],col=2)
