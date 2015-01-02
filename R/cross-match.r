

# Read names of DwarfArchives stars
names <- read.table("DwAr.names")

# Read their coordinates and transform to degrees for cross-match
a <- read.table("coordinates.txt")
ra1deg <- (a[,1]+a[,2]/60+a[,3]/3600)*(360/24)
dec1deg <- (a[,4]+a[,5]/60+a[,5]/3600)
# Read in casagrande's table
b <- read.table("casagrandeMdwarfs.csv", skip=66,sep=";")
ra2deg <- b[,15] 
dec2deg <- b[,16]


idx <- a[,1]-a[,1]
dist <- a[,1]-a[,1]
match.flag <- rep(FALSE,dim(a)[1])
match.flag.hip <- rep(FALSE,dim(a)[1])
# These are the DwarfArchives stars that must be found in Casagrande's parameters
hipparcos <- c(
               "J01432015+0419172",
               "J02050492-1736528",
               "J08550761+0132472",
               "J09360161-2139371",
               "J10121768-0344441",
               "J10285555+0050275",
               "J12245243-1814303",
               "J12475664+0945050",
               "J19323790+0034390",
               "J22021026+0124006",
               "J22094029-0438267"
               )

# Find nearest neighbour to each DwarfArchive star 
for (i in 1:dim(a)[1])
  {
    dists <- sqrt((ra1deg[i]-ra2deg)^2+(dec1deg[i]-dec2deg)^2)*3600
    idx[i] <- which.min(dists) 
    dist[i] <- dists[idx[i]]
# Accept match only if distance is 5500 arc seconds    
    if (dists[idx[i]] < 5500) match.flag[i]=TRUE    
    if (dists[idx[i]] < 5500 & sum(names[i,2] == hipparcos) > 0) match.flag.hip[i]=TRUE    
#    if (dists[idx[i]] < 600) match.flag[i]=TRUE    
#    if (dists[idx[i]] < 600 & sum(names[i,2] == hipparcos) > 0) match.flag.hip[i]=TRUE    
  }

# Print names of matches to check that we have all hipparcos stars
print(names[match.flag,c(1,2)]) 

# Load Casagrande's data 
load("tbr.RData")

ind <- which(match.flag)
matrix <- matrix(NA,nrow=length(ind), ncol=20)
j <- 0
for (i in ind)
  {
    j <- j+1
    idp <- grep(names[i,2],tbr[,1],fixed=TRUE)
    if (length(idp > 1))
      {
        if (length(idp) > 1) matrix <- rbind(matrix,matrix(NA,nrow=(length(idp)-1),ncol=20))
        for (m in 0:(length(idp)-1))
          {
            l <- 0
            for (k in c(1,5,6,7,8,13,14,15,16,17,18,19,20,21,23,25,26,27))
              {
                l <- l+1
                matrix[j+m,l] <- tbr[idp[m+1],k]
              }
            matrix[j+m,l+1] <- b[idx[i],2]
            matrix[j+m,l+2] <- b[idx[i],13]
          }
      }
  }

plot(as.numeric(matrix[,18]), as.numeric(matrix[,20]),xlim=c(-1,1),ylim=c(-1,.5),xlab="Our M/H prediction",ylab="Casatella's M/H")
abline(0,1)
points(as.numeric(matrix[,15]), as.numeric(matrix[,20]),pch=16,col="red")
points(as.numeric(matrix[,13]), as.numeric(matrix[,20]),pch=16,col="blue")






plot(ra1deg,dec1deg,xlim=c(0,360),ylim=c(-90,90))
points(ra2deg,dec2deg,pch=16,col="blue")
points(ra1deg[match.flag==TRUE],dec1deg[match.flag==TRUE],pch=16,col="red")
