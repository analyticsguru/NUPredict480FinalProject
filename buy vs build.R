#### BUY VERSUS BUILD ####
##########################################################################################################################
## #--------------------------------------------------------------------------------------------------
## read in data: 
setwd("C:/Users/Kathleen/new R folder/480")
da <- read.csv("dataset.csv", header = TRUE, strip.white = TRUE, na.strings = "EMPTY") 

dim(da) #17 4
head(da)   

#year   component   component_name  external internal
#1 2012         1              CPU       42       39
#2 2012         2       Video card       30       31
#3 2012         6            Wi-Fi       67       66
#4 2012         7       sound card       91       77
#5 2012         8 drive controller       10       11
#6 2012         9        DVR drive       75       60

external=da$external
internal=da$internal
component=da$component

external

class(external) #integer
class(component) #integer

best=c(16)

for (i in 1:16) {
  
  best[i]<-ifelse(external[i]>internal[i],internal[i],external[i])
} 

# tdx=c(1:572)/12+1968 # create the time index, use when comparing years
#tdx
par(mar=c(6,4,4,4),font.axis=4, cex.axis=1.5)
par(oma=c(2,2,2,2))
plot(component,external,type='l',cex=0.5,col = 4,main="Buy versus build - 2012",xaxt="n",pch=18,xlim=c(1,22),ylim=c(10,95), labels = FALSE,xlab="Components",ylab="Dollar value")
#for(j in 1:4) for(i in 0:10) mtext(as.character(i),side=j,line=i)
lines(component,internal, type = "l", lwd=1,pch=18,  col = 3)#black
points(component,best,  pch=18,col = 2)#red

lablist<-da$component_name
axis(1, at=seq(1, 15, by=1), labels = FALSE)
text(seq(1, 16, by=1), par("usr")[3] - 0.1, line=3,labels = lablist, srt = 90, pos = 1,las = 1, xpd = TRUE,cex.axis=0.7, tck=-.001,cex=.7)


par(xpd=TRUE)
legend(16,90,legend=c("External","Best value","Internal"),fill=c("blue", "red", "green"))
axis(2, at=seq(20, 90, by=10), labels = c("$20","$30","$40","$50","$60","$70","$80","$90"),cex.axis=0.7, tck=-.001,cex=.7)



