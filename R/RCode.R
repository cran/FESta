StdEffort<- function (sp_catch,tot_catch,effort,meg)
{
sp_catch1<-sp_catch[ ,-1]
tot_catch1<-tot_catch[ ,-1]
effort1<-effort[ ,-1]

k<-0
for(i in 1:ncol(effort1))
  {
     k[i]<-min(effort1[ ,i][which (effort1[,i]>0)])
  }

#replacing zero with lowest effort
for(i in 1:ncol(effort1))
  for(j in 1:nrow(effort1))
  {
    if (is.na(effort1[j,i]))
    {
      effort1[j,i]<-k[i]
    }
  }

#catch Proportion
catch_prop=sp_catch1/tot_catch1

count<-0
for(i in 1:ncol(catch_prop))
{
count[i]<-length(catch_prop[ ,i][!is.na(catch_prop[ ,i])])
}


avg<-0
std<-0
wt<-0
for(i in 1:ncol(catch_prop))
{
avg[i]<-mean(catch_prop[ ,i], na.rm=TRUE) #removing missing values
std[i]<-var(catch_prop[ ,i], na.rm=TRUE)*((count[i]-1)/count[i]) #removing missing values
}
cv<-avg/(std+1)
sm=sum(cv)

for(i in 1:ncol(catch_prop))
{
  wt[i]<-cv[i]/sm
}

stde<-catch_prop
for(i in 1:ncol(catch_prop))
{
  for(j in 1:nrow(catch_prop))
    {
    stde[j,i]<- wt[i]*catch_prop[j,i]*effort1[j,i]
  }
}

catchPerHour<-tot_catch1/effort1

avg_hour<-0

for(i in 1:ncol(catchPerHour))
{
  avg_hour[i]<-mean(catchPerHour[ ,i], na.rm=TRUE) #removing missing values
}
if (meg==FALSE)
{
fact<-min(avg_hour)
}
else
{
fact<-max(avg_hour)
}

factor<-avg_hour/fact

gear_Names<-as.vector(colnames(stde))
for(i in 1:ncol(stde))
{
  if (factor[i]==1)
  {
    STD_Unit<-gear_Names[i]
  }
}

names(STD_Unit)<-paste("Total_Effort interms of the following Gear Units:")

std_effort<-stde
for(i in 1:ncol(catch_prop))
{
  for(j in 1:nrow(catch_prop))
  {
    std_effort[j,i]<-stde[j,i]*factor[i]
  }
}

std_effort[is.na(std_effort)]<-0
year<-sp_catch[ ,1]

Total_Catch<-rowSums(sp_catch1, na.rm = TRUE)
Total_Effort<-rowSums(std_effort, na.rm = TRUE)
CPUE<-Total_Catch/Total_Effort

opar <- par("mfrow", "mar")
on.exit(par(opar))
par(mar=c(5.1, 7.1, 4.1, 6.1))
## Plot first set of data and draw its axis
plot(year,Total_Catch, pch=16, axes=FALSE, xlab="", ylab="",
     type="b",col="black", main="Catch and CPUE")
axis(2,col="black",las=1,)  ## las=1 makes horizontal labels
mtext("Species Catch",side=2,line=4)
box()

## Allow a second plot on the same graph
par(new=TRUE)
## Plot the second plot and put axis scale on right
plot(year, CPUE, pch=15,  xlab="", ylab="",
     axes=FALSE, type="b", col="red")

## a little farther out (line=4) to make room for labels
mtext("Catch Per Unit Effort",side=4,col="red",line=3.5)
axis(4, col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,year)
mtext("Years",side=1,col="black",line=2.5)

Std_fishing_effort<-cbind(year,Total_Effort,Total_Catch,CPUE)
#write.table(Std_fishing_effort,file="Standardised fishing effort.csv",sep=",",row.names=F,col.names=T)
Result<-list(Std_fishing_effort,STD_Unit)
return(Result)

}

