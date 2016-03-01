dm1<<-read.csv(file="F:/final/distance/dm1.csv",header=T)
dm2<<-read.csv(file="F:/final/distance/dm2.csv",header=T)
dm3<<-read.csv(file="F:/final/distance/dm3.csv",header=T)
data=read.csv(file="F:/final/seg.csv",header=T,dec=".")
data=data[,-1]
dm1<<-dm1[,-1]
dist=matrix(nrow=3000,ncol=3000)

data2=matrix(nrow=30311,ncol=4)
for(i in 1:30311)
{
  
  data2[i,1]= paste(data[i,1],"-", data[i,2])
  data2[i,2]= data[i,3]
  data2[i,3]=data[i,4]
  data2[i,4]=data[i,5]
}

library(parallel)
cl = makeCluster(getOption("cl.cores",4))

fun=function(data2)
{
  dist=matrix(nrow=3000,ncol=3000)
  dm1<<-read.csv(file="F:/final/distance/dm1.csv",header=T)
  dm2<<-read.csv(file="F:/final/distance/dm2.csv",header=T)
  dm3<<-read.csv(file="F:/final/distance/dm3.csv",header=T)
  dm1<<-dm1[,-1]
for(i in 1:30)
{
  for(j in 1:i)
  {
    flight=0
    go=0
    adj=0
    num=matrix(nrow=3,ncol=2)
    num[1,1]=which(dm1$line==data2[i,1])
    num[1,2]=which(dm1$line==data2[j,1])
    num[2,1]=which(dm2$X==data2[i,2])
    num[2,2]=which(dm2$X==data2[j,2])
    num[3,1]=which(dm3$X==data2[i,3])
    num[3,2]=which(dm3$X==data2[j,3])
    flight=dm1[num[1,1],num[1,2]]
    go=dm2[num[2,1],num[2,2]]
    adj=dm3[num[3,1],num[3,2]]
    dist[i,j]=flight+go+adj
  }
  
}
  return(dist)
}
system.time({res = parLapply(cl,data2,fun)})

fill=matrix(nrow=30311,ncol=3)
for(i in 1:30311)
{
  
  fill[i,1]=which(dm1$line==data2[i,1])
  fill[i,2]=which(dm2$X==data2[i,2])
  fill[i,3]=which(dm3$X==data2[i,3])

  
}

data3=cbind(data2,fill)

write.csv(data3, file="F:/final/distance/data3.csv")

library(HiPLARM)



