data=read.csv(file="F:/final/SE.csv",header=T,dec=".")

library(rJava)
library(Rwordseg)

test= data    #读取数据
res = test[test != " "]      #读取test，且剔除test=“ ”

#打折：1
#特价：2
#其他：3
#无  ：4

#到  ：1
#去  ：2
#至  ：3
#-   ：4
#无  ：5

data1=matrix(nrow=30315,ncol=5)
data2=matrix(nrow=30311,ncol=4)

for(i in 1:30315)
{
  seg=segmentCN(res[i])

  
  if(nchar(seg[2])<2)
  {
    data1[i,2]=seg[3]
  }else{
    data1[i,2]=seg[2]
  }
  if(nchar(seg[1])>=2)
  {
    data1[i,1]=seg[1]
  }else if(seg[1]=="万"){
    data1[i,1]="万州"
    if(nchar(seg[3])<2){
      data1[i,2]=seg[4]
    }else{
      data1[i,2]=seg[3]
    }
  }else if(seg[1]=="香"){
    data1[i,1]="香格里拉"
    if((nchar(seg[1])+nchar(seg[2])+nchar(seg[3]))==4){
      if(nchar(seg[4])==1){
        data1[i,2]=seg[5]
      }else{
        data1[i,2]=seg[4]
      }
      
    }else{data1[i,2]=seg[6]}
  }
  
  ###对到达的修正
  if(data1[i,2]=="万"|data1[i,2]=="州"){data1[i,2]="万州"}
  if(data1[i,2]=="香"|data1[i,2]=="格"){data1[i,2]="香格里拉"}
  if(data1[i,2]=="无"){data1[i,2]="无锡"}
  if(data1[i,2]=="飞"){data1[i,2]=seg[4]}
  if(data1[i,2]=="飞往"){data1[i,2]=seg[3]}
  if(data1[i,2]=="飞天"){data1[i,2]="天津"}
  if(data1[i,2]=="大"|data1[i,2]=="邱"){data1[i,2]="大邱"}
  if(data1[i,2]=="厘"){data1[i,2]="巴厘岛"}
  if(data1[i,2]=="白"){data1[i,2]="白俄罗斯"}
  if(data1[i,2]=="到底"|data1[i,2]=="特"|data1[i,2]=="底"){data1[i,2]="底特律"}
  if(data1[i,2]=="哥"|data1[i,2]=="德"){data1[i,2]="哥德堡"}
  if(data1[i,2]=="都"){data1[i,2]="成都"}
  if(data1[i,2]=="志"|data1[i,2]=="胡"){data1[i,2]="胡志明市"}
  if(data1[i,2]=="里"|data1[i,2]=="尔"){data1[i,2]="里尔"}
  if(data1[i,2]=="临"|data1[i,2]=="沧"){data1[i,2]="临沧"}
  if(data1[i,2]=="洛"|data1[i,2]=="杉"){data1[i,2]="洛杉矶"}
  if(data1[i,2]=="明"){data1[i,2]="明斯特"}
  if(data1[i,2]=="纽"|data1[i,2]=="卡"){data1[i,2]="纽卡斯尔"}
  if(data1[i,2]=="上"){data1[i,2]="上海"}
  if(data1[i,2]=="亚"){data1[i,2]="三亚"}
  if(data1[i,2]=="泗"){data1[i,2]="泗水"}  
  if(data1[i,2]=="特价"){data1[i,2]="印度"}  
  if(data1[i,2]=="阳光"){data1[i,2]="仰光"}  
  if(data1[i,2]=="往返"){data1[i,2]="北京"} 
  if(data1[i,2]=="普吉"){data1[i,2]="普吉岛"} 
  
  if(grepl("打折",data[i,1]))
  {
    data1[i,3]=1
  }else if(grepl("特价",data[i,1])){
    data1[i,3]=2
  }else if(grepl("折扣",data[i,1])){
    data1[i,3]=3
  }else if(grepl("便宜",data[i,1])){
    data1[i,3]=3
  }else if(grepl("低价",data[i,1])){
    data1[i,3]=3
  }else if(grepl("优惠",data[i,1])){
    data1[i,3]=3
  }else if(grepl("特惠",data[i,1])){
    data1[i,3]=3
  }else{data1[i,3]=4}
  
  
  
  if(grepl("到",data[i,1]))
  {
    data1[i,4]=1
  }else if(grepl("去",data[i,1])){
    data1[i,4]=2
  }else if(grepl("至",data[i,1])){
    data1[i,4]=3
  }else if(regexpr("-",data[i,1])==(nchar(data1[i,1])+1)){
    data1[i,4]=4
  }else{
    data1[i,4]=5
  }
  
  data1[i,5]=data[i,2]
  data2[i,1]= paste(data1[i,1]," - ", data1[i,2])
  data2[i,2]= data1[i,3]
  data2[i,3]=data1[i,4]
  data2[i,4]=data1[i,5]
  
}

#对到达的修正
  
data1[30141,2]="舟山"
data1=data1[-c(22634:22637),]

for(i in 1:30311)
{

  data2[i,1]= paste(data1[i,1]," - ", data1[i,2])
  data2[i,2]= data1[i,3]
  data2[i,3]=data1[i,4]
  data2[i,4]=data1[i,5]
}






write.csv(data1,file = "F:/final/seg.csv")


for(i in 1:30315){
  seg=data1[i,]
  if(nchar(seg[1])<2|seg[2]<2)
  {
    print(seg)
  }
  
}





