data=read.csv(file="F:/final/SE.csv",header=T,dec=".")

library(rJava)
library(Rwordseg)

test= data    #��ȡ����
res = test[test != " "]      #��ȡtest�����޳�test=�� ��

#���ۣ�1
#�ؼۣ�2
#������3
#��  ��4

#��  ��1
#ȥ  ��2
#��  ��3
#-   ��4
#��  ��5

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
  }else if(seg[1]=="��"){
    data1[i,1]="����"
    if(nchar(seg[3])<2){
      data1[i,2]=seg[4]
    }else{
      data1[i,2]=seg[3]
    }
  }else if(seg[1]=="��"){
    data1[i,1]="�������"
    if((nchar(seg[1])+nchar(seg[2])+nchar(seg[3]))==4){
      if(nchar(seg[4])==1){
        data1[i,2]=seg[5]
      }else{
        data1[i,2]=seg[4]
      }
      
    }else{data1[i,2]=seg[6]}
  }
  
  ###�Ե��������
  if(data1[i,2]=="��"|data1[i,2]=="��"){data1[i,2]="����"}
  if(data1[i,2]=="��"|data1[i,2]=="��"){data1[i,2]="�������"}
  if(data1[i,2]=="��"){data1[i,2]="����"}
  if(data1[i,2]=="��"){data1[i,2]=seg[4]}
  if(data1[i,2]=="����"){data1[i,2]=seg[3]}
  if(data1[i,2]=="����"){data1[i,2]="���"}
  if(data1[i,2]=="��"|data1[i,2]=="��"){data1[i,2]="����"}
  if(data1[i,2]=="��"){data1[i,2]="���嵺"}
  if(data1[i,2]=="��"){data1[i,2]="�׶���˹"}
  if(data1[i,2]=="����"|data1[i,2]=="��"|data1[i,2]=="��"){data1[i,2]="������"}
  if(data1[i,2]=="��"|data1[i,2]=="��"){data1[i,2]="��±�"}
  if(data1[i,2]=="��"){data1[i,2]="�ɶ�"}
  if(data1[i,2]=="־"|data1[i,2]=="��"){data1[i,2]="��־����"}
  if(data1[i,2]=="��"|data1[i,2]=="��"){data1[i,2]="���"}
  if(data1[i,2]=="��"|data1[i,2]=="��"){data1[i,2]="�ٲ�"}
  if(data1[i,2]=="��"|data1[i,2]=="ɼ"){data1[i,2]="��ɼ��"}
  if(data1[i,2]=="��"){data1[i,2]="��˹��"}
  if(data1[i,2]=="Ŧ"|data1[i,2]=="��"){data1[i,2]="Ŧ��˹��"}
  if(data1[i,2]=="��"){data1[i,2]="�Ϻ�"}
  if(data1[i,2]=="��"){data1[i,2]="����"}
  if(data1[i,2]=="��"){data1[i,2]="��ˮ"}  
  if(data1[i,2]=="�ؼ�"){data1[i,2]="ӡ��"}  
  if(data1[i,2]=="����"){data1[i,2]="����"}  
  if(data1[i,2]=="����"){data1[i,2]="����"} 
  if(data1[i,2]=="�ռ�"){data1[i,2]="�ռ���"} 
  
  if(grepl("����",data[i,1]))
  {
    data1[i,3]=1
  }else if(grepl("�ؼ�",data[i,1])){
    data1[i,3]=2
  }else if(grepl("�ۿ�",data[i,1])){
    data1[i,3]=3
  }else if(grepl("����",data[i,1])){
    data1[i,3]=3
  }else if(grepl("�ͼ�",data[i,1])){
    data1[i,3]=3
  }else if(grepl("�Ż�",data[i,1])){
    data1[i,3]=3
  }else if(grepl("�ػ�",data[i,1])){
    data1[i,3]=3
  }else{data1[i,3]=4}
  
  
  
  if(grepl("��",data[i,1]))
  {
    data1[i,4]=1
  }else if(grepl("ȥ",data[i,1])){
    data1[i,4]=2
  }else if(grepl("��",data[i,1])){
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

#�Ե��������
  
data1[30141,2]="��ɽ"
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




