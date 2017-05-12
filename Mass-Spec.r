require(gdata)
require(ggplot2)
require(plotly)
require(plyr)
require(d3heatmap)
require(reshape2)
require(RColorBrewer)
require(heatmaply)
library(scales)

cols<-brewer.pal(11,"Spectral")
t<-read.xls("cmce_volcano.xlsx",header=T)

x
y
colour
scale
logx
logy

labels
nudge_x
nudge_y

g<-ggplot(t,aes(y=Other,x=AVG.Log.Ratio,text=Genes,colour=Qvalue))+geom_point()+theme_bw()+
  geom_text(aes(label=ifelse(Other>200,as.character(Genes),'')),nudge_y =10)

l<-c("rpb1","rpb2","pfk1")
g<-ggplot(t,aes(y=Other,x=AVG.Log.Ratio,text=Genes,colour=Qvalue))+geom_point()+theme_bw()+
  geom_text(aes(label=ifelse(Genes %in% l,as.character(Genes),'')),nudge_y =10)

ggplotly(g)
a<-"X"
b<-"sample.name"
c<-"Total.Area"
m<-read.csv("h3.csv",header=T)
mm<-dcast(m, list(b,a),value.var = c,fun.aggregate = sum)
mn<-cbind(sample=mm[,1],mm[,-1]/rowSums(mm[,-1]))
mmelt<-melt(mn)

g<-ggplot(mmelt,aes(x=sample,y=value,fill=variable))+geom_bar(stat="identity",position="dodge")+
  theme_dark()+theme(text = element_text(size=12),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_viridis(discrete=T,guide=F)+xlab("Sample name")+
  ylab("Normalised proportion")+facet_wrap(~variable,scales="free")
ggsave(g,device = "png",filename = "sample.png")

g2<-ggplot(m,aes(x=sample.name,y=Total.Area,fill=X))+geom_bar(stat="identity",position="fill")+
  theme_dark()+theme(text = element_text(size=12),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_viridis(discrete=T)+xlab("Sample name")+
  ylab("Normalised proportion")
ggsave(g2,device = "png",filename = "sample2.png")

g3<-ggplot(mmelt,aes(x=sample,y=variable,fill=value))+geom_tile(colour="black")+
  theme_minimal()+theme(text = element_text(size=12),axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradientn(name="Normalised proportion",colours = brewer.pal(9,"Purples"))+xlab("Sample name")+geom_text(aes(label=round(value,digits = 4)))+ylab("e")
g3
