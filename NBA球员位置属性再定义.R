library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(corrplot)

setwd("D://狗熊会/人才计划/Final Project")
dat0<- read.csv("NBA_data2.csv",stringsAsFactors = FALSE)
dat0<-unique(dat0)
dat0=subset(dat0,dat0$场次>=21)

pts1 <- ggplot(dat0,aes(x=reorder(位置,得分),y=得分))+
  geom_boxplot(fill=c("#EF3B2C","#EF3B2C","#2171B5","#2171B5","#2171B5" ))+
  scale_fill_brewer(palette = "Blues")+
  labs(x="",y="得分")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
pts1

# brewer.pal(9,"Blues")

ast <- ggplot(dat0,aes(x=位置,y=助攻))+
  geom_boxplot(fill=c("#EF3B2C","#2171B5","#2171B5","#2171B5","#EF3B2C" ))+
  scale_fill_brewer(palette = "Reds")+
  labs(x="",y="助攻数")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
ast

reb <- ggplot(dat0,aes(x=位置,y=防守篮板))+
  geom_boxplot(fill=c("#EF3B2C","#2171B5","#2171B5","#2171B5","#EF3B2C" ))+
  scale_fill_brewer(palette = "Reds")+
  labs(x="",y="防守篮板")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
reb

per <- ggplot(dat0,aes(x=位置,y=命中率))+
  geom_boxplot(fill=c("#EF3B2C","#2171B5","#2171B5","#2171B5","#EF3B2C" ))+
  scale_fill_brewer(palette = "Reds")+
  labs(x="",y="命中率")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
per
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pts1, vp = vplayout(1, 1))
print(ast, vp = vplayout(1, 2))
print(reb, vp = vplayout(2, 1))
print(per, vp = vplayout(2, 2))

dri_pts <- ggplot(dat0,aes(x=位置,y=突破得分))+
  geom_boxplot(fill=c("#EF3B2C","#2171B5","#2171B5","#2171B5","#EF3B2C" ))+
  labs(x="",y="突破得分")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
dri_pts

brewer.pal(9,"Reds")

cat_pts <- ggplot(dat0,aes(x=位置,y=接球投篮得分))+
  geom_boxplot(fill=c("#EF3B2C","#2171B5","#2171B5","#2171B5","#EF3B2C" ) )+
  labs(x="",y="接球投篮得分")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
cat_pts

pul_pts <- ggplot(dat0,aes(x=位置,y=急停跳投得分))+
  geom_boxplot(fill=c("#EF3B2C","#2171B5","#2171B5","#2171B5","#EF3B2C" ))+
  labs(x="",y="急停跳投得分")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
pul_pts

thr_pts <- ggplot(dat0,aes(x=位置,y=三秒区得分))+
  geom_boxplot(fill=c("#EF3B2C","#2171B5","#2171B5","#2171B5","#EF3B2C" ))+
  labs(x="",y="三秒区得分")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
thr_pts

pai_pts <- ggplot(dat0,aes(x=位置,y=低位触球得分))+
  geom_boxplot(fill="#EF3B2C")+
  labs(x="",y="低位触球区得分")
pai_pts
brewer.pal(9,"Reds")
elb_pts <- ggplot(dat0,aes(x=位置,y=顶区得分))+
  geom_boxplot(fill=c("#EF3B2C","#2171B5","#2171B5","#2171B5","#EF3B2C" ))+
  labs(x="",y="顶区得分")+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
  
elb_pts

pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(dri_pts, vp = vplayout(1, 1))
print(thr_pts, vp = vplayout(2, 1))
print(pul_pts, vp = vplayout(1, 2))
print(elb_pts, vp = vplayout(2, 2))

########################降维
d_pca=dat0[,-c(1:8,9)]
cor1 <- cor(d_pca)
corrplot(cor1,tl.cex=0.7,tl.col= "black")
library(mvstats)
install.packages("mvstats")
library(psych)
stone <- fa.parallel(d_pca,fa='pc',n.iter = 100,show.legend = FALSE)
data_pca <- factpc(d_pca,2,rotation="varimax")	
data_pca
data_pca$Vars	
cbind(round(data_pca$loadings,2),round(data_pca$common,2))
myscore=data_pca$scores
temp1=dat0[order(myscore[,1],decreasing=T),]
temp1[1:10,1]

temp2=dat0[order(myscore[,2],decreasing=T),]
temp2[1:10,1]

install.packages("ggfortify")
dat0 <- cbind(dat0,myscore)
colnames(dat0)[c(26:27)] <- c("进攻因子","防守因子")


library(factoextra)
fviz_nbclust(dat0[,c(26:27)],kmeans,method="wss")+geom_vline(xintercept=4,linetype=2) 
library(ggfortify)
rownames(dat0)=dat0$姓名
k=kmeans(dat0[,c(26:27)],3)
k$size
autoplot(kmeans(dat0[,c(26:27)],3),data=dat0[,c(26:27)],frame.type="norm",color=k$cluster)+
  # geom_point(fill=k$cluster)+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
d3=cbind(dat0,k$cluster)
colnames(d3)[28]="类别"
#write.csv(d3,"result3.csv")

d3.1=subset(d3,d3$类别==1)
d3.11=as.data.frame(table(d3.1$位置))
d3.11["类别"]="内线攻城锤"

d3.2=subset(d3,d3$类别==2)
d3.21=as.data.frame(table(d3.2$位置))
d3.21["类别"]="超级攻击手"

d3.3=subset(d3,d3$类别==3)
d3.31=as.data.frame(table(d3.3$位置))
d3.31["类别"]="移动炮台手"

d4=rbind(d3.11,d3.21,d3.31)
p4=ggplot(d4,aes(x=类别,y=Freq,fill=Var1))+
  geom_bar(stat= 'identity', position = 'stack',width=0.5)+
  scale_fill_brewer(palette = 'Blues')+
  theme(legend.position="top")+
  labs(x="",y="频次")+
  guides(fill=guide_legend(title=NULL))+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
p4
brewer.pal(9,"Blues")
pts2 <- ggplot(d3,aes(x=as.factor(类别),y=得分))+
  geom_boxplot(fill=c( "#C6DBEF" ,"#6BAED6" ,"#2171B5" ))+
  labs(x="",y="得分")+
  scale_x_discrete(breaks=c("1", "2", "3"),
                   labels=c("外线高射炮", "超级得分手", "内线攻城锤"))+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
pts2

thr2 <- ggplot(d3,aes(x=as.factor(类别),y=三秒区得分))+
  geom_boxplot(fill=c( "#C6DBEF" ,"#6BAED6" ,"#2171B5" ))+
  labs(x="",y="三秒区得分")+
  scale_x_discrete(breaks=c("1", "2", "3"),
                   labels=c("外线高射炮", "超级得分手", "内线攻城锤"))+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
thr2

reb2 <- ggplot(d3,aes(x=as.factor(类别),y=防守篮板+进攻篮板))+
  geom_boxplot(fill=c( "#C6DBEF" ,"#6BAED6" ,"#2171B5" ))+
  labs(x="",y="篮板")+
  scale_x_discrete(breaks=c("1", "2", "3"),
                   labels=c("外线高射炮", "超级得分手", "内线攻城锤"))+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
reb2

ast2 <- ggplot(d3,aes(x=as.factor(类别),y=助攻))+
  geom_boxplot(fill=c( "#C6DBEF" ,"#6BAED6" ,"#2171B5" ))+
  labs(x="",y="助攻")+
  scale_x_discrete(breaks=c("1", "2", "3"),
                   labels=c("外线高射炮", "超级得分手", "内线攻城锤"))+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
ast2

pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pts2, vp = vplayout(1, 1))
print(thr2, vp = vplayout(1, 2))
print(reb2, vp = vplayout(2, 1))
print(ast2, vp = vplayout(2, 2))