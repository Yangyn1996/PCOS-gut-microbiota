rm(list=ls())#clear Global Environment


#安装所需R包
#install.packages("vegan")
#install.packages("ggplot2")
#加载包
library(vegan)#计算距离时需要的包
library(ggplot2)#绘图包
library(forcats)
#读取数据，一般所需是数据行名为样本名、列名为OTUxxx的数据表
otu_raw <- read.table(file="genus-3.txt",sep="\t",header=T,check.names=FALSE ,row.names=1)
#由于排序分析函数所需数据格式原因，需要对数据进行转置
otu <- t(otu_raw)

data.hell<-decostand(otu,method="hellinger")
#计算bray_curtis距离
otu.distance <- vegdist(data.hell)
#pcoa分析
pcoa <- cmdscale (otu.distance,eig=TRUE)
pc12 <- pcoa$points[,1:2]
pc <- round(pcoa$eig/sum(pcoa$eig)*100,digits=2)#解释度

###绘图###
#pcl2原来是matrix,转化为data.frame
pc12 <- as.data.frame(pc12)
#给pc12添加samp1es变量
pc12$samples <- row.names(pc12)
head(pc12)

#绘图
p <- ggplot(pc12,aes(x=V1, y=V2))+#指定数据、X轴、Y轴
  geom_point(size=3)+#绘制点图并设定大小
  theme_bw()#主题
p

#读入分组文件
group <- read.table("group-3.txt", sep='\t', header=T)
#修改列名
colnames(group) <- c("samples","group")
#将绘图数据和分组合并
df <- merge(pc12,group,by="samples")
head(df)
write.table(x=df,file ='PCOA.txt')


#也可以直接读取算好的数据进行绘图，不运行前面的计算过程
df<- read.csv('pcoa.csv',header = T,row.names = 1)
head(df)
#绘图
color=c("#72bcd5","#e76254","#F7AA58","#7DC69B","#e76254","#197EC099","#F7AA58","#87BBA4","#FFE898","#72bcd5","#F7AA58","#1597A5","#E76254","#FFE898","#FFD06F","#FFC24B","#4B0082","#FEB3AE","#0000FF","#4B0082","#FF0000","#FFEFD5","#006400","#808080","#FF8C00")#颜色变量
p1 <-ggplot(data=df,aes(x=V1,y=V2,
                        color=group,shape=group))+#指定数据、X轴、Y轴，颜色
  theme_bw()+#主题设置
  geom_point(shape = 16, size = 3)+#绘制点图并设定大小
  #scale_shape_manual(values = c(1:10))+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+#图中虚线
  #geom_text(aes(label=samples, y=V2+0.03,x=V1+0.03,  vjust=0),size=3.5)+#添加数据点的标签
  # guides(color=guide_legend(title=NULL))+#去除图例标题
  labs(x=paste0("PC1 ",pc[1],"%"),
       y=paste0("PC2 ",pc[2],"%"))+#将x、y轴标题改为贡献度
  #stat_ellipse(data=df,
  # geom = "polygon",level=0.90,
  #  linetype = 2,size=0.5,
  #  aes(fill=group),
  #  alpha=0.5,
  #  show.legend = T)+
  scale_color_manual(values = color) +#点的颜色设置
  scale_fill_manual(values = c("#72bcd5","#e76254","#F7AA58","#7DC69B","#e76254","#197EC099","#F7AA58","#87BBA4","#FFE898","#72bcd5","#82B29A","#F7AA58","#E57B7F","#72BCD5","#87BBA4","#E57B7F","#BDB5E1","#197EC099","#F7AA58","#1597A5","#E76254","#FFE898","#FFD06F","#B24745FF","#374E55FF", "#DF8F44FF","#1597A5","#FFC24B","#4B0082","#FEB3AE","#0000FF","#4B0082","#F5FFFA","#FF0000","#006400","#808080","#FF8C00"))+
  theme(axis.title.x=element_text(size=9),#修改X轴标题文本
        axis.title.y=element_text(size=9,angle=90),#修改y轴标题文本
        axis.text.y=element_text(size=9),#修改x轴刻度标签文本
        axis.text.x=element_text(size=9),#修改y轴刻度标签文本
        panel.grid=element_blank())#隐藏网格线
p1


library(ggpubr)
library(ggsignif)
# 绘制y轴为PC2值的分组箱线图
df$group <-fct_inorder(df$group)
p2 <- ggplot(df,aes(x=group,y=V2))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.1,size=0.3)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  geom_boxplot(aes(fill=group), #绘制箱线图函数
               outlier.colour="white",size=0.3)+#异常点去除
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(color = "white"),#坐标轴的线设为显示
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),#关闭刻度
        legend.position = 'none')+
  xlab("") + ylab("")+
  scale_fill_manual(values=c("#F7AA58","#72bcd5","#e76254","#7DC69B","#e76254","#197EC099","#F7AA58","#87BBA4","#F7AA58","#e76254","#87BBA4","#E57B7F","#BDB5E1","#197EC099","#F7AA58","#1597A5","#E76254","#FFD06F","#72BCD5","#B24745FF","#374E55FF", "#DF8F44FF","#1597A5","#FFC24B","#4B0082","#FEB3AE","#0000FF","#4B0082","#F5FFFA","#FF0000","#006400","#808080","#FF8C00"))+#指定颜色
  geom_signif(comparisons = list(c("HC","PCOS-L"),
                                 c("HC","PCOS-H"),
                                 c("PCOS-L","PCOS-H")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,0.35,0.45,0.55,0.65),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")
p2

# 绘制y轴为PC1值的分组箱线图
p3 <- ggplot(df,aes(x=group,y=V1))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.1,size=0.3)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  coord_flip()+
  geom_boxplot(aes(fill=group), #绘制箱线图函数
               outlier.colour="white",size=0.3)+#异常点去除
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(color = "white"),#坐标轴的线设为显示
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),#关闭刻度
        legend.position = 'none')+
  xlab("") + ylab("")+
  scale_fill_manual(values=c("#F7AA58","#72bcd5","#e76254","#7DC69B","#e76254","#197EC099","#F7AA58","#87BBA4","#F7AA58","#e76254","#87BBA4","#E57B7F","#BDB5E1","#197EC099","#F7AA58","#1597A5","#E76254","#FFD06F","#72BCD5","#B24745FF","#374E55FF", "#DF8F44FF","#1597A5","#FFC24B","#4B0082","#FEB3AE","#0000FF","#4B0082","#F5FFFA","#FF0000","#006400","#808080","#FF8C00"))+#指定颜色
  geom_signif(comparisons = list(c("HC","PCOS-L"),
                                 c("HC","PCOS-H"),
                                 c("PCOS-L","PCOS-H")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.5,0.6,0.7,0.8,0.9,1.0,1.1,0.35,0.45,0.55,0.65),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")
p3
# ggpubr::ggarrange()函数对图进行拼接
ggarrange(p3, NULL, p1, p2, widths = c(5,3), heights = c(2,4), align = "hv")
