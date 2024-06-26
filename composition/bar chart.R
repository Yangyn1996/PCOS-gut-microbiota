setwd("D:/R_script/【18】堆叠柱形图")
#相关包的载入：
library(ggtree)
library(ggplot2)
library(paletteer)#配色包
library(aplot)#聚类重排+拼图
library(forcats)
library(patchwork)
#本地数据的载入：
df <- read.csv("abundance.csv",header = T)
df

#指定绘图顺序：
df$genus <- factor(df$genus,levels = unique(df$genus))
df$sample<-fct_inorder(df$sample)
#ggplot2建立映射关系
p <- ggplot(df,aes(x = sample,
                   y = value,
                   fill = genus))
p

#添加自定义配色方案：
d_palettes <- palettes_d_names #使用paletteer配色包，先查看离散型变量配色方案；
d_palettes
colcors_merge <- data.frame(d_palettes)
paletteer_d("pals::alphabet", n = 23) #选择配色主题和颜色数；

col <- paletteer_d("pals::alphabet", n = 23) #新建自定义配色;
p <- p+scale_fill_manual(values = rev(col))

#绘制横向的堆叠柱形图并翻转横纵坐标：
p1 <- p +
  geom_bar(position = "fill",
           stat="identity",
           alpha = 0.4)+
  coord_flip()#坐标轴翻转
p1


#首先转换数据格式：
df1<-reshape2::dcast(df,genus~sample,
                     value.var = "value")
rownames(df1)<-df1[,1] #把第一列数据行名（此时的数据内框会多一列重复的行名）
dft <- df1[,-1] #减去多余列
head(dft)

#转置：
dftt <- t(dft)

#以Bray-curtis计算样本间距离：
dis<- vegan::vegdist(dftt, method = 'bray')
#常见聚类方法有 jaccard,euclidean,hellinger,manhattan,

#进行UPGMA 层级聚类：
p2 <- ggtree(hclust(dist(dis),method = 'average'))
p2

p3 <- p2+geom_tiplab()#查看分组标签，拼合时不需要加
p3

#将堆叠柱形图和聚类树拼合，并按照聚类重排：
p4 <- p1%>%insert_left(p2,width = 0.2)
p4