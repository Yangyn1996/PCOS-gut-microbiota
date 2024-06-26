library(ggbeeswarm)
#设置工作目录；
setwd("D:/R_script/【24】expression level")
#读入数据；
dt <- read.csv("data.csv",row.names=1,header = T)
#预览数据的前六行；
head(dt)

#载入reshape2；
library(reshape2)
#载入ggplot2;
library(ggplot2)
#指定绘图顺序；
#levels不能有重复值，这里使用unique去重；
dt$group <- factor(dt$group,levels = unique(dt$group), ordered = T)
#将数据与图形建立映射；
p1 <- ggplot(dt,aes(x=group,y=shannon))
p1

#绘制蜂群图；
p2 <- p1+geom_beeswarm(data=dt,mapping=aes(color=group),
                       priority="descending",
                       alpha=0.7,
                       dodge.width = 0,
                       size=2,
                       cex = 0.7,
                       show.legend=F)
p2

#priority = c("ascending","descending", "density", "random", "none")；
#默认参数为"ascending" /əˈsendɪŋ/，上升、升序；
#"descending" /dɪˈsendɪŋ/ 降序、下降；
#而"density" 朝向上下两个方向；
#"random" 和 "none" 效果相似，点的分布为随机状；
#绘制“朝上”的蜂群图，有点像高粱的穗子;


#自定义半透明颜色（红绿橙）；
mycolor <- c("#72bcd5","#7DC69B","#F2A1A7")
p3 <- p2 + scale_colour_manual(values=alpha(mycolor,0.9))
p3