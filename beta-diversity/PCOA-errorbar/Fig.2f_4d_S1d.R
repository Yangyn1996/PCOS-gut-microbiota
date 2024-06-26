setwd("D:/R_script/【21】PCoA/PCoAerrorbar")
args <- c("PCOA_points.txt", "metadata_PCOA.txt", "PCoA","group")
in_points_txt <- args[1]
metafile <- args[2]
outprefix <- args[3]
column <- args[4]
print(length(args))

library("openxlsx")
library("dplyr")
library("reshape2")
library("ggplot2")
library("ggpubr")


df <- read.table(in_points_txt,fill=T, header=T, check.names=F, sep="\t", as.is=T, numerals="no.loss", quote="")
colnames(df)[1] <- "name"


meta <- read.table(metafile, fill=T, header=T, check.names=F, sep="\t", as.is=T, numerals="no.loss", quote="")
meta
design <- meta[[column]]
names(design) <- meta$name

###################################
# change col order
df_samples <- df$name
meta_samples <- meta$name

print("data sample not in meta samples:")
print(df_samples[! df_samples %in% meta_samples])

print("meta sample not in data samples:")
print(meta_samples[! meta_samples %in% df_samples])

new_samples <- meta_samples[meta_samples %in% df_samples]

df <- df[df$name %in% new_samples,]
meta <- meta[meta$name %in% new_samples,]
###################################

# u1 <- meta$name
# u2 <-  df$name
# insect <- intersect(u1, u2)
# if ( ! ( length(u1) == length(u2) & length(insect) == length(u2) ) ){
#     stop("Sample Errors")
# }

df$group <- factor(design[df$name], levels=unique(meta[[column]]))
p1n <- colnames(df)[2]
p2n <- colnames(df)[3]
colnames(df)[2] <- "p1"
colnames(df)[3] <- "p2"

dfg <- df %>% group_by(group) %>% summarise(
  mean_p1=mean(p1), 
  up_p1=(mean(p1) + sd(p1)/sqrt(length(p1))),
  low_p1=(mean(p1) - sd(p1)/sqrt(length(p1))),
  mean_p2=mean(p2), 
  up_p2=(mean(p2) + sd(p2)/sqrt(length(p2))),
  low_p2=(mean(p2) - sd(p2)/sqrt(length(p2))),
  .groups = 'drop') %>% ungroup() %>% as.data.frame()

if (length(args) > 4){
  # 安照组的上级分组着色 多个点一个颜色 上级分组是组后缀
  dfg$g2 <- t(data.frame(strsplit(dfg$group, "_")))[,2]
}else{
  # 安照组着色 一个点一个颜色
  dfg$g2 <- dfg$group
}

x_mov <- ( max(dfg$up_p1) - min(dfg$low_p1) ) / 50
y_mov <- ( max(dfg$up_p2) - min(dfg$low_p2) ) / 50

p <- ggplot(data = dfg, aes(x=mean_p1, y=mean_p2, color=g2)) + geom_point() + 
  geom_errorbar(aes(group = group, ymax = up_p2, ymin = low_p2), position = position_dodge(width = 0.1), width = 0.001) + 
  geom_errorbar(aes(group = group, xmax = up_p1, xmin = low_p1), position = position_dodge(width = 0.1), width = 0.001) + 
  xlab(p1n) + ylab(p2n) + geom_text(aes(x=mean_p1, y=mean_p2, label=group), vjust=1.5, hjust=-0.3, size=3) + 
  theme(legend.position="none")+ xlim(c(min(dfg$low_p1), max(dfg$up_p1)*1.2))
p