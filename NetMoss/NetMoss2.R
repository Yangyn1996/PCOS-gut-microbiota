rm(list=ls())#clear Global Environment
#example for single file
#load package
library(NetMoss2)

setwd('D:/R_script/【27】NetMoss')

#load dataset
case_dir = read.table("case_dir.txt",header = T,sep = '\t')
control_dir = read.table("control_dir.txt",header = T,sep = '\t')

#contruct networks    ####if files exist, skip
library(rsparcc)
netBuild(case_dir = case_dir,
         control_dir = control_dir,
         method = "sparcc")     

d_net = read.table("d_net.txt",header = T,sep = '\t',row.names = 1)
h_net = read.table("h_net.txt",header = T,sep = '\t',row.names = 1)

#calculate NetMoss score
nodes_result = NetMoss(case_dir = case_dir,
                       control_dir = control_dir,
                       net_case_dir = d_net,
                       net_control_dir = h_net)
result = nodes_result[[1]]   ####NetMoss score result

#plot networks
#netPlot(nodes_result)    ####image saved
netPlot(result = nodes_result,
        num.top = 7,
        num.score = 30,
        e.th = 0.15,
        my.layout = layout_components,
        my.label = TRUE)
#plot roc 
#trim markers
marker = data.frame(result[which(result$p.adj < 0.05),])
marker = data.frame(marker[which(marker$NetMoss_Score > 0.8),])   ####marker selection
rownames(marker) = marker$taxon_names

#construct metadata    ######if file exists, skip
#metadata
#case = nodes_result[[4]]
#control = nodes_result[[5]]
#metadata = data.frame(sample_id = c(colnames(case[,-1]),
#                                    colnames(control[,-1])),
#                      type = c(rep("disease",length(colnames(case[,-1]))),
#                               rep("healthy",length(colnames(control[,-1])))))
#metadata$sample_id = as.character(metadata$sample_id)
#metadata$type = as.factor(metadata$type)
#rownames(metadata) = metadata$sample_id
metadata = read.table("metadata.txt",header = T,sep = '\t')
metadata
ROC = netROC(case_dir =  case_dir,
             control_dir =  control_dir,
             marker = marker,
             metadata = metadata,
             plot.roc = TRUE, 
             train.num = 20)    ####image saved
