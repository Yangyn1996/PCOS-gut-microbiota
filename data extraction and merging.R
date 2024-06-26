install.packages("openxlsx")
install.packages("tidyverse")
install.packages("stringr")

library(tidyverse)
library(openxlsx)
library(stringr)
#设置工作路径
setwd("D:\\qiime2\\分割\\PRJNA394687-1000")
list_name = dir("./",pattern = ".xlsx")
list_name
#sheet填第几个工作表（如2/3），cols=填第几列（如4，如果是多列就是C（4，5））
re = map(list_name, ~ read.xlsx(.,sheet=2,cols = c(4,5)))
re
names(re) = list_name
re
#逗号后面可以修改输出名称以及路径
write.xlsx(re,"D:\\qiime2\\分割\\PRJNA394687-1000\\re_hebing100.xlsx")

###输出后就用excel切割表格到另一个文件文件夹###
###输出后就用excel切割表格到另一个文件文件夹###
###输出后就用excel切割表格到另一个文件文件夹###

#获取切割后的文件夹路径
list.files("D:\\qiime2\\分割\\changshi\\组合",pattern = "*.xlsx",full.names = T) %>% 
  lapply(readxl::read_excel) %>% 
  #by=的意思是根据什么合并
  reduce(full_join,by="scientific_name") %>%
  #根据什么排列
  arrange(scientific_name) %>% 
select(contains("scientific_name")|contains("relative_abunda")) -> aa
#下面是修改表头，获取原文件夹的路径，格式是（ERR209061.data.xlsx）这样的
list.files("D:\\qiime2\\分割\\changshi\\组合",pattern = "*.xlsx",full.names = F)
colnames(aa)<-c("scientific_name",str_replace(
  list.files("D:\\qiime2\\分割\\changshi\\组合",pattern = "*.xlsx",full.names = F)
  ,".data.xlsx",""))
head(aa)
#逗号后面可以修改输出名称以及路径
write.xlsx(aa,"D:\\qiime2\\分割\\changshi\\组合\\genus.xlsx")
