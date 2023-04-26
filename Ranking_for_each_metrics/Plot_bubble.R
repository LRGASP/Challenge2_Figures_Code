#install.packages('LSD');
library('LSD')
library(ggplot2)
#install.packages('plyr');
library(plyr)
#install.packages('ggridges');
library(ggridges)
#install.packages('RColorBrewer');
library(RColorBrewer)
library(dplyr)
library(ggplot2)
#install.packages('ggprism');
library(ggprism)
library(ggridges)
#install.packages('ggpubr')
library(ggpubr)
library(cowplot)


dat <- read.table('tools.txt',header = FALSE, sep = "\t");
head(dat)

colorset = c("#873600","#BA4A00","#DC7633","#EDBB99");


x1 <- ggplot(dat,aes(x=V4,y=V1,color=V2)) + geom_point(size=4) +
#      scale_size(range=c(1,4),breaks=c(4,3,2,1),labels=c("1","2","3","4"),name="Ranking") +
    scale_colour_manual(values=colorset) +
    theme(axis.text.x=element_text(angle = 90,hjust = 1))  + 
    theme_prism(base_size = 5)+ 
    theme(legend.position="none")+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
  axis.text.y=element_blank(),axis.text.x=element_blank(),
  axis.line.x = element_line(size=0.2,colour="black"))




dat <- read.table('protocols_platforms.txt',header = FALSE, sep = "\t");
head(dat)

x2 <- ggplot(dat,aes(x=V4,y=V1,color=V2)) + geom_point(size=4) +
#      scale_size(range=c(1,4),breaks=c(4,3,2,1),labels=c("1","2","3","4"),name="Ranking") +
    scale_colour_manual(values=colorset) +
    theme(axis.text.x=element_text(angle = 90,hjust = 1))  + 
    theme_prism(base_size = 5)+ 
    theme(legend.position="none")+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
  axis.text.y=element_blank(),axis.text.x=element_blank(),
  axis.line.x = element_line(size=0.2,colour="black"))

p1 <- ggarrange(x1,x2,ncol = 2, align = "v")

ggsave(filename = paste0("fig_bubble_diagram_new",".pdf"), 
           plot =  p1, 
           width=20, 
           height=4,
           units = "cm",
           dpi=300 )


dat <- read.table('tools.txt',header = FALSE, sep = "\t");
head(dat)

x1 <- ggplot(dat,aes(x=V4,y=V1,color=V2)) + geom_point(size=4) +
#      scale_size(range=c(1,4),breaks=c(4,3,2,1),labels=c("1","2","3","4"),name="Ranking") +
    scale_colour_manual(values=colorset)
    

p1 <- ggarrange(x1,x2,ncol = 2, align = "v")

ggsave(filename = paste0("fig_legend_new",".pdf"), 
           plot =  x1, 
           width=10, 
           height=4,
           units = "cm",
           dpi=300 )