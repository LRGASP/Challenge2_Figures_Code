#install.packages('pheatmap')
library(pheatmap)
library(ggplot2)

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

#install.packages('LSD');
library('LSD')
library(MASS)
library(ggplot2)
library(viridis)
theme_set(theme_bw(base_size = 16))
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
#install.packages('ggpubr')
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(scales)
library(RColorBrewer)
#install.packages('ggthemes');
library(ggthemes)
library(ggprism)
library(ggridges)
library(scales)

dat1 <- read.table('3_true_abund.tsv',header = F, sep = "\t");
head(dat1)

rownames(dat1) = c("RSEM","IsoTools","TALON","IsoQuant","Bambu","FLAIR")
colnames(dat1) = c("K1","K2","K3","K4");
head(dat1)

#color.key <- c("#CC0000","#FF3333","white","#3399FF","#3300CC")
pheatmap(dat1[,1:4], legend = F, cluster_col = FALSE, cluster_row = FALSE, show_colnames=F, show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(5))


## Kvalue
dat2 <- read.table('3_K_value.tsv',header = F, sep = "\t");
head(dat2)

rownames(dat2) = c("RSEM","IsoTools","TALON","IsoQuant","Bambu","FLAIR")
colnames(dat2) = c("K1","K2","K3","K4");
head(dat2)

color.key <- c("#3300CC","#3399FF","white","#FF3333","#CC0000")
pheatmap(dat2[,1:4], legend = F, cluster_col = FALSE, cluster_row = FALSE,  show_colnames=F, show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))



## Iso_num
dat3 <- read.table('3_num_isoforms.tsv',header = F, sep = "\t");
head(dat3)

rownames(dat3) = c("RSEM","IsoTools","TALON","IsoQuant","Bambu","FLAIR")
colnames(dat3) = c("K1","K2","K3","K4");
head(dat3)

color.key <- c("#3300CC","#3399FF","white","#FF3333","#CC0000")
pheatmap(dat3[,1:4], legend = F, cluster_col = FALSE, cluster_row = FALSE, show_colnames=F,show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))


## Exon_num
dat4 <- read.table('3_num_exons.tsv',header = F, sep = "\t");
head(dat4)

rownames(dat4) = c("RSEM","IsoTools","TALON","IsoQuant","Bambu","FLAIR")
colnames(dat4) = c("K1","K2","K3","K4");
head(dat4)

color.key <- c("#3300CC","#3399FF","white","#FF3333","#CC0000")
pheatmap(dat4[,1:4], legend = T, cluster_col = FALSE, cluster_row = FALSE, show_colnames=F,show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))


## Isoform length
dat5 <- read.table('3_isoform_length.tsv',header = F, sep = "\t");
head(dat5)

rownames(dat5) = c("RSEM","IsoTools","TALON","IsoQuant","Bambu","FLAIR")
colnames(dat5) = c("K1","K2","K3","K4");
head(dat5)

color.key <- c("#3300CC","#3399FF","white","#FF3333","#CC0000")
pheatmap(dat5[,1:4], legend = F, cluster_col = FALSE,  cluster_row = FALSE, show_colnames=F,show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))







x1<- pheatmap(dat1[,1:4], legend = F, cluster_col = FALSE, cluster_row = FALSE, show_colnames=F, show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50));
x2 <- pheatmap(dat2[,1:4], legend = F, cluster_col = FALSE, cluster_row = FALSE, show_colnames=F, show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))
x3 <- pheatmap(dat3[,1:4], legend = F, cluster_col = FALSE, cluster_row = FALSE,  show_colnames=F, show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))
x4 <- pheatmap(dat4[,1:4], legend = F, cluster_col = FALSE, cluster_row = FALSE, show_colnames=F, show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))
x5 <- pheatmap(dat5[,1:4], legend = F, cluster_col = FALSE, cluster_row = FALSE, show_colnames=F, show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))





require(ggplotify)
x1 = as.ggplot(x1)
x2 = as.ggplot(x2)
x3 = as.ggplot(x3)
x4 = as.ggplot(x4)
x5 = as.ggplot(x5)



plot_tmp <- plot_grid(plot_grid(x1,x2,x3,x4,x5,ncol=5),nrow=1)

ggsave(filename = paste0("fig_heatmap_new",".pdf"), 
           plot =  plot_tmp, 
           width=90, 
           height=18,
           units = "cm",
           dpi=300 )

x11 <- pheatmap(dat5[,1:4], legend = T, cluster_col = FALSE,  show_colnames=F, show_rownames=T,scale = "row",color=colorRampPalette(c("#EAF2F8","#2980B9","#154360"))(50))
x11 = as.ggplot(x11)

ggsave(filename = paste0("fig_legend_new",".pdf"), 
           plot =  x11, 
           width=18, 
           height=18,
           units = "cm",
           dpi=300 )
