library('LSD')
library(ggplot2)
library(plyr)
library(ggridges)
library(RColorBrewer)
library(dplyr)
library(ggprism)
library(ggpubr)
library(cowplot)
library(MASS)
library(viridis)
theme_set(theme_bw(base_size = 16))
library(grid)
library(gridExtra)
library(scales)
library(ggthemes)
library(ggridges)
library(pheatmap)

## CV curves-------------------------------------------------------------------------------------------

## H1-mix CapTrap ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_CapTrap_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_CapTrap_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x2 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_CapTrap_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_CapTrap_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_CapTrap_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p1 <- ggarrange(x1,x2,NULL,x3,x4,NULL,x5,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-mix CapTrap PacBio
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_CapTrap_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_CapTrap_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x7 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_CapTrap_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_CapTrap_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_CapTrap_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p2 <- ggarrange(x6,x7,NULL,x8,x9,NULL,x10,NULL,NULL,ncol = 9, align = "v",hjust=0)



## H1-mix cDNA ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_cDNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_cDNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x12 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_cDNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_cDNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_cDNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_cDNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p3 <- ggarrange(x11,x12,x13,x14,x15,NULL,x16,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-mix cDNA PacBio
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_cDNA_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_cDNA_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x18 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_cDNA_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_cDNA_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoTools_cDNA_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_cDNA_PacBio_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p4 <- ggarrange(x17,x18,x19,NULL,x20,x21,x22,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-mix dRNA ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_dRNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_dRNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x24 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_dRNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_dRNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_dRNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_dRNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/NanoSim_dRNA_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x29 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p5 <- ggarrange(x23,x24,x25,x26,x27,NULL,x28,x29,NULL,ncol = 9, align = "v",hjust=0)


## H1-mix R2C2 ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_R2C2_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x30 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_R2C2_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x31 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_R2C2_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x32 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_R2C2_ONT_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x33 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p6 <- ggarrange(x30,x31,NULL,NULL,x32,NULL,x33,NULL,NULL,ncol = 9, align = "v",hjust=0)

## H1-mix cDNA Illumina
dat <- read.table('RNA-seq_data/COV_plot_data/RSEM_cDNA_Illumina_H1-mix_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x34 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,x34,ncol = 9, align = "v",hjust=0)

H1_mix <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)


## WTC11 CapTrap ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_CapTrap_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_CapTrap_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x2 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_CapTrap_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_CapTrap_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_CapTrap_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p1 <- ggarrange(x1,x2,NULL,x3,x4,NULL,x5,NULL,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 CapTrap PacBio
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_CapTrap_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_CapTrap_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x7 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_CapTrap_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_CapTrap_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_CapTrap_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p2 <- ggarrange(x6,x7,NULL,x8,x9,NULL,x10,NULL,NULL,ncol = 9, align = "v",hjust=0)



## WTC11 cDNA ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_cDNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_cDNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x12 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_cDNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_cDNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_cDNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_cDNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p3 <- ggarrange(x11,x12,x13,x14,x15,NULL,x16,NULL,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 cDNA PacBio
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_cDNA_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_cDNA_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x18 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_cDNA_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_cDNA_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoTools_cDNA_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_cDNA_PacBio_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p4 <- ggarrange(x17,x18,x19,NULL,x20,x21,x22,NULL,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 dRNA ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_dRNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_dRNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x24 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_dRNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_dRNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_dRNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_dRNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/NanoSim_dRNA_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x29 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p5 <- ggarrange(x23,x24,x25,x26,x27,NULL,x28,x29,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 R2C2 ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_R2C2_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x30 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_R2C2_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x31 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_R2C2_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x32 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_R2C2_ONT_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x33 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p6 <- ggarrange(x30,x31,NULL,NULL,x32,NULL,x33,NULL,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 cDNA Illumina
dat <- read.table('RNA-seq_data/COV_plot_data/RSEM_cDNA_Illumina_WTC11_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x34 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,x34,ncol = 9, align = "v",hjust=0)

WTC11 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)


## H1-hESC CapTrap ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_CapTrap_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_CapTrap_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x2 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_CapTrap_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_CapTrap_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_CapTrap_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p1 <- ggarrange(x1,NULL,NULL,x3,x4,NULL,x5,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC CapTrap PacBio
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_CapTrap_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_CapTrap_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x7 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_CapTrap_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_CapTrap_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_CapTrap_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p2 <- ggarrange(x6,NULL,NULL,x8,x9,NULL,x10,NULL,NULL,ncol = 9, align = "v",hjust=0)



## H1-hESC cDNA ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_cDNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_cDNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x12 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_cDNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_cDNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_cDNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_cDNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/NanoSim_cDNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x35 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p3 <- ggarrange(x11,NULL,x13,x14,x15,NULL,x16,x35,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC cDNA PacBio
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_cDNA_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_cDNA_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x18 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_cDNA_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_cDNA_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoTools_cDNA_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_cDNA_PacBio_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p4 <- ggarrange(x17,NULL,x19,NULL,x20,x21,x22,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC dRNA ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_dRNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_dRNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x24 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_dRNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_dRNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_dRNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_dRNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/NanoSim_dRNA_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x29 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p5 <- ggarrange(x23,NULL,x25,x26,x27,NULL,x28,x29,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC R2C2 ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_R2C2_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x30 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/BambuLR_R2C2_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x31 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_R2C2_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x32 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_R2C2_ONT_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x33 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p6 <- ggarrange(x30,NULL,NULL,NULL,x32,NULL,x33,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC cDNA Illumina
dat <- read.table('RNA-seq_data/COV_plot_data/RSEM_cDNA_Illumina_H1-hESC_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x34 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,x34,ncol = 9, align = "v",hjust=0)

H1_hESC <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)





## H1-DE CapTrap ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_CapTrap_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))



dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_CapTrap_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_CapTrap_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_CapTrap_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p1 <- ggarrange(x1,NULL,NULL,x3,x4,NULL,x5,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE CapTrap PacBio
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_CapTrap_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_CapTrap_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_CapTrap_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_CapTrap_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p2 <- ggarrange(x6,NULL,NULL,x8,x9,NULL,x10,NULL,NULL,ncol = 9, align = "v",hjust=0)



## H1-DE cDNA ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_cDNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_cDNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_cDNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_cDNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_cDNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/NanoSim_cDNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x35 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p3 <- ggarrange(x11,NULL,x13,x14,x15,NULL,x16,x35,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE cDNA PacBio
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_cDNA_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_cDNA_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_cDNA_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/IsoTools_cDNA_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_cDNA_PacBio_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p4 <- ggarrange(x17,NULL,x19,NULL,x20,x21,x22,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE dRNA ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_dRNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))



dat <- read.table('RNA-seq_data/COV_plot_data/FLAIR_dRNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/FLAMES_dRNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_dRNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_dRNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

dat <- read.table('RNA-seq_data/COV_plot_data/NanoSim_dRNA_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x29 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p5 <- ggarrange(x23,NULL,x25,x26,x27,NULL,x28,x29,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE R2C2 ONT
dat <- read.table('RNA-seq_data/COV_plot_data/Bambu_R2C2_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x30 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))



dat <- read.table('RNA-seq_data/COV_plot_data/IsoQuant_R2C2_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x32 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))


dat <- read.table('RNA-seq_data/COV_plot_data/LAPA_R2C2_ONT_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x33 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p6 <- ggarrange(x30,NULL,NULL,NULL,x32,NULL,x33,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE cDNA Illumina
dat <- read.table('RNA-seq_data/COV_plot_data/RSEM_cDNA_Illumina_H1-DE_COV.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x34 <- ggplot(data = dat, mapping = aes(x = V1,y = V2)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0, 1.5), breaks = seq(0,1.5,1.5)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))

p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,x34,ncol = 9, align = "v",hjust=0)

H1_DE <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)

x1 <- ggarrange(H1_mix,WTC11,ncol = 2, align = "v",hjust=0)
x2 <- ggarrange(H1_hESC,H1_DE,ncol = 2, align = "v",hjust=0)

fig_merge <- ggarrange(x1,x2,nrow = 2, align = "v",hjust=0)

ggsave(filename = paste0("fig_COV_curves",".pdf"), 
           plot =  fig_merge, 
           width=45, 
           height=40,
           units = "cm",
           dpi=300 )


## CM curves-------------------------------------------------------------------------------------------
## H1-mix CapTrap ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_CapTrap_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_CapTrap_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x2 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_CapTrap_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_CapTrap_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_CapTrap_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p1 <- ggarrange(x1,x2,NULL,x3,x4,NULL,x5,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-mix CapTrap PacBio
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_CapTrap_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_CapTrap_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x7 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_CapTrap_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_CapTrap_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_CapTrap_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p2 <- ggarrange(x6,x7,NULL,x8,x9,NULL,x10,NULL,NULL,ncol = 9, align = "v",hjust=0)



## H1-mix cDNA ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_cDNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_cDNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x12 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_cDNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_cDNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_cDNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_cDNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p3 <- ggarrange(x11,x12,x13,x14,x15,NULL,x16,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-mix cDNA PacBio
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_cDNA_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_cDNA_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x18 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_cDNA_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_cDNA_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoTools_cDNA_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_cDNA_PacBio_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p4 <- ggarrange(x17,x18,x19,NULL,x20,x21,x22,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-mix dRNA ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_dRNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_dRNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x24 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_dRNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_dRNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_dRNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_dRNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/NanoSim_dRNA_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x29 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p5 <- ggarrange(x23,x24,x25,x26,x27,NULL,x28,x29,NULL,ncol = 9, align = "v",hjust=0)


## H1-mix R2C2 ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_R2C2_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x30 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_R2C2_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x31 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_R2C2_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x32 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_R2C2_ONT_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x33 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p6 <- ggarrange(x30,x31,NULL,NULL,x32,NULL,x33,NULL,NULL,ncol = 9, align = "v",hjust=0)

## H1-mix cDNA Illumina
dat <- read.table('RNA-seq_data/CM_plot_data/RSEM_cDNA_Illumina_H1-mix_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x34 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#C0504D') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,x34,ncol = 9, align = "v",hjust=0)

H1_mix <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)


## WTC11 CapTrap ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_CapTrap_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_CapTrap_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x2 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_CapTrap_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_CapTrap_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_CapTrap_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p1 <- ggarrange(x1,x2,NULL,x3,x4,NULL,x5,NULL,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 CapTrap PacBio
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_CapTrap_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_CapTrap_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x7 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_CapTrap_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_CapTrap_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_CapTrap_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p2 <- ggarrange(x6,x7,NULL,x8,x9,NULL,x10,NULL,NULL,ncol = 9, align = "v",hjust=0)



## WTC11 cDNA ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_cDNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_cDNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x12 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_cDNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_cDNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_cDNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_cDNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p3 <- ggarrange(x11,x12,x13,x14,x15,NULL,x16,NULL,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 cDNA PacBio
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_cDNA_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_cDNA_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x18 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_cDNA_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_cDNA_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoTools_cDNA_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_cDNA_PacBio_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p4 <- ggarrange(x17,x18,x19,NULL,x20,x21,x22,NULL,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 dRNA ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_dRNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_dRNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x24 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_dRNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_dRNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_dRNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_dRNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/NanoSim_dRNA_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x29 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p5 <- ggarrange(x23,x24,x25,x26,x27,NULL,x28,x29,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 R2C2 ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_R2C2_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x30 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_R2C2_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x31 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_R2C2_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x32 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_R2C2_ONT_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x33 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p6 <- ggarrange(x30,x31,NULL,NULL,x32,NULL,x33,NULL,NULL,ncol = 9, align = "v",hjust=0)


## WTC11 cDNA Illumina
dat <- read.table('RNA-seq_data/CM_plot_data/RSEM_cDNA_Illumina_WTC11_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x34 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#4F81BD') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,x34,ncol = 9, align = "v",hjust=0)

WTC11 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)


## H1-hESC CapTrap ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_CapTrap_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_CapTrap_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x2 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_CapTrap_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_CapTrap_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_CapTrap_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p1 <- ggarrange(x1,NULL,NULL,x3,x4,NULL,x5,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC CapTrap PacBio
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_CapTrap_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_CapTrap_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x7 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_CapTrap_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_CapTrap_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_CapTrap_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p2 <- ggarrange(x6,NULL,NULL,x8,x9,NULL,x10,NULL,NULL,ncol = 9, align = "v",hjust=0)



## H1-hESC cDNA ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_cDNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_cDNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x12 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_cDNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_cDNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_cDNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_cDNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/NanoSim_cDNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x35 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p3 <- ggarrange(x11,NULL,x13,x14,x15,NULL,x16,x35,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC cDNA PacBio
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_cDNA_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_cDNA_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x18 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_cDNA_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_cDNA_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoTools_cDNA_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_cDNA_PacBio_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p4 <- ggarrange(x17,NULL,x19,NULL,x20,x21,x22,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC dRNA ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_dRNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_dRNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x24 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_dRNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_dRNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_dRNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_dRNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/NanoSim_dRNA_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x29 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p5 <- ggarrange(x23,NULL,x25,x26,x27,NULL,x28,x29,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC R2C2 ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_R2C2_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x30 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/BambuLR_R2C2_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x31 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_R2C2_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x32 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_R2C2_ONT_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x33 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p6 <- ggarrange(x30,NULL,NULL,NULL,x32,NULL,x33,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-hESC cDNA Illumina
dat <- read.table('RNA-seq_data/CM_plot_data/RSEM_cDNA_Illumina_H1-hESC_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x34 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#8064A2') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,x34,ncol = 9, align = "v",hjust=0)

H1_hESC <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)








## CM curves-----------------------------------------------

## H1-DE CapTrap ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_CapTrap_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))



dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_CapTrap_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_CapTrap_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_CapTrap_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p1 <- ggarrange(x1,NULL,NULL,x3,x4,NULL,x5,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE CapTrap PacBio
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_CapTrap_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_CapTrap_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_CapTrap_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_CapTrap_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p2 <- ggarrange(x6,NULL,NULL,x8,x9,NULL,x10,NULL,NULL,ncol = 9, align = "v",hjust=0)



## H1-DE cDNA ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_cDNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_cDNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_cDNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_cDNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_cDNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/NanoSim_cDNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x35 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p3 <- ggarrange(x11,NULL,x13,x14,x15,NULL,x16,x35,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE cDNA PacBio
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_cDNA_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_cDNA_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_cDNA_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/IsoTools_cDNA_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_cDNA_PacBio_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p4 <- ggarrange(x17,NULL,x19,NULL,x20,x21,x22,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE dRNA ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_dRNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))



dat <- read.table('RNA-seq_data/CM_plot_data/FLAIR_dRNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/FLAMES_dRNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_dRNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_dRNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

dat <- read.table('RNA-seq_data/CM_plot_data/NanoSim_dRNA_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x29 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p5 <- ggarrange(x23,NULL,x25,x26,x27,NULL,x28,x29,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE R2C2 ONT
dat <- read.table('RNA-seq_data/CM_plot_data/Bambu_R2C2_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x30 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))



dat <- read.table('RNA-seq_data/CM_plot_data/IsoQuant_R2C2_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x32 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))


dat <- read.table('RNA-seq_data/CM_plot_data/LAPA_R2C2_ONT_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x33 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p6 <- ggarrange(x30,NULL,NULL,NULL,x32,NULL,x33,NULL,NULL,ncol = 9, align = "v",hjust=0)


## H1-DE cDNA Illumina
dat <- read.table('RNA-seq_data/CM_plot_data/RSEM_cDNA_Illumina_H1-DE_CM.txt',header = F, sep = "\t");
names(dat) <- c('V1','V2','V3','V4','V5','V6');
head(dat)

x34 <- ggplot(data = dat, mapping = aes(x = V2,y = V1)) + 
  geom_line(shape=19,size=0.5,color='#75923C') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(0.5, 1), breaks = seq(0.5,1,0.5)) +
  scale_x_continuous(limits=c(1.1, 10), breaks = seq(1.1,10,8.9))

p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,x34,ncol = 9, align = "v",hjust=0)

H1_DE <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)

x1 <- ggarrange(H1_mix,WTC11,ncol = 2, align = "v",hjust=0)
x2 <- ggarrange(H1_hESC,H1_DE,ncol = 2, align = "v",hjust=0)

fig_merge <- ggarrange(x1,x2,nrow = 2, align = "v",hjust=0)

ggsave(filename = paste0("fig_CM_curves",".pdf"), 
           plot =  fig_merge, 
           width=45, 
           height=40,
           units = "cm",
           dpi=300 )












## Scatter plot for cell mixing experiment-----------------------------------------------------------------------------
## CapTrap ONT
dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/Bambu_CapTrap_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x1 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))+
  ggtitle("SCC=0.85")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/FLAMES_CapTrap_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x2 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))+
  ggtitle("SCC=0.70")+theme(plot.title = element_text(size = 18, hjust = 0.5))



dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/IsoQuant_CapTrap_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x3 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.88")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/LAPA_CapTrap_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x4 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.84")+theme(plot.title = element_text(size = 18, hjust = 0.5))


p1 <- ggarrange(x1,NULL,x2,x3,NULL,x4,NULL,NULL,ncol = 8, align = "v",hjust=0)



## CapTrap PacBio
dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/Bambu_CapTrap_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x5 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))+
  ggtitle("SCC=0.81")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/FLAMES_CapTrap_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x6 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.81")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/IsoQuant_CapTrap_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x7 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.84")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/LAPA_CapTrap_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x8 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.87")+theme(plot.title = element_text(size = 18, hjust = 0.5))


p2 <- ggarrange(x5,NULL,x6,x7,NULL,x8,NULL,NULL,ncol = 8, align = "v",hjust=0)

## cDNA ONT
dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/Bambu_cDNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x9 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.55")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/FLAIR_cDNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x10 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.84")+theme(plot.title = element_text(size = 18, hjust = 0.5))

dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/FLAMES_cDNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x11 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.76")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/IsoQuant_cDNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x12 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.88")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/LAPA_cDNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x13 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.83")+theme(plot.title = element_text(size = 18, hjust = 0.5))
   


p3 <- ggarrange(x9,x10,x11,x12,NULL,x13,NULL,NULL,ncol = 8, align = "v",hjust=0)


## cDNA PacBio
dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/Bambu_cDNA_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x14 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.48")+theme(plot.title = element_text(size = 18, hjust = 0.5))



dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/FLAIR_cDNA_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x15 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.83")+theme(plot.title = element_text(size = 18, hjust = 0.5))



dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/IsoQuant_cDNA_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x16 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.86")+theme(plot.title = element_text(size = 18, hjust = 0.5))



dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/IsoTools_cDNA_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x17 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.85")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/LAPA_cDNA_PacBio_RD.tsv',header = T, sep = "\t");
head(dat)

x18 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.86")+theme(plot.title = element_text(size = 18, hjust = 0.5))



p4 <- ggarrange(x14,x15,NULL,x16,x17,x18,NULL,NULL,ncol = 8, align = "v",hjust=0)



## dRNA ONT
dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/Bambu_dRNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x19 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.52")+theme(plot.title = element_text(size = 18, hjust = 0.5))



dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/FLAIR_dRNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x20 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))+
  ggtitle("SCC=0.75")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/FLAMES_dRNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x21 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.79")+theme(plot.title = element_text(size = 18, hjust = 0.5))



dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/IsoQuant_dRNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x22 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.82")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/LAPA_dRNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x23 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.75")+theme(plot.title = element_text(size = 18, hjust = 0.5))


dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/NanoSim_dRNA_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x24 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.74")+theme(plot.title = element_text(size = 18, hjust = 0.5))


p5 <- ggarrange(x19,x20,x21,x22,NULL,x23,x24,NULL,ncol = 8, align = "v",hjust=0)


## R2C2 ONT
dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/Bambu_R2C2_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x25 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.40")+theme(plot.title = element_text(size = 18, hjust = 0.5))



dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/IsoQuant_R2C2_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x26 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.69")+theme(plot.title = element_text(size = 18, hjust = 0.5))



dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/LAPA_R2C2_ONT_RD.tsv',header = T, sep = "\t");
head(dat)

x27 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9))  +
  ggtitle("SCC=0.57")+theme(plot.title = element_text(size = 18, hjust = 0.5))



p6 <- ggarrange(x25,NULL,NULL,x26,NULL,x27,NULL,NULL,ncol = 8, align = "v",hjust=0)

## cDNA Illumina
dat <- read.table('Cell_mixing_experiment/Relative_error_isoform_level/RSEM_cDNA_Illumina_RD.tsv',header = T, sep = "\t");
head(dat)

x28 <- ggplot(data = dat, mapping = aes(x = expected_TPM,y = observed_TPM)) + 
  geom_pointdensity(shape=19,size=0.01) + 
  scale_color_viridis(option="turbo") + 
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.8,size = 0.5, se = F,color="#5D6D7E") + 
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank()) +
  scale_y_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1,10,9)) +
  ggtitle("SCC=0.87")+theme(plot.title = element_text(size = 18, hjust = 0.5))


p7 <- ggarrange(NULL,NULL,NULL,NULL,NULL,NULL,NULL,x28,ncol = 8, align = "v",hjust=0)


fig_merge <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 7, align = "v",hjust=0)

ggsave(filename = paste0("fig_mix_experiment_scatter",".pdf"), 
           plot =  fig_merge, 
           width=40, 
           height=40,
           units = "cm",
           dpi=300 )
























