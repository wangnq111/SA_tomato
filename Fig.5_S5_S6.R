library(ggplot2)
library(LorMe)
library(patchwork)

#Figure 5A####
##Figure 5A####
DR_SPH1_0=read.csv("./sourcedata/6.Data_FIG.5A.csv",header = T)
signif_results<- auto_signif_test(data =DR_SPH1_0,treatment_col =1,value_col =2,prior = T)
letters<- data.frame(signif_results$comparison_letters,letterp=max(1.3*(signif_results$comparison_letters %>% .[,'Mean'])+1.3*(signif_results$comparison_letters %>% .[,'std'])))
mean_frame<- aggregate(DR_SPH1_0[,2],by=list(DR_SPH1_0[,1]),FUN=mean)
Sd<- aggregate(DR_SPH1_0[,2],by=list(DR_SPH1_0[,1]),FUN=sd) %>% .[,'x']
Treatment_Name<- mean_frame$Group.1
N<- table(DR_SPH1_0[,1]) %>% as.numeric()
Mean<- mean_frame[,'x']
SEM<- Sd/(N^0.5)
input_mean_frame<- data.frame(Treatment_Name,N,Mean,Sd,SEM)
p_DR_SPH1 <- ggplot(input_mean_frame,aes(x=factor(Treatment_Name, levels=c('CK','RSA89','RSA89+SA')),y=Mean))+
  theme_zg()+
  geom_bar(stat = 'identity',size=0.2,width=0.5,color='#000000',alpha=0.8)+
  geom_errorbar(aes(ymin=Mean-Sd,ymax=Mean+Sd),size=0.2,width=0.2)+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.65))+
  labs(x='',fill='',y='Disease index')+
  geom_text(data=letters,aes(x=as.factor(compare),y=0.6,label=Letters),size=6)+
  theme(legend.position='None',aspect.ratio = 1)
p_DR_SPH1

##Figure 5B####
invader=read.csv("./sourcedata/6.Data_FIG.5B.csv",header = T)
signif_results<- auto_signif_test(data =invader,treatment_col =1,value_col =2,prior = T)
letters<- data.frame(signif_results$comparison_letters,letterp=max(1.3*(signif_results$comparison_letters %>% .[,'Mean'])+1.3*(signif_results$comparison_letters %>% .[,'std'])))
mean_frame<- aggregate(invader[,2],by=list(invader[,1]),FUN=mean)
Sd<- aggregate(invader[,2],by=list(invader[,1]),FUN=sd) %>% .[,'x']
Treatment_Name<- mean_frame$Group.1
N<- table(invader[,1]) %>% as.numeric()
Mean<- mean_frame[,'x']
SEM<- Sd/(N^0.5)
input_mean_frame<- data.frame(Treatment_Name,N,Mean,Sd,SEM)
p_invader <- ggplot(input_mean_frame,aes(x=as.factor(Treatment_Name),y=Mean))+
  theme_zg()+
  geom_bar(stat = 'identity',size=0.2,width=0.5,color='#000000',alpha=0.8)+
  geom_errorbar(aes(ymin=Mean-Sd,ymax=Mean+Sd),size=0.2,width=0.1)+
  scale_y_continuous(expand = c(0,0),limits = c(0,5.5))+
  labs(x='',fill='',y='Pathogen abundance')+
  geom_text(data=letters,aes(x=as.factor(compare),y=5.2,label=Letters),size=6)+
  theme(legend.position='None')
p_invader

##Figure 5C####
liquor_SPH1 <- read.csv("./sourcedata/6.Data_FIG.5C.csv",header=T)
signif_results<- auto_signif_test(data =liquor_SPH1,treatment_col =1,value_col =2,prior = T)
mean_frame<- aggregate(liquor_SPH1[,2],by=list(liquor_SPH1[,1]),FUN=mean)
Sd<- aggregate(liquor_SPH1[,2],by=list(liquor_SPH1[,1]),FUN=sd) %>% .[,'x']
Treatment_Name<- mean_frame$Group.1
N<- table(liquor_SPH1[,1]) %>% as.numeric()
Mean<- mean_frame[,'x']
SEM<- Sd/(N^0.5)
input_mean_frame<- data.frame(Treatment_Name,N,Mean,Sd,SEM)

p_cul_SPH1<-ggplot(input_mean_frame,aes(x=as.factor(Treatment_Name),y=Mean))+   
  geom_bar(position =position_dodge(0.6),width = 0.5,stat = "identity", color='#000000',alpha=0.8)+
  geom_errorbar(aes(ymin=Mean-Sd,ymax=Mean+Sd),size=0.2,width=0.1)+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.55))+
  geom_signif(comparisons = list(c("RSA89+SA", "RSA89")), 
              map_signif_level = TRUE, 
              annotations = c('****'),
              tip_length = 0.2,
              y_position = 0.5, 
              textsize = 4,
              vjust = 0.1)+
  labs(x='',fill='',y='Pathogen abundance')+
  theme_zg()+ 
  theme(legend.position='None')
p_cul_SPH1

##patch####
p_DR_SPH1/p_invader/p_cul_SPH1
ggsave("./Figures/Fig.5.ABC.pdf",width = 3,height = 9)

#Figure S5####
SA_effect=read.csv("./sourcedata/6.Data_FIG.S5.csv",header=T)
mean_frame<- aggregate(SA_effect[,2],by=list(SA_effect[,1]),FUN=mean)
Sd<- aggregate(SA_effect[,2],by=list(SA_effect[,1]),FUN=sd) %>% .[,'x']
Treatment_Name<- mean_frame$Group.1
N<- table(SA_effect[,1]) %>% as.numeric()
Mean<- mean_frame[,'x']
SEM<- Sd/(N^0.5)
input_mean_frame<- data.frame(Treatment_Name,N,Mean,Sd,SEM)

p_SAeffect<-ggplot(input_mean_frame,aes(x=as.factor(Treatment_Name),y=Mean))+   
  geom_bar(position =position_dodge(0.6),width = 0.5,stat = "identity", color='#000000',alpha=0.8)+
  geom_errorbar(aes(ymin=Mean-Sd,ymax=Mean+Sd),size=0.2,width=0.1)+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.75))+
  geom_signif(comparisons = list(c("RSA89+SA", "RSA89"),c("R.sola+SA","R.sola")), 
              map_signif_level = TRUE, 
              annotations = c('ns'),
              tip_length = 0.2,
              y_position = 0.65, 
              textsize = 4,
              vjust = 0.1)+
  labs(x='',fill='',y='Pathogen abundance')+
  theme_zg()+ 
  theme(legend.position='None')

ggsave(p_SAeffect,file='./Supplement Figures/Fig.S5.pdf',width = 3,height = 2)

#Figure S6####
motility_SPH1 <- read.csv('./sourcedata/6.Data_FIG.S6.csv',header = T)

p_motility_SPH1 <- ggboxplot(motility_SPH1, x='Type', # 数据集的Species列作为横坐标
                             y='Value',  # 数据集的value列作为纵坐标
                             color = 'State',
                             palette = 'nejm',add = 'jitter') +
  scale_color_manual(values = c("#6F99AD","#BF5960"))+
  stat_compare_means(aes(group=State), method = "wilcox.test", #像这种非配对的非正态分布的数据，可以用非参数检验方法，这里选的是非参的其中一种，叫秩和检验
                     size=4, # 将p值字体改小一些
                     label = "p.signif") + theme_test()+
  scale_y_continuous(expand = c(0.1,0.1)) +
  labs(y="Diameter (mm)", x="") +
  theme(axis.text = element_text(color="black", size=10),
        axis.line = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
  ) 
p_motility_SPH1
ggsave(p_motility_SPH1,file='./Supplement Figures/Fig.S6.pdf',width = 4,height = 3)

