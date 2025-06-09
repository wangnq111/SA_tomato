library(ggplot2)
library(ggpubr)
library(magrittr)
if (!require(LorMe)) install.packages("LorMe")
library(LorMe)
#Fig 1####
##data loading####
Exudate_disease=read.table("./sourcedata/1.Exudate_disease.txt",header = T,sep="\t")

##configuration####
Exudate_disease$Abbreviation=factor(Exudate_disease$Abbreviation,levels =Exudate_disease$Abbreviation%>%rev() )
effect_col=color_scheme("Plan3") %>% .[c(1,3,6)]
names(effect_col)=c("Inhibition","Neutral","Promotion")

##Visualization####
ggplot(Exudate_disease,aes(x=Disease/100,y=Abbreviation))+
  geom_point(size = 2, alpha = 0.8, pch = 21, color = "black",aes(fill=Label))+
  scale_fill_manual(values =effect_col )+
  scale_color_manual(values =effect_col )+
  geom_segment(aes(x=Disease/100-0.002,xend = 0, color = Label), show.legend = FALSE, size = 0.4, alpha = 0.8)+
  geom_vline(xintercept = 0.5,lty=2)+
  theme_zg()+
  labs(x="Disease index",y="substance",fill="In vitro effect")+theme(legend.position = c(0.8,0.8))
ggsave("./Figures/Fig1a.pdf",width = 5,height = 4)

#Fig S1####
SA_sterilized=read.csv("./sourcedata/1.Data_FIGS1.csv",header = T)
mean_frame<- aggregate(SA_sterilized[,2],by=list(SA_sterilized$Group),FUN=mean)
Sd<- aggregate(SA_sterilized[,2],by=list(SA_sterilized$Group),FUN=sd) %>% .[,'x']
Treatment_Name<- mean_frame$Group.1
N<- table(SA_sterilized[,1]) %>% as.numeric()
Mean<- mean_frame[,'x']
SEM<- Sd/(N^0.5)
input_mean_frame<- data.frame(Treatment_Name,N,Mean,Sd,SEM)
ggplot(input_mean_frame,aes(x=as.factor(Treatment_Name),y=Mean))+
  theme_zg()+
  geom_bar(stat = 'identity',size=0.2,width=0.5,fill="gray",color="black",linewidth=.5)+
  geom_errorbar(aes(ymin=Mean-Sd,ymax=Mean+Sd),size=0.4,width=0.1,linewidth=.5)+
  #geom_signif(comparisons = list("CK","SA"),test="t.test")+
  stat_compare_means(method="wilcox.test",label.x.npc = 0.5,label.y.npc = 0.85,aes(label = after_stat(p.signif)))+
  scale_y_continuous(expand = c(0,0.0),limits = c(0,0.7))+
  labs(x='',fill='',y='Disease index')+
  theme(legend.position = 'right')

ggsave("./Supplement Figures/Fig.S1.pdf",width = 3,height = 3)
