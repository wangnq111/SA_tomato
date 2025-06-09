library(vegan)
if (!require(LorMe)) install.packages("LorMe")
library(LorMe)
if (!require(pairwiseAdonis)) devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
library(ggplot2)
#community analysis####
##data loading####
asv <- read.csv('./sourcedata/2.asv.csv',header=T)
Metafile=read.table("./sourcedata/3.Metafile.txt",header = T,sep="\t")

##data cleaning####
asv_clean <- Filter_function(input = asv,threshold = 3,format = 2)

##packaging####
summary_object=tax_summary(inputtable =asv_clean[,2:25],groupfile = Metafile,reads = T,taxonomytable = asv_clean[,c(1,26)],outputtax = "standard")

##config####
color_combine=color_scheme("Plan3") %>% .[c(1,2,5,6)]
names(color_combine)=c("Rhizosphere_SA","Soil_SA","Soil_Water","Rhizosphere_Water")
summary_object_plan1=object_config(taxobj = summary_object,treat_location = 5,rep_location = 4,treat_col = color_combine,treat_order = names(color_combine)[c(2,3,1,4)])

sub_pot_object=sub_tax_summary(summary_object_plan1,Site=="Rhizosphere")
sub_soil_object=sub_tax_summary(summary_object_plan1,Site=="Soil")

#Fig.2####
###alpha diveristy####
alpha_frame=Alpha_diversity_calculator(taxobj = summary_object_plan1,taxlevel = "Base")
alpha_frame$alphaframe$Combine_tag=factor(alpha_frame$alphaframe$Combine_tag,levels = c("Soil_Water","Soil_SA","Rhizosphere_Water","Rhizosphere_SA"))
inputdata=subset(alpha_frame$alphaframe,Indexname %in% c("ACE","Shannon"))
alpha_plot=ggplot(inputdata,aes(x=as.factor(inputdata[,5]),y=inputdata[,7]))+
  scale_y_continuous(expand = c(0.06,0.01))+
  scale_color_manual(values=color_combine)+
  scale_fill_manual(values=color_combine)+
  theme_zg()+
  facet_wrap(~get(colnames(inputdata)[6]),scales ='free_y',strip.position ='top',as.table =T)+
  geom_boxplot(aes(fill=factor(inputdata[,5])),alpha=0.8,width=0.5,outlier.color =NA,color='#000000',linewidth=0.2)+
  labs(x='',fill='',y="")+
  theme(legend.position = 'right')+guides(fill="none")
alpha_plot

###structure####
structure <- structure_plot(taxobj = summary_object_plan1,taxlevel = "Base",diagram = "ellipse",ellipse.level = 0.6)
pcoa_coord=structure$PCoA_coordinates

pcoa=ggplot(data = pcoa_coord, aes(x = pcoa_coord[, 1],y = pcoa_coord[, 2])) +
  labs(x = "Pco1:13.51%",y = "Pco2:8.19%",fill="",shape="")+
  geom_hline(yintercept = 0, linetype = 4,color = "grey") + 
  geom_vline(xintercept = 0, linetype = 4,color = "grey")+
  stat_ellipse(aes(color=Combine_tag),level = 0.6,lty=2,show.legend = F)+
  geom_point(size = 2, alpha = 0.8,color="black",aes(shape=Site,fill=Combine_tag))+
  scale_shape_manual(values=c(21,24))+
  scale_fill_manual(values = color_combine)+
  scale_color_manual(values = color_combine)+
  theme_zg()+guides(color="none",shape="none",fill="none")

set.seed(999)
adonis2(t(summary_object_plan1$Base[,-1])~Site*Treatment,data=summary_object_plan1$Groupfile,by = "terms") #overall

set.seed(999)
pairwise.adonis(x = t(summary_object_plan1$Base[,-1]), factors = summary_object_plan1$Groupfile$Combine_tag, sim.method = 'bray', perm = 999, p.adjust.m = 'holm')

###composition####
composition=community_plot(taxobj = summary_object_plan1,taxlevel = "Phylum",n = 10,palette = "Paired",rmprefix = " p__")
com_plot=composition$barplot
(alpha_plot|pcoa)/com_plot
##Visualization####
ggsave("./Figures/Fig.2.pdf",width = 10,height = 8)
save(summary_object_plan1,file="./sourcedata/4.taxsummary.rda")
