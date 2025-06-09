if (!require(LorMe)) install.packages("LorMe")
library(LorMe)
library(patchwork)
#data loading####
load("./sourcedata/4.taxsummary.rda")
load("./sourcedata/5.indicator_result.rda")

##tag edition####
indicator$tag="None"
indicator$tag[indicator$s.Rhizosphere_SA==1&indicator$s.Rhizosphere_Water==0&indicator$s.Soil_SA==0&indicator$s.Soil_Water==0&indicator$padj<0.05]="Rhizosphere_SA"
indicator$tag[indicator$s.Rhizosphere_SA==0&indicator$s.Rhizosphere_Water==1&indicator$s.Soil_SA==0&indicator$s.Soil_Water==0&indicator$padj<0.05]="Rhizosphere_Water"
indicator$tag[indicator$s.Rhizosphere_SA==0&indicator$s.Rhizosphere_Water==0&indicator$s.Soil_SA==1&indicator$s.Soil_Water==0&indicator$padj<0.05]="Soil_SA"
indicator$tag[indicator$s.Rhizosphere_SA==0&indicator$s.Rhizosphere_Water==0&indicator$s.Soil_SA==0&indicator$s.Soil_Water==1&indicator$padj<0.05]="Soil_Water"

#network analysis####
network_results=network_analysis(taxobj = summary_object_plan1,taxlevel = "Species",reads = T,n =6,threshold = 0.6)  #ava3,can use
network_nodes=network_results$Nodes_info
#Visualization
set.seed(1)
network_initial=network_withdiff(network_obj = network_results,diff_frame = indicator,aes_col =c(summary_object_plan1$configuration$treat_col) )


network_visual_re(network_visual_obj =network_initial,module_paint = T,module_num = c(2,4,5,8),module_palette =color_scheme(c("aquamarine3","antiquewhite2","goldenrod2"),7)%>%.[1:4]) 
#save width 8 x height 4

##module analysis####
Base_percent=summary_object_plan1$Species_percent
Module_visual=function(Module){
  Module_rel=Base_percent[which(Base_percent$Species %in% network_nodes$Species[network_nodes$No.module==Module]),-1] %>% colSums() 
  compare_frame=data.frame(summary_object_plan1$Groupfile,Module_rel)
  my_comparisons =list(c("Rhizosphere_Water","Rhizosphere_SA"),c("Soil_Water","Soil_SA"))
  compare_frame$Combine_tag=factor(compare_frame$Combine_tag,levels=c("Soil_Water","Soil_SA","Rhizosphere_Water","Rhizosphere_SA"))
  compare=ggplot(compare_frame,aes(x=as.factor(compare_frame[,5]),y=compare_frame[,6]))+
    geom_rect(xmin = 2.5, xmax = 5, ymin = -Inf, ymax = Inf, fill =  "#FBFAED",alpha=.1)+
    geom_rect(xmin = 0, xmax = 2.5, ymin = -Inf, ymax = Inf, fill =  "white",alpha=.1)+
    scale_y_continuous(labels = scales::percent,expand = c(0.05,0.01))+
    scale_color_manual(values=summary_object_plan1$configuration$treat_col)+
    scale_fill_manual(values=summary_object_plan1$configuration$treat_col)+
    theme_zg()+
    geom_boxplot(aes(fill=factor(compare_frame[,5])),alpha=0.8,width=0.5,outlier.color =NA,color='#000000',linewidth=0.2)+
    labs(x='',fill='',y='',title=paste("Module#",Module))+
    stat_compare_means(aes(label = after_stat(p.signif)),comparisons = my_comparisons,method = "wilcox.test")+
    theme(legend.position = 'right',
          #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9),
          axis.text.x=element_blank(),
          panel.background=element_rect(color="transparent"),
          axis.ticks.x = element_blank(),plot.title = element_text(hjust=0.5))+
    guides(color='none',fill='none')
  compare %>% return()
}
## Fig. S3####
(Module_visual(2)|
    Module_visual(4)|
    Module_visual(5)) 

ggsave("./Supplement Figures/Fig.S3.pdf",width = 6,height = 2.5)

##Fig. 4b&c####
M8=network_nodes[network_nodes$No.module==8,]
M8$rel=Base_percent[Base_percent$Species %in% M8$Species,] %>% .[,-1] %>% rowMeans()
M8_comp=aggregate(t(Base_percent[Base_percent$Species %in% M8$Species,] %>% .[,-1]),
                  by=list(summary_object_plan1$Groupfile$Combine_tag),FUN=mean)
colnames(M8_comp)[-1]=Base_percent[Base_percent$Species %in% M8$Species,] %>% .[,1]
Species_order=colMeans(M8_comp[,2:10]) %>% sort() %>% names() %>% rev() %>% .[c(5,1:4,6:9)]
M8_comp_long=gather(M8_comp,"tax","rel",-Group.1)
M8_comp_long$Group.1=factor(M8_comp_long$Group.1,levels =c("Soil_Water","Soil_SA","Rhizosphere_Water","Rhizosphere_SA") )
M8_comp_long$tax=factor(M8_comp_long$tax,levels=Species_order)

require(ggalluvial)
M8_all=ggplot(M8_comp_long, aes(x = as.factor(Group.1), y = rel, fill = tax, 
                                stratum = tax, alluvium = tax)) + 
  geom_col(position = "stack", width = 0.8)+
  geom_flow(alpha = 0.5, width = 0.1)+
  scale_fill_manual(values = color_scheme("Plan9"))+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),labels = scales::label_percent())+
  labs(x="",y="Relativa abundance")+
  theme_zg()+
  theme(legend.text = element_text(size = 8, face = "bold"), 
        strip.text = element_text(size = 8, face = "bold"), 
        axis.ticks.x = element_blank(), panel.background = element_rect(color = NA), 
        axis.line.y = element_line(size = 0.4, color = "black"), 
        axis.ticks.length.y = unit(0.4, "lines"), axis.ticks.y = element_line(color = "black", size = 0.4))+
  guides(fill="none")

((plot_spacer()/(Module_visual(8)|M8_all))+plot_layout(heights =   c(1.2,1)))
ggsave("./Figures/Fig.4b.pdf",width = 5,height = 4.4)
