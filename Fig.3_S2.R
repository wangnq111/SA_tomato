library(ggtree) # an R package for visualization of tree and annotation data
library(ape) # Analyses of Phylogenetics and Evolution
library(vegan) # Community Ecology Package
library(ggtreeExtra) # An R Package To Add Geometric Layers On Circular Or Other Layout Tree Of "ggtree"
library(ggnewscale) # Multiple Fill and Colour Scales in 'ggplot2'
library(ggstar) 
library(picante)
library(circlize)
library(stringr)
library(tidyr)
library(dplyr)
if (!require(LorMe)) install.packages("LorMe")
library(LorMe)
library(ggplot2)
library(ggpubr)
library(patchwork)

#data loading####
load("./sourcedata/4.taxsummary.rda")

#Fig.S2####
##indicator analysis
set.seed(0)
indicator<-indicator_analysis(taxobj =summary_object_plan1,taxlevel = "Species" )
indicator_anno=subset(indicator,padj<0.05)

#configuration
phylum_col=community_plot(taxobj = summary_object_plan1,taxlevel = "Phylum",n = 10,palette = "Paired") %$%
  filled_color %>%
  c(.,rep("#B15928",10))
names(phylum_col)[11:16]=
  unique(indicator_anno$Phylum)[!unique(indicator_anno$Phylum) %in% names(phylum_col)[1:10]]
phylum_col=phylum_col[1:16]
indicator_anno_long=gather(indicator_anno[,c(1:4,8)],"tag","value",-SpeciesID) %>% subset(.,value!=0)
indicator_anno_long=left_join(indicator_anno_long,summary_object_plan1$Species_taxonomy)
indicator_anno_long$combine=indicator_anno_long$tag
indicator_anno_long=separate(indicator_anno_long,col = combine,into = c("Site","State"),sep = "_")
col_temp=color_scheme("Plan3") %>% .[c(1,2,5,6)]
names(col_temp)=c("s.Rhizosphere_SA","s.Soil_SA","s.Soil_Water","s.Rhizosphere_Water")

###sort
indicator_anno_long1=data.frame()
for(i in unique(indicator_anno_long$tag)){
  for(j in names(phylum_col)){
    indicator_anno_long1=rbind(indicator_anno_long1,subset(indicator_anno_long,tag==i&Phylum==j))
  }
}

##add phylum color
for(i in unique(indicator_anno_long1$Phylum)){
  indicator_anno_long1$phylumcol[indicator_anno_long1$Phylum==i]=phylum_col[which(names(phylum_col)==i)]
}

##add genus col
genus_col=color_scheme("Plan10",8)
names(genus_col)=table(indicator_anno_long1$Genus) %>% sort() %>% names() %>% tail(8)
indicator_anno_long1$genuscol="transparent"
for(i in names(genus_col)){
  indicator_anno_long1$genuscol[indicator_anno_long1$Genus==i]=genus_col[which(names(genus_col)==i)]
}

#add relative abundance
i=1
while(i <=nrow(indicator_anno_long1)){
  ID=indicator_anno_long1$SpeciesID[i]
  state=indicator_anno_long1$State[i]
  site=indicator_anno_long$Site[i] %>% str_remove(.,pattern="s.")
  Base_percent=summary_object_plan1$Species_percent
  Base_percent$Species=summary_object_plan1$Species_taxonomy$SpeciesID
  indicator_anno_long1$rel[i]=
    Base_percent[which(Base_percent$Species==ID),which(summary_object_plan1$Groupfile$Site==site&summary_object_plan1$Groupfile$Treatment==state)+1] %>%
    as.numeric() %>% mean()
  i=i+1
} #add relative abundance


#trans to numeric
for(i in unique(indicator_anno_long1$tag)){
  indicator_anno_long1$statvalue[indicator_anno_long1$tag==i]=1:length(which(indicator_anno_long1$tag==i))
}

##add specific tag
specific_ID=c(indicator_anno$SpeciesID[indicator_anno$s.Rhizosphere_SA==1&indicator_anno$s.Rhizosphere_Water==0&indicator_anno$s.Soil_SA==0&indicator_anno$s.Soil_Water==0],
              indicator_anno$SpeciesID[indicator_anno$s.Rhizosphere_SA==0&indicator_anno$s.Rhizosphere_Water==1&indicator_anno$s.Soil_SA==0&indicator_anno$s.Soil_Water==0],
              indicator_anno$SpeciesID[indicator_anno$s.Rhizosphere_SA==0&indicator_anno$s.Rhizosphere_Water==0&indicator_anno$s.Soil_SA==1&indicator_anno$s.Soil_Water==0],
              indicator_anno$SpeciesID[indicator_anno$s.Rhizosphere_SA==0&indicator_anno$s.Rhizosphere_Water==0&indicator_anno$s.Soil_SA==0&indicator_anno$s.Soil_Water==1])

indicator_anno_long1$sptag="None"
indicator_anno_long1$sptag[indicator_anno_long1$tag=="s.Rhizosphere_SA"&indicator_anno_long1$SpeciesID %in%specific_ID]="SP" 
indicator_anno_long1$sptag[indicator_anno_long1$tag=="s.Rhizosphere_Water"&indicator_anno_long1$SpeciesID %in%specific_ID]="SP"
indicator_anno_long1$sptag[indicator_anno_long1$tag=="s.Soil_SA"&indicator_anno_long1$SpeciesID %in%specific_ID]="SP"
indicator_anno_long1$sptag[indicator_anno_long1$tag=="s.Soil_Water"&indicator_anno_long1$SpeciesID %in%specific_ID]="SP"
indicator_anno_long1$sptag_col[indicator_anno_long1$tag=="s.Rhizosphere_SA"&indicator_anno_long1$sptag=="SP"]="#4070AF"
indicator_anno_long1$sptag_col[indicator_anno_long1$tag=="s.Rhizosphere_Water"&indicator_anno_long1$sptag=="SP"]="#D42C24"
indicator_anno_long1$sptag_col[indicator_anno_long1$tag=="s.Soil_SA"&indicator_anno_long1$sptag=="SP"]="#8CA5BB"
indicator_anno_long1$sptag_col[indicator_anno_long1$tag=="s.Soil_Water"&indicator_anno_long1$sptag=="SP"]="#F18159"
indicator_anno_long1$sptag_col[indicator_anno_long1$sptag=="None"]="transparent"
##visualization
pdf("./Supplement Figures/Fig.S2.pdf",width = 6,height = 6)
circos.par(gap.degree=c(1,1,1,30))
circos.initialize(indicator_anno_long1$tag, x=indicator_anno_long1$statvalue) #initialize

circos.track(indicator_anno_long1$tag, ylim=c(0,1), bg.col =col_temp[c(1,4,2,3)],track.height=.12,
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(-1), 
                           CELL_META$sector.index,niceFacing =F)
             }) #label layer
circos.track(indicator_anno_long1$tag, ylim=c(0,1),
             panel.fun = function(x, y) {
               #circos.text(CELL_META$xcenter, 
               #            CELL_META$cell.ylim[2] + mm_y(7), 
               #            CELL_META$sector.index)
               circos.axis(labels.cex = 0.6)
             })  ##text layer

for(i in unique(indicator_anno_long1$tag)){
  range=indicator_anno_long1$statvalue[indicator_anno_long1$tag==i]
  breaks = seq(min(range), max(range), by = 1)
  n_breaks = length(breaks)
  
  circos.rect(sector.index = i,breaks[-n_breaks], rep(0, n_breaks - 1),
              breaks[-1], rep(1, n_breaks - 1),
              col = indicator_anno_long1$phylumcol[indicator_anno_long1$tag==i], border = NA)    
} 
circos.track(indicator_anno_long1$tag, ylim=c(0,0.085),
             track.height=.1,bg.border = "transparent",bg.col="gray95")  #"transparent"

for (i in unique(indicator_anno_long1$tag)){
  circos.barplot(sector.index = i,
                 value = indicator_anno_long1[indicator_anno_long1$tag==i,]$rel,
                 pos = indicator_anno_long1[indicator_anno_long1$tag==i,]$statvalue,
                 col = col_temp[which(names(col_temp)==i)],
                 bar_width = 0.8,
                 border="transparent")
} #relative abundance bar
circos.yaxis(side =  "right",labels.cex = 0.4,labels = c(0,0.05,0.10))
circos.track(indicator_anno_long1$tag, ylim=c(0,1),track.height=.05,bg.border = "transparent")
for(i in unique(indicator_anno_long1$tag)){
  range=indicator_anno_long1$statvalue[indicator_anno_long1$tag==i]
  breaks = seq(min(range), max(range), by = 1)
  n_breaks = length(breaks)
  circos.rect(sector.index = i,breaks[-n_breaks], rep(0, n_breaks - 1),
              breaks[-1], rep(1, n_breaks - 1),
              col = indicator_anno_long1$sptag_col[indicator_anno_long1$tag==i], border = NA)    
} #genus color fill
circos.clear()
dev.off()

#Fig.3####
asvmean=aggregate(summary_object_plan1$Species_percent[,-1] %>% t(),by=list(summary_object_plan1$Groupfile$Combine_tag),FUN=mean)%>% 
  .[,-1]%>%t()%>% as.data.frame()
colnames(asvmean)=c("Rhizosphere_SA","Rhizosphere_Water","Soil_SA","Soil_Water")
rownames(asvmean)=summary_object_plan1$Species_taxonomy$SpeciesID
indicator_anno1=data.frame(indicator,asvmean)
indicator_anno1$y=-log10(indicator_anno1$padj)
indicator_anno1$size[indicator_anno1$s.Rhizosphere_SA==1]=log10(indicator_anno1$Rhizosphere_SA[indicator_anno1$s.Rhizosphere_SA==1])
indicator_anno1$size[indicator_anno1$s.Rhizosphere_Water==1]=log10(indicator_anno1$Rhizosphere_Water[indicator_anno1$s.Rhizosphere_Water==1])
indicator_anno1$size[indicator_anno1$s.Soil_SA==1]=log10(indicator_anno1$Soil_SA[indicator_anno1$s.Soil_SA==1])
indicator_anno1$size[indicator_anno1$s.Soil_Water==1]=log10(indicator_anno1$Soil_Water[indicator_anno1$s.Soil_Water==1])
indicator_anno1$sizetag[indicator_anno1$size>(-2)]="I"
indicator_anno1$sizetag[indicator_anno1$size>(-3)&indicator_anno1$size<(-2)]="II"
indicator_anno1$sizetag[indicator_anno1$size>(-4)&indicator_anno1$size<(-3)]="III"
indicator_anno1$sizetag[indicator_anno1$size<(-4)]="IIII"

indicator_anno1$x=indicator_anno1$stat  
indicator_anno1=indicator_anno1[which(rowSums(indicator_anno1[,1:4])==1),] #only keep specific
indicator_anno1$y[indicator_anno1$s.Rhizosphere_Water==1]=-indicator_anno1$y[indicator_anno1$s.Rhizosphere_Water==1]
indicator_anno1$x[indicator_anno1$s.Soil_SA==1|indicator_anno1$s.Soil_Water==1]=-indicator_anno1$x[indicator_anno1$s.Soil_SA==1|indicator_anno1$s.Soil_Water==1]
indicator_anno1$y[indicator_anno1$s.Soil_Water==1]=-indicator_anno1$y[indicator_anno1$s.Soil_Water==1]
indicator_anno1$tag[indicator_anno1$s.Rhizosphere_SA==1]="Rhizosphere_SA"
indicator_anno1$tag[indicator_anno1$s.Rhizosphere_Water==1]="Rhizosphere_Water"
indicator_anno1$tag[indicator_anno1$s.Soil_SA==1]="Soil_SA"
indicator_anno1$tag[indicator_anno1$s.Soil_Water==1]="Soil_Water"
indicator_anno1$tag[indicator_anno1$padj>0.05]="none"

anno_text=subset(indicator_anno1,stat>0.6&abs(y)>(2.5)&tag!="none")  #&size>(-2.5)
vol=ggplot(indicator_anno1,aes(x=x,y=y,fill=tag))+
  geom_rect(xmin = 0, xmax = 1, ymin = -3.1, ymax = 3.1, fill =  "#FBFAED",alpha=.1)+
  geom_hline(aes(yintercept=0),lty=2)+
  geom_vline(aes(xintercept=0),lty=2)+
  geom_point(aes(size=sizetag),pch=21,color="black",alpha=.8)+
  geom_text(data=anno_text,aes(x=x+0.05,y=y+0.05,label=Genus),size=2)+
  scale_size_manual(values = c("I"=2.5,"II"=2,"III"=1,"IIII"=.5))+
  scale_fill_manual(values = c(summary_object_plan1$configuration$treat_col,"none"="gray"))+
  theme_zg()+
  labs(x="Enrichement factor",y="-log10(qvalue)")+
  theme(legend.position = 'right',
        #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9),
        #axis.text.x=element_blank(),
        panel.background=element_rect(color="transparent"),
        axis.ticks.x = element_blank()
  )+
  guides(color='none',fill='none',size="none")

###barplot
genus_long=summary_object_plan1$Species_percent[which(summary_object_plan1$Species_percent$Species %in% (anno_text$Species[anno_text$tag=="Rhizosphere_SA"] %>% .[c(6,4,3,1)])),] %>%
  gather(.,"Samplen_num","Rel",-Species)
genus_long$Samplen_num=str_remove_all( genus_long$Samplen_num,pattern = "X") %>% as.numeric()
genus_long=left_join(genus_long,summary_object_plan1$Groupfile)
my_comparsion=list(c("Soil_Water","Soil_SA"),
                   c("Rhizosphere_Water","Rhizosphere_SA"))

genus_long$Species=factor(genus_long$Species,levels=unique(genus_long$Species))
genus_long$Combine_tag=factor(genus_long$Combine_tag,levels=c("Soil_Water","Soil_SA","Rhizosphere_Water","Rhizosphere_SA"))
box=ggplot(genus_long,aes(x=as.factor(Combine_tag),y=Rel))+
  geom_rect(xmin = 2.5, xmax = 5, ymin = -Inf, ymax = Inf, fill =  "#FBFAED",alpha=.1)+
  geom_rect(xmin = 0, xmax = 2.5, ymin = -Inf, ymax = Inf, fill =  "white",alpha=.1)+
  scale_y_continuous(labels = scales::percent,expand = c(0.05,0.01))+
  scale_color_manual(values=summary_object_plan1$configuration$treat_col)+
  scale_fill_manual(values=summary_object_plan1$configuration$treat_col)+
  theme_zg()+
  facet_wrap(~get(colnames(genus_long)[1]),scales ='free_y',strip.position ='top',as.table =T,nrow=1)+
  geom_boxplot(aes(fill=factor(Combine_tag)),alpha=0.8,width=0.5,outlier.color =NA,color='#000000',linewidth=0.2)+
  labs(x='',fill='',y='Relative abundace')+
  stat_compare_means(aes(label = after_stat(p.signif)),comparisons = my_comparsion,method = "wilcox.test")+
  theme(legend.position = 'right',
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9),
        panel.background=element_rect(color="transparent"),
        axis.ticks.x = element_blank())+
  guides(color='none',fill='none')
(plot_spacer()|vol)/box+plot_layout(heights = c(2,1))
ggsave("./Figures/Fig.3.pdf",width = 6,height = 6)

save(indicator_anno,indicator,file="./sourcedata/5.indicator_result.rda")
