
#Install packages
install.packages(c("broom.mixed","lmer4", "lmertest", "Gmisc", "tidyverse"))

#Load packages
library(broom.mixed)
library(lme4)
library(lmerTest)
library(Gmisc)
library(tidyverse)


#Load df
main_dir=" " # add path to df

df <- read_csv(pathJoin(main_dir,"df.csv"))

### MIXED MODEL Functional Connectivty###

#subset df for analysis
df_fc<-df  %>% 
  filter(analysis=='network') # choose network or task/category sensitive target ROIs 'network' or 'task'

# run mixed model
mod<-lmer(z_post_pre~ 0 + roi_pair+age_group_demean+ time_demean +fd_mean_demean + (1|id), data=df_fc)

# tidy model output and correct p-values of the variables in interest
tidymod<-tidy(mod, conf.int=T) 
tidymod<- tidymod %>% filter(!is.na(statistic)) %>% filter(!term=='fd_mean_demean', !term=='Time_demean') %>%
  mutate(pfdr=p.adjust(p.value, method = "fdr", n = length(p.value)))



### PLOT FIGURES ###
pal<-c('#4c72b0','#dd8452') #colours

mytheme<- theme(axis.text.x=element_text(size=12,angle= 90),
                panel.background = element_blank(),
                legend.background = element_blank(),
                legend.key= element_blank(),
                legend.key.size = unit(1, 'cm'), 
                legend.key.height = unit(1, 'cm'), 
                legend.key.width = unit(1, 'cm'),
                legend.title = element_text(size=14), 
                legend.text = element_text(size=12),
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_line(colour = "black"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size= 18, margin = margin(r=20)),
                axis.line.y = element_line( colour = "black"),
                axis.text.y=element_text(size=12,angle= 0))

        
#Boxplot raw (z) change in rsFC for the stimuli sensitive ROI
df  %>%
  filter(analysis=='task') %>% 
  mutate(
    age_group=factor(age_group, levels=c("young", "old")),
    roi_pair= case_when (
    roi_pair=="Hippocampus_FFA"~"FFA", 
    roi_pair=="Hippocampus_PPA"~"PPA",
    TRUE ~ "LOC")) %>%
  ggplot(aes(x=reorder(roi_pair, -z_post_pre), y=z_post_pre), group=age_group)+
  stat_boxplot(geom ='errorbar', width=0.2, size=0.2,aes(fill=age_group),position=position_dodge(width=0.75)) +
  geom_boxplot(aes(fill=age_group),outlier.shape = NA,position=position_dodge(width=0.75))+ #outlier.shape=NA to avoid double points when adding geomjitter
  geom_jitter(aes(fill=age_group),position=position_jitterdodge(dodge.width=0.9), alpha = 0.5, size=0.2)+
  scale_fill_discrete(type=pal, name=NULL, labels = c("YA", "OA"))+
  coord_cartesian(ylim=c(-1, 1.00), expand=F)+
  labs(y= " Post-encoding change in hippocampal rsFC(z)"  )+
  mytheme 
 

#Boxplot raw (z) change in rsFC forYeo7 network
df %>%
  filter(analysis=='network') %>% 
  mutate(age_group=factor(age_group, levels=c("young", "old"))) %>% 
  ggplot(aes(x=reorder(roi_pair, -z_post_pre), y=z_post_pre), group=age_group)+
  stat_boxplot(geom ='errorbar', width=0.3, size=0.2,aes(fill=age_group),position=position_dodge(width=0.75)) +
  geom_boxplot(aes(fill=age_group),outlier.shape = NA,position=position_dodge(width=0.75), size=0.2)+ #outlier.shape=NA to avoid double points when adding geomjitter
  geom_jitter(aes(fill=age_group),position=position_jitterdodge(dodge.width=0.9), alpha = 0.5, size=0.2)+
  scale_fill_discrete(type=pal, name=NULL, labels = c("YA", "OA"))+
  scale_x_discrete(labels=c("Hippocampus_Default" = "Default", "Hippocampus_DorsalAttention" = "DorsAttn",
                            "Hippocampus_FrontoParietal" = "FrontPar", "Hippocampus_Limbic" = "Limbic", "Hippocampus_Visual" = "Vis",
                            "Hippocampus_Somatomotor" = "SomMot", "Hippocampus_VentralAttention" = "SalVentAttn"))+
  coord_cartesian(ylim=c(-1, 1.0), expand=F)+
  labs(y= " Post-encoding change in hippocampal rsFC(z)"  )+
  mytheme  



### MEMORY MIXED MODELS ###

df_memory<-df %>% 
  filter(analysis=="task") # choose network or task/category sensitive target ROIs 'network' or 'task'
wide<-df_memory %>%
    pivot_wider(names_from=c("roi_pair"), values_from="z_post_pre", names_sep="_") # make wider for correct sample size when predicting memory

#single seed example - Choose test and roi pair
modmem<-lmer(afc1~ Hippocampus_PPA + age_group_demean+time_demean+ (1|id), data=wide)
summary(modmem)


