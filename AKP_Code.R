######################################

################ Figure1A
rm(list = ls())
setwd("F:/plot")
getwd()
library(dplyr)
mm<-read.csv("mean.csv")
mm <- mm %>% 
  filter(Compartment == "Stem")
mm<-mm[-2]
mm<-mm[-3]
mm<-mm[1:3]
m1<-mm
head(m1)
mw <- dcast(m1, State ~ Site, value.var = "Richness")  
df1<-mw
rownames(df1)<-df1$State
df1<-df1[-1]
df1<-as.data.frame(df1)
df<- df1[, colSums(is.na(df1)) == 0]  
min(df)
max(df)
df1<-df
df1_tmp <- rbind(rep(380,30), rep(-20,30), df1)  
library(fmsb) 
pdf("Stem1.pdf",width=6,height=6)
mycol <- c("#f26115","#194a55")
p1 <- radarchart(df1_tmp,axistype = 1,pty = 32,caxislabels = seq(-20,380,100),
                 pcol = mycol,pfcol = alpha(mycol,0.1),plwd = 3.5, 
                 plty = 1,
                 cglty = 1, 
                 cglcol = 'grey',axislabcol = 'black',
                 centerzero =T, 
                 vlcex = 0.9)
dev.off()

################ Figure1B
rm(list = ls())
setwd("F:/plot")
getwd()
library(dplyr)
library(vegan)
library(randomForest)
library(rfPermute)
library(ggplot2)
library(tidyverse)
library(patchwork)
w<-read.csv("whole.csv",row.names = 1)
w1 <- w %>% 
  filter(Compartment != "Soil")
set.seed(1)
RF<- randomForest(Richness~., w1[,-2:-5], importance = T)

RFs<- rfPermute(Richness~., w1[,-2:-5], nperm=99, ntree=501)

df1<- subset(w1, gro3 == "RhizosphereHealthy")[,-2:-5]
set.seed(1)
RF1<- randomForest(Richness~., df1, importance = T)
RF1s<- rfPermute(Richness~., df1, nperm=99, ntree=501)

df2<- subset(w1, gro3 == "RhizosphereDisease")[,-2:-5]
set.seed(1)
RF2<- randomForest(Richness~., df2, importance = T)
RF2s<- rfPermute(Richness~., df2, nperm=99, ntree=501)

df3<- subset(w1, gro3 == "RootHealthy")[,-2:-5]
set.seed(1)
RF3<- randomForest(Richness~., df3, importance = T)
RF3s<- rfPermute(Richness~., df3, nperm=99, ntree=501)

df4<- subset(w1, gro3 == "RootDisease")[,-2:-5]
set.seed(1)
RF4<- randomForest(Richness~., df4, importance = T)
RF4s<- rfPermute(Richness~., df4, nperm=99, ntree=501)

df5<- subset(w1, gro3 == "StemHealthy")[,-2:-5]
set.seed(1)
RF5<- randomForest(Richness~., df5, importance = T)
RF5s<- rfPermute(Richness~., df5, nperm=99, ntree=501)


df6<- subset(w1, gro3 == "StemDisease")[,-2:-5]
set.seed(1)
RF6<- randomForest(Richness~., df6, importance = T)
RF6s<- rfPermute(Richness~., df6, nperm=99, ntree=501)


df7<- subset(w1, gro3 == "SeedHealthy")[,-2:-5]
set.seed(1)
RF7<- randomForest(Richness~., df7, importance = T)
RF7s<- rfPermute(Richness~., df7, nperm=99, ntree=501)


df8<- subset(w1, gro3 == "SeedDisease")[,-2:-5]
set.seed(1)
RF8<- randomForest(Richness~., df8, importance = T)
RF8s<- rfPermute(Richness~., df8, nperm=99, ntree=501)

RF

bar<- data.frame(variable = unique(w1$gro3),
                 Exp = c(68.94, 69.58, 64.03, 57.82, 79.07, 91.18,85.09,88.53))#整理来自上文的%Var explained数据
bar$variable<- factor(bar$variable, levels = bar$variable)
barplot<- ggplot(bar, aes(variable, Exp))+
  geom_bar(stat = "identity", fill = "#83ba9e")+
  scale_y_continuous(expand = c(0,0))+
  theme_classic(base_line_size = 0.75)+
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(color = "black", size = 12))+
  labs(title = "Explained variation (%)", size = 12)
barplot

library(reshape2)
r<- data.frame(ENV = colnames(w1)[6:22],
               RhizosphereHealthy = (cor(df1))[1,2:18],
               RhizosphereDisease = (cor(df2))[1,2:18],
               RootHealthy = (cor(df3))[1,2:18],
               RootDisease = (cor(df4))[1,2:18],
               StemHealthy = (cor(df5))[1,2:18],
               StemDisease = (cor(df6))[1,2:18],
               SeedHealthy = (cor(df7))[1,2:18],
               SeedDisease = (cor(df8))[1,2:18])%>%
  melt(id = "ENV", value.name = "Correlation")
r
r$ENV<- factor(r$ENV, levels = rev(colnames(w1)[6:22]))


circle<- data.frame(ENV = colnames(w1)[6:22]) %>%
  left_join(data.frame(ENV = row.names(importance(RF1s)),
                       RhizosphereHealthy = ifelse(importance(RF1s)[,2]<0.05,
                                                   importance(RF1s)[,1], NA))) %>%
  left_join(data.frame(ENV = row.names(importance(RF2s)),
                       RhizosphereDisease = ifelse(importance(RF2s)[,2]<0.05,
                                                   importance(RF2s)[,1], NA))) %>%
  left_join(data.frame(ENV = row.names(importance(RF3s)),
                       RootHealthy  = ifelse(importance(RF3s)[,2]<0.05,
                                             importance(RF3s)[,1], NA))) %>%
  left_join(data.frame(ENV = row.names(importance(RF4s)),
                       RootDisease = ifelse(importance(RF4s)[,2]<0.05,
                                            importance(RF4s)[,1], NA))) %>%
  left_join(data.frame(ENV = row.names(importance(RF5s)),
                       StemHealthy = ifelse(importance(RF5s)[,2]<0.05,
                                            importance(RF5s)[,1], NA))) %>%
  left_join(data.frame(ENV = row.names(importance(RF6s)),
                       StemDisease = ifelse(importance(RF6s)[,2]<0.05,
                                            importance(RF6s)[,1], NA))) %>%
  left_join(data.frame(ENV = row.names(importance(RF7s)),
                       SeedHealthy = ifelse(importance(RF7s)[,2]<0.05,
                                            importance(RF7s)[,1], NA))) %>%
  left_join(data.frame(ENV = row.names(importance(RF8s)),
                       SeedDisease = ifelse(importance(RF8s)[,2]<0.05,
                                            importance(RF8s)[,1], NA))) %>%
  melt(id = "ENV", value.name = "Importance");circle
circle$ENV<- factor(circle$ENV, rev(colnames(w1)[6:22]))

heatmap<- ggplot()+
  geom_tile(data = r, aes(x = variable, y = ENV, fill = Correlation))+ 
  scale_fill_gradientn(colors = c("#266b69",'white',"#ea894e"),
                       limit = c(-1, 1))+
  geom_point(data = circle, aes(x = variable, y = ENV, 
                                size = Importance), shape = 21)+ 
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, color = "black",
                                   size = 12, vjust = 0.6), 
        axis.text.y = element_text(color = 'black', size = 12),
        legend.title = element_text(size = 10))+
  labs(y = '', x = '')
heatmap

p1<-barplot + heatmap +
  plot_layout(ncol = 1, heights = c(1, 6))
p1

ggsave("随机森林相关性.pdf",p1,width = 6,height =10)



################ Figure1C
rm(list = ls())
getwd()
library(linkET)
library(ggplot2)
library(dplyr)
library(openxlsx)

rhh<-read.csv("RhizosphereHealthy.csv",row.names = 1)
rhh1<-rhh[-1]
rhh1<-rhh1[-2:-8]

rhd<-read.csv("RhizosphereDiseased.csv",row.names = 1)
rhd1<-rhd[-1]
rhd1<-rhd1[-2:-8]

roh<-read.csv("RootHealthy.csv",row.names = 1)
roh1<-roh[-1]
roh1<-roh1[-2:-8]

rod<-read.csv("RootDiseased.csv",row.names = 1)
rod1<-rod[-1]
rod1<-rod1[-2:-8]


sth<-read.csv("StemHealthy.csv",row.names = 1)
sth1<-sth[-1]
sth1<-sth1[-2:-8]

std<-read.csv("StemDiseased.csv",row.names = 1)
std1<-std[-1]
std1<-std1[-2:-8]

seh<-read.csv("SeedHealthy.csv",row.names = 1)
seh1<-seh[-1]
seh1<-seh1[-2:-8]

sed<-read.csv("SeedDiseased.csv",row.names = 1)
sed1<-sed[-1]
sed1<-sed1[-2:-8]

library(dplyr)  


merged1 <- left_join(rhh1, rhd1, by = "Si")  


merged2 <- left_join(merged1, roh1, by = "Si")  


merged3 <- left_join(merged2, rod1, by = "Si")  
merged4 <- left_join(merged3, sth1, by = "Si")  
merged5 <- left_join(merged4, std1, by = "Si")  
merged6 <- left_join(merged5, seh1, by = "Si")  
merged7 <- left_join(merged6, sed1, by = "Si")  
rownames(akk)<-akk$ID

group<-read.csv("group.csv")

e<-read.csv("Dis1.csv")
e<-e[-1]
e<-e[-2:-3]
ww<-merge(e,merged7,by="Si")


ww1<-ww
write.csv(ww1,"ww1.csv")

ww1[is.na(ww1)] <- 0
# Mantel test
ww2<-ww1
rownames(ww2)<-ww2$Si
ww2<-ww2[-1]

otu<-ww2[18:322457]
env<-ww2[1:17]
mantel = mantel_test(
  spec = otu, env = env,
  spec_select = list(rhh =1:40305,rhd = 40306:80610,roh = 80611:120915,rod=120916:161220,
                     sth =161221:201525,std = 201526:241830,seh = 241831:282135,sed=282136:322440), 
  spec_dist =  dist_func(.FUN = "vegdist", method = "bray"),
  env_dist = dist_func(.FUN = "vegdist", method = "euclidean"),
  mantel_fun = 'mantel', 
  na_omit=TRUE,
)


mantel$P_adj_BH <- p.adjust(mantel$p, method = 'BH')
write.csv(mantel,"mantel_env.csv")
#################################
#############################
mantel<-read.csv("mantel_env.csv",row.names = 1)
mantel01<-mantel

mantel01 <- mantel %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(P_adj_BH, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

p<-qcorrplot(correlate(env),
             type = "lower", 
             diag = FALSE) + 
  geom_square() +
  geom_couple(data =mantel01, 
              aes(colour = pd, 
                  size = rd),
              curvature = nice_curvature()) + 
  scale_fill_gradientn(
    colours = colorRampPalette(colors =c("#194a55", "white","#f26115"),space="Lab")(10),
    limits = c(-1, 1),
    breaks = seq(-1,1,0.5))+ 
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values =c('#7AA15E','#B7B663', "#A2A2A288")) + 
  guides(size = guide_legend(title = "Mantel's r", 
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))
p
ggsave("mantel_env_2.pdf",p,width = 12,height =10)



################ Figure2
#################Richness SD
da<-da%>% 
  select(Compartment,State,group,value) %>%
  group_by(Compartment,State,group) %>% 
  summarise_all(sd)
###################################Beta Dispersion
for (file_name in file_names) {
  data <- read.csv(file_name, row.names = 1)
  data1<-data
  group<-data
  dist_mat <- vegdist(data1,method = 'bray')
  mod <- betadisper(d = dist_mat, group =group$State,type = 'centroid')
  cat(paste("File name: ", file_name, "\n"))
  print(mod)
  data.frame()
  write.csv(data.frame(mod$distances) ,paste0(file_name, ".", file_name, "_Dispersion.csv"))
}

#################AVD
otu <- read.csv('w.csv', row.names = 1)
ai <- abs(otu-apply(otu, 1, mean))/apply(otu, 1, sd)
avd <- colSums(ai)/(1*nrow(otu))
av<-data.frame(avd)
write.csv(av,"AVD.csv")

#################MST
library(NST)
set.seed(123)
tnst <- tNST(comm = comm, group = group, dist.method = "bray", null.model = 'PF', 
             rand = 500, nworker = 6)

#######################Calculate AKP
sd<-read.csv("richnesssd.csv") 
sd<-sd[-1:-2]
dis<-read.csv("meanDispersion.csv",row.names = 1) 
sddis<-merge(dis,sd,bu="group")

mst<-read.csv("meanMST.csv",row.names = 1) 
mst<-mst[-1:-3]

aa<-merge(sddis,mst,by="group")
avd<-read.csv("meanAVD.csv",row.names = 1)
avd<-avd[-1:-3]
wh<-merge(aa,avd,by="group")
write.csv(wh,"WholeD.csv")
 
rownames(wh)<-wh$group
subset_data <- wh[, c("sd", "Dis", "MST", "avd")]
 
normalized_data <- apply(subset_data, 2, function(x) {
  (x - min(x)) / (max(x) - min(x))
})

wh1<-as.data.frame(normalized_data)
wh1$K<-(wh1$sd + wh1$Dis + wh1$MST + wh1$avd)/4
AKP<-wh1
################ Figure3
###########################Calculate 16s rrn copy number 
asv <- read.csv('w1.csv',row.names=1)

classifer <- read.csv('tax.csv',row.names=1)

rrnDB = read.delim("rrnDB-5.8_pantaxa_stats_RDP.tsv")


classifer$genus_rrn <- ifelse(!is.na(match(classifer$Genus, rrnDB$name)),
                              rrnDB$mean[match(classifer$Genus, rrnDB$name)], -1)

classifer$family_rrn <- ifelse(!is.na(match(classifer$Family, rrnDB$name)),
                               rrnDB$mean[match(classifer$Family, rrnDB$name)], -1)

classifer$order_rrn  <- ifelse(!is.na(match(classifer$Order, rrnDB$name)),
                               rrnDB$mean[match(classifer$Order, rrnDB$name)], -1)

classifer$class_rrn  <- ifelse(!is.na(match(classifer$Class, rrnDB$name)),
                               rrnDB$mean[match(classifer$Class, rrnDB$name)], -1)

classifer$phylum_rrn  <- ifelse(!is.na(match(classifer$Phylum, rrnDB$name)),
                                rrnDB$mean[match(classifer$Phylum, rrnDB$name)], -1)

classifer$domain_rrn  <- ifelse(!is.na(match(classifer$Doman, rrnDB$name)),
                                rrnDB$mean[match(classifer$Doman, rrnDB$name)], -1)

classifer$rrn_copy <- NA
for (i in 1:nrow(classifer[,8:14])) {
  index <- which(classifer[,8:14][i, ] != -1)[1]

  if (!is.na(index)) {
    classifer[,8:14]$rrn_copy[i] <- classifer[,8:14][i, index]
  } else {
    classifer[,8:14]$rrn_copy[i] <- classifer[,8:14][i, ncol(classifer[,8:14])]
  }
}
write.csv(classifer,"rrn.csv")



#################Figure3A
aa<-read.csv("rrn.csv")
#aa$CopyNumber
set.seed(123) 
sample_size <- 4999 
sample_data <- sample(aa$CopyNumber, sample_size, replace = FALSE)  
sample_data <- as.data.frame(sample_data)
aa <- aa[order(aa$CopyNumber), ]  
total_rows <- nrow(aa)  
top_40 <- aa[1:(total_rows * 0.4), ]  
top_40$CO<-c("Ol")
bottom_40 <- aa[(total_rows * 0.6):total_rows, ]  
bottom_40$CO<-c("Co")
middle_20 <- aa[(total_rows * 0.4 + 1):(total_rows * 0.6), ]  
middle_20$CO<-c("Mid")
result <- rbind(top_40, middle_20, bottom_40)  
write.csv(result,"dis result.csv")
aa<-read.csv("dis result.csv")
cols01<-c("#c62d17","#023f75","#f6c619")
library(ggplot2)
p1<-ggplot(aa, aes(x=CopyNumber,fill=CO)) +  
  geom_histogram(binwidth=0.02) +  
  theme_minimal() + 
  scale_fill_manual(values = cols01)+
  theme_bw(base_size = 16) +
  theme(axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=20,vjust = 1.5),
        axis.title.y=element_text(colour='black', size=20,vjust = 1.5),#face = "bold"),
        axis.text.y=element_text(colour='black',size=18),#face = "bold"),
        axis.text.x=element_text(colour='black',size=18),
        strip.text = element_text(colour = "black",size = 15,face = "bold"),
        legend.position = 'right')+
  labs(x="CopyNumber", y="Frequency", title="Distribution of CopyNumber")
p1
ggsave("Distribution of CopyNumber12.pdf",p1,width = 5,height = 4)



###############Fig3 d-f
wh<-read.csv("mergtable249wh.csv")
head(wh)

cbbPalette<-c("#c74732","#aebea6","#83ba9e","#0f6657")

p1<-ggplot(wh, aes( x = rrn, y = K, color = Compartment)) +
  geom_point( aes( color = Compartment), size=4)+  
  scale_color_manual(values = cbbPalette) +
  geom_smooth(method = 'lm', formula = y ~ x, se = T) + 
  stat_cor(data=wh, method = "spearman",size=6,label.y.npc = "top", label.x.npc = "center")+
  theme_bw()+
  theme(axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=20,vjust = 1.5),
        axis.title.y=element_text(colour='black', size=20,vjust = 1.5),#face = "bold"),
        axis.text.y=element_text(colour='black',size=18),#face = "bold"),
        axis.text.x=element_text(colour = "black",size = 18,#face = "bold",
                                 angle = 0,hjust = 0.5,vjust = 0.5),
        strip.text = element_text(colour = "black",size = 18,face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "top")
p1

########################Fig 4 
#functional network
library(igraph)
library(dplyr)
library(Hmisc)
library(preprocessCore)
x<-as.data.frame(t(read.csv("kow.csv",row.names = 1)))
x$SampleID<-row.names(x)
group<-read.csv("group.csv")
xa<-merge(group,x,by="SampleID")
xa <- xa %>% 
  filter(Compartment == "Rhizosphere")
rownames(xa)<-xa$SampleID
x2<-xa[-1:-5]
x1<-as.data.frame(t(x2))
stw<-x1
stw$ID<-row.names(stw)
x=x1
x1[x1>0] <- 1
otu<- x[which(rowSums(x1) >=23), ]

b<-rcorr(t(otu),type="spearman")
b2<-b
rr<-b$r
pp<-b$P
rr[abs(rr)<0.8]<-0
pp<-p.adjust(pp,method="BH")
pp[pp>=0.001&pp<1]<-0  
pp[pp<0.001]<-1
z<-rr*pp
diag(z)<-0
g<-graph.adjacency(z,weighted=TRUE,mode="undirected")
g <- simplify(g)
g

cols<-c("#f49128","#194a55","#187c65","#c29f62","#83ba9e","#c62d17",
        "#023f75","#ea894e","#266b69","#eb4601","#f6c619","#fa6e01","#2f2f2f",
        "#972b1d","#e6a84b","#4c211b","#ff717f")
col_g <- "#C1C1C1"


pdf(paste0("module1_rh.pdf"),width=12, height=8)
par(mfrow=c(1,1),mar=c(0,0,1,0),font.main=4)

g1 <- g
E(g1)$correlation <- E(g1)$weight
E(g1)$weight1<-E(g1)$weight
E(g1)$weight<-abs(E(g1)$weight)
set.seed(007)
V(g1)$modularity <- membership(cluster_fast_greedy(g1))

V(g1)$label <- V(g1)$name
V(g1)$label <- NA
modu_sort <- V(g1)$modularity %>% table() %>% sort(decreasing = T)

top_num <- 3
modu_name <- names(modu_sort[1:3])
modu_cols <- cols[1:length(modu_name)]
names(modu_cols) <- modu_name
V(g1)$color <- V(g1)$modularity
V(g1)$color[!(V(g1)$color %in% modu_name)] <- col_g
V(g1)$color[(V(g1)$color %in% modu_name)] <- modu_cols[match(V(g1)$color[(V(g1)$color %in% modu_name)],modu_name)]
V(g1)$frame.color <- V(g1)$color

E(g1)$color <- col_g
for ( i in modu_name){
  col_edge <- cols[which(modu_name==i)]
  otu_same_modu <-V(g1)$name[which(V(g1)$modularity==i)]
  E(g1)$color[(data.frame(as_edgelist(g1))$X1 %in% otu_same_modu)&(data.frame(as_edgelist(g1))$X2 %in% otu_same_modu)] <- col_edge
}

sub_net_layout <- layout_with_fr(g1, niter=999,grid = 'nogrid')
plot(g1,layout=sub_net_layout, edge.color = E(g1)$color,vertex.size=2,
    
     mark.col=V(g1)$color)
title(main = paste0('Nodes=',length(V(g1)$name),', ','\nEdges=',nrow(data.frame(as_edgelist(g1)))))
legend("right", legend = modu_name, col = modu_cols, bty = "n", fill = modu_cols, border = modu_cols)  # Update legend parameters

dev.off()


###################module
g1<-g
E(g1)$weight<-abs(E(g1)$weight)
cfg_t<-cluster_fast_greedy(as.undirected(g1))
m1<-data.frame(cfg_t[[1]])
names(m1)[1]<-c("ID")
m1$module<-rep("1")
m2<-data.frame(cfg_t[[2]])
names(m2)[1]<-c("ID")
m2$module<-rep("2")

m3<-data.frame(cfg_t[[3]])
names(m3)[1]<-c("ID")
m3$module<-rep("3")
ra<-stw

m11<-merge(m1,ra,by="ID")
m22<-merge(m2,ra,by="ID")
m33<-merge(m3,ra,by="ID")



microbe<-rbind(m11,m22,m33)
write.csv(microbe,"module_Rh.csv")

############################################
rownames(m11)<-m11$ID
m111<-as.data.frame(t(m11[-1:-2]))
m111$ID<-row.names(m111)
k<-read.csv("ak.csv")
names(k)[1]<-c("ID")
akk<-merge(k,m111,by="ID")
rownames(akk)<-akk$ID
akk<-akk[-1]

library(reshape2)
ak1<-melt(akk,id=1)

ak2<-akk

ak2$mod1 <- rowMeans(ak2[,-1], na.rm = TRUE)  
ak2<-ak2[-2:-1734]
head(ak2)

library(ggplot2)
p<-ggplot() + 
  geom_smooth(data=ak1, aes(x= K, y= log(value+1), group = variable), col= "grey", method = "lm" , size = 0.05,  fill=NA)+
  geom_smooth(data=ak2, aes(x= K, y= log(mod1+1) ), col= "#f49128", method = "lm", size = 5)+
  labs(x = "AK",y = "Log (abundance + 1)")+ylim(-1,10)+
  theme_bw()+
  theme(legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))
p
ggsave("M1_AK_lm_Rh.pdf",p,width =7,height = 6)
library(nlme) 
library(MuMIn)
lme0<-lme(value~K,random=~1|variable,data=ak1, control=lmeControl(opt = "optim"))
summary(lme0)
anova(lme0)
r.squaredGLMM(lme0)

############################################
rownames(m22)<-m22$ID
m222<-as.data.frame(t(m22[-1:-2]))
m222$ID<-row.names(m222)
k<-read.csv("ak.csv")
names(k)[1]<-c("ID")
akk<-merge(k,m222,by="ID")
rownames(akk)<-akk$ID
akk<-akk[-1]

library(reshape2)
ak1<-melt(akk,id=1)

ak2<-akk

ak2$mod1 <- rowMeans(ak2[,-1], na.rm = TRUE)  
ak2<-ak2[-2:-1621]
head(ak2)

library(ggplot2)
p<-ggplot() + 
  geom_smooth(data=ak1, aes(x= K, y= log(value+1), group = variable), col= "grey", method = "lm" , size = 0.05,  fill=NA)+
  geom_smooth(data=ak2, aes(x= K, y= log(mod1+1) ), col= "#194a55", method = "lm", size = 5)+
  labs(x = "AK",y = "Log (abundance + 1)")+ylim(-1,10)+
  theme_bw()+
  theme(legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))
p
ggsave("M2_AK_rh_lm.pdf",p,width =7,height = 6)
library(nlme) 
library(MuMIn)
lme0<-lme(value~K,random=~1|variable,data=ak1, control=lmeControl(opt = "optim"))
summary(lme0)
anova(lme0)
r.squaredGLMM(lme0)

############################################
rownames(m33)<-m33$ID
m333<-as.data.frame(t(m33[-1:-2]))
m333$ID<-row.names(m333)
k<-read.csv("ak.csv")
names(k)[1]<-c("ID")
akk<-merge(k,m333,by="ID")
rownames(akk)<-akk$ID
akk<-akk[-1]

library(reshape2)
ak1<-melt(akk,id=1)

ak2<-akk

ak2$mod1 <- rowMeans(ak2[,-1], na.rm = TRUE)  
ak2<-ak2[-2:-279]
head(ak2)

library(ggplot2)

p<-ggplot() + 
  geom_smooth(data=ak1, aes(x= K, y= log(value+1), group = variable), col= "grey", method = "lm" , size = 0.05,  fill=NA)+
  geom_smooth(data=ak2, aes(x= K, y= log(mod1+1) ), col= "#187c65", method = "lm", size = 5)+
  labs(x = "AK",y = "Log (abundance + 1)")+ylim(-1,10)+
  theme_bw()+
  theme(legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))
p
ggsave("M3_AK_Rhlm.pdf",p,width =7,height = 6)
library(nlme) 
library(MuMIn)
lme0<-lme(value~K,random=~1|variable,data=ak1, control=lmeControl(opt = "optim"))
summary(lme0)
anova(lme0)
r.squaredGLMM(lme0)

################################Fig5

library(plspm)
library(lavaan)
library(haven)
library(dplyr)
library(Hmisc)
library(semPlot)
library(ggplot2)
a<-read.csv("mergtable93wh.csv")
b<-read.csv("richness.csv")
w<-merge(a,b,by="group")
dis<-read.csv("Dis.csv")
dis<-dis[-1:-5]
dis1 <- dis %>% 
  group_by(group) %>%  
  summarise_all(mean, na.rm = TRUE)
w1<-merge(w,dis1,by="group")
head(w1)
dat<-w1
df<-scale(dat[-1:-4])
head(df)
#########################################
library(piecewiseSEM)
df<-as.data.frame(df)
keeley_psem2 <- psem(
  lm(Richness ~ Sta + K + AGS + GC + OlRA, data = df),
  lm(OlRA ~ Sta + K + AGS + GC , data = df),
  lm(GC ~ Sta + K , data = df),
  lm(AGS ~ Sta + K , data = df),
  lm(K ~ Sta, data = df),
  Richness%~~%Sta,
  OlRA%~~%Sta,
  data = df
)
summary(keeley_psem2)
##################effect
e<-read.csv("effect.csv")
w <- e %>% 
  filter(Group == "whole")
dt2<-w
dt2$Tre <-factor(dt2$Tre, levels = c('Diseased', 'AKP', 'AGS','GC','Oligotrophs'))


p<-ggplot(dt2, aes(Tre, Effect,fill = DI)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7)  +
  scale_fill_manual(values = c("Direct effect"="#194a55","Indirect effect"="#f26115"))+
  scale_y_continuous(name = "Standardized effects from SEM",limits = c(0,0.8),
                     breaks = seq(0,0.8,0.2),expand = c(0,0))+
  theme_bw(base_size = 16) +
  theme(axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=20,vjust = 1.5),
        axis.title.y=element_text(colour='black', size=20,vjust = 1.5),
        axis.text.y=element_text(colour='black',size=18),
        axis.text.x=element_text(colour='black',size=18,angle = 60,hjust = 1),
        strip.text = element_text(colour = "black",size = 15,face = "bold"),
        legend.position = 'right')
p
