require(rjson)
require(reshape)
require(ggplot2)
library(scales)
library(nlstools)
library(stringr)
library(gridExtra)
library(XML)
library(plyr)
setwd("~/workspace/viaf-analysis")
filter1 <- fromJSON(file='filter1.json')
filter2 <- fromJSON(file='filter2.json')
filter3 <- fromJSON(file='filter3.json')
alln <- fromJSON(file='alln.json')
langtot <- fromJSON(file='langtotals_1.json')

filter1m <- melt(filter1)
filter2m <- melt(filter2)
filter3m <- melt(filter3)
allnm <- melt(alln)

langtotm <-melt(langtot)
langtotm <- langtotm[order(langtotm$value),]
  

a <- data.frame(matrix(ncol = 0, nrow = length(filter1m$value)))
a["langs"] <- filter1m[c(order(filter1m$L1,filter1m$value)),"L1"]
a["uniqs"] <- filter1m[c(order(filter1m$L1,filter1m$value)),"value"]
a["tots"] <- langtotm[c(order(langtotm$L1,langtotm$value)),"value"]

#linear scale
p1 <- ggplot(a, aes(x=tots, y=uniqs), geom="line")+ geom_point(stat="identity") + 
  geom_text(data=a, aes(x=tots,y=uniqs,label=langs,colour=factor(langs),alpha=.9), 
            position=position_dodge(width=0.8,height=0.8),vjust=0, angle = 0, size = 8, guide = FALSE) +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  geom_smooth(method="lm", se=FALSE) +
  labs(x="Total VIAF IDs", y="Unique VIAF IDs", title="Comparison of Contributors in VIAF by total and unique IDs \n Linear Scale") +
  theme(legend.position="none")
p1

#log-log plot
p2 <- ggplot(a, aes(x=tots, y=uniqs), geom="line")+ geom_point(stat="identity") + 
  geom_text(data=a, aes(x=tots,y=uniqs,label=langs,colour=factor(langs),alpha=.9), position=position_dodge(width=0.8,height=0.8),vjust=0, angle = 0, size = 5, guide = FALSE) +
  scale_x_log10(labels=comma) + 
  scale_y_log10(labels=comma) +
  labs(x="Total VIAF IDs", y="Unique VIAF IDs", title="Comparison of Contributors in VIAF by total and unique IDs \n Log-Log Scale") +
  theme(legend.position="none")
p2


#lets try this with a insstead of filter1m
q <- data.frame(matrix(ncol = 0, nrow = length(a$tots)))
q["langs"] <- a[c(order(a$tots,a$langs)),"langs"]
q["uniqs"] <- a[c(order(a$tots,a$langs)),"uniqs"]
q["tots"] <- a[c(order(a$tots,a$langs)),"tots"]
#q["resid"] <- a[c(order(a$uniqs,a$langs)),"resid"]
q["rat"] <- q[,"uniqs"] / q[,"tots"]
 
                
totordplot1 <- ggplot(q, aes(x=langs, y=rat, fill=tots/max(tots)*100))+ 
  geom_bar(stat="identity", width = .9) + 
  #geom_bar(stat="identity", width = .5, alpha = .5, colour='red', aes(x=langs, y=resid), data=q)+
  scale_x_discrete(limits=q$langs) +
  scale_y_continuous(labels=percent)+
  scale_fill_gradient(low=muted("darkgreen"), high=muted("purple"), 
                      limits=c(0,100), "Total Records \n as % of DNB")+
  geom_text(aes(x=langs,y=0,label=paste(round(100*rat,2), '%', sep='')),hjust=0, angle = 90, colour='grey', data=q) +
  labs(title="Uniquness of Authority Files Ordered by Size")+
  theme_bw()
totordplot1
                

#on to the pairs
#i'd like to make an upper triangular heatmap
filter2m.1k <- subset(filter2m, value > 20000)
pairs <- data.frame(matrix(ncol = 0, nrow = length(filter2m.1k$value)))
pairs["langs"] <- filter2m.1k[order(-filter2m.1k$value),"L1"]
pairs["tots"] <- filter2m.1k[order(-filter2m.1k$value),"value"]
pairs["isP"] <- rep(TRUE, nrow(pairs))
pairs["hasDNB"] <- grepl('DNB', pairs[,"langs"])
#triples

filter3m.1k <- subset(filter3m, value > 8000)
trips <- data.frame(matrix(ncol = 0, nrow = length(filter3m.1k$value)))
trips["langs"] <- filter3m.1k[order(-filter3m.1k$value),"L1"]
                 trips["tots"] <- filter3m.1k[order(-filter3m.1k$value),"value"]
                 trips["hasDNB"] <- grepl('DNB', trips[,"langs"])
                 trips["isP"] <- rep(FALSE, nrow(trips))
                 
pairsplot <- ggplot(pairs[1:15,], aes(x=langs, y=tots, fill=factor( ! hasDNB)))+
                   geom_bar(stat="identity", width=.75, size=3, alpha=1)+
                   scale_x_discrete(limits=pairs[1:15,]$langs,labels=rep('',15))+
                   scale_y_continuous(label=comma)+
                   scale_fill_brewer("DNB in cluster?", labels=c('Yes', 'No'), palette="Set2")+
                   geom_text(aes(x=langs,y=0,
                                 label=paste(langs, ' ', prettyNum(tots, big.mark=","))),
                             hjust=0, angle = 90, colour=muted('pink'))+
                   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                   labs(x="Pair",y='')
pairsplot


                 
tripsplot <- ggplot(trips[1:15,], aes(x=langs, y=tots, fill=factor(! hasDNB)))+
                   geom_bar(stat="identity", width=.75, size=3, alpha=1)+
                   scale_x_discrete(limits=trips[1:15,]$langs,labels=rep('',15))+
                   scale_y_continuous(label=comma)+
                   scale_fill_brewer("DNB in cluster?", labels=c('Yes', 'No'), palette="Set2")+
                   geom_text(aes(x=langs,y=0,
                                 label=paste(langs, ' ', prettyNum(tots, big.mark=","))),
                             hjust=0, angle = 90, colour=muted('pink'))+
                   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                   labs(x="Triple",y='')
tripsplot
                 
                 
                 
tmp <- ggplot_gtable(ggplot_build(tripsplot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]
                 
grid.arrange(arrangeGrob(pairsplot + theme(legend.position="none"),
                                          tripsplot + theme(legend.position="none"),
                                          main ="Comparison of Highest Co-occurring Authority File Clusters \n Pairs and Triples",
                                          left ="Number of Authority Files in Cluster"), legend, 
                              widths=unit.c(unit(.85, "npc"), legend$width), nrow=1)


#alln



allnm["univgroup"] <- rep(1,length(allnm[,0]))
allnm["ninecol"] <- as.numeric(allnm[,"L1"]) %% 6
allnm["rat"] <- allnm[,"value"] / sum(allnm[,"value"])  
allnm["sumsupto"] <- cumsum(allnm$rat)



allnplot1 <- ggplot(allnm, aes(x=factor(univgroup), y=rat, fill=factor(ninecol))) +
  scale_y_continuous(breaks=allnm$sumsupto, 
                     labels=paste(round(100*allnm$sumsupto,2), '%', sep=''))+
  scale_x_discrete(breaks="none",label="none")+
  geom_bar(stat ="identity" ,position = "stack") + 
  geom_text(aes(y=sumsupto-(rat/2),
                label=allnm$L1),
            size=(1/as.numeric(allnm$L1)) *15 ) +
  scale_fill_brewer(type = "seq", palette = 3,guide='none')+
  coord_cartesian(ylim=c(0, 1))+
  theme(title=element_blank())

allnplot1

allnplot2 <- ggplot(allnm, aes(x=factor(univgroup), y=rat, fill=factor(ninecol))) +
  scale_y_continuous(breaks=subset(allnm$sumsupto, allnm$sumsupto <0.9995), 
                     labels=paste(round(100*subset(allnm$sumsupto, allnm$sumsupto <0.9995),2), '%', sep=''))+
  scale_x_discrete(breaks="none",label="none")+
  geom_bar(stat ="identity" ,position = "stack") + 
  geom_text(aes(y=sumsupto-(rat/2),
                label=allnm$L1),
            size=(1/as.numeric(allnm$L1)) *60 ) +
  scale_fill_brewer(type = "seq", palette = 3,guide='none')+
  coord_cartesian(ylim=c(0.9798524, 1))+
  theme(title=element_blank())

allnplot2

allnplot3 <- ggplot(allnm, aes(x=factor(univgroup), y=rat, fill=factor(ninecol))) +
  scale_y_continuous(breaks=subset(allnm$sumsupto, allnm$sumsupto <0.999999), 
                     labels=paste(round(100*subset(allnm$sumsupto, allnm$sumsupto <0.999999),4), '%', sep=''))+
  scale_x_discrete(breaks="none",label="none")+
  geom_bar(stat ="identity" ,position = "stack") + 
  geom_text(aes(y=sumsupto-(rat/2),
                label=allnm$L1),
            size=(1/as.numeric(allnm$L1)) *120 ) +
  scale_fill_brewer(type = "seq", palette = 3,guide='none')+
  coord_cartesian(ylim=c(0.9999168, 1))+
  theme(title=element_blank())

allnplot3

grid.arrange(allnplot1, allnplot2, allnplot3, ncol=3, 
             left="Composition of VIAF Clusters", sub="Number of Authority Files in Cluster", main="Composition of VIAF Clusters by number of Authority Files")