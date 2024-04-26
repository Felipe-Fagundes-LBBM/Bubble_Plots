install.packages("dplyr")
install.packages("ggplot")
install.packages("ggrepel")

library(dplyr)
library(ggplot2)
library(ggrepel)

setwd("C:/Users/LBBM/Desktop/Bubleplot_protein")

####---MF----atv---------------------------------------------------

data <- read.delim2("MF_ATV_PROTEOMA.txt", sep = "\t", header = T, skip = 1)

data <- select(data, -Genes)

data <-data[order(data$P.value , decreasing = T),]
level_order = data$Term

data$P.value=as.numeric(as.character(data$P.value))

#data$Genes=as.numeric(as.character(data$Genes))

data$Combined.Score=as.numeric(as.character(data$Combined.Score))

str(data)

#pdf("Bubleplot_MFGO_ATV.pdf", width=8.5, height=4)

plot <- data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
         size=-log10(P.value), 
         color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = F)+
  scale_size(range = c(3, 12), name="-log10 (Combined.Score)")+
  scale_colour_gradient(low = "blue2", high = "coral2",name="Genes") +
  scale_fill_manual(values = c("#636363"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 15, colour = "#636363"), 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 18),
        legend.position="bottom",
        legend.text = element_text(size = 20),
        panel.background = element_blank()) +
        scale_x_continuous(limits = c(1, 3), breaks = seq(1, 3, by = 0.5)) +
  ylab("Molecular Function-GO") +
  xlab("-log10 (P-value)")

plot

dev.off()

?geom_label_repel

gene <- facet_wrap(data$Genes, scales = "free", strip.position = "right")

plot + geom_text_repel(aes(label = data$Genes), size = 6, min.segment.length = 0, 
                       seed = 42, box.padding = 0.5, max.overlaps = Inf,
                       arrow = arrow(length = unit(0.010, "npc")),
                       nudge_x = .15, nudge_y = .5, 
                       color = "black")

####-----MF----ros-----------------------------------------------------

data <- read.delim2("MF_ROS_PROTEOMA.txt", sep = "\t", header = T, skip = 1)

data <- select(data, -Genes)

data <-data[order(data$P.value , decreasing = T),]
level_order = data$Term

data$P.value=as.numeric(as.character(data$P.value))

#data$Genes=as.numeric(as.character(data$Genes))

data$Combined.Score=as.numeric(as.character(data$Combined.Score))

str(data)

plot <- data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = F)+
  scale_size(range = c(3, 12), name="-log10 (Combined.Score)")+
  scale_colour_gradient(low = "blue2", high = "coral2",name="Genes") +
  scale_fill_manual(values = c("#636363"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 15, colour = "#636363"), 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 18),
        legend.position="bottom",
        legend.text = element_text(size = 20),
        panel.background = element_blank()) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 0.5))+
  ylab("Molecular Function-GO") +
  xlab("-log10 (P-value)")

plot

####-----MF----ros-----------------------------------------------------

data <- read.delim2("MF_ROS_PROTEOMA.txt", sep = "\t", header = T, skip = 1)
data <-data[order(data$P.value , decreasing = T),]
level_order = data$Term

data$P.value=as.numeric(as.character(data$P.value))

#data$Genes=as.numeric(as.character(data$Genes))

data$Combined.Score=as.numeric(as.character(data$Combined.Score))

str(data)

plot <- data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = F)+
  scale_size(range = c(3, 12), name="-log10 (Combined.Score)")+
  scale_colour_gradient(low = "blue2", high = "coral2",name="Genes") +
  scale_fill_manual(values = c("#636363"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 15, colour = "#636363"), 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 18),
        legend.position="bottom",
        legend.text = element_text(size = 20),
        panel.background = element_blank()) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 0.5))+
  ylab("Molecular Function-GO") +
  xlab("-log10 (P-value)")

plot

####----BP----ros--------------------------------------------------

data <- read.delim2("MF_ATV_PROTEOMA.txt", sep = "\t", header = T, skip = 1)
data <-data[order(data$P.value , decreasing = T),]
level_order = data$Term

data$P.value=as.numeric(as.character(data$P.value))

#data$Genes=as.numeric(as.character(data$Genes))

data$Combined.Score=as.numeric(as.character(data$Combined.Score))

str(data)

plot <- data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = F)+
  scale_size(range = c(3, 12), name="-log10 (Combined.Score)")+
  scale_colour_gradient(low = "blue2", high = "coral2",name="Genes") +
  scale_fill_manual(values = c("#636363"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 15, colour = "#636363"), 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 18),
        legend.position="bottom",
        legend.text = element_text(size = 20),
        panel.background = element_blank()) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 0.5))+
  geom_label_repel(aes(label = data$Genes), 
                   color = "black", size = 6, fontface = "bold" ,
                   box.padding = 1, nudge_x = .15, nudge_y = .5, max.overlaps = Inf) +
  ylab("Molecular Function-GO") +
  xlab("-log10 (P-value)")
