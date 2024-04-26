install.packages("dplyr")
install.packages("ggplot")

library(dplyr)
library(ggplot2)

setwd("C:\\Users\\User\\Desktop\\TERMs André")

####---MF----atv---------------------------------------------------

data <- read.delim2("Term_MF_atv.txt", sep = "\t", header = T)
data <-data[order(data$P.value , decreasing = TRUE),]
level_order = data$Term

data$P.value=as.numeric(as.character(data$P.value))

data$Adjusted.P.value=as.numeric(as.character(data$Adjusted.P.value))

data$Combined.Score=as.numeric(as.character(data$Combined.Score))

str(data)

pdf("Bubleplot_MFGO_ATV.pdf", width=8.5, height=4)

data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
         size=-log10(Adjusted.P.value), 
         color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = F)+
  scale_size(range = c(3, 12), name="-log10 (Adjusted.P.value)")+
  scale_colour_gradient(low = "blue2", high = "coral2",name="Combi. Score") +
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
dev.off()


####-----BP----atv-----------------------------------------------------

data <- read.delim2("Term_BP_atv.txt", sep = "\t", header = T)
data <-data[order(data$P.value, decreasing = TRUE),]
level_order = data$Term

data$P.value=as.numeric(as.character(data$P.value))

data$Adjusted.P.value=as.numeric(as.character(data$Adjusted.P.value))

data$Combined.Score=as.numeric(as.character(data$Combined.Score))


pdf("Bubleplot_BPGO_ATV.pdf",width=9.5, height=6)

data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(Adjusted.P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = T)+
  scale_size(range = c(3, 12), name="-log10 (Adjusted.P.value)")+
  scale_colour_gradient(low = "blue2", high = "coral2",
                        name="Combin. Score") +
  scale_fill_manual(values = c("#636363"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 15, colour = "#636363"), 
        axis.text.y = element_text(size = 22),
        axis.text.x = element_text(size = 20),
        legend.position="bottom",
        legend.text = element_text(size = 20),
        panel.background = element_blank()) +
        scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 0.5))+
  ylab("Biologic Process-GO") +
  xlab("-log10 P-value")
dev.off()

####-----MF----ros-----------------------------------------------------

data <- read.delim2("Term_MF_ros.txt", sep = "\t", header = T)
data <-data[order(data$P.value, decreasing = TRUE),]
level_order = data$Term



data$P.value=as.numeric(as.character(data$P.value))

data$Adjusted.P.value=as.numeric(as.character(data$Adjusted.P.value))

data$Combined.Score=as.numeric(as.character(data$Combined.Score))

pdf("Bubleplot_MFGO_ROS.pdf", width=10, height=6)

data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(Adjusted.P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = T)+
  scale_size(range = c(3, 12), name="-log10 (Adjusted p-value)")+
  scale_colour_gradient(low = "blue2", high = "coral2",
                        name="Combi. Score") +
  scale_fill_manual(values = c("#636363"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 14, colour = "#636363"), 
        axis.text.y = element_text(size = 22),
        axis.text.x = element_text(size = 24),
        legend.position="bottom",
        legend.text = element_text(size = 28),
        panel.background = element_blank()) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 0.5))+
  ylab("Molecular Function-GO") +
  xlab("-log10 P-value")
dev.off()



####----BP----ros--------------------------------------------------

data <- read.delim2("Term_BP_ros.txt", sep = "\t", header = T)
data <-data[order(data$P.value, decreasing = TRUE),]
level_order = data$Term

data$P.value=as.numeric(as.character(data$P.value))

data$Adjusted.P.value=as.numeric(as.character(data$Adjusted.P.value))

data$Combined.Score=as.numeric(as.character(data$Combined.Score))

pdf("Bubleplot_BPGO_ROS.pdf", width=7.5, height=5)
data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(Adjusted.P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = T)+
  scale_size(range = c(3, 12), name="-log10 (Adjusted p-value)")+
  scale_colour_gradient(low = "blue2", high = "coral2",
                        name="Combi. Score") +
  scale_fill_manual(values = c("#636363"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 14, colour = "#636363"), 
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        legend.position="bottom",
        legend.text = element_text(size = 12),
        panel.background = element_blank()) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 0.5))+
  ylab("Biologic Process - GO") +
  xlab("-log10 P-value")
dev.off()