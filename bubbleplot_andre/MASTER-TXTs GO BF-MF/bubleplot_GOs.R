
library(dplyr)
library(ggplot2)

data <- read.delim2("MF_GO_plot.txt", sep = "\t", header = T)
data <-data[order(data$P.value, decreasing = TRUE),]
level_order = data$Term

pdf("Bubleplot_MFGO_ATV.pdf", width=8.5, height=4)
data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
         size=-log10(Adjusted.P-value), 
         color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = F)+
  scale_size(range = c(3, 12), name="-log10 (Adjusted.P-value)")+
  scale_colour_gradient(low = "blue2", high = "coral2",
                        name="Combi. Score") +
  scale_fill_manual(values = c("black"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        legend.position="bottom",
        legend.text = element_text(size = 20),
        panel.background = element_blank()) +
  ylab("M.F. GO") +
  xlab("-log10 (p-value)")
dev.off()

#### BP

data <- read.delim2("BP_GO_plot.txt", sep = "\t", header = T)
data <-data[order(data$P.value, decreasing = TRUE),]
level_order = data$Term

pdf("Bubleplot_BPGO_ATV.pdf")
data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(Adjusted.P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = T)+
  scale_size(range = c(3, 12), name="-log10 (Adjusted p-value)")+
  scale_colour_gradient(low = "blue2", high = "coral2",
                        name="Combi. Score") +
  scale_fill_manual(values = c("black"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        legend.position="bottom",
        legend.text = element_text(size = 12),
        panel.background = element_blank()) +
  ylab("Biologic Process - GO") +
  xlab("-log10 (p-value)")
dev.off()

#### ROS
data <- read.delim2("MF_GO_plot_ros.txt", sep = "\t", header = T)
data <-data[order(data$P.value, decreasing = TRUE),]
level_order = data$Term

pdf("Bubleplot_MFGO_ROS.pdf")
data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(Adjusted.P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = T)+
  scale_size(range = c(3, 12), name="-log10 (Adjusted p-value)")+
  scale_colour_gradient(low = "blue2", high = "coral2",
                        name="Combi. Score") +
  scale_fill_manual(values = c("black"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        legend.position="bottom",
        legend.text = element_text(size = 12),
        panel.background = element_blank()) +
  ylab("Molecular Function - GO") +
  xlab("-log10 (p-value)")
dev.off()



#### BP

data <- read.delim2("BP_GO_plot_ros.txt", sep = "\t", header = T)

data <-data[order(data$P.value, decreasing = TRUE),]

level_order = data$Term

pdf("Bubleplot_BPGO_ROS.pdf")
data %>%
  ggplot(aes(x=-log10(P.value), y=factor(Term, levels=level_order), 
             size=-log10(Adjusted.P.value), 
             color=Combined.Score)) +
  geom_point(alpha=0.95, show.legend = T)+
  scale_size(range = c(3, 12), name="-log10 (Adjusted p-value)")+
  scale_colour_gradient(low = "blue2", high = "coral2",
                        name="Combi. Score") +
  scale_fill_manual(values = c("black"))+
  theme(panel.grid = element_line(colour="gray"),
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        legend.position="bottom",
        legend.text = element_text(size = 12),
        panel.background = element_blank()) +
  ylab("Biologic Process - GO") +
  xlab("-log10 (p-value)")
dev.off()