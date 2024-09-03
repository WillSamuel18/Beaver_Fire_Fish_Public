rm(list=ls(all=TRUE))
setwd("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2")
data2 <- read.csv("grayling_pres_new.csv",header=T)


#hist(data2$MEANANNCMS_Brabets, breaks = 20)
#hist(data2$GRADIENT_MEAN, breaks = 20)
#hist(data2$WIDTH_M_MEAN, breaks = 20)
#hist(data2$DEPTH_M_MEAN, breaks = 20)

quantile(data2$MEANANNCMS_Brabets, probs = c(0.10,0.95))
quantile(data2$GRADIENT_MEAN, probs = c(0.55,0.95))
#quantile(data2$WIDTH_M_MEAN, probs = c(0.05,0.95))
#quantile(data2$DEPTH_M_MEAN, probs = c(0.05,0.95))

median(data2$GRADIENT_MEAN)

cor(data2$MEANANNCMS_MEAN,data2$WIDTH_M_MEAN)

library(ggplot2)

ggplot(data2, aes(x=MEANANNCMS_MEAN)) + 
  geom_histogram(binwidth = 50,color="black", fill="cornflowerblue")+
  ylab("Frequency")+
  xlab("Mean annual discharge (m/s^2)")+
  labs(title = "A", face = "bold")+
  theme_classic()+
  theme(text = element_text(size = 15),
               axis.text=element_text(color="black"))


p1 <- ggplot(data2, aes(x = MEANANNCMS_MEAN)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "cornflowerblue") +
  ylab("Frequency") +
  xlab("Mean Annual Discharge (m/s^2)") +
  labs(title = "A") +
  theme_classic() +
  theme(
    text = element_text(size = 15),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold")  # Set the title to bold
  )
p1

ggsave(plot= p1,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/Appendix A figA1.jpeg",
       dpi = 500, 
       height = 4,
       width = 6,
       units = "in")



ggplot(data2, aes(x=GRADIENT_MEAN)) + 
  geom_histogram(binwidth = 0.01,color="black", fill="cornflowerblue")+
  ylab("Frequency")+
  xlab("Gradient (%)")+
  theme_classic()+
  theme(text = element_text(size = 15),
        axis.text=element_text(color="black"))



p2 <- ggplot(data2, aes(x = GRADIENT_MEAN)) + 
  geom_histogram(binwidth = 0.01,color="black", fill="cornflowerblue")+
  ylab("Frequency") +
  xlab("Gradient (%)") +
  labs(title = "B") +
  theme_classic() +
  theme(
    text = element_text(size = 15),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold")  # Set the title to bold
  )
p2

ggsave(plot= p2,
       filename = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Thesis/Data and Analysis/Beaver Project/Beaver_Project_2/Figures/Appendix A figB1.jpeg",
       dpi = 500, 
       height = 4,
       width = 6,
       units = "in")







#Thresholds
# flow > 0.06 cms
# gradient < 0.04
# width > 2.6
# depth > 0.14
