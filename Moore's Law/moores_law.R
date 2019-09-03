#libraries
library(tidyverse)
library(scales)

#Getting and cleaning up the data
#I will only be using year + number of transistors
cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")
cpu2 <- cpu%>%select(transistor_count, date_of_introduction)

gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")
gpu2 <- gpu%>%select(transistor_count, date_of_introduction)

ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")
ram2 <- ram%>%select(transistor_count, date_of_introduction)


#Bringing it all together
all <- rbind(cpu2%>%mutate(what = "Microprocessors (CPU)"),
             gpu2%>%mutate(what = "Graphics Processing Unit (GPU)"),
             ram2%>%mutate(what = "Random-access memory (RAM)"))



#Before plotting, I need an extra df that I will use to create annotations
#As I will be using facet_wrap later I need this to be able to have a single
#annotation that I will use as my y axis title
#If you want to grasp the logic:
# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2
 

ann_text <- data.frame(transistor_count=rep(max(all$transistor_count*18, na.rm = TRUE),3),
                       date_of_introduction=rep(1933, 3),
                       what = c("Graphics Processing Unit (GPU)",
                                "Microprocessors (CPU)",
                                "Random-access memory (RAM)"),
                       label=c("Number of transistors",
                               "", ""))


#Plot plot plotting

all%>%
  ggplot(aes(x=date_of_introduction, y=transistor_count, fill = what))+
  geom_point(alpha = 0.5, shape = 21)+
  geom_smooth(method = "lm", se = FALSE, aes(color = what))+
  facet_wrap(~what)+
  #this is actually setting the y title:
  geom_text(data = ann_text,
            aes(label= label, x= date_of_introduction, y=transistor_count),
            hjust = 0, color = "white",
            size = 3.2, color = "lightblue2")+
  scale_y_log10(labels = comma)+
  #This allows to write outside of plots:
  coord_cartesian(xlim = c(1963, 2019), clip = "off")+
  scale_x_continuous(breaks = seq(1965, 2015, by = 10))+
  scale_fill_manual(values = c("lightskyblue", "deepskyblue", "dodgerblue"))+
  scale_color_manual(values = c("lightskyblue", "deepskyblue", "dodgerblue"))+
  guides(color = FALSE, fill = FALSE)+
  labs(x = NULL,
       y = NULL,
       title = "Exponential Growth of Number of Transistors, 1963-2019",
       caption = "#tidytuesday by @ariamsita, Data:Wikipedia")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "black"),
        panel.grid = element_line(color = "grey23"),
        plot.margin = unit(c(1.2,0.5,0.5,0.5), "cm"), #adds some space around
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(angle = 45),
        strip.text =  element_text(color = "lightblue2", 
                                   face = "bold",size = 10),
        plot.caption = element_text(color = "lightblue2"),
        plot.title = element_text(face = "bold", hjust = 0.5,color = "cadetblue1",
                                  size = 15, vjust = 4), #vjust to move it towards margins
        panel.spacing = unit(2, "lines"))+
  ggsave("computers.png", width = 10, height = 5)
