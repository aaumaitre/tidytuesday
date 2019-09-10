#libraries
library(tidyverse)
library(png)
library(gridGraphics)
library(extrafont)

#data
tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")

#Cleaning the age column, the one I'll be using
injuries <- tx_injuries%>%
  mutate(age = as.numeric(age))%>%
  filter(age != "NA")
  

#creating dataframe with density as I want to plot with geom_line
inj_dens <- density(injuries$age)
df <- data.frame(x=inj_dens$x, y=inj_dens$y)%>%filter(x>= 0)


#Second data frame that will be used to create "fake gridlines"
#I'm basically taking the density df and selecting every 20th row
#this will create the "structure" of the rollercoaster
df2 <- df[seq(1, nrow(df), 20), ]


#images that will be used in the plot (the wagons)
img1 <- readPNG("roller.png")
g1 <- rasterGrob(img1, interpolate=FALSE)

img2 <- readPNG("roller2.png")
g2 <- rasterGrob(img2, interpolate=FALSE)

img3 <- readPNG("roller3.png")
g3 <- rasterGrob(img3, interpolate=FALSE)


#plotting!
df%>%
  ggplot(aes(x,y))+ #x is age, y is density
  geom_linerange(data = df2, aes(x =x, ymin = 0, ymax = y), 
                 color = 'grey40', alpha = 0.6) + #the gridlines
  geom_line(color = "#e44fb7", size = 1.5)+
  #Now adding the wagons:
  annotation_custom(g1, xmin=5.5, xmax=13, ymin=0.025, ymax=0.030) +
  annotation_custom(g2, xmin=32, xmax=38, ymin=0.013, ymax=0.019) +
  annotation_custom(g3, xmin=75, xmax=83, ymin=-0.0015, ymax=0.004) +
  #The two annotations: one curve and one text for each
  geom_curve(x = 13, y = 0.028, xend = 18, yend = 0.029, color = "#ec99d3",
             curvature = -0.2,  arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("text", x= 23.5, y = 0.029, 
           label= "There is a peak in \ninjuries among \nchildren aged around 10",
           color = "white", size = 2.7)+
  geom_curve(x = 37, y = 0.017, xend = 43, yend = 0.019, color = "#ec99d3",
             curvature = -0.2,  arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("text", x= 51, y = 0.019, 
           label= "From age 35 onwards, \ninjuries sharply decrease \n(probably attendance to \namusement parks too!)",
           color = "white", size = 2.7)+
  scale_x_continuous(breaks = seq(0, 70, by = 10))+
  scale_y_continuous(limits = c(0, 0.03))+
  theme_minimal()+
  labs(title = "Age distribution of Amusement Park injuries in Texas",
       y = "Density",
       x = "Age of injured person",
       caption = "#tidytuesday by @ariamsita, data: data.world")+
  theme(plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1.2,0.5,0.5,0.5), "cm"), #adds some space around
        text = element_text(color = "white", size = 10,
                            family = "Eras Light ITC", face = "bold"),
        axis.text = element_text(color = "white"),
        axis.title.y = element_text(angle = 0),
        plot.caption = element_text(color = "#ec99d3", size = 9,
                                    family = "Arial"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5,color = "white",
                                  size = 16, vjust = 6,
                                  family = "Eras Demi ITC"), #vjust to move it towards margins
        panel.spacing = unit(2, "lines"))+
  ggsave("rollercoaster.png", height = 6, width = 9)
