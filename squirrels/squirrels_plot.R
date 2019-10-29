#libraries
library(tidyverse)
library(png)
library(gridGraphics)
library(extrafont)
library(ggchicklet)

#data
nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Creating data frame of summary statistics
# For this, I compute the mean of some of the logical variables,
# to know the proportion of squirrels doing each of the activities
# Then, I convert to long format
squirrels_summary <- nyc_squirrels%>%
  summarize(Running = mean(running, na.rm = TRUE),
            Climbing = mean(climbing, na.rm = TRUE),
            Eating = mean(eating, na.rm = TRUE),
            Foraging = mean(foraging, na.rm = TRUE),
            People = mean(approaches, na.rm = TRUE))%>%
  gather(key = "activity", value = "proportion")


# Importing the squirrel icons and making them 
# readable by ggplot
# source: http://newwaysys.com/2018/6/cartoon-squirrel-funny-forest-wild-animals-running-standing-jumping-vector-clip-art-collection-wildlife-animal-mammal-image_cqrecords.com/

img1 <- readPNG("squirrels/foraging.png")
g1 <- rasterGrob(img1, interpolate=FALSE)

img2 <- readPNG("squirrels/eating.png")
g2 <- rasterGrob(img2, interpolate=FALSE)

img3 <- readPNG("squirrels/running.png")
g3 <- rasterGrob(img3, interpolate=FALSE)

img4 <- readPNG("squirrels/climbing.png")
g4 <- rasterGrob(img4, interpolate=FALSE)

img5 <- readPNG("squirrels/people.png")
g5 <- rasterGrob(img5, interpolate=FALSE)


#plotting!

squirrels_summary%>%
  ggplot(aes(x= reorder(activity, -proportion), y = proportion))+
  #similar to geo_bar but you get rounded output
  # I wanted the plot to look a bit cartoon-like :)
  geom_chicklet(fill = "saddlebrown", width = .8)+
  #text in the bars:
  geom_text(aes(y = (proportion - .02), label = activity),
            fontface = "bold", color = "seashell1", size = 4,
            family = "Chiller")+
  #bringing the  icons in (trial and error for getting the right values)
  annotation_custom(g1, xmin=0.7, xmax=1.4, ymin=0.475, ymax=0.54) +
  annotation_custom(g2, xmin=1.7, xmax=2.3, ymin=0.252, ymax=0.34) +
  annotation_custom(g3, xmin=2.7, xmax=3.3, ymin=0.241, ymax=0.33) +
  annotation_custom(g4, xmin=3.7, xmax=4.3, ymin=0.22, ymax=0.31) +
  annotation_custom(g5, xmin=4.7, xmax=5.3, ymin=0.06, ymax=0.13) +
  labs(title = "New York squirrels like...",
       x = NULL, y = "%",
       caption = "#tidytuesday by @ariamsita, data: NYC squirrel census")+
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                     limits = c(0, .54))+
  theme_minimal()+ 
  theme(axis.line = element_line(color = "saddlebrown"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(hjust = 0.05, size = 7, 
                                    color = "grey40", angle = 0),
        axis.text.y = element_text(size = 6.5, color = "grey40"), 
        plot.caption = element_text(size = 7.5, color = "grey40")
        plot.title = element_text(color = "saddlebrown", 
                                  family = "Ink Free", size = 14, 
                                  face = "bold", hjust = 0.5))+ 
  ggsave("sq.png")
