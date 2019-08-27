# Tidy Tuesday plots

## Week 35/2019: Simpsons Guest Stars

Data and dataset information can be found [here](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-27).

**Plot:**

![](https://i.ibb.co/JkBqMyf/image.png) 

**Code:**
_(script available for downlowad)_
```` r
#Libraries
library(tidyverse)
library(extrafont)
library(ggpubr)

#Loading data
simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|")

#Tidying up data to make it easier to work with
simpsons_tidy <- simpsons%>%
  filter(season != "Movie")%>% #Decided not to include the movie
  mutate(role = as.factor(role),
         guest_star = as.factor(guest_star),
         role_simple = ifelse(role %in% c("Himself", "Herself"), "Himself or Herself", 
                              "Character in the show"), #reducing roles to two categories
         season_group = case_when( season %in% 1:10 ~ "Seasons 1-10",
                                   season %in% 11:20 ~ "Seasons 11-20",
                                   season %in% 21:30 ~ "Seasons 21-30"), #for later faceting
         season = as.numeric(season))


#First plot: faceted bars

p1 <- simpsons_tidy%>%
  group_by(season_group, season, role_simple)%>%
  count()%>%
  ggplot(aes(x=season, y=n, fill = role_simple))+
  geom_bar(stat = "identity", width = 0.6)+
  facet_wrap(~season_group, ncol = 1, scales = "free_x")+
  labs(title = "Number of guest stars in The Simpsons by season",
       x = NULL,
       y = NULL,
       fill = "Appearing  as")+
  scale_fill_manual(values = c("#FFD90F", "#4F76DF"))+ #Simpsons colors
  scale_x_continuous(breaks = seq(1,30, by = 1))+ #Show all years
  theme_minimal()+
  theme(plot.background = element_rect(fill = "black"),
        panel.grid = element_line(color = "grey23"),
        plot.margin = unit(c(1.2,0.5,0.5,0.5), "cm"), #adds some space around
        text = element_text(color = "white", family = "Berlin Sans FB Demi"),
        legend.title = element_text(size = 9),
        axis.text = element_text(color = "white"),
        strip.text =  element_text(color = "#FFD90F", face = "bold",size = 10),
        plot.title = element_text(face = "bold", 
                                  family = "Berlin Sans FB Demi", 
                                  hjust = 0.5,
                                  color = "#FFD90F",
                                  size = 16, vjust = 4), #vjust to move it towards margins
        legend.position = "top")


#Plot 2: Most frequent guests

p2 <- simpsons_tidy%>%
  group_by(guest_star)%>%
  count()%>%
  arrange(desc(n))%>%
  filter(n >= 9)%>% #keeping only top ones
  ggplot(aes(x = 1.15, y = reorder(guest_star, n)))+ #x value chosen at random then adjusted
  geom_text(aes(label = reorder(guest_star, n), x = 1.2), 
            hjust = 0, color = "white",
            family = "Berlin Sans FB Demi",
            size = 4)+
  geom_point(aes(size = n), color = "#FFD90F")+
  geom_text(aes(label = ifelse(n>150, paste0(n, "\ntimes"), n),  #word "times" only for first 
                x = 1.15), 
            color = "black", family = "Berlin Sans FB Demi", size = 4,
            lineheight = 0.6)+ #reducing gap between lines
  scale_size(range = c(6, 16), guide = FALSE)+  #size for points
  xlim(c(1,1.5))+ #no real x but this controls the appearance
  labs(title = "Most frequent guest stars",
       caption = "@ariamsita  ")+
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1.2,0.2,0,0), "cm"),
        text = element_text(color = "white", 
                            family = "Berlin Sans FB Demi"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", 
                                  family = "Berlin Sans FB Demi", 
                                  hjust = 0.5,
                                  color = "#FFD90F",
                                  size = 16,
                                  vjust = 4),
        plot.caption = element_text(color = "white"))

#Creating a plot list  to fix some issues with ggarrange (lines in the middle)
plot.list <- lapply(list(p1, p2), 
                    function(p) p + 
                      theme(plot.background = element_rect(color = "black")))

#Final plot: voilà!
ggarrange(plotlist = plot.list, ncol = 2, widths = c(2.8, 1))+
  ggsave("simpsons.png", height = 6, width = 12)
````
