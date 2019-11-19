# data
cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

#libraries
library(tidyverse)
library(ariamsita)  #available at devtools::install_github("aaumaitre/ariamsita")
library(wesanderson)

#tidying up the data
clean_cran <- cran_code %>%
  separate(version, into = c("vers", NA), sep = 2) %>%
  mutate(vers = as.numeric(str_remove(vers, "[.-]"))) %>%
  filter(vers <= 10) %>%
  select(vers, blank, comment, code) %>%
  gather(-vers, key = "line_of", value = "n_lines") %>%
  group_by(vers, line_of) %>%
  summarize(avg_code = mean(n_lines, na.rm = TRUE))

clean_cran$line_of <- factor(clean$line_of, levels = rev(c("code", "comment", "blank")))


#plot
clean_cran %>%
  ggplot(aes(x = factor(vers), y = avg_code, color = line_of, fill = line_of)) +
  geom_point(position = position_dodge(0.5)) +
  geom_bar(width = 0.1, position = position_dodge(0.5), stat = "identity") +
  annotate("text", y = 1000, x = 12, label = "Average number of", color = "white") +
  annotate("text", y = 1850, x = 12, label = "code, ", color = "#46ACC8", fontface = "bold") +
  annotate("text", y = 2400, x = 12, label = "comment ", color = "#E2D200", fontface = "bold") +
  annotate("text", y = 2900, x = 12, label = "and ", color = "white") +
  annotate("text", y = 3250, x = 12, label = "blank ", color = "#DD8D29", fontface = "bold") +
  annotate("text", y = 4250, x = 12, label = "lines by package version", color = "white") +
  guides(color = FALSE, fill = FALSE) +
  scale_color_manual(
    values = wes_palette("FantasticFox1"),
    breaks = c("code", "comment", "blank")
  ) +
  scale_fill_manual(
    values = wes_palette("FantasticFox1"),
    breaks = c("code", "comment", "blank")
  ) +
  labs(
    y = "Average lines",
    x = "Package\nversion",
    title = "Do lines of code increase with package updates?",
    caption = "#tidytuesday by @ariamsita, data: CRAN"
  ) +
  coord_flip(clip = "off", ylim = c(0, 5500)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.grid = element_line(color = "gray20"),
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold", vjust = 3),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.title.y = element_text(angle = 0, size = 9),
    legend.position = c(0.8, 0.2), legend.text = element_text(color = "white"),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    plot.caption = element_text(color = "white")
  ) +
  ggsave("cran.png", width = 8, height = 6)
