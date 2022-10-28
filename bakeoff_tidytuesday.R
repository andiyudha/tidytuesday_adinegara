# Tidytuesday
# 2022-10-25: The Great British Bake Off
# use pkg "bakeoff"

# clear workspaces --------------------------------------------------------

# clear workspace variables
rm(list = ls())
# clear window (same as ctrl+L. )
cat("\014")   
# close all plots
graphics.off()


# load library ------------------------------------------------------------

#install.packages("bakeoff")
library(rstudioapi)
library(bakeoff)
library(tidyverse)
library(hrbrthemes)
library(patchwork)
library(lubridate)
library(extrafont)
library(ggtext)

# set & working directory -------------------------------------------------

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# open data ---------------------------------------------------------------

bakeoff::bakers %>% glimpse()
bakeoff::ratings %>% glimpse()

my_font <- "Roboto Condensed"

# Dumbbell Plot

bakeoff::ratings %>% 
  select(series, episode, viewers_7day) %>% 
  group_by(series) %>%
  filter(episode == 1 | episode == max(episode)) %>%
  mutate(episode = recode(episode, `1` = "first", .default = "last")) %>%
  ungroup() %>% 
  ggplot(aes(x = viewers_7day,
                        y = fct_rev(factor(series)),
                        color = episode,
                        group = series)) +
  geom_line(size = .75) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(0,20, by = 2)) +
  scale_color_manual(values = c("first" = "lightskyblue", "last" = "maroon1")) +
  labs(y = "Series", 
       x = "Viewers (millions)", 
       color = "Episode",
       subtitle = "**<span style='color:grey27;font-size:20px;'>Great British Bake Off**</span>
       **<span style='color:maroon1;font-size:20px;'>Finales**</span>
       **<span style='color:grey27;font-size:20px;'>Get More Viewers than**</span>
       **<span style='color:lightskyblue;font-size:20px;'>Premieres**</span>
       ") +
  theme(text = element_text(family = my_font), 
        legend.position = "none",
        plot.subtitle = element_markdown(color = "#292929"),
        plot.background = element_rect("ivory2"),
        panel.background = element_blank(),
        axis.title = element_text(color = "#292929", face = "bold"),
        axis.text = element_text(color = "#292929", face = "bold"),
        panel.grid = element_blank()) -> first_plot


bakeoff::ratings %>% 
  select(series, episode, viewers_7day) %>%
  group_by(series) %>%
  filter(episode == 1 | episode == max(episode)) %>%
  mutate(episode = recode(episode, `1` = "first", .default = "last")) %>%
  spread(episode, viewers_7day) %>%
  mutate(
    finale_bump = last - first
  ) %>%
  ggplot(aes(series, finale_bump, fill = finale_bump)) + 
  geom_bar(stat = "identity") +
  ylim(0, 6) +
  coord_polar() +
  scale_fill_gradient(low = "#ffb950", high = "#a50104") +
  scale_x_continuous(breaks = 1:10) +
  labs(x = NULL, y = NULL, 
       fill = "Finale Bumps", 
       subtitle = "Finale Bumps Great British Bake Off from
       <span style='color:#ffb950;'>low</span> to
       <span style='color:#a50104;'>high</span>.<br>
       Finale 'Bumps' were Smallest for Series 1 and 10.") +
  theme(text = element_text(family = my_font), 
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect("ivory2"),
        panel.background = element_blank(),
        axis.title = element_text(color = "#292929", face = "bold"),
        axis.text = element_text(color = "#292929", face = "bold"),
        plot.subtitle = element_markdown(face = "bold", color = "#292929"),
        strip.text.x = element_text(face = "bold", color = "#292929"),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines")) -> bumps_plot

# combine plot

com_plot <-  bumps_plot + first_plot

com_plot + 
  plot_annotation(title = "Great British Bake Off: An Infographic Using R",
                           subtitle = "As of 2022 there were 10 Series in Great British Bake Off.",
                           caption = "Data: Great British Bakeoff by Alison Hill, Chester Ismay, and Richard Iannone | viz = Muhammad Andi Yudha", 
                           theme = theme(plot.title = element_text(color = "#292929", size = 22, face = "bold", family = my_font),
                                         plot.subtitle = element_text(color = "#292929", size = 14, family = my_font),
                                         plot.caption = element_text(color = "grey50", face = "bold.italic", family = my_font),
                                         plot.background = element_rect("ivory2"),
                                         panel.background = element_rect("ivory2")))

ggsave("British_Bake_Off.png",
       width = 30,
       height = 18,
       units = "cm",
       dpi = 500,
       type = "cairo-png")
