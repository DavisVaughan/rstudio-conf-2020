library(slide)
library(ggplot2)
library(gganimate)
library(tibble)
library(vctrs)
library(scales)
library(magrittr)

roll <- tibble(
  xmin = c(rep(1L, 3), 2:8),
  xmax = 1:10,
  ymin = 1L,
  ymax = 3L,
  group = seq_along(xmin)
)

expand <- tibble(
  xmin = 1L,
  xmax = 1:10,
  ymin = 5L,
  ymax = 7L,
  group = seq_along(xmin)
)

df <- vec_rbind(roll = roll, expand = expand, .names_to = "type")

plot <- df %>%
  ggplot() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#DCDCDC", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(family = "Inconsolata", size = 14, colour = "black"),
    axis.text.y = element_text(family = "Inconsolata", size = 14, colour = "black")
  ) +
  geom_rect(
    mapping = aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      group = group
    ),
    fill = "#353535"
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = c(2, 6), labels = c("Rolling", "Expanding")) +
  transition_manual(group)

# - type = "cairo" because the default of "quartz" doesn't seem to match the Keynote color profile
#   meaning that the plot background looks noticeably different
# - width/height should be 488x100, but to make the text clearer we 4x the resolution so we also
#   4x the width/height. Then in Keynote we resize it back to 488x100
# - fps = 5 to be slow enough to explain
anim <- animate(plot, width = 488*2, height = 100*2, res = 72*2, pointsize = 14, type = "cairo", fps = 5)

anim

anim_save("slide-vs-expand.gif", animation = anim, path = "anim")
