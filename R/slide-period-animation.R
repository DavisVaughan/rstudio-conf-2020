library(slide)
library(ggplot2)
library(gganimate)
library(tibble)
library(vctrs)
library(scales)
library(magrittr)

index <- as.Date(c(
  "2019-01-01",
  "2019-01-02",
  "2019-02-01",
  "2019-03-01",
  "2019-05-02",
  "2019-05-05",
  "2019-06-01",
  "2019-09-01",
  "2019-09-05",
  "2019-10-01",
  "2019-11-01",
  "2019-11-30",
  "2019-12-10",
  "2020-02-02"
))

index_label <- c(
  "01\nJan",
  "02",
  "01\nFeb",
  "01\nMar",
  "02\nMay",
  "05",
  "01\nJun",
  "01\nSep",
  "05",
  "01\nOct",
  "01\nNov",
  "30",
  "10\nDec",
  "02\nFeb"
)

xmin_data <- slide_period_int(seq_along(index), index, "month", ~.x[1], .before = 2)
xmax_data <- slide_period_int(seq_along(index), index, "month", ~.x[length(.x)], .before = 2)

period_data <- tibble(
  xmin = xmin_data,
  xmax = xmax_data,
  ymin = 1L,
  ymax = 4L,
  group = seq_along(xmin),
  text = c(
    "Nov -\nJan",
    "Dec - Feb",
    "Jan - Mar",
    "Mar - May",
    "Apr - Jun",
    "Jul -\nSep",
    "Aug - Oct",
    "Sep - Nov",
    "Oct - Dec",
    "Dec -\nFeb"
  )
)

index_label_data <- tibble(
  x = seq_along(index),
  y = c(-1.25, -.35, -1.25, -1.25, -1.25, -.35, -1.25, -1.25, -.35, -1.25, -1.25, -.35, -1.25, -1.25),
  label = index_label
)

index_label_data <- vec_repeat(index_label_data, times = 10)
index_label_data[["group"]] <- vec_repeat(1:10, each = 14)
index_label_data[["colour"]] <- c(
  c(rep("black", 0), rep("pink", 2), rep("black", 12)),
  c(rep("black", 0), rep("pink", 3), rep("black", 11)),
  c(rep("black", 0), rep("pink", 4), rep("black", 10)),
  c(rep("black", 3), rep("pink", 3), rep("black", 8)),
  c(rep("black", 4), rep("pink", 3), rep("black", 7)),
  c(rep("black", 7), rep("pink", 2), rep("black", 5)),
  c(rep("black", 7), rep("pink", 3), rep("black", 4)),
  c(rep("black", 7), rep("pink", 5), rep("black", 2)),
  c(rep("black", 9), rep("pink", 4), rep("black", 1)),
  c(rep("black", 12), rep("pink", 2), rep("black", 0))
)

plot <- ggplot() +
  theme_minimal() +
  theme(
    # Background plot color
    plot.background = element_rect(fill = "#DCDCDC", color = NA),
    # Grid lines get in the way
    panel.grid = element_blank(),
    # Manually adding X axes
    axis.text.x = element_blank(),
    # 14 point size, matching keynote font and color
    axis.text.y = element_blank(),
    # No axes titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # Use standard .5 margin + 2 margin spaces at the bottom for the x axis
    plot.margin = unit(c(.5, .5, 2.5, .5), "lines"),
    # no legend
    legend.position = "none"
  ) +
  geom_rect(
    mapping = aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      group = group
    ),
    data = period_data,
    fill = "#353535"
  ) +
  # Place text in the center of the rectangles
  geom_text(
    mapping = aes(
      x = xmin + (xmax - xmin) / 2,
      y = ymin + (ymax - ymin) / 2,
      label = text,
      group = group
    ),
    data = period_data,
    color = "#DCDCDC"
  ) +
  geom_text(
    mapping = aes(
      x = x,
      y = y,
      label = label,
      group = group,
      colour = colour
    ),
    family = "Inconsolata",
    size = 5,
    data = index_label_data
  ) +
  # Fix limits and turn off clipping to allow placement of axes
  coord_cartesian(ylim = c(0, 4), xlim = c(1, 14), clip = "off") +
  scale_color_manual(values = c(pink = "#FF4888", black = "black")) +
  transition_manual(group)

anim <- animate(plot, width = 490*2, height = 100*2, res = 72*2, pointsize = 14, type = "cairo", duration = 20)

anim

anim_save("slide-period.gif", animation = anim, path = "anim")
