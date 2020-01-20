library(slide)
library(ggplot2)
library(gganimate)
library(tibble)
library(vctrs)
library(scales)
library(magrittr)

slide_xmin <- slide_int(1:10, ~.x[1], .before = 2)

slide_data <- tibble(
  xmin = slide_xmin,
  xmax = c(1.5, 2:10),
  ymin = 5L,
  ymax = 7L,
  group = seq_along(xmin),
  text = c(
    "a",
    "a / b",
    "a / b / c",
    "b / c / d",
    "c / d / e",
    "d / e / f",
    "e / f / g",
    "f / g / h",
    "g / h / i",
    "h / i / j"
  )
)

index <- c(1, 2, 4, 5, 6, 7, 9, 10, 11, 13)
slide_index_xmin <- slide_index_int(1:10, index, ~.x[1], .before = 2)

slide_index_data <- tibble(
  xmin = slide_index_xmin,
  xmax = c(1.5, 2:10),
  ymin = 1L,
  ymax = 3L,
  group = seq_along(xmin),
  text = c(
    "a",
    "a / b",
    "b / c",
    "c / d",
    "c / d / e",
    "d / e / f",
    "f / g",
    "g / h",
    "g / h / i",
    "i / j"
  )
)

index_label_data <- tibble(
  x = 1:10,
  y = -2,
  label = as.character(index)
)

index_label_data <- vec_repeat(index_label_data, times = 10)
index_label_data[["group"]] <- vec_repeat(1:10, each = 10)
index_label_data[["colour"]] <- c(
  c(rep("black", 0), rep("pink", 1), rep("black", 9)),
  c(rep("black", 0), rep("pink", 2), rep("black", 8)),
  c(rep("black", 1), rep("pink", 2), rep("black", 7)),
  c(rep("black", 2), rep("pink", 2), rep("black", 6)),
  c(rep("black", 2), rep("pink", 3), rep("black", 5)),
  c(rep("black", 3), rep("pink", 3), rep("black", 4)),
  c(rep("black", 5), rep("pink", 2), rep("black", 3)),
  c(rep("black", 6), rep("pink", 2), rep("black", 2)),
  c(rep("black", 6), rep("pink", 3), rep("black", 1)),
  c(rep("black", 8), rep("pink", 2), rep("black", 0))
)

df <- vec_rbind(slide = slide_data, slide_index = slide_index_data, .names_to = "type")

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
    axis.text.y = element_text(family = "Inconsolata", size = 14, colour = "black"),
    # No axes titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # Use standard .5 margin + 2 margin spaces at the bottom for the two x axes
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
    data = df,
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
    data = df,
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
    fontface = "bold",
    size = 5,
    data = index_label_data
  ) +
  # Fix limits and turn off clipping to allow placement of axes
  coord_cartesian(ylim = c(0, 7), xlim = c(1, 10), clip = "off") +
  # Add index axis (size is not in points here, matched by eye)
  # Add index axis label
  annotate(
    "text",
    x = -0.25,
    y = -2,
    label = "Index:",
    family = "Inconsolata",
    size = 5,
    colour = "black",
    fontface = "bold"
  ) +
  # Add x axis
  annotate(
    "text",
    x = 1:10,
    y = -0.5,
    label = letters[1:10],
    family = "Inconsolata",
    size = 5,
    colour = "black"
  ) +
  # Add x axis label, use extra spacing to match Index:
  annotate(
    "text",
    x = -0.25,
    y = -0.5,
    label = "    X:",
    family = "Inconsolata",
    size = 5,
    colour = "black"
  ) +
  # Y axis labels
  scale_y_continuous(breaks = c(2, 6), labels = c("slide_index()", "slide()")) +
  scale_color_manual(values = c(pink = "#FF4888", black = "black")) +
  transition_manual(group)

anim <- animate(plot, width = 490*2, height = 163*2, res = 72*2, pointsize = 14, type = "cairo", fps = 5)

anim

anim_save("slide-index-vs-slide.gif", animation = anim, path = "anim")
