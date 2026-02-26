library(tidyverse)
library(latex2exp)
library(extrafont)

font_import()
loadfonts(device = "win")

legend_only_plot <- function(
    labels,
    linetypes=NULL,
    colors = NULL,
    title = NULL,
    nrow = NULL,
    ncol = NULL,
    key_width = 1,
    key_height = 1,
    text_size = 12,
    title_size = 13,
    position = "right"
) {
  
  df <- data.frame(
    x = 1,
    y = seq_along(labels),
    group = factor(labels, levels = labels)
  )
  
  ggplot(df, aes(x, y, color = group, linetype=group)) +
    geom_line(linewidth = 1) +
    scale_linetype_manual(
      values = linetypes,
      name = title, 
      labels = TeX(labels)
    )  +
    scale_color_manual(
      values = colors,
      name = title, 
      labels = TeX(labels)
    ) +
    guides(
      color = guide_legend(
        nrow = nrow,
        ncol = ncol,
        byrow = TRUE,
        keywidth = key_width,
        keyheight = key_height
      ),
      linetype = guide_legend(
        nrow = nrow,
        ncol = ncol,
        byrow = TRUE,
        keywidth = key_width,
        keyheight = key_height
      )
    ) +
    theme_void() +
    theme(
      legend.position = position,
      legend.text = element_text(size = text_size, family="CMU Serif"),
      legend.title = element_text(size = title_size , family="CMU Serif")
    )
}

legend_only_plot(
  labels = c("$w_0=40, m_0=160$", "$w_0 = 10, m_0 = 40$", "$w_0 = 4, m_0 = 16$"),
  linetypes = c("solid", "dashed", "dotted"),
  colors = c("#004488", "#BB5566", "#DDAA33"),
  title = "Group",
  ncol = 3,
  nrow=1,
  text_size = 10, 
  key_width = 2, 
  key_height=5
)
