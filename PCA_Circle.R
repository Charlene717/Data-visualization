suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

## -------------------------
## 原始點
## -------------------------
df <- data.frame(
  Sample    = c("HEALTHY1","HEALTHY2","RDEB1","RDEB2","RDEB3","RDEB4"),
  Condition = c("HEALTHY","HEALTHY","RDEB","RDEB","RDEB","RDEB"),
  PC1       = c( 31.781473, 35.201900,  -10.118765, -31.781473, -22.660333,  -2.422803),
  PC2       = c( -2.553191, -5.106383,  14.680851, -18.510638,   7.659574,   2.553191)
)
df$Condition <- factor(df$Condition, levels = c("HEALTHY","RDEB"))

## -------------------------
## 🔧 workaround：讓 HEALTHY 有第 3 點（極小 jitter）
## -------------------------
eps <- 1e-6
df_ellipse <- bind_rows(
  df,
  df %>%
    filter(Condition == "HEALTHY") %>%
    slice(1) %>%
    mutate(
      PC1 = PC1 + eps,
      PC2 = PC2 + eps,
      Sample = "HEALTHY_fake"
    )
)

## -------------------------
## 典雅配色
## -------------------------
cols <- c(
  HEALTHY = "#2F7D32",
  RDEB    = "#8B1E1E"
)

## -------------------------
## 標籤自動避讓
## -------------------------
use_repel <- requireNamespace("ggrepel", quietly = TRUE)

p <- ggplot(df_ellipse, aes(PC1, PC2, color = Condition)) +
  
  ## filled ellipse
  stat_ellipse(
    aes(fill = Condition, group = Condition),
    geom  = "polygon",
    alpha = 0.12,
    color = NA,
    level = 0.95,
    show.legend = FALSE
  ) +
  
  ## ellipse outline
  stat_ellipse(
    aes(group = Condition),
    level = 0.95,
    linewidth = 1
  ) +
  
  ## 真實點
  geom_point(
    data = df,
    size = 3
  ) +
  
  ## 標籤
  { if (use_repel)
    ggrepel::geom_text_repel(
      data = df,
      aes(label = Sample),
      size = 5,
      fontface = "bold",
      box.padding = 0.4,
      point.padding = 0.3,
      max.overlaps = Inf,
      show.legend = FALSE
    )
    else
      geom_text(
        data = df,
        aes(label = Sample),
        vjust = -0.9,
        size = 5,
        fontface = "bold",
        show.legend = FALSE
      )
  } +
  
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60)) +
  
  ## =========================
## ✅ 灰階背景 + 白色網格（像原圖）
## =========================
theme(
  panel.background = element_rect(fill = "grey90", color = NA),
  plot.background  = element_rect(fill = "white", color = NA),
  
  panel.grid.major = element_line(color = "white", linewidth = 0.8),
  panel.grid.minor = element_line(color = "white", linewidth = 0.4),
  
  axis.line        = element_blank(),
  axis.ticks       = element_blank(),
  
  axis.title       = element_text(face = "bold", size = 14),
  axis.text        = element_text(size = 12),
  legend.title     = element_text(face = "bold", size = 13),
  legend.text      = element_text(size = 12)
) +
  
  labs(
    x = "PC1: 72% variance",
    y = "PC2: 12% variance",
    color = "Condition"
  )

print(p)