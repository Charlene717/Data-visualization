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

## =========================
## ✅ 你只要調這兩個參數
## =========================
level_RDEB    <- 0.80  # ↓變小 = 更緊（常用 0.68 / 0.80 / 0.90）
level_HEALTHY <- 0.95  # ↑變大 = 更鬆（0.90~0.99 都可）
pair_rel      <- 0.18  # HEALTHY 補點偏移比例：↑更鬆，↓更擠（0.10~0.30 常用）

## -------------------------
## 對 n=2 的群補 2 點（讓 stat_ellipse 穩定可畫）
## -------------------------
add_two_points_for_pairs <- function(dat, group_col = "Condition",
                                     x_col = "PC1", y_col = "PC2",
                                     rel = 0.18, min_abs = 1e-3) {
  dat %>%
    group_by(.data[[group_col]]) %>%
    group_modify(function(d, key) {
      if (nrow(d) != 2) return(d)
      
      x1 <- d[[x_col]][1]; y1 <- d[[y_col]][1]
      x2 <- d[[x_col]][2]; y2 <- d[[y_col]][2]
      dx <- x2 - x1; dy <- y2 - y1
      dist <- sqrt(dx^2 + dy^2)
      if (dist == 0) dist <- 1
      
      # perpendicular unit vector
      ux <- -dy / dist
      uy <-  dx / dist
      
      off <- max(dist * rel, min_abs)
      mx <- (x1 + x2) / 2
      my <- (y1 + y2) / 2
      
      pA <- d[1, , drop = FALSE]; pA$Sample <- paste0(as.character(key[[1]]), "_fakeA")
      pB <- d[1, , drop = FALSE]; pB$Sample <- paste0(as.character(key[[1]]), "_fakeB")
      
      pA[[x_col]] <- mx + ux * off
      pA[[y_col]] <- my + uy * off
      pB[[x_col]] <- mx - ux * off
      pB[[y_col]] <- my - uy * off
      
      bind_rows(d, pA, pB)
    }) %>%
    ungroup()
}

df_ellipse <- add_two_points_for_pairs(df, rel = pair_rel)

## -------------------------
## 配色
## -------------------------
cols <- c(HEALTHY = "#2F7D32", RDEB = "#8B1E1E")

## 標籤自動避讓
use_repel <- requireNamespace("ggrepel", quietly = TRUE)

## -------------------------
## 分開畫 ellipse（同樣 stat_ellipse，但各自 level）
## -------------------------
df_h <- df_ellipse %>% filter(Condition == "HEALTHY")
df_r <- df_ellipse %>% filter(Condition == "RDEB")

p <- ggplot() +
  
  ## RDEB (較緊)
  stat_ellipse(
    data = df_r,
    aes(PC1, PC2, group = Condition, fill = Condition),
    type = "norm", level = level_RDEB,
    geom = "polygon", alpha = 0.12, color = NA, show.legend = FALSE
  ) +
  stat_ellipse(
    data = df_r,
    aes(PC1, PC2, group = Condition, color = Condition),
    type = "norm", level = level_RDEB,
    linewidth = 1
  ) +
  
  ## HEALTHY (較鬆)
  stat_ellipse(
    data = df_h,
    aes(PC1, PC2, group = Condition, fill = Condition),
    type = "norm", level = level_HEALTHY,
    geom = "polygon", alpha = 0.12, color = NA, show.legend = FALSE
  ) +
  stat_ellipse(
    data = df_h,
    aes(PC1, PC2, group = Condition, color = Condition),
    type = "norm", level = level_HEALTHY,
    linewidth = 1
  ) +
  
  ## 真實點
  geom_point(
    data = df,
    aes(PC1, PC2, color = Condition),
    size = 3
  ) +
  
  ## 標籤（只標原始點）
  { if (use_repel)
    ggrepel::geom_text_repel(
      data = df,
      aes(PC1, PC2, label = Sample, color = Condition),
      size = 5, fontface = "bold",
      box.padding = 0.4, point.padding = 0.3,
      max.overlaps = Inf, show.legend = FALSE
    )
    else
      geom_text(
        data = df,
        aes(PC1, PC2, label = Sample, color = Condition),
        vjust = -0.9, size = 5, fontface = "bold", show.legend = FALSE
      )
  } +
  
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60)) +
  
  ## 灰階背景 + 白色網格（像原圖）
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
  labs(x = "PC1: 72% variance", y = "PC2: 12% variance", color = "Condition")

print(p)