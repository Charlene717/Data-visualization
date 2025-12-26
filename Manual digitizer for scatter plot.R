## =========================================================
## Manual digitizer for PCA scatter from an image (PNG/JPG)
## - Click 4 calibration points (2 for x-axis ticks, 2 for y-axis ticks)
## - Then click N data points
## - Outputs a CSV with PC1/PC2
## =========================================================

suppressPackageStartupMessages({
  library(png)
  library(jpeg)
})

digitize_points_from_image <- function(
    img_path,
    x_tick_vals = c(-60, 60),   # numeric: left & right tick values on x-axis
    y_tick_vals = c(-60, 60),   # numeric: bottom & top tick values on y-axis
    n_points = 6,               # how many data points you want to click
    point_names = NULL,         # optional vector length n_points
    out_csv = "digitized_points.csv"
) {
  stopifnot(file.exists(img_path))
  stopifnot(length(x_tick_vals) == 2, length(y_tick_vals) == 2)
  stopifnot(is.numeric(x_tick_vals), is.numeric(y_tick_vals))
  stopifnot(n_points >= 1)
  
  ext <- tolower(tools::file_ext(img_path))
  img <- switch(
    ext,
    "png"  = png::readPNG(img_path),
    "jpg"  = jpeg::readJPEG(img_path),
    "jpeg" = jpeg::readJPEG(img_path),
    stop("Unsupported image type. Use PNG/JPG/JPEG.")
  )
  
  ## --- show image in base graphics (normalized 0..1 coordinates)
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  par(mar = c(1, 1, 1, 1))
  
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i",
       xlab = "", ylab = "", axes = FALSE)
  rasterImage(img, 0, 0, 1, 1)
  
  message("Step 1/3: Click TWO points on the X-axis ticks (LEFT then RIGHT).")
  xcal <- locator(2)
  if (is.null(xcal)) stop("No clicks detected.")
  points(xcal$x, xcal$y, pch = 4, cex = 1.4, lwd = 2)
  
  message("Step 2/3: Click TWO points on the Y-axis ticks (BOTTOM then TOP).")
  ycal <- locator(2)
  if (is.null(ycal)) stop("No clicks detected.")
  points(ycal$x, ycal$y, pch = 4, cex = 1.4, lwd = 2)
  
  ## calibration coordinates (in normalized image space)
  x1_pix <- xcal$x[1]; x2_pix <- xcal$x[2]
  y1_pix <- ycal$y[1]; y2_pix <- ycal$y[2]
  
  ## numeric axis values
  x1_val <- x_tick_vals[1]; x2_val <- x_tick_vals[2]
  y1_val <- y_tick_vals[1]; y2_val <- y_tick_vals[2]
  
  ## define pixel -> data mapping (assumes axes are not rotated)
  map_x <- function(px) x1_val + (px - x1_pix) / (x2_pix - x1_pix) * (x2_val - x1_val)
  map_y <- function(py) y1_val + (py - y1_pix) / (y2_pix - y1_pix) * (y2_val - y1_val)
  
  message(sprintf("Step 3/3: Click %d data points (in the order you want).", n_points))
  pts <- locator(n_points)
  if (is.null(pts) || length(pts$x) < n_points) stop("Not enough points clicked.")
  
  points(pts$x, pts$y, pch = 16, cex = 1.2)
  
  pc1 <- map_x(pts$x)
  pc2 <- map_y(pts$y)
  
  if (is.null(point_names)) {
    point_names <- paste0("P", seq_len(n_points))
  }
  stopifnot(length(point_names) == n_points)
  
  df_out <- data.frame(
    Sample = point_names,
    PC1 = pc1,
    PC2 = pc2
  )
  
  write.csv(df_out, out_csv, row.names = FALSE)
  message("Saved: ", normalizePath(out_csv, winslash = "/"))
  print(df_out)
  
  invisible(df_out)
}

## =========================================================
## ✅ Example usage
## 1) Put your image path here
## 2) Click x ticks: -60 then 60
## 3) Click y ticks: -60 then 60
## 4) Click points (e.g., HEALTHY1, HEALTHY2, RDEB1, RDEB2, RDEB3, RDEB4)
## =========================================================

df_clicked <- digitize_points_from_image(
  img_path = "C:/Users/q2330/Dropbox/KGD_Lab/200251226_PCA畫圈/螢幕擷取畫面 2025-12-26 141300.png",
  x_tick_vals = c(-60, 60),
  y_tick_vals = c(-60, 60),
  n_points = 6,
  point_names = c("HEALTHY1","HEALTHY2","RDEB1","RDEB2","RDEB3","RDEB4"),
  out_csv = "pca_points_clicked.csv"
)