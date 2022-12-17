
# 第4講 そのデータは「月並み」か「特殊」か？標準偏差で評価する -----------------------------------------

# 利用パッケージ
library(tidyverse)
library(magick)

# チェック用
library(ggplot2)


# ch 4-4 ----------------------------------------------------------------

### ・データの加工 -----

# データ数を指定
N <- 30

# データを生成
data_x <- rnorm(n = N, mean = 0, sd = 1)

# 平均値・分散・標準偏差を計算
mean_x     <- sum(data_x) / length(data_x)
variance_x <- sum((data_x - mean_x)^2) / length(data_x)
sd_x       <- sqrt(variance_x)
mean_x; variance_x; sd_x


# 定数を指定
a <- 3

# データを加工
data_y <- data_x + a
data_y <- data_x * a

# 平均値・分散・標準偏差を計算
mean_y     <- sum(data_y) / length(data_y)
variance_y <- sum((data_y - mean_y)^2) / length(data_y)
sd_y       <- sqrt(variance_y)
mean_y; variance_y; sd_y


# データを格納
data_x_df <- tibble::tibble(value = data_x)
data_y_df <- tibble::tibble(value = data_y)

# 階級値を作成
class_width <- 0.5
class_value_vec <- seq(
  from = floor(min(data_x, data_y)), 
  to = ceiling(max(data_x, data_y)), 
  by = class_width
)
length(class_value_vec)

# ラベル用の文字列を作成
label_text <- paste0(
  "list(", 
  "N==", N, 
  ", bar(x)==", round(mean_x, 3), ", S[x]==", round(sd_x, 3), 
  ", bar(y)==", round(mean_y, 3), ", S[y]==", round(sd_y, 3), 
  ")"
)

# ヒストグラムを作成
ggplot() + 
  geom_histogram(data = data_x_df, mapping = aes(x = value), 
                 breaks = class_value_vec+0.5*class_width, closed ="left", 
                 color = "#00A968", fill = NA, size = 1, linetype = "dashed") + # 元データの度数
  geom_vline(xintercept = mean_x, 
             color = "red", size = 1, linetype = "dashed") + # 元データの平均値
  geom_segment(mapping = aes(x = mean_x-sd_x, y = 0, xend = mean_x+sd_x, yend = 0), 
               color = "blue", size = 1, linetype = "dashed", 
               arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 元データの平均値 ± 標準偏差
  geom_histogram(data = data_y_df, mapping = aes(x = value), 
                 breaks = class_value_vec, closed ="left", 
                 fill = "#00A968", alpha = 0.5) + # 加工データの度数
  geom_vline(xintercept = mean_y, 
             color = "red", size = 1) + # 加工データの平均値
  geom_segment(mapping = aes(x = mean_y-sd_y, y = 0, xend = mean_y+sd_y, yend = 0), 
               color = "blue", size = 1, 
               arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 加工データの平均値 ± 標準偏差
  scale_x_continuous(breaks = seq(from = min(class_value_vec), to = max(class_value_vec), by = 1)) + 
  labs(
    #title = expression(y[n] == x[n] + a), 
    title = expression(y[n] == a * x[n]), 
    subtitle = parse(text = label_text), 
    x = "class value", y = "frequency"
  )


### ・データの標準化 -----

# データ数を指定
N <- 30

# 平均・標準偏差パラメータを指定
mu    <- 5
sigma <- 2

# データを生成
data_x <- rnorm(n = N, mean = mu, sd = sigma)

# 平均値・分散・標準偏差を計算
mean_x     <- sum(data_x) / length(data_x)
variance_x <- sum((data_x - mean_x)^2) / length(data_x)
sd_x       <- sqrt(variance_x)
mean_x; variance_x; sd_x


# データを加工
data_y <- (data_x - mean_x) / sd_x

# 平均値・分散・標準偏差を計算
mean_y     <- sum(data_y) / length(data_y)
variance_y <- sum((data_y - mean_y)^2) / length(data_y)
sd_y       <- sqrt(variance_y)
mean_y; variance_y; sd_y


# データを格納
data_x_df <- tibble::tibble(value = data_x)
data_y_df <- tibble::tibble(value = data_y)

# 階級値を作成
class_width <- 0.5
class_value_vec <- seq(
  from = floor(min(data_x, data_y)), 
  to = ceiling(max(data_x, data_y)), 
  by = class_width
)
length(class_value_vec)

# ラベル用の文字列を作成
param_text <- paste0(
  'x[n] %~% N(x ~ "|" ~ mu==', mu, ", sigma==", sigma, ")"
)
label_text <- paste0(
  "list(", 
  "N==", N, 
  ", bar(x)==", round(mean_x, 3), ", S[x]==", round(sd_x, 3), 
  ", bar(y)==", round(mean_y, 3), ", S[y]==", round(sd_y, 3), 
  ")"
)

# ヒストグラムを作成
ggplot() + 
  geom_histogram(data = data_x_df, mapping = aes(x = value), 
                 breaks = class_value_vec+0.5*class_width, closed ="left", 
                 color = "#00A968", fill = NA, size = 1, linetype = "dashed") + # 元データの度数
  geom_vline(xintercept = mean_x, 
             color = "red", size = 1, linetype = "dashed") + # 元データの平均値
  geom_segment(mapping = aes(x = mean_x-sd_x, y = 0, xend = mean_x+sd_x, yend = 0), 
               color = "blue", size = 1, linetype = "dashed", 
               arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 元データの平均値 ± 標準偏差
  geom_histogram(data = data_y_df, mapping = aes(x = value), 
                 breaks = class_value_vec+0.5*class_width, closed ="left", 
                 fill = "#00A968", alpha = 0.5) + # 加工データの度数
  geom_vline(xintercept = mean_y, 
             color = "red", size = 1) + # 加工データの平均値
  geom_segment(mapping = aes(x = mean_y-sd_y, y = 0, xend = mean_y+sd_y, yend = 0), 
               color = "blue", size = 1, 
               arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 加工データの平均値 ± 標準偏差
  scale_x_continuous(breaks = seq(from = min(class_value_vec), to = max(class_value_vec), by = 1)) + 
  labs(title = parse(text = param_text), 
       subtitle = parse(text = label_text), 
       x = "class value", y = "frequency")


# おまけ：標準化処理をアニメーションで可視化 -------------------------------------------------------

# データ数を指定
N <- 30

# 平均・標準偏差パラメータを指定
mu    <- 5
sigma <- 2

# データを生成
data_x <- rnorm(n = N, mean = mu, sd = sigma)

# 平均値・分散・標準偏差を計算
mean_x     <- sum(data_x) / length(data_x)
variance_x <- sum((data_x - mean_x)^2) / length(data_x)
sd_x       <- sqrt(variance_x)
mean_x; variance_x; sd_x


# 画像の保存先を指定
dir_path <- "tmp_folder"

# フレーム数を指定
frame_num <- 30

# 定数として利用する値を作成
a_vals <- seq(from = 0, to = mean_x, length.out = frame_num)
b_vals <- seq(from = 1, to = sd_x, length.out = frame_num)

# データ加工用の値を変更して作図
for(i in 1:frame_num) {
  # i番面の値を取得
  a <- a_vals[i]
  b <- b_vals[i]
  
  # データを加工
  data_y <- (data_x - a) / b
  
  # 平均値・分散・標準偏差を計算
  mean_y     <- sum(data_y) / length(data_y)
  variance_y <- sum((data_y - mean_y)^2) / length(data_y)
  sd_y       <- sqrt(variance_y)
  
  # データを格納
  data_x_df <- tibble::tibble(value = data_x)
  data_y_df <- tibble::tibble(value = data_y)
  
  # 階級値を作成
  class_width <- 0.5
  class_value_vec <- seq(
    from = floor(min(data_x, data_y)), 
    to = ceiling(max(data_x, data_y)), 
    by = class_width
  )
  
  # ラベル用の文字列を作成
  data_text <- paste0(
    "y[n]==(x[n] - ", round(a, 2), ") / ", round(b, 2)
  )
  label_text <- paste0(
    "list(", 
    "N==", N, 
    ", bar(x)==", round(mean_x, 3), ", S[x]==", round(sd_x, 3), 
    ", bar(y)==", round(mean_y, 3), ", S[y]==", round(sd_y, 3), 
    ")"
  )
  
  # ヒストグラムを作成
  g <- ggplot() + 
    geom_histogram(data = data_x_df, mapping = aes(x = value), 
                   breaks = class_value_vec+0.5*class_width, closed ="left", 
                   color = "#00A968", fill = NA, size = 1, linetype = "dashed") + # 元データの度数
    geom_vline(xintercept = mean_x, 
               color = "red", size = 1, linetype = "dashed") + # 元データの平均値
    geom_segment(mapping = aes(x = mean_x-sd_x, y = 0, xend = mean_x+sd_x, yend = 0), 
                 color = "blue", size = 1, linetype = "dashed", 
                 arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 元データの平均値 ± 標準偏差
    geom_histogram(data = data_y_df, mapping = aes(x = value), 
                   breaks = class_value_vec+0.5*class_width, closed ="left", 
                   fill = "#00A968", alpha = 0.5) + # 加工データの度数
    geom_vline(xintercept = mean_y, 
               color = "red", size = 1) + # 加工データの平均値
    geom_segment(mapping = aes(x = mean_y-sd_y, y = 0, xend = mean_y+sd_y, yend = 0), 
                 color = "blue", size = 1, 
                 arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 加工データの平均値 ± 標準偏差
    scale_x_continuous(breaks = seq(from = -4, to = 12, by = 1)) + 
    coord_cartesian(xlim = c(-4, 12), ylim = c(0, 8)) + # 表示範囲
    labs(title = parse(text = data_text), 
         subtitle = parse(text = label_text), 
         x = "class value", y = "frequency")
  
  # グラフを書き出し
  file_name <- stringr::str_pad(i, width = 2, side = "left", pad = 0)
  ggplot2::ggsave(
    filename = paste0(dir_path, "/", file_name, ".png"), plot = g, 
    width = 800, height = 600, units = "px", dpi = 100
  )
}

# ファイルパスを作成
file_path_vec <- dir_path |> 
  list.files() |> # ファイル名を取得
  (\(.){paste0(dir_path, "/", .)})()

# gif画像を作成
gif_data <- file_path_vec |> 
  magick::image_read() |> # 画像ファイルを読み込み
  magick::image_animate(fps = 2, dispose = "previous")

# gif画像を書き出し
magick::image_write_gif(image = gif_data, path = "figure/ch04_histgram.gif", delay = 1/2)


