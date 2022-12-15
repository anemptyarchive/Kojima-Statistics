
# 第3講 データの散らばり具合を見積もる統計量――分散と標準偏差 -----------------------------------------

# 利用パッケージ
library(tidyverse)
library(magick)

# チェック用
library(ggplot2)


# ch 3-2 ------------------------------------------------------------------

# データセットを作成
data_x <- c(32, 27, 29, 34, 33)

# データ数を取得
N <- length(data_x)

# 平均値を計算
mean_x <- sum(data_x) / N
mean_x

# 偏差を計算
deviation_x <- data_x - mean_x
deviation_x

# 偏差の和を確認
sum(deviation_x)

# 偏差の2乗を確認
deviation_x^2

# 分散を計算
variance_x <- sum(deviation_x^2) / N
variance_x

# 標準偏差を計算
sd_x <- sqrt(variance_x)
sd_x


# 関数を使う場合
var(data_x) * (N - 1) / N
sd(data_x) * sqrt(N - 1) / sqrt(N)


# ch 3-3 ------------------------------------------------------------------

### ・標準偏差の計算 -----

# データセットを作成
data_x <- c(4, 4, 5, 6, 6)
data_y <- c(1, 2, 6, 7, 9)

# 平均値を計算
mean_x <- sum(data_x) / length(data_x)
mean_y <- sum(data_y) / length(data_y)
mean_x; mean_y

# 偏差を計算
deviation_x <- data_x - mean_x
deviation_y <- data_y - mean_y
deviation_x; deviation_y

# 偏差の和を確認
sum(deviation_x); sum(deviation_y)

# 分散を計算
variance_x <- sum(deviation_x^2) / length(data_x)
variance_y <- sum(deviation_y^2) / length(data_y)
variance_x; variance_y

# 標準偏差を計算
sd_x <- sqrt(variance_x)
sd_y <- sqrt(variance_y)
sd_x; sd_y


### ・ヒストグラムと標準偏差の関係 -----

# 作図用のデータフレームを作成
data_x_df <- tibble::tibble(value = data_x)
data_y_df <- tibble::tibble(value = data_y)

# データセットxのヒストグラムを作成
ggplot() + 
  geom_histogram(data = data_x_df, mapping = aes(x = value), 
                 breaks = 0:11-0.5, closed ="left", fill = "#00A968") + # 度数
  geom_vline(xintercept = mean_x, 
             color = "red",size = 1, linetype = "dashed") + # 平均値
  geom_segment(mapping = aes(x = mean_x-sd_x, y = 0, xend = mean_x+sd_x, yend = 0), 
               color = "blue", size = 1, 
               arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 平均値 ± 標準偏差
  scale_x_continuous(breaks = 0:10) + 
  coord_cartesian(ylim = c(0, 2)) + # 表示範囲
  labs(title = "dataset x", 
       subtitle = parse(text = paste0("list(mean(x)==", mean_x, ", sd(x)==", sd_y, ")")), 
       x = "class value", y = "frequency")

# データセットyのヒストグラムを作成
ggplot() + 
  geom_histogram(data = data_y_df, mapping = aes(x = value), 
                 breaks = 0:11-0.5, closed ="left", fill = "#00A968") + # 度数
  geom_vline(xintercept = mean_y, 
             color = "red",size = 1, linetype = "dashed") + # 平均値
  geom_segment(mapping = aes(x = mean_y-sd_y, y = 0, xend = mean_y+sd_y, yend = 0), 
               color = "blue", size = 1, 
               arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 平均値 ± 標準偏差
  scale_x_continuous(breaks = 0:10) + 
  coord_cartesian(ylim = c(0, 2)) + # 表示範囲
  labs(title = "dataset y", 
       subtitle = parse(text = paste0("list(mean(y)==", mean_x, ", sd(y)==", sd_y, ")")), 
       x = "class value", y = "frequency")


# 度数を集計
freq_x_df <- tibble::tibble(i = 0:10) |> 
  dplyr::group_by(i) |> 
  dplyr::mutate(freq = sum(i == data_x)) |> 
  dplyr::ungroup()
freq_y_df <- tibble::tibble(i = 0:10) |> 
  dplyr::group_by(i) |> 
  dplyr::mutate(freq = sum(i == data_y)) |> 
  dplyr::ungroup()

# データセットxのヒストグラムを作成
ggplot() + 
  geom_bar(data = freq_x_df, mapping = aes(x = i, y = freq), 
           stat = "identity", width = 1, fill = "#00A968") + # 度数
  geom_vline(xintercept = mean_x, 
             color = "red",size = 1, linetype = "dashed") + # 平均値
  geom_segment(mapping = aes(x = mean_x-sd_x, y = 0, xend = mean_x+sd_x, yend = 0), 
               color = "blue", size = 1, 
               arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 平均値 ± 標準偏差
  scale_x_continuous(breaks = 0:10) + 
  coord_cartesian(ylim = c(0, 2)) + # 表示範囲
  labs(title = "dataset x", 
       subtitle = parse(text = paste0("list(mean(x)==", mean_x, ", sd(x)==", sd_x, ")")), 
       x = "class value", y = "frequency")

# データセットyのヒストグラムを作成
ggplot() + 
  geom_bar(data = freq_y_df, mapping = aes(x = i, y = freq), 
           stat = "identity", width = 1, fill = "#00A968") + # 度数
  geom_vline(xintercept = mean_y, 
             color = "red",size = 1, linetype = "dashed") + # 平均値
  geom_segment(mapping = aes(x = mean_y-sd_y, y = 0, xend = mean_y+sd_y, yend = 0), 
               color = "blue", size = 1, 
               arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 平均値 ± 標準偏差
  scale_x_continuous(breaks = 0:10) + 
  coord_cartesian(ylim = c(0, 2)) + # 表示範囲
  labs(title = "dataset y", 
       subtitle = parse(text = paste0("list(mean(y)==", mean_x, ", sd(y)==", sd_y, ")")), 
       x = "class value", y = "frequency")


# ch 3-4 ---------------------------------------------------------------------

# 度数分布表を作成
freq_df <- tibble::tibble(
  class_value = c(1, 2, 3, 4), # 階級値
  relative_freq = c(0.3, 0.5, 0.1, 0.1) # 相対度数
) |> 
  dplyr::mutate(
    weighted_class_value = class_value * relative_freq, # 階級値 × 相対度数
    deviation = class_value - sum(weighted_class_value), # 偏差
    deviation2 = deviation^2, # 偏差の2乗
    weighted_deviation2 = deviation2 * relative_freq # 偏差の2乗 × 相対度数
  )
freq_df

# 分散を計算
variance_x <- sum(freq_df[["weighted_deviation2"]])
variance_x

# 標準偏差を計算
sd_x <- sqrt(variance_x)
sd_x


# 練習問題 --------------------------------------------------------------------

# データセットを作成
data_x <- c(6, 4, 6, 6, 6, 3, 7, 2, 2, 8)

# 平均値を計算
mean_x <- sum(data_x) / length(data_x)
mean_x

# 偏差を計算
deviation_x <- data_x - mean_x
deviation_x

# 偏差の2乗を計算
deviation2_x <- deviation_x^2
deviation2_x

# 分散を計算
variance_x <- sum(deviation2_x) / length(data_x)
variance_x

# 標準偏差を計算
sd_x <- sqrt(variance_x)
sd_x


# おまけ：ヒストグラムと標準偏差の関係のアニメーション --------------------------------------------------------------------

# 画像の保存先を指定
dir_path <- "tmp_folder"

# 階級幅の最大値(フレーム数)を指定
N <- 30

# 階級を変更して作図
data_x <- NULL
for(n in 1:N) {
  # データを生成
  data_x[n] <- sample(x = 1:10, size = 1)
  
  # データを格納
  data_df <- tibble::tibble(
    value = data_x
  )
  
  # 平均値と標準偏差を計算
  mean_x <- sum(data_x) / length(data_x)
  sd_x <- sqrt((sum(data_x) - mean_x) / length(data_x))
  print(sd_x)
  
  # ヒストグラムを作成
  g <- ggplot() + 
    geom_histogram(data = data_df, mapping = aes(x = value), 
                   breaks = 0:11-0.5, closed ="left", fill = "#00A968") + # 度数
    geom_vline(xintercept = mean_x, 
               color = "red",size = 1, linetype = "dashed") + # 平均値
    geom_segment(mapping = aes(x = mean_x-sd_x, y = 0, xend = mean_x+sd_x, yend = 0), 
                 color = "blue", size = 1, 
                 arrow = arrow(ends = "both", length = unit(10, units = "pt"))) + # 平均値 ± 標準偏差
    scale_x_continuous(breaks = 0:10) + 
    coord_cartesian(ylim = c(0, N/5)) + # 表示範囲
    labs(title = "dataset x", 
         subtitle = parse(text = paste0("list(mean(x)==", round(mean_x, 3), ", sd(x)==", round(sd_x, 3), ")")), 
         x = "class value", y = "frequency")
  
  # グラフを書き出し
  file_name <- stringr::str_pad(n, width = 2, side = "left", pad = 0)
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
magick::image_write_gif(image = gif_data, path = "figure/ch03_histgram.gif", delay = 1/2)

