
# 第2講 平均値とはやじろべえの支点である――平均値の役割と捉え方 ----------------------------------------

# 利用パッケージ
library(tidyverse)
library(magick)

# チェック用
library(ggplot2)


# ch 1-1~2 ------------------------------------------------------------------

# データを作成:(身長)
data_x <- c(
  151, 154, 158, 162, 
  154, 152, 151, 167, 
  160, 161, 155, 159, 
  160, 160, 155, 153, 
  163, 160, 165, 146, 
  156, 153, 165, 156, 
  158, 155, 154, 160, 
  156, 163, 148, 151, 
  154, 160, 169, 151, 
  160, 159, 158, 157, 
  154, 164, 146, 151, 
  162, 158, 166, 156, 
  156, 150, 161, 166, 
  162, 155, 143, 159, 
  157, 157, 156, 157, 
  162, 161, 156, 156, 
  162, 168, 149, 159, 
  169, 162, 162, 156, 
  150, 153, 159, 156, 
  162, 154, 164, 161
)

# データ数を取得
N <- length(data_x)
N

# 階級の幅を指定
class_width <- 5

# 階級用の値を作成
class_vec <- seq(
  from = floor(min(data_x) / 10) * 10, 
  to = ceiling(max(data_x) / 10) * 10, 
  by = class_width
)
class_vec


# ch 2-2~4 ------------------------------------------------------------------

# データセットから平均値を計算
mean_x <- sum(data_x) / N
mean_x


# 度数分布表を作成:(離散値)
freq_df <- tibble::tibble(
  class_lower = class_vec[-length(class_vec)] + 1, # 階級の下限
  class_upper = class_vec[-1] # 階級の上限
) |> 
  dplyr::group_by(class_lower, class_upper) |> # 階級ごとの計算用
  dplyr::mutate(
    class_value = median(c(class_lower, class_upper)), # 階級値
    freq = sum((data_x >= class_lower) == (data_x <= class_upper)) # 度数
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    relative_freq = freq / N, # 相対度数
    cumulative_freq = cumsum(freq), # 累積度数
    weighted_class_value = class_value * relative_freq # 階級値 × 相対度数
  )
freq_df

# 階級値と相対度数から平均値を計算
mean_x <- sum(freq_df[["weighted_class_value"]])
mean_x

# ヒストグラムと平均値の関係を作図
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = class_value, y = freq), 
           stat = "identity", width = class_width, fill = "#00A968") + # ヒストグラム
  geom_vline(xintercept = mean_x, color ="red", size = 1, linetype ="dashed") + # 平均値
  scale_x_continuous(breaks = freq_df[["class_value"]]) + # x軸
  labs(title = "Histogram", 
       subtitle = parse(text = paste0("bar(x)==", mean_x)), 
       x = "class value", y = "frequency")


# データを格納
data_df <- tibble::tibble(
  n = 1:N, # データ番号
  x = data_x
)

# 各データと平均値の関係を作図
mean_x <- sum(data_df[["x"]]) / nrow(data_df)
ggplot() + 
  geom_bar(data = data_df, mapping = aes(x = n, y = x), 
           stat = "identity", fill = "#00A968") + # データセット
  geom_hline(yintercept = mean_x, color ="red", size = 1, linetype ="dashed") + # 平均値
  labs(title = "dataset", 
       subtitle = parse(text = paste0("bar(x)==", mean_x)), 
       x = "n", y = expression(x[n]))


# 階級値を複製
class_value_df <- freq_df |> 
  dplyr::select(class_value, freq) |> 
  tidyr::uncount(freq) |> # 度数に応じて複製
  dplyr::mutate(n = dplyr::row_number()) # データ番号

# 各階級と平均値の関係を作図
mean_x <- sum(freq_df[["weighted_class_value"]])
ggplot() + 
  geom_bar(data = class_value_df, mapping = aes(x = n, y = class_value), 
           stat = "identity", fill = "#00A968") + # データセット
  geom_hline(yintercept = mean_x, color ="red", size = 1, linetype ="dashed") + # 平均値
  labs(title = "class value", 
       subtitle = parse(text = paste0("bar(x)==", mean_x)), 
       x = "n", y = expression(x[n]))


# 練習問題 --------------------------------------------------------------------

# 度数分布を作成
freq_df <- tibble::tibble(
  class_value = c(30, 50, 70, 90, 110, 130), # 階級値
  freq = c(5, 10, 15, 40, 20, 10) # 度数
) |> 
  dplyr::mutate(
    relative_freq = freq / sum(freq), # 相対度数
    cumulative_freq = cumsum(freq), # 累積度数
    weighted_class_value = class_value * relative_freq # 階級値 × 相対度数
  )
freq_df

# 階級値と相対度数から平均値を計算
mean_x <- sum(freq_df[["weighted_class_value"]])
mean_x


# おまけ：階級とヒストグラムの関係 ---------------------------------------------------------------------

# 画像の保存先を指定
dir_path <- "tmp_folder"

# 階級幅の最大値(フレーム数)を指定
class_width_max <- 30

# 階級を変更して作図
for(class_width in 1:class_width_max) {
  # 階級用の値を作成
  class_vec <- seq(
    from = floor(min(data_x) / 10) * 10, 
    to = ceiling(max(data_x) / 10) * 10, 
    by = class_width
  )
  class_vec <- c(class_vec, class_vec[length(class_vec)]+class_width) # 集計用に上限以上の値を1つ追加
  
  # 度数分布表を作成:(離散値)
  freq_df <- tibble::tibble(
    class_lower = class_vec[-length(class_vec)] + 1, # 階級の下限
    class_upper = class_vec[-1] # 階級の上限
  ) |> 
    dplyr::group_by(class_lower, class_upper) |> # 階級ごとの計算用
    dplyr::mutate(
      class_value = median(c(class_lower, class_upper)), # 階級値
      freq = sum((data_x >= class_lower) == (data_x <= class_upper)) # 度数
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      relative_freq = freq / length(data_x), # 相対度数
      weighted_class_value = class_value * relative_freq # 階級値 × 相対度数
    )
  
  # 平均値を計算
  mean_x <- sum(freq_df[["weighted_class_value"]])
  
  # ヒストグラムと平均値の関係を作図
  g <- ggplot() + 
    geom_bar(data = freq_df, mapping = aes(x = class_value, y = freq), 
             stat = "identity", width = class_width, fill = "#00A968") + # ヒストグラム
    geom_vline(xintercept = mean_x, color ="red", size = 1, linetype ="dashed") + # 平均値
    scale_x_continuous(breaks = freq_df[["class_value"]]) + # x軸
    coord_cartesian(xlim = c(min(data_x), max(data_x)), 
                    ylim = c(0, length(data_x))) + # 表示範囲
    labs(title = "Histogram", 
         subtitle = parse(text = paste0("bar(x)==", mean_x)), 
         x = "class value", y = "frequency")
  
  # グラフを書き出し
  file_name <- stringr::str_pad(class_width, width = 2, side = "left", pad = 0)
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
magick::image_write_gif(image = gif_data, path = "figure/ch02_histgram.gif", delay = 1/2)


