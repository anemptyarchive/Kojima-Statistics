
# 第1講 度数分布表とヒストグラムで、データの特徴を浮き彫りにする ----------------------------------------

# 利用パッケージ
library(tidyverse)

# チェック用
library(ggplot2)


# ch 1-1~2 -----------------------------------------------------------------

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
length(data_x)

# データを作成:(体重)
data_x <- c(
  48, 54, 47, 50, 53, 43, 45, 43, 
  44, 47, 58, 46, 46, 63, 49, 50, 
  48, 43, 46, 45, 50, 53, 51, 58, 
  52, 53, 47, 49, 45, 42, 51, 49, 
  58, 54, 45, 53, 50, 69, 44, 50, 
  58, 64, 40, 57, 51, 69, 58, 47, 
  62, 47, 40, 60, 48, 47, 53, 47, 
  52, 61, 55, 55, 48, 48, 46, 52, 
  45, 38, 62, 47, 55, 50, 46, 47, 
  55, 48, 50, 50, 54, 55, 48, 50
)
length(data_x)

# 最小値・最大値を抽出
min_x <- min(data_x)
max_x <- max(data_x)
min_x; max_x

# 階級を指定
class_vec <- seq(from = floor(min_x/10)*10, to = ceiling(max_x/10)*10, by = 5)
class_vec

# 度数分布表を作成:(連続値)
freq_df <- tibble::tibble(
  class_lower = class_vec[-length(class_vec)], # 階級の下限
  class_upper = class_vec[-1] # 階級の上限
) |> 
  dplyr::group_by(class_lower, class_upper) |> 
  dplyr::mutate(
    class_value = median(c(class_lower, class_upper)), # 階級値
    freq = sum((data_x >= class_lower) == (data_x < class_upper)) # 度数
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    relative_freq = freq / length(data_x), # 相対度数
    cumulative_freq = cumsum(freq) # 累積度数
  )
freq_df

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
    cumulative_freq = cumsum(freq) # 累積度数
  )
freq_df

# ヒストグラムを作成
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = class_value, y = freq), 
           stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = freq_df[["class_value"]]) + # x軸
  labs(title = "Histogram", 
       x = "class value", y = "frequency")


# データを格納
data_df <- tibble::tibble(
  x = data_x
)

# 階級値を計算
class_vals <- cbind(class_vec[-1]+1, class_vec[-length(class_vec)]) |> 
  apply(1, median)

# ヒストグラムを作成
ggplot() + 
  geom_histogram(data = data_df, mapping = aes(x = x), 
                 breaks = class_vec, fill = "#00A968") + # ヒストグラム
  scale_x_continuous(breaks = class_vals) + # x軸
  labs(title = "Histogram", 
       x = "class value", y = "frequency")


