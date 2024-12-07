library(tidyverse)
# 導入 data 資料夾中的 "3-16.csv"
data_3_16 <- readr::read_csv("data/3-16.csv")
glimpse(data_3_16)
# 篩選 "基金名稱" 欄位中包含「紐約」的資料
ny_funds <- data_3_16 |>
  dplyr::filter(stringr::str_detect(`基金名稱`, "紐約"))
glimpse(ny_funds)
unique(ny_funds)
view(ny_funds)
# 篩選 "基金名稱" 欄位包含「紐約」且 "基金種類" 為「股票型」的資料
ny_stock_funds <- ny_funds |>
  dplyr::filter(`基金種類` == "股票型")
glimpse(ny_stock_funds)
unique(ny_stock_funds)
# 篩選 "基金名稱" 欄位包含「紐約」且 "基金種類" 為「固定收益型」的資料
ny_bond_funds <- ny_funds |>
  dplyr::filter(`基金種類` == "固定收益型")
glimpse(ny_bond_funds)
# 按照 "最新淨值" 欄位從大到小排列
ny_funds_sorted <- ny_funds |>
  dplyr::arrange(dplyr::desc(`最新淨值`))
glimpse(ny_funds_sorted)
# 篩選 "最新淨值" 欄位大於 2 的資料
ny_funds_above_2 <- ny_funds |>
  dplyr::filter(`最新淨值` > 2)
glimpse(ny_funds_above_2)
# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
library(ggplot2)

ggplot(ny_funds, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 篩選 "基金名稱" 欄位中包含「富蘭克林」的資料
franklin_funds <- data_3_16 |>
  dplyr::filter(stringr::str_detect(`基金名稱`, "富蘭克林"))
glimpse(franklin_funds)
view(franklin_funds)
# 篩選 "基金名稱" 欄位包含「富蘭克林」且 "基金種類" 為「股票型」的資料
franklin_stock_funds <- franklin_funds |>
  dplyr::filter(`基金種類` == "股票型")
glimpse(franklin_stock_funds)
# 篩選 "基金名稱" 欄位包含「富蘭克林」且 "基金種類" 為「固定收益型」的資料
franklin_bond_funds <- franklin_funds |>
  dplyr::filter(`基金種類` == "固定收益型")
glimpse(franklin_bond_funds)
# 按照 "最新淨值" 欄位從大到小排列
franklin_bond_funds_sorted <- franklin_bond_funds |>
  dplyr::arrange(dplyr::desc(`最新淨值`))
glimpse(franklin_bond_funds_sorted)
# 篩選 "最新淨值" 欄位大於 50 的資料
franklin_funds_above_50 <- franklin_funds |>
  dplyr::filter(`最新淨值` > 50)
glimpse(franklin_funds_above_50)
# 篩選 "最新淨值" 欄位小於 50 的資料
franklin_funds_below_50 <- franklin_funds |>
  dplyr::filter(`最新淨值` < 50)
glimpse(franklin_funds_below_50)
# 載入 ggplot2 套件
library(ggplot2)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 載入 ggplot2 套件
library(ggplot2)

# 篩選 "最新淨值" 大於 50 的資料
franklin_funds_above_50 <- franklin_funds |>
  dplyr::filter(`最新淨值` > 50)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds_above_50, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖（最新淨值大於50）",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 載入 ggplot2 套件
library(ggplot2)

# 篩選 "最新淨值" 小於 50 的資料
franklin_funds_below_50 <- franklin_funds |>
  dplyr::filter(`最新淨值` < 50)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds_below_50, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖（最新淨值小於50）",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 載入 ggplot2 套件
library(ggplot2)

# 篩選 "最新淨值" 小於 5 的資料
franklin_funds_below_5 <- franklin_funds |>
  dplyr::filter(`最新淨值` < 5)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds_below_5, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖（最新淨值小於5）",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 載入 ggplot2 套件
library(ggplot2)

# 篩選 "最新淨值" 範圍在 6 到 10 之間的資料
franklin_funds_6_to_10 <- franklin_funds |>
  dplyr::filter(`最新淨值` >= 6 & `最新淨值` <= 10)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds_6_to_10, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖（最新淨值範圍在6到10之間）",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 載入 ggplot2 套件
library(ggplot2)

# 篩選 "最新淨值" 範圍在 11 到 20 之間的資料
franklin_funds_11_to_20 <- franklin_funds |>
  dplyr::filter(`最新淨值` >= 11 & `最新淨值` <= 20)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds_11_to_20, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖（最新淨值範圍在11到20之間）",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 載入 ggplot2 套件
library(ggplot2)

# 篩選 "最新淨值" 範圍在 21 到 30 之間的資料
franklin_funds_21_to_30 <- franklin_funds |>
  dplyr::filter(`最新淨值` >= 21 & `最新淨值` <= 30)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds_21_to_30, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖（最新淨值範圍在21到30之間）",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 載入 ggplot2 套件
library(ggplot2)

# 篩選 "最新淨值" 範圍在 31 到 40 之間的資料
franklin_funds_31_to_40 <- franklin_funds |>
  dplyr::filter(`最新淨值` >= 31 & `最新淨值` <= 40)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds_31_to_40, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖（最新淨值範圍在31到40之間）",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 載入 ggplot2 套件
library(ggplot2)

# 篩選 "最新淨值" 範圍在 41 到 50 之間的資料
franklin_funds_41_to_50 <- franklin_funds |>
  dplyr::filter(`最新淨值` >= 41 & `最新淨值` <= 50)

# 繪製 "基金代碼" 和 "最新淨值" 的柱狀圖
ggplot(franklin_funds_41_to_50, aes(x = `基金代碼`, y = `最新淨值`)) +
  geom_col() +
  labs(title = "基金代碼與最新淨值的柱狀圖（最新淨值範圍在41到50之間）",
       x = "基金代碼",
       y = "最新淨值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 找出 "最新淨值" 欄位最大的資料
max_value_fund <- franklin_funds |>
  dplyr::filter(`最新淨值` == max(`最新淨值`, na.rm = TRUE))

# 顯示最大值的基金資料
max_value_fund

# 顯示最大 "最新淨值"
max_value <- max(max_value_fund$`最新淨值`, na.rm = TRUE)
max_value


# 找出 "最新淨值" 欄位最小的資料
min_value_fund <- franklin_funds |>
  dplyr::filter(`最新淨值` == min(`最新淨值`, na.rm = TRUE))

# 顯示最小值的基金資料
min_value_fund

# 顯示最小 "最新淨值"
min_value <- min(min_value_fund$`最新淨值`, na.rm = TRUE)
min_value














