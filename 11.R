library(tidyverse)
# 導入 CSV 檔案
file_path <- "data/資訊開放平台-11310.csv"

# 使用 read_csv 讀取檔案
data <- read_csv(file_path)

# 檢查資料
glimpse(data)

library(dplyr)

# 將 "遊客人次" 轉換為數值
data <- data %>%
  mutate(
    `遊客人次` = as.numeric(str_remove_all(`遊客人次`, ","))
  )

# 檢查結果
glimpse(data)



library(stringr)

# 將 "遊客人次" 排成有序因子
data <- data %>%
  mutate(
    # 如果 "遊客人次" 包含非數字字元，先移除並轉換為數值
    `遊客人次` = as.numeric(str_remove_all(`遊客人次`, ",")),
    
    # 排成有序因子
    `遊客人次_有序` = factor(
      `遊客人次`, 
      levels = sort(unique(`遊客人次`)), 
      ordered = TRUE
    )
  )

# 檢查結果
glimpse(data)

levels(data$`遊客人次_有序`)

library(dplyr)

# 篩選 "遊客人次" 大於 100,000 的行
data_filtered <- data %>%
  filter(`遊客人次` > 100000)

# 檢查結果
glimpse(data_filtered)
View(data_filtered)

unique_values_above_100k <- unique(data$`遊客人次`[data$`遊客人次` > 100000])
unique_values_above_100k

library(dplyr)

# 篩選出 "遊客人次" 小於 100,000 的行
data_filtered <- data %>%
  filter(`遊客人次` < 100000)

# 檢查篩選後的資料
glimpse(data_filtered)  # 檢查資料結構
View(data_filtered)     # 在視覺化介面中檢查資料


# 檢查 "遊客人次" 欄位的結構和資料
summary(data$`遊客人次`)  # 檢查數值摘要
unique(data$`遊客人次`)   # 檢查唯一值
glimpse(data$`遊客人次`)  # 檢查欄位結構


# 將 "遊客人次" 欄位的數值加總
total_visitors <- sum(data$`遊客人次`, na.rm = TRUE)

# 顯示結果
total_visitors

# 計算 "遊客人次" 欄位的平均數
average_visitors <- mean(data$`遊客人次`, na.rm = TRUE)

# 顯示結果
average_visitors

glimpse(data)


library(dplyr)

# 篩選出 "門票收入" 大於 0 的行
data_filtered <- data %>%
  filter(`門票收入` > 0)

# 檢查篩選後的資料
glimpse(data_filtered)  # 檢查資料結構
View(data_filtered)     # 在視覺化介面中檢查資料
unique(data$`門票收入`)   # 檢查唯一值


library(dplyr)

# 將 "上年同月份遊客人次" 轉換為有序因子，並且由小到大排序
data <- data %>%
  mutate(
    `上年同月份遊客人次` = factor(
      `上年同月份遊客人次`,
      levels = sort(unique(`上年同月份遊客人次`)), # 由小到大排序唯一值
      ordered = TRUE
    )
  )

# 檢查結果
glimpse(data)
unique(data$`上年同月份遊客人次`)   # 檢查唯一值

view(data)




# 篩選包含 "區" 的資料
filtered_data <- data |>
  filter(str_detect(as.character(`觀光遊憩區別`), "區"))

# 查看結果
print(filtered_data)


# 篩選字數超過五個字的資料
filtered_data <- data |>
  filter(str_length(as.character(`觀光遊憩區別`)) > 5)

# 查看結果
print(filtered_data)


# 篩選字數小於五個字的資料
filtered_data <- data |>
  filter(str_length(as.character(`觀光遊憩區別`)) < 5)

# 查看結果
print(filtered_data)

