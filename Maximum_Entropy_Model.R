# --- Preparation

setwd("/Users/zz/Downloads")
library(tidyverse)
library(readxl)

红腹角雉 = read_csv("全-红腹角雉.csv")

# setwd("/Users/zz/Downloads/大相岭-生物活动")
# 红腹角雉 = read_xlsx("人.xlsx")


species_list <- list(红腹角雉)

# Define the EPSG codes
wgs84_epsg <- 4326        # for WGS 84 (Geographic 
utm_zone_48n_epsg <- 32648  #  for UTM Zone 48N (Projected

transformed_species_list <- list()

library(sf)
# transform coordinates
for (i in 1:length(species_list)) {
  species_data <- species_list[[i]]
  
  species_sf <- st_as_sf(species_data, coords = c("经度（度）", "纬度（度）"), crs = wgs84_epsg)
  
  species_sf_utm <- st_transform(species_sf, crs = utm_zone_48n_epsg)
  
  utm_coords <- st_coordinates(species_sf_utm)
  species_data_utm <- cbind(species_data, utm_coords)
  
  transformed_species_list[[i]] <- species_data_utm
}

combined_data_utm = species_data_utm


# ONLY independent events (time interval method)
time_threshold <- 30 * 60  # 30 minutes in seconds

combined_data_utm <- combined_data_utm %>%
  arrange(物种, 拍摄时间)

# independent events
combined_data_utm <- combined_data_utm %>%
  group_by(物种) %>%
  mutate(
    time_diff = c(Inf, diff(as.POSIXct(拍摄时间))),
    independent_event = ifelse(time_diff > time_threshold, 1, 0)
  ) %>%
  ungroup()

红腹角雉 <- combined_data_utm %>%
  filter(independent_event == 1)


红腹角雉 <- 红腹角雉 %>%
  dplyr::select(X, Y) %>% 
  distinct()

红腹角雉 <- 红腹角雉 %>% 
  mutate(Species = "红腹角雉") %>%
  dplyr::select(Species, everything())

# write_csv(红腹角雉, "人.csv")

# write_csv(红腹角雉, "红腹角雉.csv")


#物种数据Species data prep
#--->
#环境数据Env data prep


library(raster)

setwd("/Users/zz/Downloads/大相岭-环境变量")

# 获取当前工作目录下所有的tif文件
raster_files <- list.files(pattern = "\\.tif$", full.names = TRUE)

# read all rasters
for (raster_file in raster_files) {
  raster_data <- raster(raster_file)
  
  # check
  if (!is.na(summary(raster_data)[1])) {
    # 构建输出ASCII文件的路径和文件名
    output_folder <- "/Users/zz/Downloads/daxiangling_environment"
    output_file <- file.path(output_folder, gsub("\\.tif$", ".asc", basename(raster_file)))
    
    # 将栅格数据转换为ASCII格式并导出
    writeRaster(raster_data, file = output_file, format = "ascii")
    print(paste("转换并导出成功:", output_file))  # success
  } else {
    warning(paste("栅格数据读取失败，SKIP跳过文件:", raster_file))
  }
}

#--- MaxEnt plotting for single species

### for "bird"
library(raster)
library(ggplot2)
library(rasterVis)

raster_path <- raster_path <- "/Users/zz/Downloads/111红腹角雉output-bootstrap/红腹角雉_avg.asc"

raster_data <- raster(raster_path)
# CRS to UTM WGS 84 48N
crs(raster_data) <- CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +north")
raster_data_geo <- projectRaster(raster_data, crs = CRS("+proj=longlat +datum=WGS84"))

# def for reclassification matrix
rcl <- matrix(c(0, 0.1484, 1,    # 第一类：低于阈值1的赋值为1
                0.1484, 0.4674, 2, # 第二类：在阈值1和阈值2之间的赋值为2
                0.4674, 1, 3),   # 第三类：高于阈值2的赋值为3
              nrow = 3, byrow = TRUE)

reclassified_data <- reclassify(raster_data_geo, rcl)

# adapt for ggplot2
reclassified_df <- as.data.frame(reclassified_data, xy = TRUE, na.rm = TRUE)
names(reclassified_df)[3] <- "value"

library(dplyr)

# 使用count函数统计value列中每个值的出现次数
value_counts <- reclassified_df %>% 
  count(value) %>%  
  mutate(prop = n * (30^2) / 1000000)  

print(value_counts)


# final plotting
ggplot(reclassified_df) +
  geom_raster(aes(x = x, y = y, fill = factor(value))) +
  scale_fill_manual(values = c("#DFEDD6", "#93CE5A", "#376819"),
                    labels = c("不适宜栖息地", "次适宜栖息地", "最适宜栖息地")) +
  theme_minimal() +
  labs(fill = NULL) +
  theme(
    panel.grid = element_blank(),  # 移除网格线
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 添加黑色边框
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5), 
    axis.ticks = element_line(color = "black"),  # 添加坐标轴刻度
    axis.ticks.length = unit(-0.2, "cm"),  # 调整刻度长度
    legend.position = "right",
    legend.key = element_rect(color = "black")  # 为图例添加边框
  ) +
  scale_x_continuous(name = NULL, labels = function(x) paste0(format(x, nsmall = 2), "°E")) +
  scale_y_continuous(name = NULL, labels = function(y) paste0(format(y, nsmall = 2), "°N")) +
  coord_fixed()  # 保持经纬度比例固定

### for human

raster_path <- raster_path <- "/Users/zz/Downloads/222人output-bootstrap/人_avg.asc"

raster_data <- raster(raster_path)
# CRS to UTM WGS 84 48N
crs(raster_data) <- CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +north")
raster_data_geo <- projectRaster(raster_data, crs = CRS("+proj=longlat +datum=WGS84"))

# def for reclassification matrix
rcl <- matrix(c(0, 0.1019, 1,    # 第一类：低于阈值1的赋值为1
                0.1019, 0.4131, 2, # 第二类：在阈值1和阈值2之间的赋值为2
                0.4131, 1, 3),   # 第三类：高于阈值2的赋值为3
              nrow = 3, byrow = TRUE)

reclassified_data <- reclassify(raster_data_geo, rcl)

# adapt for ggplot2
reclassified_df <- as.data.frame(reclassified_data, xy = TRUE, na.rm = TRUE)
names(reclassified_df)[3] <- "value"

library(dplyr)

# count A
value_counts <- reclassified_df %>% 
  count(value) %>%
  mutate(prop = n * (30^2) / 1000000)

print(value_counts)


# final final plotting
ggplot(reclassified_df) +
  geom_raster(aes(x = x, y = y, fill = factor(value))) +
  scale_fill_manual(values = c("#DFEDD6", "#93CE5A", "#376819"),
                    labels = c("低频出现", "次高频出现", "高频出现")) +
  theme_minimal() +
  labs(fill = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    legend.position = "right",
    legend.key = element_rect(color = "black")
  ) +
  scale_x_continuous(name = NULL, labels = function(x) paste0(format(x, nsmall = 2), "°E")) +
  scale_y_continuous(name = NULL, labels = function(y) paste0(format(y, nsmall = 2), "°N")) +
  coord_fixed()  # 保持经纬度比例固定

###for dual species
library(raster)
library(ggplot2)
library(dplyr)
library(parallel)


raster_path_1 <- "/Users/zz/Downloads/111红腹角雉output-bootstrap/红腹角雉_avg.asc"
raster_path_2 <- "/Users/zz/Downloads/222人output-bootstrap/人_avg.asc"


num_cores <- detectCores() - 1  # leave system resources free
cl <- makeCluster(num_cores)

clusterEvalQ(cl, library(raster))

# Define the reclassification matrices
rcl_1 <- matrix(c(0, 0.4674, 2,  # 不适宜和次适宜区域 -> 2
                  0.4674, 1, 4),  # 最适宜区域 -> 4
                nrow = 2, byrow = TRUE)

rcl_2 <- matrix(c(0, 0.4131, 0,  # 不适宜和次适宜区域 -> 0
                  0.4131, 1, 1),  # 最适宜区域 -> 1
                nrow = 2, byrow = TRUE)

# Export  variables to the cluster
clusterExport(cl, list("rcl_1", "rcl_2", "raster_path_1", "raster_path_2"))

# process species 1
raster_data_1 <- raster(raster_path_1)
crs(raster_data_1) <- CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +north")
raster_data_geo_1 <- projectRaster(raster_data_1, crs = CRS("+proj=longlat +datum=WGS84"))

# process species 2
raster_data_2 <- raster(raster_path_2)
crs(raster_data_2) <- CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +north")
raster_data_geo_2 <- projectRaster(raster_data_2, crs = CRS("+proj=longlat +datum=WGS84"))


reclassify_raster_chunk <- function(raster_chunk, reclass_matrix) {
  reclassify(raster_chunk, reclass_matrix)
}


reclassified_data_1 <- clusterApply(cl, list(raster_data_geo_1), reclassify_raster_chunk, rcl_1)
reclassified_data_2 <- clusterApply(cl, list(raster_data_geo_2), reclassify_raster_chunk, rcl_2)

combined_raster <- overlay(reclassified_data_1[[1]], reclassified_data_2[[1]], fun = function(x, y) {
  return(x + y)
})

combined_raster[is.na(combined_raster)] <- 0  # marginal dots

# Convert combined raster to a df
combined_df <- as.data.frame(combined_raster, xy = TRUE, na.rm = TRUE)
names(combined_df)[3] <- "value"

# Create a custom color mapping
value_colors <- c("2" = "#DFEDD6",   # 人类不适宜，红腹角雉不适宜
                  "3" = "#93CE5A",   # 人类适宜，红腹角雉不适宜
                  "4" = "#376819",   # 人类不适宜，红腹角雉适宜
                  "5" = "#FF0000")   # 人类适宜，红腹角雉适宜

# Plot the combined raster
ggplot(combined_df) +
  geom_tile(aes(x = x, y = y, fill = factor(value))) +
  scale_fill_manual(values = value_colors,
                    labels = c("2" = "Low/Medium Human, Not Suitable Lophura",
                               "3" = "High Human, Not Suitable Lophura",
                               "4" = "Low/Medium Human, Suitable Lophura",
                               "5" = "High Human, Suitable Lophura")) +
  theme_minimal() +
  labs(fill = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 8),  # Reduce font size
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 8),  # Reduce font size for y-axis
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    legend.position = "right",
    legend.key = element_rect(color = "black")
  ) +
  scale_x_continuous(name = NULL, labels = function(x) paste0(format(x, nsmall = 2), "°E")) +
  scale_y_continuous(name = NULL, labels = function(y) paste0(format(y, nsmall = 2), "°N")) +
  coord_fixed() +
  ggtitle("Habitat Overlapping Analysis")


# ----------------------计算4种情况的覆盖面积--------------------
# Calculate the area for each category in square kilometers (km^2)
area_vals <- freq(combined_raster, useNA = "no")
area_vals_df <- as.data.frame(area_vals)
colnames(area_vals_df) <- c("Value", "Pixel_Count")
pixel_area_km2 <- res(combined_raster)[1] * res(combined_raster)[2] / 1e6  # Convert m^2 to km^2
area_vals_df$Area_km2 <- area_vals_df$Pixel_Count * pixel_area_km2
print(area_vals_df)

stopCluster(cl)