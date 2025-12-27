library(sf)
library(dplyr)
library(readr)

# 📁 데이터 불러오기
data <- read_csv("C:/2025independentstudy/input/filtered_species_data_modified.csv")

# 🧼 필요한 열 선택
data_clean <- data %>%
  select(species, longitude, latitude) %>%
  na.omit()

# 🌍 sf 객체로 변환 (WGS84 → EPSG:4326)
data_sf <- st_as_sf(data_clean, coords = c("longitude", "latitude"), crs = 4326)

# 📏 거리 계산을 위해 투영 (단위: meter) – UTM으로 변환
# 예: New York 인근이면 zone 18N = EPSG:32618 (지역에 따라 조정)
data_proj <- st_transform(data_sf, crs = 32618)

# 🧹 thinning 함수 정의
thin_sf_points <- function(sf_data, min_dist = 1000) {
  kept <- list()
  coords <- st_coordinates(sf_data)
  remaining <- 1:nrow(sf_data)
  
  while (length(remaining) > 0) {
    idx <- remaining[1]
    kept <- append(kept, idx)
    
    # 거리 계산
    dists <- sqrt((coords[remaining, 1] - coords[idx, 1])^2 +
                    (coords[remaining, 2] - coords[idx, 2])^2)
    
    # min_dist 이상만 남김
    remaining <- remaining[dists > min_dist]
  }
  
  sf_data[unlist(kept), ]
}

# ⚙️ 실행
set.seed(123)
thinned_sf <- thin_sf_points(data_proj, min_dist = 5000)  # 5km

# 💾 저장 (경도/위도 재추출 후 CSV로)
thinned_latlon <- st_transform(thinned_sf, 4326) %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

write_csv(thinned_latlon, "C:/2025independentstudy/output/thinned_data_5km.csv")


#-------------------------------------------
library(readr)
library(dplyr)

# 원본 데이터 불러오기
data <- read_csv("C:/2025independentstudy/output/thinned_data_1km.csv")

# 경도 기준 정렬
thin_sorted <- data %>% arrange(longitude)

# 총 행 수와 절단점 계산
n <- nrow(thin_sorted)
cut1 <- floor(n / 3)
cut2 <- floor(2 * n / 3)

# 세 구간 정의
range1 <- 1:cut1
range2 <- (cut1 + 1):cut2
range3 <- (cut2 + 1):n

# 각 파트 생성 (각각 하나씩 제외)
part1 <- thin_sorted[-range1, ]  # 첫 번째 구간 제외
part2 <- thin_sorted[-range2, ]  # 두 번째 구간 제외
part3 <- thin_sorted[-range3, ]  # 세 번째 구간 제외

# CSV 저장
write_csv(part1, "C:/2025independentstudy/output/thinned_1kmpart1.csv")
write_csv(part2, "C:/2025independentstudy/output/thinned_1kmpart2.csv")
write_csv(part3, "C:/2025independentstudy/output/thinned_1kmpart3.csv")

#-------------------------------------------------------------
library(sf)
library(readr)
library(dplyr)
library(stringr)

# 📁 변환할 파일 목록
files <- c("thinned_data_1km.csv", "thinned_1kmpart1.csv", "thinned_1kmpart2.csv", "thinned_1kmpart3.csv")
dir_path <- "C:/2025independentstudy/output/"

# ⚙️ 파일별 EPSG:4326 변환 및 저장
for (file in files) {
  # 전체 경로 생성 및 데이터 불러오기
  full_path <- file.path(dir_path, file)
  data <- read_csv(full_path)
  
  # sf 객체로 변환 (좌표계: EPSG:4326)
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
  
  # 정확한 경도/위도 추출
  data_latlon <- data_sf %>%
    mutate(
      longitude = st_coordinates(.)[, 1],
      latitude = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry()
  
  # 새로운 파일명 생성 (예: thinned_1kmpart1_EPSG4326.csv)
  file_base <- str_remove(file, "\\.csv$")
  new_filename <- paste0(file_base, "_EPSG4326.csv")
  
  # 저장
  write_csv(data_latlon, file.path(dir_path, new_filename))
}

