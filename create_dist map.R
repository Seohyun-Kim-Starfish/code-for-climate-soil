install.packages(c("sf", "terra"))

# 패키지 로드
library(sf)
library(terra)

# 파일 경로 설정
road_path <- "C:/Users/김연찬/Downloads/USA_Detailed_Streams/ny_Detailed_Streams.shp"

# 도로 shapefile 불러오기
roads <- st_read(road_path)

# 좌표계 확인 및 EPSG:4326으로 변환
roads <- st_transform(roads, crs = 4326)

# 전체 경계 (extent) 계산
road_extent <- st_bbox(roads)

# 해상도: 100m ≈ 0.000898 deg (대략적 변환)
res_deg <- 100 / 111320

# 래스터 템플릿 생성 (EPSG:4326, 해상도 100m)
template_rast <- rast(
  xmin = road_extent["xmin"], xmax = road_extent["xmax"],
  ymin = road_extent["ymin"], ymax = road_extent["ymax"],
  resolution = res_deg,
  crs = "EPSG:4326"
)

# 도로를 벡터에서 SpatVector로 변환
roads_vect <- vect(roads)

# 도로를 래스터로 변환 (도로=1, 비도로=NA)
road_rast <- rasterize(roads_vect, template_rast, field=1)

# EPSG:4326은 거리 계산 부정확 → EPSG:5070 (meter 기반)으로 변환
template_5070 <- project(template_rast, "EPSG:5070")
road_rast_5070 <- project(road_rast, "EPSG:5070")

# 도로로부터 거리 계산 (단위: meter)
dist_5070 <- distance(road_rast_5070)

# 다시 EPSG:4326으로 변환 (method="bilinear"로 보간)
dist_4326 <- project(dist_5070, "EPSG:4326", method = "bilinear")

# 결과 저장 (AAIGrid/ASCII: .asc)
output_path <- "C:/Users/김연찬/Documents/maxent_input/ny_stream_dist.asc"
writeRaster(dist_4326, output_path, overwrite = TRUE, filetype = "AAIGrid")

#-------------------------------------------------
library(terra)

# GeoTIFF 파일 경로
tif_path <- "C:/Users/김연찬/Documents/maxent_input/ny_stream_dist.tif"

# 래스터 불러오기
r <- rast(tif_path)

# 저장 경로 (.asc 형식)
asc_path <- "C:/Users/김연찬/Documents/maxent_input/ny_stream_dist.asc"

# ASC로 저장 (AAIGrid)
writeRaster(r, asc_path, filetype = "AAIGrid", overwrite = TRUE)
