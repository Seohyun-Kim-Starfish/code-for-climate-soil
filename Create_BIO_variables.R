library(terra)
library(remotes)
remotes::install_github("MatMatt/MODIS", dependencies = TRUE)
library(MODIS)
library(prism)
library(raster)
library(sf)
library(dismo)
library(curl)

# 📁 작업 디렉토리 설정
base_dir <- "C:/ny_climate_project"
prism_dir <- file.path(base_dir, "prism_data")
output_dir <- file.path(base_dir, "output")

# PRISM 데이터 저장 경로 설정
prism_set_dl_dir(prism_dir)
getOption("prism.path")  # 확인

# 🔽 PRISM 데이터 다운로드 (tmean만 예시, ppt/tmin/tmax도 필요 시 추가)
get_prism_monthlys(type = "ppt", years = 2015:2024, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "tmin", years = 2015:2024, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "tmax", years = 2015:2024, mon = 1:12, keepZip = FALSE)

# 📄 다운로드된 파일 확인
list.files(prism_dir, recursive = TRUE)

# 🌐 뉴욕 경계 Shapefile 불러오기
ny_shp <- st_read(file.path(base_dir, "tl_2024_NY_landbarrier.shp"))
ny_shp_sp <- as(st_geometry(ny_shp), "Spatial")  # raster 호환 위해 변환

# 🗂 PRISM 파일 목록 불러오기
prism_files <- prism_archive_ls()
if (length(prism_files) == 0) stop("❌ PRISM 데이터가 없습니다. prism_download()로 데이터를 먼저 받으세요.")

# 📦 변수별 정리
ppt_files  <- prism_files[grepl("ppt", prism_files)]
tmin_files <- prism_files[grepl("tmin", prism_files)]
tmax_files <- prism_files[grepl("tmax", prism_files)]

# 🔧 PRISM raster stack 로드 함수
load_prism_stack <- function(files) {
  rasters <- lapply(files, function(f) {
    file_name <- paste0(f, ".bil")
    full_path <- file.path(prism_dir, f, file_name)
    if (!file.exists(full_path)) stop(paste("❌ 파일 없음:", full_path))
    raster(full_path)
  })
  stack(rasters)
}

# 📥 Stack 불러오기
ppt_stack  <- load_prism_stack(ppt_files)
tmin_stack <- load_prism_stack(tmin_files)
tmax_stack <- load_prism_stack(tmax_files)

# ✂️ 클리핑 (뉴욕 기준)
ppt_clip  <- mask(crop(ppt_stack, ny_shp_sp), ny_shp_sp)
tmin_clip <- mask(crop(tmin_stack, ny_shp_sp), ny_shp_sp)
tmax_clip <- mask(crop(tmax_stack, ny_shp_sp), ny_shp_sp)

# 🌡 평균 기온 계산
tmean_clip <- (tmin_clip + tmax_clip) / 2

# 📊 월별 평균 함수
average_monthly_stack <- function(stack_120) {
  monthly_means <- list()
  for (i in 1:12) {
    monthly_layers <- stack_120[[seq(i, nlayers(stack_120), 12)]]
    monthly_means[[i]] <- mean(monthly_layers)
  }
  return(stack(monthly_means))
}

# 📆 월별 평균 계산
ppt_avg12  <- average_monthly_stack(ppt_clip)
tmin_avg12 <- average_monthly_stack(tmin_clip)
tmax_avg12 <- average_monthly_stack(tmax_clip)

# 🌍 Bioclim 변수 계산
bioclim_vars <- biovars(prec = ppt_avg12, tmin = tmin_avg12, tmax = tmax_avg12)

# 📐 좌표계 및 해상도 설정
target_crs <- CRS(SRS_string = "EPSG:4326")
res_deg <- 30 / 111320  # 30m 해상도 ≈ 0.0002695도

# 📏 템플릿 raster 생성 (30m 해상도)
template_raster <- raster(extent(ny_shp_sp), res = res_deg, crs = target_crs)

# 📁 저장 디렉토리 생성
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

base::options(digits = 6)
base::options(scipen = 999)
# 💾 결과 저장 (GTiff, 메모리 절약용)
bio_names <- paste0("BIO", 1:19)

for (i in 1:19) {
  message("🔄 Processing ", bio_names[i])
  
  # 1. bioclim 변수 하나 가져오기
  bio_layer <- rast(bioclim_vars[[i]])  # raster -> SpatRaster 변환
  
  # 2. 좌표계 재투영 (terra는 faster)
  bio_proj <- project(bio_layer, target_crs, method = "bilinear")
  
  # 3. 템플릿에 맞춰 리샘플링
  bio_resampled <- resample(bio_proj, rast(template_raster), method = "bilinear")
  
  # 4. 파일명 설정 및 저장
  output_file <- file.path(output_dir, paste0(bio_names[i], "_bio_variable.tif"))
  
  # 5. 디스크 기반 쓰기 (메모리 사용 최소화)
  writeRaster(bio_resampled, filename = output_file, overwrite = TRUE, 
              filetype = "GTiff", NAflag = -9999)
}

#------------asc변환--------------
