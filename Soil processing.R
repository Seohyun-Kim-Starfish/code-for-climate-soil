install.packages("modeest")

library(sf)
library(terra)
library(dplyr)
library(modeest)  # mode 계산용

# 경로
gdb_path <- "C:/Users/김연찬/Documents/ssurgo_unzip/gSSURGO_NY.gdb"
output_dir <- "C:/Users/김연찬/Desktop/Maps30m"
dir.create(output_dir, showWarnings = FALSE)

# 읽을 변수
numeric_vars <- c("slopegraddcp", "ecec_r", "sumbases_r", "silttotal_r", "claytotal_r", "caco3_r", "flodfreqdcd", "flodfreqmax", "forpehrtdcp")
all_vars <- c(numeric_vars)
#----------------------------------------
# GDB 경로
gdb_path <- "C:/Users/김연찬/Documents/ssurgo_unzip/gSSURGO_NY.gdb"

# 공간정보 있는 폴리곤 (mukey 포함)
mu_poly <- st_read(gdb_path, layer = "MUPOLYGON", quiet = TRUE)

# 속성 테이블 (mukey 기준으로 조인할 데이터)
muaggatt <- st_read(gdb_path, layer = "muaggatt", quiet = TRUE)
component <- st_read(gdb_path, layer = "component", quiet = TRUE)
chorizon <- st_read(gdb_path, layer = "chorizon", quiet = TRUE)

# 필요한 열만 남기기
vars_needed <- c("mukey", "slopegraddcp", "flodfreqdcd", "flodfreqmax", 
                 "forpehrtdcp", "ecec_r", "sumbases_r", 
                 "silttotal_r", "claytotal_r", "caco3_r")

# 필요한 열만 선택
muaggatt_sel <- muaggatt %>%
  select(mukey, slopegraddcp, flodfreqdcd, flodfreqmax, forpehrtdcp)

component_sel <- component %>%
  select(mukey, cokey)

chorizon_sel <- chorizon %>%
  select(cokey, ecec_r, sumbases_r, silttotal_r, claytotal_r, caco3_r)

# 데이터 통합
mu_data <- muaggatt_sel %>%
  left_join(component_sel, by = "mukey") %>%
  left_join(chorizon_sel, by = "cokey")

safe_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# 각 mukey별 요약 통계 계산
# - numeric 변수는 평균값(mean), categorical 변수는 최빈값(mode)
summary_by_mukey <- mu_data %>%
  group_by(mukey) %>%
  summarise(
    slopegraddcp = mean(slopegraddcp, na.rm = TRUE),
    ecec_r = mean(ecec_r, na.rm = TRUE),
    sumbases_r = mean(sumbases_r, na.rm = TRUE),
    silttotal_r = mean(silttotal_r, na.rm = TRUE),
    claytotal_r = mean(claytotal_r, na.rm = TRUE),
    caco3_r = mean(caco3_r, na.rm = TRUE),
    flodfreqdcd = safe_mode(flodfreqdcd),
    flodfreqmax = safe_mode(flodfreqmax),
    forpehrtdcp = safe_mode(forpehrtdcp)
  )


# 공간 데이터와 결합
mu_poly_joined <- mu_poly %>%
  left_join(summary_by_mukey, by = c("MUKEY" = "mukey"))


library(sp)

for (var in numeric_vars) {
  message("Processing: ", var)
  
  # 해당 변수만 추출
  var_sf <- mu_poly_joined %>%
    select(all_of(var))
  
  # NA 값 제거 (terra::rasterize는 NA geometry를 무시)
  var_sf <- var_sf[!is.na(var_sf[[var]]), ]
  
  # sf 객체를 sp 객체로 변환
  var_sp <- as(var_sf, "Spatial")
  
  # 기준 해상도 설정 (예: 100m)
  r_template <- rast(ext(var_sp), resolution = 100, crs = st_crs(var_sf)$wkt)
  
  # 래스터화
  r <- terra::rasterize(var_sp, r_template, field = var, fun = mean)
  
  # 저장
  writeRaster(r, filename = file.path(output_dir, paste0(var, ".asc")), overwrite = TRUE)
}



#-------------------------------------------기
library(sf)
library(dplyr)
library(terra)

# 경로 설정
gdb_path <- "C:/Users/김연찬/Documents/ssurgo_unzip/gSSURGO_NY.gdb"
output_dir <- "C:/Users/김연찬/Desktop/Maps30m"
dir.create(output_dir, showWarnings = FALSE)

# 변수 설정
chorizon_numeric <- c("claytotal_r", "caco3_r")
component_numeric <- c("slope_r")
categorical_vars <- c("flodfreqdcd", "flodfreqmax", "forpehrtdcp")
numeric_vars <- c(chorizon_numeric, component_numeric)

# 최빈값 함수
safe_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 데이터 불러오기 및 통합
message("Loading and joining data...")
chorizon <- st_read(gdb_path, layer = "chorizon", quiet = TRUE)
chorizon_sel <- chorizon %>%
  select(cokey, all_of(chorizon_numeric)) %>%
  group_by(cokey) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")
rm(chorizon); gc()

component <- st_read(gdb_path, layer = "component", quiet = TRUE)
component_sel <- component %>%
  select(mukey, cokey, all_of(component_numeric))
rm(component); gc()

muaggatt <- st_read(gdb_path, layer = "muaggatt", quiet = TRUE)
muaggatt_sel <- muaggatt %>% select(mukey, all_of(categorical_vars))
rm(muaggatt); gc()

mu_data <- component_sel %>%
  left_join(chorizon_sel, by = "cokey") %>%
  left_join(muaggatt_sel, by = "mukey") %>%
  group_by(mukey) %>%
  summarise(
    across(all_of(numeric_vars), ~mean(.x, na.rm = TRUE)),
    across(all_of(categorical_vars), ~safe_mode(.x)),
    .groups = "drop"
  )
rm(chorizon_sel, component_sel, muaggatt_sel); gc()

mu_poly <- st_read(gdb_path, layer = "MUPOLYGON", quiet = TRUE)
mu_poly_joined <- mu_poly %>% left_join(mu_data, by = c("MUKEY" = "mukey"))
rm(mu_poly); gc()

# 벡터 변환 및 템플릿 생성 (정확히 30m 해상도)
mu_vect <- vect(mu_poly_joined)
mu_vect <- project(mu_vect, "EPSG:4326")
res_meters <- 30  # 30m 해상도
res_deg <- res_meters / 111320  # 위도 기준 1도 ≈ 111.32km
r_template <- rast(ext(mu_vect), resolution = res_deg, crs = "EPSG:4326")

# 숫자형 변수 래스터화
message("Rasterizing numeric variables...")
for (var in numeric_vars) {
  message(sprintf("\nProcessing numeric variable: %s", var))
  temp_vect <- mu_vect[!is.na(mu_vect[[var]]), ]
  if (nrow(temp_vect) == 0) next
  r <- rasterize(temp_vect, r_template, field = var, fun = mean)
  writeRaster(r, filename = file.path(output_dir, paste0(var, ".asc")), overwrite = TRUE)
  rm(temp_vect, r); gc()
}


#------------숫자형 데이터 최적화 버전-----------------

library(sf)
library(dplyr)
library(terra)

# 경로 설정
gdb_path <- "C:/Users/김연찬/Documents/ssurgo_unzip/gSSURGO_NY.gdb"
output_dir <- "C:/Users/김연찬/Documents/output"
dir.create(output_dir, showWarnings = FALSE)


# 변수 설정
chorizon_numeric <- c()
component_numeric <- c("hydricrating", "drainagecl")
categorical_vars <- c("flodfreqdcd", "flodfreqmax", "forpehrtdcp")
numeric_vars <- c(chorizon_numeric, component_numeric)

# 최빈값 함수
safe_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

message("Loading chorizon...")
chorizon <- st_read(gdb_path, layer = "chorizon", quiet = TRUE) %>%
  select(cokey, all_of(chorizon_numeric)) %>%
  group_by(cokey) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")
gc()

message("Loading component...")
component <- st_read(gdb_path, layer = "component", quiet = TRUE) %>%
  select(mukey, cokey, all_of(component_numeric))
gc()

message("Loading muaggatt...")
muaggatt <- st_read(gdb_path, layer = "muaggatt", quiet = TRUE) %>%
  select(mukey, all_of(categorical_vars))
gc()

message("Joining and summarizing attribute tables...")
mu_data <- component %>%
  left_join(chorizon, by = "cokey") %>%
  left_join(muaggatt, by = "mukey") %>%
  group_by(mukey) %>%
  summarise(
    across(all_of(numeric_vars), ~mean(.x, na.rm = TRUE)),
    across(all_of(categorical_vars), ~safe_mode(.x)),
    .groups = "drop"
  )
gc()

message("Reading MUPOLYGON and joining attributes...")
mu_poly <- st_read(gdb_path, layer = "MUPOLYGON", quiet = TRUE) %>%
  left_join(mu_data, by = c("MUKEY" = "mukey")) %>%
  st_transform("EPSG:4326")
gc()

message("Converting to terra vector...")
mu_vect <- vect(mu_poly)
rm(mu_poly); gc()

res_meters <- 100
res_deg <- res_meters / 111320
r_template <- rast(ext(mu_vect), resolution = res_deg, crs = "EPSG:4326")

message("Rasterizing numeric variables...")
for (var in numeric_vars) {
  message(sprintf("📌 Processing: %s", var))
  if (!(var %in% names(mu_vect))) next
  temp_vect <- mu_vect[!is.na(mu_vect[[var]]), ]
  if (nrow(temp_vect) == 0) next
  r <- rasterize(temp_vect, r_template, field = var, fun = "mean")
  writeRaster(r, filename = file.path(output_dir, paste0(var, ".asc")), overwrite = TRUE)
  rm(r, temp_vect); gc()
}

#---------------------카테고리형---------------------------

library(sf)
library(dplyr)
library(terra)

# 경로 설정
gdb_path <- "C:/Users/김연찬/Documents/ssurgo_unzip/gSSURGO_NY.gdb"
output_dir <- "C:/Users/김연찬/Desktop/Maps30m"
dir.create(output_dir, showWarnings = FALSE)

# 필요한 카테고리형 변수
categorical_vars <- c("flodfreqdcd", "flodfreqmax", "forpehrtdcp")

# 숫자 매핑 정의
custom_maps <- list(
  flodfreqdcd = c("None" = 0, "Rare" = 1, "Occasional" = 2, "Frequent" = 3, "Very frequent" = 5),
  flodfreqmax = c("None" = 0, "Rare" = 1, "Occasional" = 2, "Frequent" = 3, "Very frequent" = 5),
  forpehrtdcp = c("Not rated" = 0, "Slight" = 1, "Moderate" = 2, "Severe" = 3)
)

# 안전한 최빈값 함수
safe_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 데이터 불러오기
component <- st_read(gdb_path, layer = "component", quiet = TRUE) %>%
  select(mukey, cokey) %>%
  distinct()

muaggatt <- st_read(gdb_path, layer = "muaggatt", quiet = TRUE)
muaggatt_sel <- muaggatt %>% select(mukey, all_of(categorical_vars))

mu_data <- component %>%
  left_join(muaggatt_sel, by = "mukey") %>%
  group_by(mukey) %>%
  summarise(across(all_of(categorical_vars), ~safe_mode(.x)), .groups = "drop")

# polygon 정보 조인
mu_poly <- st_read(gdb_path, layer = "MUPOLYGON", quiet = TRUE)
mu_poly$MUKEY <- as.character(mu_poly$MUKEY)
mu_data$mukey <- as.character(mu_data$mukey)
mu_poly_joined <- mu_poly %>% left_join(mu_data, by = c("MUKEY" = "mukey"))

# 숫자 변환
mu_poly_joined$flodfreqdcd_num  <- custom_maps$flodfreqdcd[mu_poly_joined$flodfreqdcd]
mu_poly_joined$flodfreqmax_num <- custom_maps$flodfreqmax[mu_poly_joined$flodfreqmax]
mu_poly_joined$forpehrtdcp_num <- custom_maps$forpehrtdcp[mu_poly_joined$forpehrtdcp]

# terra로 변환
mu_vect <- vect(mu_poly_joined)
mu_vect <- project(mu_vect, "EPSG:4326")
fixed_res <- 30 / 111320
r_template <- rast(ext(mu_vect), resolution = fixed_res, crs = "EPSG:4326")

# rasterize 실행
for (var in c("flodfreqdcd_num", "flodfreqmax_num", "forpehrtdcp_num")) {
  message(sprintf("▶ Rasterizing: %s", var))
  temp_vect <- mu_vect[!is.na(mu_vect[[var]]), ]
  if (nrow(temp_vect) == 0) next
  r <- rasterize(temp_vect, r_template, field = var, fun = "mean", touches = TRUE)
  out_path <- file.path(output_dir, paste0(var, ".asc"))
  writeRaster(r, out_path, overwrite = TRUE, NAflag = -9999)
  rm(temp_vect, r); gc()
}