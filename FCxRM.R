library(ENMeval)
library(terra)
library(parallel)
library(doParallel)

# 1. 경로 및 데이터 로드
base_dir <- "D:/미국연구2월리트라이"
occ_path <- file.path(base_dir, "MaxEnt_input/thinned_data_5km.csv")
env_dir <- file.path(base_dir, "MaxEnt_input/pearson")
bias_path <- file.path(base_dir, "MaxEnt_input/bias_file_full_res.asc")

# 환경 변수 및 출현 지점 로드
env_stack <- rast(list.files(env_dir, pattern = "\\.asc$", full.names = TRUE))
occ_coords <- read.csv(occ_path)[, c("longitude", "latitude")]
bias_raster <- rast(bias_path)

# 2. Bias File 가중 배경 지점 추출
set.seed(123)
bg_coords <- spatSample(bias_raster, size = 10000, method = "weights", na.rm = TRUE, xy = TRUE)
bg_coords <- bg_coords[, c("x", "y")]
colnames(bg_coords) <- c("longitude", "latitude")

occ_envs <- terra::extract(env_stack, occ_coords, ID = FALSE)
bg_envs <- terra::extract(env_stack, bg_coords, ID = FALSE)

# 좌표와 환경 변수 결합 (ENMevaluate에 데이터프레임으로 전달)
occ_df <- cbind(occ_coords, occ_envs)
bg_df <- cbind(bg_coords, bg_envs)

# 4. 파라미터 리스트 생성
fc_list <- unlist(lapply(1:5, function(i) apply(combn(c("L","Q","H","P","T"), i), 2, paste0, collapse = "")))
rm_list <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)

# 5. RM별 루프 실행 (진행률 확인용)
results_list <- list()
total_steps <- length(rm_list)

# 병렬 처리 설정 (코어 절반 사용 권장)
cores <- floor(parallel::detectCores() / 2)
cl <- makeCluster(cores)
registerDoParallel(cl)

for (i in 1:total_steps) {
  curr_rm <- rm_list[i]
  
  # 실시간 진행률 출력
  cat(sprintf("[%d / %d] 현재 RM = %s 작업 중... (%s)\n", 
              i, total_steps, curr_rm, Sys.time()))
  
  # 현재 RM에 대해 31개 FC 조합을 병렬로 실행
  # envs = NULL로 설정하여 래스터 복제에 의한 메모리 에러 방지
  res <- ENMevaluate(
    occs = occ_df, 
    bg = bg_df,
    envs = NULL,  # 이미 데이터프레임에 값이 있으므로 NULL 설정
    partitions = "randomkfold", 
    partition.settings = list(kfolds = 5),
    tune.args = list(fc = fc_list, rm = curr_rm),
    algorithm = "maxnet",
    parallel = TRUE,
    numCores = cores
  )
  
  # 결과 테이블만 추출해서 저장
  results_list[[i]] <- eval.results(res)
  
  # 루프 한 주기가 끝날 때마다 메모리 강제 정리
  rm(res)
  gc()
}

stopCluster(cl)

# 6. 모든 결과 합치기 및 저장
final_eval <- do.call(rbind, results_list)

# 논문용 핵심 지표 선택
final_csv <- final_eval[, c("tune.args", "AICc", "delta.AICc", 
                            "auc.train", "auc.val.avg", "auc.diff.avg", 
                            "or.10p.avg")]

# 열 이름 변경
colnames(final_csv) <- c("Settings", "AICc", "delta_AICc", 
                         "AUC_train", "AUC_test", "AUC_diff", "Omission_10pc")

# delta_AICc 순으로 정렬
final_csv <- final_csv[order(final_csv$delta_AICc), ]
output_file <- file.path(base_dir, "model_Optimization.csv")
write.csv(final_csv, output_file, row.names = FALSE)
