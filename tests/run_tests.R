#!/usr/bin/env Rscript
# ==========================================================================
# 執行全部測試:   Rscript tests/run_tests.R
#
# 這個檔案要在 repo 根目錄執行(不是在 tests/ 裡面)。
# 全綠大約需要 30 秒。任何一條紅的,exit code 就是 1 —— CI 抓得到。
# ==========================================================================

suppressPackageStartupMessages(library(testthat))

source("R/01_setup.R")
source("R/02_flow.R")
source("R/03_heat.R")

## testthat 會把工作目錄切到 tests/,參數檔的相對路徑會失效 —— 先轉絕對路徑
GT_PARAMS_FILE <- normalizePath(GT_PARAMS_FILE, mustWork = TRUE)

cat("\n================ geothermal-doublet 測試 ================\n\n")
t0 <- proc.time()[["elapsed"]]

files <- sort(list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE))
if (length(files) == 0L) stop("tests/ 裡沒有 test_*.R", call. = FALSE)

n_fail <- 0L
for (f in files) {
  cat("---", basename(f), "---\n")
  res <- as.data.frame(test_file(f, reporter = "summary",
                                 env = new.env(parent = globalenv())))
  n_fail <- n_fail + sum(res$failed) + sum(res$error)
}

cat(sprintf("\n共 %.0f 秒。", proc.time()[["elapsed"]] - t0))
if (n_fail > 0L) {
  cat(sprintf(" %d 條紅的。\n\n", n_fail))
  quit(status = 1L)
}
cat(" 全綠。\n\n")
