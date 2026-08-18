#!/usr/bin/env Rscript
# ==========================================================================
# 一次跑完整條流程:   Rscript run_all.R
#
# 照編號依序執行 R/ 底下的腳本。全部跑完約 2 分鐘,產出在 outputs/。
# 每一支也都可以單獨執行(例如只重畫圖:Rscript R/05_viz.R)。
# ==========================================================================

t0 <- proc.time()[["elapsed"]]
step <- function(n, title) cat(sprintf(
  "\n=== [%s] %s %s\n", n, title, strrep("=", max(0, 56 - nchar(title)))))

step("1/4", "02_flow.R  穩態水頭 + Darcy 通量")
source("R/02_flow.R");  gt_run_flow()

step("2/4", "03_heat.R  溫度移流–延散")
source("R/03_heat.R");  gt_run_heat()

step("3/4", "04_sweep.R  設計掃描(井距 x 抽注率)")
source("R/04_sweep.R"); gt_run_sweep()

step("4/4", "05_viz.R  互動式報告")
source("R/05_viz.R");   gt_run_viz()

cat(sprintf("\n全部完成,共 %.0f 秒。\n", proc.time()[["elapsed"]] - t0))
cat("接著跑一次測試確認結果可信:  Rscript tests/run_tests.R\n\n")
