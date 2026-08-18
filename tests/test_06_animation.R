# ==========================================================================
# 播放鍵 —— 一個「圖畫得出來，但按鈕是死的」的陷阱
#
# 溫度場動畫的播放鍵送給 plotly.js 的是
#     args = [ <要播的影格清單>, <播放選項> ]
# plotly.js 用 JSON 的 null 代表「全部影格」。但 R 的 NULL 經 htmlwidgets
# 序列化之後不是 null，而是 []，也就是「零個影格」——
# 於是播放鍵按下去什麼都不會發生：不報錯、console 全乾淨、圖也還在，
# 只是不動。用眼睛看圖是看不出來的，所以這條測試改看送出去的 JSON。
#
# 對應真實事故：報告產出後播放鍵無作用（args 第一項是 []）。
# ==========================================================================

skip_if_not_installed("plotly")

suppressPackageStartupMessages(library(plotly))

## testthat 會把工作目錄切到 tests/,但 R/05_viz.R 自己會 source("R/01_setup.R")
## 這類相對於 repo 根目錄的路徑 —— 所以這支測試整段都在根目錄底下跑,
## 跑完再切回去(單獨執行時本來就在根目錄,setwd(".") 不做事)。
gt_root   <- if (file.exists("R/05_viz.R")) "." else ".."
gt_old_wd <- setwd(gt_root)
withr::defer(setwd(gt_old_wd), testthat::teardown_env())

source("R/05_viz.R")

gt_anim_fig <- function() {
  ## 刻意用小模型：這條測的是按鈕的接線，不是物理
  m  <- gt_model(gt_params(Lx = 1600, Ly = 1000, dx = 40,
                           L_doublet = 400, t_end_yr = 4))
  fl <- gt_solve_flow(m)
  ht <- gt_solve_heat(m, fl, store_snaps = TRUE, progress = FALSE)
  gt_plot_temperature(m, ht, gt_streamlines(m, fl), every = 4L)
}

test_that("動畫圖真的有影格，而且播放鍵指到的是同一批影格", {
  p   <- gt_anim_fig()
  spec <- jsonlite::fromJSON(plotly::plotly_json(p, jsonedit = FALSE),
                             simplifyVector = FALSE)

  frame_names <- vapply(spec$frames, function(f) f$name, character(1))
  expect_gt(length(frame_names), 1L)

  play <- spec$layout$updatemenus[[1]]$buttons[[1]]
  expect_identical(play$method, "animate")

  ## 這行就是整條測試的重點：第一項不能是空的（NULL -> [] 的陷阱）
  play_frames <- unlist(play$args[[1]])
  expect_gt(length(play_frames), 0L)
  expect_identical(as.character(play_frames), as.character(frame_names))
})

test_that("滑桿的每一步都指到一個真的存在的影格", {
  p    <- gt_anim_fig()
  spec <- jsonlite::fromJSON(plotly::plotly_json(p, jsonedit = FALSE),
                             simplifyVector = FALSE)

  frame_names <- vapply(spec$frames, function(f) f$name, character(1))
  steps <- spec$layout$sliders[[1]]$steps
  expect_identical(length(steps), length(frame_names))

  step_targets <- vapply(steps, function(s) as.character(s$args[[1]][[1]]),
                         character(1))
  expect_true(all(step_targets %in% frame_names))
})
