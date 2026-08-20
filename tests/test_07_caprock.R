# ==========================================================================
# 蓋層/底岩傳導(issue #4)
#
# 之前上下邊界完全絕熱;現在含水層每一格可以跟一疊蓋層/底岩的一維傳導層
# 交換熱量,開關是 lambda_cap。這個檔案驗證:
#   1. lambda_cap = 0(預設)完全等於 #4 之前的行為 —— 回歸測試
#   2. 開啟時,能量守恆仍然到機器精度(蓋層/底岩的熱容也算進 E_tot)
#   3. 開啟時,生產井末溫上升、熱突破延後 —— 這正是這個 issue 要的效果
#      (冷水鋒面附近的含水層比週圍岩層冷,熱會從蓋層/底岩流回含水層,
#       替鋒面「補溫」,不是原本 EOR 熱損失情境裡的單向散熱)
#   4. 敏感度:lambda_cap 越大,補溫效果越明顯,方向要單調
# ==========================================================================

gt_cap_setup <- function(...) {
  m <- gt_model(gt_params(Lx = 1600, Ly = 1000, dx = 25,
                          L_doublet = 400, t_end_yr = 15, ...))
  list(m = m, fl = gt_solve_flow(m))
}

test_that("lambda_cap = 0 逐位元回到 issue #4 之前的絕熱行為", {
  s <- gt_cap_setup()
  ht_default <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  ht_explicit0 <- gt_solve_heat(s$m, s$fl, lambda_cap = 0,
                                store_snaps = FALSE, progress = FALSE)
  expect_false(ht_default$has_cap)
  expect_null(ht_default$Zcap)
  expect_identical(ht_default$hist$Temp_pro, ht_explicit0$hist$Temp_pro)
  expect_identical(ht_default$hist$E_tot, ht_explicit0$hist$E_tot)
})

test_that("開啟蓋層/底岩後,能量守恆仍然到機器精度", {
  s <- gt_cap_setup(lambda_cap = 216000, n_z_cap = 8)
  ht <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  expect_true(ht$has_cap)
  expect_lt(ht$energy_err_rel, 1e-9)
})

test_that("dt_max 會把蓋層/底岩第一層的穩定上限算進去", {
  s <- gt_cap_setup(lambda_cap = 216000, n_z_cap = 8)
  ht_off <- gt_solve_heat(s$m, s$fl, lambda_cap = 0,
                          store_snaps = FALSE, progress = FALSE)
  ht_on  <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  expect_lt(ht_on$dt_max, ht_off$dt_max)
})

test_that("超過蓋層穩定上限的 dt 一樣會炸掉", {
  s <- gt_cap_setup(lambda_cap = 216000, n_z_cap = 8)
  ht <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  ## 跟 test_02_cfl.R 同一個檢驗方式:場內溫度離開 [70, 130] 好幾個數量級
  bad <- gt_solve_heat(s$m, s$fl, dt = 3 * ht$dt_max,
                       store_snaps = FALSE, progress = FALSE)
  expect_gt(max(abs(bad$Temp)), 1e6)
})

test_that("開啟蓋層/底岩會讓生產井末溫升高、熱突破延後", {
  s  <- gt_cap_setup(lambda_cap = 216000, n_z_cap = 10)
  off <- gt_solve_heat(s$m, s$fl, lambda_cap = 0,
                       store_snaps = FALSE, progress = FALSE)
  on  <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)

  T_end_off <- utils::tail(off$hist$Temp_pro, 1)
  T_end_on  <- utils::tail(on$hist$Temp_pro, 1)
  expect_gt(T_end_on, T_end_off)

  bt_off <- gt_breakthrough(off$hist$t_day, off$hist$Temp_pro,
                            s$m$p$T_res, s$m$p$T_drop_crit)
  bt_on  <- gt_breakthrough(on$hist$t_day, on$hist$Temp_pro,
                            s$m$p$T_res, s$m$p$T_drop_crit)
  expect_false(is.na(bt_off))
  if (!is.na(bt_on)) expect_gt(bt_on, bt_off)
})

test_that("敏感度:lambda_cap 越大,補溫效果越明顯(單調)", {
  s <- gt_cap_setup(n_z_cap = 8)
  lo <- gt_solve_heat(s$m, s$fl, lambda_cap = 108000,
                      store_snaps = FALSE, progress = FALSE)
  hi <- gt_solve_heat(s$m, s$fl, lambda_cap = 432000,
                      store_snaps = FALSE, progress = FALSE)
  T_lo <- utils::tail(lo$hist$Temp_pro, 1)
  T_hi <- utils::tail(hi$hist$Temp_pro, 1)
  expect_gt(T_hi, T_lo)
})
