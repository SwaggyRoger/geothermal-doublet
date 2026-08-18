# ==========================================================================
# 能量守恆 —— 有限體積法白送的一條恆等式
#
# 內部每一個面,對「左邊那格」是流出、對「右邊那格」是流入,數值一模一樣。
# 所以全場加總時內部通量兩兩相消,總能量只能被井與定水頭邊界改變:
#
#     E(t) - E(0)  ==  累積(井 + 邊界)輸入
#
# 這不是近似,是恆等式,誤差應該落在機器精度(~1e-14)。只要有人在通量
# 表示式裡打錯一個號、或是把某一個面算了兩次,這條會立刻紅掉 ——
# 而溫度圖看起來仍然完全正常。這就是它的價值。
# ==========================================================================

gt_energy_setup <- function(...) {
  m <- gt_model(gt_params(Lx = 1600, Ly = 1000, dx = 25,
                          L_doublet = 400, t_end_yr = 5, ...))
  list(m = m, fl = gt_solve_flow(m))
}

test_that("熱傳輸的能量守恆到機器精度", {
  s  <- gt_energy_setup()
  ht <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  expect_lt(ht$energy_err_rel, 1e-9)
})

test_that("保守示蹤劑的質量守恆到機器精度", {
  s  <- gt_energy_setup()
  tr <- gt_solve_heat(s$m, s$fl, C_bulk = s$m$p$phi * s$m$C_w,
                      store_snaps = FALSE, progress = FALSE)
  expect_lt(tr$energy_err_rel, 1e-9)
})

test_that("關掉延散與傳導,守恆仍然成立", {
  s  <- gt_energy_setup(lambda_bulk = 0, alpha_L = 0)
  ht <- gt_solve_heat(s$m, s$fl, lambda_bulk = 0,
                      store_snaps = FALSE, progress = FALSE)
  expect_lt(ht$energy_err_rel, 1e-9)
})

test_that("有區域流(邊界有進出)時,守恆仍然成立", {
  ## 邊界通量若處理錯(例如流出的水帶走 T_res 而不是本格溫度),
  ## 這條就會紅 —— 而且是唯一會紅的一條。
  s  <- gt_energy_setup(grad_regional = 0.002)
  ht <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  expect_lt(ht$energy_err_rel, 1e-9)
  expect_gt(sum(pmax(s$fl$Qext, 0)), 0)   # 確認邊界真的有水進來
})

test_that("生產井帶走的能量方向正確(儲層在降溫,不是升溫)", {
  s  <- gt_energy_setup(t_end_yr = 20)
  ht <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  expect_lt(utils::tail(ht$hist$E_tot, 1), ht$hist$E_tot[1])
  expect_lt(utils::tail(ht$hist$Temp_pro, 1), s$m$p$T_res)
})
