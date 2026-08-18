# ==========================================================================
# 顯式法的穩定條件 —— 網格法最經典的陷阱
#
# 顯式時間積分的更新式是
#     Temp_new = (1 + dt*cP/(V*C)) * Temp + (一堆非負的鄰格項)
# cP 恆為負。dt 一旦大到讓「自己這一格的係數」變負,解就開始振盪、長出
# 物理上不存在的新極值,然後指數放大。
#
# 重點:這不是一個「慢慢變差」的過程,是懸崖。而且它不會警告你 ——
# 它只會安靜地給你 10^14 degC。所以第二條測試不是驗證程式會動,
# 而是驗證「那條上限是真的」。
# ==========================================================================

gt_cfl_setup <- function() {
  m <- gt_model(gt_params(Lx = 1600, Ly = 1000, dx = 25,
                          L_doublet = 400, t_end_yr = 3))
  list(m = m, fl = gt_solve_flow(m))
}

test_that("自動選出來的 dt 在穩定上限之內", {
  s  <- gt_cfl_setup()
  ht <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  expect_lte(ht$dt, ht$dt_max)
  expect_equal(ht$dt / ht$dt_max, s$m$p$dt_safety, tolerance = 0.05)
})

test_that("穩定的解不產生新極值,也不會有 NaN", {
  s  <- gt_cfl_setup()
  ht <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)
  expect_true(all(is.finite(ht$Temp)))
  ## 最大值原理:場內溫度不可能低於回注水,也不可能高於儲層初始溫度
  expect_gte(min(ht$Temp), s$m$p$T_inj - 1e-6)
  expect_lte(max(ht$Temp), s$m$p$T_res + 1e-6)
})

test_that("超過穩定上限就會炸掉 —— 證明那條上限不是裝飾", {
  s  <- gt_cfl_setup()
  ht <- gt_solve_heat(s$m, s$fl, store_snaps = FALSE, progress = FALSE)

  bad <- gt_solve_heat(s$m, s$fl, dt = 2 * ht$dt_max,
                       store_snaps = FALSE, progress = FALSE)
  ## dt 加倍之後,溫度會離開 [70, 130] 好幾個數量級
  expect_gt(max(bad$Temp), 1e6)
  expect_lt(min(bad$Temp), -1e6)

  ## 注意:1.2 倍還活著。上限是「保證不振盪」的充分條件,取的是全場最嚴的
  ## 那一格,所以略微超過時通常還撐得住 —— 這正是它危險的地方:
  ## 你不會在稍微越界時得到警告,只會在某天換了參數之後突然全錯。
  mild <- gt_solve_heat(s$m, s$fl, dt = 1.2 * ht$dt_max,
                        store_snaps = FALSE, progress = FALSE)
  expect_true(all(is.finite(mild$Temp)))
})

test_that("dt_max 會隨著抽注率變大而變小", {
  ## 流速越快,允許的時間步長越短。這個方向感錯了,掃描分析就會在高抽注率
  ## 的那幾格悄悄崩掉。
  m1 <- gt_model(gt_params(Lx = 1600, Ly = 1000, dx = 25, L_doublet = 400,
                           Q_well = 2160, t_end_yr = 1))
  m2 <- gt_model(gt_params(Lx = 1600, Ly = 1000, dx = 25, L_doublet = 400,
                           Q_well = 8640, t_end_yr = 1))
  h1 <- gt_solve_heat(m1, gt_solve_flow(m1), store_snaps = FALSE, progress = FALSE)
  h2 <- gt_solve_heat(m2, gt_solve_flow(m2), store_snaps = FALSE, progress = FALSE)
  expect_gt(h1$dt_max, h2$dt_max)
})
