# ==========================================================================
# 與解析解對照 —— Gringarten & Sauty (1975)
#
# 無區域流時,抽注井對之間「最短那條流線」的水體到達時間有閉合解:
#
#     t_min = pi * phi * b * L^2 / (3 * Q)
#
# 熱鋒面再慢 R 倍。這條式子是設計圖那張熱區圖的理論骨架 ——
# 「熱突破時間正比於 井距^2 / 抽注率」就是從這裡來的。
#
# 這條測試要誠實一點:一階上風差分有數值延散,鋒面會被抹開,所以模擬量到
# 的「最早到達」一定比解析解早。因此不能寫成「誤差 < 2%」那種假精確的
# 判準。改成三件真正成立的事:
#   1. 單邊界限:最早到達 < 解析解 < 中位到達  (物理上必然)
#   2. 收斂性  :網格加密時,最早到達單調逼近解析解  (數值上必然)
#   3. 量級    :不能差到兩倍以上(抓 pi/3 打成 pi、L 沒平方、忘了 phi)
# ==========================================================================

#' 用純平流(關掉傳導與延散)的保守示蹤劑量到達時間
gt_gs_arrival <- function(dx, frac, t_end = 900) {
  p <- gt_params(Lx = 1600, Ly = 1000, dx = dx, L_doublet = 400,
                 grad_regional = 0, lambda_bulk = 0, alpha_L = 0)
  m  <- gt_model(p)
  fl <- gt_solve_flow(m)
  tr <- gt_solve_heat(m, fl, C_bulk = p$phi * m$C_w, t_end = t_end,
                      store_snaps = FALSE, progress = FALSE)
  list(t = gt_arrival(tr$hist$t_day, tr$hist$Temp_pro,
                      p$T_res, p$T_inj, frac = frac),
       t_gs = m$t_water_gs, m = m)
}

test_that("解析解的公式本身算對了", {
  m <- gt_model(gt_params(L_doublet = 800, Q_well = 4320))
  expect_equal(m$t_water_gs,
               pi * m$p$phi * m$p$b * m$L_actual^2 / (3 * m$p$Q_well),
               tolerance = 1e-12)
  ## 正比於 L^2:井距加倍,時間變四倍
  m2 <- gt_model(gt_params(L_doublet = 1600, Q_well = 4320))
  expect_equal(m2$t_water_gs / m$t_water_gs, 4, tolerance = 1e-6)
  ## 反比於 Q:抽注率加倍,時間減半
  m3 <- gt_model(gt_params(L_doublet = 800, Q_well = 8640))
  expect_equal(m3$t_water_gs / m$t_water_gs, 0.5, tolerance = 1e-6)
  ## 熱的版本就是再乘 R
  expect_equal(m$t_heat_gs / m$t_water_gs, m$R_thermal, tolerance = 1e-12)
})

test_that("最早到達 < 解析解 < 中位到達", {
  a1  <- gt_gs_arrival(25, frac = 0.01)
  a50 <- gt_gs_arrival(25, frac = 0.50, t_end = 4000)
  ## 延散只會讓鋒面提早到,不會延後 -> 1% 到達必定早於銳利鋒面解
  expect_lt(a1$t, a1$t_gs)
  ## 而井對的流線走時分布很寬,中位到達必定晚於「最快那條流線」
  expect_gt(a50$t, a50$t_gs)
})

test_that("網格加密時,最早到達單調逼近解析解", {
  ## 這是真正的驗證:不是比對一個數字,是確認「錯誤會隨著網格細化而消失」。
  ## 如果公式本身寫錯,加密網格只會穩定地收斂到錯誤的答案,這條就會紅。
  r <- vapply(c(50, 25, 12.5), function(dx) {
    a <- gt_gs_arrival(dx, frac = 0.01)
    a$t / a$t_gs
  }, numeric(1))

  expect_true(all(diff(r) > 0), info = paste("比值:", paste(round(r, 3),
                                                            collapse = " -> ")))
  expect_true(all(r < 1))            # 永遠從下方逼近
  expect_gt(r[3], 0.7)               # 最細的網格要走到七成以上
})

test_that("量級對得上(抓 pi/3、L^2、phi 這類公式打錯)", {
  a <- gt_gs_arrival(25, frac = 0.05)
  expect_gt(a$t / a$t_gs, 0.4)
  expect_lt(a$t / a$t_gs, 1.0)
})
