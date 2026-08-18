# ==========================================================================
# 質量守恆 —— 穩態流場最基本的一條
#
# 離散式解出來之後,「每一格流進去的水 = 流出來的水」必須成立到解算器的
# 收斂容差。這條不過,後面的溫度場算得再漂亮都是假的:速度場錯了。
#
# 它抓得到的真實錯誤:邊界鏡像寫反、井的源匯項少除一個 Tr、SOR 沒收斂就
# 回傳、dx 在推導時沒消乾淨。
# ==========================================================================

gt_test_model <- function(...) {
  gt_model(gt_params(Lx = 1600, Ly = 1000, dx = 25, L_doublet = 400, ...))
}

test_that("SOR 有收斂", {
  fl <- gt_solve_flow(gt_test_model())
  expect_true(fl$converged)
  expect_lt(fl$iters, 100000L)
})

test_that("每一格的質量殘差相對抽注率小於 1e-8", {
  m  <- gt_test_model()
  fl <- gt_solve_flow(m)
  resid <- gt_flow_imbalance(m, fl$h)[!m$fixed]
  expect_lt(max(abs(resid)) / m$p$Q_well, 1e-8)
})

test_that("全域收支平衡:注入 = 生產,邊界淨通量為零", {
  m  <- gt_test_model()
  fl <- gt_solve_flow(m)
  ## 注入與生產等量,所以定水頭邊界的淨通量必須是 0
  expect_lt(abs(sum(fl$Qext)) / m$p$Q_well, 1e-8)
  ## 而且左右邊界確實各自有進有出(區域坡降是真的有作用,不是擺設)
  m2  <- gt_test_model(grad_regional = 0.002)
  fl2 <- gt_solve_flow(m2)
  expect_gt(sum(pmax(fl2$Qext, 0)), 0)
  expect_lt(sum(pmin(fl2$Qext, 0)), 0)
})

test_that("不透水邊界真的不透水", {
  ## 上下邊界是 no-flow(鏡像法)。面通量陣列的最外圈必須全部是 0。
  m  <- gt_test_model()
  fl <- gt_solve_flow(m)
  expect_true(all(fl$Qyf[1, ] == 0))
  expect_true(all(fl$Qyf[nrow(fl$Qyf), ] == 0))
  expect_true(all(fl$Qxf[, 1] == 0))
  expect_true(all(fl$Qxf[, ncol(fl$Qxf)] == 0))
})

test_that("井真的抽到了設定的量", {
  m  <- gt_test_model()
  fl <- gt_solve_flow(m)
  ## 生產井那一格,四個面流進來的水必須等於抽水量
  nx <- m$nx; ny <- m$ny
  qin <- fl$Qxf[, 1:nx] - fl$Qxf[, 2:(nx + 1L)] +
         fl$Qyf[1:ny, ] - fl$Qyf[2:(ny + 1L), ]
  expect_equal(qin[m$idx_pro], m$p$Q_well, tolerance = 1e-8)
  expect_equal(qin[m$idx_inj], -m$p$Q_well, tolerance = 1e-8)
})
