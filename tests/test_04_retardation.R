# ==========================================================================
# 熱遲滯因子 R —— 這個 repo 最重要的一條測試
#
#     R = C_bulk / (phi * C_water)
#       = (phi*rho_c_w + (1-phi)*rho_c_s) / (phi * rho_c_w)
#       = 2.4e6 / (0.10 * 4.2e6)  =  5.71
#
# 意思是:熱鋒面走得比水慢 5.71 倍。示蹤劑打穿 != 熱打穿。
#
# 為什麼一定要有這條測試:忘記 R(或把 C_bulk 誤用成 C_water)不會讓程式
# 壞掉、不會噴 warning、圖畫出來一樣漂亮 —— 只是熱突破時間會差 5.7 倍,
# 井距會少估到剩五分之一。這是「藏在數字裡的 bug」的教科書範例。
#
# 做法:用同一個流場跑兩次,唯一的差別是儲熱項 C_bulk。
#   熱      C_bulk = phi*C_w + (1-phi)*C_s   -> R = 5.71
#   示蹤劑  C_bulk = phi*C_w                 -> R = 1
# 兩者的控制方程式差一個時間縮放,所以到達時間的比值必須「剛好」等於 R。
# 時間步長也按 R 縮放,連數值延散的 Courant 數都一致 —— 這條可以要求到
# 0.5% 以內,不需要放水。
# ==========================================================================

test_that("R 的算式本身就對(直接對參數表算一次)", {
  p <- gt_params()
  m <- gt_model(p)
  hand <- (p$phi * p$rho_c_w + (1 - p$phi) * p$rho_c_s) / (p$phi * p$rho_c_w)
  expect_equal(m$R_thermal, hand, tolerance = 1e-12)
  expect_equal(m$R_thermal, 2.4e6 / (0.10 * 4.2e6), tolerance = 1e-9)
  expect_gt(m$R_thermal, 1)          # 熱一定比水慢,不可能反過來
})

test_that("模擬量到的遲滯比值 = 理論 R", {
  p <- gt_params(Lx = 1600, Ly = 1000, dx = 25, L_doublet = 400,
                 grad_regional = 0, lambda_bulk = 0, t_end_yr = 25)
  m  <- gt_model(p)
  fl <- gt_solve_flow(m)

  heat <- gt_solve_heat(m, fl, store_snaps = FALSE, progress = FALSE)
  ## 對照組:唯一的差別是 C_bulk。時間長度與時間步長都除以 R,
  ## 讓兩次計算的 Courant 數完全一致(否則數值延散會不一樣)。
  tracer <- gt_solve_heat(m, fl,
                          C_bulk = p$phi * m$C_w,
                          t_end  = p$t_end_yr * DAYS_PER_YEAR / m$R_thermal,
                          dt     = heat$dt / m$R_thermal,
                          store_snaps = FALSE, progress = FALSE)
  expect_equal(tracer$nstep, heat$nstep)

  t50_heat <- gt_arrival(heat$hist$t_day, heat$hist$Temp_pro,
                         p$T_res, p$T_inj, frac = 0.5)
  t50_trac <- gt_arrival(tracer$hist$t_day, tracer$hist$Temp_pro,
                         p$T_res, p$T_inj, frac = 0.5)
  expect_false(is.na(t50_heat))
  expect_false(is.na(t50_trac))
  expect_equal(t50_heat / t50_trac, m$R_thermal, tolerance = 0.005)
})

test_that("不只中位到達時間 —— 整條突破曲線都差一個 R 的時間縮放", {
  p <- gt_params(Lx = 1600, Ly = 1000, dx = 25, L_doublet = 400,
                 grad_regional = 0, lambda_bulk = 0, t_end_yr = 25)
  m  <- gt_model(p)
  fl <- gt_solve_flow(m)
  heat   <- gt_solve_heat(m, fl, store_snaps = FALSE, progress = FALSE)
  tracer <- gt_solve_heat(m, fl, C_bulk = p$phi * m$C_w,
                          t_end = p$t_end_yr * DAYS_PER_YEAR / m$R_thermal,
                          dt    = heat$dt / m$R_thermal,
                          store_snaps = FALSE, progress = FALSE)
  ## 只挑 25 年內真的到得了的比例 —— t50 已經落在 22 年,再往上就會拿到
  ## NA,那是「模擬還沒跑到」而不是「物理不對」。
  for (fr in c(0.1, 0.2, 0.35, 0.5)) {
    lbl <- sprintf("到達比例 %.0f%%", 100 * fr)
    th <- gt_arrival(heat$hist$t_day, heat$hist$Temp_pro,
                     p$T_res, p$T_inj, frac = fr)
    tt <- gt_arrival(tracer$hist$t_day, tracer$hist$Temp_pro,
                     p$T_res, p$T_inj, frac = fr)
    expect_false(is.na(th), info = paste(lbl, "熱:模擬期間內未到達"))
    expect_false(is.na(tt), info = paste(lbl, "示蹤劑:模擬期間內未到達"))
    expect_equal(th / tt, m$R_thermal, tolerance = 0.005, info = lbl)
  }
})

test_that("熱突破一定比示蹤劑突破晚很多(方向感檢查)", {
  ## 上面那條驗的是「比值等於 R」,萬一有人把 C_bulk 的公式整個寫錯,
  ## 量到的比值和報出來的 R 會一起錯、一起通過。所以再加一條絕對檢查:
  ## 熱突破必須落在示蹤劑突破的 3 到 10 倍之間(合理岩層的 R 範圍)。
  p <- gt_params(Lx = 1600, Ly = 1000, dx = 25, L_doublet = 400,
                 grad_regional = 0, lambda_bulk = 0, t_end_yr = 25)
  m  <- gt_model(p)
  fl <- gt_solve_flow(m)
  heat   <- gt_solve_heat(m, fl, store_snaps = FALSE, progress = FALSE)
  tracer <- gt_solve_heat(m, fl, C_bulk = p$phi * m$C_w,
                          t_end = 3000, store_snaps = FALSE, progress = FALSE)
  th <- gt_arrival(heat$hist$t_day, heat$hist$Temp_pro, p$T_res, p$T_inj, 0.5)
  tt <- gt_arrival(tracer$hist$t_day, tracer$hist$Temp_pro,
                   p$T_res, p$T_inj, 0.5)
  expect_gt(th / tt, 3)
  expect_lt(th / tt, 10)
})
