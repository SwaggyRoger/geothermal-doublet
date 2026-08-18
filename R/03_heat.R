# ==========================================================================
# 03_heat.R — 溫度的移流–延散(熱傳輸)
#
# 用「能量的有限體積法」寫,而不是直接寫 dT/dt 的偏微分方程。理由有三:
#   1. 能量守恆變成一條可以測到機器精度的恆等式(見 tests/)。
#   2. 井的源匯項自然而然就對了,不用另外湊。
#   3. 熱遲滯因子 R 不是「乘上去的修正」,而是從 C_b / (phi * C_w) 自己長
#      出來的 —— 學生可以在結果裡「量」到它,再回頭對理論值。
#
# 每一格:   V * C_bulk * dTemp/dt = sum(面能量通量) + 井 + 邊界
#   平流:   C_w * Q_face * Temp_供給格        (一階上風差分)
#   傳導:   lambda_e * b * (Temp_鄰 - Temp_P)  (A_face/dx = b,dx 消掉)
#   延散:   lambda_e = lambda_bulk + C_w * alpha_L * |Darcy 通量|
#
# 時間:顯式 Euler。穩定條件由離散式自己算出來(不是背公式),見 dt_max。
#
# 執行:  Rscript R/03_heat.R      (需要先跑過 02_flow.R)
# ==========================================================================

source("R/01_setup.R")

## ---- 求解器 -------------------------------------------------------------

#' 解溫度場的時間演化
#'
#' @param C_bulk  儲熱的體積熱容。預設 m$C_b(熱)。
#'                傳 m$p$phi * m$C_w 就變成「保守示蹤劑」(R = 1)——
#'                tests/test_retardation.R 就是靠這一個參數做對照組。
#' @param dt      NULL = 自動取 dt_safety * 穩定上限
#' @return list(Temp, hist, snaps, dt, dt_max, energy_err_rel, ...)
gt_solve_heat <- function(m, fl,
                          C_bulk      = m$C_b,
                          lambda_bulk = m$p$lambda_bulk,
                          t_end       = m$p$t_end_yr * DAYS_PER_YEAR,
                          dt          = NULL,
                          snap_per_yr = m$p$snap_per_yr,
                          store_snaps = TRUE,
                          progress    = TRUE) {
  nx <- m$nx; ny <- m$ny
  C_w <- m$C_w; dxm <- m$p$dx; bm <- m$p$b

  ## --- 面上的量(整場只算一次)---
  Qxw <- fl$Qxf[, 1:nx, drop = FALSE]            # 西面,正 = 流入本格
  Qxe <- fl$Qxf[, 2:(nx + 1L), drop = FALSE]     # 東面,正 = 流出本格
  Qyn <- fl$Qyf[1:ny, , drop = FALSE]            # 北面,正 = 流入本格
  Qys <- fl$Qyf[2:(ny + 1L), , drop = FALSE]     # 南面,正 = 流出本格

  lam_x <- lambda_bulk + C_w * m$p$alpha_L * abs(fl$Qxf) / (dxm * bm)
  lam_y <- lambda_bulk + C_w * m$p$alpha_L * abs(fl$Qyf) / (dxm * bm)
  lam_x[, 1] <- 0; lam_x[, nx + 1L] <- 0         # 外側面絕熱:只有平流進出
  lam_y[1, ] <- 0; lam_y[ny + 1L, ] <- 0
  Gxw <- lam_x[, 1:nx, drop = FALSE]        * bm # 傳導導度 [J/day/K]
  Gxe <- lam_x[, 2:(nx + 1L), drop = FALSE] * bm
  Gyn <- lam_y[1:ny, , drop = FALSE]        * bm
  Gys <- lam_y[2:(ny + 1L), , drop = FALSE] * bm

  ## --- 把整條能量方程式攤成「常數係數 x 鄰格溫度」---------------------
  ## dE = cW*Tw + cE*Te + cN*Tn + cS*Ts + cP*Temp + cst
  ## 這些係數只和(已經解好的)流場有關,整場只算一次,迴圈裡就不用再碰
  ## pmax/pmin。這是這支程式最重要的一個效能決定。
  cW <- C_w * pmax(Qxw, 0) + Gxw
  cE <- -C_w * pmin(Qxe, 0) + Gxe
  cN <- C_w * pmax(Qyn, 0) + Gyn
  cS <- -C_w * pmin(Qys, 0) + Gys
  ## 匯項(生產井、流出邊界)對「自己這一格」的係數,能量帳也要用到
  w_sink <- C_w * (pmin(m$Qw, 0) + pmin(fl$Qext, 0))
  cst <- C_w * (pmax(m$Qw, 0) * m$p$T_inj + pmax(fl$Qext, 0) * m$p$T_res)
  cP <- C_w * (pmin(Qxw, 0) - pmax(Qxe, 0) + pmin(Qyn, 0) - pmax(Qys, 0)) -
        (Gxw + Gxe + Gyn + Gys) + w_sink

  ## --- 顯式法的穩定上限 ------------------------------------------------
  ## 更新式是  Temp_new = (1 + dt*cP/(V*C)) * Temp + (全部非負的鄰格項)
  ## cP 恆為負。只要「自己這一格的係數」掉到負的,解就會開始振盪、長出
  ## 物理上不存在的新極值,最後炸成 NaN。所以穩定條件就是這一句話:
  ##        dt <= V * C_bulk / (-cP)
  ## 不是背來的 CFL 公式,是從你自己寫的離散式讀出來的。
  dt_max <- min(m$V_cell * C_bulk / pmax(-cP, .Machine$double.eps))

  if (is.null(dt)) {
    ## 自動選 dt:先取安全係數,再微調成整除,終點才會剛好落在 t_end
    dt <- m$p$dt_safety * dt_max
    nstep <- max(1L, as.integer(ceiling(t_end / dt)))
    dt <- t_end / nstep
  } else {
    ## 呼叫端指定 dt 時,一律照用不動 —— 測試要靠「dt 剛好是多少」來
    ## 驗證穩定上限與熱遲滯比值,偷偷改小它會讓那些測試失去意義。
    nstep <- max(1L, as.integer(ceiling(t_end / dt)))
  }

  ## 把 dt/(V*C) 併進係數,迴圈裡就只剩乘加
  fac <- dt / (m$V_cell * C_bulk)
  aW <- fac * cW; aE <- fac * cE; aN <- fac * cN; aS <- fac * cS
  aP <- 1 + fac * cP; acst <- fac * cst
  i_sink <- which(w_sink != 0)                    # 只有生產井與流出邊界
  w_sink_v <- w_sink[i_sink]
  cst_sum <- sum(cst)

  ## --- IC ---
  Temp <- matrix(m$p$T_res, ny, nx)
  E0 <- sum(m$V_cell * C_bulk * Temp)

  ## --- 記錄容器 ---
  t_day    <- numeric(nstep + 1L)
  Temp_pro <- numeric(nstep + 1L)
  E_tot    <- numeric(nstep + 1L)
  E_in_cum <- numeric(nstep + 1L)
  t_day[1] <- 0; Temp_pro[1] <- Temp[m$idx_pro]; E_tot[1] <- E0

  snap_every <- if (store_snaps) {
    max(1L, as.integer(round(DAYS_PER_YEAR / snap_per_yr / dt)))
  } else NA_integer_
  snaps <- list(); snap_t <- numeric(0)
  if (store_snaps) { snaps[[1]] <- Temp; snap_t <- 0 }

  cum <- 0
  iw <- c(1L, 1:(nx - 1L)); ie <- c(2:nx, nx)     # 鏡像 padding 的取用索引
  jn <- c(1L, 1:(ny - 1L)); js <- c(2:ny, ny)     # (最外側面通量 = 0,取誰都行)
  for (k in seq_len(nstep)) {
    ## 這一步進來多少能量?只有井與定水頭邊界能改變總能量 ——
    ## 內部面通量兩兩相消,這是有限體積法白送的守恆性質。用「更新前」的
    ## 溫度算,才和顯式 Euler 一致。
    cum <- cum + dt * (cst_sum + sum(w_sink_v * Temp[i_sink]))

    ## 平流(上風)+ 傳導 + 延散 + 井 + 邊界,全部收在這一行
    Temp <- aP * Temp +
            aW * Temp[, iw, drop = FALSE] + aE * Temp[, ie, drop = FALSE] +
            aN * Temp[jn, , drop = FALSE] + aS * Temp[js, , drop = FALSE] +
            acst

    t_day[k + 1L]    <- k * dt
    Temp_pro[k + 1L] <- Temp[m$idx_pro]
    E_tot[k + 1L]    <- m$V_cell * C_bulk * sum(Temp)
    E_in_cum[k + 1L] <- cum

    if (store_snaps && k %% snap_every == 0L) {
      snaps[[length(snaps) + 1L]] <- Temp
      snap_t <- c(snap_t, k * dt)
    }
    if (progress && k %% max(1L, nstep %/% 10L) == 0L) {
      cat(sprintf("\r  %3.0f%%  t = %6.1f yr  生產井 %.2f degC",
                  100 * k / nstep, k * dt / DAYS_PER_YEAR, Temp[m$idx_pro]))
      utils::flush.console()
    }
  }
  if (progress) cat("\n")

  ## 能量守恆的相對誤差:內部面通量兩兩相消,總量只能由井與邊界改變
  scale <- max(abs(E_in_cum[nstep + 1L]), .Machine$double.eps)
  energy_err_rel <- abs((E_tot[nstep + 1L] - E0) - E_in_cum[nstep + 1L]) / scale

  list(
    Temp = Temp, dt = dt, dt_max = dt_max, nstep = nstep,
    hist = data.frame(t_day = t_day, t_yr = t_day / DAYS_PER_YEAR,
                      Temp_pro = Temp_pro, E_tot = E_tot, E_in_cum = E_in_cum),
    snaps = snaps, snap_t = snap_t,
    energy_err_rel = energy_err_rel,
    C_bulk = C_bulk, lambda_bulk = lambda_bulk
  )
}

## ---- 以 script 執行時才跑 ----------------------------------------------

gt_run_heat <- function() {
  load(gt_out("02_flow.RData"))            # 帶回 m 與 fl
  cat("解溫度場 ...\n")

  tic <- proc.time()[["elapsed"]]
  ht <- gt_solve_heat(m, fl)
  cat(sprintf("  dt = %.3f day (穩定上限 %.3f day), %d 步, %.1f s\n",
              ht$dt, ht$dt_max, ht$nstep, proc.time()[["elapsed"]] - tic))
  cat(sprintf("  能量守恆相對誤差 = %.3e\n", ht$energy_err_rel))

  bt <- gt_breakthrough(ht$hist$t_day, ht$hist$Temp_pro,
                        m$p$T_res, m$p$T_drop_crit)
  cat(sprintf("  生產井末溫 = %.2f degC (起始 %.1f)\n",
              utils::tail(ht$hist$Temp_pro, 1), m$p$T_res))
  if (is.na(bt)) {
    cat(sprintf("  在 %g 年內未達 -%g degC 的熱突破判準\n",
                m$p$t_end_yr, m$p$T_drop_crit))
  } else {
    cat(sprintf("  熱突破 (-%g degC) = %.0f day = %.1f yr   [解析解參考 %.1f yr]\n",
                m$p$T_drop_crit, bt, bt / DAYS_PER_YEAR,
                m$t_heat_gs / DAYS_PER_YEAR))
  }

  save(m, fl, ht, file = gt_out("03_heat.RData"))
  cat("已寫出 ", gt_out("03_heat.RData"), "\n", sep = "")
  invisible(ht)
}

if (sys.nframe() == 0L) gt_run_heat()
