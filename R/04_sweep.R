# ==========================================================================
# 04_sweep.R — 設計掃描:井距 x 抽注率 -> 熱突破年數
#
# 這一支才是「工程決策」的來源。單次模擬只回答「這個配置能撐幾年」;
# 掃描回答的是「要撐 30 年,井距要開多大」—— 那是 PI 會有感的輸出。
#
# 為了跑得完,掃描用比較粗的網格(dx_sweep)與比較大的模型域。
# 粗網格的數值延散比較大 -> 突破時間會略微低估(偏保守)。這是誠實的
# 取捨,不是 bug;README 有寫。
#
# 執行:  Rscript R/04_sweep.R        (約 1–2 分鐘)
# ==========================================================================

source("R/01_setup.R")
source("R/02_flow.R")
source("R/03_heat.R")

## ---- 掃描設定 -----------------------------------------------------------
SWEEP_L   <- c(400, 600, 800, 1000, 1200, 1400, 1600) # 井距 [m]
SWEEP_Q   <- c(2160, 4320, 6480, 8640)               # 抽注率 [m3/day] = 25..100 L/s
SWEEP_GRID <- list(Lx = 4000, Ly = 2400, dx = 40)    # 粗網格 + 夠大的模型域
SWEEP_YEARS <- 60                                    # 要看得到 30 年等值線

#' 跑一個配置,回傳熱突破年數(未突破則為 NA)
gt_sweep_one <- function(L, Q, years = SWEEP_YEARS, grid = SWEEP_GRID) {
  p <- gt_params(L_doublet = L, Q_well = Q, t_end_yr = years,
                 Lx = grid$Lx, Ly = grid$Ly, dx = grid$dx)
  m  <- gt_model(p)
  fl <- gt_solve_flow(m)
  ht <- gt_solve_heat(m, fl, store_snaps = FALSE, progress = FALSE)
  bt <- gt_breakthrough(ht$hist$t_day, ht$hist$Temp_pro,
                        p$T_res, p$T_drop_crit)
  data.frame(
    L_doublet  = m$L_actual,
    Q_well     = Q,
    Q_Ls       = Q / 86.4,
    bt_yr      = bt / DAYS_PER_YEAR,
    bt_yr_gs   = m$t_heat_gs / DAYS_PER_YEAR,   # 解析解(銳利鋒面)參考值
    Temp_end   = utils::tail(ht$hist$Temp_pro, 1),
    dt_day     = ht$dt,
    energy_err = ht$energy_err_rel
  )
}

gt_run_sweep <- function() {
  grid <- SWEEP_GRID
  cat(sprintf("設計掃描:%d 個井距 x %d 個抽注率 = %d 次模擬\n",
              length(SWEEP_L), length(SWEEP_Q),
              length(SWEEP_L) * length(SWEEP_Q)))
  cat(sprintf("  掃描網格 %g x %g m, dx = %g m, 模擬 %g 年\n",
              grid$Lx, grid$Ly, grid$dx, SWEEP_YEARS))

  rows <- list(); n <- 0L
  tic <- proc.time()[["elapsed"]]
  for (Q in SWEEP_Q) {
    for (L in SWEEP_L) {
      n <- n + 1L
      r <- gt_sweep_one(L, Q)
      rows[[n]] <- r
      cat(sprintf("  [%2d/%2d] L = %4.0f m, Q = %5.1f L/s -> %s\n",
                  n, length(SWEEP_L) * length(SWEEP_Q), r$L_doublet, r$Q_Ls,
                  if (is.na(r$bt_yr)) sprintf("> %g yr", SWEEP_YEARS)
                  else sprintf("%.1f yr", r$bt_yr)))
    }
  }
  sweep <- do.call(rbind, rows)
  cat(sprintf("  共 %.0f s\n", proc.time()[["elapsed"]] - tic))

  ## 對每一個抽注率,內插出「撐得到 30 年」需要的井距
  need30 <- do.call(rbind, lapply(split(sweep, sweep$Q_well), function(d) {
    d <- d[order(d$L_doublet), ]
    ok <- is.na(d$bt_yr) | d$bt_yr >= 30
    k  <- which(ok)[1L]
    Lneed <- if (is.na(k)) NA_real_
             else if (k == 1L) d$L_doublet[1L]
             else if (is.na(d$bt_yr[k])) NA_real_
             else stats::approx(d$bt_yr[(k - 1L):k], d$L_doublet[(k - 1L):k],
                                xout = 30)$y
    data.frame(Q_well = d$Q_well[1L], Q_Ls = d$Q_Ls[1L], L_need_30yr = Lneed)
  }))
  cat("\n--- 撐滿 30 年所需的最小井距 ---------------------------\n")
  for (i in seq_len(nrow(need30))) {
    cat(sprintf("  %5.1f L/s  ->  %s\n", need30$Q_Ls[i],
                if (is.na(need30$L_need_30yr[i])) "掃描範圍內都不夠"
                else sprintf("%.0f m", need30$L_need_30yr[i])))
  }
  cat("---------------------------------------------------------\n")

  save(sweep, need30, file = gt_out("04_sweep.RData"))
  cat("已寫出 ", gt_out("04_sweep.RData"), "\n", sep = "")
  invisible(sweep)
}

if (sys.nframe() == 0L) gt_run_sweep()
