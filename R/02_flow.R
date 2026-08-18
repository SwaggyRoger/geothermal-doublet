# ==========================================================================
# 02_flow.R — 穩態水頭場 + Darcy 通量場
#
# 物理:侷限含水層穩態流   Tr * lap(h) = -Q_well / dx^2
# 離散:五點差分。把它整理成「每格 = 四鄰平均 + 源匯」的形式後,dx 會消掉:
#
#       h_P = ( h_N + h_S + h_W + h_E + Q_well/Tr ) / 4
#
# 解法:紅黑 SOR(超鬆弛)。全程向量化 —— 一次更新半個棋盤,
#       絕對不要寫雙層 for loop 逐格計算(R 會慢上千倍)。
#
# 執行:  Rscript R/02_flow.R
# ==========================================================================

source("R/01_setup.R")

## ---- 求解器 -------------------------------------------------------------

#' 紅黑 SOR 解穩態水頭
#'
#' @param tol   收斂判準:最大格點殘差 [m]
#' @return list(h, Qxf, Qyf, Qext, resid_max, iters, converged, omega)
gt_solve_flow <- function(m, tol = 1e-11, maxit = 100000L, progress = FALSE) {
  nx <- m$nx; ny <- m$ny

  h <- matrix(m$p$h_ref, ny, nx)
  h[m$fixed] <- m$h_bc[m$fixed]

  src <- m$Qw / m$Tr                      # 源匯項換算成水頭單位 [m]
  free <- !m$fixed
  chk  <- (row(h) + col(h)) %% 2          # 棋盤:0 = 紅, 1 = 黑

  ## 先算好要更新的格點索引(整數索引比每一步重算邏輯遮罩快得多)。
  ## 自由格一定落在第 2..nx-1 行,所以「內部區塊」的索引剛好差一整行 ny。
  idx <- list(which(free & chk == 0), which(free & chk == 1))

  ## 矩形域最佳鬆弛因子的經典估計值(接近 2 但不能等於 2)
  omega <- 2 / (1 + sin(pi / max(nx, ny)))

  ii <- 2:(nx - 1)                        # 只有內部行需要更新(左右是定水頭)
  src_ii <- src[, ii, drop = FALSE]

  iters <- maxit
  converged <- FALSE
  for (k in seq_len(maxit)) {
    for (id in idx) {
      hp <- rbind(h[1, ], h, h[ny, ])     # 上下鏡像 padding = no-flow 邊界
      core <- (hp[1:ny, ii, drop = FALSE] + hp[3:(ny + 2L), ii, drop = FALSE] +
               h[, ii - 1L, drop = FALSE] + h[, ii + 1L, drop = FALSE] +
               src_ii) / 4
      h[id] <- h[id] + omega * (core[id - ny] - h[id])
    }
    if (k %% 20L == 0L || k == maxit) {
      r <- max(abs(gt_flow_imbalance(m, h)[free]))
      ## 看著殘差一個數量級一個數量級往下掉 —— 這就是「收斂」長的樣子
      if (progress && k %% 100L == 0L) {
        cat(sprintf("\r  迭代 %5d   最大質量殘差 %9.2e m3/day", k, r))
        utils::flush.console()
      }
      if (r / m$Tr < tol) { iters <- k; converged <- TRUE; break }
    }
  }
  if (progress) cat("\r", strrep(" ", 58), "\r", sep = "")
  if (!converged) warning("SOR 未在 maxit 內收斂", call. = FALSE)

  Qext <- gt_flow_imbalance(m, h)         # 自由格 ~= 0;定水頭格 = 邊界通量
  Qext[!m$fixed] <- 0                     # 自由格的殘差不是物理通量,歸零

  ## 面通量 [m3/day]。 Q_face = Tr * (h_upstream - h_downstream)
  ##   Qxf 是 ny x (nx+1):Qxf[, i] = 穿過「第 i 格西側面」的流量,正 = +x
  ##   Qyf 是 (ny+1) x nx:Qyf[j, ] = 穿過「第 j 格北側面」的流量,正 = +y
  ## 最外側的面是 no-flow 或已由 Qext 代表,所以補 0。
  Qxf <- cbind(0, m$Tr * (h[, 1:(nx - 1L)] - h[, 2:nx]), 0)
  Qyf <- rbind(0, m$Tr * (h[1:(ny - 1L), ] - h[2:ny, ]), 0)

  list(h = h, Qxf = Qxf, Qyf = Qyf, Qext = Qext,
       resid_max = max(abs(gt_flow_imbalance(m, h)[free])),
       iters = iters, converged = converged, omega = omega)
}

#' 每一格的質量不平衡量 [m3/day]
#'
#'   sum(面流入) + Q_well + Q_ext = 0
#'   sum(面流入) = Tr * (sum(h_鄰) - 4*h_P)      (dx 消掉了)
#'
#' 所以  Q_ext = Tr * (4*h_P - sum(h_鄰)) - Q_well
#' 在自由格,這個值必須是 0(數值誤差內)—— 這就是 tests 裡的質量守恆檢查。
#' 在定水頭格,它就是流進/流出模型域的邊界通量。
gt_flow_imbalance <- function(m, h) {
  nx <- m$nx; ny <- m$ny
  hp <- rbind(h[1, ], h, h[ny, ])         # 上下鏡像
  hp <- cbind(hp[, 1], hp, hp[, nx])      # 左右鏡像(讓最外行的外側面通量 = 0)
  sum_nb <- hp[1:ny, 2:(nx + 1L)] + hp[3:(ny + 2L), 2:(nx + 1L)] +
            hp[2:(ny + 1L), 1:nx] + hp[2:(ny + 1L), 3:(nx + 2L)]
  m$Tr * (4 * h - sum_nb) - m$Qw
}

#' 格心的孔隙(滲流)流速 [m/day] —— 畫流線與熱延散都要用
gt_pore_velocity <- function(m, fl) {
  nx <- m$nx; ny <- m$ny
  denom <- 2 * m$p$dx * m$p$b * m$p$phi
  list(vx = (fl$Qxf[, 1:nx] + fl$Qxf[, 2:(nx + 1L)]) / denom,
       vy = (fl$Qyf[1:ny, ] + fl$Qyf[2:(ny + 1L), ]) / denom)
}

#' 從注入井周圍放示蹤粒子,順流積分出流線(給 05_viz.R 疊圖用)
#'
#' @param t_max_yr 積分的時間上限。沒有這個上限,遠場那些幾乎不動的質點會
#'   一路飄到天邊,圖上就變成一堆沒有意義的長線 —— 限制「幾年內走得到的
#'   地方」才是有物理意義的畫法。
gt_streamlines <- function(m, fl, n_seed = 24L, max_steps = 4000L,
                           t_max_yr = 60) {
  v <- gt_pore_velocity(m, fl)
  dx <- m$p$dx
  Lx <- m$p$Lx; Ly <- m$p$Ly

  bilin <- function(M, px, py) {
    fi <- px / dx + 0.5; fj <- py / dx + 0.5
    i0 <- min(max(floor(fi), 1), m$nx - 1L); tx <- fi - i0
    j0 <- min(max(floor(fj), 1), m$ny - 1L); ty <- fj - j0
    (1 - tx) * (1 - ty) * M[j0,     i0    ] + tx * (1 - ty) * M[j0,     i0 + 1L] +
    (1 - tx) *      ty  * M[j0 + 1L, i0   ] + tx *      ty  * M[j0 + 1L, i0 + 1L]
  }

  x0 <- m$x[m$i_inj]; y0 <- m$y[m$j_wel]
  xp <- m$x[m$i_pro]; yp <- m$y[m$j_wel]
  ang <- seq(0, 2 * pi, length.out = n_seed + 1L)[-(n_seed + 1L)]
  r0  <- 1.5 * dx

  out <- vector("list", n_seed)
  for (s in seq_len(n_seed)) {
    px <- x0 + r0 * cos(ang[s]); py <- y0 + r0 * sin(ang[s])
    path <- matrix(NA_real_, max_steps, 2)
    for (k in seq_len(max_steps)) {
      path[k, ] <- c(px, py)
      ux <- bilin(v$vx, px, py); uy <- bilin(v$vy, px, py)
      sp <- sqrt(ux^2 + uy^2)
      if (!is.finite(sp) || sp < 1e-12) break
      hstep <- 0.5 * dx / sp                       # 每步走半格
      mx <- px + 0.5 * hstep * ux                  # RK2(中點法)
      my <- py + 0.5 * hstep * uy
      ux <- bilin(v$vx, mx, my); uy <- bilin(v$vy, mx, my)
      px <- px + hstep * ux; py <- py + hstep * uy
      if (px < 0 || px > Lx || py < 0 || py > Ly) break
      if (sqrt((px - xp)^2 + (py - yp)^2) < 1.5 * dx) break
    }
    keep <- stats::complete.cases(path)
    if (sum(keep) > 2L) {
      out[[s]] <- data.frame(line = s, x = path[keep, 1], y = path[keep, 2])
    }
  }
  do.call(rbind, out[!vapply(out, is.null, logical(1))])
}

## ---- 以 script 執行時才跑 ----------------------------------------------
## sys.nframe() == 0 只有在「檔案被當成腳本直接執行」時成立;
## 被 source() 進來時 > 0,所以下面這段不會重跑。

gt_run_flow <- function() {
  m <- gt_model(gt_params())
  gt_report(m)

  cat("解穩態水頭 (紅黑 SOR) ...\n")
  tic <- proc.time()[["elapsed"]]
  fl <- gt_solve_flow(m, progress = TRUE)
  cat(sprintf("  omega = %.4f, %d 次迭代, %.1f s\n",
              fl$omega, fl$iters, proc.time()[["elapsed"]] - tic))
  cat(sprintf("  最大格點質量殘差 = %.3e m3/day  (井的抽注率 = %g)\n",
              fl$resid_max, m$p$Q_well))
  cat(sprintf("  水頭範圍 %.3f – %.3f m\n", min(fl$h), max(fl$h)))
  cat(sprintf("  邊界淨通量 = %.3e m3/day (應為 0:注入 = 生產)\n",
              sum(fl$Qext)))

  save(m, fl, file = gt_out("02_flow.RData"))
  cat("已寫出 ", gt_out("02_flow.RData"), "\n", sep = "")
  invisible(fl)
}

if (sys.nframe() == 0L) gt_run_flow()
