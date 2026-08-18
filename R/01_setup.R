# ==========================================================================
# 01_setup.R — 參數、網格、初始條件 (IC) 與邊界條件 (BC)
#
# 這個檔案「只定義函式,不做運算」。02–05 每一支都以 source("R/01_setup.R")
# 取用這裡的東西,所以它必須很便宜。
#
# 座標與矩陣的對應(全 repo 一致 —— 搞錯這個,所有圖都會轉 90 度):
#   矩陣 m[j, i]:  j = 列 = y 方向,  i = 行 = x 方向
#   x_i = (i - 0.5) * dx        y_j = (j - 0.5) * dx
#
# 命名慣例:溫度一律叫 Temp,絕不叫 T —— 在 R 裡 T 是 TRUE 的別名,
# 拿來當變數名遲早會被咬一口。導水係數叫 Tr。
# ==========================================================================

## ---- 檔案位置(全 repo 只在這裡寫死一次)-------------------------------
GT_PARAMS_FILE <- "params/params.csv"
GT_OUT_DIR     <- "outputs"

#' 產出檔的路徑(順便確保資料夾存在)
gt_out <- function(...) {
  if (!dir.exists(GT_OUT_DIR)) dir.create(GT_OUT_DIR, recursive = TRUE)
  file.path(GT_OUT_DIR, ...)
}

## ---- 參數 ---------------------------------------------------------------

#' 讀參數表,允許以 ... 就地覆寫(掃描分析、測試都靠這個)
#'
#' gt_params(L_doublet = 1200, Q_well = 8640)
gt_params <- function(file = GT_PARAMS_FILE, ...) {
  tab <- utils::read.csv(file, stringsAsFactors = FALSE)
  p <- as.list(as.numeric(tab$value))
  names(p) <- tab$name
  if (anyNA(unlist(p))) {
    stop("params.csv 有無法轉成數字的 value 欄", call. = FALSE)
  }
  ov <- list(...)
  bad <- setdiff(names(ov), names(p))
  if (length(bad) > 0L) {
    stop("未知的參數:", paste(bad, collapse = ", "), call. = FALSE)
  }
  p[names(ov)] <- ov
  p
}

## ---- 模型(網格 + IC + BC + 衍生常數)-----------------------------------

#' 由一組參數建出完整的模型設定
#'
#' 回傳的 list 就是後面所有 script 的「單一事實來源」。
gt_model <- function(p) {
  nx <- as.integer(round(p$Lx / p$dx))
  ny <- as.integer(round(p$Ly / p$dx))
  stopifnot(nx >= 10L, ny >= 10L)

  x <- (seq_len(nx) - 0.5) * p$dx
  y <- (seq_len(ny) - 0.5) * p$dx

  ## 井位:x 置中、間距 L_doublet。注入井放在上游(左側)—— 這是保守配置,
  ## 區域流會把冷水往生產井推。把它換到下游是很好的課後練習。
  i_inj <- which.min(abs(x - (p$Lx / 2 - p$L_doublet / 2)))
  i_pro <- which.min(abs(x - (p$Lx / 2 + p$L_doublet / 2)))
  j_wel <- which.min(abs(y - p$Ly / 2))
  L_actual <- x[i_pro] - x[i_inj]   # 井距會被網格卡到格點上,解析解要用這個

  idx_inj <- j_wel + (i_inj - 1L) * ny   # 矩陣是 column-major
  idx_pro <- j_wel + (i_pro - 1L) * ny

  ## BC:左右定水頭(區域水力坡降),上下不透水(no-flow,用鏡像法表達)
  fixed <- matrix(FALSE, ny, nx)
  fixed[, 1]  <- TRUE
  fixed[, nx] <- TRUE
  h_bc <- matrix(0, ny, nx)
  h_bc[, 1]  <- p$h_ref
  h_bc[, nx] <- p$h_ref - p$grad_regional * p$Lx

  ## 井的源匯項 [m3/day],正 = 注入
  Qw <- matrix(0, ny, nx)
  Qw[idx_inj] <-  p$Q_well
  Qw[idx_pro] <- -p$Q_well

  ## 熱物性
  C_w <- p$rho_c_w                                        # 水的體積熱容
  C_b <- p$phi * p$rho_c_w + (1 - p$phi) * p$rho_c_s      # 岩水混合體
  R_thermal <- C_b / (p$phi * C_w)                        # 熱遲滯因子

  L_act <- L_actual
  ## Gringarten & Sauty (1975):無區域流時,抽注井對之間最短流線的
  ## 示蹤劑到達時間。熱鋒面再慢 R 倍。
  t_water_gs <- pi * p$phi * p$b * L_act^2 / (3 * p$Q_well)
  t_heat_gs  <- R_thermal * t_water_gs

  list(
    p = p, nx = nx, ny = ny, x = x, y = y,
    Tr = p$K * p$b,                        # 導水係數 [m2/day]
    V_cell = p$dx * p$dx * p$b,            # 單格體積 [m3]
    fixed = fixed, h_bc = h_bc, Qw = Qw,
    i_inj = i_inj, i_pro = i_pro, j_wel = j_wel,
    idx_inj = idx_inj, idx_pro = idx_pro, L_actual = L_act,
    C_w = C_w, C_b = C_b, R_thermal = R_thermal,
    t_water_gs = t_water_gs, t_heat_gs = t_heat_gs
  )
}

## ---- 小工具 -------------------------------------------------------------

DAYS_PER_YEAR <- 365.25

#' 生產井溫度歷線 → 熱突破時間 [day]
#'
#' 定義:生產溫度首次低於 T_res - drop 的時刻(線性內插)。
gt_breakthrough <- function(t_day, Temp_pro, T_res, drop) {
  crit <- T_res - drop
  k <- which(Temp_pro <= crit)[1L]
  if (is.na(k) || k == 1L) return(NA_real_)
  t0 <- t_day[k - 1L]; t1 <- t_day[k]
  y0 <- Temp_pro[k - 1L]; y1 <- Temp_pro[k]
  t0 + (crit - y0) * (t1 - t0) / (y1 - y0)
}

#' 到達時間:歷線走完 frac 比例的總降幅所需的時間 [day]
#' frac = 0.5 就是 t50(突破曲線的中位到達時間)。
gt_arrival <- function(t_day, Temp_pro, T_res, T_inj, frac = 0.5) {
  gt_breakthrough(t_day, Temp_pro, T_res, drop = frac * (T_res - T_inj))
}

#' 把模型設定印成人看得懂的一段話
gt_report <- function(m) {
  p <- m$p
  cat(sprintf(
    paste0(
      "--- geothermal-doublet 模型設定 -----------------------------\n",
      "  網格        %d x %d 格, dx = %g m  (%g m x %g m)\n",
      "  含水層      b = %g m, K = %g m/day, Tr = %g m2/day, phi = %.2f\n",
      "  井          注入 (%.0f, %.0f) / 生產 (%.0f, %.0f), 井距 = %g m\n",
      "  抽注率      %g m3/day  (%.1f L/s)\n",
      "  溫度        儲層 %g degC, 回注 %g degC\n",
      "  熱遲滯因子  R = C_b / (phi * C_w) = %.3g / (%.2f * %.3g) = %.2f\n",
      "  解析解參考  示蹤劑突破 %.0f day (%.1f yr) / 熱突破 %.0f day (%.1f yr)\n",
      "-------------------------------------------------------------\n"
    ),
    m$ny, m$nx, p$dx, p$Lx, p$Ly,
    p$b, p$K, m$Tr, p$phi,
    m$x[m$i_inj], m$y[m$j_wel], m$x[m$i_pro], m$y[m$j_wel], m$L_actual,
    p$Q_well, p$Q_well / 86.4,
    p$T_res, p$T_inj,
    m$C_b, p$phi, m$C_w, m$R_thermal,
    m$t_water_gs, m$t_water_gs / DAYS_PER_YEAR,
    m$t_heat_gs,  m$t_heat_gs / DAYS_PER_YEAR
  ))
  invisible(m)
}
