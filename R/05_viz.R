# ==========================================================================
# 05_viz.R — 互動式圖表與報告
#
# 產出:  outputs/report.html   —— 一個檔案,直接用瀏覽器打開,可以寄給 PI
#
# 為什麼是 plotly 而不是 ggplot2:
#   模擬結果是「一個會隨時間變化的場」。靜態 PNG 只能給你一個時刻的切片,
#   看的人沒辦法問「那第 12 年的時候鋒面到哪裡」「這一格到底幾度」。
#   plotly 給的是:播放鍵 + 時間滑桿、滑鼠停在任一格讀出精確溫度、
#   框選放大。同樣一份資料,能回答的問題多一個量級。
#
# 為什麼要自己寫 inline:htmlwidgets 的 selfcontained = TRUE 需要 pandoc,
#   這台機器沒有。所以下面 gt_write_selfcontained() 自己把 lib/ 底下的
#   js/css 塞回 HTML —— 大約 30 行,換來「一個檔案就能寄出去」。
#
# 執行:  Rscript R/05_viz.R      (需要先跑過 03_heat.R 與 04_sweep.R)
# ==========================================================================

source("R/01_setup.R")
source("R/02_flow.R")

suppressPackageStartupMessages({
  library(plotly)        # 場圖與動畫(canvas,扛得住 16000 格 x 21 影格)
  library(highcharter)   # 歷線與設計圖(SVG,標註 API 好用、視覺漂亮)
  library(htmltools)
})

## ---- 色階 ---------------------------------------------------------------

#' 把 viridisLite 的調色盤轉成 plotly 要的 colorscale 格式
gt_colorscale <- function(pal) {
  n <- length(pal)
  lapply(seq_len(n), function(i) list((i - 1) / (n - 1), pal[i]))
}
GT_CS_TEMP <- gt_colorscale(viridisLite::inferno(32))
GT_CS_HEAD <- gt_colorscale(viridisLite::mako(32))
GT_CS_YEAR <- gt_colorscale(viridisLite::viridis(32))

## ---- 共用圖層 -----------------------------------------------------------

#' 矩陣 -> plotly 要的「每列一個 array」。直接丟 matrix 給 frames 會被
#' 序列化成一維陣列,圖就整個歪掉 —— 這是換 plotly 時最容易踩的坑。
gt_zrows <- function(M, digits = 1) {
  M <- round(M, digits)
  lapply(seq_len(nrow(M)), function(j) unname(M[j, ]))
}

#' 流線:全部串成「一條」有 NA 斷點的線,只用一個 trace(快很多)
gt_stream_trace <- function(p, sl, colour) {
  parts <- split(sl, sl$line)
  xs <- unlist(lapply(parts, function(d) c(d$x, NA)), use.names = FALSE)
  ys <- unlist(lapply(parts, function(d) c(d$y, NA)), use.names = FALSE)
  add_trace(p, x = xs, y = ys, type = "scatter", mode = "lines",
            line = list(color = colour, width = 1),
            hoverinfo = "skip", showlegend = FALSE, name = "流線")
}

gt_well_traces <- function(p, m) {
  yw <- m$y[m$j_wel]
  p |>
    add_trace(x = m$x[m$i_inj], y = yw, type = "scatter", mode = "markers+text",
              marker = list(symbol = "triangle-down", size = 13,
                            color = "#38bdf8",
                            line = list(color = "white", width = 1.5)),
              text = sprintf("注入 %g°C", m$p$T_inj), textposition = "top center",
              textfont = list(color = "white", size = 12),
              hovertemplate = "注入井<br>x %{x:.0f} m, y %{y:.0f} m<extra></extra>",
              showlegend = FALSE) |>
    add_trace(x = m$x[m$i_pro], y = yw, type = "scatter", mode = "markers+text",
              marker = list(symbol = "triangle-up", size = 13,
                            color = "#f43f5e",
                            line = list(color = "white", width = 1.5)),
              text = "生產", textposition = "top center",
              textfont = list(color = "white", size = 12),
              hovertemplate = "生產井<br>x %{x:.0f} m, y %{y:.0f} m<extra></extra>",
              showlegend = FALSE)
}

gt_field_layout <- function(p, m, title) {
  layout(p,
    title = list(text = title, x = 0, font = list(size = 16)),
    xaxis = list(title = "x (m)", constrain = "domain",
                 range = c(0, m$p$Lx), zeroline = FALSE),
    yaxis = list(title = "y (m)", scaleanchor = "x", scaleratio = 1,
                 range = c(0, m$p$Ly), zeroline = FALSE),
    margin = list(t = 46, l = 60, r = 20, b = 50)
  )
}

## ---- 圖 1:穩態水頭 + 流線 ----------------------------------------------

gt_plot_head <- function(m, fl, sl) {
  plot_ly(height = 660) |>
    add_trace(type = "heatmap", x = m$x, y = m$y, z = gt_zrows(fl$h, 3),
              colorscale = GT_CS_HEAD, zsmooth = "best",
              colorbar = list(title = list(text = "水頭\n(m)"), len = 0.8),
              hovertemplate = paste0(
                "x %{x:.0f} m, y %{y:.0f} m<br>",
                "<b>水頭 %{z:.3f} m</b><extra></extra>")) |>
    gt_stream_trace(sl, "rgba(255,255,255,0.55)") |>
    gt_well_traces(m) |>
    gt_field_layout(m, sprintf(
      "穩態水頭場與流線 —— 井距 %g m,抽注 %.0f L/s",
      m$L_actual, m$p$Q_well / 86.4))
}

## ---- 圖 2:溫度場動畫 ----------------------------------------------------

#' 帶播放鍵與時間滑桿的溫度場
#'
#' @param every 每隔幾張快照取一張當影格(快照是半年一張)
gt_plot_temperature <- function(m, ht, sl, every = 4L) {
  keep <- seq(1L, length(ht$snaps), by = every)
  if (utils::tail(keep, 1) != length(ht$snaps)) {
    keep <- c(keep, length(ht$snaps))
  }
  yrs <- ht$snap_t[keep] / DAYS_PER_YEAR

  ## 預設停在「最後一格」而不是 t = 0 —— t = 0 整場都是 130 degC,
  ## 打開報告只會看到一片空白。開檔先給結論,按 播放 再看它怎麼發生。
  k0 <- length(keep)

  p <- plot_ly(height = 700) |>
    add_trace(type = "heatmap", x = m$x, y = m$y,
              z = gt_zrows(ht$snaps[[keep[k0]]]),
              zmin = m$p$T_inj, zmax = m$p$T_res,
              colorscale = GT_CS_TEMP, zsmooth = "best",
              colorbar = list(title = list(text = "溫度\n(°C)"), len = 0.8),
              hovertemplate = paste0(
                "x %{x:.0f} m, y %{y:.0f} m<br>",
                "<b>%{z:.1f} °C</b><extra></extra>")) |>
    gt_stream_trace(sl, "rgba(255,255,255,0.35)") |>
    gt_well_traces(m) |>
    gt_field_layout(m, sprintf(
      "溫度場演化 —— 井距 %g m,抽注 %.0f L/s,熱遲滯 R = %.2f",
      m$L_actual, m$p$Q_well / 86.4, m$R_thermal))

  ## 影格:只更新第 0 號 trace(熱圖),流線與井位不動
  p$x$frames <- lapply(seq_along(keep), function(k) list(
    name   = sprintf("%.1f", yrs[k]),
    data   = list(list(z = gt_zrows(ht$snaps[[keep[k]]]))),
    traces = list(0L)
  ))

  anim <- function(name) list(list(name), list(
    mode = "immediate", frame = list(duration = 0, redraw = TRUE),
    transition = list(duration = 0)))

  layout(p,
    updatemenus = list(list(
      type = "buttons", direction = "left", showactive = FALSE,
      x = 0, xanchor = "left", y = -0.16, yanchor = "top", pad = list(t = 4),
      buttons = list(
        ## fromcurrent = FALSE:停在最後一格時按播放,要從頭重播
        list(label = "▶ 播放", method = "animate", args = list(NULL, list(
          mode = "immediate", fromcurrent = FALSE,
          frame = list(duration = 220, redraw = TRUE),
          transition = list(duration = 0)))),
        list(label = "⏸ 暫停", method = "animate", args = anim(NULL))
      ))),
    sliders = list(list(
      active = k0 - 1L, x = 0.12, len = 0.88, y = -0.14, yanchor = "top",
      pad = list(t = 4),
      currentvalue = list(prefix = "t = ", suffix = " 年",
                          font = list(size = 14)),
      steps = lapply(seq_along(keep), function(k) list(
        label = sprintf("%.0f", yrs[k]),
        method = "animate",
        args = anim(sprintf("%.1f", yrs[k]))
      ))
    )),
    margin = list(t = 46, l = 60, r = 20, b = 96)
  )
}

## ---- 圖 3:生產井歷線 (highcharter) --------------------------------------
##
## 圖 3 / 圖 4 的資料量很小(幾百點、28 格),用 Highcharts 畫比較好看,
## 而且 plotLines / plotBands / dataLabels 這類「標註」的 API 比 plotly 直覺。
## 場圖(圖 1、圖 2)則留給 plotly —— 16000 格 x 21 影格是 canvas 的活,
## Highcharts 的 SVG 扛不動。工具跟著題目走,不要跟著習慣走。

GT_HC_FONT <- "'Microsoft JhengHei','Segoe UI',system-ui,sans-serif"

gt_hc_base <- function(hc, height) {
  hc |>
    hc_chart(style = list(fontFamily = GT_HC_FONT), zoomType = "x") |>
    hc_size(height = height) |>
    hc_credits(enabled = TRUE)
}

#' 每張 highcharter 圖的最後一道手續
#'
#' highcharter 只要看到 chart.style.fontFamily,就會在瀏覽器端偷偷往
#' Google Fonts 發一個 request 去抓同名字型。兩個問題:
#'   1. 報告要能離線打開(而且我們花力氣做成 self-contained 就是為了這個)
#'   2. 字型名稱裡的引號會讓它組出語法錯誤的 jQuery selector,
#'      整個 highcharter binding 直接拋例外 —— 圖不會畫出來,只是空白。
#' 把 fonts 欄位清掉就好,本機字型照樣生效。
gt_hc_finish <- function(hc) {
  hc$x$fonts <- NULL
  hc
}

#' 數值向量對 -> Highcharts 的 [[x, y], ...]
gt_xy <- function(x, y) Map(function(a, b) c(a, b), x, y)

gt_plot_history <- function(m, ht) {
  h    <- ht$hist
  crit <- m$p$T_res - m$p$T_drop_crit
  bt   <- gt_breakthrough(h$t_day, h$Temp_pro, m$p$T_res, m$p$T_drop_crit)
  gs   <- m$t_heat_gs / DAYS_PER_YEAR

  ## 歷線每一步都有記錄,幾千點對圖沒有幫助 —— 抽稀到約 600 點
  k <- unique(round(seq(1, nrow(h), length.out = min(nrow(h), 600))))
  d <- h[k, ]

  hc <- highchart() |>
    gt_hc_base(420) |>
    hc_title(text = sprintf("生產井溫度歷線 —— 熱突破 %s",
               if (is.na(bt)) sprintf("未在 %g 年內發生", m$p$t_end_yr)
               else sprintf("%.1f 年", bt / DAYS_PER_YEAR)),
             align = "left", style = list(fontSize = "16px",
                                          fontWeight = "bold")) |>
    hc_xAxis(
      title = list(text = "時間 (年)"), min = 0, max = m$p$t_end_yr,
      plotLines = list(list(
        value = gs, color = "#64748b", dashStyle = "Dot", width = 1.5,
        zIndex = 3,
        label = list(text = sprintf("解析解 %.1f 年", gs), rotation = 0,
                     y = 14, x = 4,
                     style = list(color = "#475569", fontSize = "12px"))))) |>
    hc_yAxis(
      title = list(text = "生產溫度 (°C)"),
      plotBands = list(list(from = -1e6, to = crit,
                            color = "rgba(220,38,38,0.06)")),
      plotLines = list(list(
        value = crit, color = "#dc2626", dashStyle = "Dash", width = 1.5,
        zIndex = 3,
        label = list(text = sprintf("熱突破判準 %g °C", crit), x = 8,
                     style = list(color = "#dc2626", fontSize = "12px"))))) |>
    hc_add_series(
      name = "生產溫度", type = "line", color = "#1d4ed8", lineWidth = 2.4,
      marker = list(enabled = FALSE),
      data = gt_xy(round(d$t_yr, 3), round(d$Temp_pro, 3))) |>
    hc_tooltip(headerFormat = "", shared = FALSE, borderWidth = 0,
               pointFormat = paste0(
                 "t = <b>{point.x:.1f}</b> 年<br>",
                 "生產溫度 <b>{point.y:.2f} °C</b>")) |>
    hc_legend(enabled = FALSE)

  if (!is.na(bt)) {
    hc <- hc_add_series(hc, name = "熱突破", type = "scatter",
      color = "#dc2626", marker = list(radius = 6, symbol = "circle"),
      data = gt_xy(round(bt / DAYS_PER_YEAR, 2), crit),
      tooltip = list(headerFormat = "",
        pointFormat = sprintf("<b>熱突破 %.1f 年</b>", bt / DAYS_PER_YEAR)))
  }
  gt_hc_finish(hc)
}

## ---- 圖 4:設計圖 (highcharter heatmap) ---------------------------------

gt_plot_design <- function(sweep, need30, years_max = 60) {
  Ls <- sort(unique(sweep$L_doublet))
  Qs <- sort(unique(sweep$Q_Ls))
  dL <- if (length(Ls) > 1L) min(diff(Ls)) else 200
  dQ <- if (length(Qs) > 1L) min(diff(Qs)) else 25

  dat <- lapply(seq_len(nrow(sweep)), function(r) {
    v <- sweep$bt_yr[r]
    capped <- is.na(v) || v > years_max
    list(x     = sweep$L_doublet[r],
         y     = sweep$Q_Ls[r],
         value = if (capped) years_max else round(v, 1),
         lab   = if (capped) sprintf("> %g", years_max) else sprintf("%.0f", v))
  })

  ok <- need30[is.finite(need30$L_need_30yr), ]

  hc <- highchart() |>
    gt_hc_base(460) |>
    hc_chart(type = "heatmap", zoomType = NULL) |>
    hc_add_dependency("modules/coloraxis.js") |>
    hc_add_dependency("modules/heatmap.js") |>
    hc_title(text = "設計圖:井距 × 抽注率 → 熱突破年數",
             align = "left",
             style = list(fontSize = "16px", fontWeight = "bold")) |>
    hc_subtitle(text = "青色線 = 撐滿 30 年所需的最小井距", align = "left",
                style = list(color = "#64748b")) |>
    hc_xAxis(title = list(text = "井距 (m)"), tickPositions = Ls,
             min = min(Ls) - dL / 2, max = max(Ls) + dL / 2) |>
    hc_yAxis(title = list(text = "抽注率 (L/s)"), tickPositions = Qs,
             min = min(Qs) - dQ / 2, max = max(Qs) + dQ / 2) |>
    hc_colorAxis(min = 0, max = years_max,
                 stops = color_stops(12, viridisLite::viridis(12)),
                 labels = list(format = "{value} 年")) |>
    hc_add_series(
      name = "熱突破", type = "heatmap", data = dat,
      colsize = dL, rowsize = dQ,
      borderWidth = 2, borderColor = "#ffffff",
      dataLabels = list(enabled = TRUE, format = "{point.lab}",
                        color = "#ffffff", style = list(
                          textOutline = "none", fontWeight = "bold",
                          fontSize = "13px")),
      tooltip = list(headerFormat = "",
        pointFormat = paste0(
          "井距 <b>{point.x} m</b>,抽注 <b>{point.y} L/s</b><br>",
          "熱突破 <b>{point.lab} 年</b>")))

  if (nrow(ok) > 0L) {
    hc <- hc_add_series(hc,
      name = "30 年門檻", type = "spline", colorAxis = FALSE,
      color = "#0891b2", lineWidth = 3,
      marker = list(enabled = TRUE, radius = 5, symbol = "circle",
                    fillColor = "#22d3ee", lineColor = "#0e7490",
                    lineWidth = 2),
      data = gt_xy(round(ok$L_need_30yr), ok$Q_Ls),
      tooltip = list(headerFormat = "",
        pointFormat = paste0("抽注 <b>{point.y} L/s</b><br>",
                             "撐 30 年需要井距 <b>{point.x} m</b>")))
  }
  gt_hc_finish(hc_legend(hc, enabled = FALSE))
}

## ---- 自己做 self-contained HTML(不需要 pandoc)------------------------

gt_write_selfcontained <- function(tags, file) {
  work <- file.path(tempdir(), "gt_html")
  unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
  tmp <- file.path(work, "index.html")
  htmltools::save_html(tags, tmp, libdir = "lib")

  txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  ## 一次掃完所有 match 再重組,不要邊改邊重掃 —— 內嵌進去的 js 內容
  ## 本身可能又長得像一個 <script src=...> 標籤。
  inline <- function(txt, rx, make) {
    g <- gregexpr(rx, txt, perl = TRUE)[[1]]
    if (g[1] == -1L) return(txt)
    st <- as.integer(g); ln <- attr(g, "match.length")
    out <- character(0); pos <- 1L
    for (k in seq_along(st)) {
      out <- c(out, substr(txt, pos, st[k] - 1L),
               make(substring(txt, st[k], st[k] + ln[k] - 1L)))
      pos <- st[k] + ln[k]
    }
    paste0(paste(out, collapse = ""), substr(txt, pos, nchar(txt)))
  }
  slurp <- function(rel) paste(
    readLines(file.path(work, rel), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n")

  txt <- inline(txt, '<script[^>]*\\bsrc="lib/[^"]+"[^>]*>\\s*</script>',
    function(tag) {
      js <- slurp(sub('.*\\bsrc="(lib/[^"]+)".*', "\\1", tag))
      ## js 字面值裡若出現 </script 會提早關掉標籤
      paste0("<script>", gsub("</script", "<\\\\/script", js, fixed = TRUE),
             "</script>")
    })
  txt <- inline(txt, '<link[^>]*\\bhref="lib/[^"]+\\.css"[^>]*/?>',
    function(tag) paste0("<style>",
      slurp(sub('.*\\bhref="(lib/[^"]+)".*', "\\1", tag)), "</style>"))

  con <- file(file, open = "wb"); on.exit(close(con))
  writeBin(charToRaw(enc2utf8(txt)), con)
  invisible(file)
}

## ---- 報告版面 -----------------------------------------------------------

GT_CSS <- "
  body { margin:0; background:#f8fafc; color:#0f172a;
         font-family:'Microsoft JhengHei','Segoe UI',system-ui,sans-serif; }
  .wrap { max-width:1080px; margin:0 auto; padding:32px 24px 80px; }
  h1 { font-size:26px; margin:0 0 4px; }
  h2 { font-size:18px; margin:40px 0 4px; padding-top:16px;
       border-top:1px solid #e2e8f0; }
  .sub { color:#64748b; margin:0 0 24px; font-size:14px; }
  p.note { color:#475569; font-size:14px; line-height:1.7; margin:6px 0 14px; }
  .kpi { display:flex; flex-wrap:wrap; gap:10px; margin:18px 0 8px; }
  .kpi div { background:#fff; border:1px solid #e2e8f0; border-radius:10px;
             padding:10px 14px; min-width:132px; }
  .kpi b { display:block; font-size:19px; margin-top:2px; }
  .kpi span { font-size:12px; color:#64748b; }
  .card { background:#fff; border:1px solid #e2e8f0; border-radius:12px;
          padding:8px; margin:12px 0; }
  code { background:#e2e8f0; padding:1px 5px; border-radius:4px; font-size:13px; }
"

gt_kpi <- function(...) {
  items <- list(...)
  div(class = "kpi", lapply(items, function(it)
    div(tags$span(it[[1]]), tags$b(it[[2]]))))
}

gt_report <- function(m, fl, ht, sweep = NULL, need30 = NULL) {
  sl <- gt_streamlines(m, fl)
  bt <- gt_breakthrough(ht$hist$t_day, ht$hist$Temp_pro,
                        m$p$T_res, m$p$T_drop_crit)

  need_txt <- if (is.null(need30)) "—" else {
    ok <- need30[is.finite(need30$L_need_30yr), ]
    if (nrow(ok) == 0L) "掃描範圍內都不夠" else
      paste(sprintf("%.0f L/s → %.0f m", ok$Q_Ls, ok$L_need_30yr),
            collapse = " ・ ")
  }

  tagList(
    tags$head(tags$meta(charset = "utf-8"),
              tags$title("地熱抽注井對熱突破模擬"),
              tags$style(HTML(GT_CSS))),
    div(class = "wrap",
      h1("地熱抽注井對 —— 熱突破模擬"),
      p(class = "sub", sprintf(
        "合成參數的 2D 數值實驗 ・ 網格 %d × %d @ %g m ・ 產生於 %s",
        m$ny, m$nx, m$p$dx, format(Sys.Date()))),

      gt_kpi(
        list("熱遲滯因子 R", sprintf("%.2f", m$R_thermal)),
        list("熱突破 (−2°C)", if (is.na(bt)) sprintf("> %g 年", m$p$t_end_yr)
                              else sprintf("%.1f 年", bt / DAYS_PER_YEAR)),
        list("解析解參考", sprintf("%.1f 年", m$t_heat_gs / DAYS_PER_YEAR)),
        list(sprintf("%g 年後生產溫度", m$p$t_end_yr),
             sprintf("%.1f °C", utils::tail(ht$hist$Temp_pro, 1))),
        list("能量守恆誤差", sprintf("%.1e", ht$energy_err_rel))
      ),
      p(class = "note", HTML(sprintf(
        "熱鋒面比水慢 <b>%.2f 倍</b>(R = C<sub>bulk</sub> / (φ·C<sub>water</sub>))。
         這是這整個模擬最重要的一個數字:示蹤劑打穿 <b>不等於</b> 熱打穿。
         忘了它,井距會少估到只剩五分之一,而圖看起來完全正常。",
        m$R_thermal))),

      h2("1. 穩態流場"),
      p(class = "note", "五點差分 + 紅黑 SOR 解 Laplace 方程,兩口井是點源匯,
        左右為定水頭(區域水力坡降),上下不透水。流線是從注入井放出的水質點
        路徑。滑鼠停在任一格可以讀出水頭。"),
      div(class = "card", gt_plot_head(m, fl, sl)),

      h2("2. 溫度場演化"),
      p(class = "note", HTML("按 <b>▶ 播放</b>,或直接拖時間滑桿。白色區域是
        還沒被影響的儲層,黑色是冷水。注意冷水團是<b>沿著流線被拉長</b>的,
        不是同心圓 —— 這就是為什麼「兩井距離」不等於「熱走的距離」。")),
      div(class = "card", gt_plot_temperature(m, ht, sl)),

      h2("3. 生產井溫度歷線"),
      p(class = "note", "紅色虛線是熱突破判準。灰色點線是 Gringarten & Sauty
        的銳利鋒面解析解 —— 模擬值比它早到,因為熱延散把鋒面抹開了。
        這是物理,不是數值誤差。"),
      div(class = "card", gt_plot_history(m, ht)),

      if (!is.null(sweep)) tagList(
        h2("4. 設計圖:要撐 30 年,井距要開多大"),
        p(class = "note", HTML(sprintf(
          "亮青色線就是這個 repo 的<b>工程結論</b>:%s。<br>
           熱突破時間大致正比於 井距² / 抽注率 —— 想加大產能,井距要
           跟著開根號放大。掃描用較粗的網格(dx = 40 m),數值延散偏大,
           所以年數是<b>保守</b>的。", need_txt))),
        div(class = "card", gt_plot_design(sweep, need30))
      ),

      h2("5. 這份結果憑什麼可信"),
      tags$ul(class = "note",
        tags$li(HTML(sprintf(
          "<b>質量守恆</b>:每一格的流量殘差 &lt; %.1e m³/day,相對抽注率
           %.0e。", fl$resid_max, fl$resid_max / m$p$Q_well))),
        tags$li(HTML(sprintf(
          "<b>能量守恆</b>:總能量變化 vs 井與邊界的累積淨輸入,相對誤差
           %.1e(機器精度)。", ht$energy_err_rel))),
        tags$li(HTML(sprintf(
          "<b>穩定條件</b>:dt = %.3f 天,離散式自己算出來的上限是 %.3f 天。",
          ht$dt, ht$dt_max))),
        tags$li(HTML("<b>熱遲滯因子</b>:把同一個流場拿去跑保守示蹤劑,
           量到的 t50 比值必須等於理論 R —— 見 <code>tests/</code>。")),
        tags$li(HTML("跑 <code>Rscript tests/run_tests.R</code> 可以把
           上面每一條重新驗一次。"))
      )
    )
  )
}

## ---- 以 script 執行時才跑 ----------------------------------------------

gt_run_viz <- function() {
  load(gt_out("03_heat.RData"))            # 帶回 m, fl, ht

  sweep <- NULL; need30 <- NULL
  sweep_file <- gt_out("04_sweep.RData")
  if (file.exists(sweep_file)) {
    load(sweep_file)                       # 帶回 sweep, need30
  } else {
    message("找不到 ", sweep_file, ",報告會少掉設計圖(先跑 R/04_sweep.R)")
  }

  cat("組報告 ...\n")
  page <- gt_report(m, fl, ht, sweep, need30)
  out  <- gt_out("report.html")
  gt_write_selfcontained(page, out)
  cat(sprintf("已寫出 %s  (%.1f MB,單一檔案,可直接寄出)\n",
              out, file.size(out) / 1024^2))
  invisible(out)
}

if (sys.nframe() == 0L) gt_run_viz()
