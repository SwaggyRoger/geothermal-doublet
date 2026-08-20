#!/usr/bin/env Rscript
# ==========================================================================
# 00_intro.R — 產生專案介紹頁  outputs/intro.html
#
# 這是 demo 的第一份投影材料:在跳進 R 之前,先把「這個案子在算什麼」
# 講清楚 —— 假設場景、初始/邊界條件、控制方程式、每支腳本做什麼、
# 執行時畫面會出現什麼、最後產出什麼。
#
# 關鍵設計:參數表是「從 params/params.csv 現讀現生」的,不是手打進 HTML 的。
# 所以參數一改,介紹頁自動跟著對。投影片與程式不同步是所有 demo 的死因。
#
# 執行:  Rscript R/00_intro.R      (約 10 秒,不需要先跑過任何東西)
# ==========================================================================

source("R/01_setup.R")
source("R/05_viz.R")     # 借用 gt_write_selfcontained() 與版面 CSS

## ---- 參數表的中文說明層 -------------------------------------------------
## params.csv 的欄位維持英文(程式讀的),中文只是呈現。找不到對應就退回
## csv 裡的 description,所以漏掉一個也不會壞。

GT_PARAM_ZH <- list(
  Lx = c("模型域長度（x，區域流方向）", "網格haha"),
  Ly = c("模型域寬度（y）", "網格"),
  dx = c("網格間距（dx = dy）", "網格"),
  b  = c("含水層厚度", "含水層"),
  K  = c("水力傳導度", "含水層"),
  phi = c("有效孔隙率", "含水層"),
  grad_regional = c("區域水力坡降（左→右）", "邊界條件"),
  h_ref = c("左側邊界的定水頭", "邊界條件"),
  Q_well = c("注入率 = 生產率", "井"),
  L_doublet = c("注入井與生產井的距離", "井"),
  T_res = c("儲層初始溫度", "初始條件"),
  T_inj = c("回注水溫度", "井"),
  rho_c_w = c("水的體積熱容 (ρc)w", "熱物性"),
  rho_c_s = c("岩石基質的體積熱容 (ρc)s", "熱物性"),
  lambda_bulk = c("岩水混合體的熱傳導度（2.5 W/m/K）", "熱物性"),
  alpha_L = c("縱向熱延散度", "熱物性"),
  t_end_yr = c("模擬期間", "數值設定"),
  dt_safety = c("時間步長取穩定上限的幾成", "數值設定"),
  snap_per_yr = c("每年儲存幾張溫度場快照", "數值設定"),
  T_drop_crit = c("判定熱突破的生產溫度降幅", "判準")
)

GT_PARAM_ORDER <- c("網格", "含水層", "井", "初始條件", "邊界條件",
                    "熱物性", "數值設定", "判準")

gt_param_table <- function(file = GT_PARAMS_FILE) {
  tab <- utils::read.csv(file, stringsAsFactors = FALSE)
  zh  <- vapply(tab$name, function(n) {
    if (!is.null(GT_PARAM_ZH[[n]])) GT_PARAM_ZH[[n]][1] else ""
  }, character(1))
  grp <- vapply(tab$name, function(n) {
    if (!is.null(GT_PARAM_ZH[[n]])) GT_PARAM_ZH[[n]][2] else "其他"
  }, character(1))
  tab$zh <- ifelse(nzchar(zh), zh, tab$description)
  tab$grp <- factor(grp, levels = c(GT_PARAM_ORDER, "其他"))
  tab[order(tab$grp), ]
}

gt_param_html <- function() {
  d <- gt_param_table()
  rows <- list()
  for (g in levels(d$grp)) {
    sub <- d[d$grp == g, ]
    if (nrow(sub) == 0L) next
    rows[[length(rows) + 1L]] <- tags$tr(class = "grp",
      tags$td(colspan = 4, g))
    for (i in seq_len(nrow(sub))) {
      rows[[length(rows) + 1L]] <- tags$tr(
        tags$td(tags$code(sub$name[i])),
        tags$td(sub$zh[i]),
        tags$td(class = "num", format(sub$value[i], big.mark = ",",
                                      scientific = FALSE, trim = TRUE)),
        tags$td(class = "unit", ifelse(sub$unit[i] == "-", "—", sub$unit[i])))
    }
  }
  tags$table(class = "params",
    tags$thead(tags$tr(tags$th("參數"), tags$th("意義"),
                       tags$th(class = "num", "值"), tags$th("單位"))),
    tags$tbody(rows))
}

## ---- 圖 A:模型域示意（照比例畫）---------------------------------------

gt_intro_domain <- function(m) {
  p <- m$p
  yw <- m$y[m$j_wel]
  gl <- seq(0, p$Lx, by = 400)          # 每 400 m 一條參考線 = 20 格
  gr <- seq(0, p$Ly, by = 400)
  gx <- unlist(lapply(gl, function(v) c(v, v, NA)))
  gy <- unlist(lapply(gl, function(v) c(0, p$Ly, NA)))
  gx2 <- unlist(lapply(gr, function(v) c(0, p$Lx, NA)))
  gy2 <- unlist(lapply(gr, function(v) c(v, v, NA)))

  plot_ly(height = 460) |>
    add_trace(x = c(0, p$Lx, p$Lx, 0, 0), y = c(0, 0, p$Ly, p$Ly, 0),
              type = "scatter", mode = "lines", fill = "toself",
              fillcolor = "rgba(254,243,199,0.75)",
              line = list(color = "#a16207", width = 2),
              hoverinfo = "skip", showlegend = FALSE) |>
    add_trace(x = c(gx, gx2), y = c(gy, gy2), type = "scatter",
              mode = "lines", line = list(color = "rgba(161,98,7,0.22)",
                                          width = 1),
              hoverinfo = "skip", showlegend = FALSE) |>
    add_trace(x = c(0, 0), y = c(0, p$Ly), type = "scatter", mode = "lines",
              line = list(color = "#1d4ed8", width = 6),
              hovertemplate = sprintf("定水頭 h = %.2f m<extra></extra>",
                                      p$h_ref),
              showlegend = FALSE) |>
    add_trace(x = c(p$Lx, p$Lx), y = c(0, p$Ly), type = "scatter",
              mode = "lines", line = list(color = "#1d4ed8", width = 6),
              hovertemplate = sprintf("定水頭 h = %.2f m<extra></extra>",
                                      p$h_ref - p$grad_regional * p$Lx),
              showlegend = FALSE) |>
    add_trace(x = c(0, p$Lx), y = c(0, 0), type = "scatter", mode = "lines",
              line = list(color = "#475569", width = 6, dash = "solid"),
              hovertemplate = "不透水邊界 (no-flow)<extra></extra>",
              showlegend = FALSE) |>
    add_trace(x = c(0, p$Lx), y = c(p$Ly, p$Ly), type = "scatter",
              mode = "lines", line = list(color = "#475569", width = 6),
              hovertemplate = "不透水邊界 (no-flow)<extra></extra>",
              showlegend = FALSE) |>
    add_trace(x = m$x[m$i_inj], y = yw, type = "scatter", mode = "markers",
              marker = list(symbol = "triangle-down", size = 16,
                            color = "#38bdf8",
                            line = list(color = "#0c4a6e", width = 2)),
              hovertemplate = sprintf(
                "注入井<br>%g °C, %g m³/day<extra></extra>",
                p$T_inj, p$Q_well), showlegend = FALSE) |>
    add_trace(x = m$x[m$i_pro], y = yw, type = "scatter", mode = "markers",
              marker = list(symbol = "triangle-up", size = 16,
                            color = "#f43f5e",
                            line = list(color = "#881337", width = 2)),
              hovertemplate = sprintf("生產井<br>%g m³/day<extra></extra>",
                                      p$Q_well), showlegend = FALSE) |>
    add_trace(x = c(m$x[m$i_inj], m$x[m$i_pro]), y = c(yw, yw),
              type = "scatter", mode = "lines",
              line = list(color = "#0f172a", width = 1.5, dash = "dot"),
              hoverinfo = "skip", showlegend = FALSE) |>
    layout(
      title = list(text = "模型域、初始條件與邊界條件", x = 0,
                   font = list(size = 16)),
      xaxis = list(title = "x (m)", range = c(-90, p$Lx + 90),
                   zeroline = FALSE, constrain = "domain"),
      yaxis = list(title = "y (m)", range = c(-150, p$Ly + 240),
                   zeroline = FALSE, scaleanchor = "x", scaleratio = 1),
      annotations = list(
        ## 兩側的定水頭標註畫在「域內」而不是域外 —— 放外面會直接壓在
        ## y 軸的刻度數字與軸標題上。箭頭指回邊界,語意一樣清楚。
        list(x = 0, y = p$Ly / 2, ax = 78, ay = 0, text = sprintf(
               "定水頭<br>h = %.2f m", p$h_ref),
             font = list(color = "#1d4ed8", size = 11), showarrow = TRUE,
             arrowhead = 0, arrowcolor = "#1d4ed8", xanchor = "left",
             bgcolor = "rgba(255,255,255,0.92)", bordercolor = "#1d4ed8",
             borderwidth = 1, borderpad = 3),
        list(x = p$Lx, y = p$Ly / 2, ax = -78, ay = 0, text = sprintf(
               "定水頭<br>h = %.2f m", p$h_ref - p$grad_regional * p$Lx),
             font = list(color = "#1d4ed8", size = 11), showarrow = TRUE,
             arrowhead = 0, arrowcolor = "#1d4ed8", xanchor = "right",
             bgcolor = "rgba(255,255,255,0.92)", bordercolor = "#1d4ed8",
             borderwidth = 1, borderpad = 3),
        list(x = p$Lx / 2, y = p$Ly, ay = -30, ax = 0,
             text = "不透水邊界（no-flow）", showarrow = FALSE,
             yanchor = "bottom", font = list(color = "#475569", size = 11)),
        list(x = p$Lx / 2, y = 0, ay = 0, ax = 0,
             text = "不透水邊界（no-flow）", showarrow = FALSE,
             yanchor = "top", font = list(color = "#475569", size = 11)),
        list(x = m$x[m$i_inj], y = yw, ax = 0, ay = -46,
             text = sprintf("注入井 %g °C", p$T_inj), showarrow = TRUE,
             arrowhead = 0, arrowcolor = "#0c4a6e",
             font = list(color = "#ffffff", size = 11),
             bgcolor = "rgba(12,74,110,0.9)", borderpad = 4),
        list(x = m$x[m$i_pro], y = yw, ax = 0, ay = -46,
             text = "生產井", showarrow = TRUE, arrowhead = 0,
             arrowcolor = "#881337", font = list(color = "#ffffff", size = 11),
             bgcolor = "rgba(136,19,55,0.9)", borderpad = 4),
        list(x = (m$x[m$i_inj] + m$x[m$i_pro]) / 2, y = yw, ay = 26, ax = 0,
             text = sprintf("井距 L = %g m", m$L_actual), showarrow = FALSE,
             yanchor = "top", font = list(color = "#0f172a", size = 11),
             bgcolor = "rgba(255,255,255,0.85)", borderpad = 2),
        list(x = 240, y = p$Ly - 170, text = sprintf(
               "初始條件<br>全場 T = %g °C", p$T_res),
             showarrow = FALSE, xanchor = "left",
             font = list(color = "#92400e", size = 12),
             bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#d97706",
             borderwidth = 1, borderpad = 5),
        list(x = 240, y = 240, text = "水平、均質、侷限含水層",
             showarrow = FALSE, xanchor = "left",
             font = list(color = "#a16207", size = 11))
      ),
      ## 左邊界要留給「2000」這種四位數刻度 + 直立的軸標題,60 會撞在一起
      margin = list(t = 46, l = 86, r = 20, b = 40)
    )
}

## ---- 圖 B:五點差分模板（inline SVG）------------------------------------

GT_SVG_STENCIL <- '
<svg viewBox="0 0 360 250" style="width:100%;max-width:360px;height:auto;
     font-family:inherit" role="img" aria-label="五點差分模板">
  <defs><marker id="ah" markerWidth="7" markerHeight="7" refX="6" refY="3.5"
    orient="auto"><path d="M0,0 L7,3.5 L0,7 z" fill="#94a3b8"/></marker></defs>
  <g stroke="#cbd5e1" stroke-width="1">
    <line x1="20" y1="60"  x2="340" y2="60"/>
    <line x1="20" y1="120" x2="340" y2="120"/>
    <line x1="20" y1="180" x2="340" y2="180"/>
    <line x1="60"  y1="10" x2="60"  y2="215"/>
    <line x1="120" y1="10" x2="120" y2="215"/>
    <line x1="180" y1="10" x2="180" y2="215"/>
    <line x1="240" y1="10" x2="240" y2="215"/>
    <line x1="300" y1="10" x2="300" y2="215"/>
  </g>
  <g stroke="#94a3b8" stroke-width="1.6" marker-end="url(#ah)">
    <line x1="180" y1="76"  x2="180" y2="118"/>
    <line x1="180" y1="164" x2="180" y2="122"/>
    <line x1="136" y1="120" x2="178" y2="120"/>
    <line x1="224" y1="120" x2="182" y2="120"/>
  </g>
  <g>
    <circle cx="180" cy="120" r="19" fill="#1d4ed8"/>
    <circle cx="180" cy="60"  r="15" fill="#bfdbfe" stroke="#60a5fa"/>
    <circle cx="180" cy="180" r="15" fill="#bfdbfe" stroke="#60a5fa"/>
    <circle cx="120" cy="120" r="15" fill="#bfdbfe" stroke="#60a5fa"/>
    <circle cx="240" cy="120" r="15" fill="#bfdbfe" stroke="#60a5fa"/>
  </g>
  <text x="180" y="125" fill="#ffffff" font-size="14" font-weight="bold"
        text-anchor="middle">P</text>
  <g fill="#1e3a8a" font-size="13" text-anchor="middle" font-weight="bold">
    <text x="180" y="65">N</text><text x="180" y="185">S</text>
    <text x="120" y="125">W</text><text x="240" y="125">E</text>
  </g>
  <text x="20" y="238" fill="#94a3b8" font-size="11">
    一格 = dx × dx × b,四個鄰格決定它</text>
</svg>'

## ---- 圖 C:腳本流程（inline SVG）----------------------------------------

GT_SVG_PIPELINE <- '
<svg viewBox="0 0 980 250" style="width:100%;min-width:760px;height:auto;
     font-family:inherit" role="img" aria-label="腳本執行流程">
  <defs><marker id="pa" markerWidth="8" markerHeight="8" refX="7" refY="4"
    orient="auto"><path d="M0,0 L8,4 L0,8 z" fill="#64748b"/></marker></defs>
  <g font-size="12">
    <g>
      <rect x="8" y="60" width="150" height="62" rx="9" fill="#f1f5f9"
            stroke="#cbd5e1"/>
      <text x="83" y="84" text-anchor="middle" fill="#0f172a"
            font-weight="bold">params.csv</text>
      <text x="83" y="104" text-anchor="middle" fill="#64748b"
            font-size="11">20 個物理參數</text>
    </g>
    <line x1="162" y1="91" x2="188" y2="91" stroke="#64748b"
          stroke-width="1.6" marker-end="url(#pa)"/>
    <g>
      <rect x="192" y="52" width="150" height="78" rx="9" fill="#eff6ff"
            stroke="#93c5fd"/>
      <text x="267" y="76" text-anchor="middle" fill="#1e3a8a"
            font-weight="bold">01_setup.R</text>
      <text x="267" y="95" text-anchor="middle" fill="#475569"
            font-size="11">網格・IC/BC・R</text>
      <text x="267" y="113" text-anchor="middle" fill="#94a3b8"
            font-size="10">只定義,不運算</text>
    </g>
    <line x1="346" y1="91" x2="372" y2="91" stroke="#64748b"
          stroke-width="1.6" marker-end="url(#pa)"/>
    <g>
      <rect x="376" y="52" width="140" height="78" rx="9" fill="#eff6ff"
            stroke="#93c5fd"/>
      <text x="446" y="76" text-anchor="middle" fill="#1e3a8a"
            font-weight="bold">02_flow.R</text>
      <text x="446" y="95" text-anchor="middle" fill="#475569"
            font-size="11">穩態水頭 + 流速</text>
      <text x="446" y="113" text-anchor="middle" fill="#94a3b8"
            font-size="10">約 8 秒</text>
    </g>
    <line x1="520" y1="91" x2="546" y2="91" stroke="#64748b"
          stroke-width="1.6" marker-end="url(#pa)"/>
    <g>
      <rect x="550" y="52" width="140" height="78" rx="9" fill="#eff6ff"
            stroke="#93c5fd"/>
      <text x="620" y="76" text-anchor="middle" fill="#1e3a8a"
            font-weight="bold">03_heat.R</text>
      <text x="620" y="95" text-anchor="middle" fill="#475569"
            font-size="11">溫度演化 40 年</text>
      <text x="620" y="113" text-anchor="middle" fill="#94a3b8"
            font-size="10">約 9 秒</text>
    </g>
    <line x1="694" y1="91" x2="720" y2="91" stroke="#64748b"
          stroke-width="1.6" marker-end="url(#pa)"/>
    <g>
      <rect x="724" y="52" width="140" height="78" rx="9" fill="#eff6ff"
            stroke="#93c5fd"/>
      <text x="794" y="76" text-anchor="middle" fill="#1e3a8a"
            font-weight="bold">04_sweep.R</text>
      <text x="794" y="95" text-anchor="middle" fill="#475569"
            font-size="11">28 次模擬</text>
      <text x="794" y="113" text-anchor="middle" fill="#94a3b8"
            font-size="10">約 85 秒</text>
    </g>
    <path d="M794 134 L794 168 L560 168 L560 186" stroke="#64748b"
          stroke-width="1.6" fill="none" marker-end="url(#pa)"/>
    <path d="M620 134 L620 158 L540 158 L540 186" stroke="#64748b"
          stroke-width="1.6" fill="none" marker-end="url(#pa)"/>
    <g>
      <rect x="452" y="188" width="180" height="52" rx="9" fill="#ecfdf5"
            stroke="#6ee7b7"/>
      <text x="542" y="210" text-anchor="middle" fill="#065f46"
            font-weight="bold">05_viz.R</text>
      <text x="542" y="228" text-anchor="middle" fill="#475569"
            font-size="11">互動報告・約 35 秒</text>
    </g>
    <line x1="636" y1="214" x2="662" y2="214" stroke="#64748b"
          stroke-width="1.6" marker-end="url(#pa)"/>
    <g>
      <rect x="666" y="188" width="198" height="52" rx="9" fill="#fef3c7"
            stroke="#fcd34d"/>
      <text x="765" y="210" text-anchor="middle" fill="#92400e"
            font-weight="bold">report.html</text>
      <text x="765" y="228" text-anchor="middle" fill="#78350f"
            font-size="11">單一檔案・可離線・可寄出</text>
    </g>
    <g>
      <rect x="8" y="188" width="330" height="52" rx="9" fill="#fef2f2"
            stroke="#fecaca"/>
      <text x="30" y="210" fill="#991b1b" font-weight="bold">tests/</text>
      <text x="30" y="228" fill="#7f1d1d" font-size="11">
        5 支・60 條斷言・約 25 秒・全綠才算數</text>
    </g>
    <text x="10" y="176" fill="#94a3b8" font-size="10">
      每一支把結果存成 outputs/0X_*.RData,下一支直接讀 —— 所以可以只重跑後面某一段</text>
  </g>
</svg>'

## ---- 版面 ---------------------------------------------------------------

GT_INTRO_CSS <- paste0(GT_CSS, "
  h3 { font-size:15px; margin:24px 0 6px; color:#0f172a; }
  .eq { background:#0f172a; color:#e2e8f0; border-radius:10px;
        padding:14px 18px; margin:10px 0; overflow-x:auto;
        font-family:Consolas,'Courier New',monospace; font-size:14px;
        line-height:1.9; white-space:pre; }
  .eq .c { color:#64748b; }
  .eq .k { color:#7dd3fc; }
  table.params, table.files { border-collapse:collapse; width:100%;
        font-size:13.5px; margin:12px 0; background:#fff;
        border:1px solid #e2e8f0; border-radius:10px; overflow:hidden; }
  table.params th, table.files th { background:#f1f5f9; text-align:left;
        padding:8px 12px; font-size:12.5px; color:#475569; }
  table.params td, table.files td { padding:7px 12px;
        border-top:1px solid #f1f5f9; vertical-align:top; }
  table.params td.num { text-align:right; font-family:Consolas,monospace; }
  table.params td.unit { color:#64748b; font-size:12.5px; }
  tr.grp td { background:#f8fafc; font-weight:bold; color:#0f172a;
        font-size:12.5px; padding-top:10px; }
  pre.term { background:#0f172a; color:#cbd5e1; border-radius:10px;
        padding:14px 18px; overflow-x:auto; font-size:12.5px;
        line-height:1.65; font-family:Consolas,'Courier New',monospace; }
  pre.term b { color:#4ade80; font-weight:normal; }
  pre.term i { color:#facc15; font-style:normal; }
  .scroll { overflow-x:auto; }
  .two { display:flex; gap:18px; flex-wrap:wrap; align-items:flex-start; }
  .two > * { flex:1 1 320px; min-width:0; }
  .callout { border-left:4px solid #0ea5e9; background:#f0f9ff;
        padding:12px 16px; border-radius:0 8px 8px 0; margin:14px 0;
        font-size:14px; line-height:1.75; color:#0c4a6e; }
  .warn { border-left-color:#f59e0b; background:#fffbeb; color:#78350f; }
")

gt_intro_page <- function(m) {
  p <- m$p
  tagList(
    tags$head(tags$meta(charset = "utf-8"),
              tags$title("地熱抽注井對模擬 — 專案說明"),
              tags$style(HTML(GT_INTRO_CSS))),
    div(class = "wrap",

      h1("地熱抽注井對 — 這個案子在算什麼"),
      p(class = "sub", "規格 → 假設場景 → 初始/邊界條件 → 控制方程式 → 檔案怎麼用 → 產出什麼"),

      div(class = "callout", HTML(
        "<b>一句話：</b>一口注水井把用過的冷水打回地下，一口生產井抽熱水上來。
         問 —— <b>幾年後，生產井會被自己回注的冷水打穿？</b><br>
         這個問題的答案決定兩口井要隔多遠，而井距決定整個電廠的資本支出。")),

      h2("1. 假設場景與背景"),
      p(class = "note", HTML(
        "地熱發電抽上來的熱水，取完熱之後必須<b>回注</b>地下 —— 一來維持地層壓力，
         二來那些水含礦物質不能直接排放。但回注的水是冷的（本案 70 °C）。
         它在地下會被含水層推著往生產井走。<br><br>
         一開始沒事，因為熱從岩石傳回水裡。但冷水團會越來越大，總有一天
         生產井的水溫開始下滑 —— 那就是<b>熱突破（thermal breakthrough）</b>。
         發生得太早，電廠就提前報廢。")),
      p(class = "note", HTML(
        "本案把場景簡化成一個<b>水平、均質、侷限的 2D 含水層</b>，
         參數全部是合成的（不涉及任何真實場址或合作單位資料），
         但取值都在真實裂隙型地熱儲層的合理範圍。")),

      h3("完整參數表（由 params/params.csv 現讀，不是手打的）"),
      gt_param_html(),

      h2("2. 初始條件與邊界條件"),
      div(class = "card", gt_intro_domain(m)),
      p(class = "note", style = "margin-top:-4px;color:#64748b;font-size:13px",
        sprintf("模型域 %g × %g m，dx = %g m → %d × %d 格。圖上的灰線每 400 m 一條，也就是每 20 格一條。",
                p$Lx, p$Ly, p$dx, m$nx, m$ny)),
      p(class = "note", HTML(sprintf(
        "<b>初始條件（IC）</b>：t = 0 時全場溫度 %g °C，尚未受任何擾動。<br>
         <b>邊界條件（BC）</b>：左右兩側是<b>定水頭</b>（Dirichlet），
         兩者相差 %.2f m，代表整個區域本來就有的水力坡降；
         上下兩側是<b>不透水</b>（no-flow），用鏡像法表達。<br>
         <b>溫度的邊界</b>：水從邊界流進來時帶 %g °C，流出去時帶走該格當下的溫度
         —— 上風判斷。這一條若寫反，能量守恆測試會立刻紅，而且是唯一會紅的一條。",
        p$T_res, p$grad_regional * p$Lx, p$T_res))),

      h2("3. 控制方程式"),

      h3("3.1 流場（穩態）"),
      p(class = "note", "先解水往哪裡流。侷限含水層的穩態流是 Laplace 方程，兩口井是點源匯："),
      HTML('<div class="eq"><span class="k">Tr</span> · ∇²h  =  −Q<sub>well</sub> / (dx·dy)          <span class="c">Tr = K·b（導水係數）</span></div>'),
      p(class = "note", "五點差分離散之後整理成「四鄰平均 + 源匯」的形式，dx 剛好消掉："),
      div(class = "two",
        div(HTML(GT_SVG_STENCIL)),
        div(
          HTML('<div class="eq">h<sub>P</sub> = ( h<sub>N</sub> + h<sub>S</sub> + h<sub>W</sub> + h<sub>E</sub> + Q/Tr ) / 4</div>'),
          p(class = "note", HTML(
            "把它套用到每一格，就是一個大型線性系統。本案用<b>紅黑 SOR</b>
             迭代解 —— 把格子塗成棋盤，先更新紅格（鄰居全是黑的），再更新黑格。
             這樣可以整批向量化，不用逐格 for loop。"))
        )),
      p(class = "note", HTML("解出水頭之後，用 Darcy 定律取每個面的體積流率（dx 又消掉了）：")),
      HTML('<div class="eq">Q<sub>face</sub> = Tr · ( h<sub>上游</sub> − h<sub>下游</sub> )        <span class="c">[m³/day]</span></div>'),

      h3("3.2 熱傳輸（暫態）"),
      p(class = "note", HTML(
        "這裡是本案最關鍵的設計決定：<b>寫成「能量」的有限體積式，
         而不是直接離散溫度的偏微分方程。</b>對每一格記一本能量帳：")),
      HTML('<div class="eq">V · C<sub>bulk</sub> · dT/dt  =  Σ(平流) + Σ(傳導 + 延散) + 井 + 邊界

  <span class="k">平流</span>  C<sub>water</sub> · Q<sub>face</sub> · T<sub>供給格</sub>              <span class="c">一階上風差分</span>
  <span class="k">傳導</span>  λ<sub>eff</sub> · b · ( T<sub>鄰</sub> − T<sub>P</sub> )              <span class="c">A_face/dx = b</span>
  <span class="k">延散</span>  λ<sub>eff</sub> = λ<sub>bulk</sub> + C<sub>water</sub> · α<sub>L</sub> · |q|
  <span class="k">井</span>    注入 +C<sub>water</sub>·Q·T<sub>inj</sub>   生產 −C<sub>water</sub>·Q·T<sub>本格</sub></div>'),
      p(class = "note", HTML(
        "這樣寫換來三件事：(1) 內部每個面對兩邊的貢獻數值相同、符號相反，
         全場加總時<b>兩兩相消</b> —— 能量守恆變成可以測到機器精度的恆等式；
         (2) 井的源匯項自然就對；(3) 下面那個 R 是自己長出來的。")),

      h3("3.3 熱遲滯因子 R —— 全案最重要的一個數字"),
      HTML(sprintf('<div class="eq">R = C<sub>bulk</sub> / ( φ · C<sub>water</sub> )
  = ( φ·(ρc)<sub>w</sub> + (1−φ)·(ρc)<sub>s</sub> ) / ( φ·(ρc)<sub>w</sub> )
  = ( %.2f×%.1e + %.2f×%.1e ) / ( %.2f×%.1e )
  = <span class="k">%.2f</span></div>', p$phi, p$rho_c_w, 1 - p$phi,
      p$rho_c_s, p$phi, p$rho_c_w, m$R_thermal)),
      p(class = "note", HTML(sprintf(
        "把能量方程除以 C<sub>bulk</sub>、令 v = q/φ（孔隙流速），會得到："))),
      HTML('<div class="eq">∂T/∂t = <span class="k">(1/R)</span> · [ −v·∇T + α<sub>L</sub>|v|·∇²T ]</div>'),
      div(class = "callout", HTML(sprintf(
        "<b>意思是：熱鋒面走得比水慢 %.2f 倍。</b>因為岩石也會蓄熱 ——
         水流過去，得先把沿路的岩石降溫，自己才走得動。<br>
         所以<b>示蹤劑打穿 ≠ 熱打穿</b>。做示蹤劑試驗看到 4 年打穿，
         不代表 4 年後電廠就完蛋。<br><br>
         程式裡<b>沒有任何一行寫 <code>* R</code></b> —— 它是從能量式自己長出來的。
         我們反而是在測試裡把它「量」回來，再對照理論值（實測誤差 0.000%%）。",
        m$R_thermal))),

      h3("3.4 顯式法的穩定條件"),
      p(class = "note", "時間積分用顯式 Euler。把更新式攤開後長這樣："),
      HTML('<div class="eq">T<sup>新</sup> = ( 1 + dt·c<sub>P</sub>/(V·C<sub>bulk</sub>) ) · T + ( 一堆非負的鄰格項 )

<span class="c">c_P 恆為負。只要「自己這一格的係數」掉到負的,解就開始振盪。所以:</span>

        <span class="k">dt  ≤  V · C<sub>bulk</sub> / ( −c<sub>P</sub> )</span></div>'),
      p(class = "note", class = "note", HTML(
        "<b>這不是查表來的 CFL 公式，是從自己寫的離散式讀出來的。</b>
         好處是換任何參數它都自動正確。實測：超過上限 2 倍，溫度會噴到
         ±10<sup>14</sup> °C；但超過 1.2 倍<b>還活著</b> ——
         那才是危險的地方，它不會在你稍微越界時警告你。")),

      h3("3.5 解析解（拿來對答案）"),
      HTML(sprintf('<div class="eq">t<sub>min</sub> = π · φ · b · L² / ( 3 · Q )        <span class="c">Gringarten &amp; Sauty (1975)</span>
t<sub>heat</sub> = R · t<sub>min</sub>

<span class="c">本案:</span> t<sub>min</sub> = %.0f 天 (%.1f 年)   t<sub>heat</sub> = %.0f 天 (<span class="k">%.1f 年</span>)</div>',
        m$t_water_gs, m$t_water_gs / DAYS_PER_YEAR,
        m$t_heat_gs, m$t_heat_gs / DAYS_PER_YEAR)),
      p(class = "note", HTML(
        "無區域流時，井對之間「最短那條流線」的到達時間有閉合解。
         <b>熱突破時間 ∝ 井距² / 抽注率</b> 就是從這裡來的 ——
         第 4 節那張設計圖的理論骨架。")),

      h2("4. 每一支檔案做什麼、怎麼用"),
      div(class = "scroll", HTML(GT_SVG_PIPELINE)),
      tags$table(class = "files",
        tags$thead(tags$tr(tags$th("檔案"), tags$th("做什麼"),
                           tags$th("怎麼跑"), tags$th("耗時"),
                           tags$th("寫出什麼"))),
        tags$tbody(
          tags$tr(tags$td(tags$code("R/00_intro.R")), tags$td("產生你正在看的這一頁"),
                  tags$td(tags$code("Rscript R/00_intro.R")), tags$td("10 秒"),
                  tags$td(tags$code("intro.html"))),
          tags$tr(tags$td(tags$code("R/01_setup.R")),
                  tags$td("參數、網格、IC/BC、熱物性。只定義函式，不做運算"),
                  tags$td(tags$em("被其他支 source")), tags$td("—"), tags$td("—")),
          tags$tr(tags$td(tags$code("R/02_flow.R")),
                  tags$td("紅黑 SOR 解穩態水頭，取 Darcy 面通量、追流線"),
                  tags$td(tags$code("Rscript R/02_flow.R")), tags$td("8 秒"),
                  tags$td(tags$code("02_flow.RData"))),
          tags$tr(tags$td(tags$code("R/03_heat.R")),
                  tags$td("溫度的移流–延散，40 年、6920 步"),
                  tags$td(tags$code("Rscript R/03_heat.R")), tags$td("9 秒"),
                  tags$td(tags$code("03_heat.RData"))),
          tags$tr(tags$td(tags$code("R/04_sweep.R")),
                  tags$td("井距 × 抽注率 掃描，28 次獨立模擬"),
                  tags$td(tags$code("Rscript R/04_sweep.R")), tags$td("85 秒"),
                  tags$td(tags$code("04_sweep.RData"))),
          tags$tr(tags$td(tags$code("R/05_viz.R")),
                  tags$td("組成互動報告（plotly 場圖動畫 + highcharter 圖表）"),
                  tags$td(tags$code("Rscript R/05_viz.R")), tags$td("35 秒"),
                  tags$td(tags$code("report.html"))),
          tags$tr(tags$td(tags$code("run_all.R")),
                  tags$td("02 → 03 → 04 → 05 一次跑完"),
                  tags$td(tags$code("Rscript run_all.R")), tags$td("2 分鐘"),
                  tags$td("以上全部")),
          tags$tr(tags$td(tags$code("tests/run_tests.R")),
                  tags$td("5 支測試、60 條斷言"),
                  tags$td(tags$code("Rscript tests/run_tests.R")),
                  tags$td("25 秒"), tags$td(tags$em("綠 / 紅")))
        )),
      div(class = "callout warn", HTML(
        "兩個常見卡點：<br>
         ① Windows 裝 R 預設<b>不會</b>把它加進 PATH。先跑
         <code>$env:Path += \";C:\\Program Files\\R\\R-4.5.1\\bin\"</code><br>
         ② 所有指令都要在 <b>repo 根目錄</b>執行，不要 cd 進 <code>R/</code>。")),

      h2("5. 執行的時候，畫面上會看到什麼"),
      p(class = "note", HTML(
        "這不是一個跑完才吐結果的黑盒子 —— 兩支主要的計算都會即時回報進度。")),

      h3("02_flow.R：看著殘差收斂"),
      p(class = "note", HTML(
        "SOR 是迭代法。它每 100 次迭代回報一次「最大質量殘差」——
         也就是「還有多少水沒對上帳」。這個數字要一路掉到接近機器精度，
         解才算數。<b>這一行是在同一個位置原地更新的：</b>")),
      HTML('<pre class="term">解穩態水頭 (紅黑 SOR) ...
  迭代   100   最大質量殘差  <i>5.43e+00</i> m3/day
  迭代   600   最大質量殘差  <i>2.58e-02</i> m3/day
  迭代  1200   最大質量殘差  <i>1.54e-04</i> m3/day
  迭代  1800   最大質量殘差  <i>9.21e-07</i> m3/day
  迭代  2400   最大質量殘差  <i>5.63e-09</i> m3/day
  omega = 1.9615, 2420 次迭代, 7.5 s
  <b>最大格點質量殘差 = 4.832e-09 m3/day  (井的抽注率 = 4320)</b>
  水頭範圍 92.385 – 106.957 m
  邊界淨通量 = -9.876e-07 m3/day (應為 0:注入 = 生產)</pre>'),

      h3("03_heat.R：看著生產井的溫度自己往下掉"),
      p(class = "note", HTML(
        "進度條旁邊那個溫度是<b>生產井當下的水溫</b>。
         前十幾年它會一動也不動地停在 130，然後開始鬆動、加速下滑。
         那個數字比進度條有說服力得多 —— 它就是這整個案子要回答的東西。")),
      HTML('<pre class="term">解溫度場 ...
  [======..................]  25%   t =  10.0 年   生產井 <i>130.00</i> degC    2.1 s
  [============............]  50%   t =  20.0 年   生產井 <i>126.65</i> degC    4.3 s
  [==================......]  75%   t =  30.0 年   生產井 <i>117.66</i> degC    6.4 s
  [========================] 100%   t =  40.0 年   生產井 <i>110.72</i> degC    8.6 s
  dt = 2.111 day (穩定上限 2.639 day), 6920 步, 8.7 s
  <b>能量守恆相對誤差 = 1.240e-13</b>
  生產井末溫 = 110.72 degC (起始 130.0)
  <b>熱突破 (-2 degC) = 6590 day = 18.0 yr   [解析解參考 24.3 yr]</b></pre>'),

      h3("04_sweep.R：28 次模擬，一行一個結果"),
      HTML('<pre class="term">設計掃描:7 個井距 x 4 個抽注率 = 28 次模擬
  [ 3/28] L =  800 m, Q =  25.0 L/s -> 32.5 yr
  [10/28] L =  800 m, Q =  50.0 L/s -> 16.7 yr
  [17/28] L =  800 m, Q =  75.0 L/s -> 11.3 yr
  [24/28] L =  800 m, Q = 100.0 L/s ->  8.5 yr
  共 84 s

--- 撐滿 30 年所需的最小井距 ---------------------------
  <b> 25.0 L/s  ->  768 m
   50.0 L/s  ->  1037 m
   75.0 L/s  ->  1236 m
  100.0 L/s  ->  1401 m</b></pre>'),

      h3("tests/run_tests.R：綠色的點就是一條通過的斷言"),
      HTML('<pre class="term">================ geothermal-doublet 測試 ================

--- test_01_flow_mass.R ---    <span style="color:#4ade80">............</span>
--- test_02_cfl.R ---          <span style="color:#4ade80">.........</span>
--- test_03_energy.R ---       <span style="color:#4ade80">.......</span>
--- test_04_retardation.R ---  <span style="color:#4ade80">.....................</span>
--- test_05_gringarten.R ---   <span style="color:#4ade80">...........</span>

共 25 秒。 <b>全綠。</b></pre>'),

      h2("6. 最後要產出什麼"),
      p(class = "note", HTML(
        "<code>outputs/report.html</code> —— <b>一個檔案</b>，8 MB，
         沒有任何外部相依，離線打得開，可以直接寄給計畫主持人。裡面有：")),
      tags$ol(class = "note",
        tags$li(HTML("<b>穩態流場</b> — 水頭等值圖 + 從注入井放出的流線，可 hover 讀值")),
        tags$li(HTML("<b>溫度場演化</b> — 播放鍵 + 時間滑桿，滑鼠停在任一格讀出溫度")),
        tags$li(HTML("<b>生產井歷線</b> — 熱突破判準線、解析解參考線")),
        tags$li(HTML("<b>設計圖</b> — 井距 × 抽注率 → 熱突破年數，附「撐滿 30 年」的門檻線")),
        tags$li(HTML("<b>驗證摘要</b> — 這份結果憑什麼可信"))
      ),
      div(class = "callout", HTML(sprintf(
        "<b>而整個案子的工程結論就是一句話：</b><br>
         這組地層條件下，抽注 50 L/s 的雙井系統，
         井距要開到 <b>約 1040 m</b> 才撐得滿 30 年。
         目前設計的 800 m 只能撐 <b>%.1f 年</b>。", 18.0)))
    )
  )
}

## ---- 執行 ---------------------------------------------------------------

gt_run_intro <- function() {
  m <- gt_model(gt_params())
  cat("組介紹頁 ...\n")
  out <- gt_out("intro.html")
  gt_write_selfcontained(gt_intro_page(m), out)
  cat(sprintf("已寫出 %s  (%.1f MB)\n", out, file.size(out) / 1024^2))
  invisible(out)
}

if (sys.nframe() == 0L) gt_run_intro()
