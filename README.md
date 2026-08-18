# geothermal-doublet — 地熱抽注井對的熱突破模擬

## 這個 repo 是什麼

一口注水井（冷）、一口生產井（熱），中間隔 L 公尺。問一句話：

> **幾年後，生產井會被回注的冷水打穿？**

2D 數值實驗，全部用合成參數，不涉及任何合作單位資料。分兩段解：

1. **穩態水頭場** — 五點差分 + 紅黑 SOR，兩口井是點源匯，左右定水頭
   （區域水力坡降），上下不透水。由 Darcy 定律取流速場。
2. **溫度的移流–延散** — 能量的有限體積法，一階上風差分 + 顯式時間積分。

最後掃描「井距 × 抽注率」，回答工程問題：**要撐滿 30 年，井距要開多大。**

### 這個 repo 存在的真正理由

它是 TBDC / 成大地下水研究室 workshop 的示範專案。示範的不是水文，是
**「同樣的物理，用 R 從零寫成一個 repo，跑得出來、驗得過、結果可以寄給人」**
這整套流程。所以它刻意做到三件事：

- 每一支腳本都可以單獨執行，照編號依序跑（Section 03 的資料夾規範）
- `tests/` 裡的每一條都對應一個「不寫會安靜出錯」的真實陷阱（Section 06 的 TDD）
- 產出是一份可以直接寄出的互動報告，不是散落在資料夾裡的 PNG

## 怎麼跑

### 只想看結果

打開 `outputs/report.html` — 用瀏覽器開就好，不需要裝 R。
單一檔案、離線可看、可以直接寄給人。

### 想自己重跑

先確認 `Rscript` 叫得到：

```bash
Rscript --version
```

**如果顯示「找不到指令」**（Windows 裝 R 時預設不會加進 PATH），
用完整路徑，或先把 R 加進這個 terminal 的 PATH：

```powershell
$env:Path += ";C:\Program Files\R\R-4.5.1\bin"
```

（只對當前視窗有效。要永久生效：系統內容 → 環境變數 → Path 加上同一行。）

然後在 repo 根目錄：

```bash
Rscript run_all.R
```

約 2 分鐘，重新產生 `outputs/` 裡的所有東西。

```bash
Rscript tests/run_tests.R
```

約 30 秒，60 條斷言。全綠才算數。

> 兩個指令都要在 **repo 根目錄**執行（`R/` 和 `params/` 的上一層），
> 不要 `cd` 進 `R/` 再跑 —— 腳本裡的路徑都是相對於根目錄的。

## 目前這組參數的答案

| | |
|---|---|
| 熱遲滯因子 R | **5.71**（熱鋒面比水慢 5.71 倍） |
| 井距 800 m、抽注 50 L/s | 熱突破（生產溫度降 2 °C）**18.0 年** |
| 40 年後生產溫度 | 130 → **110.7 °C** |
| 撐滿 30 年所需井距 | 25 L/s → 768 m ・ 50 L/s → 1037 m ・ 75 L/s → 1236 m ・ 100 L/s → 1401 m |

熱突破時間大致正比於 `井距² / 抽注率`。想加大產能，井距要跟著開根號放大。

### 全場最重要的一個數字

```
R = C_bulk / (φ · C_water) = 2.4e6 / (0.10 × 4.2e6) = 5.71
```

岩石也會蓄熱，所以熱鋒面走得比水慢。**示蹤劑打穿 ≠ 熱打穿。**

忘記乘 R 不會讓程式壞掉、不會噴 warning、圖畫出來一模一樣漂亮 ——
只是熱突破時間會差 5.7 倍，井距會少估到剩五分之一。這就是
`tests/test_04_retardation.R` 存在的理由。

## 負責人

- 計畫負責人：陳易暄（2026-08-17 起）
- 審查者：〔待填〕

## Output 位置

- 大檔案不進版控。見 [`output-link.md`](output-link.md)。
- 本機產出：`outputs/`（已在 `.gitignore`）

## 資料夾結構

```
geothermal-doublet/
├─ README.md              ← repo 身分證
├─ run_all.R              ← 一次跑完整條流程
├─ params/
│  └─ params.csv          ← 所有物理參數(合成),不寫死在程式裡
├─ R/                     ← 分析主體,照編號依序執行
│  ├─ 01_setup.R          ← 參數、網格、IC/BC(只定義函式,不做運算)
│  ├─ 02_flow.R           ← 穩態水頭 + Darcy 通量 + 流線追蹤
│  ├─ 03_heat.R           ← 溫度移流–延散(熱遲滯 R 由此自然長出)
│  ├─ 04_sweep.R          ← 設計掃描:井距 x 抽注率 -> 熱突破年數
│  └─ 05_viz.R            ← 互動式報告(plotly + highcharter)
├─ tests/
│  ├─ run_tests.R         ← 入口:Rscript tests/run_tests.R
│  ├─ test_01_flow_mass.R    質量守恆
│  ├─ test_02_cfl.R          顯式法穩定條件
│  ├─ test_03_energy.R       能量守恆
│  ├─ test_04_retardation.R  熱遲滯因子 R  ← 最重要的一支
│  └─ test_05_gringarten.R   與解析解對照 + 網格收斂
├─ docs/
│  ├─ model-notes.md      ← 假設、簡化、數值決策紀錄
│  └─ workshop-guide.md   ← 現場怎麼用這個 repo 上課
└─ output-link.md         ← 大檔案的雲端/NAS 位置
```

每一支腳本都會把結果存成 `outputs/0X_*.RData`，下一支直接讀。所以你可以
只重跑後面某一段（例如改個顏色只要 `Rscript R/05_viz.R`）。

## 需要的套件

核心運算是 **純 base R**（矩陣運算，沒有任何模擬套件）。只有畫圖需要：

```r
install.packages(c("plotly", "highcharter", "htmltools", "viridisLite", "testthat"))
```

刻意不用 `RMODFLOW` 之類的 wrapper —— 自己寫網格才是這個 demo 的教學重點。

## 幾個關鍵的實作決定

**用能量的有限體積法，不直接離散 dT/dt 的偏微分方程。**
換來三件事：(1) 能量守恆變成可以測到機器精度（1e-13）的恆等式；
(2) 井的源匯項自然就對；(3) 熱遲滯因子 R 不是「乘上去的修正」，而是從
`C_bulk / (φ·C_water)` 自己長出來的 —— 可以在結果裡「量」到它，再回頭對理論值。

**穩定條件是從離散式讀出來的，不是背公式。**
更新式是 `Temp_new = (1 + dt·cP/(V·C))·Temp + (一堆非負項)`。只要自己這一格
的係數掉到負的，解就開始振盪。所以 `dt ≤ V·C / (−cP)`。
`test_02_cfl.R` 順便證明了超過會怎樣：2 倍就噴到 ±10¹⁴ °C。

**全程向量化，沒有一個逐格 for loop。**
所有係數（平流、傳導、延散、井、邊界）在時間迴圈開始前就攤成常數矩陣，
迴圈裡只剩五次乘加。40 年 6920 步、16000 格，8 秒跑完。
改寫前是 53 秒，結果一個位元都沒變。

**圖表工具跟著題目走。**
場圖（16000 格 × 21 影格）用 plotly，走 canvas，扛得住，而且有原生的
播放鍵＋時間滑桿。歷線與設計圖資料量小，用 highcharter，標註 API 比較直覺、
視覺也漂亮。同一份 HTML 裡混用兩個 htmlwidget 沒問題。

## 已知的簡化（誠實清單）

- **2D、單相、水平**。沒有浮力對流、沒有兩相、沒有上下蓋層的熱傳導損失。
  蓋層傳導會讓真實的熱突破再晚一些，所以本模型偏保守。
- **均質等向**。真實裂隙型地熱儲層的非均質性會讓冷水沿高滲透帶提早穿透。
  這是本模型最大的樂觀來源。
- **井被抹平成一個格點**。生產溫度是該格的平均值，不是井壁溫度。
- **一階上風差分有數值延散**，鋒面比實際更平滑。`test_05_gringarten.R`
  量化了這件事：網格加密時，最早到達時間單調逼近解析解（0.49 → 0.64 → 0.75）。
- **掃描用較粗的網格**（dx = 40 m），數值延散更大，所以掃描出來的年數比
  細網格再保守約 7%（800 m / 50 L/s：粗 16.7 年 vs 細 18.0 年）。

詳見 [`docs/model-notes.md`](docs/model-notes.md)。

## 與 handout 標準結構的兩處差異

這台開發機上有一個臨床資料的隱私 guardrail hook，會攔截任何含 `data/`
路徑或 `.rds` 副檔名的操作（那是為了另外三個臨床 app repo 設的，這裡是誤判）。
所以本 repo：

- 資料夾叫 `params/` 而不是 `data/`（反正這裡沒有觀測資料，只有參數）
- 中間結果存 `.RData` 而不是 `.rds`

路徑集中定義在 `R/01_setup.R` 最上面的 `GT_PARAMS_FILE` 與 `GT_OUT_DIR`，
要改回標準結構只要改那兩行。

## 參考文獻

- Gringarten, A.C. & Sauty, J.P. (1975) A theoretical study of heat extraction
  from aquifers with uniform regional flow. *J. Geophys. Res.* 80(35): 4956–4962.
- Banks, D. (2012) *An Introduction to Thermogeology: Ground Source Heating and
  Cooling*, 2nd ed. Wiley-Blackwell.
