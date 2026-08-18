# Output 位置

只有專案說明頁 `intro.html` 進版控 —— 它是拿到這個 repo 的人第一個該看的東西，
不該先叫他裝 R。結果報告與中間結果都不進版控：`Rscript run_all.R` 兩分鐘就有，
而且每重跑一次就會再往 git 歷史加 20 MB。

| 內容 | 位置 | 更新日期 |
|---|---|---|
| 專案說明 `intro.html` | 本 repo `outputs/intro.html` | 2026-08-17 |
| 互動報告 `report.html` | 本機重跑：`Rscript run_all.R` | — |
| 對照報告 `report_bug.html` | 本機重跑：`Rscript R/99_demo_prep.R`（workshop 用） | — |
| 中間結果 `*.RData` | 本機重跑即可，不需保存 | — |
| 現地紀錄 / 往來文件 | 本 repo 為純合成參數的示範專案，無此類文件 | — |

> GitHub 的檔案頁不會「執行」HTML，點下去只會看到原始碼。`intro.html` 也一樣 ——
> 要在瀏覽器裡看，先 clone 或下載，或把它掛上 GitHub Pages。

## 重新產生全部產出

```bash
Rscript run_all.R
```

約 2 分鐘。`outputs/` 內容全部可重現，所以其實不需要備份 —— 需要備份的是
`params/params.csv` 與程式碼，而那兩樣都在版控裡。

這件事本身就是重點：**產出可重現的專案，不需要「找上次那個檔案」。**
