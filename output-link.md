# Output 位置

大檔案不進版控。`outputs/` 已列入 `.gitignore`。

| 內容 | 位置 | 更新日期 |
|---|---|---|
| 互動報告 `report.html` | 〔雲端/NAS > Lab > geothermal-doublet > outputs〕 | 〔待填〕 |
| 中間結果 `*.RData` | 本機重跑即可，不需保存 | — |
| 現地紀錄 / 往來文件 | 本 repo 為純合成參數的示範專案，無此類文件 | — |

## 重新產生全部產出

```bash
Rscript run_all.R
```

約 2 分鐘。`outputs/` 內容全部可重現，所以其實不需要備份 —— 需要備份的是
`params/params.csv` 與程式碼，而那兩樣都在版控裡。

這件事本身就是重點：**產出可重現的專案，不需要「找上次那個檔案」。**
