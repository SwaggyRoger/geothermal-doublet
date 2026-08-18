#!/usr/bin/env Rscript
# ==========================================================================
# 99_demo_prep.R — workshop 現場示範用的預備腳本(不屬於主流程)
#
# 產生一份「物理寫錯」的報告 outputs/report_bug.html:
# 熱遲滯因子被拿掉(C_bulk 誤寫成 phi * rho_c_w,忘記岩石也會蓄熱)。
#
# 為什麼要預先產:現場改完 01_setup.R 之後,要重跑 03_heat.R + 05_viz.R
# 才看得到那張「錯得很漂亮」的圖 —— 那是四十秒的空白。預先做好放在另一個
# 分頁,現場只要切過去,節奏才不會斷。
#
# 執行(開場前做一次):  Rscript R/99_demo_prep.R
#
# 它不會動到 outputs/ 裡任何正確的產出,只多寫一個 report_bug.html。
# ==========================================================================

source("R/01_setup.R")
source("R/02_flow.R")
source("R/03_heat.R")
source("R/05_viz.R")

cat("產生「忘記熱遲滯」的錯誤版報告 ...\n")

p  <- gt_params()
m  <- gt_model(p)
fl <- gt_solve_flow(m)

## 這就是現場要當眾改的那一行的效果:
##   對的  C_b <- phi*rho_c_w + (1-phi)*rho_c_s    -> R = 5.71
##   錯的  C_b <- phi*rho_c_w                      -> R = 1.00
C_bug <- p$phi * p$rho_c_w

ht <- gt_solve_heat(m, fl, C_bulk = C_bug, progress = TRUE)

bt_bug <- gt_breakthrough(ht$hist$t_day, ht$hist$Temp_pro,
                          p$T_res, p$T_drop_crit)
cat(sprintf("\n  錯誤版:R = %.2f,熱突破 %.1f 年,40 年末溫 %.1f degC\n",
            C_bug / (p$phi * m$C_w), bt_bug / DAYS_PER_YEAR,
            utils::tail(ht$hist$Temp_pro, 1)))
cat(sprintf("  能量守恆誤差 = %.1e  <-- 注意:還是綠的\n", ht$energy_err_rel))

## 報告裡的 R 仍然顯示 5.71(因為 gt_model 沒被改),這正好模擬現場的情境:
## 有人只改了儲熱項,文件與註解都還寫著正確的值。
sweep <- NULL; need30 <- NULL
if (file.exists(gt_out("04_sweep.RData"))) load(gt_out("04_sweep.RData"))

out <- gt_out("report_bug.html")
gt_write_selfcontained(gt_report(m, fl, ht, sweep, need30), out)
cat(sprintf("已寫出 %s  (%.1f MB)\n", out, file.size(out) / 1024^2))
cat("\n開場前把它和 report.html 各開一個分頁,現場只要切分頁。\n")
