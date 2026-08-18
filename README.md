# geothermal-doublet — thermal breakthrough in a geothermal well doublet

**English** | [繁體中文](README.zh-TW.md)

## What this repo is

One injection well (cold), one production well (hot), separated by L metres.
One question:

> **After how many years does re-injected cold water break through to the
> production well?**

A 2D numerical experiment on entirely synthetic parameters — no partner
organisation's data is involved. It is solved in two stages:

1. **Steady-state head field** — five-point finite differences + red-black SOR.
   The two wells are point source/sink terms, the left and right boundaries are
   fixed head (the regional hydraulic gradient), top and bottom are no-flow.
   The velocity field follows from Darcy's law.
2. **Advection–dispersion of temperature** — a finite-volume formulation of the
   energy balance, first-order upwind differencing, explicit time integration.

Finally it sweeps "well spacing × pumping rate" to answer the engineering
question: **how far apart must the wells be to last a full 30 years?**

### Why this repo really exists

It is the demo project for the TBDC / NCKU groundwater lab workshop. What it
demonstrates is not the hydrology — it is the whole loop of
**"take the same physics, write it from scratch in R as a repo, make it run,
make it verifiable, and make the result something you can email to someone."**
So it deliberately does three things:

- Every script runs standalone, and they run in numbered order (the folder
  convention from Section 03)
- Every test in `tests/` maps to a real trap that fails *silently* if you skip
  it (the TDD material from Section 06)
- The deliverable is one interactive report you can send as-is, not a pile of
  PNGs scattered across a folder

## How to run it

### If you only want to see the results

Just open these two files in a browser — no R installation needed. Each is a
single self-contained file, viewable offline, and safe to email.

| File | Contents |
|---|---|
| `outputs/intro.html` | **Project brief** — the scenario, IC/BC, governing equations, what each script is for, and what the run looks like on screen |
| `outputs/report.html` | **Results** — flow field, animated temperature field, production-well time series, design chart, verification summary |

The live demo order is: `intro.html` → run it once in R → `report.html`.
The minute-by-minute script is in [`docs/workshop-guide.md`](docs/workshop-guide.md).

### If you want to re-run it yourself

First check that `Rscript` is reachable:

```bash
Rscript --version
```

**If you get "command not found"** (the Windows R installer does not add R to
PATH by default), either use the full path, or add R to this terminal's PATH:

```powershell
$env:Path += ";C:\Program Files\R\R-4.5.1\bin"
```

(That affects the current window only. To make it permanent: System Properties →
Environment Variables → add the same entry to Path.)

Then, from the repo root:

```bash
Rscript run_all.R
```

About 2 minutes; regenerates everything under `outputs/`.

```bash
Rscript tests/run_tests.R
```

About 30 seconds, 66 assertions. It only counts if they are all green.

> Both commands must be run from the **repo root** (the level above `R/` and
> `params/`). Do not `cd` into `R/` and run from there — every path inside the
> scripts is relative to the root.

## The answer for this parameter set

| | |
|---|---|
| Thermal retardation factor R | **5.71** (the thermal front is 5.71× slower than the water) |
| 800 m spacing, 50 L/s | Thermal breakthrough (2 °C drop in production temperature) at **18.0 years** |
| Production temperature after 40 years | 130 → **110.7 °C** |
| Spacing needed to last 30 years | 25 L/s → 768 m ・ 50 L/s → 1037 m ・ 75 L/s → 1236 m ・ 100 L/s → 1401 m |

Breakthrough time scales roughly as `spacing² / pumping rate`. If you want more
capacity, the spacing has to grow with the square root of it.

### The single most important number here

```
R = C_bulk / (φ · C_water) = 2.4e6 / (0.10 × 4.2e6) = 5.71
```

The rock stores heat too, so the thermal front travels more slowly than the
water. **Tracer breakthrough ≠ thermal breakthrough.**

Forgetting to multiply by R does not crash anything, does not raise a warning,
and the plots come out looking exactly as pretty — the breakthrough time is
just off by a factor of 5.7, and the required spacing is underestimated to a
fifth of its true value. That is precisely why
`tests/test_04_retardation.R` exists.

## Where the outputs live

- Large files are not version-controlled. See [`output-link.md`](output-link.md).
- Local outputs: `outputs/` (already in `.gitignore`)

## Folder structure

```
geothermal-doublet/
├─ README.md              ← the repo's ID card (English, this file)
├─ README.zh-TW.md        ← Traditional Chinese version
├─ run_all.R              ← runs the whole pipeline end to end
├─ params/
│  └─ params.csv          ← every physical parameter (synthetic), never hard-coded in the scripts
├─ R/                     ← the analysis itself, run in numbered order
│  ├─ 00_intro.R          ← builds the project brief page (spec / IC / BC / equations / usage)
│  ├─ 01_setup.R          ← parameters, grid, IC/BC (defines functions only, computes nothing)
│  ├─ 02_flow.R           ← steady head + Darcy flux + streamline tracing
│  ├─ 03_heat.R           ← temperature advection–dispersion (R emerges naturally here)
│  ├─ 04_sweep.R          ← design sweep: spacing x pumping rate -> years to breakthrough
│  ├─ 05_viz.R            ← the interactive report (plotly + highcharter)
│  └─ 99_demo_prep.R      ← optional for the workshop: a "physics done wrong" counter-example report
├─ tests/
│  ├─ run_tests.R         ← entry point: Rscript tests/run_tests.R
│  ├─ test_01_flow_mass.R    mass conservation
│  ├─ test_02_cfl.R          stability limit of the explicit scheme
│  ├─ test_03_energy.R       energy conservation
│  ├─ test_04_retardation.R  thermal retardation factor R  ← the important one
│  ├─ test_05_gringarten.R   comparison against the analytical solution + grid convergence
│  └─ test_06_animation.R    the play button's frame list (a dead button draws fine)
├─ docs/
│  ├─ model-notes.md      ← assumptions, simplifications, numerical decisions
│  └─ workshop-guide.md   ← how to actually teach from this repo
└─ output-link.md         ← where the large files live (cloud / NAS)
```

Each script saves its result to `outputs/0X_*.RData`, which the next one reads
directly. So you can re-run just a later stage on its own (changing a colour,
for instance, only needs `Rscript R/05_viz.R`).

## Packages required

The core computation is **pure base R** (matrix arithmetic, no simulation
package anywhere). Only the plotting needs anything:

```r
install.packages(c("plotly", "highcharter", "htmltools", "viridisLite", "testthat"))
```

Wrappers such as `RMODFLOW` are deliberately avoided — writing the grid
yourself is the teaching point of this demo.

## A few key implementation decisions

**A finite-volume energy balance, rather than discretising the dT/dt PDE directly.**
That buys three things: (1) energy conservation becomes an identity you can
measure to machine precision (1e-13); (2) the well source/sink terms come out
right by construction; (3) the thermal retardation factor R is not a correction
bolted on afterwards — it grows out of `C_bulk / (φ·C_water)` by itself, so you
can *measure* it in the results and check it against theory.

**The stability limit is read off the discretisation, not recalled from a formula.**
The update is `Temp_new = (1 + dt·cP/(V·C))·Temp + (a pile of non-negative terms)`.
As soon as the cell's own coefficient goes negative, the solution starts to
oscillate. Hence `dt ≤ V·C / (−cP)`. `test_02_cfl.R` also demonstrates what
exceeding it looks like: at 2× it blows up to ±10¹⁴ °C.

**Fully vectorised — not one cell-by-cell for loop.**
Every coefficient (advection, conduction, dispersion, wells, boundaries) is
flattened into constant matrices before the time loop starts, leaving only five
multiply-adds inside the loop. 40 years, 6920 steps, 16000 cells, 8 seconds.
Before the rewrite it took 53 seconds, and the results did not change by a
single bit.

**The plotting tool follows the problem.**
The field plots (16000 cells × 21 frames) use plotly, which renders to canvas
and copes with that, and which has a native play button and time slider. The
time series and design chart carry far less data, so they use highcharter —
its annotation API is more intuitive and it looks better. Mixing two
htmlwidgets in one HTML file is not a problem.

## Known simplifications (the honest list)

- **2D, single-phase, horizontal.** No buoyancy-driven convection, no two-phase
  flow, no conductive heat loss to the cap and base rock. Cap-rock conduction
  would push real breakthrough somewhat later, so this model errs conservative.
- **Homogeneous and isotropic.** Heterogeneity in a real fractured geothermal
  reservoir lets cold water break through early along high-permeability paths.
  This is the model's largest source of optimism.
- **Wells are smeared over a single cell.** The production temperature is that
  cell's average, not the temperature at the well face.
- **First-order upwind differencing carries numerical dispersion**, so the front
  is smoother than reality. `test_05_gringarten.R` quantifies this: as the grid
  is refined, the earliest arrival time converges monotonically towards the
  analytical solution (0.49 → 0.64 → 0.75).
- **The sweep uses a coarser grid** (dx = 40 m) and therefore more numerical
  dispersion, which makes the swept years about 7% more conservative than the
  fine grid (800 m / 50 L/s: coarse 16.7 years vs fine 18.0 years).

Details in [`docs/model-notes.md`](docs/model-notes.md).

## Two notes on naming

Anyone familiar with the standard folder structure from the handout will spot
two differences:

- Parameters live in `params/`, not under the conventional name — this repo has
  no observational data at all, only a table of physical parameters, and
  calling that "data" would be misleading.
- Intermediate results are stored as `.RData` (`save()` / `load()`) rather than
  `.rds`.

Both are just naming. The paths are defined in one place, `GT_PARAMS_FILE` and
`GT_OUT_DIR` at the top of `R/01_setup.R`; switching to another convention means
editing those two lines.

## References

- Gringarten, A.C. & Sauty, J.P. (1975) A theoretical study of heat extraction
  from aquifers with uniform regional flow. *J. Geophys. Res.* 80(35): 4956–4962.
- Banks, D. (2012) *An Introduction to Thermogeology: Ground Source Heating and
  Cooling*, 2nd ed. Wiley-Blackwell.
