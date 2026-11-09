# Gauge R&R Control Charts

Shiny application accompanying the manuscript on online monitoring of
measurement systems. It implements four control charts computed over
non-overlapping windows of `w` parts:

| Chart         | Statistic                  | In-control distribution |
|---------------|----------------------------|-------------------------|
| Repeatability | `MSE_t`                    | scaled chi-square, `w(n-1)` df |
| MS_Part       | `MS_t,Part`                | scaled chi-square, `w-1` df |
| SNR           | `MS_t,Part / MSE_t`        | `(1 + n*gamma) * F(w-1, w(n-1))` |
| VCR           | `U_t / (U_t + V_t)`        | `Beta(w(n-1)/2, (w-1)/2)` |

Phase 1 estimates the in-control variance components and control limits;
Phase 2 data are plotted against those limits.

## Repository contents

```
app.R                      Shiny application
generate_sample_data.R     Script that simulates the example data
data/
  phase1_baseline.xlsx                 Phase 1, 25 windows, in control
  phase2_A_in_control.xlsx             Phase 2, 10 windows, in control
  phase2_B_repeatability_shift.xlsx    Phase 2, sigma_rep x 1.5 from window 6
  phase2_C_part_variance_shift.xlsx    Phase 2, sigma_part x 2 from window 6
  phase2_D_part_variance_reduction.xlsx Phase 2, sigma_part x 0.5 from window 6
```

All example data are **simulated** from the one-factor random-effects model

`y_ij = mu + P_i + e_ij`, with `P_i ~ N(0, sigma_part^2)` and `e_ij ~ N(0, sigma_rep^2)`,

using `mu = 50`, `sigma_part = 1`, `sigma_rep = 0.3`, `n = 3` replicates per
part and `w = 20` parts per window. No real measurement data are included.
Running `generate_sample_data.R` (seed 2026) reproduces the files exactly.

Each file has the columns `Part`, `Replicate` and `Measurement`
(one row per measurement).

## Requirements

R (>= 4.0) with the packages `shiny` and `readxl`
(and `writexl` to regenerate the example data):

```r
install.packages(c("shiny", "readxl", "writexl"))
```

## Running the app

```r
shiny::runApp("app.R")
```

## Example walkthrough

1. **Phase 1:** upload `data/phase1_baseline.xlsx`, keep the defaults
   (Part column `Part`, Measurement column `Measurement`, `w = 20`,
   `alpha = 0.0027`) and click **Run Phase 1**. All 25 windows should be
   within the limits.
2. **Phase 2:** upload one of the `phase2_*.xlsx` files and click
   **Add Phase 2 Data**. Use **Reset Phase 2** before loading another
   scenario.

Expected behaviour with the provided files (windows 6-10 are the shifted ones):

| Scenario | Repeatability | MS_Part | SNR | VCR |
|----------|---------------|---------|-----|-----|
| A: in control              | no signals | no signals | no signals | no signals |
| B: repeatability increase  | upper signals | no signals | lower signals | upper signals |
| C: part variance increase  | no signals | upper signals | upper signals | lower signals |
| D: part variance decrease  | no signals | lower signals | lower signals | upper signals |

Since VCR is a monotone transformation of SNR, the two charts signal on
the same windows (in opposite directions).

Points can be clicked to display their exact values.
