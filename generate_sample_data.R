# =====================================================================
# Generate example data sets for the Gauge R&R Control Charts app
#
# Model (one-factor random effects, parts measured repeatedly):
#   y_ij = mu + P_i + e_ij,   P_i ~ N(0, sigma2_part),  e_ij ~ N(0, sigma2_rep)
#   i = part, j = 1, ..., n replicate measurements
#
# All data are simulated; no real measurement data are included.
# Running this script reproduces the files in data/ exactly.
# =====================================================================

library(writexl)

set.seed(2026)

# --- Design settings ---
w  <- 20     # parts per window (default window size in the app)
n  <- 3      # replicate measurements per part
mu <- 50     # process mean

# --- In-control variance components ---
sigma_part <- 1.0    # part-to-part standard deviation
sigma_rep  <- 0.3    # repeatability standard deviation

# --- Helper: simulate a block of parts ---
# first_id   : ID of the first part in the block
# n_parts    : number of parts
# s_part     : part-to-part SD used for this block
# s_rep      : repeatability SD used for this block
sim_block <- function(first_id, n_parts, s_part, s_rep) {
  ids    <- first_id:(first_id + n_parts - 1)
  p_eff  <- rnorm(n_parts, 0, s_part)
  data.frame(
    Part        = rep(ids, each = n),
    Replicate   = rep(seq_len(n), times = n_parts),
    Measurement = round(mu + rep(p_eff, each = n) +
                          rnorm(n_parts * n, 0, s_rep), 4)
  )
}

out_dir <- "data"
dir.create(out_dir, showWarnings = FALSE)

# ---------------------------------------------------------------------
# Phase 1: in-control baseline, 25 windows x 20 parts = 500 parts
# ---------------------------------------------------------------------
phase1 <- sim_block(1, 25 * w, sigma_part, sigma_rep)
write_xlsx(phase1, file.path(out_dir, "phase1_baseline.xlsx"))

# ---------------------------------------------------------------------
# Phase 2, Scenario A: in control, 10 windows (200 parts)
# ---------------------------------------------------------------------
p2_ic <- sim_block(1001, 10 * w, sigma_part, sigma_rep)
write_xlsx(p2_ic, file.path(out_dir, "phase2_A_in_control.xlsx"))

# ---------------------------------------------------------------------
# Phase 2, Scenario B: repeatability shift
#   windows 1-5 in control, windows 6-10 with sigma_rep inflated by 1.5
# ---------------------------------------------------------------------
p2_rep <- rbind(
  sim_block(2001,         5 * w, sigma_part, sigma_rep),
  sim_block(2001 + 5 * w, 5 * w, sigma_part, 1.5 * sigma_rep)
)
write_xlsx(p2_rep, file.path(out_dir, "phase2_B_repeatability_shift.xlsx"))

# ---------------------------------------------------------------------
# Phase 2, Scenario C: part-to-part variance shift
#   windows 1-5 in control, windows 6-10 with sigma_part inflated by 2
# ---------------------------------------------------------------------
p2_part <- rbind(
  sim_block(3001,         5 * w, sigma_part,     sigma_rep),
  sim_block(3001 + 5 * w, 5 * w, 2 * sigma_part, sigma_rep)
)
write_xlsx(p2_part, file.path(out_dir, "phase2_C_part_variance_shift.xlsx"))

# ---------------------------------------------------------------------
# Phase 2, Scenario D: part-to-part variance reduction
#   windows 1-5 in control, windows 6-10 with sigma_part halved
#   (e.g., a narrower range of parts reaches the gauge) -> lower-side
#   signals on the MS_Part and SNR charts, upper-side on the VCR chart
# ---------------------------------------------------------------------
p2_low <- rbind(
  sim_block(4001,         5 * w, sigma_part,       sigma_rep),
  sim_block(4001 + 5 * w, 5 * w, 0.5 * sigma_part, sigma_rep)
)
write_xlsx(p2_low, file.path(out_dir, "phase2_D_part_variance_reduction.xlsx"))

cat("Example data written to", normalizePath(out_dir), "\n")
