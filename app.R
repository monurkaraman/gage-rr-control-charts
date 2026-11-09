# =====================================================================
# Gauge R&R Control Charts - Shiny application
#
# Online monitoring of a measurement system with four control charts
# computed over non-overlapping windows of w parts:
#   1. Repeatability : MSE_t                 ~ scaled chi-square
#   2. MS_Part       : MS_t,Part             ~ scaled chi-square
#   3. SNR           : MS_t,Part / MSE_t     ~ scaled F
#   4. VCR           : U_t / (U_t + V_t)     ~ Beta
#
# Phase 1 estimates the in-control variance components and control
# limits; Phase 2 data are monitored against those limits.
#
# Input: an Excel file with one column for the part ID and one column
# for the measurement (one row per measurement). Example data sets are
# provided in the data/ folder (see README.md).
# =====================================================================

library(shiny)
library(readxl)

# --- Helper: Compute window statistics using aov ---
calc_stats <- function(data, part_col, meas_col, w) {
  if (!(part_col %in% names(data)) || !(meas_col %in% names(data))) {
    return(list(error = paste0("Column not found. Available columns: ",
                               paste(names(data), collapse = ", "))))
  }
  data <- data[order(data[[part_col]]), ]
  parts <- unique(data[[part_col]])
  n_win <- length(parts) %/% w
  leftover <- length(parts) %% w
  
  if (n_win == 0) return(list(mse = numeric(0), ms_part = numeric(0),
                              sse = numeric(0), ss_part = numeric(0),
                              n = 0, n_win = 0, leftover = leftover))
  
  mse_vec     <- numeric(n_win)
  ms_part_vec <- numeric(n_win)
  sse_vec     <- numeric(n_win)
  ss_part_vec <- numeric(n_win)
  n_vec       <- numeric(n_win)
  
  for (g in 1:n_win) {
    p_idx <- parts[((g-1)*w + 1):(g*w)]
    sub <- data[data[[part_col]] %in% p_idx, ]
    sub$part_f <- factor(sub[[part_col]])
    
    fit <- aov(as.formula(paste0("`", meas_col, "` ~ part_f")), data = sub)
    tbl <- summary(fit)[[1]]
    
    ss_part_vec[g] <- tbl["part_f",    "Sum Sq"]
    sse_vec[g]     <- tbl["Residuals", "Sum Sq"]
    ms_part_vec[g] <- tbl["part_f",    "Mean Sq"]
    mse_vec[g]     <- tbl["Residuals", "Mean Sq"]
    
    # n per window (harmonic mean for unbalanced)
    ni <- as.integer(table(sub$part_f))
    n_vec[g] <- length(ni) / sum(1 / ni)
  }
  
  n_harm <- mean(n_vec)
  
  list(mse = mse_vec, ms_part = ms_part_vec,
       sse = sse_vec, ss_part = ss_part_vec,
       n = n_harm, n_win = n_win, leftover = leftover)
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Gauge R&R Control Charts"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Phase 1 - Baseline"),
                 fileInput("f1", "Upload Excel (.xlsx)", accept = c(".xlsx", ".xls")),
                 fluidRow(
                   column(6, textInput("pc1", "Part Column", "Part")),
                   column(6, textInput("mc1", "Measurement Column", "Measurement"))
                 ),
                 fluidRow(
                   column(6, numericInput("w", "Window Size (w)", 20, min = 2)),
                   column(6, numericInput("alpha", "Alpha", 0.0027, step = 0.0001))
                 ),
                 actionButton("go1", "Run Phase 1", class = "btn-primary", width = "100%"),
                 hr(),
                 h4("Phase 2 - Monitoring"),
                 fileInput("f2", "Upload New Excel (.xlsx)", accept = c(".xlsx", ".xls")),
                 fluidRow(
                   column(6, textInput("pc2", "Part Column", "Part")),
                   column(6, textInput("mc2", "Measurement Column", "Measurement"))
                 ),
                 actionButton("go2", "Add Phase 2 Data", class = "btn-success", width = "100%"),
                 actionButton("reset2", "Reset Phase 2", class = "btn-warning btn-sm", width = "100%"),
                 hr(),
                 h4("In-Control Estimates"),
                 tableOutput("ic_table"),
                 hr(),
                 h4("Latest Window Values"),
                 tableOutput("latest_table"),
                 hr(),
                 verbatimTextOutput("info")
    ),
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("Repeatability",
                         plotOutput("p_rep", height = "450px", click = "click_rep"),
                         verbatimTextOutput("click_rep_info")),
                tabPanel("MS_Part",
                         plotOutput("p_part", height = "450px", click = "click_part"),
                         verbatimTextOutput("click_part_info")),
                tabPanel("SNR",
                         plotOutput("p_snr", height = "450px", click = "click_snr"),
                         verbatimTextOutput("click_snr_info")),
                tabPanel("VCR",
                         plotOutput("p_vcr", height = "450px", click = "click_vcr"),
                         verbatimTextOutput("click_vcr_info"))
              )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  rv <- reactiveValues(p1 = NULL,
                       p2_mse = numeric(0), p2_ms_part = numeric(0),
                       p2_snr = numeric(0), p2_vcr = numeric(0),
                       p2_msg = "")
  
  # ============================================================
  # Phase 1
  # ============================================================
  observeEvent(input$go1, {
    req(input$f1)
    d <- tryCatch(read_excel(input$f1$datapath), error = function(e) NULL)
    if (is.null(d)) {
      showNotification("Error reading Excel file.", type = "error", duration = 10)
      return()
    }
    res <- calc_stats(d, input$pc1, input$mc1, input$w)
    if (!is.null(res$error)) {
      showNotification(res$error, type = "error", duration = 10)
      rv$p1 <- NULL; return()
    }
    
    w     <- input$w
    n     <- res$n
    alpha <- input$alpha
    nu_rep  <- w * (n - 1)
    nu_part <- w - 1
    
    # In-control variance estimates (Phase 1 averages)
    sig2_rep  <- mean(res$mse)         # sigma^2_Rep,0
    sig2_part <- (mean(res$ms_part) - sig2_rep) / n  # sigma^2_Part,0
    if (sig2_part < 0) sig2_part <- 0  # guard against negative estimate
    
    gamma <- sig2_part / sig2_rep  # variance ratio (part-to-repeatability)
    
    # --- Control Statistics ---
    # Repeatability: MSE_t (directly)
    # MS_Part: MS_t,Part (directly)
    # SNR: MS_t,Part / MSE_t
    # VCR: U_t / (U_t + V_t) where U_t = SSE_t/sig2_rep, V_t = SS_Part/(sig2_rep + n*sig2_part)
    
    snr_vec <- res$ms_part / res$mse
    
    scale_part <- sig2_rep + n * sig2_part  # = E[MS_Part] under in-control
    U_vec <- res$sse / sig2_rep
    V_vec <- res$ss_part / scale_part
    vcr_vec <- U_vec / (U_vec + V_vec)
    
    # --- Control Limits ---
    # Repeatability (MSE): MSE ~ (sig2_rep / nu_rep) * chi2(nu_rep)
    cl_rep <- c(
      LCL = sig2_rep / nu_rep * qchisq(alpha / 2, nu_rep),
      CL  = sig2_rep,
      UCL = sig2_rep / nu_rep * qchisq(1 - alpha / 2, nu_rep)
    )
    
    # MS_Part: MS_Part ~ (scale_part / nu_part) * chi2(nu_part)
    cl_part <- c(
      LCL = scale_part / nu_part * qchisq(alpha / 2, nu_part),
      CL  = scale_part,
      UCL = scale_part / nu_part * qchisq(1 - alpha / 2, nu_part)
    )
    
    # SNR: SNR ~ (1 + n*gamma) * F(nu_part, nu_rep)
    ratio_0 <- 1 + n * gamma
    cl_snr <- c(
      LCL = ratio_0 * qf(alpha / 2, nu_part, nu_rep),
      CL  = ratio_0,
      UCL = ratio_0 * qf(1 - alpha / 2, nu_part, nu_rep)
    )
    
    # VCR: VCR ~ Beta(nu_rep/2, nu_part/2)
    cl_vcr <- c(
      LCL = qbeta(alpha / 2, nu_rep / 2, nu_part / 2),
      CL  = nu_rep / (nu_rep + nu_part),
      UCL = qbeta(1 - alpha / 2, nu_rep / 2, nu_part / 2)
    )
    
    cl <- list(rep = cl_rep, part = cl_part, snr = cl_snr, vcr = cl_vcr)
    
    rv$p1 <- list(mse = res$mse, ms_part = res$ms_part,
                  sse = res$sse, ss_part = res$ss_part,
                  snr = snr_vec, vcr = vcr_vec,
                  cl = cl, w = w, n = n,
                  nu_rep = nu_rep, nu_part = nu_part,
                  sig2_rep = sig2_rep, sig2_part = sig2_part,
                  gamma = gamma, scale_part = scale_part,
                  leftover = res$leftover)
    
    rv$p2_mse <- numeric(0); rv$p2_ms_part <- numeric(0)
    rv$p2_snr <- numeric(0); rv$p2_vcr <- numeric(0)
    
    msg <- ""
    if (res$leftover > 0) msg <- paste0(res$leftover, " leftover parts excluded in Phase 1.")
    rv$p2_msg <- msg
  })
  
  # ============================================================
  # Phase 2 - accumulate
  # ============================================================
  observeEvent(input$go2, {
    req(input$f2, rv$p1)
    d <- tryCatch(read_excel(input$f2$datapath), error = function(e) NULL)
    if (is.null(d)) {
      showNotification("Error reading Excel file.", type = "error", duration = 10)
      return()
    }
    res <- calc_stats(d, input$pc2, input$mc2, rv$p1$w)
    if (!is.null(res$error)) {
      showNotification(res$error, type = "error", duration = 10)
      return()
    }
    if (res$n_win == 0) {
      showNotification("Insufficient data for a complete window.", type = "warning", duration = 10)
      return()
    }
    
    # Phase 2 control statistics using Phase 1 in-control estimates
    snr_vec <- res$ms_part / res$mse
    
    U_vec <- res$sse / rv$p1$sig2_rep
    V_vec <- res$ss_part / rv$p1$scale_part
    vcr_vec <- U_vec / (U_vec + V_vec)
    
    rv$p2_mse     <- c(rv$p2_mse, res$mse)
    rv$p2_ms_part <- c(rv$p2_ms_part, res$ms_part)
    rv$p2_snr     <- c(rv$p2_snr, snr_vec)
    rv$p2_vcr     <- c(rv$p2_vcr, vcr_vec)
    
    msg <- paste0(res$n_win, " window(s) added. Total Phase 2 windows: ", length(rv$p2_mse), ".")
    if (res$leftover > 0) msg <- paste0(msg, " ", res$leftover, " leftover parts excluded.")
    rv$p2_msg <- msg
  })
  
  observeEvent(input$reset2, {
    rv$p2_mse <- numeric(0); rv$p2_ms_part <- numeric(0)
    rv$p2_snr <- numeric(0); rv$p2_vcr <- numeric(0)
    rv$p2_msg <- "Phase 2 data cleared."
  })
  
  # ============================================================
  # Chart plotter
  # ============================================================
  draw_chart <- function(title, y1, y2, limits, ylab) {
    y_all <- c(y1, y2); n1 <- length(y1); n_all <- length(y_all)
    lcl <- limits[1]; cl <- limits[2]; ucl <- limits[3]
    
    ylim <- range(c(y_all, ucl, lcl)) * c(0.9, 1.1)
    par(mar = c(5, 4, 4, 10))
    plot(1:n_all, y_all, type = "n", xlab = "Window", ylab = ylab,
         main = paste(title, "Control Chart"), ylim = ylim, las = 1)
    
    abline(h = ucl, col = "red",       lwd = 2,   lty = 2)
    abline(h = lcl, col = "red",       lwd = 2,   lty = 2)
    abline(h = cl,  col = "darkgreen", lwd = 1.5, lty = 3)
    
    signal <- y_all > ucl | y_all < lcl
    cols <- ifelse(signal, "red", "steelblue")
    pchs <- ifelse(signal, 17, 16)
    
    lines(1:n_all, y_all, col = "gray50")
    points(1:n_all, y_all, col = cols, pch = pchs, cex = 1.3)
    
    if (length(y2) > 0) {
      abline(v = n1 + 0.5, col = "orange", lty = 3, lwd = 2)
      mtext("Phase 1", side = 3, at = n1 / 2, col = "steelblue", font = 2)
      mtext("Phase 2", side = 3, at = n1 + length(y2) / 2, col = "darkorange", font = 2)
    }
    
    legend("topright", inset = c(-0.12, 0), xpd = TRUE,
           legend = c(paste("UCL =", round(ucl, 4)),
                      paste("CL  =", round(cl, 4)),
                      paste("LCL =", round(lcl, 4)), "Signal"),
           col = c("red", "darkgreen", "red", "red"), lty = c(2, 3, 2, NA),
           pch = c(NA, NA, NA, 17), lwd = c(2, 1.5, 2, NA), cex = 0.8, bg = "white")
  }
  
  # --- Helper: find clicked point ---
  find_clicked_point <- function(click, y1, y2) {
    if (is.null(click)) return(NULL)
    y_all <- c(y1, y2)
    n_all <- length(y_all)
    if (n_all == 0) return(NULL)
    idx <- round(click$x)
    if (idx < 1 || idx > n_all) return(NULL)
    if (abs(click$x - idx) > 0.5) return(NULL)
    phase <- if (idx <= length(y1)) "Phase 1" else "Phase 2"
    list(window = idx, value = y_all[idx], phase = phase)
  }
  
  # ============================================================
  # Render charts
  # ============================================================
  output$p_rep <- renderPlot({
    req(rv$p1)
    draw_chart("Repeatability", rv$p1$mse, rv$p2_mse,
               rv$p1$cl$rep, expression(MSE[t]))
  })
  output$p_part <- renderPlot({
    req(rv$p1)
    draw_chart("MS_Part", rv$p1$ms_part, rv$p2_ms_part,
               rv$p1$cl$part, expression(MS[t*",Part"]))
  })
  output$p_snr <- renderPlot({
    req(rv$p1)
    draw_chart("SNR", rv$p1$snr, rv$p2_snr,
               rv$p1$cl$snr, expression(SNR[t]))
  })
  output$p_vcr <- renderPlot({
    req(rv$p1)
    draw_chart("VCR", rv$p1$vcr, rv$p2_vcr,
               rv$p1$cl$vcr, expression(VCR[t]))
  })
  
  # ============================================================
  # Click info
  # ============================================================
  output$click_rep_info <- renderPrint({
    req(rv$p1)
    pt <- find_clicked_point(input$click_rep, rv$p1$mse, rv$p2_mse)
    if (is.null(pt)) cat("Click on a point to see its value.")
    else cat(sprintf("Window %d (%s): MSE = %.6f", pt$window, pt$phase, pt$value))
  })
  output$click_part_info <- renderPrint({
    req(rv$p1)
    pt <- find_clicked_point(input$click_part, rv$p1$ms_part, rv$p2_ms_part)
    if (is.null(pt)) cat("Click on a point to see its value.")
    else cat(sprintf("Window %d (%s): MS_Part = %.6f", pt$window, pt$phase, pt$value))
  })
  output$click_snr_info <- renderPrint({
    req(rv$p1)
    pt <- find_clicked_point(input$click_snr, rv$p1$snr, rv$p2_snr)
    if (is.null(pt)) cat("Click on a point to see its value.")
    else cat(sprintf("Window %d (%s): SNR = %.6f", pt$window, pt$phase, pt$value))
  })
  output$click_vcr_info <- renderPrint({
    req(rv$p1)
    pt <- find_clicked_point(input$click_vcr, rv$p1$vcr, rv$p2_vcr)
    if (is.null(pt)) cat("Click on a point to see its value.")
    else cat(sprintf("Window %d (%s): VCR = %.6f", pt$window, pt$phase, pt$value))
  })
  
  # ============================================================
  # Info tables
  # ============================================================
  output$ic_table <- renderTable({
    req(rv$p1)
    data.frame(
      Parameter = c("Repeatability Variance", "Part Variance", "gamma (variance ratio)"),
      Estimate  = c(rv$p1$sig2_rep, rv$p1$sig2_part, rv$p1$gamma)
    )
  }, striped = TRUE, bordered = TRUE, width = "100%", digits = 6)
  
  output$latest_table <- renderTable({
    req(rv$p1)
    if (length(rv$p2_mse) > 0) {
      src <- "Phase 2"
      vals <- c(tail(rv$p2_mse, 1), tail(rv$p2_ms_part, 1),
                tail(rv$p2_snr, 1), tail(rv$p2_vcr, 1))
    } else {
      src <- "Phase 1"
      vals <- c(tail(rv$p1$mse, 1), tail(rv$p1$ms_part, 1),
                tail(rv$p1$snr, 1), tail(rv$p1$vcr, 1))
    }
    data.frame(
      Statistic = c("MSE", "MS_Part", "SNR", "VCR"),
      Value     = vals,
      Source    = rep(src, 4)
    )
  }, striped = TRUE, bordered = TRUE, width = "100%", digits = 6)
  
  output$info <- renderPrint({
    if (is.null(rv$p1)) {
      if (nzchar(rv$p2_msg)) cat(rv$p2_msg, "\n") else cat("Phase 1 not yet run.")
      return()
    }
    p <- rv$p1
    cat("Phase 1: w =", p$w, "| n (harmonic) =", round(p$n, 2),
        "| Windows =", length(p$mse), "\n")
    cat("Phase 2 Windows:", length(rv$p2_mse), "\n")
    cat("sig2_Rep =", round(p$sig2_rep, 4), "| sig2_Part =", round(p$sig2_part, 4),
        "| gamma =", round(p$gamma, 4), "\n")
    if (nzchar(rv$p2_msg)) cat("\n", rv$p2_msg, "\n")
  })
}

shinyApp(ui, server)
