test_that("PlotRobustnessSequential: sequential, BF composite, robustness points and variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(1)
  n <- 100
  dfLines <- data.frame(
    x = seq_len(n),
    y = c(0, rnorm(n - 1, seq_len(n - 1) / 30, .5))
  )

  # Case A: simple sequential line plot
  p_seq <- PlotRobustnessSequential(dfLines = dfLines, xName = "n")
  testthat::expect_s3_class(p_seq, "jaspGraphs")
  vdiffr::expect_doppelganger("PlotRobustnessSequential-sequential", p_seq)

  # Case B: add BF -> composite with BF pizza and text
  BF10 <- exp(tail(dfLines$y, 1))
  p_bf <- PlotRobustnessSequential(dfLines = dfLines, xName = "n", BF = BF10, bfType = "BF10")
  testthat::expect_s3_class(p_bf, "jaspGraphsPlot")

  # Snapshot main graph and BF pizza if present
  if (!is.null(p_bf$subplots[["mainGraph"]]))
    vdiffr::expect_doppelganger("PlotRobustnessSequential-BF-main", p_bf$subplots[["mainGraph"]])
  if (!is.null(p_bf$subplots[["BFpizza"]]))
    vdiffr::expect_doppelganger("PlotRobustnessSequential-BF-pizza", p_bf$subplots[["BFpizza"]])

  # Case C: robustness plot with dfPoints and labels (from example)
  x <- seq_len(n)/100
  y <- cos(pi * x)
  dfLines2 <- data.frame(x = x, y = y)

  xpts <- dfLines2$x[c(15, 30, 50, 80)]
  ypts <- dfLines2$y[c(15, 30, 50, 80)]

  BFsubscript <- "[0][1]"
  label1 <- c(
    gettextf("max BF%s", BFsubscript),
    gettext("user prior"),
    gettext("wide prior"),
    gettext("ultrawide prior")
  )
  label1[1] <- gsub(pattern = "\\s+", "~", label1[1])
  label1[-1] <- paste0("\"", label1[-1], "\"")
  label1 <- paste0("paste(", label1, ", ':')")

  BFandSubscript <- gettextf("BF%s", BFsubscript)
  BFandSubscript <- gsub(pattern = "\\s+", "~", BFandSubscript)
  label2 <- c(
    gettextf("%s at r==%s", format(ypts[1],  digits = 4), format(xpts[1], digits = 4)),
    paste0(BFandSubscript, "==", format(ypts[2], digits = 4)),
    paste0(BFandSubscript, "==", format(ypts[3], digits = 4)),
    paste0(BFandSubscript, "==", format(ypts[4], digits = 4))
  )
  label2[1L] <- gsub(pattern = "\\s+", "~", label2[1])

  dfPoints <- data.frame(
    x = xpts,
    y = c(ypts[1], ypts[2], ypts[3], ypts[4]),
    g = label1,
    label1 = jaspGraphs::parseThis(label1),
    label2 = jaspGraphs::parseThis(label2),
    stringsAsFactors = FALSE
  )

  p_pts <- PlotRobustnessSequential(dfLines = dfLines2, xName = expression(paste("Prior width ", kappa)), dfPoints = dfPoints)
  testthat::expect_s3_class(p_pts, "jaspGraphsPlot")
  # legend subplot and main plot
  if (!is.null(p_pts$subplots[[1]]))
    vdiffr::expect_doppelganger("PlotRobustnessSequential-points-legend", p_pts$subplots[[1]])
  if (!is.null(p_pts$subplots[[2]]))
    vdiffr::expect_doppelganger("PlotRobustnessSequential-points-main", p_pts$subplots[[2]])

  # Case D: arrow label variants
  p_arrow1 <- PlotRobustnessSequential(dfLines = dfLines, arrowLabel = c("top", "bottom"))
  p_arrow2 <- PlotRobustnessSequential(dfLines = dfLines, arrowLabel = jaspGraphs::parseThis(c("alpha", "beta")))
  vdiffr::expect_doppelganger("PlotRobustnessSequential-arrow-plain", p_arrow1)
  vdiffr::expect_doppelganger("PlotRobustnessSequential-arrow-parse", p_arrow2)

  # Case E: evidence text variants with BF
  p_ev <- PlotRobustnessSequential(dfLines = dfLines, BF = 1, evidenceTxt = c("I'm above!", "I'm below!"))
  testthat::expect_s3_class(p_ev, "jaspGraphsPlot")
  if (!is.null(p_ev$subplots[["mainGraph"]]))
    vdiffr::expect_doppelganger("PlotRobustnessSequential-evidence-main", p_ev$subplots[["mainGraph"]])

})
