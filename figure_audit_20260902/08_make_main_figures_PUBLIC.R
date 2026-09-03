# ============================================================
# Sepsis V1 — PUBLIC main-figure reproduction v1.1
# Portable / package-relative version
#
# PURPOSE
#   Reproduce manuscript Figures 1–4 from the PUBLIC package only.
#   v1.1 fixes Figure 1 layout/architecture routing and Figure 3B
#   genus-to-status visual alignment; numerical sources are unchanged.
#   No author-specific drive path is used.
#
# REQUIRED PACKAGE STRUCTURE
#   <package-root>/
#     expected_results/scientific_outputs/
#     figure_audit_20260902/
#       V1_Figure1_Screening_Flow_FINAL_20260901.csv
#       08_make_main_figures_PUBLIC.R   [this file may live here]
#
# REQUIRED R PACKAGES
#   ggplot2, patchwork
#
# USAGE
#   From package root:
#     Rscript figure_audit_20260902/08_make_main_figures_PUBLIC.R
#
#   Or from anywhere:
#     Rscript 08_make_main_figures_PUBLIC.R "/path/to/package-root"
#
#   Optional second argument = output directory:
#     Rscript 08_make_main_figures_PUBLIC.R "/path/to/package-root" "/path/to/output"
#
# IMPORTANT
#   - No statistical model is refit.
#   - All numerical panels read frozen PUBLIC CSV sources.
#   - AUC < 0.5 is preserved; scores are never inverted.
#   - Project-level feature absence is not plotted as a zero effect.
#   - PRJNA912621 Figure 4C is labelled cholestasis/SIC, not generic
#     organ dysfunction.
# ============================================================

options(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 0. Utilities
# ------------------------------------------------------------
need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Required package '", pkg, "' is not installed.\n",
      "Install with: install.packages('", pkg, "')"
    )
  }
}
need_pkg("ggplot2")
need_pkg("patchwork")

library(ggplot2)
library(patchwork)

args <- commandArgs(trailingOnly = TRUE)

script_dir <- function() {
  full <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", full, value = TRUE)
  if (length(hit)) {
    p <- sub("^--file=", "", hit[[1]])
    return(dirname(normalizePath(p, winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

is_pkg_root <- function(x) {
  dir.exists(file.path(x, "expected_results", "scientific_outputs")) &&
    file.exists(file.path(x, "figure_audit_20260902",
                          "V1_Figure1_Screening_Flow_FINAL_20260901.csv"))
}

find_pkg_root <- function() {
  if (length(args) >= 1L && nzchar(args[[1]])) {
    p <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
    if (!is_pkg_root(p)) {
      stop("Argument 1 is not a valid public package root: ", p)
    }
    return(p)
  }

  starts <- unique(c(script_dir(), getwd()))
  for (st in starts) {
    cur <- normalizePath(st, winslash = "/", mustWork = FALSE)
    for (i in 0:6) {
      if (is_pkg_root(cur)) return(cur)
      par <- dirname(cur)
      if (identical(par, cur)) break
      cur <- par
    }
  }
  stop(
    "Could not auto-detect package root.\n",
    "Pass it explicitly as argument 1."
  )
}

PKG_ROOT <- find_pkg_root()
SRC_ROOT <- file.path(PKG_ROOT, "expected_results", "scientific_outputs")
AUDIT_ROOT <- file.path(PKG_ROOT, "figure_audit_20260902")

OUT_ROOT <- if (length(args) >= 2L && nzchar(args[[2]])) {
  normalizePath(args[[2]], winslash = "/", mustWork = FALSE)
} else {
  file.path(PKG_ROOT, "figures_reproduced_public")
}
dir.create(OUT_ROOT, recursive = TRUE, showWarnings = FALSE)

read_csv <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path)
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
           fileEncoding = "UTF-8-BOM")
}

require_cols <- function(x, cols, label) {
  miss <- setdiff(cols, names(x))
  if (length(miss)) {
    stop(label, " is missing columns: ", paste(miss, collapse = ", "))
  }
}

pretty_genus <- function(x) {
  x <- as.character(x)
  x <- sub("^Genus__", "", x)
  sub("\\|.*$", "", x)
}

close_enough <- function(x, y, tol = 1e-6) {
  is.finite(x) && is.finite(y) && abs(x - y) <= tol
}

get_summary_value <- function(summary_df, topic) {
  z <- summary_df$Value[summary_df$Topic == topic]
  if (length(z) != 1L) stop("Cannot uniquely resolve summary topic: ", topic)
  z[[1]]
}

COL <- list(
  dark = "#333333",
  grid = "#E6E6E6",
  icu = "#6D8EAA",
  healthy = "#73A86D",
  development = "#4C78A8",
  external = "#C95A49",
  sepsis = "#B85C50",
  trauma = "#E49B4A",
  positive = "#B95846",
  negative = "#4C78A8",
  neutral = "#9A9A9A",
  purple = "#7B62A3"
)

theme_pub <- function(base_size = 9) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle = element_text(size = base_size - 1, colour = "#555555"),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, colour = COL$dark),
      legend.text = element_text(size = base_size - 1),
      legend.title = element_text(size = base_size - 1),
      plot.margin = margin(7, 8, 7, 8)
    )
}

save_pub <- function(plot_obj, stem, width, height) {
  pdf_file <- file.path(OUT_ROOT, paste0(stem, ".pdf"))
  tif_file <- file.path(OUT_ROOT, paste0(stem, ".tiff"))

  ggsave(pdf_file, plot_obj, width = width, height = height,
         units = "in", device = cairo_pdf)

  tif_ok <- TRUE
  tryCatch(
    ggsave(tif_file, plot_obj, width = width, height = height,
           units = "in", dpi = 600, device = "tiff", compression = "lzw"),
    error = function(e) {
      tif_ok <<- FALSE
      warning("TIFF export failed on this system: ", conditionMessage(e))
    }
  )
  invisible(c(pdf = pdf_file, tiff = if (tif_ok) tif_file else NA_character_))
}

# ------------------------------------------------------------
# 1. Read frozen public sources
# ------------------------------------------------------------
screen <- read_csv(file.path(
  AUDIT_ROOT, "V1_Figure1_Screening_Flow_FINAL_20260901.csv"
))
f2a <- read_csv(file.path(SRC_ROOT, "21_Figure2A_Beta_PERMANOVA_Source.csv"))
f2b <- read_csv(file.path(SRC_ROOT, "21_Figure2B_ICU_Meta_Source.csv"))
f2c <- read_csv(file.path(SRC_ROOT, "21_Figure2C_Healthy_Meta_Source.csv"))
f3a <- read_csv(file.path(SRC_ROOT, "21_Figure3A_SDI_AUC_Source.csv"))
f3b <- read_csv(file.path(SRC_ROOT, "21_Figure3B_Candidate_Diagnosis_Source.csv"))
f3c <- read_csv(file.path(SRC_ROOT, "21_Figure3C_External_SDI_Distribution_Source.csv"))
f4a <- read_csv(file.path(SRC_ROOT, "21_Figure4A_T01_Shannon_Source.csv"))
f4b <- read_csv(file.path(SRC_ROOT, "21_Figure4B_T03_Shannon_Source.csv"))
f4c <- read_csv(file.path(SRC_ROOT, "21_Figure4C_T06_Shannon_Source.csv"))
f4d <- read_csv(file.path(SRC_ROOT, "21_Figure4D_T01_SDI_Source.csv"))
numsum <- read_csv(file.path(SRC_ROOT, "21_Results_Number_Summary.csv"))

# ------------------------------------------------------------
# 2. Strict source assertions
# ------------------------------------------------------------
require_cols(screen, c("Metric", "Value"), "Figure 1 screening source")
screen$Value <- as.numeric(screen$Value)

sv <- function(m) {
  z <- screen$Value[screen$Metric == m]
  if (length(z) != 1L || !is.finite(z)) stop("Bad screening metric: ", m)
  z
}

SCR <- c(
  raw = sv("Raw_query_hit_rows"),
  unique = sv("Unique_BioProjects"),
  excluded = sv("Excluded"),
  eligible = sv("Eligible_reconstructed_search"),
  locked = sv("Locked_recovered_exact_query"),
  added = sv("Additional_eligible_retrospective"),
  known = sv("Known_item_recovered"),
  final = sv("Final_frozen_V1_analytic_set"),
  pending = sv("Pending")
)
EXP <- c(raw=197, unique=143, excluded=126, eligible=17,
         locked=6, added=11, known=1, final=7, pending=0)
if (!identical(as.numeric(SCR), as.numeric(EXP))) {
  stop("Figure 1 screening source does not match the frozen final flow.")
}
if (SCR["excluded"] + SCR["eligible"] != SCR["unique"]) stop("126 + 17 != 143")
if (SCR["locked"] + SCR["added"] != SCR["eligible"]) stop("6 + 11 != 17")
if (SCR["locked"] + SCR["known"] != SCR["final"]) stop("6 + 1 != 7")

require_cols(f2a, c("Analysis_Set","Meta_Family","R2","FDR"), "Figure 2A")
if (nrow(f2a) != 4L) stop("Figure 2A must have four rows.")

require_cols(f2b, c("Genus_Feature","REML_Effect","REML_CI_Low","REML_CI_High"), "Figure 2B")
require_cols(f2c, c("Genus_Feature","REML_Effect","REML_CI_Low","REML_CI_High"), "Figure 2C")

require_cols(f3a, c("Dataset","AUC","AUC_CI_Low","AUC_CI_High"), "Figure 3A")
if (!close_enough(f3a$AUC[f3a$Dataset=="Development_Pooled"], 0.731213, 1e-6)) {
  stop("Development pooled AUC changed.")
}
if (!close_enough(f3a$AUC[grepl("^A04", f3a$Dataset)], 0.200980, 1e-6)) {
  stop("A04 AUC changed.")
}
if (!close_enough(f3a$AUC[grepl("^A05", f3a$Dataset)], 0.271605, 1e-6)) {
  stop("A05 AUC changed.")
}

require_cols(f3b, c("Genus_Feature","Missing_In_External_Project",
                    "Mean_Development_Expected_Signed_Effect",
                    "Mean_External_Expected_Signed_Effect","Diagnostic_Label"),
             "Figure 3B")
if (sum(f3b$Diagnostic_Label=="COMPLETE_EXTERNAL_DIRECTION_REVERSAL") != 4L ||
    sum(f3b$Diagnostic_Label=="PROJECT_LEVEL_FEATURE_ABSENCE") != 2L) {
  stop("Figure 3B diagnosis is not the frozen 4 reversal + 2 absence.")
}

require_cols(f4a, c("Time_Label","Time_Order_Resolved","N","Mean","SE"), "Figure 4A")
require_cols(f4b, c("Time_Label","Time_Order_Resolved","N","Mean","SE"), "Figure 4B")
require_cols(f4c, c("Time_Label","Time_Order_Resolved","Status_Resolved","N","Mean","SE"), "Figure 4C")
require_cols(f4d, c("Time_Label","Time_Order_Resolved","N","Mean","SE"), "Figure 4D")

ncheck <- with(f4c, setNames(N, paste(Time_Label, Status_Resolved, sep="|")))
expected_n <- c("Day 1|No"=10, "Day 1|Yes"=10,
                "Day 3|No"=10, "Day 3|Yes"=10,
                "Day 7|No"=9,  "Day 7|Yes"=9)
if (!all(ncheck[names(expected_n)] == expected_n)) {
  stop("Figure 4C group-specific N differs from the frozen source.")
}

# ------------------------------------------------------------
# 3. FIGURE 1
# ------------------------------------------------------------
# v1.1 layout revision:
#   - Panel A explicitly branches 17 eligible -> 6 locked + 11 additional.
#   - The 11 additional eligible branch terminates and does NOT feed the
#     frozen analytic set.
#   - Panel B restores the complete audited architecture:
#       Healthy meta -> General dysbiosis context only
#       SDI -> Locked validation -> FAILED external transfer
#           -> Post-hoc failure diagnosis
#       Supportive S01-S06 remains an independent branch.
#   - Increased spacing prevents box/text/edge overlap.

# ---------- Panel A: reconstructed screening flow ----------
nodes1 <- data.frame(
  id = c("search","raw","unique","excluded","eligible","locked","added","known","final"),
  x = c(1.15,1.15,1.15,0.55,1.75,1.75,0.55,1.75,1.75),
  y = c(8.55,7.55,6.55,5.15,5.15,3.72,3.72,2.28,0.95),
  label = c(
    "Three fixed NCBI BioProject\nquery families",
    "197 raw query-hit rows",
    "143 unique BioProjects",
    "126 excluded\n58 animal/preclinical\n42 pediatric/neonatal\n20 non-16S/incompatible\n5 no qualifying question\n1 non-gut",
    "17 eligible in\nreconstructed search",
    "6 frozen V1 projects\nrecovered by exact query",
    "11 additional eligible\nidentified retrospectively\n(not added post hoc)",
    "Known-item/original-study\naccession cross-check\nPRJNA691455",
    "FINAL FROZEN V1\nANALYTIC SET = 7"
  ),
  fill = c("#F3F3F3","#DCE8F5","#DCE8F5","#F3CBC5","#E5F1E4",
           "#DCE8F5","#FFF0D6","#FFF0D6","#DDEBF7"),
  w = c(1.15,0.95,0.95,1.02,1.02,1.02,1.02,1.18,1.35),
  h = c(.68,.56,.56,1.30,.82,.82,.98,.90,.78),
  stringsAsFactors = FALSE
)
nodes1$xmin <- nodes1$x - nodes1$w/2
nodes1$xmax <- nodes1$x + nodes1$w/2
nodes1$ymin <- nodes1$y - nodes1$h/2
nodes1$ymax <- nodes1$y + nodes1$h/2

# Straight/diagonal edges are kept outside the boxes.
edges1 <- data.frame(
  x = c(
    1.15, 1.15,              # search -> raw -> unique
    1.15, 1.15,              # unique -> excluded / eligible
    1.75, 1.75,              # eligible -> locked / added branch start
    1.75, 1.75               # locked -> known -> final
  ),
  y = c(
    8.21, 7.27,
    6.27, 6.27,
    4.74, 4.74,
    3.31, 1.83
  ),
  xend = c(
    1.15, 1.15,
    0.55, 1.75,
    1.75, 0.55,
    1.75, 1.75
  ),
  yend = c(
    7.83, 6.83,
    5.82, 5.58,
    4.14, 4.14,
    2.73, 1.34
  )
)

p1a <- ggplot() +
  geom_segment(
    data=edges1,
    aes(x=x,y=y,xend=xend,yend=yend),
    linewidth=.42, colour="#777777",
    arrow=grid::arrow(length=grid::unit(1.4,"mm"), type="closed")
  ) +
  geom_rect(
    data=nodes1,
    aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=fill),
    colour="#666666", linewidth=.38
  ) +
  geom_text(
    data=nodes1,
    aes(x=x,y=y,label=label),
    size=2.45, family="Arial", lineheight=.94
  ) +
  scale_fill_identity() +
  annotate(
    "text", x=.55, y=2.92,
    label="Documented for transparency;\nnot incorporated post hoc",
    size=2.15, family="Arial", colour="#777777", lineheight=.92
  ) +
  annotate(
    "text", x=1.15, y=.18,
    label="Retrospectively reconstructed registry; the 11 additional eligible projects were documented but not added post hoc.",
    size=2.15, family="Arial", colour="#555555"
  ) +
  coord_cartesian(xlim=c(-.08,2.35), ylim=c(.02,8.95), clip="off") +
  labs(
    title="A  Reconstructed project screening flow",
    subtitle="Frozen 2026-09-01"
  ) +
  theme_void(base_family="Arial") +
  theme(
    plot.title=element_text(face="bold", size=10, hjust=0),
    plot.subtitle=element_text(size=8, colour="#555555"),
    plot.margin=margin(8,10,8,6)
  )

# ---------- Panel B: prespecified analysis architecture ----------
# Complete architecture restored from the audited figure.
nodes2 <- data.frame(
  id=c(
    "cohorts","frozen","discovery",
    "icu_family","icu_meta","candidates","sdi","locked","failed","diagnosis",
    "healthy_family","healthy_meta","dysbiosis",
    "supportive"
  ),
  x=c(
    .80,2.35,3.90,
    3.15,3.15,3.15,3.15,3.15,3.15,5.25,
    5.00,5.00,5.00,
    7.00
  ),
  y=c(
    8.05,8.05,8.05,
    6.85,5.72,4.59,3.46,2.33,1.20,1.20,
    6.85,5.72,4.59,
    6.85
  ),
  label=c(
    "Seven public human cohorts\nindependently processed",
    "Frozen genus tables\nand analysis sets",
    "Discovery analyses\nA01, A02, A03, A06",
    "ICU-control family\nA01 + A02",
    "ICU-control\nmeta-analysis",
    "Candidate genera",
    "Prespecified\nsix-genus SDI",
    "Locked external validation\nA04: healthy | A05: trauma",
    "FAILED\nExternal transfer",
    "Post-hoc failure diagnosis\nDirection reversal +\nproject-level feature absence",
    "Healthy-control family\nA03 + A06",
    "Healthy-control\nmeta-analysis",
    "General dysbiosis\ncontext only",
    "Supportive analyses\nS01–S06\nLongitudinal / intervention /\norgan dysfunction / cholestasis"
  ),
  fill=c(
    "#F3F3F3","#F3F3F3","#DCE8F5",
    "#DCE8F5","#DCE8F5","#DCE8F5","#DCE8F5","#F3CBC5","#E9B8B0","#FFF0D6",
    "#E5F1E4","#E5F1E4","#E5F1E4",
    "#E9DFF0"
  ),
  w=c(
    1.38,1.22,1.35,
    1.25,1.25,1.16,1.16,1.55,1.18,1.72,
    1.32,1.32,1.32,
    1.68
  ),
  h=c(
    .72,.72,.72,
    .70,.66,.62,.70,.80,.66,.98,
    .70,.66,.66,
    1.02
  ),
  stringsAsFactors=FALSE
)
nodes2$xmin <- nodes2$x-nodes2$w/2
nodes2$xmax <- nodes2$x+nodes2$w/2
nodes2$ymin <- nodes2$y-nodes2$h/2
nodes2$ymax <- nodes2$y+nodes2$h/2

# Edges are deliberately routed so they do not pass through boxes.
edges2 <- data.frame(
  x=c(
    1.49, 2.96,               # cohorts -> frozen -> discovery
    3.90, 3.90, 3.90,         # discovery -> ICU / healthy / supportive
    3.15,3.15,3.15,3.15,3.15,# ICU chain
    5.00,5.00                 # healthy chain
  ),
  y=c(
    8.05,8.05,
    7.68,7.68,7.68,
    6.50,5.39,4.28,3.11,1.93,
    6.50,5.39
  ),
  xend=c(
    1.74,3.22,
    3.15,5.00,7.00,
    3.15,3.15,3.15,3.15,3.15,
    5.00,5.00
  ),
  yend=c(
    8.05,8.05,
    7.20,7.20,7.20,
    6.05,4.92,3.82,2.74,1.54,
    6.05,4.92
  )
)

# Failed external transfer -> post-hoc diagnosis: horizontal connector.
failure_edge <- data.frame(
  x=3.74, y=1.20, xend=4.39, yend=1.20
)

p1b <- ggplot() +
  geom_segment(
    data=edges2,
    aes(x=x,y=y,xend=xend,yend=yend),
    linewidth=.38, colour="#777777"
  ) +
  geom_segment(
    data=failure_edge,
    aes(x=x,y=y,xend=xend,yend=yend),
    linewidth=.38, colour="#777777"
  ) +
  geom_rect(
    data=nodes2,
    aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=fill),
    colour="#666666", linewidth=.36
  ) +
  geom_text(
    data=nodes2,
    aes(x=x,y=y,label=label),
    size=2.38, family="Arial", lineheight=.94
  ) +
  scale_fill_identity() +
  annotate(
    "text", x=4.15, y=.35,
    label="External validation data were not used for feature selection, coefficient refitting, or threshold tuning.",
    size=2.15, family="Arial", colour="#555555"
  ) +
  coord_cartesian(xlim=c(-.05,8.05), ylim=c(.10,8.55), clip="off") +
  labs(
    title="B  Prespecified analysis architecture",
    subtitle="Seven public human cohorts; 12 prespecified analysis sets (A01–A06 and S01–S06)"
  ) +
  theme_void(base_family="Arial") +
  theme(
    plot.title=element_text(face="bold", size=10, hjust=0),
    plot.subtitle=element_text(size=8, colour="#555555"),
    plot.margin=margin(8,6,8,10)
  )

fig1 <- p1a + p1b + plot_layout(widths=c(.78,1.72))
save_pub(
  fig1,
  "Figure_1_Study_Selection_and_Analysis_Architecture_PUBLIC",
  15.2, 8.2
)

# ------------------------------------------------------------
# 4. FIGURE 2
# ------------------------------------------------------------
f2a$Short <- sub("_.*$", "", f2a$Analysis_Set)
f2a$Family <- ifelse(grepl("ICU", f2a$Meta_Family, ignore.case=TRUE),
                     "ICU control", "Healthy control")
f2a$Label <- paste0("R² = ", sprintf("%.2f", 100*f2a$R2),
                    "%\nFDR = ", sprintf("%.3f", f2a$FDR))
f2a$Y <- factor(paste0(f2a$Short, "\n",
                       ifelse(f2a$Family=="ICU control","ICU","Healthy")),
                levels=rev(c("A01\nICU","A02\nICU","A03\nHealthy","A06\nHealthy")))

p2a <- ggplot(f2a, aes(x=100*R2, y=Y, colour=Family)) +
  geom_segment(aes(x=0,xend=100*R2,yend=Y), linewidth=1.0, alpha=.25) +
  geom_point(size=3.4) +
  geom_text(aes(label=Label), hjust=-.06, colour=COL$dark, size=2.6, lineheight=.95) +
  scale_colour_manual(values=c("ICU control"=COL$icu,"Healthy control"=COL$healthy),
                      guide="none") +
  scale_x_continuous(limits=c(0,max(100*f2a$R2)*1.42), expand=c(0,0)) +
  labs(title="A  Community-level effects", x="PERMANOVA R² (%)", y=NULL) +
  theme_pub() +
  theme(axis.line.y=element_blank(),axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(colour=COL$grid,linewidth=.35))

SDI_GENERA <- c("Bilophila","Butyricimonas","Parabacteroides",
                "Hoylesella","Campylobacter","Atopobium")

meta_panel <- function(z, title, subtitle, xlab, emphasize=FALSE) {
  z$Genus <- pretty_genus(z$Genus_Feature)
  z$Effect <- as.numeric(z$REML_Effect)
  z$Low <- as.numeric(z$REML_CI_Low)
  z$High <- as.numeric(z$REML_CI_High)
  z <- z[order(z$Effect),,drop=FALSE]
  z$GenusF <- factor(z$Genus, levels=z$Genus)
  z$Direction <- ifelse(z$Effect>=0,"Higher in sepsis","Lower in sepsis")
  z$Candidate <- z$Genus %in% SDI_GENERA
  z$Alpha <- if (emphasize) ifelse(z$Candidate,1,.35) else .9
  z$Size <- if (emphasize) ifelse(z$Candidate,2.8,1.9) else 2.3
  ggplot(z,aes(x=Effect,y=GenusF)) +
    geom_vline(xintercept=0,linetype="dashed",linewidth=.42,colour="#888888") +
    geom_errorbarh(aes(xmin=Low,xmax=High,alpha=Alpha),
                   height=0,linewidth=.58,colour="#555555",show.legend=FALSE) +
    geom_point(aes(colour=Direction,size=Size,alpha=Alpha),show.legend=FALSE) +
    scale_colour_manual(values=c("Higher in sepsis"=COL$positive,
                                 "Lower in sepsis"=COL$negative)) +
    scale_size_identity() + scale_alpha_identity() +
    labs(title=title,subtitle=subtitle,x=xlab,y=NULL) +
    theme_pub() +
    theme(axis.line.y=element_blank(),axis.ticks.y=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.major.x=element_line(colour=COL$grid,linewidth=.35))
}

p2b <- meta_panel(f2b,"B  ICU-control meta-analysis",
                  "Six prespecified SDI candidates emphasized",
                  "Pooled CLR effect: Sepsis − ICU control",TRUE)
p2c <- meta_panel(f2c,"C  Healthy-control meta-analysis",NULL,
                  "Pooled CLR effect: Sepsis − healthy",FALSE)

fig2 <- p2a+p2b+p2c+plot_layout(widths=c(1,1.12,1.12))
save_pub(fig2, "Figure_2_Discovery_and_Meta_Analysis_PUBLIC", 13, 6.8)

# ------------------------------------------------------------
# 5. FIGURE 3
# ------------------------------------------------------------
f3a$Stage <- ifelse(grepl("^A0[45]",f3a$Dataset),"External validation","Development")
f3a$Display <- ifelse(f3a$Dataset=="Development_Pooled","Development pooled",
               ifelse(grepl("^A01",f3a$Dataset),"A01 ICU discovery",
               ifelse(grepl("^A02",f3a$Dataset),"A02 ICU discovery",
               ifelse(grepl("^A04",f3a$Dataset),"A04 external healthy",
                      "A05 external trauma"))))
auc_order <- c("A01 ICU discovery","Development pooled","A02 ICU discovery",
               "A04 external healthy","A05 external trauma")
f3a$Display <- factor(f3a$Display, levels=rev(auc_order))

p3a <- ggplot(f3a,aes(x=AUC,y=Display,colour=Stage)) +
  geom_vline(xintercept=.5,linetype="dashed",linewidth=.45,colour="#777777") +
  geom_errorbarh(aes(xmin=AUC_CI_Low,xmax=AUC_CI_High),height=0,linewidth=.65) +
  geom_point(size=3.2) +
  scale_colour_manual(values=c("Development"=COL$development,
                               "External validation"=COL$external)) +
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.2)) +
  labs(title="A  Prespecified equal-weight SDI",
       x="AUC (95% bootstrap CI)",y=NULL,colour=NULL) +
  theme_pub() +
  theme(legend.position="top",axis.line.y=element_blank(),axis.ticks.y=element_blank(),
        panel.grid.major.y=element_blank())

f3b$Genus <- pretty_genus(f3b$Genus_Feature)
f3b$Missing <- as.character(f3b$Missing_In_External_Project) %in% c("TRUE","True","1",1)

# Status is derived from actual project-level missingness, not from panel position.
f3b$Status <- ifelse(f3b$Missing, "Absent", "Reversed")

# Fixed display order matching the audited manuscript figure (top -> bottom).
display_order_f3b <- c(
  "Campylobacter","Atopium_PLACEHOLDER",
  "Bilophila","Butyricimonas","Parabacteroides","Hoylesella"
)
# Correct spelling after using a placeholder to avoid accidental substring changes.
display_order_f3b[display_order_f3b=="Atopium_PLACEHOLDER"] <- "Atopobium"

unknown_genera <- setdiff(f3b$Genus, display_order_f3b)
if (length(unknown_genera)) {
  stop("Unexpected Figure 3B genus/genera: ", paste(unknown_genera, collapse=", "))
}
f3b$GenusF <- factor(f3b$Genus, levels=rev(display_order_f3b))
meas <- f3b[!f3b$Missing,,drop=FALSE]

# Exact frozen mapping must hold before plotting.
expected_status_f3b <- c(
  Bilophila="Reversed",
  Butyricimonas="Reversed",
  Parabacteroides="Reversed",
  Hoylesella="Reversed",
  Campylobacter="Absent",
  Atopobium="Absent"
)
observed_status_f3b <- setNames(f3b$Status, f3b$Genus)
if (!all(observed_status_f3b[names(expected_status_f3b)] == expected_status_f3b)) {
  stop("Figure 3B genus-to-status mapping differs from frozen expectation.")
}

status_x <- max(
  c(
    f3b$Mean_Development_Expected_Signed_Effect,
    meas$Mean_External_Expected_Signed_Effect
  ),
  na.rm=TRUE
) + 0.85

x_min_f3b <- min(
  c(
    f3b$Mean_Development_Expected_Signed_Effect,
    meas$Mean_External_Expected_Signed_Effect
  ),
  na.rm=TRUE
) - 0.45

p3b <- ggplot(f3b,aes(y=GenusF)) +
  geom_vline(xintercept=0,linetype="dashed",linewidth=.42,colour="#888888") +
  geom_segment(
    data=meas,
    aes(
      x=Mean_Development_Expected_Signed_Effect,
      xend=Mean_External_Expected_Signed_Effect,
      yend=GenusF
    ),
    colour="#BDBDBD",linewidth=.8
  ) +
  geom_point(
    aes(x=Mean_Development_Expected_Signed_Effect,colour="Development"),
    size=2.8
  ) +
  geom_point(
    data=meas,
    aes(x=Mean_External_Expected_Signed_Effect,colour="External"),
    size=2.8
  ) +
  geom_text(
    data=f3b[f3b$Status=="Reversed",,drop=FALSE],
    aes(x=status_x,label=Status),
    inherit.aes=TRUE,
    hjust=.5,size=2.65,family="Arial",colour=COL$external
  ) +
  geom_text(
    data=f3b[f3b$Status=="Absent",,drop=FALSE],
    aes(x=status_x,label=Status),
    inherit.aes=TRUE,
    hjust=.5,size=2.65,family="Arial",colour=COL$neutral
  ) +
  annotate(
    "text", x=status_x, y=Inf, label="External status",
    vjust=1.35, fontface="bold", family="Arial", size=2.7
  ) +
  scale_colour_manual(
    values=c("Development"=COL$development,"External"=COL$external)
  ) +
  coord_cartesian(xlim=c(x_min_f3b,status_x+0.55),clip="off") +
  labs(
    title="B  Candidate direction diagnosis",
    subtitle="External effects are plotted only for genera detected in the external project",
    x="Expected-signed effect",y=NULL,colour=NULL
  ) +
  theme_pub() +
  theme(
    legend.position="top",
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_line(colour=COL$grid,linewidth=.35),
    plot.margin=margin(7,12,7,8)
  )

make_ext_box <- function(prefix, comparator, colour, title) {
  z <- f3c[grepl(paste0("^",prefix),f3c$Analysis_Set),,drop=FALSE]
  z$Group <- ifelse(grepl("Sepsis",z$Comparison_Group,ignore.case=TRUE),
                    "Sepsis",comparator)
  z$Group <- factor(z$Group,levels=c(comparator,"Sepsis"))
  cc <- c(colour,COL$sepsis); names(cc)<-c(comparator,"Sepsis")
  ggplot(z,aes(x=Group,y=SDI_Equal_Balance,fill=Group,colour=Group)) +
    geom_boxplot(width=.56,outlier.shape=NA,alpha=.16,linewidth=.65) +
    geom_jitter(width=.09,height=0,size=1.7,alpha=.76) +
    scale_fill_manual(values=cc)+scale_colour_manual(values=cc) +
    labs(title=title,x=NULL,y="Locked equal-weight SDI") +
    theme_pub() +
    theme(legend.position="none",panel.grid.major.y=element_line(colour=COL$grid,linewidth=.35),
          panel.grid.major.x=element_blank(),axis.line.x=element_blank(),
          axis.ticks.x=element_blank())
}

p3c <- make_ext_box("A04","Healthy",COL$healthy,"C  A04 external: Healthy vs Sepsis")
p3d <- make_ext_box("A05","Trauma",COL$trauma,"D  A05 external: Trauma vs Sepsis")

fig3 <- (p3a+p3b)/(p3c+p3d)+plot_layout(heights=c(1,1.06))
save_pub(fig3, "Figure_3_SDI_Development_and_External_Failure_PUBLIC", 12, 8.7)

# ------------------------------------------------------------
# 6. FIGURE 4
# ------------------------------------------------------------
time_label <- function(x) {
  y <- as.character(x)
  y[y=="M1_initial"] <- "Baseline"
  y[y=="M2.5_ICU_day5"] <- "Day 5"
  y[y=="M2.6_ICU_day6"] <- "Day 6"
  y[y=="M2.7_ICU_day7"] <- "Day 7"
  y[y=="M3_discharge"] <- "Discharge"
  y
}

single_traj <- function(z,title,ytitle,colour,descriptive_discharge=FALSE) {
  z <- z[order(z$Time_Order_Resolved),,drop=FALSE]
  z$X <- seq_len(nrow(z))
  z$Lab <- time_label(z$Time_Label)
  z$Axis <- paste0(z$Lab,"\n(n=",z$N,")")
  p <- ggplot(z,aes(x=X,y=Mean)) +
    geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=.10,
                  colour=colour,linewidth=.58) +
    geom_point(colour=colour,size=2.5) +
    scale_x_continuous(breaks=z$X,labels=z$Axis) +
    labs(title=title,x=NULL,y=ytitle) +
    theme_pub() +
    theme(panel.grid.major.y=element_line(colour=COL$grid,linewidth=.35),
          panel.grid.major.x=element_blank())
  if (nrow(z)>=2) {
    if (descriptive_discharge && tail(z$Lab,1)=="Discharge") {
      p <- p +
        geom_line(data=z[-nrow(z),,drop=FALSE],aes(x=X,y=Mean),
                  inherit.aes=FALSE,colour=colour,linewidth=.8) +
        geom_segment(x=z$X[nrow(z)-1],xend=z$X[nrow(z)],
                     y=z$Mean[nrow(z)-1],yend=z$Mean[nrow(z)],
                     colour=colour,linewidth=.8,linetype="dashed",alpha=.55) +
        geom_point(data=z[nrow(z),,drop=FALSE],aes(x=X,y=Mean),
                   inherit.aes=FALSE,shape=21,fill="white",colour=colour,
                   size=3.0,stroke=.8)
    } else {
      p <- p + geom_line(colour=colour,linewidth=.8)
    }
  }
  p
}

fdr_t01 <- get_summary_value(numsum,"T01 Shannon overall FDR")
fdr_t03 <- get_summary_value(numsum,"T03 Shannon overall FDR")
fdr_t06 <- get_summary_value(numsum,"T06 Shannon interaction FDR")

p4a <- single_traj(f4a,"A  PRJEB33360 Shannon diversity","Mean Shannon ± SE",
                   COL$external,TRUE) +
  labs(subtitle="Discharge estimate shown descriptively (n=3)") +
  annotate("label",x=Inf,y=Inf,label=paste0("Overall time FDR = ",fdr_t01),
           hjust=1.05,vjust=1.15,size=2.5)

p4b <- single_traj(f4b,"B  PRJNA691455 Shannon diversity","Mean Shannon ± SE",
                   COL$development,FALSE) +
  annotate("label",x=Inf,y=Inf,label=paste0("Overall time FDR = ",fdr_t03),
           hjust=1.05,vjust=1.15,size=2.5)

f4c <- f4c[order(f4c$Time_Order_Resolved,f4c$Status_Resolved),,drop=FALSE]
f4c$Time <- factor(f4c$Time_Label,levels=c("Day 1","Day 3","Day 7"))
f4c$Cholestasis <- factor(f4c$Status_Resolved,levels=c("No","Yes"))
n_no <- vapply(c("Day 1","Day 3","Day 7"),
               function(t) f4c$N[f4c$Time_Label==t & f4c$Status_Resolved=="No"], numeric(1))
n_yes <- vapply(c("Day 1","Day 3","Day 7"),
                function(t) f4c$N[f4c$Time_Label==t & f4c$Status_Resolved=="Yes"], numeric(1))
xl <- setNames(paste0(c("Day 1","Day 3","Day 7"),
                      "\nNo=",n_no," | Yes=",n_yes),
               c("Day 1","Day 3","Day 7"))

p4c <- ggplot(f4c,aes(x=Time,y=Mean,group=Cholestasis,
                      colour=Cholestasis,shape=Cholestasis)) +
  geom_line(linewidth=.82) +
  geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=.08,linewidth=.56) +
  geom_point(size=2.5) +
  scale_x_discrete(labels=xl) +
  scale_colour_manual(values=c("No"="#6E8BA4","Yes"=COL$external)) +
  scale_shape_manual(values=c("No"=16,"Yes"=17)) +
  labs(title="C  Cholestasis-associated Shannon trajectories",
       x=NULL,y="Mean Shannon ± SE",colour="Cholestasis",shape="Cholestasis") +
  annotate("label",x=Inf,y=Inf,label=paste0("Time × cholestasis FDR = ",fdr_t06),
           hjust=1.05,vjust=1.15,size=2.5) +
  theme_pub() +
  theme(legend.position="top",
        panel.grid.major.y=element_line(colour=COL$grid,linewidth=.35),
        panel.grid.major.x=element_blank())

p4d <- single_traj(f4d,"D  Locked SDI trajectory in PRJEB33360",
                   "Mean equal-weight SDI ± SE",COL$purple,TRUE) +
  labs(subtitle="Discharge estimate shown descriptively (n=3)")

fig4 <- (p4a+p4b)/(p4c+p4d)
save_pub(fig4, "Figure_4_Longitudinal_and_Supportive_Analyses_PUBLIC", 12, 8.7)

# ------------------------------------------------------------
# 7. Public reproduction manifest
# ------------------------------------------------------------
manifest <- data.frame(
  Figure=c("Figure 1","Figure 2","Figure 3","Figure 4"),
  Source=c(
    "figure_audit_20260902/V1_Figure1_Screening_Flow_FINAL_20260901.csv + prespecified design",
    "expected_results/scientific_outputs/21_Figure2A/B/C_*",
    "expected_results/scientific_outputs/21_Figure3A/B/C_*",
    "expected_results/scientific_outputs/21_Figure4A/B/C/D_* + 21_Results_Number_Summary.csv"
  ),
  Statistical_refit="NO",
  stringsAsFactors=FALSE
)
write.csv(manifest,file.path(OUT_ROOT,"PUBLIC_Figure_Reproduction_Manifest.csv"),
          row.names=FALSE,fileEncoding="UTF-8")
writeLines(capture.output(sessionInfo()),
           file.path(OUT_ROOT,"PUBLIC_Figure_Reproduction_sessionInfo.txt"))

cat(
  "\nPUBLIC FIGURE REPRODUCTION COMPLETE\n",
  "Package root: ", PKG_ROOT, "\n",
  "Output: ", OUT_ROOT, "\n",
  "Statistical model refit: NO\n",
  "Figure 1 screen: 197 -> 143 -> 126/17 -> 6+11 + PRJNA691455 -> 7\n",
  "Figure 3 diagnosis: 4 reversal + 2 project-level absence\n",
  "Figure 4C: cholestasis/SIC; Day1 10/10, Day3 10/10, Day7 9/9\n",
  sep=""
)
