# Explore how the assigned GOS feeding strategy relates to piglet growth over time.
# Compare day-58 weight and phase-specific performance, with explicit uncertainty.
# This is a secondary reanalysis of the whole feeding strategy, not isolated GOS.
#
# STUDY: https://doi.org/10.1186/s40104-024-01047-y
# DATA:  https://doi.org/10.5061/dryad.0k6djhb5w
# The eight sources below are eight sheets in ONE workbook.
#
# EXECUTION OUTLINE
# 1. Import: read eight sheets as text; preserve originals and provenance.
# 2. Clean: standardize missing values; flag IDs, annotations and numeric issues.
# 3. Reshape/link: build repeated weights; connect pens and feeding phases.
# 4. Model: estimate treatment contrasts and longitudinal treatment-by-time effects.
# 5. Check sensitivity: robust errors, complete trajectories and annotation exclusion.
# 6. Generate: separate longitudinal/effect charts, tables and audit outputs.
#
# READING GUIDE
# Each numbered stage explains purpose, input, functions, named outputs and checks.
# Function definitions specify reusable operations; calls underneath execute them.
# Objects flow top to bottom: raw -> cleaned records -> pen/phase tables -> models
# -> sensitivity estimates -> charts. Do not run later stages before earlier ones.
#
# INTERPRETATION
# The endpoint was selected for this reanalysis, not prospectively registered.
# Source animal counts/removal histories remain unresolved. Missing is not mortality.
# Later phases follow phase-1 supplementation under common diets.
# Repeated age measurements support longitudinal inference, not time-series forecasting.
# No ingredient prices or economic effects are estimated here.

# SETUP: check dependencies, choose input/output paths and create output directories.
# requireNamespace() tests availability without installing anything automatically.
required_packages <- c("readxl", "nlme")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1),
    quietly = TRUE)]
if (length(missing_packages)) {
    stop("Install required R packages first: ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages({
    library(readxl)
    library(nlme)
})
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) {
    args <- c("raw/MSG2_USDA2.0_data_dryad.xlsx", "outputs_structured")
}
if (length(args) != 2L || !file.exists(args[1])) {
    stop("Supply an existing workbook and output directory. See HOW TO RUN above.")
}
input <- normalizePath(args[1])
out <- args[2]
for (directory in c("", "raw_exports", "cleaned", "audit", "results", "figures")) {
    dir.create(file.path(out, directory), recursive = TRUE, showWarnings = FALSE)
}
# export_csv(x, name, folder): save a table with no row-number column; NA stays blank.
# It reads the output directory 'out'; it does not change the source workbook.
export_csv <- function(x, name, folder = "cleaned") {
    write.csv(x, file.path(out, folder, paste0(name, ".csv")), row.names = FALSE, na = "")
}
capture.output(sessionInfo(), file = file.path(out, "audit/sessionInfo.txt"))
writeLines(c(paste("Input:", basename(input)), paste("MD5:", unname(tools::md5sum(input))),
    paste("Execution:", Sys.time())), file.path(out, "audit/provenance.txt"))

# =============================================================================
# 1. IMPORT THE EIGHT SOURCE SHEETS
# =============================================================================
# PURPOSE: Preserve source text before any transformation.
# INPUT: input: path to the original Excel workbook.
# FUNCTIONS: excel_sheets() lists sheets; read_excel() imports text; export_csv() exports copies.
# OUTPUT: raw: named list of eight data frames; raw_exports/: source-sheet CSV files.
# CHECKS / FLAGS: Stop if a required sheet is missing or if the workbook has a different sheet count.

message("Stage 1/6: importing eight source sheets")
expected_sheets <- c("Pig performance (farrowing)", "Pig performance (Nursery)", "Feed (Farrowing)",
    "Feed (Nursery)", "VFA", "Histology", "Cytokines", "abbreviations")
sheets <- excel_sheets(input)
if (!setequal(sheets, expected_sheets) || length(sheets) != 8L) {
    stop("Unexpected workbook sheets. Found: ", paste(sheets, collapse = ", "))
}
raw <- setNames(lapply(sheets, function(s) as.data.frame(read_excel(input, sheet = s, col_names = FALSE,
    col_types = "text", .name_repair = "minimal"))), sheets)
for (s in sheets) {
    names(raw[[s]]) <- paste0("col", seq_len(ncol(raw[[s]])))
    export_csv(raw[[s]], gsub("[^A-Za-z0-9]+", "_", s), "raw_exports")
}

# =============================================================================
# 2. CLEAN RECORDS WITHOUT HIDING SOURCE PROBLEMS
# =============================================================================
# PURPOSE: Parse values while preserving source rows, original identifiers and flags.
# INPUT: raw: all eight imported sheets.
# FUNCTIONS: clean_text() normalizes blanks; parse_numeric_flagged() parses numbers; record_issue() appends audit issues; read_named_table() names columns.
# OUTPUT: farrowing_pigs, nursery_pigs, feed, ff, vfa, hi and cy; issues audit table.
# CHECKS / FLAGS: No imputation or automatic outlier deletion. Ambiguous IDs cannot establish animal identity.

message("Stage 2/6: cleaning records and recording flags")
# 2A. Define cleaning helpers BEFORE calling them.
# issues has one row per problem: sheet, source_row, field, raw_value and issue.
# record_issue() appends to this shared audit table using <<-; it never deletes source rows.
# clean_text() maps only empty strings, dots and literal NA to R missing values.
# parse_numeric_flagged() optionally strips an asterisk for parsing, records its presence,
# and flags nonnumeric/censored values before setting the numeric result to NA.
# read_named_table() removes a sheet's header row and retains the original row number.
issues <- data.frame(sheet = character(), source_row = integer(), field = character(), raw_value = character(),
    issue = character())
record_issue <- function(sheet, rows, field, values, reason) {
    if (length(rows))
        issues <<- rbind(issues, data.frame(sheet = sheet, source_row = rows, field = field,
            raw_value = as.character(values), issue = reason))
}
clean_text <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", ".", "NA")] <- NA_character_
    x
}
parse_numeric_flagged <- function(x, sheet, rows, field, stars = FALSE) {
    original <- clean_text(x)
    clean <- original
    if (stars) {
        i <- which(grepl("\\*", original))
        record_issue(sheet, rows[i], field, original[i], "Asterisk annotation preserved; numeric component used; sensitivity excludes annotated animals")
        clean <- gsub("\\*", "", clean)
    }
    v <- suppressWarnings(as.numeric(clean))
    bad <- which(!is.na(clean) & is.na(v))
    record_issue(sheet, rows[bad], field, original[bad], "Non-numeric or censored value retained in raw export; numeric value set missing")
    v
}
read_named_table <- function(sheet, cols) {
    d <- raw[[sheet]][-1, seq_along(cols), drop = FALSE]
    names(d) <- cols
    d[] <- lapply(d, clean_text)
    d$source_row <- seq_len(nrow(d)) + 1L
    d
}
# Retain row-level records, add technical record keys (not animal identities).
farrowing_pigs <- read_named_table("Pig performance (farrowing)", c("litter", "farrowing",
    "tag", "sex", "d7", "d14", "d21", "d23"))
farrowing_pigs$record_key <- sprintf("F%03d", seq_len(nrow(farrowing_pigs)))
nursery_pigs <- read_named_table("Pig performance (Nursery)", c("pig_id", "notch", "block",
    "farrowing", "nursery", "pen", "d7", "d21", "d23", "d31", "d44", "d58"))
nursery_pigs$record_key <- sprintf("N%03d", seq_len(nrow(nursery_pigs)))
for (obj in c("farrowing_pigs", "nursery_pigs")) {
    d <- get(obj)
    ss <- if (obj == "farrowing_pigs")
        "Pig performance (farrowing)" else "Pig performance (Nursery)"
    idcol <- if (obj == "farrowing_pigs")
        "tag" else "pig_id"
    bad <- which(tolower(d[[idcol]]) == "no tag")
    record_issue(ss, d$source_row[bad], idcol, d[[idcol]][bad], "No tag; retained using source-row technical key; no inferred cross-sheet identity")
    d[[paste0(idcol, "_original")]] <- d[[idcol]]
    d[[idcol]][bad] <- NA_character_
    for (id in intersect(c("pig_id", "notch"), names(d))) {
        ii <- which(!is.na(d[[id]]) & (duplicated(d[[id]]) | duplicated(d[[id]], fromLast = TRUE)))
        record_issue(ss, d$source_row[ii], id, d[[id]][ii], "Duplicate identifier; records retained; ambiguous joins prohibited")
    }
    d$annotated_weight <- apply(d[, grep("^d[0-9]+$", names(d)), drop = FALSE], 1, function(x) any(grepl("\\*",
        x), na.rm = TRUE))
    for (v in grep("^d[0-9]+$", names(d), value = TRUE)) d[[v]] <- parse_numeric_flagged(d[[v]],
        ss, d$source_row, v, TRUE)
    assign(obj, d)
}
stopifnot(nrow(nursery_pigs) == 216, length(unique(nursery_pigs$pen)) == 60, all(nursery_pigs$nursery %in%
    c("NG-", "NG+")), all(nursery_pigs$farrowing %in% c("FC", "FG-", "FG+")))
for (v in c("farrowing", "nursery", "block", "pen")) nursery_pigs[[v]] <- factor(nursery_pigs[[v]])
nursery_pigs$nursery <- relevel(nursery_pigs$nursery, "NG-")
nursery_pigs$farrowing <- relevel(nursery_pigs$farrowing, "FC")
farrowing_pigs$litter <- factor(farrowing_pigs$litter)
farrowing_pigs$farrowing <- factor(farrowing_pigs$farrowing, levels = c("FC", "FG-", "FG+"))
export_csv(nursery_pigs, "nursery_pigs")
export_csv(farrowing_pigs, "farrowing_pigs")

# 2B. Extract nursery feed blocks using the documented workbook layout.
# Each block holds 60 pens; phase lengths are 8, 13 and 14 days.
# Arithmetic validation compares recomputed intake with the supplied value.
# Feed blocks: original values preserved, no hidden unit conversion.
fnraw <- raw[["Feed (Nursery)"]]
feed <- do.call(rbind, lapply(seq_len(3), function(k) {
    starts <- c(3, 66, 129)
    d <- fnraw[starts[k] + 0:59, 1:9]
    names(d) <- c("farrowing", "nursery", "block", "pen", "source_d7", "feed_kg", "pen_g_day",
        "headcount", "pig_g_day")
    d[] <- lapply(d, clean_text)
    d$source_row <- starts[k] + 0:59
    d$phase <- k
    d$start_day <- c(23, 31, 44)[k]
    d$end_day <- c(31, 44, 58)[k]
    d$days <- d$end_day - d$start_day
    d
}))
for (v in c("source_d7", "feed_kg", "pen_g_day", "headcount", "pig_g_day")) feed[[v]] <- parse_numeric_flagged(feed[[v]],
    "Feed (Nursery)", feed$source_row, v)
feed$adfi_g_day_recomputed <- feed$feed_kg * 1000/feed$days/feed$headcount
feed$adfi_difference <- feed$adfi_g_day_recomputed - feed$pig_g_day
stopifnot(nrow(feed) == 180, max(abs(feed$adfi_difference), na.rm = TRUE) < 1e-07, !anyDuplicated(paste(feed$pen,
    feed$phase)))
export_csv(feed, "nursery_feed_by_phase")
# Farrowing feeder serves multiple litters: no litter/feeder crosswalk is supplied.
ffraw <- raw[["Feed (Farrowing)"]]
ff <- do.call(rbind, lapply(1:2, function(k) {
    start <- c(3, 14)[k]
    d <- ffraw[start + 0:7, 1:7]
    names(d) <- c("farrowing", "feeder", "total_div2_source", "litter_g_day", "n_pigs_source",
        "pig_g_day", "litter_total_source")
    d$week <- k
    d$source_row <- start + 0:7
    d
}))
export_csv(ff, "farrowing_feed_source_units")

# 2C. Clean terminal-sample measurements separately from repeated growth data.
# VFA discrepancies are flagged, not silently corrected. Histology keeps its IDs.
vraw <- raw[["VFA"]]
vfa <- vraw[-c(1, 2), c(1:9, 15:20)]
names(vfa) <- c("animal", "farrowing", "nursery", "acetate_pct", "propionate_pct", "butyrate_pct",
    "valerate_pct", "bca_pct", "total_mmol_l", "acetate_mmol_l", "propionate_mmol_l", "butyrate_mmol_l",
    "valerate_mmol_l", "bca_mmol_l", "total_check")
vfa$source_row <- seq_len(nrow(vfa)) + 2L
vfa[] <- lapply(vfa, clean_text)
for (v in setdiff(names(vfa), c("animal", "farrowing", "nursery", "source_row"))) vfa[[v]] <- parse_numeric_flagged(vfa[[v]],
    "VFA", as.integer(vfa$source_row), v)
vfa$stage <- ifelse(is.na(vfa$nursery), "preweaning", "postweaning")
vfa$butyrate_pct_from_concentrations <- 100 * vfa$butyrate_mmol_l/(vfa$total_mmol_l - vfa$bca_mmol_l)
vfa$butyrate_pct_discrepancy <- vfa$butyrate_pct - vfa$butyrate_pct_from_concentrations
ii <- which(abs(vfa$butyrate_pct_discrepancy) > 0.02)
record_issue("VFA", as.integer(vfa$source_row[ii]), "butyrate_pct", vfa$butyrate_pct[ii],
    "Reported mol percent differs from concentration-based calculation excluding BCA; retain and run concentration sensitivity")
hi <- read_named_table("Histology", c("animal", "farrowing", "nursery", "villus", "crypt",
    "villus_crypt", "width", "area"))
for (v in c("villus", "crypt", "villus_crypt", "width", "area")) hi[[v]] <- parse_numeric_flagged(hi[[v]],
    "Histology", hi$source_row, v)
hi$stage <- ifelse(is.na(hi$nursery), "preweaning", "postweaning")

# 2D. Parse cytokine concentrations and keep censoring indicators.
# Standards and blanks are not animal observations; only numeric animal IDs enter.
cyraw <- raw[["Cytokines"]]
analytes <- as.character(cyraw[3, seq(3, 39, 3)])
cy <- do.call(rbind, lapply(seq_along(analytes), function(j) {
    rr <- which(grepl("^[0-9]+$", cyraw[[1]]))
    rawv <- clean_text(cyraw[rr, 4 + 3 * (j - 1)])
    data.frame(animal = cyraw[rr, 1], analyte = analytes[j], raw_concentration = rawv, concentration_pg_ml = parse_numeric_flagged(rawv,
        "Cytokines", rr, analytes[j]), censored = grepl("OOR|<|>", rawv), source_row = rr)
}))

# The abbreviations sheet remains an exported reference dictionary, not model data.

# =============================================================================
# 3. RESHAPE WEIGHTS AND LINK PENS TO FEEDING PHASES
# =============================================================================
# PURPOSE: Create analysis units without assigning a pen intake repeatedly to individual pigs.
# INPUT: nursery_pigs, feed and cleaned terminal-sample records.
# FUNCTIONS: lapply()/rbind() reshape; calculate_pen_mean() aggregates; merge() links pen and phase; match() links unambiguous IDs.
# OUTPUT: weights_long, pen_weights, phase, penmeta, feed_link_audit and terminal-sample links.
# CHECKS / FLAGS: Require unique pen metadata and pen-phase keys. Screen membership before computing efficiency ratios.

message("Stage 3/6: reshaping weights and linking pens/phases")
# 3A. One row per source-record/day. The technical record key is NOT an inferred pig ID.
# Preserve every missing observation; also summarize missingness and negative changes.
weights_long <- do.call(rbind, lapply(c(7, 21, 23, 31, 44, 58), function(day) {
    d <- nursery_pigs[, c("record_key", "pig_id", "notch", "block", "farrowing", "nursery",
        "pen", "source_row", "annotated_weight")]
    d$day <- day
    d$weight_kg <- nursery_pigs[[paste0("d", day)]]
    d
}))
export_csv(weights_long, "nursery_weights_long")
missing <- do.call(rbind, lapply(c("d7", "d21", "d23", "d31", "d44", "d58"), function(v) data.frame(day = v,
    n = nrow(nursery_pigs), observed = sum(!is.na(nursery_pigs[[v]])), missing = sum(is.na(nursery_pigs[[v]])))))
export_csv(missing, "weight_missingness", "audit")
missing_group <- aggregate(is.na(weight_kg) ~ day + nursery + farrowing, weights_long, sum)
names(missing_group)[4] <- "missing"
export_csv(missing_group, "missing_by_treatment", "audit")
negative <- do.call(rbind, lapply(list(c(7, 21), c(21, 23), c(23, 31), c(31, 44), c(44, 58)),
    function(days) {
        delta <- nursery_pigs[[paste0("d", days[2])]] - nursery_pigs[[paste0("d", days[1])]]
        i <- which(delta < 0)
        data.frame(record_key = nursery_pigs$record_key[i], start = rep(days[1], length(i)),
            end = rep(days[2], length(i)), change_kg = delta[i])
    }))
export_csv(negative, "negative_weight_changes_retained", "audit")

# 3B. calculate_pen_mean(day, d) returns pen, day, observed count and mean body weight.
# Build baseline means and paired-animal phase gains before attaching feed.
# Pen-level weight and phase performance. Never duplicate feed per pig.
penmeta <- unique(nursery_pigs[, c("pen", "block", "farrowing", "nursery")])
stopifnot(nrow(penmeta) == 60)
calculate_pen_mean <- function(day, d = nursery_pigs) {
    val <- d[[paste0("d", day)]]
    ii <- split(seq_len(nrow(d)), d$pen, drop = TRUE)
    do.call(rbind, lapply(names(ii), function(p) {
        ix <- ii[[p]]
        ok <- is.finite(val[ix])
        data.frame(pen = p, day = day, n = sum(ok), mean_kg = if (any(ok))
            mean(val[ix][ok]) else NA_real_)
    }))
}
pen_weights <- do.call(rbind, lapply(c(23, 31, 44, 58), calculate_pen_mean))
pen_weights <- merge(pen_weights, penmeta, by = "pen", sort = FALSE)
base <- subset(pen_weights, day == 23, select = c(pen, mean_kg))
names(base)[2] <- "baseline_kg"
pen_weights <- merge(pen_weights, base, by = "pen", sort = FALSE)
pen_weights$day_f <- factor(pen_weights$day)
export_csv(pen_weights, "pen_weights_long")
phase <- do.call(rbind, lapply(1:3, function(k) {
    a <- c(23, 31, 44)[k]
    b <- c(31, 44, 58)[k]
    do.call(rbind, lapply(levels(nursery_pigs$pen), function(p) {
        d <- nursery_pigs[nursery_pigs$pen == p, ]
        x <- d[[paste0("d", a)]]
        y <- d[[paste0("d", b)]]
        ok <- is.finite(x) & is.finite(y)
        data.frame(pen = p, phase = k, n_start = sum(is.finite(x)), n_end = sum(is.finite(y)),
            n_paired = sum(ok), paired_gain_kg = sum((y - x)[ok]), adg_g_day = if (any(ok))
                mean((y - x)[ok])/(b - a) * 1000 else NA_real_)
    }))
}))
phase <- merge(phase, feed, by = c("pen", "phase"), sort = FALSE)
phase$complete_membership <- with(phase, n_start == headcount & n_end == headcount & n_paired ==
    headcount)
# Eligibility is a consistency screen, not proof of absence of within-phase movements.
phase$gain_feed_screened <- ifelse(phase$complete_membership, phase$paired_gain_kg/phase$feed_kg,
    NA_real_)
phase$feed_gain_screened <- ifelse(phase$complete_membership & phase$paired_gain_kg > 0,
    phase$feed_kg/phase$paired_gain_kg, NA_real_)
export_csv(phase, "pen_phase_performance")
export_csv(phase[, c("pen", "phase", "n_start", "n_end", "n_paired", "headcount", "complete_membership")],
    "feed_link_audit", "audit")

# A merge must neither lose nor multiply a pen-phase row.
stopifnot(nrow(phase) == nrow(feed), !anyDuplicated(phase[c("pen", "phase")]))
# 3C. link_terminal_samples() uses only unambiguous nursery IDs and checks treatment agreement.
# Unmatched or inconsistent terminal samples remain in cleaned exports but do not
# enter the nursery terminal-outcome models. No litter identities are invented.
lookup <- nursery_pigs[!is.na(nursery_pigs$pig_id), c("pig_id", "pen", "block", "farrowing",
    "nursery")]
ambig <- lookup$pig_id[duplicated(lookup$pig_id) | duplicated(lookup$pig_id, fromLast = TRUE)]
lookup <- lookup[!lookup$pig_id %in% ambig, ]
link_terminal_samples <- function(d) {
    ix <- match(d$animal, lookup$pig_id)
    d$pen <- lookup$pen[ix]
    d$block <- lookup$block[ix]
    d$match_consistent <- !is.na(ix) & as.character(d$nursery) == as.character(lookup$nursery[ix]) &
        as.character(d$farrowing) == as.character(lookup$farrowing[ix])
    d
}
vfa <- link_terminal_samples(vfa)
hi <- link_terminal_samples(hi)
export_csv(vfa, "vfa")
export_csv(hi, "histology")
ix <- match(cy$animal, hi$animal)
for (v in c("farrowing", "nursery", "stage", "pen", "block", "match_consistent")) cy[[v]] <- hi[[v]][ix]
export_csv(cy, "cytokines_long")

# =============================================================================
# 4. FIT TREATMENT AND LONGITUDINAL MODELS
# =============================================================================
# PURPOSE: Estimate the whole feeding-strategy contrast and whether it varies with age.
# INPUT: pen_weights, phase, penmeta, farrowing_pigs and matched terminal records.
# FUNCTIONS: lm() fits ANCOVA; lme() fits repeated pen means; anova() compares ML models.
# OUTPUT: primary, long_full, long_reduced, fm and results: estimates, SEs, CIs and p-values.
# CHECKS / FLAGS: Pen is the nursery analysis unit; litter is the farrowing unit. Terminal models are exploratory.

message("Stage 4/6: fitting treatment and longitudinal models")
# 4A. record_treatment_contrast(fit, label, term, family) extracts the NG+ minus NG- contrast.
# It appends one row to results using <<-. Each family controls later multiplicity.
# set_treatment_references(d) sets NG- and FC as reference groups before model fitting.
# Primary ANCOVA: day-58 pen mean ~ nursery diet + farrowing diet + block + baseline.
results <- data.frame()
record_treatment_contrast <- function(fit, label, term = "nurseryNG+", family = "core") {
    cf <- coef(summary(fit))
    if (!term %in% rownames(cf))
        return(NULL)
    se <- cf[term, 2]
    df <- df.residual(fit)
    est <- cf[term, 1]
    q <- qt(0.975, df)
    r <- data.frame(outcome = label, estimate = est, se = se, lower = est - q * se, upper = est +
        q * se, p = cf[term, 4], n = nobs(fit), df = df, family = family)
    results <<- rbind(results, r)
    r
}
set_treatment_references <- function(d) {
    for (v in intersect(c("nursery", "farrowing", "block", "pen"), names(d))) d[[v]] <- factor(d[[v]])
    d$nursery <- relevel(d$nursery, "NG-")
    d$farrowing <- relevel(d$farrowing, "FC")
    d
}
end <- set_treatment_references(subset(pen_weights, day == 58))
primary <- lm(mean_kg ~ nursery + farrowing + block + baseline_kg, end)
record_treatment_contrast(primary, "Day 58 weight (kg), observed animals")
capture.output(summary(primary), file = file.path(out, "results/primary_model.txt"))

# 4B. Fit separate phase growth, intake and screened-efficiency contrasts.
for (k in 1:3) {
    d <- set_treatment_references(subset(phase, phase == k))
    record_treatment_contrast(lm(adg_g_day ~ nursery + farrowing + block, d), paste0("Phase ",
        k, " ADG (g/day)"), family = "phase_growth")
    record_treatment_contrast(lm(pig_g_day ~ nursery + farrowing + block, d), paste0("Phase ",
        k, " ADFI (g/day)"), family = "phase_intake")
    d <- subset(d, complete_membership)
    if (nrow(d) > 12)
        record_treatment_contrast(lm(gain_feed_screened ~ nursery + farrowing + block, d),
            paste0("Phase ", k, " G:F, membership-screened"), family = "screened_efficiency")
}
# Repeated post-baseline pen weights: categorical time, random pen intercept,
# phase-specific residual variance. No extrapolation or forecasting.
post <- set_treatment_references(subset(pen_weights, day > 23))
post$day_f <- factor(post$day)
long_full <- lme(mean_kg ~ nursery * day_f + farrowing + block + baseline_kg, random = ~1 |
    pen, weights = varIdent(form = ~1 | day_f), data = post, method = "ML", na.action = na.omit,
    control = lmeControl(opt = "optim", maxIter = 200))
long_reduced <- update(long_full, fixed = mean_kg ~ nursery + day_f + farrowing + block +
    baseline_kg)
capture.output(summary(long_full), anova(long_reduced, long_full), file = file.path(out,
    "results/longitudinal_model.txt"))
interaction_test <- anova(long_reduced, long_full)
export_csv(as.data.frame(interaction_test), "longitudinal_interaction_test", "results")

# 4C. Secondary baseline comparison and nursery-by-farrowing interaction.
# Baseline imbalance and effect modification are secondary, not selective model gates.
record_treatment_contrast(lm(baseline_kg ~ nursery + farrowing + block, end), "Day 23 baseline weight (kg)",
    family = "baseline")
effect_mod <- update(primary, . ~ . + nursery:farrowing)
capture.output(anova(primary, effect_mod), file = file.path(out, "results/farrowing_by_nursery_interaction.txt"))
# Farrowing randomized comparison at litter level (not pig level).
fl <- do.call(rbind, lapply(levels(farrowing_pigs$litter), function(l) {
    d <- farrowing_pigs[farrowing_pigs$litter == l, ]
    data.frame(litter = l, farrowing = as.character(d$farrowing[1]), baseline = mean(d$d7,
        na.rm = TRUE), end = mean(d$d23, na.rm = TRUE), n_start = sum(is.finite(d$d7)), n_end = sum(is.finite(d$d23)))
}))
fl$farrowing <- factor(fl$farrowing, levels = c("FC", "FG-", "FG+"))
fm <- lm(end ~ farrowing + baseline, fl)
export_csv(fl, "farrowing_litter_means")
capture.output(summary(fm), anova(fm), file = file.path(out, "results/farrowing_model.txt"))
fc <- as.data.frame(coef(summary(fm)))
fc$term <- rownames(fc)
export_csv(fc, "farrowing_model_coefficients", "results")

# 4D. fit_terminal_models() aggregates matched terminal outcomes within pen before fitting.
# These measurements cannot establish mediation of growth.
fit_terminal_models <- function(d, vars, prefix) {
    d <- set_treatment_references(subset(d, stage == "postweaning" & match_consistent))
    if (!nrow(d))
        return()
    for (v in vars) {
        # aggregate within pen if more than one sampled animal shares a pen
        dd <- aggregate(d[[v]], d[, c("pen", "nursery", "farrowing", "block")], mean, na.rm = TRUE)
        names(dd)[5] <- "value"
        if (sum(is.finite(dd$value)) > 12)
            record_treatment_contrast(lm(value ~ nursery + farrowing + block, dd), paste(prefix,
                v), family = "mechanistic_exploratory")
    }
}
fit_terminal_models(vfa, c("butyrate_pct", "butyrate_mmol_l", "total_mmol_l"), "VFA")
fit_terminal_models(hi, c("villus", "crypt", "villus_crypt", "width", "area"), "Histology")
for (a in unique(cy$analyte)) {
    d <- subset(cy, analyte == a & stage == "postweaning" & match_consistent & !is.na(concentration_pg_ml) &
        concentration_pg_ml > 0)
    if (nrow(d) > 12) {
        d <- set_treatment_references(d)
        d$value <- log(d$concentration_pg_ml)
        dd <- aggregate(value ~ pen + nursery + farrowing + block, d, mean)
        record_treatment_contrast(lm(value ~ nursery + farrowing + block, dd), paste("Log cytokine",
            a), family = "cytokine_exploratory")
    }
}

# =============================================================================
# 5. RUN SENSITIVITY CHECKS AND AUDIT INFERENCE
# =============================================================================
# PURPOSE: Check how final-weight estimates respond to variance and record-selection choices.
# INPUT: primary, nursery_pigs, penmeta and the model results table.
# FUNCTIONS: model.matrix()/hatvalues() form HC3 covariance; complete.cases() selects trajectories; lm() refits.
# OUTPUT: HC3, complete-trajectory and annotation-exclusion rows; adjusted p-values and issue audit.
# CHECKS / FLAGS: Sensitivity agreement does not resolve nonrandom missingness or incomplete removal histories.

message("Stage 5/6: running sensitivity checks")
# 5A. HC3 changes standard errors, not fitted treatment coefficients.
# The t reference distribution below is an approximation using model residual df.
# Heteroskedasticity-robust HC3 sensitivity for the primary pen-level contrast.
X <- model.matrix(primary)
u <- residuals(primary)
h <- hatvalues(primary)
inv <- solve(crossprod(X))
Vhc3 <- inv %*% crossprod(X, X * as.numeric(u^2/(1 - h)^2)) %*% inv
term <- "nurseryNG+"
est <- coef(primary)[term]
se <- sqrt(Vhc3[term, term])
df <- df.residual(primary)
robust <- data.frame(outcome = "Day 58 weight, HC3 sensitivity", estimate = est, se = se,
    lower = est - qt(0.975, df) * se, upper = est + qt(0.975, df) * se, p = 2 * pt(-abs(est/se),
        df), n = nobs(primary), df = df, family = "sensitivity")
results <- rbind(results, robust)

# 5B. Complete trajectories: require all four nursery weights for each source record.
# Recalculate baseline and final pen means on the same retained records.
# Sensitivity 1: only animals with all four nursery time points.
balanced <- nursery_pigs[complete.cases(nursery_pigs[, c("d23", "d31", "d44", "d58")]), ]
sens <- merge(calculate_pen_mean(58, balanced), calculate_pen_mean(23, balanced)[, c("pen",
    "mean_kg")], by = "pen", suffixes = c("", "_baseline"))
sens <- set_treatment_references(merge(sens, penmeta, by = "pen"))
record_treatment_contrast(lm(mean_kg ~ nursery + farrowing + block + mean_kg_baseline, sens),
    "Day 58 weight (kg), complete-trajectory animals", family = "sensitivity")
# Sensitivity 2: omit source-annotated weight records, not statistical outliers.
noann <- nursery_pigs[!nursery_pigs$annotated_weight, ]
sa <- merge(calculate_pen_mean(58, noann), calculate_pen_mean(23, noann)[, c("pen", "mean_kg")],
    by = "pen", suffixes = c("", "_baseline"))
sa <- set_treatment_references(merge(sa, penmeta, by = "pen"))
record_treatment_contrast(lm(mean_kg ~ nursery + farrowing + block + mean_kg_baseline, sa),
    "Day 58 weight (kg), annotated records excluded", family = "sensitivity")

# 5C. Holm adjustment within phase families; BH within exploratory families.
# Keep the primary contrast separate from these secondary multiple comparisons.
results$p_adjusted <- results$p
for (f in unique(results$family)) {
    i <- which(results$family == f)
    if (f %in% c("phase_growth", "phase_intake", "screened_efficiency"))
        results$p_adjusted[i] <- p.adjust(results$p[i], "holm")
    if (grepl("exploratory", f))
        results$p_adjusted[i] <- p.adjust(results$p[i], "BH")
}
export_csv(results, "nursery_treatment_contrasts", "results")
export_csv(issues, "issues_requiring_review", "audit")

# Export model objects so reviewers can inspect residuals, coefficients and formulas.
saveRDS(list(primary = primary, longitudinal = long_full, longitudinal_reduced = long_reduced,
    farrowing = fm), file.path(out, "results/fitted_models.rds"))

# =============================================================================
# 6. GENERATE CHARTS, TABLES AND THE EXECUTION RECORD
# =============================================================================
# PURPOSE: Translate the fitted models and observed records into separate reviewable outputs.
# INPUT: pen_weights, primary, results and missingness summaries.
# FUNCTIONS: png()/plot()/lines()/segments() draw figures; export_csv() saves tables; dev.off() closes images.
# OUTPUT: figures/longitudinal/, figures/causal_effects/, diagnostics, tables and methodology log.
# CHECKS / FLAGS: Growth curves are descriptive; effect charts use adjusted estimates. Intervals are pointwise.

message("Stage 6/6: generating charts and reports")
# 6A. Four-panel primary diagnostics: residual patterns, normality and influence.
# 6B. Growth trajectories: unadjusted pen means with t intervals at each age.
# 6C. Phase effects: adjusted daily-gain differences with 95% model intervals.
# 6D. Missingness chart: missing records are not classified as deaths.
# Primary diagnostics and data summaries; counts are animals/records, not replicates.
png(file.path(out, "figures/04_primary_diagnostics.png"), width = 1500, height = 1200, res = 160)
par(mfrow = c(2, 2))
plot(primary)
dev.off()
summ <- do.call(rbind, lapply(split(pen_weights, list(pen_weights$nursery, pen_weights$day),
    drop = TRUE), function(d) data.frame(nursery = as.character(d$nursery[1]), day = d$day[1],
    mean = mean(d$mean_kg), se = sd(d$mean_kg)/sqrt(nrow(d)), pens = nrow(d))))
export_csv(summ, "growth_descriptive", "results")
cols <- c(`NG-` = "#355C7D", `NG+` = "#C06C44")
png(file.path(out, "figures/01_growth_trajectories.png"), width = 1700, height = 1100, res = 170)
par(mar = c(5, 5, 4, 2))
plot(NA, xlim = c(22, 59), ylim = range(summ$mean + 2 * summ$se, summ$mean - 2 * summ$se),
    xlab = "Age (days)", ylab = "Pen mean body weight (kg)", main = "Nursery growth by assigned phase-1 diet")
for (g in names(cols)) {
    d <- summ[summ$nursery == g, ]
    d <- d[order(d$day), ]
    lines(d$day, d$mean, type = "b", pch = 19, col = cols[g], lwd = 2)
    arrows(d$day, d$mean - qt(0.975, 29) * d$se, d$day, d$mean + qt(0.975, 29) * d$se, angle = 90,
        code = 3, length = 0.04, col = cols[g])
}
legend("topleft", c("Without GOS (NG-)", "With GOS (NG+)"), col = cols, lwd = 2, bty = "n")
mtext("Unadjusted pen means and 95% t intervals; pen membership varies", side = 3, line = 0.3,
    cex = 0.8)
dev.off()
pg <- subset(results, family == "phase_growth")
png(file.path(out, "figures/02_phase_growth_effects.png"), width = 1700, height = 1100, res = 170)
par(mar = c(5, 12, 4, 2))
plot(pg$estimate, 1:nrow(pg), xlim = range(c(0, pg$lower, pg$upper)), yaxt = "n", ylab = "",
    xlab = "Adjusted NG+ minus NG- daily gain (g/day)", main = "Phase-specific feeding-strategy effects",
    pch = 19, col = cols[2])
axis(2, 1:nrow(pg), labels = pg$outcome, las = 1, cex.axis = 0.9)
segments(pg$lower, 1:nrow(pg), pg$upper, 1:nrow(pg), col = cols[2], lwd = 2)
abline(v = 0, lty = 2)
mtext("95% model intervals; phase-specific p-values Holm adjusted in results table", side = 3,
    line = 0.3, cex = 0.8)
dev.off()
png(file.path(out, "figures/03_weight_missingness.png"), width = 1500, height = 1000, res = 160)
barplot(missing$missing, names.arg = missing$day, col = "#B45A4C", ylab = "Missing weight records",
    main = "Missing observations retained without imputation")
dev.off()
# Record enough detail to review every decision before publication.
writeLines(c("# Cleaning and analysis execution log", "", "## Executed in R", "1. Imported all eight sheets as text and exported immutable raw tables. Recorded source checksum and sessionInfo.",
    "2. Converted dots and empty cells to missing. Preserved source row numbers and original tags. No observation was deleted because of an outlier rule.",
    "3. Retained duplicate/no-tag identifiers. Added source-row record keys, which are technical keys and do not claim animal identity. Ambiguous IDs excluded only from cross-sheet joins.",
    "4. Removed asterisks only for numeric parsing; flagged those cells and ran an exclusion sensitivity. Their meaning remains unresolved.",
    "5. Reshaped nursery weights to weights_long format and feed blocks to 180 pen-phase records. Verified intake arithmetic for 8, 13 and 14-day intervals.",
    "6. Linked feed once per pen and phase. Computed paired-animal growth; efficiency ratios only where observed endpoint counts match the recorded headcount. This does not establish no within-phase movement.",
    "7. Fitted a pen-level ANCOVA for day-58 weight with day-23 baseline, farrowing treatment and block. This primary endpoint was chosen for this reanalysis after seeing the workbook structure, not prospectively registered.",
    "8. Fitted categorical-time repeated pen-weight models with a pen random intercept and day-specific residual variance. Tested nursery-treatment-by-time interaction by ML likelihood ratio test.",
    "9. Ran complete-trajectory and asterisk-exclusion sensitivities. Compared ADG and intake by phase, with Holm adjustments within each three-phase family.",
    "Also computed an HC3 robust standard-error sensitivity for the primary pen-level ANCOVA.",
    "10. Analysed farrowing outcomes at litter level; explored postweaning VFA, histology and log cytokines at pen level where identities/treatments matched. Used BH adjustments within exploratory outcome families.",
    "11. Exported diagnostics, plots, coefficients, audit tables and R session metadata.",
    "", "## Interpretation limits", "This is a reanalysis of randomized feeding strategies, not proof that GOS alone mediated growth. The enriched ingredient replaced normal whey permeate and the reported diet energy differed. Account for the whole formulation change.",
    "GOS nursery treatment was given only in phase 1; subsequent phases are follow-up under common diets. Phase 2/3 contrasts describe possible carryover.",
    "Workbook has 291 farrowing and 216 nursery records. The paper describes 288 piglets and 180 nursery pigs. The reason for these discrepancies is not resolved. Source-data reanalysis is provisional until allocation and sampling histories are reconciled.",
    "No deaths are inferred from missing weights. Planned terminal sampling may account for some missingness; no complete removal/death log is supplied.",
    "Nursery analyses use pen-level replication. Litter identities cannot be unambiguously recovered for every nursery record, so litter clustering across nursery pens is not fully represented.",
    "Primary final weight compares observed animals. Complete-case sensitivity also depends on selection into complete trajectories. Neither removes bias from nonrandom missingness.",
    "Pen feed ratios are exploratory consistency-screened ratios, not verified original-trial G:F estimates. No full-period feed conversion is asserted without removal timing.",
    "Gut measurements are terminal cross-sectional samples. They do not identify causal mediation of growth by fermentation.",
    "A handful of age points supports longitudinal analysis; no ARIMA forecasting, seasonality, economic analysis or future-performance prediction is claimed.",
    "Cytokine out-of-range values are censored and excluded from numeric models without substitution. Calibration curves/detection limits were not reconstructed.",
    "VFA mol-percent discrepancies are flagged; concentration results provided as a sensitivity. No disputed source value is silently corrected.",
    "This script does not publish files; publication is a separate repository operation.",
    "", "## Sources", "Dataset: https://doi.org/10.5061/dryad.0k6djhb5w", "Study: https://doi.org/10.1186/s40104-024-01047-y"),
    file.path(out, "METHODS_AND_CLEANING.md"))
cat("DONE\n")
print(missing)
print(results)
cat("Issues:", nrow(issues), "\n")
print(table(phase$phase, phase$complete_membership))

# 6E. Separate chart sets for the longitudinal and treatment-effect reports.
for (d in c("longitudinal", "causal_effects")) dir.create(file.path(out, "figures", d), showWarnings = FALSE)
file.copy(file.path(out, "figures/01_growth_trajectories.png"), file.path(out, "figures/longitudinal/01_growth_trajectories.png"),
    overwrite = TRUE)
file.copy(file.path(out, "figures/03_weight_missingness.png"), file.path(out, "figures/longitudinal/02_weight_missingness.png"),
    overwrite = TRUE)
file.copy(file.path(out, "figures/02_phase_growth_effects.png"), file.path(out, "figures/causal_effects/02_phase_growth_effects.png"),
    overwrite = TRUE)
fr <- subset(results, family %in% c("core", "sensitivity"))
fr <- fr[match(c("Day 58 weight (kg), observed animals", "Day 58 weight, HC3 sensitivity",
    "Day 58 weight (kg), complete-trajectory animals", "Day 58 weight (kg), annotated records excluded"),
    fr$outcome), ]
labels <- c("Observed animals", "HC3 robust errors", "Complete trajectories", "Annotated records excluded")
png(file.path(out, "figures/causal_effects/01_final_weight_effects.png"), width = 1800, height = 1100,
    res = 170)
par(mar = c(5, 14, 5, 2))
plot(fr$estimate, rev(seq_len(nrow(fr))), xlim = range(c(0, fr$lower, fr$upper)), yaxt = "n",
    ylab = "", xlab = "Adjusted NG+ minus NG- day-58 weight (kg)", main = "Final-weight effects and sensitivity checks",
    pch = 19, col = "#355C7D")
axis(2, rev(seq_len(nrow(fr))), labels = labels, las = 1, cex.axis = 0.9)
segments(fr$lower, rev(seq_len(nrow(fr))), fr$upper, rev(seq_len(nrow(fr))), col = "#355C7D",
    lwd = 2)
abline(v = 0, lty = 2, col = "grey40")
mtext("Pointwise 95% intervals. Source-data questions remain unresolved.", side = 3, line = 0.6,
    cex = 0.8)
dev.off()
