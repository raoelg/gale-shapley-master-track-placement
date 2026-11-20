# Generates synthetic test datasets + config files for reference.R / compare.js
suppressPackageStartupMessages({library(jsonlite); library(writexl)})
root <- commandArgs(trailingOnly = TRUE)[1]
dir.create(file.path(root, "cases"), showWarnings = FALSE, recursive = TRUE)
set.seed(1)
tracknames_all <- c("Clinical", "Developmental", "Brain and Cognition", "Social", "Work and Organizational",
                    "Methods", "psychology-honours", "Track_B")
firsts <- c("Anna", "Bram", "Chloé", "Daan", "Eva", "Femke", "Gijs", "Hana", "Ivo", "Jasmijn", "Koen", "Lotte", "Mehmet", "Noor", "Olaf", "Priya")
lasts <- c("de Vries", "Jansen", "Bakker", "Visser", "Smit", "Meijer", "O'Brien", "van Dijk", "Mulder", "Bos")

make_case <- function(name, nstud, ntrack, qprob = c(.3, .5, .2), nappl = c(1, ntrack), info = TRUE, prefcol = TRUE,
                      ndup = 0, quota_frac = 0.8, format = "csv", quota_format = "csv", shuffle_rows = TRUE,
                      configs = list(list(seed = 20180228, samplekind = "Rejection")), ids = "num",
                      extra_quota_track = FALSE, names_style = "std", ws = FALSE, trackcol = "Track") {
  tr <- tracknames_all[seq_len(ntrack)]
  rows <- list()
  for (i in seq_len(nstud)) {
    k <- sample(seq(nappl[1], nappl[2]), 1)
    tsel <- sample(tr, k)
    sid <- if (ids == "num") as.character(10000000 + sample(1e6, 1)) else paste0(sample(c("s", "S", "x"), 1), i)
    q <- sample(c("E", "V", "O"), k, replace = TRUE, prob = qprob)
    nm <- paste(sample(firsts, 1), sample(lasts, 1))
    rows[[i]] <- data.frame(StudentID = sid, Name = nm, Email = paste0("st", i, "@example.org"),
                            Track = tsel, Preference = as.character(seq_len(k)), Qualification = q, stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, rows)
  d <- d[!duplicated(d$StudentID) | TRUE, ]
  if (ndup > 0) {
    di <- sample(nrow(d), ndup)
    dd <- d[di, ]; dd$Qualification <- sample(c("E", "V", "O"), ndup, replace = TRUE); dd$Preference <- as.character(as.integer(dd$Preference) + 1)
    d <- rbind(d, dd)
  }
  if (shuffle_rows) d <- d[sample(nrow(d)), ]
  if (!info) d <- d[, c("StudentID", "Track", "Preference", "Qualification")]
  if (!prefcol) d$Preference <- NULL
  if (ws) d$Track <- paste0(d$Track, " ")   # trailing whitespace (only trimmed for xlsx by readxl)
  if (names_style == "fuzzy") names(d)[names(d) == "StudentID"] <- "Student nr";
  if (names_style == "fuzzy") names(d)[names(d) == "Qualification"] <- "Qualificaton"
  names(d)[names(d) == "Track"] <- trackcol
  rownames(d) <- NULL
  dir <- file.path(root, "cases", name); dir.create(dir, showWarnings = FALSE)
  appfile <- if (format == "csv") file.path(dir, "applications.csv") else file.path(dir, "applications.xlsx")
  if (format == "csv") write.csv(d, appfile, row.names = FALSE) else write_xlsx(d, appfile)
  qtr <- sort(unique(trimws(d[[trackcol]])))
  if (extra_quota_track) qtr <- c(qtr, "Unused track")
  quota <- as.data.frame(as.list(setNames(pmax(1, round(quota_frac * nstud / ntrack + sample(-2:2, length(qtr), TRUE))), qtr)), check.names = FALSE)
  quotafile <- if (quota_format == "csv") file.path(dir, "quota.csv") else if (quota_format == "xlsx") file.path(dir, "quota.xlsx") else NULL
  if (identical(quota_format, "csv")) write.csv(quota, quotafile, row.names = FALSE)
  if (identical(quota_format, "xlsx")) write_xlsx(quota, quotafile)
  for (j in seq_along(configs)) {
    cf <- configs[[j]]
    cf$appfile <- normalizePath(appfile); cf$quotafile <- if (is.null(quotafile)) NULL else normalizePath(quotafile)
    cf$outdir <- file.path(normalizePath(dir), paste0("r_", j))
    writeLines(toJSON(cf, auto_unbox = TRUE, null = "null"), file.path(dir, paste0("config_", j, ".json")))
  }
}
both <- function(seeds) unlist(lapply(seeds, function(s) list(list(seed = s, samplekind = "Rejection"), list(seed = s, samplekind = "Rounding"))), recursive = FALSE)

make_case("c01_small_csv", 30, 4, configs = both(c(20180228, 1)))
make_case("c02_xlsx", 60, 5, format = "xlsx", quota_format = "xlsx", configs = both(20180228))
make_case("c03_nopref_noinfo", 40, 4, info = FALSE, prefcol = FALSE, configs = both(c(20180228, 42)))
make_case("c04_dups_tight", 80, 6, ndup = 10, quota_frac = 0.4, configs = both(20180228))
make_case("c05_loose_quota", 50, 4, quota_frac = 3, configs = both(7))
make_case("c06_many_unqualified", 70, 5, qprob = c(.2, .3, .5), configs = both(c(20180228, 123456)))
make_case("c07_large", 400, 8, nappl = c(2, 6), configs = both(c(20180228, 99)))
make_case("c08_subsets_single", 45, 6, nappl = c(1, 2), configs = both(20180228))
make_case("c09_alnum_ids_noinfo", 50, 5, ids = "alnum", info = FALSE, configs = both(20180228))
make_case("c10_noquota", 30, 4, quota_format = "none", configs = list(list(seed = 20180228, samplekind = "Rejection")))
make_case("c11_extra_quota_track", 40, 4, extra_quota_track = TRUE, configs = both(5))
make_case("c12_fuzzy_names", 40, 4, names_style = "fuzzy", configs = list(list(seed = 3, samplekind = "Rejection")))
make_case("c13_ties_all_E", 60, 4, qprob = c(1, 0, 0), quota_frac = 0.5, configs = both(20180228))
make_case("c14_xlsx_ws_quota_csv", 50, 4, format = "xlsx", ws = TRUE, configs = both(11))
make_case("c15_trackcol_renamed", 40, 4, trackcol = "Master track", configs = list(list(seed = 20180228, samplekind = "Rejection")))
make_case("c16_badlevels", 30, 4, configs = list(list(seed = 1, samplekind = "Rejection", qlevels = "E, V")))
make_case("c17_low_withdraw", 30, 4, configs = list(list(seed = 2, samplekind = "Rejection", withdrawrate = 0.5)))
make_case("c18_seed_neg_rate30", 60, 5, configs = list(list(seed = -5, samplekind = "Rejection", withdrawrate = 30), list(seed = 0, samplekind = "Rounding", withdrawrate = 30)))
make_case("c19_ignorecol_dupnames", 60, 4, configs = list(list(seed = 20180228, samplekind = "Rejection", remove_cols = list("Email")), list(seed = 20180228, samplekind = "Rounding", remove_cols = list("Email"))))
# "patched" configs: run R with minimal fixes for the 3 places where the JS port deliberately deviates
for (cs in c("c10_noquota", "c15_trackcol_renamed", "c17_low_withdraw")) {
  cf <- fromJSON(file.path(root, "cases", cs, "config_1.json")); cf$patch <- TRUE; cf$outdir <- sub("r_1$", "r_9", cf$outdir)
  writeLines(toJSON(cf, auto_unbox = TRUE, null = "null"), file.path(root, "cases", cs, "config_9.json"))
}
# C-locale runs (R started with LC_ALL=C; compare.js with collation "C")
dir.create(file.path(root, "cases_C"), showWarnings = FALSE)
for (cs in c("c07_large", "c09_alnum_ids_noinfo", "c12_fuzzy_names")) {
  dir.create(file.path(root, "cases_C", cs), showWarnings = FALSE)
  for (j in 1:2) {
    f <- file.path(root, "cases", cs, paste0("config_", j, ".json")); if (!file.exists(f)) next
    cf <- fromJSON(f); cf$outdir <- file.path(normalizePath(file.path(root, "cases_C", cs)), paste0("r_", j))
    writeLines(toJSON(cf, auto_unbox = TRUE, null = "null"), file.path(root, "cases_C", cs, paste0("config_", j, ".json")))
  }
}
