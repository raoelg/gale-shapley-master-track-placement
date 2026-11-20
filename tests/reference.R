# reference.R -- non-Shiny reference implementation of the server logic of the
# "Optimal Master Track Placement" Shiny app (app.R + GaleShapely.R), used to
# verify the browser port (app.js) numerically.
#
# Usage:  Rscript reference.R config.json
# config.json fields (all optional except appfile/outdir):
#   appfile, quotafile (or null), seed (20180228), samplekind ("Rejection"|"Rounding"),
#   header1 (true), header3 (true), qlevels ("E, V, O"), withdrawrate (10),
#   student/track/preference/qualification (column names; default = the app's auto-guess,
#   "" = nothing selected), remove_cols ([]), waitlist (true), outdir, galeshapely (path)
#
# The pipelines below are copied from app.R as literally as possible.  Shiny-only
# constructs (reactiveValues, input$..., showNotification, renderXXX) are replaced by
# plain variables / functions.  Modernisation changes are marked with "## CHANGED".

suppressPackageStartupMessages({
  library(tidyverse, warn.conflicts = FALSE)
  library(readxl)
  library(jsonlite)
})
options(warn = 1, dplyr.summarise.inform = FALSE)

args <- commandArgs(trailingOnly = TRUE)
cfg <- fromJSON(args[1], simplifyVector = TRUE)
`%||%` <- function(a, b) if (is.null(a)) b else a
gs_path <- cfg$galeshapely %||% file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), "orig", "GaleShapely.R")
source(gs_path)

outdir <- cfg$outdir
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
out <- function(name, x) {
  if (is.data.frame(x)) x <- as.data.frame(lapply(x, function(v) if (is.factor(v)) as.character(v) else v),
                                          stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  writeLines(toJSON(x, dataframe = "columns", matrix = "rowmajor", na = "null", digits = NA,
                    auto_unbox = FALSE, null = "null"), file.path(outdir, paste0(name, ".json")), useBytes = TRUE)
}
out_df <- function(name, x) {  # data frame incl. column order
  if (is.null(x)) return(out(name, NULL))
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  out(name, list(names = names(x), data = x))
}
out_mat <- function(name, m) out(name, list(colnames = colnames(m), nrow = nrow(m), data = unname(m)))
showNotification <- function(...) invisible(NULL)
PATCH <- isTRUE(cfg$patch)  # FALSE = original behaviour; TRUE = apply the 3 minimal fixes marked "## PATCH"
errors <- list()

RNGkind(sample.kind = cfg$samplekind %||% "Rejection")
input <- list(iseed = cfg$seed %||% 20180228, header1 = cfg$header1 %||% TRUE, header3 = cfg$header3 %||% TRUE,
              qlevels = cfg$qlevels %||% "E, V, O", withdrawrate = cfg$withdrawrate %||% 10,
              remove_cols = cfg$remove_cols %||% character(0))
val <- new.env()
val$quota <- NULL; val$Waitlist <- NULL

## ---- observe(): read file 1 and guess column mapping ------------------------------
inFile <- list(datapath = cfg$appfile, name = basename(cfg$appfile))
if (grepl("\\.xlsx?$", inFile$datapath)) {
  dat = read_excel(inFile$datapath, sheet = 1, col_names = TRUE, col_types = "text", trim_ws = TRUE) %>%
    data.frame() # automatically adjust the column names in the way read.csv does
} else {
  dat = read.csv(inFile$datapath, header = input$header1, check.names = T, colClasses = "character")
}
nms = names(dat)
selected = function(pattern, x, y=tolower(x)) {z=x[agrep(pattern, y)]; if(length(z)>0) z[which.min(adist(pattern, z))]}
guess <- list(student = selected("student", nms) %||% "", track = selected("track", nms) %||% "",
              preference = if ("Preference" %in% nms) "Preference" else "",
              qualification = selected("qualification", nms) %||% "")
out("guess", lapply(guess, unbox))
out_df("dat0", dat)
for (k in c("student", "track", "preference", "qualification"))
  input[[k]] <- if (!is.null(cfg[[k]])) cfg[[k]] else guess[[k]]
val$dat0 = dat

## ---- output$contents0 ---------------------------------------------------------------
contents0 <- function() {
    dat = val$dat0

    dat = dat[, !(names(dat) %in% input$remove_cols)]
    names(dat)[match(input$student, names(dat))] = "StudentID"
    names(dat)[match(input$track, names(dat))] = "Track"
    names(dat)[match(input$preference, names(dat))] = "Preference"
    names(dat)[match(input$qualification, names(dat))] = "Qualification"

    if (!is.numeric(dat$Qualification)) {
      qlevels = trimws(strsplit(input$qlevels, split = ",")[[1]])
      if (! all(dat$Qualification %in% qlevels)) {
        unique.found = unique(dat$Qualification)
        Found = Expected = rep(NA, max(length(unique.found), length(qlevels)))
        Found[1:length(unique.found)] = unique.found
        Expected[1:length(qlevels)] = qlevels
        d <- tibble(`Values Found` = paste("Qualification column is non-numeric and contains values not listed as levels. ",
                                      "Qualification column should be numerical, or levels should be specified. ",
                                      "Check Qualification column selection and qualification levels specification."),
                        `Values Expected` = "")
        d <- rbind(d, tibble(`Values Found` = Found, `Values Expected` = Expected))
        return(d)
      }
      dat = dat %>% mutate(Qualification = factor(as.character(Qualification) %>% trimws, qlevels, ordered = TRUE))
      showNotification("Qualification turned into factor", duration = 1)
    }

    dat = if (is.null(dat$Preference)) {
      dat %>% group_by(StudentID) %>% mutate(Preference = 1:n())
    } else {
      showNotification("Arranged by Preference.", duration = 1)
      dat %>% group_by(StudentID) %>% arrange(as.numeric(Preference)) %>% mutate(Preference = 1:n())
    }

    n0 = nrow(dat)
    showNotification(paste(colnames(dat), collapse = ", "),duration = 5)
    dat = dat %>% ungroup %>% dplyr::filter(., !duplicated(select(dat, StudentID, Track)))
    if(nrow(dat) != n0)
      showNotification("Duplicate rows (with potentially inconsistent Qualification and/or Preference) were removed from the data set. (First occurences were kept.)", duration = NULL, type = "warning")
    val$nduplicates <- n0 - nrow(dat)

    cleanup = function(x) paste0(unique(x %>% as.character %>% trimws %>% if_else(is.na(.),"",.)), collapse = "|")
    dat2 <-
      dat %>% as_tibble() %>%
      group_by(StudentID) %>%
      mutate_at(vars(-Track, -Preference, -Qualification), funs(cleanup(.))) %>%
      mutate(StudentID2 = if (ncol(.) > 4) rep("",n()) else StudentID) %>%
      unite(Info, -StudentID, -Track, -Preference, -Qualification, sep=":") %>%
      ungroup() %>%
      mutate(Info = gsub(":\\s*$","", Info)) %>%
      mutate(Info = if (length(unique(Info)) < length(unique(StudentID))) paste(StudentID, Info, sep = ":") else Info) %>%
      select(StudentID, Info, Track, Preference, Qualification) %>%
      mutate_if(is.character, trimws)

    set.seed(input$iseed) # should work even if seed is not set and is NULL value
    if (!is.numeric(dat2$Qualification))
      qlevels = levels(dat2$Qualification)
    CP <- dat2 %>% select(-Preference) %>%
      filter(if (is.numeric(Qualification)) Qualification > min(Qualification) else Qualification != tail(levels(Qualification),1)) %>%
      arrange(Track, Qualification) %>%
      spread(Track, Qualification) %>%
      gather(College, Collegepref, -Info, -StudentID) %>%
      mutate(Collegepref = if (is.character(Collegepref)) factor(Collegepref, qlevels, ordered=T)) %>%
      arrange(College, Collegepref) %>%
      group_by(College, Collegepref) %>%
      mutate(lottery.rank=sample(n())) %>%
      arrange(College, Collegepref, lottery.rank) %>%
      ungroup() %>%
      group_by(College) %>%
      mutate(Collegepref = 1:n()) %>%
      select(-StudentID, - lottery.rank) %>%
      spread(College, Info) %>%
      select(-Collegepref) -> CPtest

    SP <- dat2 %>% select(-Preference) %>%
      filter(if (is.numeric(Qualification)) Qualification > min(Qualification) else Qualification != tail(levels(Qualification),1)) %>%
      group_by(StudentID) %>%
      mutate(Studentpref = 1:n(), Track = trimws(Track)) %>%
      ungroup() %>%
      select(-Qualification, -StudentID) %>%
      spread(Info, Track) %>%
      select(-Studentpref) %>%
      as.matrix %>%
      rbind(., matrix(NA, ncol(CP) - nrow(.), ncol(.))) -> SPtest

    val$studentPrefs <- as.matrix(SP)
    val$collegePrefs <- as.matrix(CP)
    val$dat = dat2
    dat2
}
c0 <- tryCatch(contents0(), error = function(e) { errors$contents0 <<- conditionMessage(e); NULL })
out_df("contents0", c0)
if (!is.null(val$collegePrefs)) {
  out_mat("CP", val$collegePrefs); out_mat("SP", val$studentPrefs)
  out("nduplicates", unbox(val$nduplicates))
}

## ---- output$contents3 ---------------------------------------------------------------
contents3 <- function() {
    inFile <- if (is.null(cfg$quotafile)) NULL else list(datapath = cfg$quotafile)
    if (is.null(inFile))
      return(NULL)
    if (grepl("\\.xlsx?$", inFile$datapath)) {
      val$quota <-
        read_excel(inFile$datapath, sheet = 1, col_names = TRUE, col_types = "numeric") %>%
        as.matrix() %>% head(1)
    } else {
      val$quota <- head(as.matrix(read.csv(inFile$datapath, header = input$header3, check.names = FALSE)), 1)
    }
    if (!is.null(val$collegePrefs)) {
      namesFoundApplication = colnames(val$collegePrefs)
      namesFoundQuota = colnames(val$quota)
      m = max(length(namesFoundApplication), length(namesFoundQuota))
      namesFoundApplication = c(namesFoundApplication, rep(NA, m))[1:m]
      namesFoundQuota = c(namesFoundQuota, rep(NA, m))[1:m]
      if (!all(namesFoundApplication %in% namesFoundQuota)) {
        showNotification("Found track names in student applications that have no quota specified. Please correct.", duration=NULL, type = "error")
        return(tibble(`Track names found in applications` = namesFoundApplication, `Track names found in quota` = namesFoundQuota))
      }
    }
    val$quota
}
c3 <- tryCatch(contents3(), error = function(e) { errors$contents3 <<- conditionMessage(e); NULL })
out_df("contents3", c3)

## ---- output$myText ------------------------------------------------------------------
myText <- function() {
    if (is.null(val$collegePrefs) || is.null(val$studentPrefs))
      return(NULL)
    studentPrefs = apply(val$studentPrefs, 2, match, colnames(val$collegePrefs))
    collegePrefs = apply(val$collegePrefs, 2, match, colnames(val$studentPrefs))
    quota = if (is.null(val$quota)) ncol(studentPrefs) else as.integer(val$quota[,colnames(val$collegePrefs)])
    ## CHANGED: removed save(VAL, file="val.Rdata") (side effect only)
    val$Appl <- flexGaleShapely(studentPrefs, collegePrefs, quota)
    val$Waitlist <- NULL
    val$datAugmented <-
      left_join(
        val$dat %>% mutate(Qualification = Qualification),
        val$Appl %>% mutate(Accepting_Track = college.name),
        c(Info = "student.name")
      ) %>%
      mutate(
        Qualified = if (is.numeric(Qualification)) Qualification > min(Qualification) else Qualification != tail(levels(Qualification),1),
        Accepted = ifelse(Accepting_Track == Track, "yes", "no"),
        Reason = case_when(
          !Qualified ~ "Not qualified",
          Qualified & not.onlist ~ "Ranking & capacity",
          !not.onlist ~ paste0("Accepted in ", Accepting_Track)
        )
      ) %>%
      left_join(
        val$Appl %>% group_by(college.name) %>% summarise(Highest_accepted = max(collegepref), Quotum = max(quota)),
        c("Track" = "college.name")
      ) %>%
      left_join(
        val$collegePrefs %>% as.table() %>%
          data.frame(stringsAsFactors = F) %>%
          select(-1) %>%
          group_by(Var2) %>%
          mutate(Track=Var2, Info=as.character(Freq), Rank = row_number()) %>%
          select(Track:Rank)
      )
    val$datAugmented %>%
      select(StudentID, Info, Track, Qualification, Preference, Rank, Highest_accepted, Quotum, Accepted, Reason)
}
res <- tryCatch(myText(), error = function(e) { errors$myText <<- conditionMessage(e); NULL })
out_df("result", res)
if (!is.null(val$Appl)) out_df("Appl", val$Appl)

## ---- output$details -----------------------------------------------------------------
details <- function() {
    if (is.null(val$Appl))
      return(NULL)
    studentPrefs = apply(val$studentPrefs, 2, match, colnames(val$collegePrefs))
    collegePrefs = apply(val$collegePrefs, 2, match, colnames(val$studentPrefs))
    quota = if (is.null(val$quota)) ncol(studentPrefs) else as.numeric(val$quota[,colnames(val$collegePrefs)])
    Colleges = data.frame(Other_College = colnames(collegePrefs), college.other = 1:ncol(collegePrefs))
    Appl = val$Appl %>% filter(!is.na(not.onlist))
    betterMatches(studentPrefs, collegePrefs, Appl) %>%
      left_join(Colleges, by = "college.other") %>%
      select(Student = student.name, `Accepting College` = college.name, `Student Preference` = studpref.accepted,
             `Accepting College Preference` = collegepref.accepted,
             `Other College` = Other_College,
             `Student Preference Other` = studpref.other,
             `Other College Preference` = collegepref.other,
             `Lowest Preference Accepted` = maxcollegepref.other
      )
}
det <- tryCatch(details(), error = function(e) { errors$details <<- conditionMessage(e); NULL })
out_df("details", det)

## ---- summary plots: the data behind them ----------------------------------------------
if (!is.null(val$Appl)) {
  bar <- val$Appl %>% count(studpref)
  out_df("summary_bar", bar)
  pie <- val$Appl %>% filter(!is.na(not.onlist)) %>% group_by(studpref) %>% summarise(count = n()) %>%
    mutate(percentage = 100*count/sum(count))
  out_df("summary_pie", pie)
}

## ---- collegeAdmissions (as list of character vectors instead of HTML tags) -------------
collegeAdmissions <- function() {
    if (is.null(val$Appl))
      return(NULL)
    Wait = NULL
    if (!is.null(val$Waitlist))
      Wait = val$Waitlist %>% select(-`<NA>`, -college.name) %>%
      gather(college.name, `Post placement chance`, -student, -student.name, -not.onlist) %>%
      select(student.name, college.name, `Post placement chance`)  %>%
      filter(`Post placement chance` > 0) %>%
      arrange(desc(`Post placement chance`))
    if (!is.null(Wait)) {
      Appl = val$Appl %>% full_join(Wait) %>% filter(!is.na(not.onlist) | !is.na(`Post placement chance`))
      Appl = Appl %>% ungroup %>% mutate(student.name = ifelse(is.na(`Post placement chance`),
                student.name, paste0("(", student.name, ") [", `Post placement chance`, "%]")))
    } else {
      Appl = val$Appl %>% filter(!is.na(not.onlist))
    }
    x <- split(as.character(Appl$student.name), Appl$college.name)
    list(headers = vapply(names(x), function(nm) paste0(nm, " [", paste(val$quota[1, nm]), "]"), ""),  ## CHANGED: paste() so a NULL quota gives "" (renderPrint/tags did the same)
         lists = unname(x))
}
ca <- tryCatch(collegeAdmissions(), error = function(e) { errors$collegeAdmissions <<- conditionMessage(e); NULL })
out("admissions", ca)

## ---- downloadData -------------------------------------------------------------------
download <- function(file) {
      if (is.null(val$Appl))
        return(NULL)
      Appl2 = val$datAugmented
      dat0 = val$dat0 %>% select(-one_of(c(input$qualification, input$preference)))
      if (is.null(val$Waitlist)) {
        Appl2 = Appl2 %>%
          select(StudentID, Info, Track, Qualification, Preference, Rank, Highest_accepted, Quotum, Accepted, Reason) %>%
          left_join(dat0, ., by = c(StudentID = input$student, Track = input$track) %>% structure(names(.), .Names = .))
        if (PATCH) Appl2 <- Appl2 %>% rename(Track = all_of(input$track))  ## PATCH (optional): app.R fails unless the track column is called "Track"
        write.csv(Appl2 %>% arrange(Track, Info) %>% { if (PATCH) rename(., !!input$track := Track) else . }, file, row.names = FALSE, fileEncoding = "UTF-16LE", na = "")
      } else {
        Appl2 = Appl2 %>%
          left_join(
            val$Waitlist %>% select(-`<NA>`) %>%
              gather(Track, `Post placement chance`, -student, -student.name, -not.onlist, -college.name) %>%
              mutate(Info=student.name) %>% select(Info, Track, `Post placement chance`) %>%
              filter(`Post placement chance` > 0)
          ) %>%
          select(StudentID, Info, Track, Qualification, Preference, Rank, Highest_accepted, Quotum, Accepted, Reason, `Post placement chance`) %>%
          left_join(dat0, ., by = c(StudentID = input$student, Track = input$track) %>% structure(names(.), .Names = .))
        if (PATCH) Appl2 <- Appl2 %>% rename(Track = all_of(input$track))  ## PATCH (optional), see above
        write.csv(Appl2 %>% arrange(Track, Info) %>% { if (PATCH) rename(., !!input$track := Track) else . }, file, row.names = FALSE, fileEncoding = "UTF-16LE", na = "")
      }
}
tryCatch(download(file.path(outdir, "download.csv")), error = function(e) errors$download <<- conditionMessage(e))

## ---- output$waitlist (RNG stream continues after the lottery) ------------------------
waitlist <- function() {
    if (is.null(val$Appl))
      return(NULL)
    studentPrefs2 = apply(val$studentPrefs, 2, match, colnames(val$collegePrefs))
    collegePrefs2 = apply(val$collegePrefs, 2, match, colnames(val$studentPrefs))
    quota2 = if (is.null(val$quota)) { if (PATCH) ncol(studentPrefs2) else ncol(studentPrefs) } else as.numeric(val$quota[,colnames(val$collegePrefs)])  ## PATCH (optional): `studentPrefs` is undefined here
    Appl2 = val$Appl %>% filter(!is.na(not.onlist))
    nsim = 20
    nwithdrawals = nrow(collegePrefs2) * as.numeric(input$withdrawrate) / 100
    y = replicate(nsim, {
      dropouts = Appl2[sample(which(!Appl2$not.onlist), nwithdrawals),]
      collegePrefs2DO = apply(apply(collegePrefs2, 2, setdiff, dropouts$student), 2, rank)
      studentPrefs2DO = if (PATCH) studentPrefs2[, setdiff(seq_len(ncol(studentPrefs2)), dropouts$student), drop=F] else  ## PATCH (optional): -integer(0) selects no columns
        studentPrefs2[,-dropouts$student, drop=F]
      Appl2DO = flexGaleShapely(studentPrefs2DO, collegePrefs2DO, quota2)
      Appl2DO %>% select(student.name, college.name) %>%
        mutate(student.name = as.character(student.name), college.name = as.character(college.name))
    }, simplify = F)
    val$Waitlist = Reduce(function(u,v) left_join(x = u, v, by="student.name", suffix=c("",".y")), y) %>%
      left_join(., Appl2 %>% select(student, student.name)) %>%
      arrange(student) %>%
      gather(sim, college.name, -student.name, -student) %>%
      group_by(student, student.name, college.name) %>%
      summarise(count=n()) %>%
      arrange(student, count) %>%
      spread(college.name, count, fill=0) %>%
      ungroup %>%
      mutate_at(vars(-student, -student.name), all_vars(100*round(./nsim,2))) %>%
      left_join(Appl2 %>% select(student.name, not.onlist, college.name), by="student.name") %>%
      filter_at(vars(-student, -student.name, -not.onlist, -college.name), all_vars(!not.onlist & ((.+`<NA>`) < 100) | not.onlist & `<NA>` < 100))
    val$Waitlist
}
if (isTRUE(cfg$waitlist %||% TRUE) && !is.null(val$Appl)) {
  out("rng_before_waitlist", .Random.seed)
  wl <- tryCatch(waitlist(), error = function(e) { errors$waitlist <<- conditionMessage(e); NULL })
  out_df("waitlist", wl)
  if (!is.null(val$Waitlist)) {
    ca2 <- tryCatch(collegeAdmissions(), error = function(e) { errors$collegeAdmissions2 <<- conditionMessage(e); NULL })
    out("admissions_wl", ca2)
    tryCatch(download(file.path(outdir, "download_wl.csv")), error = function(e) errors$download_wl <<- conditionMessage(e))
  }
}
out("errors", lapply(errors, unbox))
