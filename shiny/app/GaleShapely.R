#### A function that implements the most flexible Gale-Shapely algorithm: ####

# prefsStudents: columns correspond to student ids, rows to preference rank, cells indicate the college id 
#                (e.g., 2nd entry in 1st column contains college ID of second prefered college of student '1')
# prefsColleges: columns correspond to college ids, rows to preference rank, cells indicate the student id
#                (e.g., 3rd entry in 2nd column contains the student ID of the student that is the 3rd prefered student of college '2')
# collegeQuotas: for each college (column in prefColleges) the number of vacancies
flexGaleShapely <- function(prefsStudents, prefsColleges, collegeQuotas = ncol(prefsStudents)) {
  stopifnot(require(dplyr))
  
  # check the input
  # ... to be implemented yet...
  if (NCOL(prefsStudents) != NROW(prefsColleges) || NROW(prefsStudents) != NCOL(prefsColleges) || 
      !is.numeric(prefsStudents) || !is.numeric(prefsColleges) || !is.matrix(prefsStudents) || !is.matrix(prefsColleges))
    stop("'",deparse(substitute(prefsStudents)),"' and '", deparse(substitute(prefsColleges)), "' are not compatible numeric matrices.")
  if (any(is.na(prefsColleges)))
    stop("'", deparse(substitute(prefsColleges)),"' contains missing values.")
  if (any(diff(apply(prefsColleges, 2, sort)) != 1))
    stop("'", deparse(substitute(prefsColleges)), "' has columns that are not complete proper orderings of students.")
  if (any(diff(apply(prefsStudents, 2, sort.int, na.last=TRUE)) == 0, na.rm = TRUE))
    stop("'", deparse(substitute(prefsStudents)), "' has columns that are not proper orderings of colleges.")
  
  #
  nstudents = ncol(prefsStudents)
  ncolleges = ncol(prefsColleges)
  quotas = rep(1, ncolleges)
  quotas[] = collegeQuotas # recycle values in collegeQuotas
  
  # augment preference matrices with a 'not placed college' which has equal preference for all students and colleges, and place for all
  notPlacedCode = ncolleges + 1
  studentPrefs = rbind(prefsStudents, notPlacedCode); 
  studentPrefs[is.na(studentPrefs)] = notPlacedCode;
  collegePrefs = prefsColleges; print("collegePrefs assigned")
  collegePrefs = cbind(collegePrefs,1)
  quotaCollege = c(quotas, nstudents)
  
  # turn college prefs rank-indexed encoding into pref student-indexed encoding (assuming definite order of students for each college)
  collegePrefRanks = apply(collegePrefs, 2, order) # columns correspond to colleges, rows to students, cells to student rank within college
  # better use function(x){n=length(x);y=rep(n,n);y[x]=1:n;y} instead of 'order'
  
  # add names for clarity
  snames = if (is.null(colnames(prefsStudents))) as.character(1:nstudents) else colnames(prefsStudents)
  cnames = if (is.null(colnames(prefsColleges))) as.character(1:ncolleges) else colnames(prefsColleges)
  
  # the actual algorithm
  Application = data.frame(student.name = snames, student = 1:nstudents, studpref = 0, not.onlist=TRUE)
  repeat {
    old.studpref = Application$studpref; old.not.onlist = Application$not.onlist
    Application = Application %>%  
      ungroup() %>% 
      mutate(studpref = pmin(studpref+not.onlist,nrow(studentPrefs)), college = studentPrefs[cbind(studpref,student)]) %>%
      mutate(collegepref = collegePrefRanks[cbind(student, college)]) %>%
      group_by(college) %>%
      arrange(collegepref) %>%
      mutate(rank.in.college = 1:n(), quota = quotaCollege[college]) %>%
      mutate(not.onlist = rank.in.college > quota) 
    #print(as.data.frame(Appl %>% arrange(college, rank.in.college) %>% select(student,college,studpref,collegepref,not.onlist)))
    cat("."); flush.console()
    if (!any(Application$not.onlist, na.rm=T) || all(old.studpref==Application$studpref & old.not.onlist == Application$not.onlist)) break
  }; cat("\n")
  Application = Application %>% 
    ungroup %>% 
    mutate(not.onlist = ifelse(college == notPlacedCode, TRUE, not.onlist), college = ifelse(not.onlist, NA, college)) %>%
    left_join(data.frame(college=1:ncolleges, college.name = cnames), by = "college")
  Application
}


### A function that implements the stability test ###

betterMatches <- function(studentPrefs, collegePrefs, Application) {
  studentPrefRanks = apply(studentPrefs, 2, function(x){n=length(x);y=rep(NA,n);i=!is.na(x);y[x[i]]=(1:n)[i];y}) # gebruik van order is minder algemeen geldig!
  collegePrefRanks = apply(collegePrefs, 2, order) # colleges hebben complete proper ordeningen
  nstudents = ncol(studentPrefRanks)
  ncolleges = ncol(collegePrefRanks) 
  Appl = Application %>% 
    group_by(college) %>% 
    mutate(collegepref.max=max(collegepref)) 
  collegepref.max = Appl %>% select(college, collegepref.max) %>% unique %>% arrange(college) %>% .$collegepref.max
  Appl = Appl %>% select(student, college, studpref, collegepref, student.name, college.name)
  
  expand.grid(student = 1:nstudents, college = 1:ncolleges) %>%
    mutate(studpref = studentPrefRanks[cbind(college,student)], collegepref = collegePrefRanks[cbind(student,college)]) %>% # all pairwise prefs
    mutate(maxcollegepref.other = collegepref.max[college]) %>% # add max pref for other college
    left_join(Appl, by = "student", suffix = c(".other",".accepted"))  %>% # lookup corresponding matched prefs
    mutate(student.prefers.other.college = studpref.other < studpref.accepted) %>%
    mutate(other.college.prefers.student = collegepref.other <= maxcollegepref.other) %>% 
    mutate(better.match = student.prefers.other.college & other.college.prefers.student) %>%
    filter(!is.na(better.match))
}



# ###########################
# 
# #### test data ###
# nmen = 400
# nwomen = 8
# # generate cardinal utilities
# uM = matrix(runif(nmen*nwomen), nrow = nwomen, ncol = nmen)
# uW = matrix(runif(nwomen*nmen), nrow = nmen, ncol = nwomen)
# 
# studentPrefs = apply(uM, 2, order) # sortIndex(uM) + 1
# collegePrefs = apply(uW, 2, order) # sortIndex(uW) + 1
# studentPrefs[4:8,] = NA
# 
# # add names for easier understanding of the output
# colnames(studentPrefs) = replicate(nmen, rawToChar(as.raw(sample(c(65+0:25,97+0:25),15))))
# colnames(collegePrefs) = LETTERS[1:nwomen]
# 
# # college quota
# quota = c(10,10,30,30,30,100,25,25)
# 
# #### Run Gale-Shapely algorithm to accept students in colleges respecting their quota ##
# Appl = flexGaleShapely(studentPrefs, collegePrefs, quota)
# betterMatches(studentPrefs, collegePrefs, Appl) %>% filter(student==1)


