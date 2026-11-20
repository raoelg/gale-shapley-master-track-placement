#
# This is Shiny web application interfaces the Gale-Shapely algorithm to
# run it on a college application data set
#
# Raoul Grasman (c) University of Amsterdam
#

library(tidyverse, warn.conflicts = F)
library(shiny)
library(readxl)
source("GaleShapely.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Optimal Master Track Placement"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tags$h3("Track placement"),
        numericInput("iseed", "Number to use as shuffling seed.",min=0,step=1,value=20180228),
        fileInput("file1", "Upload Student Applications (xls/csv)", 
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".xlsx",
                    ".xls")
        ),
        checkboxInput("header1", "Header", TRUE),
        # tags$p("Student applications must have columns: Student, Track, Preference, Qualification"),
        selectInput("student", label = "StudentID", choices = c("")),
        selectInput("track", label = "Track", choices = c("")),
        selectInput("preference", label = "Preference", choices = c("")),
        selectInput("qualification", label = "Qualification", choices = c("")),
        textInput("qlevels", label = "Qualification levels", value = "E, V, O"),
        tags$p(tags$b("Note:"),"Order levels top to bottom, separated by commas. Ignored if Qualification column is quantitative. The last level is interpreted as 'unqualified' and causes the corresponding preference to be ignored.", style="font-size:small"),
        selectInput("remove_cols", "Ignore columns", choices = "", multiple = TRUE),
        tags$hr(),
        fileInput("file3", "Upload Master Track Quota (xls/csv)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".xlsx",
                    ".xls")
        ),
        checkboxInput("header3", "Header", TRUE),
        tags$p("Track quota: Columns correspond to Track IDs, row 1 contains track quota."),
        tags$hr(),
        tags$h3("Waiting list"),
        sliderInput("withdrawrate",
                     "Percentage withdrawal:",
                     min = 0.5,
                     max = 30,
                     value = 10),
        tags$p("Percentage of expected withdrawals after acceptance. Used in waitlist computations.")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Imported Student Applications", HTML("<h4>Quotas</h4>"), tableOutput("contents3"), HTML("<h4>Applications</h4>"), dataTableOutput("contents0")),
          # tabPanel("Student preferences", dataTableOutput("contents1")),
          # tabPanel("College preferences", HTML("<h4>Quotas</h4>"), tableOutput("contents3"), HTML("<h4>Preferences</h4>"), dataTableOutput("contents2")),
          tabPanel("Result", column(10,""), fluidRow(column(2,downloadButton("downloadData", "Download"))), tags$hr(), dataTableOutput("myText")),
          tabPanel("Summary", fluidRow(column(6, plotOutput("summary")), column(6,plotOutput("summary2"))), htmlOutput("collegeAdmissions", container=tags$p)),
          tabPanel("Details", dataTableOutput("details")),
          tabPanel("Waiting list", HTML("<h4>Waiting list</h4><p>Numbers under college names give indication of chance (%) that a student will be accepted in the college after the expected number of placed students have decided to withdraw. </p><p style=text-style:italic;color:gray;>Disclaimer: These probabilities are estimated from a limited set of simulations and hence may vary from one use of this program to the next. The simulations make a very simplistic assumption that student withdrawal is uniform across Tracks and independent from their initial placement. Simulations take some time; please be patient.</p>"), dataTableOutput("waitlist"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  val <- reactiveValues(dat0 = NULL, dat = NULL, collegePrefs = NULL, studentPrefs = NULL, quota = NULL, Appl = NULL, Waitlist = NULL, datAugmented = NULL)
  
  observe({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    if (grepl("\\.xlsx?$", inFile$datapath)) {
      dat = read_excel(inFile$datapath, sheet = 1, col_names = TRUE, col_types = "text", trim_ws = TRUE) %>%
        data.frame() # automatically adjust the column names in the way read.csv does
    } else {
      dat = read.csv(inFile$datapath, header = input$header1, check.names = T, colClasses = "character")
    }
    if (is.null(dat))
      return(NULL)
    
    # guess and map column names
    nms = names(dat)
    selected = function(pattern, x, y=tolower(x)) {z=x[agrep(pattern, y)]; if(length(z)>0) z[which.min(adist(pattern, z))]}
    updateSelectInput(session, "student", choices = nms, selected = selected("student", nms))
    updateSelectInput(session, "track", choices = nms, selected = selected("track", nms))
    updateSelectInput(session, "preference", choices = nms, selected = "Preference") # only guessed if matches exactly 
    updateSelectInput(session, "qualification", choices = nms, selected = selected("qualification", nms))
    
    val$dat0 = dat
  })
  observe({
    if (is.null(val$dat0))
      return(NULL)
    nms = names(val$dat0)
    updateSelectInput(session, "remove_cols", choices = setdiff(nms, c(input$student, input$track, input$preference, input$qualification)))
  })
  
  output$contents0 <- renderDataTable({
    if (is.null(val$dat0))
      return(NULL)
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
    
        
    # dat2 <- 
    #   dat %>% as_tibble() %>% 
    #   group_by(StudentID) %>% # we have to rely on StudentID to be entrered correctly
    #   mutate(Track = ifelse(as.character(Track)=="", gsub("\\d+", "", Uniekeaanmelding), as.character(Track))) %>%
    #   mutate(Achternaam = paste0(unique(Achternaam %>% trimws),collapse = "|")) %>% # normalize 
    #   mutate(Tussenv = paste0(unique(Tussenv %>% trimws),collapse = "|")) %>% # normalize 
    #   mutate(Voornaam = paste0(unique(Voornaam %>% trimws),collapse = "|")) %>% # normalize 
    #   mutate(Track = Track %>% trimws) %>%
    #   #mutate(Track = abbreviate(Track %>% gsub("\\(..\\)|\\W","",.), 3)) %>%
    #   mutate(naam = paste(Voornaam, Tussenv, Achternaam, StudentID, sep=":")) %>%
    #   ungroup %>%
    #   select(StudentID, naam, Track, Preference, Qualification) 
    
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
      #group_by(Track, Qualification) %>% 
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
    
    #rownames(SP) = 1:NROW(SP)
    
    val$studentPrefs <- as.matrix(SP)
    val$collegePrefs <- as.matrix(CP)
    val$dat = dat2
    
    dat2
  })

   output$contents3 <- renderTable({
    inFile <- input$file3 
    
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
  })
  output$myText <- renderDataTable({
    if (is.null(val$collegePrefs) || is.null(val$studentPrefs))
      return(NULL)
    
    studentPrefs = apply(val$studentPrefs, 2, match, colnames(val$collegePrefs))
    collegePrefs = apply(val$collegePrefs, 2, match, colnames(val$studentPrefs))
    quota = if (is.null(val$quota)) ncol(studentPrefs) else as.integer(val$quota[,colnames(val$collegePrefs)])
    VAL = list(collegePrefs = val$collegePrefs, studentPrefs = val$studentPrefs, quota = val$quota)
    save(VAL, file="val.Rdata")
    
    #if (any(collegePrefs == 0)) stop("Encountered a college that ranked a Student ID who didn't apply.")
    
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
      left_join( # add column with highest accepted ranks for each college and the quotum for each college
        val$Appl %>% group_by(college.name) %>% summarise(Highest_accepted = max(collegepref), Quotum = max(quota)),
        c("Track" = "college.name")
      ) %>%
      left_join( # add within college ranks so that it can be compared with the highest accepted rank in each college
        val$collegePrefs %>% as.table() %>%      # compute table of within college ranks
          data.frame(stringsAsFactors = F) %>% 
          select(-1) %>% 
          group_by(Var2) %>% 
          mutate(Track=Var2, Info=as.character(Freq), Rank = row_number()) %>% 
          select(Track:Rank)
      ) 
    val$datAugmented %>%
      select(StudentID, Info, Track, Qualification, Preference, Rank, Highest_accepted, Quotum, Accepted, Reason)
  })
  output$details <- renderDataTable({
    if (is.null(val$Appl))
      return(NULL)
    
    studentPrefs = apply(val$studentPrefs, 2, match, colnames(val$collegePrefs))
    collegePrefs = apply(val$collegePrefs, 2, match, colnames(val$studentPrefs))
    quota = if (is.null(val$quota)) ncol(studentPrefs) else as.numeric(val$quota[,colnames(val$collegePrefs)])
    
    #Colleges = data.frame(Other_College = factor(colnames(collegePrefs))) %>% 
    # mutate(college.other=as.numeric(Other_College)) ## this is wrong? match() matches each value in studentPrefs to the dictionary colnames(collegePrefs) and returns the index of the match (!)
    Colleges = data.frame(Other_College = colnames(collegePrefs), college.other = 1:ncol(collegePrefs)) # so this must be correct
    
    Appl = val$Appl %>% filter(!is.na(not.onlist))
    betterMatches(studentPrefs, collegePrefs, Appl) %>%
      left_join(Colleges, by = "college.other") %>% #filter(student == 1) %>%
      #filter(!is.na(studpref.other)) %>%
      select(Student = student.name, `Accepting College` = college.name, `Student Preference` = studpref.accepted, 
             `Accepting College Preference` = collegepref.accepted, 
             `Other College` = Other_College,
             `Student Preference Other` = studpref.other,
             `Other College Preference` = collegepref.other, 
             `Lowest Preference Accepted` = maxcollegepref.other
      ) 
  })
  output$waitlist <- renderDataTable({
    if (is.null(val$Appl))
      return(NULL)
    
    studentPrefs2 = apply(val$studentPrefs, 2, match, colnames(val$collegePrefs))
    collegePrefs2 = apply(val$collegePrefs, 2, match, colnames(val$studentPrefs))
    quota2 = if (is.null(val$quota)) ncol(studentPrefs) else as.numeric(val$quota[,colnames(val$collegePrefs)])
    Appl2 = val$Appl %>% filter(!is.na(not.onlist))
    
    nsim = 20
    nwithdrawals = nrow(collegePrefs2) * as.numeric(input$withdrawrate) / 100
    
    y = replicate(nsim, {
      dropouts = Appl2[sample(which(!Appl2$not.onlist), nwithdrawals),]
      collegePrefs2DO = apply(apply(collegePrefs2, 2, setdiff, dropouts$student), 2, rank)
      studentPrefs2DO = studentPrefs2[,-dropouts$student, drop=F]
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
      #filter_at(vars(BCS:`<NA>`), all_vars(. < nsim)) %>%
      mutate_at(vars(-student, -student.name), all_vars(100*round(./nsim,2))) %>%
      left_join(Appl2 %>% select(student.name, not.onlist, college.name), by="student.name") %>% 
      filter_at(vars(-student, -student.name, -not.onlist, -college.name), all_vars(!not.onlist & ((.+`<NA>`) < 100) | not.onlist & `<NA>` < 100))
    
    val$Waitlist
  })
  output$summary <- renderPlot({
    require(RColorBrewer)
    if (is.null(val$Appl))
      return(NULL)
    
    Appl = val$Appl #%>% filter(!is.na(not.onlist))
    #prefDistr = table(Appl$studpref)
    #barplot(prefDistr, xlab = "Rank", main = "Number of students assigned to their Rank-th choice")
    #Appl %>% group_by(studpref) %>% summarise(count = n()) %>%
    #  ggplot(aes(x = studpref, y = count)) + geom_bar(stat = "identity") +
    Appl %>% mutate(`Student Preference` = as.factor(studpref)) %>% 
      ggplot(aes(`Student Preference`)) + geom_bar() +
      #geom_text(aes(label=count), vjust=1.6, color="white") +
      #scale_x_continuous(breaks = unique(Appl$studpref)) +
      ggtitle("Number of students assigned to their r-th choice") + 
      labs(x = expression(r))
  })
  output$summary2 <- renderPlot({
    if (is.null(val$Appl))
      return(NULL)
    
    Appl = val$Appl %>% filter(!is.na(not.onlist))
    #prefDistr = table(Appl$studpref)
    #barplot(prefDistr, xlab = "Rank", main = "Number of students assigned to their Rank-th choice")
    
    colourCount = max(nlevels(Appl$college.name) + 1, 3)
    getPalette = colorRampPalette(rev(brewer.pal(9, "Blues")))
    
    Appl %>% group_by(studpref) %>% summarise(count = n()) %>% 
      mutate(percentage = 100*count/sum(count), rank=factor(studpref,rev(studpref))) %>% 
      ggplot(aes("", percentage, fill=rank)) + 
      geom_bar(stat='identity', width = 1) + 
      geom_text(aes(y = percentage/2 + c(0,cumsum(percentage)[-length(percentage)]), 
                    label=paste0(round(percentage,1),"%")), color = "darkblue", size=4, nudge_x = 0.7) + 
      scale_fill_manual(values = getPalette(colourCount)) + # scale_fill_brewer(palette = "Blues") + 
      coord_polar("y") + theme_minimal()
  })
  output$collegeAdmissions <- renderPrint({ 
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

    split(as.character(Appl$student.name), Appl$college.name) %>% 
      lapply(as.list) %>% 
      lapply(lapply,tags$li) %>% 
      lapply(tags$ol) %>% 
      #(function(x) lapply(names(x), function(nm) 
      #  tags$div(
      #      tags$h6(paste0(nm, " [", if (nm %in% colnames(val$quota)) val$quota[1, nm] else "?", "]")), 
      #      x[nm])
      #  )
      # )
      (function(x) lapply(names(x), function(nm) tags$div(tags$div(tags$h6(paste0(nm, " [",val$quota[1, nm],"]")),x[nm]), style = "float:left; padding: 5px"))) %>% 
      do.call(tags$div,.)
    
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub("\\.\\w+$", "", input$file1), "_placement.csv", sep = "")
    },
    content = function(file) {
      
      if (is.null(val$Appl)) 
        return(NULL)
      
      Appl2 = val$datAugmented
      dat0 = val$dat0 %>% select(-one_of(c(input$qualification, input$preference)))
      
      if (is.null(val$Waitlist)) {
        Appl2 = Appl2 %>%
          select(StudentID, Info, Track, Qualification, Preference, Rank, Highest_accepted, Quotum, Accepted, Reason) %>%
          left_join(dat0, ., by = c(StudentID = input$student, Track = input$track) %>% structure(names(.), .Names = .)) 
  
        
        write.csv(Appl2 %>% arrange(Track, Info), file, row.names = FALSE, fileEncoding = "UTF-16LE", na = "")
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
          
        
        write.csv(Appl2 %>% arrange(Track, Info), file, row.names = FALSE, fileEncoding = "UTF-16LE", na = "")
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

