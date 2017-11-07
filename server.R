
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(mlr)
library(gridExtra)


shinyServer(function(input, output, session) {
  
  ###############################
  datasetInput <- function(){
    inFile <- input$file1
    #cat(paste0("FILEEEILRERAKHDKHDASD",inFile), "\n\n")
    if(input$sep == ",")
      data <- read.csv(inFile$datapath, 
                       na.strings = c("",'Ab', 'ab', 'AB'), check.names = FALSE)
    
    else if(input$sep == "\t")
      data <- read.table(inFile$datapath, 
                         header =T,
                         na.strings = c("",'Ab', 'ab', 'AB'), 
                         sep="\t", check.names = FALSE)
    
    if(colnames(data)[1]=="Roll_no" && colnames(data)[2]=="Name"){
      for(i in 3:ncol(data)){
        data[is.na(data[,i]), i] <- input$ab
      }
    }
    else if(colnames(data)[1]=="Roll_no" && colnames(data)[2] !="Name"){
      for(i in 2:ncol(data)){
        data[is.na(data[,i]), i] <- input$ab
      }
    }
    else if(colnames(data)[1]=="Name"){
      for(i in 2:ncol(data)){
        data[is.na(data[,i]), i] <- input$ab
      }
    }
    
    s <- rep(0,nrow(data))
    if(colnames(data)[1]=="Roll_no" && colnames(data)[2]=="Name"){
      for(i in 3:ncol(data)){
        j <- colnames(data)[i]
        r <- gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]])
        s <- s + data[,i]*as.numeric(r[2])/as.numeric(r[1])
      }
    }
    else if(colnames(data)[1]=="Roll_no" && colnames(data)[2] !="Name"){
      for(i in 2:ncol(data)){
        j <- colnames(data)[i]
        r <- gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]])
        s <- s + data[,i]*as.numeric(r[2])/as.numeric(r[1])
      }
    }
    else if(colnames(data)[1]=="Name"){
      for(i in 2:ncol(data)){  
        j <- colnames(data)[i]
        r <- gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]])
        s <- s + data[,i]*as.numeric(r[2])/as.numeric(r[1])
      }
    }
    data$Total_marks <- s
    
    m <- mean(data$Total_marks)
    s <- sd(data$Total_marks)
    
    ds <- c(m)
    i <- 0
    while(m - (input$de)*s >=0){
      ds <- c(ds,(m -(input$de)*s))
      m <- m- (input$de)*s
      i <- i+1
    }
    
    m <- mean(data$Total_marks)
    while(m + (input$de)*s <= 100){
      ds <- c(ds,(m + (input$de)*s))
      m <- m + (input$de)*s
    }
    ds <- round(ds, digits = 0)
    ds <- sort(ds, decreasing = TRUE)
    gr <- c("A","AB","B","BC","C","CD","D","F")
    
    
    #cat(paste0("PREVIOUS ds=",ds),"\n")
    while(length(ds) > 7){
      ds <- ds[1:(length(ds)-1)]
    }
    len <- length(ds)
    #cat(paste0("ds=",ds),"\n")
    g <- gr[1:(len+1)]
    #cat(paste0("g=",g),"\n")
    
    l <- c()
    
    for(i in 1:nrow(data)){
      n <- 0
      for(j in 1:length(ds)){
        if(data$Total_marks[i] >= ds[j]){
          l <- c(l, g[j])
          #cat(paste0("Marks:::",data$Total_marks[i],":",g[j]), "\n")
          break
        }
        else if(data$Total_marks[i] < ds[length(ds)]){
          l <- c(l, g[length(g)])
          #      cat(paste0("Mark",data$Total_marks[i],":",g[length(g)]), "\n")
          break
        }
      }
    }
    cat(paste0("mean:",mean(data$Total_marks)),"\n")
    cat(paste0("sd:",s),"\n")
    cat(paste0("mu-s:",ds),"\n")
    
    data$Grade <- l
    
    if (is.null(inFile))
      return(NULL)
    else
      return(data)
  }
  #################################
  
  output$counter <- renderPlotly({
    cat("COUNTER","\n\n\n")
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$dataset =="Relative")
      dat<-datasetInput()
    else
      dat <- datasetAbsol()
    
    
    m <- mean(dat$Total_marks)
    s <- sd(dat$Total_marks)
    
    ds <- c(m)
    i <- 0
    while(m - (input$de)*s >=0){
      ds <- c(ds,(m -(input$de)*s))
      m <- m- (input$de)*s
      i <- i+1
    }
    
    m <- mean(dat$Total_marks)
    while(m + (input$de)*s <= 100){
      ds <- c(ds,(m + (input$de)*s))
      m <- m + (input$de)*s
    }
    ds <- round(ds, digits = 0)
    ds <- sort(ds, decreasing = TRUE)
    gr <- c("A","AB","B","BC","C","CD","D","F")
    
    
    #cat(paste0("PREVIOUS ds=",ds),"\n")
    while(length(ds) > 7){
      ds <- ds[1:(length(ds)-1)]
    }
    len <- length(ds)
    #cat(paste0("ds=",ds),"\n")
    g <- gr[1:(len+1)]
    #cat(paste0("g=",g),"\n")
    ds <- sort(ds)
    cat(paste0("ds::::",ds))
    d <- data.frame("Marks_Range"=paste0("0-",ds[1]), "Grade"=g[length(g)])
    
    for(i in 1:(length(ds)-1)){
      mr <- paste0(ds[i],"-",ds[i+1])
      
      grad <- g[length(g) - i]
      print(data.frame("Marks_Range"=mr, 
                       "Grade"=grad))
      d <- rbind(d, data.frame("Marks_Range"=mr, 
                               "Grade"=grad))
    }
    d <- rbind(d, data.frame("Marks_Range"=paste0(ds[length(ds)],"-100"), 
                             "Grade"=g[1]))
    print(d)
    
    cc<- as.data.frame(table(dat$Grade))
    #ct <- count(cc)
    if(nrow(d) != nrow(cc)){
      for(i in d$Grade){
        if (i %in% cc$Var1){}
        else{
          cc <- rbind(cc, data.frame("Var1"=i,"Freq"=0))
        }
      }
    }
    
    u <- merge(d,cc,by.x="Grade", by.y="Var1")
    print(cc)
    print(d)
    print(u)
    d <- as.data.frame(table(dat$Range))
    
    plot_ly(
      x = u$Marks_Range, #d$Var1,
      y = u$Freq,
      name = "",
      type = "bar",
      text = paste("Number of Students:", u$Freq)
    ) %>%
      layout(title = "Marks Distribution",
             xaxis = list(title = "Range"),
             yaxis = list(title = "Number of Students"),
             annotations = list(x = u$Marks_Range, y = u$Freq, 
                                text = paste0(u$Grade,":",u$Freq),
                                xanchor = 'center', yanchor = 'bottom',
                                showarrow = FALSE))
  })
  ################################3
  
  ##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  output$counter2 <- renderPlotly({
    cat("COUNTER","\n\n\n")
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$dataset =="Relative")
      dat<-datasetInput()
    else
      dat <- datasetAbsol()
    
    
    m <- mean(dat$Total_marks)
    s <- sd(dat$Total_marks)
    
    ds <- c(m)
    i <- 0
    while(m - (input$de)*s >=0){
      ds <- c(ds,(m -(input$de)*s))
      m <- m- (input$de)*s
      i <- i+1
    }
    
    m <- mean(dat$Total_marks)
    while(m + (input$de)*s <= 100){
      ds <- c(ds,(m + (input$de)*s))
      m <- m + (input$de)*s
    }
    ds <- round(ds, digits = 0)
    ds <- sort(ds, decreasing = TRUE)
    gr <- c("A","AB","B","BC","C","CD","D","F")
    
    
    #cat(paste0("PREVIOUS ds=",ds),"\n")
    while(length(ds) > 7){
      ds <- ds[1:(length(ds)-1)]
    }
    len <- length(ds)
    #cat(paste0("ds=",ds),"\n")
    g <- gr[1:(len+1)]
    cat(paste0("g=",g),"\n")
    ds <- sort(ds)
    cat(paste0("ds::::",ds))
    d <- data.frame("Marks_Range"=paste0("0-",ds[1]), "Grade"=g[length(g)])
    
    for(i in 1:(length(ds)-1)){
      mr <- paste0(ds[i],"-",ds[i+1])
      
      grad <- g[length(g) - i]
      print(data.frame("Marks_Range"=mr, 
                       "Grade"=grad))
      d <- rbind(d, data.frame("Marks_Range"=mr, 
                               "Grade"=grad))
    }
    d <- rbind(d, data.frame("Marks_Range"=paste0(ds[length(ds)],"-100"), 
                             "Grade"=g[1]))
    print(d)
    
    cc<- as.data.frame(table(dat$Grade))
    #ct <- count(cc)
    if(nrow(d) != nrow(cc)){
      for(i in d$Grade){
        if (i %in% cc$Var1){}
        else{
          cc <- rbind(cc, data.frame("Var1"=i,"Freq"=0))
        }
      }
    }
    
    u <- merge(d,cc,by.x="Grade", by.y="Var1")
    print(cc)
    print(d)
    print(u)
    d <- as.data.frame(table(dat$Range))
    
    p <- data.frame("Total_marks"=dat$Total_marks, "Roll_no"=dat$Roll_no)
    p <- arrange(p, Total_marks)
    p$Roll_no <- factor(p$Roll_no, levels = p$Roll_no)
    print(p)
    print(p$Roll_no)
    col <- c("#00007F", "blue", "#007FFF", "cyan",
             "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    
    gt <- ggplot(p, aes(Total_marks, Roll_no))+
      theme(axis.text.y=element_blank(),
            axis.ticks.y = element_blank())+
      geom_point(colour=col[length(col)])+ 
      annotate("text", x = ds[1]-5, y = nrow(p)/1.2, 
               label = g[length(g)], colour = col[1], size=5.5)
    
    for(i in 1:(length(ds)-1)){
      den <- 1+ i*5/10
      cat(paste0("d[",i,"]=",ds[i],"  d[",(i+1),"]=", ds[i+1]), "\n")
      cat(paste0("grade", g[length(g)-i]), "\n")
      
      gt <- gt + geom_vline(xintercept=ds[i], colour=col[i+1])
      gt <- gt + 
        annotate("text", x = ds[i]+5, y = nrow(p)/den, 
                 label = g[length(g)-i], colour = col[i+1], size=5.5)
    }
      gt+labs(y="Roll Number", x="Total Marks")
  })
  ##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  
  output$stats <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$dataset =="Relative")
      dat<-datasetInput()
    else
      dat <- datasetAbsol()
    
    m <- mean(dat$Total_marks)
    s <- sd(dat$Total_marks)
    
    p <- data.frame("Mean"=m, "Standard_Deviation"=s, 
                    "Min_marks"=min(dat$Total_marks),
                    "Max_marks"=max(dat$Total_marks))
    p
  }, align = 'c')
  
  
  
  
  calgrade <- function(d, marks){
    p<- c()
    for(i in 1:length(d)){
      if(d[i] > (0.9)*marks  )
        p[i] <- "A"
      else if(d[i] > (0.8)*marks)
        p[i] <- "AB"
      else if(d[i] > (0.7)*marks)
        p[i] <- "B"
      else if(d[i] > (0.6)*marks)
        p[i] <- "BC"
      else if(d[i] > (0.55)*marks)
        p[i] <- "C"
      else if(d[i] > (0.40)*marks)
        p[i] <- "CD"
      else if(d[i] > (0.30)*marks)
        p[i] <- "D"
      else
        p[i] <- "F"
    }
    p<- as.factor(p)
    p
  }
  ################################
  
  ################################
  datasetAbsol <- function(){
    inFile <- input$file1
    if(input$sep == ",")
      data <- read.csv(inFile$datapath, 
                       na.strings = c("",'Ab', 'ab', 'AB'), 
                       check.names = FALSE)
    
    else if(input$sep == "\t")
      data <- read.table(inFile$datapath, 
                         header =T,
                         na.strings = c("",'Ab', 'ab', 'AB'), 
                         sep="\t", check.names = FALSE)
    
    if(colnames(data)[1]=="Roll_no" && colnames(data)[2]=="Name"){
      for(i in 3:ncol(data)){
        data[is.na(data[,i]), i] <- input$ab
      }
    }
    else if(colnames(data)[1]=="Roll_no" && colnames(data)[2] !="Name"){
      for(i in 2:ncol(data)){
        data[is.na(data[,i]), i] <- input$ab
      }
    }
    else if(colnames(data)[1]=="Name"){
      for(i in 2:ncol(data)){
        data[is.na(data[,i]), i] <- input$ab
      }
    }
    
    s <- rep(0,nrow(data))
    if(colnames(data)[1]=="Roll_no" && colnames(data)[2]=="Name"){
      for(i in 3:ncol(data)){
        j <- colnames(data)[i]
        r <- gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]])
        s <- s + data[,i]*as.numeric(r[2])/as.numeric(r[1])
      }
    }
    else if(colnames(data)[1]=="Roll_no" && colnames(data)[2] !="Name"){
      for(i in 2:ncol(data)){
        j <- colnames(data)[i]
        r <- gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]])
        s <- s + data[,i]*as.numeric(r[2])/as.numeric(r[1])
      }
    }
    else if(colnames(data)[1]=="Name"){
      for(i in 2:ncol(data)){  
        j <- colnames(data)[i]
        r <- gsub("[\\(\\)]", "", regmatches(j, gregexpr("\\(.*?\\)", j))[[1]])
        s <- s + data[,i]*as.numeric(r[2])/as.numeric(r[1])
      }
    }
    data$Total_marks <- s
    
    data$Grade <- calgrade(data$Total_marks, input$num)
    data
  }
  ##############################
  
  ##############################
  output$marks <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile)){
      output$summary <- renderPrint({ h1("Please UPload the File")})
      return(NULL)
    }
    else{
      if(input$dataset =="Relative")
        dat<-datasetInput()
      else
        dat <- datasetAbsol()
      
      #data[[colnames(data)[1]]]
      
      output$summary <- renderPrint({ h1("")})
      
      set.seed(6087)
      ggplot(dat, aes(Roll_no,Total_marks, color = Grade)) + 
        geom_point(size=2.5)+
        geom_hline(yintercept = mean(dat$Total_marks))+
        labs(list(title = "Marks Plot", x = "Roll No.", y = "Marks"))+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      #(gg <- ggplotly(p))
      #gg
    }   
  })
  
  
  
  output$marks2 <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$dataset =="Relative")
      dat<-datasetInput()
    else
      dat <- datasetAbsol()
    
    #data[[colnames(data)[1]]]
    
    
    set.seed(6087)
    ggplot(dat, aes(Roll_no,Total_marks, color = Grade)) + 
      geom_point(size=2.5)+
      geom_hline(yintercept = mean(dat$Total_marks))+
      labs(list(title = "Marks Plot", x = "Roll No.", y = "Marks"))+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    #(gg <- ggplotly(p))
    #gg
    
  })
  
  ##########################################FINAL TOUCH##########
  ##################################################################
  ###############################
  session$sendCustomMessage('unbind-DT', 'table')
  # helper function for making checkbox
  shinyInput = function(FUN, le, id,select, ...) { 
    #inputs = character(len)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(le > 0){
      cat(paste("le",le),"\n\n\n\n")
      inputs = 1:le
      for (i in seq_len(le)) { 
        inputs[i] = as.character(FUN(paste0(id, i), selected=select[i], label = NULL, ...)) 
      } 
      inputs 
    }
    else{
      return(NULL)
    }
  }
  
  shinyValue = function(data, id, len) { 
    f <- unlist(lapply(seq_len(len), function(i) { 
      value = input[[paste0(id, i)]] 
      if (is.null(value)) NA 
      else value 
    }))
    k <- data.frame(data,f)
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$sep == ",")
      da <- read.csv(inFile$datapath, 
                     na.strings = c("",'Ab', 'ab', 'AB'), check.names = FALSE)
    u<- colnames(da)
    u <- c(u, "Total_marks", "Grade")
    colnames(k) <- u
    k
  } 
  
  
  
  ###############################  
  ##################################################################  
  #    output$table <- DT::renderDataTable(DT::datatable({
  #      da <- if(input$print_data=="Yes"){
  #              if(input$dataset =="Relative")
  #                datasetInput()
  #              else
  #                datasetAbsol()
  #              
  #            }
  #      })
  #)
  
  dat = function(){
    inFile <- input$file1
    #cat(paste0("FILEEEEEEEEEEEEEE",inFile),"\n\n")
    if (is.null(inFile))
      return(NULL)
    if(input$print_data=="Yes"){
      if(input$dataset =="Relative")
        da <- datasetInput()
      else
        da <- datasetAbsol()
    }
  }       
  
  output$table = DT::renderDataTable(
    j <- {
      
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      if(input$print_data=="No")
        return(NULL)
      
      if(input$sep == ",")
        data <- read.csv(inFile$datapath, 
                         na.strings = c("",'Ab', 'ab', 'AB'), check.names = FALSE)
      u<- colnames(data)
      u <- c(u, "Total_marks", "Grade")
      cat(paste("nrow",nrow(dat()), ";;; ncol=",ncol(dat())),"\n\n")
      
      r <- data.frame(dat()[,1:(ncol(dat())-1)], Grade=shinyInput(selectInput,nrow(dat()),"selecter_",
                                                                  as.character(dat()[, ncol(dat())]),
                                                                  choices=c('A'='A',"AB"='AB',"B"='B',"BC"='BC',"C"='C',"CD"='CD',"D"='D',"F"='F'),
                                                                  width="60px"
      ))
      colnames(r)<- u
      r}, selection='none',server = FALSE, escape = FALSE, options = list( 
        paging=FALSE,
        pageLength = 5,
        preDrawCallback = JS('function() { 
                             Shiny.unbindAll(this.api().table().node()); }'), 
        drawCallback = JS('function() { 
                          Shiny.bindAll(this.api().table().node()); } ') 
        )
        )  
  
  
  
  ###############################
  
  #############################
  output$siou <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return("Please select input file")
    #      if(input$dataset =="Relative")
    #        dat<-datasetInput()
    #      else
    #        dat <- datasetAbsol()
    #l=dat()
    summary(dat())
  })
  #############################
  
  #############################
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      inFile <- input$file1
      paste( 'Graded_', inFile$name, sep='') 
    },
    content = function(file) {
      if(input$dataset =="Relative")
        fin <- datasetInput()
      else
        fin <- datasetAbsol()
      #fin <- dat()#shinyValue(dat()[,1:(ncol(dat())-1)], "selecter_",nrow(dat()))
      
      write.csv(fin, file, row.names = FALSE)
    }
  )
  
  
  
  
  output$mas2 <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$dataset =="Relative")
      dat<-datasetInput()
    else
      dat <- datasetAbsol()
    
    plot_ly(dat,y = ~Total_marks, color = ~Grade, type = "box", boxpoints = "all", jitter = 0.3,
            pointpos = -1.8)%>%
      layout(title = "Grade Summary",
             xaxis = list(title = "Grades"),
             yaxis = list(title = "Marks"))
    
  })
  
  
  output$table22 <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$dataset =="Relative")
      dat<-datasetInput()
    else
      dat <- datasetAbsol()
    
    d <- data.frame("Range"=rep(0,nrow(dat)))
    for(i in 1:nrow(dat)){
      q <- floor((dat$Total_marks[i])/5)
      r <- dat$Total_marks[i] %% 5
      dat$Range[i] <- paste(q*5,"-",(q*5)+5, sep="")
    }
    
    d <- as.data.frame(table(dat$Range))
    
    plot_ly(
      x = d$Var1, #d$Var1,
      y = d$Freq,
      name = "",
      type = "bar",
      text = paste("Number of Students:", d$Freq)
    ) %>%
      layout(title = "Bar Plot of Final Marks",
             xaxis = list(title = "Range"),
             yaxis = list(title = "Number of Students"),
             annotations = list(x = d$Var1, y = d$Freq, text = d$Freq,
                                xanchor = 'center', yanchor = 'bottom',
                                showarrow = FALSE))
  })
  
  output$table223 <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$dataset =="Relative")
      dat<-datasetInput()
    else
      dat <- datasetAbsol()
    
    #d <- data.frame("Range"=rep(0,nrow(dat)))
    dat$Range <- rep(0,nrow(dat))
    for(i in 1:nrow(dat)){
      q <- floor(dat$Total_marks[i]/5)
      r <- dat$Total_marks[i] %% 5
      dat$Range[i] <- paste(q*5,"-",(q*5)+5, sep="")
    }
    
    dd<- as.data.frame(table(dat$Range))
    ggplot(dat, aes(Roll_no,Total_marks, color = Range)) + 
      geom_point(size=2.5)+
      geom_hline(yintercept = mean(dat$Total_marks))+
      labs(list(title = "Scatter Plot with Range in Color", x = "Roll No.", y = "Total Marks"))+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
  })
  
  ########TAB2################TAB2#####################
  ########TAB2################TAB2#####################
  ########TAB2################TAB2#####################
  ################################################################
  
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    d <- data.frame("Row_no"=1:10)
    
    p <- length(input$checkGroup)
    d[,c(input$checkGroup)] <- NA
    
    if(input$checkbox == TRUE){
      p <- length(input$checkGroup)
      d[,c(input$checkGroup)] <- NA
      
      
      for(i in 1:input$num2){
        d[,paste("Quiz",i,"(", input[[paste0("qnmx",i)]],")(", 
                 input[[paste0("qwtge",i)]],")",sep="")] <- NA
      }
      if(input$asgn > 0){
        for(i in 1:input$asgn){
          d[,paste("Assignment",i,"(", input[[paste0("anmx",i)]],")(", 
                   input[[paste0("awtge",i)]],")",sep="")] <- NA
        }
      }    
      d$Row_no <- NULL
      if(input$midterm == "Yes")
        d[,paste("Midterm(", input[[paste0("mnmx")]],")(", 
                 input[[paste0("mwtge")]],")",sep="")] <- NA
      
      if(input$endterm == "Yes")
        d[,paste("Endterm(", input[[paste0("enmx")]],")(", 
                 input[[paste0("ewtge")]],")",sep="")] <- NA
      
    }else{
      for(i in 1:input$num2){
        d[,paste("Quiz",i,sep="")] <- NA
      }
      if(input$asgn > 0){
        for(i in 1:input$asgn){
          d[,paste("Assignment",i,sep="")] <- NA
        }
      }
      d$Row_no <- NULL
      if(input$midterm == "Yes")
        d[,paste0("Midterm")] <- NA
      
      if(input$endterm == "Yes")
        d[,paste0("Endterm")] <- NA
    }
    d
    
  }))
  
  output$mm <- renderUI({
    nquiz <- as.integer(input$num2)
    lapply(1:nquiz, function(i){
      list(tags$p(tags$u(h4(paste0("Quiz ", i, ":")))),
           column(6, numericInput(paste0("qnmx", i)
                                  , label = "Max. Marks", value=0, min=0)),
           column(6, numericInput(paste0("qwtge", i)
                                  , label = "Wtge of Quiz", value=0, step=0.1, min=0))
           
      )
    }) #end of lapply
  }) # end of renderUI
  
  output$ass <- renderUI({
    nquiz <- as.integer(input$asgn)
    if(nquiz > 0){
      lapply(1:nquiz, function(i){
        list(tags$p(tags$u(h4(paste0("Assignment ", i, ":")))),
             column(6, numericInput(paste0("anmx", i)
                                    , label = "Max. Marks", value=0, min=0)),
             column(6, numericInput(paste0("awtge", i)
                                    , label = "Wtge of Assignment", value=0, step=0.1, min=0))
             
        )
      }) #end of lapply
    }
  }) # end of renderUI
  
  output$mid <- renderUI({
    if(input$midterm == "Yes"){
      list(tags$p(tags$u(h4(paste0("Midterm ", ":")))),
           column(6, numericInput(paste0("mnmx")
                                  , label = "Max. Marks", value=0, min=0)),
           column(6, numericInput(paste0("mwtge")
                                  , label = "Wtge of Midterm", value=0, step=0.1, min=0))
           
      )
    }
    
  })
  
  output$end <- renderUI({
    if(input$endterm == "Yes"){
      list(tags$p(tags$u(h4(paste0("Endterm ", ":")))),
           column(6, numericInput(paste0("enmx")
                                  , label = "Max. Marks", value=0, min=0)),
           column(6, numericInput(paste0("ewtge")
                                  , label = "Wtge of Endterm", value=0, step=0.1, min=0))
           
      )
    }
    
  })
  
  
  
  output$downloadfile <- downloadHandler(
    filename = function() { 
      paste0('Enter_the_Entries','.csv') 
    },
    content = function(file) {
      da<- demo11()
      write.csv(da, file, row.names = FALSE)
    }
  )
  
  demo11 <- function(){
    d <- data.frame("Row_no"=1:10)
    d[,c(input$checkGroup)] <- NA
    
    if(input$checkbox == TRUE){
      p <- length(input$checkGroup)
      d[,c(input$checkGroup)] <- NA
      
      
      for(i in 1:input$num2){
        d[,paste("Quiz",i,"(", input[[paste0("qnmx",i)]],")(", 
                 input[[paste0("qwtge",i)]],")",sep="")] <- NA
      }
      
      d$Row_no <- NULL
      if(input$midterm == "Yes")
        d[,paste("Midterm(", input[[paste0("mnmx")]],")(", 
                 input[[paste0("mwtge")]],")",sep="")] <- NA
      
      if(input$endterm == "Yes")
        d[,paste("Endterm(", input[[paste0("enmx")]],")(", 
                 input[[paste0("ewtge")]],")",sep="")] <- NA
      
    }else{
      for(i in 1:input$num2){
        d[,paste("Quiz",i,sep="")] <- NA
      }
      
      d$Row_no <- NULL
      if(input$midterm == "Yes")
        d[,paste0("Midterm")] <- NA
      
      if(input$endterm == "Yes")
        d[,paste0("Endterm")] <- NA
    }
    d
  }
  ################################################################
  max_plots <- 5
  
  observe({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(input$dataset =="Relative")
      dat<-datasetInput()
    else
      dat <- datasetAbsol()
    output$plots <- renderUI({ get_plot_output_list(max_plots, dat) })
    output$plots22 <- renderUI({ get_plot_output_list3(max_plots, dat) })
    
  })
        })


get_plot_output_list <- function(max_plots, dat) {
  # Insert plot output objects the list
  n <- ncol(dat)
  
  if(colnames(dat)[1]=="Roll_no" && colnames(dat)[2]=="Name"){
    a <- ceiling((n -3)/2)
    b <- a+1
  }
  else if(colnames(dat)[1]=="Roll_no" && colnames(dat)[2] !="Name"){
    a <- ceiling((n -2)/2)
    b <- a+1
  }
  else if(colnames(dat)[1] !="Name"){
    a <- ceiling((n -2)/2)
    b <- a+1
  }
  if(colnames(dat)[1]=="Roll_no" && colnames(dat)[2]=="Name"){
    
    plot_output_list <- lapply(3:(3+a-1), function(i) {
      plotname <- paste("plot", i, sep="")
      plot_output_object <- plotOutput(plotname)
      plot_output_object <- renderPlotly({
        plot_ly(dat,y = ~dat[,colnames(dat)[i]],x="", type = "box",
                boxpoints = "all", jitter = 0.3,
                pointpos = -1.8) %>% 
          layout(title = colnames(dat)[i], 
                 xaxis = list(title = ""),
                 yaxis = list(title = "Marks")      
          )
        
        #p <- ggplot(dat, aes(Grade, colnames(dat)[i])) + 
        # geom_boxplot() + geom_jitter()
        
        #p
      })
    })
    
  }
  else if(colnames(dat)[1]=="Roll_no" && colnames(dat)[2] !="Name"){
    
    plot_output_list <- lapply(2:(2+a-1), function(i) {
      plotname <- paste("plot", i, sep="")
      plot_output_object <- plotOutput(plotname, height = 280, width = 250)
      plot_output_object <- renderPlotly({
        plot_ly(dat,y = ~dat[,colnames(dat)[i]],x="", type = "box", 
                boxpoints = "all", jitter = 0.3,
                pointpos = -1.8)
        
        #p <- ggplot(dat, aes(Grade, colnames(dat)[i])) + 
        # geom_boxplot() + geom_jitter()
        
        #p
      })
    })
  }
  else if(colnames(dat)[1]=="Name"){
    
    plot_output_list <- lapply(2:(2+a-1), function(i) {
      plotname <- paste("plot", i, sep="")
      plot_output_object <- plotOutputly(plotname, height = 280, width = 250)
      plot_output_object <- renderPlotly({
        plot_ly(dat,y = ~dat[,colnames(dat)[i]], x="", type = "box", boxpoints = "all", jitter = 0.3,
                pointpos = -1.8)
        
        #p <- ggplot(dat, aes(Grade, colnames(dat)[i])) + 
        # geom_boxplot() + geom_jitter()
        
        #p
      })
    })
    
  }
  
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}

####################################################
####################################################

get_plot_output_list3 <- function(max_plots, dat) {
  # Insert plot output objects the list
  n <- ncol(dat)
  
  if(colnames(dat)[1]=="Roll_no" && colnames(dat)[2]=="Name"){
    a <- ceiling((n -3)/2)
    b <- a+1
  }
  else if(colnames(dat)[1]=="Roll_no" && colnames(dat)[2] !="Name"){
    a <- ceiling((n -2)/2)
    b <- a+1
  }
  else if(colnames(dat)[1] !="Name"){
    a <- ceiling((n -2)/2)
    b <- a+1
  }
  if(colnames(dat)[1]=="Roll_no" && colnames(dat)[2]=="Name"){
    
    plot_output_list <- lapply((3+a):(n-1), function(i) {
      plotname <- paste("plot", i, sep="")
      plot_output_object <- plotOutput(plotname)
      plot_output_object <- renderPlotly({
        plot_ly(dat,y = ~dat[,colnames(dat)[i]], x="",
                type = "box", boxpoints = "all", jitter = 0.3,
                pointpos = -1.8) %>% 
          layout(title = colnames(dat)[i], 
                 xaxis = list(title = ""),
                 yaxis = list(title = "Marks")      
          )
        
        
        #p <- ggplot(dat, aes(Grade, colnames(dat)[i])) + 
        # geom_boxplot() + geom_jitter()
        
        #p
      })
    })
    
  }
  else if(colnames(dat)[1]=="Roll_no" && colnames(dat)[2] !="Name"){
    
    plot_output_list <- lapply((2+a):(n-1), function(i) {
      plotname <- paste("plot", i, sep="")
      plot_output_object <- plotOutput(plotname, height = 280, width = 250)
      plot_output_object <- renderPlotly({
        plot_ly(dat,y = ~dat[,colnames(dat)[i]], x="", type = "box", boxpoints = "all", jitter = 0.3,
                pointpos = -1.8)
        
        #p <- ggplot(dat, aes(Grade, colnames(dat)[i])) + 
        # geom_boxplot() + geom_jitter()
        
        #p
      })
    })
  }
  else if(colnames(dat)[1]=="Name"){
    
    plot_output_list <- lapply((2+a):(n-1), function(i) {
      plotname <- paste("plot", i, sep="")
      plot_output_object <- plotOutputly(plotname, height = 280, width = 250)
      plot_output_object <- renderPlotly({
        plot_ly(dat,y = ~dat[,colnames(dat)[i]], x="", type = "box", boxpoints = "all", jitter = 0.3,
                pointpos = -1.8)
        
        #p <- ggplot(dat, aes(Grade, colnames(dat)[i])) + 
        # geom_boxplot() + geom_jitter()
        
        #p
      })
    })
    
  }
  
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}