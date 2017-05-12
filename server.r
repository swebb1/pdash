require(gdata)
require(ggplot2)
require(plotly)
require(plyr)
require(d3heatmap)
require(reshape2)
require(RColorBrewer)
require(heatmaply)
require(scales)
require(egg)
cols<-brewer.pal(11,"Spectral")

shinyServer(function(input, output,session) {
  
  Vvalues<-reactiveValues(items=vector(),numeric=vector(),factor=vector())
  Pvalues<-reactiveValues(items=vector(),numeric=vector(),factor=vector())
  
  vfilt<-function(df){
    df
  }
  
  ##Get data
  VData<-reactive({
        inFile<-input$vfile
        if(length(inFile) == 0){
          return(NULL)
        }
        sep<-switch(input$vsep,space=" ",tab="\t",comma=",")
        if(input$vxls){
          df<-read.xls(inFile$datapath,header=input$vheader,fill=T)
        }
        else{
          df<-read.table(inFile$datapath,header=input$vheader,sep=sep,fill=T)
        }
        num<-names(df[,sapply(df,is.numeric),drop=F])
        for(i in 1:length(num)){
          ind<-paste0("filt",i)
          if(!is.null(input[[ind]])){
            if(input[[paste0("filt",i)]]){
              df<-subset(df,df[,num[i]]>=input[[paste0("filt_min",i)]] & df[,num[i]]<=input[[paste0("filt_max",i)]] )
            }
          }
        }
        #df<-vfilt(df)
        Vvalues$numeric<-names(df[,sapply(df,is.numeric),drop=F])
        Vvalues$factor<-names(df[,sapply(df,is.factor),drop=F])
        Vvalues$items=names(df)
        return(df)
  })
  
  PData<-reactive({
    inFile<-input$pfile
    sep<-switch(input$psep,space=" ",tab="\t",comma=",")
    if(length(inFile) == 0){
      return(NULL)
    }
    if(input$pxls){
      df<-read.xls(inFile$datapath,header=input$pheader,fill=T)
    }
    else{
      df<-read.table(inFile$datapath,header=input$pheader,sep=sep,fill=T)
    }
    Pvalues$numeric<-names(df[,sapply(df,is.numeric),drop=F])
    Pvalues$factor<-names(df[,sapply(df,is.factor),drop=F])
    Pvalues$items=names(df)
    return(df)
  })
  
  ##output tables
  output$vtable = renderDataTable({
    df<-VData()
    return(df)
  },options = list(bSortClasses = TRUE,aLengthMenu = c(10,20,50,100), iDisplayLength = 10)
  )
  
  seltable<-reactive({
    event.data <- event_data("plotly_selected", source = "subset")
    if(is.null(event.data) == T){
      return(NULL)
    }
    sdf<-VData()
    sdf<-sdf[subset(event.data, curveNumber == 0)$pointNumber + 1,]
    sdf
  })
  
  output$vseltable = renderDataTable({
    sdf<-seltable()
    return(sdf)
  },options = list(bSortClasses = TRUE,aLengthMenu = c(10,20,50,100), iDisplayLength = 10)
  )
  
  ##Create a file download button
  output$downloadFile<-renderUI({
    downloadName<-"Table.tab"
    tagList(
      textInput("tableName","Table Name:",value = downloadName),
      downloadButton('downloadData', 'Save Table')
    )
  })
  
  ##Download handler
  output$downloadData <- downloadHandler(
    filename = function() {input$tableName},
    content = function(file) {
      sdf<-seltable()
      write.table(sdf, file,sep="\t",quote=F,row.names=F)
    }
  )
  
  output$ptable = renderDataTable({
    df<-PData()
    return(df)
  },options = list(bSortClasses = TRUE,aLengthMenu = c(10,20,50,100), iDisplayLength = 10)
  )
  
  ##V plot controls
  output$vplot_cols <- renderUI({
    #isolate({
    #  if(is.null(VData())){return(NULL)}
    #})
    items=Vvalues$items
    num<-Vvalues$numeric
    tagList(
      selectInput("vx", "X axis",num,multiple=F),
      selectInput("vy", "Y axis",num,multiple=F),
      checkboxInput("vlogx", "log X axis",value = F),
      checkboxInput("vlogy", "log Y axis",value = F),
      selectInput("vcol", "Colour points by",num),
      selectInput("vtext", "Hover text",items),
      selectInput("vlabels","Label points by",c("NA",items)),
      conditionalPanel(condition="input.vlabels != 'NA'",
                       numericInput("vnudgex","Nudge labels on X-axis",0),
                       numericInput("vnudgey","Nudge labels on Y-axis",0),
                       textInput("vlabeldisplay","Labels to display","")
      )
    )
  })
  
  ##V plot controls
  output$vplot_fils <- renderUI({
    #isolate({
    #  if(is.null(VData())){return(NULL)}
    #})
    items=Vvalues$items
    num<-Vvalues$numeric
    if(length(num)==0){
      return(NULL)
    }
    lapply(1:length(num), function(i) {
        tagList(
            wellPanel(p(strong(num[i])),
              numericInput(inputId = paste0("filt_min", i), label = "Minimum",0),
              numericInput(inputId = paste0("filt_max", i), label = "Maximum",0),
              checkboxInput(inputId = paste0("filt", i),label = "Use filter",value = F)
            )
        )
    })
  })
    
  
  ##v plot
  vplot<-reactive({
    if(is.null(VData())){return(NULL)}
    par(mar=c(10,5,5,5))
    withProgress(message="Plotting...",value=0,{
    df<-VData()
    g<-ggplot(df,aes_string(y=input$vy,x=input$vx,text=input$vtext,colour=input$vcol))+geom_point()+theme_bw()
    if(input$vlabels != "NA"){
      label_display=df[,input$vlabels]
      if(input$vlabeldisplay != ""){
        label_display = eval(parse(text=paste0("subset(df,df$",input$vlabeldisplay,")[,input$vlabels]")))
        label_display=ifelse(df[,input$vlabels] %in% label_display,as.character(df[,input$vlabels]),'')
      }
      g<-g+geom_text(aes(label=label_display),nudge_y=input$vnudgey,nudge_x=input$vnudgex)
    }
    if(input$vlogx){
      g<-g+scale_x_log10()
    }
    if(input$vlogy){
      g<-g+scale_y_log10()
    }
    return(g)
    })
  })
  
  output$vplot <- renderPlotly({
    pdf(NULL)
    if(is.null(vplot())){
      return(NULL)
    }
    g<-vplot()
    ggplotly(g,source="subset")
  })

  ##P plot controls
  output$pplot_cols <- renderUI({
    #isolate({
    #  if(is.null(PData())){return(NULL)}
    #})
    df<-PData()
    items=Pvalues$items
    num<-Pvalues$numeric
    tagList(
      selectInput("sample", "Samples",items,multiple=F),
      selectInput("variable", "Variables",items,multiple=F),
      selectInput("value", "Values",num,multiple=F)
      )
  })
  
  ##p plot
  pplot<-eventReactive(input$pgo,{
    if(is.null(PData())){return(NULL)}
    par(mar=c(10,5,5,5))
    withProgress(message="Plotting...",value=0,{
      df<-PData()
      mm<-dcast(df,list(input$sample,input$variable),fun.aggregate = sum,value.var = input$value)
      mn<-cbind(sample=mm[,1],mm[,-1]/rowSums(mm[,-1]))
      mmelt<-melt(mn)
      g1<-ggplot(mmelt,aes(x=sample,y=value,fill=variable))+geom_bar(stat="identity",position="dodge")+
        theme_dark()+theme(text = element_text(size=12),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_viridis(discrete=T,guide=F)+xlab("Sample name")+
        ylab("Normalised proportion")+facet_wrap(~variable,scales="free")
      g2<-ggplot(df,aes_string(x=input$sample,y=input$value,fill=input$variable))+geom_bar(stat="identity",position="fill")+
        theme_dark()+theme(text = element_text(size=12),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_viridis(discrete=T)+xlab("Sample name")+
        ylab("Normalised proportion")
      g3<-ggplot(mmelt,aes(x=sample,y=variable,fill=value))+geom_tile(colour="black")+
        theme_minimal()+theme(text = element_text(size=12),axis.text.x = element_text(angle = 90, hjust = 1))+
        scale_fill_gradientn(name="Normalised proportion",colours = brewer.pal(9,"Purples"))+xlab("Sample name")+geom_text(aes(label=round(value,digits = 4)))+ylab(input$variable)
      return(list(g1,g2,g3))
    })
  })
  
  output$pplot1 <- renderPlot({ 
    pplot()[[1]]
  })
  output$pplot2 <- renderPlotly({
    pdf(NULL)
    if(is.null(pplot())){
      return(NULL)
    }
    ggplotly(pplot()[[2]])
  })
  output$pplot3 <- renderPlotly({
    pdf(NULL)
    if(is.null(pplot())){
      return(NULL)
    }
    ggplotly(pplot()[[3]])
  })
  
  output$downloadVplot <- downloadHandler(
    filename = function() {paste0(input$VplotName,".pdf")},
    content = function(file) {
      ggsave(file, plot = vplot(), device = "pdf")
    }
  )
  
  output$downloadPplot <- downloadHandler(
    filename = function() {paste0(input$PplotName,".pdf")},
    content = function(file) {
      ga<-ggarrange(pplot()[[1]],pplot()[[2]],pplot()[[3]],nrow = 3)
      ggsave(file, plot = ga, device = "pdf",height = 20,width=20)
    }
  )
  

  
  
}
)



