###### CODE FOR CRUDE ######
library(plyr)
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(scales)
library(xtable)
library(DT)
library(RODBC)
library(sqldf)

##########################################################################################
shinyServer(function(input,output,session){
  
  autoinvalidate<-reactiveTimer(960000,session)
  
  closingdata<-reactive({
    
    autoinvalidate()
    
    minute_15 <- read.csv("http://ds01.ddfplus.com/historical/queryminutes.ashx?username=toddgross1&password=barchart&symbol=CL*0&maxrecords=1&interval=15", header = FALSE)
    colnames(minute_15)<-c("CMDTY","DATETIME","RNDM", "Open", "High", "Low", "Close", "Volume")
    minute_15$Time<-format(as.POSIXct(strptime(minute_15$DATETIME, format = "%Y-%m-%d %H:%M")), format = "%H:%M")
    minute_15<-tail(minute_15[,c(9,4,5,6,7)])
    closing<-tail(minute_15[,5],1)
    
    return(closing)
  })
  
  retfromyclos<-reactive({
    
    daily_gold<-read.csv("http://ds01.ddfplus.com/historical/queryeod.ashx?username=toddgross1&password=barchart&symbol=CL*0&maxrecords=2", header = FALSE)
    colnames(daily_gold)<-c("Contract Name", "Date", "Open", "High", "Low", "Close", "Volume", "Vol2")
    daily_gold$Date<-format(as.POSIXct(strptime(daily_gold$Date, format = "%Y-%m-%d")), format = "%m-%d")
    daily_gold<-daily_gold[,c(2:6)]
    retyclose<-round((daily_gold[2,5]-daily_gold[1,5])*100/daily_gold[2,5],3)
    
    return(retyclose)
  })

  retfromopen<-reactive({
    
    daily_gold<-read.csv("http://ds01.ddfplus.com/historical/queryeod.ashx?username=toddgross1&password=barchart&symbol=CL*0&maxrecords=1", header = FALSE)
    colnames(daily_gold)<-c("Contract Name", "Date", "Open", "High", "Low", "Close", "Volume", "Vol2")
    daily_gold$Date<-format(as.POSIXct(strptime(daily_gold$Date, format = "%Y-%m-%d")), format = "%m-%d")
    daily_gold<-daily_gold[,c(2:6)]
    retopen<-round((daily_gold[1,5]-daily_gold[1,2])*100/daily_gold[1,2],3)
    
    return(retopen)
  })
  
 ## MADE THIS CHANGE FOR UNIFORMITY OF ROUND TIME 
  time<-reactive({
    
    autoinvalidate()
    
    minute_15 <- read.csv("http://ds01.ddfplus.com/historical/queryminutes.ashx?username=toddgross1&password=barchart&symbol=CL*0&maxrecords=1&interval=15", header = FALSE)
    colnames(minute_15)<-c("CMDTY","DATETIME","RNDM", "Open", "High", "Low", "Close", "Volume")
    minute_15$Time<-format(as.POSIXct(strptime(minute_15$DATETIME, format = "%Y-%m-%d %H:%M")), format = "%H:%M")
    minute_15<-tail(minute_15[,c(9,4,5,6,7)])
    closing<-tail(minute_15[,5],1)
    round_time<-minute_15$Time

    return(round_time)
    
  })
  
 
  datasetinput<-reactive({
    
    autoinvalidate()
    
    conn1 <- odbcDriverConnect("Driver=FREETDS; TDS_Version=8.0;Server=zs2bgd0856.database.windows.net,1433;Database=QERI DB TRADE;Uid=Qeri2014;Pwd=Penn2014!")
    
    data <- sqlQuery(conn1,"SELECT * FROM [CL_KB1]")    
  
    data<-as.data.frame(data[,c("DATETIME3", "DATETIME1", "BAR_CLASS1", "OPEN1", "HIGH1", "LOW1", "CLOSE1","ATR1PREV", "BAR_CLASS3", "OPEN3", "HIGH3", "LOW3", "CLOSE3", "ATRPREV", "TIME1","RET_FROM_OPEN", 	"RET_TO_CLOSE","PCTG_Y_ATR_RET", "PRICE", "DAYHIGH", "DAYLOW", "DU", "DD" )])
    
    data<-data[which(data$HIGH3 > data$LOW3),]
    
    a <- as.POSIXct(data$DATETIME1,format="%m/%d/%Y%H:%M") # Produces NA when format is not "%m/%d/%Y"
    b <- as.POSIXct(data$DATETIME1,format="%Y-%m-%d %H:%M:%S") # Produces NA when format is not "%d.%m.%Y"
    a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
    data$DATETIME1 <- a # Put it back in your dataframe
    
    data<-data[!duplicated(data$DATETIME1),]
    
    data$DATE<-format(strftime(data$DATETIME1, format="%d/%m/%Y"))
  
    data$TIME1<-format(strftime(data$DATETIME1, format="%H:%M"))
	  data<-data[order(data$DATETIME1),]

    data$RET_FROM_OPEN_ATR<-data$RET_FROM_OPEN/data$ATRPREV
    data$projection_high<-(data$HIGH1-data$OPEN1)/data$ATRPREV
    data$projection_low<-(data$OPEN1-data$LOW1)/data$ATRPREV
    
    data$TIME<-as.numeric(as.factor(data$TIME1))
    data$PRICE<-(1+(data$RET_TO_CLOSE*sqrt(abs(59-tail(data$TIME,1)+1)/81)))*closingdata()
    
    data<-as.data.frame(data)
    
    frequency<-data.frame(table(data$BAR_CLASS3))
    data<-merge(data, frequency, by.x="BAR_CLASS3", by.y="Var1")
    data$probability<-(1+(data$Freq*data$RET_TO_CLOSE/nrow(data)))*closingdata()
    
    return(data)
    
  })

  
   data_projections<-reactive({
    
    data<-datasetinput()
    
    data<-data[order(data$DATETIME1),]  ## I MADE THIS CHANGE 
    
    last_values<-tail(data,1)
    last_rfo_atr<-last_values$RET_FROM_OPEN_ATR
    last_rfo<-round(last_values$RET_FROM_OPEN*100,2)
    
    data<-data[which(data$BAR_CLASS3!=89),]
    
    if((last_values$HIGH3-last_values$LOW3) > 1.2 * (last_values$ATRPREV))
    {
      data_last_rfo <- data[which((data$HIGH3-data$LOW3) > 1.2 * (data$ATRPREV)),]
    } else if((last_values$HIGH3-last_values$LOW3) > 0.8 * (last_values$ATRPREV))
    {
      data_last_rfo <- data[which((data$HIGH3-data$LOW3) > 0.8 * (data$ATRPREV)),]
    } else {
      data_last_rfo <- data
    }
    
    data_last_rfo<-data[which((data$RET_FROM_OPEN_ATR > (last_rfo_atr - 0.1*abs(last_rfo_atr))) & (data$RET_FROM_OPEN_ATR < (last_rfo_atr + 0.1*abs(last_rfo_atr)))),] 
    
    uniqv <- unique(data_last_rfo$BAR_CLASS3)
    
    max_prob<-uniqv[which.max(tabulate(match(data_last_rfo$BAR_CLASS3, uniqv)))]
    
    if(input$bar_class_type == "Most Likely")
    {
      data_projections <-ddply(data[which(data$BAR_CLASS3 == max_prob),], ~TIME1, summarise,
                               
                               sd_high = round(closingdata() + ((sd(projection_high) + mean(projection_low))*tail(data$ATRPREV,1)),2), 
                               
                               Projected_High = round((closingdata() + (mean(projection_high)*tail(data$ATRPREV,1))),2), 
                               
                               Projected_Low = round((closingdata() - mean(projection_low)*tail(data$ATRPREV,1)),2),
                               
                               sd_low = round(closingdata() - ((mean(projection_low) + sd(projection_high)) * tail(data$ATRPREV,1)),2))
      
    } else {
      
      data_projections <-ddply(data[which(data$BAR_CLASS3 == input$bar_class_type),], ~TIME1, summarise,
                               
                               sd_high = round(closingdata() + ((sd(projection_high) + mean(projection_low))*tail(data$ATRPREV,1)),2), 
                               
                               Projected_High = round((closingdata() + (mean(projection_high)*tail(data$ATRPREV,1))),2), 
                               
                               Projected_Low = round((closingdata() - mean(projection_low)*tail(data$ATRPREV,1)),2),
                               
                               sd_low = round(closingdata() - ((mean(projection_low) + sd(projection_high)) * tail(data$ATRPREV,1)),2))
      
    }
    
  })
  
  output$bar_frequency<-renderPlot({
    
    autoinvalidate()
    
    data<-datasetinput()
    
data_projections    
    last_values<-tail(data,1)
    last_rfo_atr<-last_values$RET_FROM_OPEN_ATR
    last_rfo<-round(last_values$RET_FROM_OPEN*100,2)
    
    data<-data[which(data$BAR_CLASS3!=89),]
    
    if((last_values$HIGH3-last_values$LOW3) > 1.2 * (last_values$ATRPREV))
    {
      data_last_rfo <- data[which((data$HIGH3-data$LOW3) > 1.2 * (data$ATRPREV)),]
    } else if((last_values$HIGH3-last_values$LOW3) > 0.8 * (last_values$ATRPREV))
    {
      data_last_rfo <- data[which((data$HIGH3-data$LOW3) > 0.8 * (data$ATRPREV)),]
    } else {
      data_last_rfo <- data
    }
    
    data_last_rfo<-data[which((data$RET_FROM_OPEN_ATR > (last_rfo_atr - 0.1*abs(last_rfo_atr))) & (data$RET_FROM_OPEN_ATR < (last_rfo_atr + 0.1*abs(last_rfo_atr)))),] 
    
    return(
      ggplot(data_last_rfo[which(data_last_rfo$TIME1==time()), ]) +
             # Daisy made changes{
             geom_histogram(aes(BAR_CLASS3, y=(..count../sum(..count..)),fill=..density..),binwidth=1,color = "Black")+
             scale_x_continuous(limits = c(0, 37), breaks = seq(1, 36, 1)) +
             #}
             xlab("BAR CLASS TYPE") + 
             ylab("Probability") +            
			       ggtitle("PROBABILITY OF BAR CLASS",subtitle = paste("TIME: ",time(),"\n", "RET. FROM OPEN: ",retfromopen(), "%","          RET. FROM YESTERDAY'S CLOSE:", retfromyclos(),"%")) +             
             theme(plot.title = element_text(lineheight = 4, face="bold",hjust = 0.5)) + 
             theme(axis.title = element_text(face = "bold"))+ 
             theme(axis.text = element_text(face="bold"))
      )
    
  })
  # Daily price barplots
  
  output$daily_price<-renderPlotly({
    
    autoinvalidate()
    
    daily_gold<-read.csv("http://ds01.ddfplus.com/historical/queryeod.ashx?username=toddgross1&password=barchart&symbol=CL*0&maxrecords=50", header = FALSE)
    colnames(daily_gold)<-c("Contract Name", "Date", "Open", "High", "Low", "Close", "Volume", "Vol2")
    daily_gold$Date<-format(as.POSIXct(strptime(daily_gold$Date, format = "%Y-%m-%d")), format = "%m-%d")
    daily_gold<-daily_gold[,c(2:6)]
    hovertxt <- Map(function(x, y) paste0(x, ":", y), names(tail(daily_gold,10)), tail(daily_gold,1))
    hovertxt <- Reduce(function(x, y) paste0(x, "<br&gt;", y), hovertxt)
    
    plot_ly(daily_gold, x = ~Date, 
            # Daisy made changes {
            #xend = ~Date, hoverinfo = "none",
            #color = ~Close < Open, colors = c("#00b386","#ff6666"), 
            type="candlestick", open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%

      #add_segments(y = ~Low, yend = ~High, line = list(width = 1, color = "black")) %>%
      #add_segments(y = ~Open, yend = ~Close, line = list(width = 3)) %>%
      #add_markers(y = ~(Low + High)/2, hoverinfo = "text",
      #            text = hovertxt, marker = list(color = "transparent")) %>%

      layout(showlegend = FALSE,
             yaxis = list(title = "Price in $", domain = c(0, 0.9)),
             annotations = list(
               list(xref = "paper", yref = "paper",
                    x = 0, y = 1.05, showarrow = F,
                    xanchor = "left", yanchor = "top",
                    align = "center",
                    text = paste0("<b>HISTORICAL PRICE BAR CHART</b>","<br><b>CRUDE</b>")),

               list(xref = "paper", yref = "paper",
                    x = 0.7, y = 1, showarrow = F,
                    xanchor = "left", yanchor = "top",
                    align = "center",
                    text = paste(range(daily_gold$Date), collapse = " : "),
                    font = list(size = 10))),
              plot_bgcolor = "#f2f2f2")
              
              #}
  })
  
  # Output for the 15 min bar plots
  output$fifteenmin<-renderPlotly({
    
    autoinvalidate()
    
    minute_15<-read.csv("http://ds01.ddfplus.com/historical/queryminutes.ashx?username=toddgross1&password=barchart&symbol=CL*0&maxrecords=50&interval=15", header = FALSE)
    colnames(minute_15)<-c("CMDTY","DATETIME","RNDM", "Open", "High", "Low", "Close", "Volume")
    minute_15$Time<-format(as.POSIXct(strptime(minute_15$DATETIME, format = "%Y-%m-%d %H:%M")), format = "%H:%M")
    minute_15<-tail(minute_15[,c(9,4,5,6,7)])
    
    hovertxt <- Map(function(x, y)paste0(x, ":", y), names(minute_15), minute_15[1:50,])
    hovertxt <- Reduce(function(x, y)paste0(x, "<br&gt;", y), hovertxt)
    
    plot_ly(minute_15[1:10,], x = ~Time, 

      type="candlestick", open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%

      layout(showlegend = FALSE,
             yaxis = list(title = "Price", domain = c(0, 0.9)),
             annotations = list(
               list(xref = "paper", yref = "paper",
                    x = 0, y = 1.05, showarrow = F,
                    xanchor = "left", yanchor = "top",
                    align = "center",
                    text = paste0("<b>DAILY 15 MIN BAR CHART</b>","<br><b>CRUDE</b>")),
               
               list(xref = "paper", yref = "paper",
                    x = 0.7, y = 1, showarrow = F,
                    xanchor = "left", yanchor = "top",
                    align = "left",
                    text = paste(range(minute_15$Time), collapse = " : "),
                    font = list(size = 10))),
             plot_bgcolor = "#f2f2f2")
  })
  
  output$expected_returns<-renderPlot({
    
    autoinvalidate()
    
    data<-datasetinput()
    data<-data[order(data$DATETIME1),]  ## I MADE THIS CHANGE

    last_values<-tail(data,1)
    last_rfo_atr<-last_values$RET_FROM_OPEN_ATR
    last_rfo<-round(last_values$RET_FROM_OPEN*100,2)

    data<-data[which(data$BAR_CLASS3!=89),]

    if((last_values$HIGH3-last_values$LOW3) > 1.2 * (last_values$ATRPREV))
    {
      data_last_rfo <- data[which((data$HIGH3-data$LOW3) > 1.2 * (data$ATRPREV)),]
    } else if((last_values$HIGH3-last_values$LOW3) > 0.8 * (last_values$ATRPREV))
    {
      data_last_rfo <- data[which((data$HIGH3-data$LOW3) > 0.8 * (data$ATRPREV)),]
    } else {
      data_last_rfo <- data
    }

    data_last_rfo<-data[which((data$RET_FROM_OPEN_ATR > (last_rfo_atr - 0.1*abs(last_rfo_atr))) & (data$RET_FROM_OPEN_ATR < (last_rfo_atr + 0.1*abs(last_rfo_atr)))),]
    
    uniqv <- unique(data_last_rfo[which(data_last_rfo$TIME1==time()), ]$BAR_CLASS3)
    
    max_prob<-uniqv[which.max(tabulate(match(data_last_rfo[which(data_last_rfo$TIME1==time()), ]$BAR_CLASS3, uniqv)))]
    
    
    if(input$bar_class_type=="Most Likely")
    {
      return(ggplot(data[which(data$BAR_CLASS3 == max_prob),])+
               geom_histogram(aes(PRICE, y=(..count../sum(..count..)), fill=..density..), 
                              bins = input$binsize, color="Black") + 
               theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) + 
               scale_x_continuous(breaks = seq(min(round(data$PRICE,2)),max(round(data$PRICE,2)), 
                                               by=(round((max(data$PRICE)-min(data$PRICE))/input$binsize,2)))) +
               geom_vline(xintercept=closingdata(), color="Red") +
               xlab("Return to Close") +
               ylab(" Probability") + 
               ggtitle("EXPECTED RETURNS",subtitle=paste("Return to Close for Bar Class:", max_prob)) +
               theme(plot.title = element_text(lineheight = 4, face="bold",hjust = 0.5)) +
               theme(axis.title = element_text(face = "bold"))+ 
               theme(axis.text = element_text(face="bold")))
    } else if(input$bar_class_type=="Composite")
    {
      return(ggplot(data)+
               geom_histogram(aes(probability, y=(..count../sum(..count..)), fill=..density..), 
                              bins = input$binsize, color="Black") + 
               theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) + 
               scale_x_continuous(breaks = seq(min(round(data$PRICE,2)),max(round(data$PRICE,2)), 
                                               by=(round((max(data$PRICE)-min(data$PRICE))/input$binsize,2)))) +
               geom_vline(xintercept=closingdata(), color="Red") +
               xlab("Return to Close") +
               ylab(" Probability") + 
               ggtitle("EXPECTED RETURNS",subtitle = "Return to Close") +
               theme(plot.title = element_text(lineheight = 4, face="bold",hjust = 0.5)) +
               theme(axis.title = element_text(face = "bold"))+ 
               theme(axis.text = element_text(face="bold")))
      
    } else {
      return(ggplot(data[which(data$BAR_CLASS3 == input$bar_class_type),])+
             geom_histogram(aes(PRICE, y=(..count../sum(..count..)), fill=..density..), 
                              bins = input$binsize, color="Black") + 
             theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) + 
             scale_x_continuous(breaks = seq(min(round(data$PRICE,2)),max(round(data$PRICE,2)), 
                                             by=(round((max(data$PRICE)-min(data$PRICE))/input$binsize,2)))) +
             geom_vline(xintercept=closingdata(), color="Red") +
             xlab("Return to Close") +
             ylab("Probability") + 
             ggtitle("EXPECTED RETURNS",subtitle=paste("Return to Close for Bar Class:", input$bar_class_type)) +
             theme(plot.title = element_text(lineheight = 4, face = "bold",hjust = 0.5)) +
             theme(axis.title = element_text(face = "bold"))+ 
             theme(axis.text = element_text(face="bold")))
    }
    
    
  })
  
  output$projections<-renderPlot({
    
    autoinvalidate()
    
    data_projections<-data_projections()
    
    data_projections <-na.omit(data_projections)
    
    data_projections$time <- strptime(data_projections$TIME1, format = '%H:%M')
    
    time_now <- strptime(time(), format = '%H:%M')
    
    start<-length(data_projections$time[data_projections$time <= time_now])
    
    end<-length(data_projections$time[data_projections$time < strptime('18:00', format = '%H:%M')])
    
    if (time_now < data_projections$time[end]){
      projdf <- data_projections[start:end, ]
    } else{
      projdf <- rbind(data_projections[start:nrow(data_projections),], data_projections[1:end,])
    }
    
    return( ggplot(projdf) + 
    #}
             geom_point(aes(x=factor(TIME1, levels=projdf$TIME1), y=Projected_High), color="BLACK") + 
             geom_point(aes(x=factor(TIME1, levels=projdf$TIME1), y=Projected_Low), color="BLUE") +
             geom_point(aes(x=factor(TIME1, levels=projdf$TIME1), y=sd_high), color="#008000", shape=6) + 
             geom_point(aes(x=factor(TIME1, levels=projdf$TIME1), y=sd_low), color="RED", shape=6) + 
             xlab("Time") + 
             ylab("Price in $") + 
              theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) + 
             ggtitle("PROJECTIONS:HIGH LOW PATH CALCULATOR®", subtitle=paste0("Bar Class:", input$bar_class_type)) + 
             scale_y_continuous(breaks=round(seq(min(data_projections$sd_low), 
                                                 max(data_projections$sd_high),by=((max(data_projections$sd_high)-min(data_projections$sd_low))/10)),2), 
                                minor_breaks = seq(min(data_projections$sd_low), max(data_projections$sd_high),0.01)) + 
             theme(plot.title = element_text(lineheight = 4, face="bold",hjust = 0.5)) +
             theme(axis.title = element_text(face = "bold")) + 
             theme(axis.text = element_text(face="bold")))    

  })
  
  output$projections_table <- renderDataTable({
    
    autoinvalidate()
    
    data_projections<-data_projections()
     
    data_projections <-na.omit(data_projections)
    # Daisy made changes{
    #
    # start<-which(data_projections$TIME1==time(), arr.ind = TRUE)
    # 
    # end<-which(data_projections$TIME1=="23:45", arr.ind = TRUE)
    # 
    # return(t(data_projections[start:end,]))	
    
    data_projections$time <- strptime(data_projections$TIME1, format = '%H:%M')
    time_now <- strptime(time(), format = '%H:%M')
    start<-length(data_projections$time[data_projections$time <= time_now])
    end<-length(data_projections$time[data_projections$time < strptime('18:00', format = '%H:%M')])
    
    if (time_now < data_projections$time[end]){
      projdf <- data_projections[start:end, ]
    } else{
      projdf <- rbind(data_projections[start:nrow(data_projections),], data_projections[1:end,])
    }
    
    return(t(projdf[,1:5]))
    #}
    
  }, options=list(searching=FALSE, paging=FALSE, rownames=FALSE))
  
  output$drawdown_table <- renderDataTable({
    
    autoinvalidate()
    
    data<-datasetinput()
    
    atr15_mean<-sum(data.frame(tail(data$ATR1,10))*seq(from=1, to=10, by=1))*sqrt(93)/55
      
    data<-data[order(data$DATETIME1),]  ## I MADE THIS CHANGE

    last_values<-tail(data,1)
    last_rfo_atr<-last_values$RET_FROM_OPEN_ATR
    last_rfo<-round(last_values$RET_FROM_OPEN*100,2)

    data<-data[which(data$BAR_CLASS3!=89),]

    if((last_values$HIGH3-last_values$LOW3) > 1.2 * (last_values$ATRPREV))
    {
      data_last_rfo <- data[which((data$HIGH3-data$LOW3) > 1.2 * (data$ATRPREV)),]
    } else if((last_values$HIGH3-last_values$LOW3) > 0.8 * (last_values$ATRPREV))
    {
      data_last_rfo <- data[which((data$HIGH3-data$LOW3) > 0.8 * (data$ATRPREV)),]
    } else {
      data_last_rfo <- data
    }

    data_last_rfo<-data[which((data$RET_FROM_OPEN_ATR > (last_rfo_atr - 0.1*abs(last_rfo_atr))) & (data$RET_FROM_OPEN_ATR < (last_rfo_atr + 0.1*abs(last_rfo_atr)))),]

    uniqv <- unique(data_last_rfo$BAR_CLASS3)

    max_prob<-uniqv[which.max(tabulate(match(data_last_rfo$BAR_CLASS3, uniqv)))]

    if(input$bar_class_type == "Most Likely")
    {
      draw_down_1<-ddply(data[which(data$BAR_CLASS3 ==max_prob),], ~TIME1, summarise,
                         
                         Mean_DD=round((mean(DD)*atr15_mean),2), 
                         
                         sd_DD=round(((mean(DD)+sd(DD))*atr15_mean),2),
                         
                         sd_DU=round(((mean(DU)+sd(DU))*atr15_mean),2), 
                         
                         Mean_DU=round((mean(DU)*atr15_mean),2))
    } else {
      draw_down_1<-ddply(data[which(data$BAR_CLASS3 == input$bar_class_type),], ~TIME1, summarise,
                       
                       Mean_DD=round((mean(DD)*atr15_mean),2), 
                       
                       sd_DD=round(((mean(DD)+sd(DD))*atr15_mean),2),
                       
                       sd_DU=round(((mean(DU)+sd(DU))*atr15_mean),2), 
                       
                       Mean_DU=round((mean(DU)*atr15_mean),2))
    }
    start_dd<-which(draw_down_1$TIME1==time(), arr.ind = TRUE)
    
    end_dd<-start_dd+1
    
    return(t(draw_down_1[start_dd:end_dd,]))
    
  }, options=list(searching=FALSE, paging=FALSE, rownames=FALSE))

})
