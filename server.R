library(shiny)
library(plotly)
library(igraph)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
  fData = reactive({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file
    
    if (is.null(inFile)){
    data = read.csv(file="systemicR.csv",sep=",")    
    }
    else
      read.csv(file=inFile$datapath)
  })
  
  
  
  
  
  
  
  
  observeEvent(input$compute, {
    
    
    output$text1 <- renderText({ 
      data = fData()
      na = dim(data)[1]   #columns (assets)
      Ri = matrix(data[,1],na,1)    #Aggregate risk by asset
      X = data[1:na,2:(na+1)]
      X = matrix(as.numeric(as.matrix(X)),na,na)
      S = as.numeric(sqrt(t(Ri) %*% X %*% Ri)) 
      paste("Overall Risk Score",round(S,2))
    })
    
    output$plot <- renderPlot({
      data = fData()
      na = dim(data)[1]   #columns (assets)
      bnames = names(data)
      Ri = matrix(data[,1],na,1)    #Aggregate risk by asset
      X = data[1:na,2:(na+1)]
      X = matrix(as.numeric(as.matrix(X)),na,na)

      #GRAPH NETWORK: plot of the assets and the links with directed arrows
      Y = X; diag(Y)=0
      g = graph.adjacency(Y)
      V(g)$color = "#ffec78"
      V(g)$color[degree(g)==max(degree(g))] = "#ff4040"
      V(g)$color[degree(g)==min(degree(g))] = "#b4eeb4"
      V(g)$size = Ri*8+10
      
      plot.igraph(g,layout=layout.fruchterman.reingold,edge.arrow.size=0.5,
                  vertex.label.color="black",edge.arrow.width=0.8,
                  vertex.label=bnames[1:na+1],
                  vertex.label.cex=0.8)
    }, height = 550, width = 800)
    
    output$text2 <- renderText({ 
      data = fData()
      na = dim(data)[1]   #columns (assets)
      Ri = matrix(data[,1],na,1)    #Aggregate risk by asset
      X = data[1:na,2:(na+1)]
      X = matrix(as.numeric(as.matrix(X)),na,na)
      Y = X; diag(Y)=0
      g = graph.adjacency(Y)
      H = ((sum(degree(g)^2))/na)/((sum(degree(g)))/na)
      paste("Fragility of the Network is ",round(H,2))
    })
    
    output$plot2 <- renderPlotly({
      data = fData()
      na = dim(data)[1]   #columns (assets)
      Ri = matrix(data[,1],na,1)    #Aggregate risk by asset
      X = data[1:na,2:(na+1)]
      X = matrix(as.numeric(as.matrix(X)),na,na)
      S = as.numeric(sqrt(t(Ri) %*% X %*% Ri)) 
      RiskIncr = 0.5 * as.numeric((X %*% Ri + t(X) %*% Ri))/S
      RiskDecomp = RiskIncr * Ri

      sorted_RiskDecomp = sort(RiskDecomp,decreasing=TRUE,index.return=TRUE)
      RD = as.numeric(sorted_RiskDecomp$x)
      idxRD = as.character(sorted_RiskDecomp$ix)
      idxRD = paste("B",idxRD,sep="")
      
      xAx <- list(
        title = "Node Number"
      )
      yAx <- list(
        title = "Risk Decomposition")
      plot_ly(y = RD,x = idxRD,marker = list(color = toRGB("dark green")),type="bar")%>%
              layout(xaxis = xAx, yaxis = yAx)
#      barplot(t(RD),col="dark green",xlab="Node Number",names.arg=idxRD,cex.names=0.75)   
      
          })
    
    output$plot3 <- renderPlotly({
      data = fData()
      na = dim(data)[1]   #columns (assets)
      Ri = matrix(data[,1],na,1)    #Aggregate risk by asset
      X = data[1:na,2:(na+1)]
      X = matrix(as.numeric(as.matrix(X)),na,na)
      
      #GRAPH NETWORK: plot of the assets and the links with directed arrows
      Y = X; diag(Y)=0
      g = graph.adjacency(Y)
      cent = evcent(g)$vector
      #  print("Normalized Centrality Scores")
      sorted_cent = sort(cent,decreasing=TRUE,index.return=TRUE)
      Scent = sorted_cent$x
      idxScent = sorted_cent$ix
      idxScent = paste("B",idxScent,sep="")
      
      
      xAx <- list(
        title = "Node Number"
      )
      yAx <- list(
        title = "Eigen Value Centrality"
      )
     plot_ly(y = as.numeric(t(Scent)),x = idxScent,marker = list(color = toRGB("red")),type="bar")%>%
              layout(xaxis = xAx, yaxis = yAx)
#    barplot(t(Scent),col="dark red",xlab="Node Number",names.arg=idxScent,cex.names=0.75)
      
    })
    
      
    
    output$plot4 <- renderPlotly({
      data = fData()
      na = dim(data)[1]   #columns (assets)
      Ri = matrix(data[,1],na,1)    #Aggregate risk by asset
      X = data[1:na,2:(na+1)]
      X = matrix(as.numeric(as.matrix(X)),na,na)
      
      S = as.numeric(sqrt(t(Ri) %*% X %*% Ri)) 
      RiskIncr = 0.5 * as.numeric((X %*% Ri + t(X) %*% Ri))/S
      
      #COMPUTE RISK INCREMENTS
      sorted_RiskIncr = sort(RiskIncr,decreasing=TRUE,index.return=TRUE)
      RI = sorted_RiskIncr$x
      idxRI = sorted_RiskIncr$ix
      idxRI = paste("B",idxRI,sep="")
      
      
      xAx <- list(
        title = "Node Number"
      )
      yAx <- list(
        title = "Risk Increments"
      )
      plot_ly(y = as.numeric(t(RI)),x = idxRI,marker = list(color = toRGB("green")),type="bar")%>%
              layout(xaxis = xAx, yaxis = yAx)
      
#        barplot(t(RI),col="dark blue",xlab="Node Number",names.arg=idxRI,cex.names=0.75)    
    
      })
    
    
    #CRITICALITY
    #Criticality is compromise-weighted centrality. 
    #This is an element-wise multiplication of vectors $C$ and $x$. 
    
    output$plot5 <- renderPlotly({
      data = fData()
      na = dim(data)[1]   #columns (assets)
      Ri = matrix(data[,1],na,1)    #Aggregate risk by asset
      X = data[1:na,2:(na+1)]
      X = matrix(as.numeric(as.matrix(X)),na,na)

      #GRAPH NETWORK: plot of the assets and the links with directed arrows
      Y = X; diag(Y)=0
      g = graph.adjacency(Y)
      cent = evcent(g)$vector
      
      crit = Ri * cent
      print("Criticality Vector")
      print(crit) 
      sorted_crit = sort(crit,decreasing=TRUE,index.return=TRUE)
      Scrit = sorted_crit$x
      idxScrit = sorted_crit$ix
      idxScrit = paste("B",idxScrit,sep="")
      
      
      xAx <- list(
        title = "Node Number"
      )
      yAx <- list(
        title = "Criticality Vector"
      )
      plot_ly(y = as.numeric(t(sorted_crit$x)),x = idxScrit,marker = list(color = toRGB("orange")),type="bar")%>%
              layout(xaxis = xAx, yaxis = yAx)
      
#          barplot(t(Scrit),col="orange",xlab="Node Number",names.arg=idxScrit,cex.names=0.75)
    })
    
  })
  
})