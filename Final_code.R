
# Loading Required Libraries

library(xts)
library(TSstudio)
library(ggplot2)
library(ggthemes)
library(forecast)
library(tseries)
library(vars)
library(urca)
library(readr)
library(tseries)
library(kableExtra)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(vars)
library(plyr)
library(igraph)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyFiles)
library(plyr)
library(base)
library(dplyr)
library(dplyr)
library(stringr)
library(PerformanceAnalytics)
library(reshape2)


# Reading the Data

setwd("C:\\Users\\karti\\Desktop\\Capstone")
nifty_50 = read.csv('NIFTY 50_Data.csv')
nifty_auto = read.csv('NIFTY AUTO_Data.csv')
nifty_bank = read.csv('NIFTY BANK_Data.csv')
nifty_fin_25_50= read.csv("NIFTY FINANCIAL SERVICES 25_50 INDEX_Data.csv")
nifty_fmcg = read.csv('NIFTY FMCG_Data.csv')
nifty_it = read.csv('NIFTY IT_Data.csv')
nifty_media= read.csv("NIFTY MEDIA_Data.csv")
nifty_pharma = read.csv('NIFTY PHARMA_Data.csv')
nifty_consumer_durables = read.csv('NiftyConsumerDurables.csv')
nifty_realty = read.csv("NIFTY REALTY_Data.csv")

# Creating a Date object

Date <- as.Date(as.character(nifty_50$Date), "%d-%m-%Y")



# Converting the closing price from character data type to numeric data type
nifty_50$Close <- as.numeric(nifty_50$Close)
nifty_auto$Close <- as.numeric(nifty_auto$Close)
nifty_bank$Close <- as.numeric(nifty_bank$Close)
nifty_consumer_durables$Close <- as.numeric(nifty_consumer_durables$Close)
nifty_fin_25_50$Close <- as.numeric(nifty_fin_25_50$Close)
nifty_fmcg$Close <- as.numeric(nifty_fmcg$Close)
nifty_it$Close <- as.numeric(nifty_it$Close)
nifty_media$Close <- as.numeric(nifty_media$Close)
nifty_pharma$Close <- as.numeric(nifty_pharma$Close)
nifty_realty$Close <- as.numeric(nifty_realty$Close)

# Getting all close prices of stocks
all_close <- cbind(nifty_50$Close,
                   nifty_auto$Close,
                   nifty_bank$Close,
                   nifty_consumer_durables$Close,
                   nifty_fin_25_50$Close,
                   nifty_fmcg$Close,
                   nifty_it$Close,
                   nifty_media$Close, 
                   nifty_pharma$Close,
                   nifty_realty$Close)

# Setting up column names of the Dataframe

colnames(all_close) <- c("Nifty_50","Nifty_Auto","Nifty_Bank","Nifty_Consumer_Durables",
                         "Nifty_Fin_Ser_25_50","Nifty_FMCG","Nifty_IT","Nifty_Media","Nifty_Pharma",
                         "Nifty_Realty")
all_close <- na.omit(all_close)


# Checking for Null Values

##sum(is.na(all_close[,'Nifty_Auto']))  # There are no null values present in the data
#sum(is.na(all_close[,'Nifty_Bank']))
#sum(is.na(all_close[,'nifty_fin_25_50$Close']))
#sum(is.na(all_close[,'Nifty_Consumer_Durables']))
#sum(is.na(all_close[,'Nifty_Fin_Ser_20_50']))  # There are no null values present in the data
# sum(is.na(all_close[,'Nifty_FMCG']))
#sum(is.na(all_close[,'Nifty_IT']))
#sum(is.na(all_close[,'Nifty_Media']))
#sum(is.na(all_close[,'Nifty_Pharma']))
#sum(is.na(all_close[,'Nifty_Realty']))
#sum(is.na(Date))




# Creating an XTS object with close prices of all stocks

all_close <- xts(all_close, Date)

#Summary of the Statistics
summary(all_close)

# Plotting the Time Series Data

#ts_plot(all_close,
#        title = "Close Prices",
#        Ytitle = "Close Price")



# Calculating the Daily Returns

# Function defining Daily Returns

daily_returns<-function(df,stock_name)
{
  x <- data.frame(df)
  new_col <- paste("daily_return",stock_name)
  x[new_col]<-0
  for (i in 2:nrow(x[stock_name]))
  {
    x[new_col][i,1] = (x[stock_name][i,1]-x[stock_name][i-1,1])/x[stock_name][i-1,1]*100
    
  }
  return(x)
}

# Calculating Daily_Returns for all the stocks

#colnames(all_close)

all_close_dr_50 = daily_returns(all_close,"Nifty_50")

all_close_dr_auto = daily_returns(all_close,"Nifty_Auto")

all_close_dr_bank = daily_returns(all_close,"Nifty_Bank")

all_close_dr_consumer_durables = daily_returns(all_close,"Nifty_Consumer_Durables")

all_close_dr_fin_ser_25_50 = daily_returns(all_close,"Nifty_Fin_Ser_25_50")

all_close_dr_fmcg = daily_returns(all_close,"Nifty_FMCG")

all_close_dr_it = daily_returns(all_close,"Nifty_IT")

all_close_dr_media = daily_returns(all_close,"Nifty_Media")

all_close_dr_pharma = daily_returns(all_close,"Nifty_Pharma")

all_close_dr_realty = daily_returns(all_close,"Nifty_Realty")


# Getting all Daily Returns and creating an XTS object with that Daily Returns 
dr <- cbind.data.frame(all_close_dr_50[,'daily_return Nifty_50'], 
                       all_close_dr_auto[,'daily_return Nifty_Auto'],
                       all_close_dr_bank[,'daily_return Nifty_Bank'],
                       all_close_dr_consumer_durables[,'daily_return Nifty_Consumer_Durables'],
                       all_close_dr_fin_ser_25_50[,'daily_return Nifty_Fin_Ser_25_50'],
                       all_close_dr_fmcg[,'daily_return Nifty_FMCG'], 
                       all_close_dr_it[,'daily_return Nifty_IT'],
                       all_close_dr_media[,'daily_return Nifty_Media'],
                       all_close_dr_pharma[,'daily_return Nifty_Pharma'],
                       all_close_dr_realty[,'daily_return Nifty_Realty'])

# ECCM table to select AR amd MA lags

#install.packages("MTS")
#library(MTS)
#a<-Eccm(dr)

# Setting up column names
colnames(dr) <- c("Nifty_50","Nifty_Auto","Nifty_Bank","Nifty_Consumer_Durables",
                  "Nifty_Fin_Ser_25_50","Nifty_FMCG","Nifty_IT","Nifty_Media","Nifty_Pharma",
                  "Nifty_Realty")
dr <- xts(dr, Date)
#dr

# Plotting Daily Returns

#ts_plot(dr[,'Nifty_50'],title = "Nifty 50 Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_Auto'],title = "Nifty Auto Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_Consumer_Durables'],title = "Nifty Consumer Durables Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_Fin_Ser_25_50'],title = "Nifty Financial Services 25_50 Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_Bank'],title = "Nifty Bank Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_FMCG'],title = "Nifty FMCG Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_IT'],title = "Nifty IT Banks Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_Media'],title = "Nifty Media Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_Pharma'],title = "Nifty Pharma Daily Returns",Ytitle = "Daily Returns")
#ts_plot(dr[,'Nifty_Realty'],title = "Nifty Realty Daily Returns",Ytitle = "Daily Returns")


# Stationart Test

#adf.test(dr[,'Nifty_50'])   # Data is stationary
#adf.test(dr[,'Nifty_Auto'])   # Data is Stationary
#adf.test(dr[,'Nifty_Bank']) # Data is stationary
#adf.test(dr[,'Nifty_Metal'])   # Data is stationary
#adf.test(dr[,'Nifty_FMCG'])   # Data is Stationary
#adf.test(dr[,'Nifty_IT']) # Data is stationary
#adf.test(dr[,'Nifty_Media'])   # Data is stationary
#adf.test(dr[,'Nifty_Pharma']) # Data is stationary
#adf.test(dr[,'Nifty_Realty'])   # Data is stationary



#VAR

#install.packages("kableExtra")
#install.packages("dplyr")
#install.packages("formattable")
#install.packages("vars")

# v1 <-VARselect(dr,lag.max=10)
#v1$selection

#v1$criteria


### Step 2. Moving Average Representation

#MA_lag <- 6
#theta_temp <- Phi(vol_var,nstep = MA_lag)
#svar_theta_temp <- Phi(vol_svar,nstep = MA_lag)
### extract MA coefficients
#library(plyr)
#theta.list <- alply(theta_temp,3)
#svar_theta_list <- alply(svar_theta_temp,3)
#theta.list


#  FEVD  Table

make.table <- function(mat,names,date.col){
  
  temp.mat <- mat
  
  diag(temp.mat) <- 0
  to <- apply(temp.mat,2,sum)
  from <- apply(temp.mat,1,sum)
  
  mat.df <- sum.table(mat,names,to,from)
  
  net <- to - from
  
  from <- as.data.frame(from,row.names = names)
  to <- as.data.frame(to,row.names = names)
  net <- as.data.frame(net,row.names = names)
  
  colnames(from) <- date.col
  colnames(to) <- date.col
  colnames(net) <- date.col
  
  list(table=mat.df,To=to,From=from,Net=net)
  
}


cond.sum <- function(mat){
  mat.sq <- mat%*%t(mat)
  low.tri <- mat.sq[lower.tri(mat.sq,diag=F)]
  up.tri <- mat.sq[upper.tri(mat.sq,diag=F)]
  new.mat <- sum(low.tri) + sum(up.tri)
  new.mat
}

# Function to calculate row sums and column sums

sum.table <- function(mat,names,to,from){
  mat.df <- as.data.frame(mat)
  colnames(mat.df) <- names
  mat.df$From <- from
  
  to2 <- c(to,mean(to))
  
  mat.df <- rbind(mat.df,to2)
  rownames(mat.df) <- c(names,"To")
  mat.df
}

# FEVD matrix for error variance

fevd.matrix <- function(resid.var,ma.coef){
  ### ei and ej are identity matrices, or
  ### each column in ei and ej is a vector with unity at the i/jth element
  ### see equation 2.7 in Pesaran and Shin (1997)
  Qinv <- t(chol(resid.var))
  
  nobs <- dim(Qinv)[1]
  ei <- diag(nobs)
  ej <- diag(nobs)
  
  ### Initialize matrix of network connections
  Dg <- matrix(NA,ncol=nobs,nrow=nobs)
  
  ### Derive Forecast Error Variance Decomposition (FEVD) with 
  ### Generalized Variance Decomposition method
  for(j in 1:(ncol(ej))){
    ### the following steps calculate the denominator
    ### to estimate FEVD on page 3 of Pesaran and Shin (1997)
    ejj <- ej[,j]
    
    denom.step1 <- llply(ma.coef,function(x) t(ejj)%*%x%*%resid.var%*%t(x)%*%ejj)
    denom = Reduce('+',denom.step1)
    for(i in 1:(ncol(ej))){
      ### the following steps calculates equation 2.10 in Pesaran and Shin (1997)
      eii <- ej[,i]
      num.step1 <- llply(ma.coef,function(x) t(ejj)%*%x%*%Qinv%*%eii)
      num.squared <- llply(num.step1,function(x) x^2)
      num <- Reduce('+',num.squared)
      
      Dg[j,i] <- (num/denom)
      
    }
  }
  Dg
}



#error.variance.total <- fevd.matrix(svar_ecov,theta.list)
#spillover_table <- make.table(error.variance.total,colnames(dr),"2007-01-02")
#spill_table <- 100*round(spillover_table[["table"]],2)
#kable(spill_table,caption = "Return Spillovers") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




# Section 3.3: Full Sample Connectedness Table

# install.packages("plyr")

# Function for Directional matrix

directional.matrix <- function(resid.var,ma.coef){
  ### ei and ej are identity matrices, or
  ### each column in ei and ej is a vector with unity at the i/jth element
  ### see equation 2.7 in Pesaran and Shin (1997)
  cov.error <- cov(resid.var)
  nobs <- dim(cov.error)[1]
  ei <- diag(nobs)
  ej <- diag(nobs)
  
  ### Initialize matrix of network connections
  Dg <- matrix(NA,ncol=nobs,nrow=nobs)
  
  ### Derive Forecast Error Variance Decomposition (FEVD) with 
  ### Generalized Variance Decomposition method
  for(j in 1:(ncol(ej))){
    ### the following steps calculate the denominator
    ### to estimate FEVD on page 3 of Pesaran and Shin (1997)
    ejj <- ej[,j]
    little.sigma <- 1/sqrt(cov.error[j,j])
    
    denom.step1 <- llply(ma.coef,function(x) t(ejj)%*%x%*%cov.error%*%t(x)%*%ejj)
    denom = Reduce('+',denom.step1)
    for(i in 1:(ncol(ej))){
      ### the following steps calculates equation 2.10 in Pesaran and Shin (1997)
      eii <- ei[,i]
      num.step1 <- llply(ma.coef,function(x) t(ejj)%*%x%*%cov.error%*%eii)
      num.squared <- llply(num.step1,function(x) x^2)
      num.sum <- Reduce('+',num.squared)
      num <- little.sigma*num.sum
      
      Dg[j,i] <- (num/denom)
      
    }
  }
  Dg
}

# Function to normalize the values

normalize <- function(amatrix){
  
  norm <- apply(amatrix,1,sum)
  
  amatrix/norm
}



#  Networks
#dir.mat <- directional.matrix(res_t,theta.list)
#rownames(dir.mat) <- colnames(dr)
#colnames(dir.mat) <- rownames(dir.mat)


#D <- normalize(dir.mat)*100
#D
#df.list <- make.table(D,rownames(D),"2007-01-02")
#df.list <- round(df.list[["table"]],2)

#kable(df.list,caption = "Returns (Fully) Connectedness") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




# Full Sample FEVD as a Network

#install.packages("igraph")

#net.mat = t(D) - D
#net.melt1 <- melt(net.mat)
#net.melt <- net.melt1 %>% filter(value != 0 )
#colnames(net.melt)[3] <- "weight"
### calculate each percentile of the net pairwise connectedness values
### and choose only the top 10%
#net.quant <- quantile(net.melt$weight,prob=seq(0,1,by=0.01))
#new.net1 <- net.melt[net.melt$weight >= net.quant[90],]
#new.net <- new.net1[new.net1[,1] != new.net1[,2],]


### create igraph graph
##E(net.network)$weight <- as.numeric(new.net$weight)

### set graph nodes
#V(net.network)
### set edge colors
#E(net.network)$color <- ifelse(E(net.network)$weight >= net.quant[99],"black",
#                              ifelse(E(net.network)$weight < net.quant[99] & E(net.network)$weight >= net.quant[95],"red",
#plot(net.network,layout=layout.circle(net.network),
#    main="Firm Connectedness \n ( Return Spillover)",
#    xlab = "black: 1st percentile \n red: 5th percentile \n orange: 10th percentile \n node size: number of edeges connected to the node")






# Rolling Window

normalize <- function(amatrix){
  
  norm <- apply(amatrix,1,sum)
  
  amatrix/norm
}


get.trace <- function(x){ 
  A0 <- x%*%t(x)
  sum(A0)
}

# Function defining the start and end dates from the data choosen

datesFn <- function(theData.xts,window=100,iter=10){
  ### create a sequence to use as cut offs for the rolling window
  ### estimation. iter determines how far to move the window forward
  
  thedates <- index(theData.xts)
  start <- c()
  end <- c()
  maxdate <- max(thedates)
  t <- (length(thedates) - window)/iter
  i <- 1
  bigT <- t*iter
  while(i < bigT){
    
    start <- c(start,thedates[i])
    end <- c(end,thedates[i+window])
    i <- i + iter
  }
  
  date.df <- na.omit(data.frame(start=as.Date(start),end=as.Date(end)))
  
  end <- na.omit(end)
  end <- as.character(as.Date(end))
  
  
  list(dates = end,
       date.df = date.df)
}

# Date functions for dynamic inputs

# datesFn(dat,window=input$window,iter=input$windowDivider)

# Vector Auto Regression Function with all the above steps from VAR, 
# MA representation to Plotting Network Diagram.

vector_autoreg <- function(theData.xts,window=100,iter=10,ar_lag=1,ma_lag=6){
  ### create a sequence to use as cut offs for the rolling window
  ### estimation. iter determines how far to move the window forward
  dates <- datesFn(theData.xts,window=100,iter=10)
  date_names <- dates[["dates"]]
  date.df <- dates[["date.df"]]
  ndates <- length(date_names)
  alldates <- index(theData.xts)
  
  model_list <- vector("list",length=length(date_names))
  names(model_list) <- date_names
  centrality <- model_list
  ### initialize a list to hold index values
  fevd.list <- model_list
  ### names of the list come from the index of .xts data object
  spillover.table.list <- model_list
  vol.conn.index <- model_list
  ### names of the list come from the index of .xts data object
  network.list <- model_list
  net.df.list <- model_list
  nvars <- ncol(theData.xts)
  
  
  for(d in 1:ndates){
    ### estimate VAR(p) and SVAR(p)
    var_dates <- alldates[alldates >= date.df[d,"start"] & alldates <= date.df[d,"end"]]
    
    var.vol.temp <- VAR(theData.xts[var_dates,],p=ar_lag,type="none")
    amat <- diag(nvars)
    amat[lower.tri(amat)] <- NA
    svar.vol.temp <- SVAR(var.vol.temp,estmethod = "direct",Amat=amat)
    
    model_list[[date_names[d]]] <- list(orthogonal=svar.vol.temp,
                                        non_orthogonal = var.vol.temp)
    ### get MA coefficients
    theta.temp <- Phi(var.vol.temp,nstep=ma_lag)
    svar.theta <- Phi(svar.vol.temp,nstep=ma_lag)
    ### convert array to listn.ahead
    theta.list <- alply(theta.temp,3)
    svar.theta.list <- alply(svar.theta,3)
    ################## SPILLOVER #########################
    svar_ecov <-svar.vol.temp$Sigma.U
    error.variance.total <- fevd.matrix(svar_ecov,svar.theta.list)*100
    spillover.table.list[[date_names[d]]] <- make.table(error.variance.total,colnames(theData.xts),date_names[d])
    
    
    Qinv <- t(chol(svar_ecov))
    irf <- llply(theta.list,function(x) x%*%Qinv)
    num = llply(irf,cond.sum)
    ### calculate own variance shares (i.e. do not subtract the diagonal of the matrix)
    den = llply(irf,get.trace)
    sum.num = sum(unlist(num))
    sum.den = sum(unlist(den))
    S = sum.num*100/sum.den
    fevd.list[[date_names[d]]] <- S
    ########################################################
    
    ################## CONNECTEDNESS #######################
    e.cov <- cov(residuals(var.vol.temp))
    ### create directional network table
    Sconn = directional.matrix(residuals(var.vol.temp),theta.list)
    ### to normalize or not to normalize
    D = normalize(Sconn)*100
    ### calculate total connectedness index level
    vol.conn.index[[date_names[d]]] = (sum(apply(D,1,sum)  - diag(D)))/ncol(D)
    ########################################################
    
    ################## Networks #######################
    rownames(D) <- colnames(theData.xts)
    colnames(D) <- rownames(D)
    
    df.list <- make.table(D,rownames(D),date_names[d])
    
    net.df.list[[date_names[d]]] <- df.list
    ### Create network Graph
    
    ### calculate net pairwise directional connectedness, Diebold pg 4.
    ### the calculation t(D) - D is the net pairwise equivalent of 
    ### the gross net = to - from, the calculation just above.
    
    net.mat = t(D) - D
    net.melt1 <- melt(net.mat)
    net.melt <- net.melt1 %>% filter(value != 0 )
    colnames(net.melt)[3] <- "weight"
    
    ### calculate each percentile of the net pairwise connectedness values
    ### and choose only the top 10%
    net.quant <- quantile(net.melt$weight,prob=seq(0,1,by=0.01))
    new.net1 <- net.melt[net.melt$weight >= net.quant[90],]
    new.net <- new.net1[new.net1[,1] != new.net1[,2],]
    
    ### create igraph graph
    
    net.network <- graph.data.frame(new.net,direct=T)
    E(net.network)$weight <- as.numeric(new.net$weight)
    ### set graph nodes
    #V(net.network)
    ### set edge colors
    E(net.network)$color <- ifelse(E(net.network)$weight >= net.quant[99],"black",
                                   ifelse(E(net.network)$weight < net.quant[99] & E(net.network)$weight >= net.quant[95],"red",
                                          ifelse(E(net.network)$weight < net.quant[95] & E(net.network)$weight >= net.quant[90],"orange","blue")))
    
    ### set node size
    V(net.network)$size <- degree(net.network)/.5
    
    network.list[[date_names[d]]] <- net.network
    #####################################################################################
    remove(var.vol.temp,theta.temp,theta.list,e.cov,Sconn,
           num,den,sum.num,sum.den,S)
  }
  
  list(models = model_list,
       spillover_table = spillover.table.list,
       spill = fevd.list,
       conn=vol.conn.index,
       nets = network.list,
       net_df = net.df.list,
       dates = date_names)
  
}

# Vector Auto Regression function for dynamic inputs

# vector_autoreg(dat,window=input$window,iter=input$windowDivider,ar_lag=input$ar_lag,ma_lag=input$ma_lag)




# Shiny Dashboard

# Defining the Dashboard body and its contents (UI)

ui <-dashboardPage(
  
  dashboardHeader(title = "Spillovers"),
  
  dashboardSidebar(
    sidebarMenu(
      
      selectInput("choice","Choose One:",choices=c("Returns","None"),selected = "Returns"),
      dateRangeInput("dateRange","Select Dates",
                     start = "2007-01-02",
                     end = "2021-10-28",weekstart = 1),
      textInput("stocks","Ticker Input",value = "Nifty_50,Nifty_Auto,Nifty_Bank,Nifty_Consumer_Durables,Nifty_Fin_Ser_25_50,Nifty_FMCG,Nifty_IT,Nifty_Media,Nifty_Pharma,Nifty_Realty"),
      numericInput("window","Rolling Window (Days)",value=100),
      numericInput("windowDivider","Window Increment",value=10),
      numericInput("ar_lag","AR Lag (Days)",value=1),
      numericInput("ma_lag","MA Lag",value=6),
      actionButton("getIt","Submit")
      
    )
  ),
  dashboardBody(
    navbarPage("",
               tabPanel(title = "Index",
                        fluidRow(
                          plotOutput("spilloverIndex",height="500px",hover="index_hover") %>% withSpinner(color="#0dc5c1")),
                        fluidRow(verbatimTextOutput("index_hover_data"))
               ),
               tabPanel(title = "Network Visual",
                        fluidRow(uiOutput("net_dates")),
                        fluidRow(
                          plotOutput("connectedness",height="600px")
                        )),
               tabPanel(title = "Give/Receive Graphs",
                        
                        fluidRow(
                          selectInput("direction","Direction",choices = c("To","From","Net"),selected="To"),
                          plotOutput("grid_graph",height = "500px")  %>% withSpinner(color="#0dc5c1")
                        )),
               tabPanel(title = "Give/Receive Table",
                        fluidRow(uiOutput("table_dates")),
                        
                        fluidRow(column(5,tableOutput("connected_table")),
                                 column(1,downloadButton("downloadConnectedness",label="Download Connectedness Table"),offset=6)),
                        
                        fluidRow(column(5,tableOutput("spillover_table")),
                                 column(1,downloadButton("downloadSpillover",label="Download Spillover Table"),offset=6))
               ),
               tabPanel(title = "Pairwise Plot",
                        fluidRow(uiOutput("sender_tickers"),uiOutput("receiver_tickers")),
                        fluidRow(uiOutput("data_viz_checkbo")),
                        fluidRow(plotOutput("receivers",hover = "net_pairwise_hover", height = "270px")  %>% withSpinner(color="#0dc5c1")),
                        fluidRow(plotOutput("net_pairwise_plot",hover = "net_pairwise_hover", height = "270px")  %>% withSpinner(color="#0dc5c1")),
                        fluidRow(verbatimTextOutput("net_pairwise_hover_data"))),
               tabPanel(title = "Data Viz",
                        fluidRow(uiOutput("data_viz_checkbox")),
                        fluidRow(plotOutput("return_data_plot")  %>% withSpinner(color="#0dc5c1")),
               ),
               tabPanel(title = "Returns Data",
                        fluidRow(
                          column(5,tableOutput("return_table")),
                          column(1,downloadButton("downloadReturns",label="Download Returns Data"),offset=6))),
               tabPanel(title = "VAR Metrics",
                        uiOutput('matrix'))
    )
  )
)



# Defining the Server Function

server <- function(input, output, session) {
  
  output$sender_tickers <- renderUI({
    ticker_symbols <- str_split(input$stocks,",")[[1]]
    selectInput("sender","Sender",choices = ticker_symbols,selected = ticker_symbols[1])
  })
  output$receiver_tickers <- renderUI({
    ticker_symbols <- str_split(input$stocks,",")[[1]]
    selectInput("receiver","Receiver",choices = ticker_symbols,selected = ticker_symbols[2])
  })
  output$data_viz_checkbox <- renderUI({
    ticker_symbols <- str_split(input$stocks,",")[[1]]
    checkboxGroupInput("data_viz_check","Select Ticker to Show:",choices = ticker_symbols,selected = ticker_symbols[1],inline=TRUE)
  })
  
  output$data_viz_checkbo <- renderUI({
    ticker_symbols <- str_split(input$stocks,",")[[1]]
    checkboxGroupInput("data_viz_chec","Select Receiver :",choices = ticker_symbols,selected = ticker_symbols[1],inline=TRUE)
  })
  
  ### read symbols from yahoo using getSymbols from quantmod package
  returns.data <- eventReactive(input$getIt,{
    tickers <- input$stocks
    tickers <- unlist(strsplit(tickers,split=","))
    tickers <- tickers[order(tickers)]
    dat<-dr
    dat <- dr[seq(as.Date(input$dateRange[1]),as.Date(input$dateRange[2]),by ="days"),]
    list(returns = dat)
  })
  
  # returns_data <- reactive({dat})
  returns_data <- reactive({dr[seq(as.Date(input$dateRange[1]),as.Date(input$dateRange[2]),by ="days"),]})
  
  output$return_table <- renderTable({
    returns_data()
  },rownames = TRUE)
  
  output$return_data_plot <- renderPlot({
    dat <- returns.data()
    chart.TimeSeries(dat[["returns"]][,input$data_viz_check],lwd=2,auto.grid=F,ylab="Return (%)",xlab="Time",
                     main="Daily Return (%) ",lty=1,
                     legend.loc="topright")
  })
  
  output$receivers <- renderPlot({
    all <- all_close[seq(as.Date(input$dateRange[1]),as.Date(input$dateRange[2]),by ="days"),]
    chart.TimeSeries(all[,input$data_viz_chec],lwd=2,auto.grid=F,ylab="Return (%)",xlab="Time",
                     main=" Receiver ",lty=1,
                     legend.loc="topright")
  })
  
  
  theDates <- eventReactive(input$getIt,{
    dat <- returns.data()
    if(input$choice == "Returns"){
      dat = dat[["returns"]]
    }else{
      dat = dat[["return_vol"]]
    }
    datesFn(dat,window=input$window,iter=input$windowDivider)
  })
  
  
  output$net_dates <- renderUI({
    dates <- theDates()
    days <- dates[["dates"]]
    shinyWidgets::sliderTextInput(inputId = "net_dates", 
                                  label = "Date", 
                                  choices = days)
  })
  output$shock_dates <- renderUI({
    
    dates <- theDates()
    days <- dates[["dates"]]
    shinyWidgets::sliderTextInput(inputId = "shock_dates", 
                                  label = "Date", 
                                  choices = days)
  })
  output$table_dates <- renderUI({
    
    dates <- theDates()
    days <- dates[["dates"]]
    shinyWidgets::sliderTextInput(inputId = "table_dates", 
                                  label = "Date", 
                                  choices = days)
  })
  
  output$matrix <- renderTable({
    v1 <-VARselect(dr[seq(as.Date(input$dateRange[1]),as.Date(input$dateRange[2]),by ="days"),],lag.max=10)
    a <- v1$criteria
    Accuracy <-c('AIC(n)','HQ(n)','SC(n)','FPE(n)')
    matrix<-cbind(Accuracy,a)
  })
  
  
  model_run <- eventReactive(input$getIt,{
    
    dat <- returns.data()
    if(input$choice == "Returns"){
      dat = dat[["returns"]]
    }else{
      dat = dat[["return_vol"]]
    }
    
    vector_autoreg(dat,window=input$window,iter=input$windowDivider,ar_lag=input$ar_lag,ma_lag=input$ma_lag)
  })
  
  connected_grid_graphs <- reactive({
    net.df.list <- model_run()[["net_df"]]
    spill.df.list <- model_run()[["spillover_table"]]
    
    net.cols <- names(net.df.list)
    
    selected.net.df.list <- llply(net.df.list,function(df){
      df[[input$direction]]
    })
    
    selected.net.df <- as.data.frame(do.call(rbind,lapply(selected.net.df.list, function(x) t(data.frame(x)))))
    selected.net.df <- selected.net.df %>% mutate(Date = as.POSIXct(net.cols,format="%Y-%m-%d"))
    
    spill.cols <- names(spill.df.list)
    
    selected.spill.df.list <- llply(spill.df.list,function(df){
      df[[input$direction]]
    })
    
    selected.spill.df <- as.data.frame(do.call(rbind,lapply(selected.spill.df.list, function(x) t(data.frame(x)))))
    selected.spill.df <- selected.spill.df %>% mutate(Date = as.POSIXct(spill.cols,format="%Y-%m-%d"))
    
    net.melt <- melt(selected.net.df,id="Date",value.name = "Connectedness")
    spill.melt <- melt(selected.spill.df,id="Date",value.name = "Spillover")
    merge(net.melt,spill.melt,by=c("Date","variable"))
  })
  
  output$grid_graph <- renderPlot({
    tbl <- connected_grid_graphs()
    ggplot(tbl) + 
      geom_line(aes(x=Date,y=Connectedness),colour="black") +
      geom_line(aes(x=Date,y=Spillover),colour="red") +
      facet_wrap(~variable,scales="free_y") +
      theme(legend.position = "bottom")
    
  })
  
  conn_table <- reactive({
    dat <- model_run()
    tables <- dat[["net_df"]]
    tables[[as.character(input$table_dates)]][["table"]]
  })
  output$connected_table <- renderTable({
    conn_table()
  },rownames = TRUE)
  
  spill_table <- reactive({
    dat <- model_run()
    tables <- dat[["spillover_table"]]
    tables[[as.character(input$table_dates)]][["table"]]
  })
  
  output$spillover_table <- renderTable({
    spill_table()
  },rownames = TRUE)
  
  
  
  output$net_pairwise_plot <- renderPlot({
    
    sender <- input$sender
    receiver <- input$receiver
    
    dat <- model_run()
    net_stuff <- dat[["net_df"]]
    tables <- llply(net_stuff,function(stuff) stuff[["table"]])
    
    pairwise_data <- ldply(tables,function(table){
      sender_index <- which(colnames(table) == sender)
      receiver_index <- which(colnames(table) == receiver)
      
      sender_shock <- table[receiver_index,sender_index]
      receiver_shock <- table[sender_index,receiver_index]
      
      net_shock <- sender_shock - receiver_shock
    })
    colnames(pairwise_data) <- c("Time","Connectedness")
    pairwise_data <- pairwise_data %>% mutate(Time = as.POSIXct(Time,format="%Y-%m-%d"))
    
    ggplot(pairwise_data,aes(x=Time,y=Connectedness,group=1)) + 
      geom_line() +
      geom_hline(yintercept=0,color="red",linetype="dashed")+
      ggtitle(paste(sender,"to", receiver)) + 
      xlab("Time") + 
      ylab("Connectedness")
  })
  
  output$net_pairwise_hover_data <- renderText({
    
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Date=", as.Date(as.POSIXct(e$x,origin="1970-01-01",format="%Y-%m-%d",tz="UTC")), " Index=", round(e$y,2), "\n")
    }
    xy_str(input$net_pairwise_hover)
  })
  
  net.net <- reactive({
    nets <- model_run()
    nets[["nets"]]
  })
  
  output$connectedness <- renderPlot({
    netnets <- net.net()
    nets <- netnets[[as.character(input$net_dates)]]
    plot.igraph(nets,layout=layout.circle(nets),
                main="Firm Connectedness \n (Daily Return volatility)",
                xlab = "black: 1st percentile \n red: 5th percentile \n orange: 10th percentile \n node size: number of edeges connected to the node")
    
  })
  
  indexes <- reactive({
    
    index <- model_run()
    
    ############## CONNECTEDNESS ################
    vol.conn.index.out <- index[["conn"]]
    ### unlist output data
    vol.conn.index.df <- data.frame(unlist(vol.conn.index.out))
    colnames(vol.conn.index.df) = c("Index")
    ### .xts object
    vol.conn.index.df$Date <- as.POSIXct(rownames(vol.conn.index.df),format="%Y-%m-%d")
    
    ############## SPILLOVER ###################
    fevd.list.out <- index[["spill"]]
    
    fevd.df <- data.frame(unlist(fevd.list.out))
    colnames(fevd.df) = c("Index")
    fevd.df$Date <- as.POSIXct(rownames(fevd.df),format="%Y-%m-%d")
    
    ### compare the two index measures
    indice = merge(vol.conn.index.df,fevd.df,by="Date")
    colnames(indice) = c("Date","Connectedness","Spillover")
    indice
  })
  
  output$spilloverIndex <- renderPlot({
    dat <- indexes()
    dat <- xts(dat[,-1],order.by=dat[,1])
    
    ggplot(dat) + 
      geom_line(aes(x=Index,y=Connectedness,colour="Connectedness"),size=1.2) +
      geom_line(aes(x=Index,y=Spillover,colour="Spillover"),size=1.2) + theme_bw() +
      theme(legend.position = "bottom",legend.title = element_blank(),text = element_text(size=20)) + 
      xlab("Time") + ylab("Connectedness/Spillover Index") +
      scale_color_manual(values = c("black","red"))
  })
  
  output$index_hover_data <- renderText({
    
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Date=", as.Date(as.POSIXct(e$x,origin="1970-01-01",format="%Y-%m-%d",tz="UTC")), " Index=", round(e$y,2), "\n")
    }
    xy_str(input$index_hover)
  })
  
  output$downloadConnectedness <- downloadHandler(
    filename = function() {
      paste("Connectedness-",as.character(input$table_dates), ".csv", sep = "")
    },
    content = function(file){
      write.csv(conn_table(), file,row.names = TRUE)
    }
  )
  
  output$downloadSpillover <- downloadHandler(
    filename = function() {
      paste("Spillover-",as.character(input$table_dates), ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(spill_table(), file,row.names = TRUE)
    }
  )
  
  output$downloadReturns <- downloadHandler(
    filename = function() {
      "Returns.csv"
    },
    content = function(file){
      write.csv(returns_data(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

