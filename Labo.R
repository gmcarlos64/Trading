rm(list = ls())
options("scipen"=100, "digits"=4)
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teor?a Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html")
Quandl.api_key("NBGJ4fcz1LnJVqyb7zZ4")
Capital_Inicial <- 10000
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  ##############################################################################
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}
#################
archivos <- c()
pesos <- list()
for(i in 1:10){
  archivos[i]<- paste("IVV_holdings",i," IVV_holdings (2).csv", sep=" ")
}
datos_ <- read.csv(file.path = "C:\Users\if685009\Desktop\lab1","IVV_holdings(2)"
primero <- wich(x = " IVV_holdings"["A11",1]=="Ticker")
ultimo <- lenght(datos_[,1])

tk<- datos_[(primero+1):ultimo,1]
pesos <- datos_[primero:ultimo,4]

histórico_[['']] <-c("tickers"=tk,"pesos"=pesos)


############################
for(i in 1:length(tickers))
  tk <- c(Bajar_Precios(Tickers[i]))
cs <- c("date", "adj_close")
fs <- c("2018-02-02", "2019-01-02")
Datos <- list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))

for(i in 1:lenght(Datos[i]$adj_close_r)
    Rends <- xts(x = cbind(Datos[[i:lenght(Datos[i]$adj_close_r)]]),
                 order.by = Datos[[i]]$date)[-1]
    names(Rends) <- tk
    
    Port1 <- portfolio.spec(assets=tk)
    Port1 <- add.constraint(portfolio=Port1,
                            type="full_investment")
    #######################################
    Port1 <- add.constraint(portfolio=Port1,
                            type="box",
                            min=c(0.1, 0.1, 0.1), max=c(0.7, 0.7, 0.7))
    
    Port1 <- add.objective(portfolio = Port1, type = "return", name = "mean")
    
    Port1 <- optimize.portfolio(R=Rends, portfolio=Port1, optimize_method="random",
                                trace=TRUE, search_size=10000)
    
    Portafolios <- vector("list",
                          length = length(Port1$random_portfolio_objective_results))
    
    for(i in 1:length(Port1$random_portfolio_objective_results)) {
      
      Portafolios[[i]]$Pesos  <- Port1$random_portfolio_objective_results[[i]]$weights
      Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
      
      Portafolios[[i]]$Vars   <- var.portfolio(R = Port1$R, weights = Portafolios[[i]]$Pesos)
      names(Portafolios[[i]]$Medias) <- NULL
    }
    
    df_Portafolios <- data.frame(matrix(nrow=length(Port1$random_portfolio_objective_results),
                                        ncol=3,
                                        data = 0))
    colnames(df_Portafolios) <- c("Rend","Var","Clase")
    
    for(i in 1:length(Port1$random_portfolio_objective_results)) {
      
      df_Portafolios$Rend[i]  <- round(Portafolios[[i]]$Medias*252,4)
      df_Portafolios$Var[i]   <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)
      df_Portafolios$Clase[i] <- "No-Frontera"
      
      for(k in 1:length(tk)) {
        
        df_Portafolios[i,paste("Peso_", tk[k], sep="")] <- Portafolios[[i]]$Pesos[k]
        
        df_Portafolios[i,paste("Titulos_ini_", tk[k],sep="")] <-
          (Capital_Inicial*Portafolios[[i]]$Pesos[k])%/%Datos[[k]]$adj_close[1]
      }
    }
    
    Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                                name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text',
                                text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                              '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>%
      layout(title = "Portafolios (Markowitz)",
             xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                          showgrid = F),
             yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
             legend = list(orientation = 'h', y = -0.25))
    Plot_portafolios
    
    Port_1 <- df_Portafolios[which.max(df_Portafolios$Rend),]