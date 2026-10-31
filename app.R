# Laboratorio interactivo ECO1 - Salud y desarrollo (2019)
# Requiere solo shiny. Los scripts originales permanecen intactos.
library(shiny)
options(shiny.maxRequestSize = 20 * 1024^2)
base_file <- "salud_desarrollo_paises_2019.csv"
campos <- c("esperanza_vida","pib_pc_ppa","gasto_salud_pib","urbana_pct","agua_basica_pct","america_latina")
etiquetas <- c(esperanza_vida="Esperanza de vida (años)",pib_pc_ppa="PIB per cápita PPA",ln_pib="Logaritmo del PIB per cápita",gasto_salud_pib="Gasto en salud (% PIB)",urbana_pct="Población urbana (%)",agua_basica_pct="Acceso a agua básica (%)",america_latina="América Latina (=1)")
ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f6f8fc}.well{background:white;border:1px solid #dbe2ef;border-radius:12px}.tab-content{background:white;padding:22px;border:1px solid #dbe2ef;border-top:none;border-radius:0 0 12px 12px}.shiny-output-error-validation{color:#a53b32}"))),
  titlePanel("Laboratorio Econométrico | Salud y desarrollo"),
  p("Econometría I · Datos internacionales 2019 · Ejercicio académico interactivo"),
  sidebarLayout(
    sidebarPanel(width=3,
      fileInput("archivo","Base CSV (opcional)",accept=".csv"),
      helpText("Si no cargas un archivo se utiliza la base del repositorio."),
      selectInput("dependiente","Variable dependiente",choices=NULL),
      selectInput("explicativas","Variables explicativas (X)",choices=NULL,multiple=TRUE),
      checkboxInput("intercepto","Incluir constante",TRUE),
      actionButton("estimar","Estimar MCO",class="btn-primary"),
      hr(),sliderInput("nivel","Nivel de confianza",min=90,max=99,value=95,step=1),
      helpText("Cambia las variables y presiona Estimar MCO para actualizar resultados.")
    ),
    mainPanel(width=9,
      tabsetPanel(
        tabPanel("Explorar",br(),h4("Datos disponibles"),tableOutput("resumen"),h4("Primeras observaciones"),tableOutput("muestra"),plotOutput("dispersion",height="330px")),
        tabPanel("Modelo MCO",br(),verbatimTextOutput("modelo"),h4("Intervalos de confianza"),tableOutput("intervalos"),h4("Interpretación"),textOutput("interpretacion")),
        tabPanel("Supuestos",br(),h4("Diagnósticos"),tableOutput("diagnosticos"),h4("Valores ajustados y residuos"),plotOutput("residuos",height="320px"),h4("Multicolinealidad (VIF)"),tableOutput("vif")),
        tabPanel("El Salvador",br(),h4("Predicción para El Salvador"),tableOutput("slv"),helpText("La predicción se presenta solo si la base contiene iso3=SLV y hay valores completos en las variables elegidas.")),
        tabPanel("Guía",br(),h3("Actividad de Econometría I"),
          p("1. Explora la relación entre esperanza de vida e ingreso."),
          p("2. Estima esperanza_vida ~ ln_pib como regresión simple."),
          p("3. Agrega gasto en salud, urbanización, acceso al agua y la variable de América Latina."),
          p("4. Interpreta signos, magnitudes, intervalos, R cuadrado y pruebas de supuestos."),
          p("5. Examina la predicción para El Salvador y discute causalidad frente a correlación."),
          strong("Advertencia:")," Los resultados son asociaciones transversales, no estimaciones causales. Los errores robustos no corrigen endogeneidad.")
      )
    )
  )
)
server <- function(input,output,session){
  datos <- reactive({
    d <- if(is.null(input$archivo)) read.csv(base_file,check.names=FALSE,encoding="UTF-8") else read.csv(input$archivo$datapath,check.names=FALSE,encoding="UTF-8")
    validate(need(nrow(d)>0,"La base está vacía."),need("pib_pc_ppa"%in%names(d),"Falta pib_pc_ppa para crear ln_pib."))
    d$ln_pib <- ifelse(is.finite(d$pib_pc_ppa) & d$pib_pc_ppa>0,log(d$pib_pc_ppa),NA_real_)
    d
  })
  observeEvent(datos(),{
    d<-datos(); vs<-names(d)[vapply(d,is.numeric,logical(1))]
    y<-if("esperanza_vida"%in%vs) "esperanza_vida" else vs[1]
    xs<-intersect(c("ln_pib","gasto_salud_pib","urbana_pct","agua_basica_pct","america_latina"),setdiff(vs,y))
    updateSelectInput(session,"dependiente",choices=vs,selected=y)
    updateSelectInput(session,"explicativas",choices=setdiff(vs,y),selected=xs)
  })
  output$resumen<-renderTable({
    d<-datos();data.frame(Indicador=c("Filas","Variables","Países con esperanza de vida","Casos completos (variables centrales)"),
    Valor=c(nrow(d),ncol(d),if("esperanza_vida"%in%names(d))sum(!is.na(d$esperanza_vida)) else NA,
    if(all(campos%in%names(d)))sum(complete.cases(d[,campos])) else NA))
  },striped=TRUE)
  output$muestra<-renderTable(head(datos(),8),striped=TRUE)
  output$dispersion<-renderPlot({
    d<-datos();req(input$dependiente);validate(need(input$dependiente%in%names(d),"Elige una variable dependiente."))
    x<-if("ln_pib"%in%names(d)) "ln_pib" else names(d)[vapply(d,is.numeric,logical(1))][1]
    plot(d[[x]],d[[input$dependiente]],pch=19,col=rgb(.12,.35,.65,.42),xlab=x,ylab=input$dependiente,main="Exploración bivariada")
    if(x!=input$dependiente && sum(complete.cases(d[,c(x,input$dependiente)]))>3)abline(lm(reformulate(x,input$dependiente),data=d),col="darkred",lwd=2)
  })
  modelo<-eventReactive(input$estimar,{
    req(input$dependiente,input$explicativas)
    xs<-setdiff(input$explicativas,input$dependiente)
    validate(need(length(xs)>0,"Selecciona una o más variables X."),
             need(all(c(input$dependiente,xs)%in%names(datos())),"Variables no disponibles."))
    d<-datos()[,unique(c("iso3","pais",input$dependiente,xs))[unique(c("iso3","pais",input$dependiente,xs))%in%names(datos())],drop=FALSE]
    dd<-d[complete.cases(d[,c(input$dependiente,xs),drop=FALSE]),,drop=FALSE]
    validate(need(nrow(dd)>length(xs)+3,"Observaciones insuficientes para estimar."))
    m<-lm(reformulate(xs,response=input$dependiente,intercept=isTRUE(input$intercepto)),data=dd)
    validate(need(m$rank==length(coef(m)),"Regresores perfectamente colineales: cambia las variables."),
             need(df.residual(m)>2,"Grados de libertad insuficientes."))
    list(m=m,dd=dd,xs=xs)
  },ignoreInit=TRUE)
  output$modelo<-renderPrint({summary(modelo()$m)})
  output$intervalos<-renderTable({m<-modelo()$m;round(confint(m,level=input$nivel/100),4)},rownames=TRUE)
  output$interpretacion<-renderText({
    m<-modelo()$m
    paste0("R² = ",round(summary(m)$r.squared,3),"; n = ",nobs(m),". Revisa coeficientes y significancia, sin asumir causalidad.")
  })
  output$residuos<-renderPlot({
    m<-modelo()$m;plot(fitted(m),resid(m),pch=19,col=rgb(.15,.35,.65,.5),xlab="Ajustados",ylab="Residuos");abline(h=0,col="darkred",lty=2)
  })
  output$diagnosticos<-renderTable({
    m<-modelo()$m;e<-resid(m);n<-length(e);z<-fitted(m)
    # Breusch-Pagan auxiliar: e^2 ~ variables explicativas
    mm<-model.matrix(m);aux<-lm.fit(mm,e^2);rss<-sum(aux$residuals^2);tss<-sum((e^2-mean(e^2))^2)
    bp<-if(tss>0)n*(1-rss/tss) else NA_real_
    dfbp<-ncol(mm)-as.integer("(Intercept)"%in%colnames(mm))
    # RESET: regresores adicionales yhat^2,yhat^3
    reset_p<-tryCatch({
      if(qr(cbind(mm,z^2,z^3))$rank<ncol(mm)+2) return(NA_real_)
      fit2<-lm.fit(cbind(mm,z^2,z^3),m$model[[1]])
      f<-((sum(e^2)-sum(fit2$residuals^2))/2)/(sum(fit2$residuals^2)/(n-ncol(mm)-2))
      pf(f,2,n-ncol(mm)-2,lower.tail=FALSE)
    },error=function(err)NA_real_)
    data.frame(Prueba=c("Breusch-Pagan (LM auxiliar)","RESET (ajustados² y ³)"),
    Hipotesis_nula=c("Varianza constante","Forma funcional adecuada"),
    Estadistico=c(round(bp,3),NA_real_),p_valor=round(c(if(is.finite(bp)&&dfbp>0)pchisq(bp,dfbp,lower.tail=FALSE) else NA_real_,reset_p),4),
    row.names=NULL)
  },striped=TRUE)
  output$vif<-renderTable({
    v<-modelo()$xs; dd<-modelo()$dd
    if(length(v)<2)return(data.frame(Variable=v,VIF=NA_real_))
    data.frame(Variable=v,VIF=round(vapply(v,function(x){
      others<-setdiff(v,x); r<-summary(lm(reformulate(others,x),data=dd))$r.squared; if(r>=1)Inf else 1/(1-r)
    },numeric(1)),3))
  })
  output$slv<-renderTable({
    a<-modelo();d<-datos();validate(need("iso3"%in%names(d),"La base no contiene iso3."))
    slv<-d[d$iso3=="SLV",,drop=FALSE]
    validate(need(nrow(slv)>0,"No existe fila SLV."),need(all(complete.cases(slv[,c(input$dependiente,a$xs),drop=FALSE])),"El Salvador tiene valores faltantes."))
    p<-predict(a$m,newdata=slv,interval="prediction",level=input$nivel/100)
    data.frame(Pais="El Salvador",Observado=round(slv[[input$dependiente]][1],3),Predicho=round(p[1,"fit"],3),
    Limite_inferior=round(p[1,"lwr"],3),Limite_superior=round(p[1,"upr"],3),
    Residuo=round(slv[[input$dependiente]][1]-p[1,"fit"],3))
  })
}
shinyApp(ui,server)
