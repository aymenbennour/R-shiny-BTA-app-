
library(shiny)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(readxl)
library(data.table)

base20220630=read_xlsx("C:/Users/MSI/Downloads/base20220630.xlsx",1)
base=read_xlsx("C:/Users/MSI/Downloads/base20220630.xlsx",2)

base20211231=read_xlsx("C:/Users/MSI/Downloads/base20211231.xlsx")
parametres=read_xlsx("C:/Users/MSI/Downloads/parametres.xlsx")

parametres0630=read_xlsx("C:/Users/MSI/Downloads/parametres0630.xlsx")

# Define UI for application
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = 'BTA'),
  
  #layout
  dashboardSidebar(
      sidebarMenu(    
      menuItem("date", selectInput('date','select a date',c('2021-12-31','2022-06-30')),icon=icon("calendar") ),
      
      menuItem( conditionalPanel(condition="input.tabselected==2",   uiOutput('x')))
      ,
      menuItem(conditionalPanel(condition="input.tabselected==3",
                       radioGroupButtons(
                         inputId = "s",
                         label = "select ",
                         choices = c("juste_valeur_portfeuille",
                                     "nombres_de_bons"),
                       ),
      )
      ),
      
      menuItem(conditionalPanel(condition="input.tabselected==5",
           # selectInput('scenarios','choose a scenario',c('ph','pb','inv','pent','app'))))
            pickerInput(
              inputId = "scenario",
              label = "scenario",
              choices = c('ph','pb','inv','pent','app'),
              options = list(
                title = "choose a scenario")
            ))),
      menuItem(conditionalPanel(condition="input.tabselected==6",
                                 selectInput('scenario1','choose a scenario',c('ph','pb','inv','pent','app'))))
                                
                                
            
            
      )
  ),


    
    dashboardBody(
      tabsetPanel(
        tabPanel('base',value=1,  fluidRow(
          tableOutput("tab1"),
          #box('ouput2',plotOutput("plot2"))
          
          )),
          
        tabPanel('maturite_resi',value=2,textOutput('maturite_resi')),
        tabPanel('graphs',value=3,
                fluidRow(valueBoxOutput("justv",width = 4),
                         valueBoxOutput("nbrb",width = 4)  ),
                fluidRow(plotOutput('graphs')),
        ),
        tabPanel('tabs',value=4,DT::DTOutput('tabs')),
        tabPanel('courbe de taux',value=5,
                 fluidRow(
                 plotlyOutput('coubre_de_taux'),
                 plotlyOutput('sc')
                 )),
        tabPanel('diff',value=6,
                 fluidRow(
                 valueBoxOutput('box1',width = 4),
                 valueBoxOutput('box2',width=4),
                 valueBoxOutput('box3',width = 4),
                 tableOutput('diff')
        )
        ),
        id='tabselected' 
        
       
        
      )
      
    )
  
  
)





server <- function(input, output) {
  ss<-reactive({
    input$scenario1
  })
  
  base20211231$matdays= (interval('2021-12-31',base20211231$ech) %/% days(1))/365
  base20220630$matdays= (interval('2022-06-30',base20220630$ech) %/% days(1))/365
   
  s<-reactive({
    input$scenario
  })
  
  m<-reactive({
    input$date 
  })
  
  y<-reactive({
    input$s
  })
  
  
  beta_0<-as.numeric(parametres0630$`Beta 0`) 
  beta_1<-as.numeric(parametres0630$`Beta 1`)
  beta_2<-as.numeric(parametres0630$`Beta 2`)
  lambda<-(parametres0630$Lambda)
    lambda=as.numeric(lambda)
     taux<-function(i,x,q,w,z){
       
      y=x+q*((1-exp(-i*z))/(i*z))+w*((1-exp(-i*z))/(i*z)-exp(-i*z))
       return(y)
     }

     output$coubre_de_taux<-renderPlotly({
         #x=c('1','2','3','4','5','6','7','8')
        # y=as.numeric(x)
        
         #for (i in 1:20)
             
        # plot(x,taux(y),xlim=c(0,20),ylim=c(0,0.1),type='l',col='red')
        options(digits = 10)
       df3<-data.frame(matrix(ncol = 3,nrow = 20))
       colnames(df3)<-c('mat','taux','t')
       df3$taux[1]=beta_0+beta_1
       df3$mat[1]=0
        for (i in 2:20 ){
            df3$mat[i]=i
            df3$taux[i]=taux(i,beta_0,beta_1,beta_2,lambda)
        }
       df3$t=exp(df3$taux)-1
        ggplot(df3,aes(x=df3$mat,y=df3$t))+
          geom_line(color='blue')+
          coord_cartesian(xlim =c(1,20),ylim=c(0.06,0.1))+
          labs(x='maturite',y='taux')+
          ggtitle('courbe de taux')
     }
     )
     output$sc<-renderPlotly({
       df3<-data.frame(matrix(ncol = 3,nrow = 20))
       colnames(df3)<-c('mat','taux','t')
       df3$taux[1]=beta_0+beta_1
       df3$mat[1]=0
       for (i in 2:20 ){
         df3$mat[i]=i
         df3$taux[i]=taux(i,beta_0,beta_1,beta_2,lambda)
       }
       df3$t=exp(df3$taux)-1
       
                
         if (s()=='ph'){
                df4<-data.frame(matrix(ncol = 3,nrow = 20))
                colnames(df4)<-c('mat','taux','t')
                df4$taux[1]=beta_0+0.01+beta_1
                df4$mat[1]=0
                for (i in 2:20 ){
                  df4$mat[i]=i
                  df4$taux[i]=taux(i,beta_0+0.01,beta_1,beta_2,lambda)
                }
                df4$t=exp(df4$taux)-1

           df<-merge(df3,df4,by='mat')
           ggplot(df,aes(x=df$mat,y=df$t))+
             geom_line(y=df4$t,color='red')+
             geom_line(y=df3$t,color='blue')+
             coord_cartesian(xlim =c(1,20),ylim=c(0.06,0.13))+
             labs(x='maturite',y='taux')
             
         }
       else if (s()=='pb'){
          df4<-data.frame(matrix(ncol = 3,nrow = 20))
          colnames(df4)<-c('mat','taux','t')
          df4$taux[1]=beta_0-0.01+beta_1
          df4$mat[1]=0
          for (i in 2:20 ){
            df4$mat[i]=i
            df4$taux[i]=taux(i,beta_0-0.01,beta_1,beta_2,lambda)
          }
          df4$t=exp(df4$taux)-1
          
          df<-merge(df3,df4,by='mat')
          ggplot(df,aes(x=df$mat,y=df$t))+
            geom_line(y=df4$t,color='red')+
            geom_line(y=df3$t,color='blue')+
            coord_cartesian(xlim =c(1,20),ylim=c(0.06,0.13))+
            labs(x='maturite',y='taux')
        }
      else  if (s()=='inv'){
          df4<-data.frame(matrix(ncol = 3,nrow = 20))
          colnames(df4)<-c('mat','taux','t')
          df4$taux[1]=beta_0+beta_1*(-1)
          df4$mat[1]=0
          for (i in 2:20 ){
            df4$mat[i]=i
            df4$taux[i]=taux(i,beta_0,beta_1*(-1),beta_2/2*(-1),lambda)
          }
          df4$t=exp(df4$taux)-1
          
          df<-merge(df3,df4,by='mat')
          ggplot(df,aes(x=df$mat,y=df$t))+
            geom_line(y=df4$t,color='red')+
            geom_line(y=df3$t,color='blue')+
            coord_cartesian(xlim =c(1,20),ylim=c(0.06,0.13))+
            labs(x='maturite',y='taux')
      }
        
        else  if (s()=='pent'){
          df4<-data.frame(matrix(ncol = 3,nrow = 20))
          colnames(df4)<-c('mat','taux','t')
          df4$taux[1]=beta_0+beta_1
          df4$mat[1]=0
          for (i in 2:20 ){
            df4$mat[i]=i
            df4$taux[i]=taux(i,beta_0+0.02,beta_1,beta_2+0.02,lambda)
          }
          df4$t=exp(df4$taux)-1
          
          df<-merge(df3,df4,by='mat')
          ggplot(df,aes(x=df$mat,y=df$t))+
            geom_line(y=df4$t,color='red')+
            geom_line(y=df3$t,color='blue')+
            coord_cartesian(xlim =c(1,20),ylim=c(0.06,0.13))+
            labs(x='maturite',y='taux')
        }
        
        else  if (s()=='app'){
          df4<-data.frame(matrix(ncol = 3,nrow = 20))
          colnames(df4)<-c('mat','taux','t')
          df4$taux[1]=beta_0+beta_1+0.02
          df4$mat[1]=0
          for (i in 2:20 ){
            df4$mat[i]=i
            df4$taux[i]=taux(i,beta_0,beta_1+0.02,beta_2+0.02,lambda)
          }
          df4$t=exp(df4$taux)-1
          
          df<-merge(df3,df4,by='mat')
          ggplot(df,aes(x=df$mat,y=df$t))+
            geom_line(y=df4$t,color='red')+
            geom_line(y=df3$t,color='blue')+
            coord_cartesian(xlim =c(1,20),ylim=c(0.06,0.13))+
            labs(x='maturite',y='taux')
        }
      
      
   })
  
 
  
  options(digits = 2)
  
  base20211231$Mat_res= interval('2021-12-31',base20211231$ech) %/% months(1)
  base20220630$Mat_res= interval('2022-06-30',base20220630$ech) %/% months(1) 
  
  base20211231$tags=base20211231$maturite
  base20220630$tags=base20220630$maturite
  for (i in 1:14){
    if ((base20211231$Mat_res[i]>0)&(base20211231$Mat_res[i]<=12)){ base20211231$tags[i]='[0,1] annees' }
    else if ((base20211231$Mat_res[i]>12)&(base20211231$Mat_res[i]<=36)){ base20211231$tags[i]='[1,3] annees'}
    else if ((base20211231$Mat_res[i]>37)&(base20211231$Mat_res[i]<=60)){ base20211231$tags[i]='[3,5] annees'}
    else if ((base20211231$Mat_res[i]>60)&(base20211231$Mat_res[i]<=110)){ base20211231$tags[i]='>5 annees'}
  }  
  for (i in 1:14){
    if ((base20220630$Mat_res[i]>0)&(base20220630$Mat_res[i]<=12)){ base20220630$tags[i]='[0,1] annees' }
    else if ((base20220630$Mat_res[i]>12)&(base20220630$Mat_res[i]<=36)){ base20220630$tags[i]='[1,3] annees'}
    else if ((base20220630$Mat_res[i]>37)&(base20220630$Mat_res[i]<=60)){ base20220630$tags[i]='[3,5] annees'}
    else if ((base20220630$Mat_res[i]>60)&(base20220630$Mat_res[i]<=110)){ base20220630$tags[i]='>5 annees'}
  } 
  
  function1<-function(){
    df1=cbind.data.frame(base20211231$libelle,base20211231$Mat_res,base20211231$nbrbons,base20211231$tri,base20211231$juste_valeur_portfeuille)
    colnames(df1)<-c('libelle','maturite_residuelle en mois','nombres de bons','tri','juste valeur portfeuille')
    return(df1)
  }
  function2<-function(){
    df2=cbind.data.frame(base20220630$libelle,base20220630$Mat_res,base20220630$nbrbons,base20220630$tri,base20220630$juste_valeur_portfeuille) 
    colnames(df2)<-c('libelle','maturite_residuelle en mois','nombres de bons','tri','juste valeur portfeuille')  
    return(df2)
  }
  
  
  output$tabs<-DT::renderDT({
    if (m()=='2021-12-31'){
      function1()
    }
    else if(m()=='2022-06-30'){
      function2()
    }
  })
  
  
  
  ##affichage base
  output$tab1<-renderTable({
    if (m()=='2021-12-31'){
     base20211231
    }
    else if (m()=='2022-06-30'){
      base20220630
    }
  })
  
  
  
  
  
   
  ##graphs
  
  
  data1<-cbind.data.frame(base20211231$tags,base20211231$juste_valeur_portfeuille,base20211231$Mat_res,base20211231$nbrbons)
  data2<-cbind.data.frame(base20220630$tags,base20220630$juste_valeur_portfeuille,base20220630$Mat_res,base20220630$nbrbons)
  
  
  output$graphs<-renderPlot({
    options(scipen = 100)
    date<-m()
    
    
    
    if (date=='2021-12-31'){
      if(y()=='juste_valeur_portfeuille'){
        
        ggplot(data1,aes(x=base20211231$tags,base20211231$juste_valeur_portfeuille))+
          geom_col(alpha=0.7,width=0.7)+
          labs(x='maturite residuelle ',y='juste valeur portfeuille')+
          coord_cartesian(ylim = c(5000, 400000))+
          ggtitle("Histogram of",y())
      }
      else if(y()=='nombres_de_bons'){
        
        ggplot(data1,aes(x=base20211231$tags,y=base20211231$nbrbons))+
          geom_col(alpha=0.7,width=0.7)+
          labs(x='maturite residulle ',y='nombres de bons')+
          coord_cartesian(ylim = c(5000, 400000))+
          ggtitle("Histogram of",y())
      }
    }
    
    else if (date=='2022-06-30'){
      if(y()=='juste_valeur_portfeuille'){
        
        ggplot(data2,aes(x=base20220630$tags,y=base20220630$juste_valeur_portfeuille))+
          geom_col(alpha=0.7,width=0.7)+
          labs(x='maturite residuelle',y='juste valeur portfeuille')+
          coord_cartesian(ylim = c(5000, 400000))+
          ggtitle("Histogram of",y())
      }
      else if(y()=='nombres_de_bons'){
        
        ggplot(data2,aes(x=base20220630$tags,y=base20220630$nbrbons))+
          geom_col(alpha=0.7,width=0.7)+
          labs(x='maturite residuelle',y='nombres de bons')+
          coord_cartesian(ylim = c(5000, 400000))+
          ggtitle("Histogram of",y())
      }
    }
    
  })
  
  
  ##render ui   
  output$x<-renderUI({
    if (m()=='2021-12-31'){
      selectInput('id','select id',base20211231$id)
    }
    else if (m()=='2022-06-30'){
      selectInput('id','select id',base20220630$id)
    }
  })
  
  ##tableau  
  # tab<-function(){
  #   df<-data.frame(matrix(ncol = 0,nrow = 2))
  #   rownames(df)<-c('a','b')
  #   
  #   df$totale_juste_valeur_portfeuille=sum(base20211231$juste_valeur_portfeuille)
  #   df$totale_nombres_de_bons=sum(base20220630$nbrbons)
  #   return(df)
  # }
  
  
  
  
  ##portfeuille
  
  
  
  output$nbrb <- renderValueBox({
    
    if (input$date=='2021-12-31'){
      valueBox(
        
        paste0(format(round(sum(base20211231$nbrbons)))), "nombres de bons", icon = icon("thumbs-up", lib = "glyphicon"),
                                                                
        
        color = "blue"
        
      )}
    else if(input$date=='2022-06-30'){
      valueBox(
        
        paste0(format(round(sum(base20220630$nbrbons)))), "nombres de bons", icon = icon("thumbs-up", lib = "glyphicon"),
        
        color = "blue"
        
      )}
    
  })
  
  output$justv <- renderValueBox({
    
    if (input$date=='2021-12-31'){
    valueBox(
      
      paste0(format(round(sum(base20211231$juste_valeur_portfeuille))),'  MDT'), "juste_valeur", icon = icon("thumbs-up", lib = "glyphicon"),
      
      color = "blue"
      
    )}
    else if(input$date=='2022-06-30'){
      valueBox(
        
        paste0(format(round(sum(base20220630$juste_valeur_portfeuille))),'  MDT'), "juste_valeur", icon = icon("thumbs-up", lib = "glyphicon"),
        
        color = "blue"
        
      )}
    
  })  
  
  
  
  output$juste_valeur_portfeuille<-renderText({
    
    if (input$date=='2021-12-31')
      paste('la juste valeur portfeuille de la date ',input$date,'est : ',sum(base20211231$juste_valeur_portfeuille)
            
      )
    #paste(tab())
    
    else if(input$date=='2022-06-30')
      paste('la juste valeur portfeuille de la date ',input$date,'est : ',sum(base20220630$juste_valeur_portfeuille)
      )
  })
  
  
  ##maturite res   
  output$maturite_resi<-renderText({
    id1<-input$id
    date1<-ymd(input$date)
    
    if (input$date=='2021-12-31'){
      i=match(id1,base20211231$id)
      a=interval(date1,base20211231$ech[i])%/% months(1)
    }
    else if (input$date=='2022-06-30'){
      i=match(id1,base20220630$id)
      a=interval(date1,base20220630$ech[i])%/% months(1)
    }
    
    paste('la maturite residuelle de l id ',id1,' par rapport a la date ',input$date,'est :',a,' mois')
     
  })
  
  
  
# ## difference portfeuilles   
#   base20211231$valeur=base20211231$juste_valeur_portfeuille
#   base20220630$valeur=base20220630$juste_valeur_portfeuille
#   for (i in 1:14){
#     j=base20211231$Mat_res[i]/12
#     base20211231$valeur[i]= 1/1+exp(taux(j))**j -1
#   }
#   df4<-cbind.data.frame(base20211231$juste_valeur_portfeuille,base20211231$valeur)
#   output$diff<-renderTable({
#     df4
#   })
  
  # x=base20211231$libelle[2] 
  # substr(x,6,10)

  #difference portfeuilles
    
    base20220630$prix=0
   
 calcul<-function(){
   options(scipen = 10,digits = 3)
    # paste(df[df$id=="TN0008000648",])
     for (i in unique(factor(base$id))){
      data=(base[base$id==i,])
      dd=data$dd[1]
      prix=0
      x=base$x[1]
      print(i)
      print(dd)
          for (j in 1:nrow(data)){
               df=data$d[j]
               dis=interval(dd,df) %/% days(1)

               c=1000*x*dis/36500
               matres=interval('2022-06-30',df) %/% days(1)/365

                    if(j==nrow(data)){
                      f=(c+1000)/((1+taux(matres,beta_0,beta_1,beta_2,lambda))^matres)
                      }
                      else
                        f=c/((1+taux(matres,beta_0,beta_1,beta_2,lambda))^matres)


                prix=prix+f
                print(df)
                dd=df
                print(j)

          }
      base20220630[base20220630$id==i,]$prix=prix/10


 }
   return(base20220630$prix)
 }
  
 # base20220630$valeur=base20220630$nbrbons*base20220630$prix 
 base20220630$valeur=base20220630$nbrbons*calcul()
 dataframe=cbind.data.frame(base20220630$id,base20220630$juste_valeur_portfeuille,base20220630$nbrbons,calcul(),base20220630$valeur)
 colnames(dataframe)<-c('id','juste_valeur','nbr_bons','prix','valeur')
   
 xx=sum(base20220630$valeur)/100
   output$box1<-renderValueBox({
     valueBox(
       
       paste0(format(round(xx)/1000),"MDT"), "valeur_portfeuille scenario normal", icon = icon("thumbs-up", lib = "glyphicon"),
       
       
       color = "blue" 
       
     )} )
     
   base20220630$prix_sc=0
   
   
   
   output$box2<-renderValueBox({
     
     ss=ss()
     
     calculsc<-function(){
       options(scipen = 10,digits = 3)
       # paste(df[df$id=="TN0008000648",])
       for (i in unique(factor(base$id))){
         data=(base[base$id==i,])
         dd=data$dd[1]
         prix=0
         x=base$x[1]
         print(i)
         print(dd)
         for (j in 1:nrow(data)){
           df=data$d[j]
           dis=interval(dd,df) %/% days(1)
           
           c=1000*x*dis/36500
           matres=interval('2022-06-30',df) %/% days(1)/365
           
           if(j==nrow(data)){
             if(ss()=='ph'){
               f=(c+1000)/((1+taux(matres,beta_0+0.01,beta_1,beta_2,lambda))^matres)
             }
             else if (ss()=='pb'){
               f=(c+1000)/((1+taux(matres,beta_0-0.01,beta_1,beta_2,lambda))^matres)
             }
             else if (ss()=='inv'){
               f=(c+1000)/((1+taux(matres,beta_0,beta_1*(-1),beta_2/2*(-1),lambda))^matres)
             } 
             else if(ss()=='app'){
               f=(c+1000)/((1+taux(matres,beta_0,beta_1+0.02,beta_2+0.02,lambda))^matres)
             }
             else if(ss()=='pent'){
               f=(c+1000)/((1+taux(matres,beta_0+0.02,beta_1,beta_2+0.02,lambda))^matres)
             }
             
           }
           else{
             if(ss()=='ph'){
               f=c/((1+taux(matres,beta_0+0.01,beta_1,beta_2,lambda))^matres)
             }
             else if (ss()=='pb'){
               f=c/((1+taux(matres,beta_0-0.01,beta_1,beta_2,lambda))^matres)
             }
             else if (ss()=='inv'){
               f=c/((1+taux(matres,beta_0,beta_1*(-1),beta_2/2*(-1),lambda))^matres)
             } 
             else if(ss()=='app'){
               f=c/((1+taux(matres,beta_0,beta_1+0.02,beta_2+0.02,lambda))^matres)
             }
             else if(ss()=='pent'){
               f=c/((1+taux(matres,beta_0+0.02,beta_1,beta_2+0.02,lambda))^matres)
             }
           }
           prix=prix+f
           print(df)
           dd=df
           print(j)
           
         }
         base20220630[base20220630$id==i,]$prix_sc=prix/10
         
         
       }
       return(base20220630$prix_sc)
     }
     
     
     base20220630$valeursc=base20220630$nbrbons*calculsc()
     dataframe=cbind.data.frame(dataframe,calculsc(),base20220630$valeur)
     colnames(dataframe)[6:7]=c('prix_sc','valeur_sc')
     yy=sum(base20220630$valeursc)/100
     
      output$diff<-renderTable({
       dataframe
        
     })
     
     valueBox(
       
       paste0(format(round(yy))), "valeur_portfeuille scenario ", s() , icon = icon("thumbs-up", lib = "glyphicon"),
       
       
       color = "blue" 
       
     )
     
     valueBox(


       paste0(format(round(yy-xx)/1000),"MDT"), paste0("+/- value scenario : ",ss()), icon = icon("thumbs-up", lib = "glyphicon"),


       color = "green"

     )
     
     
     } )
    
    
   
   # print(x)
   # print(y)
   
   # output$box3<-renderValueBox({
   #  } )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
