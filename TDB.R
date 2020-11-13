#Chargement des packages
library(shinydashboard)
library(zoo)
library(quantmod)
library(openxlsx)
library(dplyr)
library(highcharter)
library(sqldf)
library(car)
library(multcomp)


#Chargement des donnees
data <- read.xlsx(file.choose(new = FALSE), 1)

#Tri des donnees
data <- data[order(colnames(data))]


#Liste des noms colonnes avec donnees numeriques
numericCol <- colnames(data[,unlist(lapply(data, is.numeric))])

#Liste des noms de colonnes
alphaNumericCol <- colnames(data)



ui <- dashboardPage(
  #L en tete de la page
  dashboardHeader(title = "Tableau de bord"),
  #La barre de selection de la page
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse a un facteur", tabName = "1facteur", icon = icon("th")),
      menuItem("Anova a deux facteurs", tabName = "2facteurs", icon = icon("th"))
    )
  ),
  #Le corp de la page
  dashboardBody(
    #Les onglets de la page
    tabItems(
      
      #Contenu du premier onglet
      tabItem(tabName = "1facteur",
              #On cree une ligne
              fluidRow(
                #on rajoute une boite dans laquelle on place un titre et une autre boite contenant un tableau
                box(highchartOutput("boxplot1")
                )
                ,
                #Toujours dans la ligne on cree une une boite contenant les selecteurs avec leur description
                box(h4("Selection des parametres"),
                    #Selecteur des colomnes avec des donnees numeriques
                    selectizeInput("numCol", "Critere numerique", choices = numericCol, selected = numericCol[1]),
                    
                    #Selecteur des colomnes
                    selectizeInput("alphaNumCol", "Critere alphanumerique", choices = alphaNumericCol, selected = alphaNumericCol[2]),
                    
                    #Selecteur des methodes d'ajustement
                    selectizeInput("method","Methode d\'ajustement", choices = c("none","holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr")),
                    
                    #Selecteur de l'intervalle de confiance
                    numericInput("conf","Intervalle de confiance",0.95, min = 0, max = 0.99, step = 0.05)
                )
                
              )
              ,
              
              fluidRow(
                #Boite avec un titre
                box(h4("Tableau d\'Anova"),
                    #affichage du retour de la console
                    verbatimTextOutput("anova1")
                    ),

                box(h4("Test de Levene"),
                    verbatimTextOutput("levene1")
                    ),

                box(h4("Test de Shapiro"),
                    verbatimTextOutput("shapiro1")
                    ),

                box(h4("Vue Globale"), h5("[1,] = effectifs, [2,] = moyenne , [3,] = ecart-type"),
                    verbatimTextOutput("topView1")
                    ),

                box(h4("Test de Pairwise"),
                    verbatimTextOutput("pairwise1")
                ),
                box(h4("Test de Pairwise sans homogeneite des variances"),
                    verbatimTextOutput("pairwise1_F")
                    
                ),
                box(h4("Intervalles de Confiance"),
                  plotOutput("tukeyPlot1")
                )
              )
      ),
      tabItem(tabName = "2facteurs",
              fluidRow(
                box(h4("Selection des parametres"),
                    selectizeInput("numCol2", "Critere numerique", choices = numericCol, selected = numericCol[1]),
                    selectizeInput("alphaNumCol2_1", "Premier critere alphanumerique", choices = alphaNumericCol, selected = alphaNumericCol[2]),
                    selectizeInput("alphaNumCol2_2", "Deuxieme critere alphanumerique", choices = alphaNumericCol, selected = alphaNumericCol[3])
                ),
                box(h4("Effets individuels des variances"),
                    
                    verbatimTextOutput("effetsIndiv")
                )
                
              )
              ,
              
              fluidRow(
                box(h4("Tableau d\'Anova"),
                    verbatimTextOutput("anova2")
                    ),
                    
                box(h4("Test de Shapiro"),
                    verbatimTextOutput("shapiro2")
                    ),
                    
                box(h4("Tests de Levene"),
                    verbatimTextOutput("levene2_1"),
                    verbatimTextOutput("levene2_2")
                    
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
 #PREMIERE PAGE
  
  #Donnes utilises pour l'analyse a un facteur
  unFacteur <- reactive(sqldf(paste("Select",input$numCol, "as numeric,",input$alphaNumCol, "as var From data Where numeric <> 0.99999 and var <> 0.99999 order by numeric, var", sep = " ")))
  
  #Boite a moustache
  output$boxplot1 <- renderHighchart({
      hcboxplot(x = unFacteur()$numeric, var = unFacteur()$var, outliers = FALSE, name = paste(input$numCol,"/",input$alphaNumCol, sep = "")) %>%
      hc_title(text = paste("Repartition de la variable", input$numCol, "en fonction de la variable", input$alphaNumCol, sep = " "))
  })

  #Vue globale des donnees
  output$topView1 = renderPrint(
    round(
      rbind(
        table(unFacteur()$var),
        tapply(unFacteur()$numeric,unFacteur()$var,mean),
        tapply(unFacteur()$numeric,unFacteur()$var,sd)),
      )
    )
  
  #tableau d'Anova a un facteur
  anova_1 <- reactive(lm(numeric~var,data=unFacteur()))
  
  #Tableau d' Anova
  output$anova1 <- renderPrint(summary(anova_1()))
  
  #Homogeneite des variances
  output$levene1 <- renderPrint(leveneTest(anova_1()))
  
  
  #Normalite des residus
  output$shapiro1 <- renderPrint(shapiro.test(anova_1()$residuals))
  
  
  #Test LSD (Difference la moins significative)
  output$pairwise1 <- renderPrint(pairwise.t.test(unFacteur()$numeric,unFacteur()$var,p.adjust=input$method))
  
  #Non homogeneite des varieteances
  output$pairwise1_F <- renderPrint(pairwise.t.test(unFacteur()$numeric,unFacteur()$var,pool.sd = F,p.adjust=input$method))
  
  #Test de Tukey
  tukey_1 <- reactive(TukeyHSD(aov(numeric~var,data=unFacteur()),conf.level=input$conf))
  
  #Rendu du graphique du test de Tukey
  output$tukeyPlot1 <- renderPlot(plot(tukey_1()))
  
  #Rendu du test de Tukey
  tukey_1_1 <- reactive(glht(anova_1(), mcp(var = "Tukey")))
  
  #Test de Tukey plus complet
  output$tukeyMult1 <- renderPrint(summary(tukey_1_1()))
  
  #Regroupement des moyennes
  output$tukeyRegroupement1 <- renderPrint(cld(tukey_1_1()))
  
  #Comparaison a un controle
  output$dunnett1 <- renderPrint(summary(glht(anova_1(), mcp(var = "Dunnett"))))
  
  
  
  
  
  
  #DEUXIEME PAGE
  
  #Donnes utilises pour l'analyse a deux facteurs
  unFacteur2 <- reactive(sqldf(paste("Select",input$numCol2, "as numeric,",input$alphaNumCol2_1, "as var1,",input$alphaNumCol2_2,"as var2 From data Where numeric <> 0.99999 and var1 <> 0.99999 and var2 <> 0.99999 order by numeric, var1, var2", sep = " ")))
  
  #Tableau d'Anova a deux facteurs
  anova_2 <- reactive(lm(numeric~var1+var2,data= unFacteur2() ))
  
  #Rendu du tableau d Anova
  output$anova2 <- renderPrint(summary(anova_2()))
  
  #Tests sur les effets individuels des facteurs
  output$effetsIndiv <- renderPrint(anova(anova_2()))
  
  
  #Normalite des residus
  output$shapiro2 <- renderPrint(shapiro.test(anova_2()$residuals))
  
  
  #Homogeneite des variances
  output$levene2_1 <- renderPrint(leveneTest(anova_2()$residuals~unFacteur2()$var1))
  
  output$levene2_2 <- renderPrint(leveneTest(anova_2()$residuals~unFacteur2()$var2))
  
}

shinyApp(ui, server)
