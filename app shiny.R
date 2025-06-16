library(shiny)
library(haven)
library(tidyverse)
library(caret)
library(ranger)
library(DT)
library(broom)
library(forcats)

fitxer_sav <- "C:/Users/X421/Desktop/tfg/tfg estadistica/PART PRACTICA/IMPACT_Final_All_V17_alumnes.sav"
num_arbres <- 500

# funció bootstrap IC
bootstrap_ICcate_binomial <- function(data, subgroup_var, n_bootstrap = 1000, alpha = 0.05) {
  data %>%
    group_by(subgroup = .data[[subgroup_var]]) %>%
    group_map(~{
      vals <- .x$CATE
      n <- length(vals)
      boots <- replicate(n_bootstrap, mean(sample(vals, replace = TRUE)))
      tibble(
        subgroup = unique(.x[[subgroup_var]]),
        n = n,
        cate = mean(vals),
        lower = quantile(boots, alpha/2),
        upper = quantile(boots, 1 - alpha/2)
      )
    }) %>% bind_rows()
}

ui <- fluidPage(
  titlePanel("Estimació de l'ITE amb X-learner"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", "Escull outcome:",
                  choices = c("PesoRN", "percentil_birth", "SGA_birth_hard_imput",
                              "PE", "PEearly", "PElate", "severeSGA", "SGA_prenatal")),
      selectInput("tractament", "Escull tractament:",
                  choices = c("Dieta mediterrània (2)" = 2,
                              "Reducció de l'estrès (3)" = 3)),
      actionButton("entrena", "Entrenar model"),
      hr(),
      uiOutput("formulari_pacient"),
      actionButton("calcula", "Calcular ITE")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("ITE individual",
                 uiOutput("ite_output"),
                 plotOutput("plot_resultats")
        ),
        tabPanel("Anàlisi per subgrup",
                 h4("Anàlisi avançat per subgrup"),
                 selectInput("subgrup", "Variable de subgrup:", choices = NULL),
                 uiOutput("proporcio_text"),
                 dataTableOutput("taula_ic_subgrup"),
                 plotOutput("boxplot_subgrup", height = "400px")
        ),
        tabPanel("Model lineal",
                 h4("Model lineal sobre CATEs:"),
                 h6("entre ** significa que els coeficient és significatiu (nivell de confiança=95%)"),
                 dataTableOutput("taula_lm")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  covariates       <- reactiveVal(NULL)
  tipus_covariates <- reactiveVal(NULL)
  datX             <- reactiveVal(NULL)
  tau_0            <- reactiveVal(NULL)
  tau_1            <- reactiveVal(NULL)
  
  observeEvent(input$entrena, {
    showModal(modalDialog("Entrenant model...", footer = NULL))
    withProgress(message = "Entrenant...", value = 0, {
      set.seed(1234)
      incProgress(0.1, "Carregant dades")
      dades <- read_spss(fitxer_sav)
      
      # formateja variables
      nums <- c("Cod","Edad","Talla","BMIpre","PesoRN","percentil_birth")
      facs <- setdiff(names(dades), nums)
      dades <- dades %>%
        mutate(across(all_of(facs), as.factor)) %>%
        mutate(across(setdiff(facs, c("Group_COD","Parto_tipo","SexoRN")),
                      ~fct_recode(.x, "No"="0", "Si"="1"))) %>%
        mutate(Edad = as.numeric(Edad))
      
      # outcome i covariables
      resposta <- input$outcome
      
      # 1) Detectem quins són tots els outcomes originals
      outcomes <- names(dades)%>%tail(8)
      
      # 2) Definim covs excloent Cod, Group_COD i TOTS els outcomes
      covs <- names(dades)%>%head(18) %>% .[-(1:2)]
      
      covariates(covs)
      tipus_covariates(sapply(dades[, covs], class))
      
      incProgress(0.2, "Filtrant dades")
      tract <- as.numeric(input$tractament)
      dat <- dades %>%
        filter(Group_COD %in% c(1, tract)) %>%
        rename(Y = !!resposta, T = Group_COD) %>%
        select(all_of(c("T", covs, "Y"))) %>%
        na.omit()
      
      # droplevels només si factor
      if (is.factor(dat$Y)) dat$Y <- droplevels(dat$Y)
      
      # defineix tipus resposta
      tipus <- if (is.factor(dat$Y)) {
        if (nlevels(dat$Y) < 2) {
          showNotification("Outcome amb un sol nivell.", type="error")
          removeModal(); return()
        }
        if (nlevels(dat$Y) == 2) "binaria" else "multiclasse"
      } else "numerica"
      
      metrica     <- switch(tipus, binaria="ROC", multiclasse="Accuracy", numerica="RMSE")
      norma_split <- if (tipus=="numerica") "variance" else "gini"
      sum_fun     <- if (tipus=="binaria") twoClassSummary else defaultSummary
      
      ctrl <- trainControl(method="cv", number=5,
                           classProbs=(tipus=="binaria"),
                           summaryFunction=sum_fun)
      
      incProgress(0.4, "Entrenant mu_0")
      mu0 <- train(Y~.-T, data=filter(dat,T==1),
                   method="ranger", metric=metrica, trControl=ctrl,
                   tuneGrid=expand.grid(mtry=4:6, splitrule=norma_split, min.node.size=10),
                   num.trees=num_arbres, importance="impurity")
      
      incProgress(0.6, "Entrenant mu_1")
      mu1 <- train(Y~.-T, data=filter(dat,T==tract),
                   method="ranger", metric=metrica, trControl=ctrl,
                   tuneGrid=expand.grid(mtry=4:6, splitrule=norma_split, min.node.size=10),
                   num.trees=num_arbres, importance="impurity")
      
      incProgress(0.75, "Calculant pseudo-valors")
      dat0 <- filter(dat, T==1); dat1 <- filter(dat, T==tract)
      psi0 <- if (tipus=="binaria") predict(mu1,dat0,type="prob")$Si - as.numeric(dat0$Y) else predict(mu1,dat0)-as.numeric(dat0$Y)
      psi1 <- if (tipus=="binaria") as.numeric(dat1$Y) - predict(mu0,dat1,type="prob")$Si else as.numeric(dat1$Y)-predict(mu0,dat1)
      d0 <- dat0[,covs] %>% mutate(psi=psi0)
      d1 <- dat1[,covs] %>% mutate(psi=psi1)
      
      incProgress(0.85, "Entrenant tau")
      t0 <- train(psi~., data=d0, method="ranger", metric="RMSE",
                  trControl=trainControl(method="cv",number=5),
                  tuneGrid=expand.grid(mtry=4:6, splitrule="variance", min.node.size=10),
                  num.trees=num_arbres)
      t1 <- train(psi~., data=d1, method="ranger", metric="RMSE",
                  trControl=trainControl(method="cv",number=5),
                  tuneGrid=expand.grid(mtry=4:6, splitrule="variance", min.node.size=10),
                  num.trees=num_arbres)
      
      incProgress(0.95, "Càlcul CATE")
      dat$CATE <- 0.5*predict(t0,dat) + 0.5*predict(t1,dat)
      datX(dat); tau_0(t0); tau_1(t1)
      
      vars_factor <- covariates()[ tipus_covariates()[ covariates() ] == "factor" ]
      
      updateSelectInput(
        session, "subgrup",
        choices = vars_factor,
        selected = vars_factor[1]
        )
      removeModal()
    })
  })
  
  # formulari adaptatiu (sense preguntar l'outcome)
  output$formulari_pacient <- renderUI({
    req(covariates(), tipus_covariates())
    lapply(covariates(), function(v) {
      if (tipus_covariates()[v] == "factor") {
        selectInput(v, v, choices = c("No","Si"), selected = "No")
      } else {
        numericInput(v, v, value = NA)
      }
    })
  })
  
  # predicció ITE
  observeEvent(input$calcula, {
    req(tau_0(), tau_1(), covariates(), tipus_covariates())
    
    # 1) Iniciar un data.frame d'una fila amb tantes columnes com covariables
    new_data <- setNames(
      # matriu 1xN buida
      data.frame(matrix(nrow = 1,
                        ncol = length(covariates())),
                 stringsAsFactors = FALSE),
      covariates()
    )
    
    # 2) Omplir-la, tenint en compte factors vs numèriques
    for (v in covariates()) {
      if (tipus_covariates()[v] == "factor") {
        new_data[[v]] <- factor(input[[v]], levels = c("No", "Si"))
      } else {
        new_data[[v]] <- as.numeric(input[[v]])
      }
    }
    
    # 3) Comprovar valors en blanc
    if (!all(complete.cases(new_data))) {
      output$ite_output <- renderPrint("Falten valors.")
      return()
    }
    
    # 4) Prediccions amb els noms correctes
    p0 <- predict(tau_0(),  newdata = new_data)
    p1 <- predict(tau_1(),  newdata = new_data)
    
    output$ite_output <- renderUI({
      val <- round(0.5 * (p0 + p1), 3)
      tags$div(
        style = "
      font-size: 24px;
      color: black;
      background-color: #E8F4FF;
      padding: 10px;
      border-radius: 8px;
      text-align: center;
    ",
        tags$strong("ITE: "), val
      )
    })
    
  })
  
  
  # gràfic distribució
  output$plot_resultats <- renderPlot({
    req(datX())
    ggplot(datX(), aes(x=CATE, y = ..density..)) +
      geom_histogram(bins = 30, position = "identity") +
      labs(title = "Distribució del CATE", x = "CATE", y = "Freqüència relativa")+
      geom_vline(xintercept = 0,
                 linetype = "dashed",
                 color = "red",
                 linewidth = 0.4)
  })
  
  # taula IC subgrup
  output$taula_ic_subgrup <- renderDataTable({
    req(datX(), input$subgrup)
    df <- bootstrap_ICcate_binomial(datX(), input$subgrup)
    df <- df %>% mutate(fmt = sprintf("%.3f (%.3f; %.3f)", cate, lower, upper))
    datatable(
      df %>% select(subgroup, fmt),
      colnames = c("Grup", "CATE (IC)")
    )
  })
  
  # text amb proporcions i solapament IC
  output$proporcio_text <- renderUI({
    req(datX(), input$subgrup)
    df <- bootstrap_ICcate_binomial(datX(), input$subgrup)
    # proporcions
    si_row <- df %>% filter(subgroup == "Si")
    prop_si <- if(nrow(si_row)>0) round(si_row$n / sum(df$n), 3) else 0
    # solapament IC
    overlap_msg <- {
      if (nrow(df) == 2) {
        ovl <- !(df$upper[1] < df$lower[2] || df$upper[2] < df$lower[1])
        if (ovl) "Els IC es toquen." else "Els IC <strong>no</strong> es toquen."
      } else {
        combos <- combn(nrow(df), 2)
        msgs <- apply(combos, 2, function(idx) {
          i <- idx[1]; j <- idx[2]
          ovl <- !(df$upper[i] < df$lower[j] || df$upper[j] < df$lower[i])
          paste0("Entre ", df$subgroup[i], " i ", df$subgroup[j], ": ",
                 if (ovl) "solapament" else "sense solapament")
        })
        paste(msgs, collapse = "; ")
      }
    }
    HTML(
      paste0(
        "<p><strong>Proporció de casos:</strong> ", prop_si, "</p>",
        "<p><strong>Solapament IC:</strong> ", overlap_msg, "</p>"
      )
    )
  })
  
  #Boxplot amb la variable escollida pels CATE
  output$boxplot_subgrup <- renderPlot({
    req(datX(), input$subgrup)
    # Treu només la columna CATE i la variable de subgrup escollida
    boxplot_data <- datX() %>%
      select(CATE, grup = .data[[input$subgrup]]) %>%
      filter(!is.na(grup))
    
    ggplot(boxplot_data, aes(x = grup, y = CATE, fill = grup)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.1, alpha = 0.2) +
      labs(
        x = NULL,
        y = "ITE estimat",
        title = paste("Distribució de CATE per", input$subgrup)
      ) +
      theme_minimal(base_size = 20) +
      theme(
        axis.title = element_text(face = "bold"),
        legend.position = "none"
      )
  })
  
  # taula model lineal
  output$taula_lm <- renderDataTable({
    req(datX(), covariates())
    f <- as.formula(paste("CATE ~", paste(covariates(), collapse = "+")))
    m <- lm(f, datX())
    broom::tidy(m, conf.int = TRUE) %>%
      mutate(fmt = ifelse(p.value < 0.05,
                          sprintf("**%.2f (%.2f; %.2f)**", estimate, conf.low, conf.high),
                          sprintf("%.2f (%.2f; %.2f)", estimate, conf.low, conf.high)
      )) %>%
      select(term, fmt) %>%
      rename(Variable = term, `IC 95%` = fmt)
  })
}

shinyApp(ui, server)
