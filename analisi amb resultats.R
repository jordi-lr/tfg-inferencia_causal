library(tidyverse)
library(broom)
library(knitr)
library(glue)

load("resultat.RData")

resultats_filtrat<- resultats %>%  filter(!map_lgl(dades, is.null))

resultats_bo<-resultats_filtrat %>% filter(variable!="Complic_Extra")%>%
  rename(outcome=variable)

#creo alguns objectes que necessitarem
outcomes<- resultats_bo$outcome %>% unique() %>% as.vector()

covariates<-resultats_bo$dades[[1]] %>% names() %>%
  .[-1] %>% head(16)

#### analisi general ####

fer_i_guardar_hist <- function(dades, resposta, tract, lims, model) {
  meta_learner<- substitute(dades) %>% sub("^dat", "", .)
  
  histograma<-ggplot(dades, aes(x = CATE)) +
    geom_histogram(
      aes(y=after_stat(density)),
      bins       = 30  # nombre de barres
    ) +
    labs(x= "Efecte del tractament(ITE)", y= "Freqüència") +
    theme_minimal(base_size = 16) +
    theme(
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10))+
    coord_cartesian(xlim = lims) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = "red",
               linewidth = 0.4) 
  
  print(histograma)
  
  ggsave(paste0("hist(",resposta,")",meta_learner,"_tract",tract,".jpg"), histograma,
         path="C:/Users/X421/Desktop/tfg/tfg estadistica/PART PRACTICA/plots/histogrames/hist_density",
         width = 1129 / 300,     # pulgadas
         height = 656 / 300,    # pulgadas
         dpi = 300)
}


walk(outcomes, function(var_actual) {
  dades <- resultats_filtrat %>%
    filter(variable == var_actual & tractament == 2) %>%
    pull(dades) %>% .[[1]]
  
  # Crear datT, datS, datX
  datT <- dades %>% mutate(CATE = CATE_t)
  datS <- dades %>% mutate(CATE = CATE_s)
  datX <- dades %>% mutate(CATE = CATE)
  
  # Calcular limits comuns
  lims <- range(c(datS$CATE, datT$CATE, datX$CATE), na.rm = TRUE)
  
  fer_i_guardar_hist(datS, resposta = var_actual, tract = 2, lims = lims, model = "_S")
  fer_i_guardar_hist(datT, resposta = var_actual, tract = 2, lims = lims, model = "_T")
  fer_i_guardar_hist(datX, resposta = var_actual, tract = 2, lims = lims, model = "_X")
  
  message("Guardar: ", var_actual)
})



#fer taula de models:
format_lm <- function(model, name) {
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      ci_fmt = ifelse(
        p.value < 0.05,
        sprintf("\\textbf{%.2f (%.2f; %.2f)}", estimate, conf.low, conf.high),
        sprintf("%.2f (%.2f; %.2f)", estimate, conf.low, conf.high))
    ) %>%
    select(term, ci_fmt) %>%
    rename(!!paste0("Tractament ", name) := ci_fmt)%>%
    mutate(term = gsub("Si$", "", term),
           term = factor(term, levels = c("(Intercept)",covariates)),
           term = gsub("_", "\\\\_", term)
    ) %>%
    rename(Variable = term)}



formula_lm<-as.formula(paste0("CATE ~",
                           paste0(covariates, collapse = "+"))
                    ) 

resultats_models <- resultats_bo %>%
  mutate(
    # si vull centrar i escalar:
    dades = map(dades, \(df) { df |>
        mutate(across(
          .cols=where(is.numeric) & !matches("CATE"),
          .fns=scale))}),
    models = map(dades, ~ lm(formula_lm , data = .x)),
    taula_model= map2(models, tractament, ~format_lm(.x, name=.y))
    )


taules_lm<- resultats_models %>%
  group_by(outcome) %>%
  summarise(
    taula = list(reduce(taula_model, full_join, by = "Variable")),
    .groups = "drop")

#per copiar a latex:
for (i in taules_lm$outcome){
  print(paste("taula per", i))
  taules_lm %>% filter(outcome==i)%>% select(taula) %>% .[[1]]%>%
    kable(format = "latex", booktabs = TRUE, escape = F, 
      caption = paste("Coeficients estimats i intervals de confiança model linial per CATE de", i)) %>%
    kableExtra::kable_styling(latex_options = "hold_position") %>% print()}



#pvalors dels models
resultats_models %>%
  mutate(
    pval_global = map_dbl(models, ~ {
      f_stat <- summary(.x)$fstatistic
      pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    })
  ) %>%
  select(-dades, -models, -taula_model)

#par veure a R
lm_veure_important <- function(model, name) {
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      ci_fmt = ifelse(
        p.value < 0.05,
        sprintf("**{%.2f (%.2f; %.2f)**", estimate, conf.low, conf.high),
        sprintf("%.2f (%.2f; %.2f)", estimate, conf.low, conf.high))
    ) %>%
    select(term, ci_fmt) %>%
    rename(!!paste0("Tractament ", name) := ci_fmt)%>%
    mutate(term = gsub("Si$", "", term),
           term = factor(term, levels = c("(Intercept)",covariates)),
           #term = gsub("_", "\\\\_", term)
    ) %>%
    rename(Variable = term)}

resultats_models <- resultats_bo %>%
  mutate(
    #si vull centrar i escalar:
    dades = map(dades, \(df) { df |>
        mutate(across(where(is.numeric), scale))}),
    models = map(dades, ~ lm(formula_lm , data = .x)),
    taula_model= map2(models, tractament, ~lm_veure_important(.x, name=.y))
  )

taules_importancia<- resultats_models %>%
  group_by(outcome) %>%
  summarise(
    taula = list(reduce(taula_model, full_join, by = "Variable")),
    .groups = "drop")

for (i in taules_importancia$outcome){
  print(paste("taula per", i))
  taules_importancia %>% filter(outcome==i)%>% select(taula) %>% .[[1]]%>%
    kable(caption = paste("Coeficients estimats i intervals de confiança model linial per CATE de", i),
          format="markdown")%>% print()}


#mirar distancia cate i cate_x2, entre X-learner amb i sense PS
dife_x<-resultats_bo %>%
  mutate(
    resumen_cate = map(dades, ~ tibble(
      "Diferència en la desviació" = (sd(.x$CATE)-sd(.x$CATE_x2))/sd(.x$CATE) %>% round(3),
      "Diferència en l'ATE"=(mean(.x$CATE)- mean(.x$CATE_x2))/mean(.x$CATE) %>% round(3)
      ))
  )%>% select(-dades)%>%
  unnest(resumen_cate)

dife_x %>%
  kable(format = "latex", booktabs = TRUE, escape = F, digits=3, 
        caption = "Diferències relatives entre utlitzar el X-learner amb el Propensity Score teòric i el empíric") %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>% print()



#interval confiança dels ATE: (es tira l'outcome)
IC_ate<-resultats_bo %>% filter(outcome=="PesoRN")%>%
  mutate(
    resumen_cate = map(dades, ~ tibble(
      "mean_x"=mean(.x$CATE),
      "upper" = mean(.x$CATE) + 1.96* (sd(.x$CATE)/sqrt(nrow(.x))),
      "lower"=mean(.x$CATE)-1.96* (sd(.x$CATE)/sqrt(nrow(.x)))
    ))
  )%>% select(-dades)%>%
  unnest(resumen_cate)

#mirar metriques dels metalearners 
resultats_bo %>% 
  mutate(
    resumen_cate = map(dades, ~ tibble(
      "mean_x"=mean(.x$CATE),
      "sd_t" = sd(.x$CATE_t),
      "sd_x"=sd(.x$CATE)
    ))
  )%>% select(-dades)%>%
  unnest(resumen_cate)


#també pot ser interesant visualitzar les dades (segons caracterisitiques i ITE)
#per fer scater plots amb resultats:
fer_scatterplot_result <- function(outcome, tract, variable_x, variable_col = NULL) {
  dades<- resultats%>%
    filter(variable==outcome, tractament==tract)%>%
    pull(dades) %>% .[[1]]
  

  p <- ggplot(dades, aes(x = .data[[variable_x]], y = CATE)) +
    geom_point(
      size = 2,
      alpha = 0.6
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "black",
      linewidth = 0.5,
      linetype = "dashed",
      alpha = 0.2
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(size = 22, h=0.5),
      axis.title = element_text(face = "bold", size = 19),
      axis.text = element_text(size = 15),
      legend.title = element_text(face = "bold",size = 19),
      legend.text = element_text(size = 15),
      legend.position = "right")
  
  if (!is.null(variable_col)) {
    p <- p + aes(color = .data[[variable_col]]) # +
      # labs(title = paste0("CATE en funció de ", variable_x, " diferenciant segons ", variable_col))
  } else {
    p <- p # +
      # labs(title = paste0("CATE vs ", variable_x))
  }
  
  p
}

# outcomes: "PesoRN", "percentil_birth", "SGA_birth_hard_imput", 
# "Complic_Extra"        "PE"                   "PEearly"             
# "PElate"               "severeSGA"            "SGA_prenatal"

# covariates: "Edad"           "Talla"          "BMIpre"         "Fum_10"        
# "AntRN_SGA"      "HTAcr"          "DMpreg"         "Nefropatia"    
# "Autoinm"        "Ut1T_patol"     "RiesgoPE"       "PAPPA_patol"   
# "Metrorragia"    "Criteria_minor" "TRA_incl"       "Nuliparidad" 

fer_scatterplot_result(outcome="SGA_birth_hard_imput", tract=2,
                variable_x="BMIpre", variable_col = NULL)

ggsave("scater_PE_3_TallaHTAcr.jpg",
       path = "C:/Users/X421/Desktop/tfg/tfg estadistica/PART PRACTICA/plots",
       width = 8,
       height = 6,
       dpi = 300)








####---------       Analisi en profunditat      --------#######

#per poder veure millor el proces i cada variable s'ha d'anar canviant manualment cada outcome
#per fer els intervals de confiança
#fer bootstrap:
resultats_outcome<- resultats_bo %>%
  filter(outcome=="PesoRN") #ficar l'outcome que interresa


#per fer el IC dels CATE
bootstrap_ICcate_binomial <- function(data, subgroup_var, n_bootstrap = 1000, alpha = 0.05) {
  results <- data %>% 
    group_by(subgroup = .data[[subgroup_var]]) %>%
    group_map(~{
      values <- .x[["CATE"]]
      n <- length(values)
      boot_means <- replicate(n_bootstrap, mean(sample(values, size = n, replace = TRUE)))
      tibble(
        subgroup = unique(.x[[subgroup_var]]),
        n = n,
        cate = mean(values),
        lower = quantile(boot_means, probs = alpha / 2),
        upper = quantile(boot_means, probs = 1 - alpha / 2)
      )
    }) %>% bind_rows()
  return(results)
}

data_dieta <- resultats_outcome %>% filter(tractament == 2) %>% pull(dades) %>% .[[1]]
data_reduccio<- resultats_outcome %>% filter(tractament == 3) %>% pull(dades) %>% .[[1]]

for (i in setdiff(covariates,c("Edad", "Talla","BMIpre"))){
  a<-bootstrap_ICcate_binomial(data_dieta, i, n_bootstrap = 1000, alpha = 0.05)
  a$n<-a$n/sum(a$n)
  variab<-c(i,i)
  a<-cbind(variab,a)
  a<-a %>%mutate(escrit=sprintf("%.3f (%.3f; %.3f)", cate, lower, upper))
  if(a$lower[1]<=a$upper[2] & a$upper[1]>=a$lower[2]){
    a$dins<-T}else{a$dins<-F}
  a_final<- a %>% select(c(variab,n, subgroup, escrit, dins))
  print(a_final)
}


#Fer IC de les diferencies (tractament - control) de les varaibles binàries:
bootstrap_cate_diff_correct <- function(data, subgroup_var, ite_col = "CATE", n_bootstrap = 1000, alpha = 0.05) {
  boot_diffs <- replicate(n_bootstrap, {
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    
    gS <- boot_data %>% filter(.data[[subgroup_var]] == "Si") %>% pull(.data[[ite_col]])
    gN <- boot_data %>% filter(.data[[subgroup_var]] == "No") %>% pull(.data[[ite_col]])
    
    mean(gS) - mean(gN)
  })
  
  diff_obs <- mean(data %>% filter(.data[[subgroup_var]] == "Si") %>% pull(.data[[ite_col]])) -
    mean(data %>% filter(.data[[subgroup_var]] == "No") %>% pull(.data[[ite_col]]))
  
  lower <- quantile(boot_diffs, alpha / 2)
  upper <- quantile(boot_diffs, 1 - alpha / 2)
  p_value <- mean(abs(boot_diffs) > abs(diff_obs))
  
  return(tibble(
    diff_obs = diff_obs,
    lower = lower,
    upper = upper,
    p_value = p_value
  ))
}

#faig la taula
for (i in setdiff(covariates,c("Edad", "Talla","BMIpre"))){
  a<-bootstrap_cate_diff_correct(data_reduccio, i)
  if(dplyr::between(0, a$lower, a$upper)){
    a <- a%>% mutate(igual="0_dins",
                escrit=sprintf("%.2f (%.1f; %.1f)", diff_obs, lower, upper))
  }else{
      a<-a%>%mutate(igual="fora_0",
                escrit=sprintf("\textbf{%.2f (%.1f; %.1f)}", diff_obs, lower, upper))
  }
  a<-cbind(i,a)
  a_final<-a%>%select(-c(diff_obs,lower, upper, p_value))
  print(a_final)
}


#boxplot de les variables amb més diferencia:
boxplot_data <- data_reduccio %>%
  select(CATE, c(Ut1T_patol , PAPPA_patol )) %>%
  pivot_longer(cols = c(Ut1T_patol , PAPPA_patol ), names_to = "variable", values_to = "grup") %>%
  filter(!is.na(grup))


ggplot(boxplot_data, aes(x = grup, y = CATE, fill = grup)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  facet_wrap(~variable, scales = "free_y") +
  labs(x=NULL,
    y = "ITE estimat") +
  theme_minimal(base_size = 24)+
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "none")

ggsave("boxplot_3_pes.jpg",
       path = "C:/Users/X421/Desktop/tfg/tfg estadistica/PART PRACTICA/plots",
       width = 8,
       height = 6,
       dpi = 300)


#Procés per continues (no treballarem fent bootstrap, frem model lineal)

#S'utilitza el model que es fan amb la funció "lm_veure_important" i es veuen a:
taules_importancia<- resultats_models %>%
  group_by(outcome) %>%
  summarise(
    taula = list(reduce(taula_model, full_join, by = "Variable")),
    .groups = "drop")

for (i in taules_importancia$outcome){
  print(paste("taula per", i))
  taules_importancia %>% filter(outcome==i)%>% select(taula) %>% .[[1]]%>%
    kable(caption = paste("Coeficients estimats i intervals de confiança model linial per CATE de", i),
          format="markdown")%>% print()}


#per fer la visualització amb boxplots de grups 8per veure si hi ha realcion linial a gran trets:
dades_cont <- data_reduccio %>%
  mutate(across(
      .cols = c(BMIpre, Edad, Talla),
      .fns = ~ cut(.x,
        breaks = quantile(.x, probs = seq(0, 1, 0.25), na.rm = TRUE),
        include.lowest = TRUE
      )))

ggplot(dades_cont, aes(x = Talla, y = CATE)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.2)+
  theme_minimal(base_size = 24)+
  # labs()+
  theme(
    axis.title = element_text(face = "bold"))

ggsave("boxplot_talla_3_pes.jpg",
       path = "C:/Users/X421/Desktop/tfg/tfg estadistica/PART PRACTICA/plots",
       width = 8,
       height = 6,
       dpi = 300)
