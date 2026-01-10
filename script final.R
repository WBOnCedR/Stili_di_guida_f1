
#Working set up


rm(list=ls())

library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)
library(ggcorrplot)
library(mclust)
library(caret)
library(factoextra)
library(GGally)
library(Rmixmod)
library(dotenv)
library(flexmix)
library(plotly)
options(warn = -1) 

load_dot_env()
setwd(Sys.getenv("WORK_DIR"))

data_path24 <- "C:/Users/feder/Documents/datasets/Computazionale/F1/data/dataset_completo_best_tel2024.rds"

data_path25 <-  "C:/Users/feder/Documents/datasets/Computazionale/F1/data/dataset_completo_best_tel2025.rds"

# Esplorazione del dataset

tel4 <- readRDS(data_path24)
tel5 <- readRDS(data_path25)


tel.ex <- tel4 %>% filter(GP == "United States Grand Prix" & (pilota == "VER" |pilota == "LEC"|pilota== "COL"|pilota=="BEA"))
colori_team <- c("VER" = "#0600EF", "LEC" = "#EF1A2D", "COL" = "#FF8700","BEA"="#000000")



tel_g <- tel.ex %>%
  select(rel_distance, pilota, acc_y, acc_x,speed,brake,throttle) %>%
  pivot_longer(cols = c(acc_y, acc_x,speed,throttle), 
               names_to = "variabile", 
               values_to = "valore") %>%
  mutate(variabile = factor(variabile, 
                            levels = c("acc_y", "acc_x","speed","throttle"),
                            labels = c(" Acc. lat. ", " Acc. long. ", "Velocità","Acceleratore" )))


pp <- ggplot(tel_g, aes(x = rel_distance, y = valore, color = pilota)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.4) +
  geom_line(size = 0.6, alpha = 0.8) +
  facet_grid(variabile ~ ., scales = "free_y", switch = "y") + 
  scale_color_manual(values = colori_team) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Distanza relativa",
    y = NULL,
    color = "Pilota"
  ) +
  theme(
    strip.placement = "outside", 
    strip.text.y = element_text(face = "bold", size = 7),
    strip.background = element_rect(fill = "gray96", color = NA),
    legend.position = "top",
    panel.spacing = unit(1.2, "lines"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15))
  )

print(pp)

#ggsave("report/el_ex1.pdf", pp, width = 7, height = 5, device = cairo_pdf)




tel.ex <- tel5 %>% filter(GP == "United States Grand Prix" & (pilota == "VER" |pilota == "LEC"|pilota== "COL"|pilota=="BEA"))
colori_team <- c("VER" = "#0600EF", "LEC" = "#EF1A2D", "COL" = "#FF8700","BEA"="#000000")



tel_g <- tel.ex %>%
  select(rel_distance, pilota, acc_y, acc_x,speed,brake,throttle) %>%
  pivot_longer(cols = c(acc_y, acc_x,speed,throttle), 
               names_to = "variabile", 
               values_to = "valore") %>%
  mutate(variabile = factor(variabile, 
                            levels = c("acc_y", "acc_x","speed","throttle"),
                            labels = c(" Acc. lat. ", " Acc. long. ", "Velocità","Acceleratore" )))


pp <- ggplot(tel_g, aes(x = rel_distance, y = valore, color = pilota)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.4) +
  geom_line(size = 0.6, alpha = 0.8) +
  facet_grid(variabile ~ ., scales = "free_y", switch = "y") + 
  scale_color_manual(values = colori_team) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Distanza relativa",
    y = NULL,
    color = "Pilota"
  ) +
  theme(
    strip.placement = "outside", 
    strip.text.y = element_text(face = "bold", size = 7),
    strip.background = element_rect(fill = "gray96", color = NA),
    legend.position = "top",
    panel.spacing = unit(1.2, "lines"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15))
  )

print(pp)


#ggsave("report/el_ex1.pdf", pp, width = 7, height = 5, device = cairo_pdf)








process <- function(data_path) {
    cat("Caricamento dataset \n")
    tel <- readRDS(data_path)
    head(tel)
    tel <- as_tibble(tel)
    str(tel)
    tel$lap_time <- as.numeric(tel$lap_time)
    cat("Prime statistiche descrittive e pulizia \n")
    (summary(tel))
    tel$throttle <- ifelse(tel$throttle > 100, 100, tel$throttle)
    tel <- tel %>% filter(!is.na(lap_time))
    cat("Trasformazione Accelerazioni e Creazione Decelerazioni \n")
    tel.guida <- tel %>%  
      group_by(GP,pilota) %>% 
      mutate(
        dec_x = if_else(acc_x < 0, abs(acc_x), 0),
        acc_x = if_else(acc_x > 0, acc_x, 0),
        acc_y = abs(acc_y)) %>% 
      ungroup()
    cat("Creazione variabili lag \n")
    tel.guida <- tel.guida %>%
      
      select(GP,pilota,throttle,acc_x,acc_y,dec_x,brake,speed,rel_distance) %>% 
      
      arrange(GP,pilota,rel_distance) %>%
      
      group_by(GP,pilota) %>%
      
      mutate(
        across(
          c(throttle, acc_x, acc_y,dec_x,speed),
          list(
            
            lag1 = ~round(
              ifelse(
                lag(.x, 1) == 0 & .x == 0,
                0,
                ifelse(
                  lag(.x, 1) == 0,
                  (.x-0.01)/0.01,
                  (.x - lag(.x, 1)) / lag(.x, 1))
              ),
              4
            ),
            
            lag5 = ~round(
              ifelse(
                lag(.x, 5) == 0 & .x == 0,
                0,
                ifelse(
                  lag(.x, 5) == 0,
                  (.x-0.01)/0.01,
                  (.x - lag(.x, 5)) / lag(.x, 5))
              ),
              4
            )
            
          ),
          .names = "{.col}_{.fn}"
        )
      )  %>%  
      ungroup() %>% 
      select(-rel_distance)
   cat("statistiche descrittive \n")
   tel.guida_summary <- tel.guida %>% 
     group_by(GP, pilota) %>%
     {
       lag_cols <- names(.) %>% .[str_detect(., "lag")]
       
       summarise(.,
                 across(
                   c(throttle, acc_x, acc_y,dec_x,brake,speed), 
                   list(
                     mean = ~round(mean(.x, na.rm = TRUE), 4),
                     sd = ~round(sd(.x, na.rm = TRUE), 4)
                   ),
                   .names = "{.col}_{.fn}"
                 ),
                 
                 across(
                   all_of(lag_cols),
                   ~round(mean(.x[.x > 0], na.rm = TRUE), 4),
                   .names = "{.col}_mean_pos"
                 ),
                 
                 across(
                   all_of(lag_cols),
                   ~round(mean(.x[.x <= 0], na.rm = TRUE), 4),
                   .names = "{.col}_mean_neg"
                 ),
                 
                 across(
                   all_of(lag_cols),
                   ~round(sd(.x[.x > 0], na.rm = TRUE), 4),
                   .names = "{.col}_sd_pos"
                 ),
                 
                 across(
                   all_of(lag_cols),
                   ~round(sd(.x[.x <= 0], na.rm = TRUE), 4),
                   .names = "{.col}_sd_neg"
                 ),
                 
                 .groups = "drop"
       )
     }
   tel2 <- tel %>% 
     group_by(GP,pilota) %>% 
     summarize(laptime=max(lap_time),.groups = "drop" )
   tel.guida_summary$laptime <- as.numeric(tel2$laptime)
   cat("Creazione dei CV \n")
   sd_cols <-tel.guida_summary %>% names(.) %>% .[str_detect(., "sd")]
   mean_cols <-tel.guida_summary %>%  names(.) %>% .[str_detect(., "mean")]
   
   tel.guida_summary.idx <- tel.guida_summary[sd_cols] / tel.guida_summary[mean_cols]
   
   names(tel.guida_summary.idx) <- str_replace(names(tel.guida_summary.idx), "sd", "CV")
   
   tel.guida_summary.idx$GP <- tel.guida_summary$GP                                          
   
   tel.guida_summary.idx$pilota <- tel.guida_summary$pilota 
   tel.guida_summary.idx <- tel.guida_summary.idx %>% arrange(GP,pilota) 
   tel2 <- tel2 %>% arrange(pilota,GP) 
   tel.guida_summary.idx$laptime <- tel2$laptime
   cat("Completato \n")
   return(tel.guida_summary.idx)
}

tel24 <- process(data_path24)
tel25 <- process(data_path25)

#Eliminazione dei casi limite
tel24 %>% 
  group_by(pilota) %>% 
  select(pilota,GP,everything())%>% 
  filter(is.na(throttle_CV))

tel24 %>% group_by(GP) %>% select(pilota,GP,laptime) %>% filter(laptime > 1.07*min(laptime))
#Nei GP di gran bretagna la pista si è andata ad asciugare nel tempo e piloti che non hanno avuto la possibilità di fare un giro con la pista asciutta hanno fatto ovviamente un tempo peggiore per cui non si eliminano i record.
#Per quanto riguarda il Brasile invece le condizioni di visibilità erano molto poco favorevoli quindi si ha una variabilità dei tempi molto alta
#invece Zhou in arabia saudita si è incidentato quindi non si considera nell'analisi 
tel24 <- tel24 %>% filter(!(pilota=="ZHO" & GP=="Saudi Arabian Grand Prix")) %>% select(-laptime)


tel25 %>% 
  group_by(pilota) %>% 
    select(pilota,GP,everything())%>% 
        filter(is.na(throttle_CV))

tel25 <- tel25 %>% 
            filter(!(pilota=="RUS" & GP=="Miami Grand Prix"))

tel25 %>% group_by(GP) %>% select(pilota,GP,everything()) %>% filter(laptime > 1.07*min(laptime)) %>% select(-laptime)

#Nel GP di Las Vegas, durante la Q1 la pista era inizialmente bagnata, ma si è progressivamente asciugata nel corso delle qualifiche. Poiché il dataset considera per ogni pilota solo il miglior tempo registrato, i piloti eliminati in Q1 possono avere tempi che non rispecchiano la condizione tipica della sessione (cioè tempi influenzati dalla pista bagnata) e, di conseguenza, non vengono esclusi dal dataset. In contrasto, nel GP di Olanda, il pilota Strole ha avuto un incidente in Q1 e il suo miglior tempo disponibile risulta quindi quello di un giro di riscaldamento, che non rappresenta le prestazioni reali in qualifica.
tel25 <- tel25 %>% filter(!(pilota=="STR" & GP=="Dutch Grand Prix"))%>% select(-laptime)

tel.ex <- tel5 %>% filter(GP == "Australian Grand Prix" & (pilota == "BEA" |pilota == "HAM"|pilota== "TSU"))
colori_team <- c("BEA" = "#0600EF", "HAM" = "#EF1A2D", "TSU" = "#FDD900")



tel_g <- tel.ex %>%
  select(rel_distance, pilota, acc_y, acc_x) %>%
  pivot_longer(cols = c(acc_y, acc_x), 
               names_to = "variabile", 
               values_to = "valore") %>%
  mutate(variabile = factor(variabile, 
                            levels = c("acc_y", "acc_x"),
                            labels = c(" Accelerazione laterale (g)", " Accelerazione longitudinale (g)")))


pp <- ggplot(tel_g
             , aes(x = rel_distance, y = valore, color = pilota)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.4) +
  geom_line(size = 0.6, alpha = 0.8) +
  facet_grid(variabile ~ ., scales = "free_y", switch = "y") + 
  scale_color_manual(values = colori_team) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Distanza relativa",
    y = NULL,
    color = "Pilota"
  ) +
  theme(
    strip.placement = "outside", 
    strip.text.y = element_text(face = "bold", size = 7),
    strip.background = element_rect(fill = "gray96", color = NA),
    legend.position = "top",
    panel.spacing = unit(1.2, "lines"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15))
  )

print(pp)


#ggsave("report/tel_ex2.pdf", pp, width = 7, height = 5, device = cairo_pdf)

### PCA

tel24 <- tel24 %>% mutate(across(where(is.numeric),~rescale(.,to=c(0,1))))

corr24 <- round(cor(tel24 %>% select(where(is.numeric))),4)

variabili_dipendenti24 <- findCorrelation(corr24, cutoff = 0.9, names = TRUE, exact = T,verbose = T)

print(variabili_dipendenti24)


tel.pca24 <- tel24 %>% select(-all_of(variabili_dipendenti24))



tel25 <- tel25 %>% mutate(across(where(is.numeric),~rescale(.,to=c(0,1))))

corr25 <- round(cor(tel25 %>% select(where(is.numeric))),4)

variabili_dipendenti25 <- findCorrelation(corr25, cutoff = 0.9, names = TRUE, exact = T,verbose = T)

print(variabili_dipendenti25)


tel.pca25 <- tel25 %>% select(-all_of(variabili_dipendenti25))


PCA <- princomp(tel.pca25 %>%
                  select(where(is.numeric)),cor=T)


cumulative <- as_tibble(get_eigenvalue(PCA)$cumulative.variance.percent)

cumulative$Componenti <- 1:24

p <- ggplot(cumulative, aes(x=Componenti,y=value))+
  geom_bar(stat="identity",aes(fill = value))+
  scale_fill_fermenter(palette = "Set2")+
  geom_label(aes(label=round(value,2)),stat="identity")+
  labs(title= "Cumulative variance",
       y="Proporzione")+
  theme_minimal()
print(p)
#ggsave("report/tel_pca.pdf", p, width = 7, height = 5, device = cairo_pdf)

summary(PCA)


print(PCA$loadings,cutoff = 0)

loadings <- as.data.frame(PCA$loadings[, 1:4])


loadings$Variable <- rownames(loadings)
loadings_long <- loadings %>%
  pivot_longer(cols = starts_with("Comp"), 
               names_to = "component", 
               values_to = "peso")

p <- ggplot(loadings_long, aes(x = component, y = Variable, fill = peso)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(peso, 2)), size = 2.5, color = "black")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "#E10600", name = "Pesi") +
  theme_minimal() +
  labs(x = "Componenti",
       y = "Variabili") 

print(p)

#ggsave("report/Loadings.pdf", p, width = 7, height = 5, device = cairo_pdf)



tel.comp <- as_tibble(PCA$scores[,1:4])

set.seed(126)

clust <- Mclust(tel.comp,G=1:15)
summary(clust)
plot(clust, what="classification")
plot(clust, what="uncertainty")
plot(clust, what = "BIC", legendArgs = list(x = "bottomleft"))
clust$BIC






tel.comp.labels <- tel.comp
tel.comp.labels$class <- as.factor(clust$classification)
tel.comp.labels$pilota <- tel.pca25$pilota
tel.comp.labels$GP <- tel.pca25$GP


tel.comp.labels <- tel.comp.labels %>% arrange(GP,pilota,)

colori <- c("red", "blue", "green", "orange", "purple", "cyan", "brown", "pink", "black","grey","yellow","darkblue","darkred")

p <- ggplot(tel.comp.labels, aes(x = Comp.2, y = Comp.1, color = class)) +
  geom_point(size = 2) +
  scale_color_manual(values = colori)+
  labs(
    title =  "Comp1 vs Comp2",
    x = "Componente 2",
    y = "Componente 1",
    color = "Cluster",
    shape = "Cluster"
  )+theme_minimal()

print(p)
#ggsave("report/C1_C2.pdf", p, width = 7, height = 5, device = cairo_pdf)






p_3d <- plot_ly(tel.comp.labels, 
                x = ~Comp.2, 
                y = ~Comp.1, 
                z = ~Comp.4, 
                color = ~class, 
                colors = colori,
                type = 'scatter3d', 
                mode = 'markers',
                marker = list(size = 3)) %>%
  layout(
    title = "Comp1 vs Comp2 vs Comp4",
    scene = list(
      xaxis = list(title = 'Componente 2'),
      yaxis = list(title = 'Componente 1'),
      zaxis = list(title = 'Componente 4')
    )
  )


p_3d






#Metriche di valutazione
KL <- round(abs(clust$icl - clust$bic),3); KL

round(mean(clust$uncertainty),4)



### Interpretazione dei risultati


unique_GP <- unique(tel.comp.labels$GP)


p <- ggplot(tel.comp.labels, aes(x = Comp.2, y = Comp.1)) +
    geom_point(aes(colour = as.factor(class)), alpha = 0) + 
    geom_text(aes(label = GP, colour = as.factor(class)), 
            vjust = -1, size = 3, check_overlap = T,show.legend = F) + 
  scale_color_manual(values = colori) +
  coord_cartesian(xlim = c(-5,5), ylim = c(-7,7)) +
  labs(title = paste("Plot Comp.2 vs Comp.1 "),
       colour = "Classe") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5, shape = 19))) +
  theme_minimal()

print(p)

#ggsave("report/Loadings.pdf", p, width = 7, height = 5, device = cairo_pdf)


summary <- tel.comp.labels %>%
  group_by(class) %>%
  summarize(
    across(where(is.numeric),
           list(
             mean = ~mean(.x, na.rm=T)
           )), .groups = "drop"
  )

table(tel.comp.labels$GP,tel.comp.labels$class)


#Classificazione


center_2025 <- PCA$center
sd_2025 <- PCA$scale

tel.pca24 <- scale(tel.pca24 %>% select(where(is.numeric)),center = center_2025,scale = sd_2025)

test_set <- as_tibble(tel.pca24 %*% PCA$loadings)[,1:4]


test_set$GP <- tel24$GP 
test_set$pilota <- tel24$pilota
test_set$label <- ifelse(test_set$GP == "Monaco Grand Prix", 13,
                                   ifelse(test_set$GP == "Italian Grand Prix", 8, 2))
test_set <- test_set %>% 
              filter(GP == "Monaco Grand Prix"| GP== "Italian Grand Prix"| GP == "Azerbaijan Grand Prix")

listmod=c("Gaussian_pk_L_I","Gaussian_pk_Lk_I","Gaussian_pk_L_B","Gaussian_pk_Lk_B","Gaussian_pk_L_Bk",
          "Gaussian_pk_Lk_Bk","Gaussian_pk_L_C","Gaussian_pk_Lk_C","Gaussian_pk_L_D_Ak_D","Gaussian_pk_Lk_D_Ak_D",
          "Gaussian_pk_L_Dk_A_Dk","Gaussian_pk_Lk_Dk_A_Dk","Gaussian_pk_L_Ck","Gaussian_pk_Lk_Ck")

tel.data <- tel.comp

tel.comp.labels$class <- as.factor(tel.comp.labels$class)

tel.class <- unlist( tel.comp.labels[,5] )

str(tel.class)

str(tel.data)


model <- list()
set.seed(123)
f=0
n <- nrow(tel.data)
results <- vector("list", length = 2000 * 5)
k <- 0

for (i in sample(1:100000, 2000, replace = FALSE)) {
  set.seed(i)
  f <- f+1
  for (c in c(10, 15, 30, 40, 50)) {
    k <- k + 1
    test.set.labels <- sample(1:n, c)
    
    res <- mixmodLearn(
      tel.data[-test.set.labels, ],
      tel.class[-test.set.labels],
      models = mixmodGaussianModel(
        listModels = listmod,
        equal.proportions = TRUE
      ),
      criterion = "CV"
    )
    
    results[[k]] <- tibble(
      seed  = i,
      ntest = c,
      model = res@bestResult@model,
      CV    = res@bestResult@criterionValue
    )
  }
  cat(paste("Progress",f/20,"% \n"))
}

modell <- bind_rows(results)


modell$model <- as.factor(modell$model)

sort(table(modell$model),decreasing = T)

prop.table(table(modell$model, modell$ntest), 2)


res <- mixmodLearn(
  tel.data,
  tel.class,
  models = mixmodGaussianModel(
    listModels = "Gaussian_pk_L_B",
    equal.proportions = FALSE
  ),
  criterion = "CV"
)


res


