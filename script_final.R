  
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
library(flexmix)
library(plotly)
library(dotenv)


load_dot_env()

setwd(Sys.getenv("WORK_DIR"))

data_path <- Sys.getenv("DATA")

# Esplorazione del dataset

tel <- readRDS(data_path)


tel.ex <- tel %>% filter(GP == "United States Grand Prix" & (pilota == "VER" |pilota == "LEC"|pilota== "COL"|pilota=="BEA"))
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

ggsave("report/el_ex1.pdf", pp, width = 7, height = 5, device = cairo_pdf)




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

tel.summary <- process(data_path)

# Eliminazione dei casi limite

tel.summary %>% 
  group_by(pilota) %>% 
    select(pilota,GP,everything())%>% 
        filter(is.na(throttle_CV))

tel.summary <- tel.summary %>% 
            filter(!(pilota=="RUS" & GP=="Miami Grand Prix"))

tel.summary %>% group_by(GP) %>% select(pilota,GP,everything()) %>% filter(laptime > 1.07*min(laptime)) %>% select(-laptime)

# Nel GP di Las Vegas, durante la Q1 la pista era inizialmente bagnata, ma si è progressivamente asciugata nel corso delle qualifiche. Poiché il dataset considera per ogni pilota solo il miglior tempo registrato, i piloti eliminati in Q1 possono avere tempi che non rispecchiano la condizione tipica della sessione (cioè tempi influenzati dalla pista bagnata) e, di conseguenza, non vengono esclusi dal dataset. In contrasto, nel GP di Olanda, il pilota Strole ha avuto un incidente in Q1 e il suo miglior tempo disponibile risulta quindi quello di un giro di riscaldamento, che non rappresenta le prestazioni reali in qualifica.

tel.summary <- tel.summary %>% filter(!(pilota=="STR" & GP=="Dutch Grand Prix"))%>% select(-laptime)

tel.ex <- tel %>% filter(GP == "Australian Grand Prix" & (pilota == "BEA" |pilota == "HAM"|pilota== "TSU"))
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

tel.summary <- tel.summary %>% mutate(across(where(is.numeric),~rescale(.,to=c(0,1))))

corr <- round(cor(tel.summary %>% select(where(is.numeric))),4)

variabili_dipendenti <- findCorrelation(corr, cutoff = 0.9, names = TRUE, exact = T,verbose = T)

print(variabili_dipendenti)


tel.pca <- tel.summary %>% select(-all_of(variabili_dipendenti))


PCA <- princomp(tel.pca %>%
                  select(where(is.numeric)),cor=T)


cumulative <- as_tibble(get_eigenvalue(PCA)$cumulative.variance.percent)

cumulative$Componenti <- 1:24

p <- ggplot(cumulative, aes(x=Componenti,y=value))+
  geom_bar(stat="identity",aes(fill = value))+
  scale_fill_fermenter(palette = "Set2")+
  geom_label(aes(label=round(value,2)),stat="identity")+
  labs(y="Proporzione",
       fill= "Perc")+
  theme_minimal()
print(p)
ggsave("report/tel_pca.pdf", p, width = 7, height = 5, device = cairo_pdf)

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

ggsave("report/Loadings.pdf", p, width = 7, height = 5, device = cairo_pdf)



tel.comp <- as_tibble(PCA$scores[,1:4])

colnames(tel.comp) <- c("IN_OUT","C_SHAPE","TRANS","TRACK")

set.seed(126)

clust <- Mclust(tel.comp,G=1:15)
summary(clust)
plot(clust, what="classification")
plot(clust, what="uncertainty")
plot(clust, what = "BIC", legendArgs = list(x = "bottomleft"))
clust$BIC






tel.comp.labels <- tel.comp
tel.comp.labels$class <- as.factor(clust$classification)
tel.comp.labels$pilota <- tel.pca$pilota
tel.comp.labels$GP <- tel.pca$GP


tel.comp.labels <- tel.comp.labels %>% arrange(GP,pilota,)

colori <- c("red", "blue", "green", "orange", "purple", "cyan", "brown", "pink", "black","grey","yellow","darkblue","darkred")

p <- ggplot(tel.comp.labels, aes(x = C_SHAPE, y = IN_OUT, color = class)) +
  geom_point(size = 2) +
  scale_color_manual(values = colori)+
  labs(
    x = "C_SHAPE",
    y = "IN_OUT",
    color = "Cluster",
    shape = "Cluster"
  )+theme_minimal()

print(p)
ggsave("report/C1_C2.pdf", p, width = 7, height = 5, device = cairo_pdf)






p_3d <- plot_ly(tel.comp.labels, 
                x = ~C_SHAPE, 
                y = ~IN_OUT, 
                z = ~TRACK, 
                color = ~class, 
                colors = colori,
                type = 'scatter3d', 
                mode = 'markers',
                marker = list(size = 3)) %>%
  layout( scene = list(
      xaxis = list(title = 'C_SHAPE'),
      yaxis = list(title = 'IN_OUT'),
      zaxis = list(title = 'TRACK')
    )
  )


p_3d






# Metriche di valutazione
KL <- round(abs(clust$icl - clust$bic),3); KL

round(mean(clust$uncertainty),4)



### Interpretazione dei risultati


unique_GP <- unique(tel.comp.labels$GP)


p <- ggplot(tel.comp.labels, aes(x = C_SHAPE, y = IN_OUT)) +
    geom_point(aes(colour = as.factor(class)), alpha = 0) + 
    geom_text(aes(label = GP, colour = as.factor(class)), 
            vjust = -1, size = 3, check_overlap = T,show.legend = F) + 
  scale_color_manual(values = colori) +
  coord_cartesian(xlim = c(-5,5), ylim = c(-7,7)) +
  labs(colour = "Classe") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5, shape = 19))) +
  theme_minimal()

print(p)

ggsave("report/C_SHAPE__IN_OUT__GP.pdf", p, width = 7, height = 5, device = cairo_pdf)


summary <- tel.comp.labels %>%
  group_by(class) %>%
  summarize(
    across(where(is.numeric),
           list(
             mean = ~mean(.x, na.rm=T)
           )), .groups = "drop"
  )

table(tel.comp.labels$GP,tel.comp.labels$class)








# Clustering with covariates
set.seed(12320)
final.vv <- stepFlexmix(cbind(IN_OUT, C_SHAPE, TRANS) ~ TRACK, 
                        data = tel.comp, 
                        k = 2:8,
                        nrep = 10, 
                        model = FLXMCmvnorm())

par(mfrow=c(1,1))
plot(BIC(final.vv),type='b',ylab='BIC')
points(x = which.min(BIC(final.vv)),min(BIC(final.vv)),col='red',pch=20)

plot(ICL(final.vv),type='b',ylab='ICL')
points(x = which.min(ICL(final.vv)),min(ICL(final.vv)),col='red',pch=20)

fit <- getModel(final.vv)

summary(fit)
KLdiv(fit)
fit@cluster
labs<-fit@cluster

plot(fit)

tel.comp.fit <- tel.comp
tel.comp.fit$class <- labs
tel.comp.fit$GP <- tel.pca$GP
tel.comp.fit$pilota <- tel.pca$pilota

# Analisi grafica
colore <- c("#1f77b4", "#ff7f0e", "#2ca02c","red")

p <- ggplot(data=tel.comp, mapping = aes(x=TRACK, y=C_SHAPE,color=factor(labs)))+
  geom_point()+
  scale_color_manual(values=colore)+
  geom_smooth(method="lm", se=F, size=1)+
  theme_minimal()
ggsave("report/C1_C4_R.pdf", p, width = 7, height = 5, device = cairo_pdf)

ggplot(data=tel.comp, mapping = aes(x=TRACK, y=IN_OUT,color=factor(labs)))+
  geom_point()+ 
  geom_smooth(method="lm", se=F, size=1)+
  theme_minimal()+
  labs(color="Classi")

ggplot(data=tel.comp, mapping = aes(x=TRACK, y=TRANS,color=factor(labs)))+
  geom_point()+ 
  geom_smooth(method="lm", se=F, size=1)+
  theme_minimal()


# in due dimensioni risulta molto poco interpretabile e graficamente si prova un plot in 3 
x_range <- seq(min(tel.comp$TRACK), max(tel.comp$TRACK), length.out = 100)
newdata <- data.frame(TRACK = x_range)


pred_lines <- list()


for(k in 1:4) {

    sub_data <- tel.comp[labs == k, ]
  

    m_comp1 <- lm(IN_OUT ~ TRACK, data = sub_data)
    m_comp2 <- lm(C_SHAPE ~ TRACK, data = sub_data)
  

    pred_lines[[k]] <- data.frame(
    TRACK = x_range,
    IN_OUT = predict(m_comp1, newdata = newdata),
    C_SHAPE = predict(m_comp2, newdata = newdata),
    Cluster = as.factor(k)
  )
}

plot_lines_df <- do.call(rbind, pred_lines)


tel.comp.labels$labs_factor <- as.factor(labs) 

plot_lines_df$Cluster <- as.factor(plot_lines_df$Cluster)


colore <- c("#1f77b4", "#ff7f0e", "#2ca02c","red")


p_3d <- plot_ly() %>%
  add_trace(data = tel.comp.labels, 
            x = ~TRACK, 
            y = ~IN_OUT, 
            z = ~C_SHAPE, 
            color = ~labs_factor,   
            colors = colore,          
            type = 'scatter3d', 
            mode = 'markers',
            marker = list(size = 3, opacity = 0.6),
            name = ~paste("Cluster", labs)) %>%
  add_trace(data = plot_lines_df,
            x = ~TRACK,
            y = ~IN_OUT,
            z = ~C_SHAPE,
            color = ~Cluster,         
            colors = colore,        
            type = 'scatter3d',
            mode = 'lines',
            line = list(width = 6),
            showlegend = FALSE) %>% 
  
  layout(
      scene = list(
      xaxis = list(title = 'TRACK (Predittore)'),
      yaxis = list(title = 'IN_OUT'),
      zaxis = list(title = 'TRANS')
    )
  )

p_3d

#Interpretazione dei risultati

summary <- tel.comp.fit %>%
  group_by(class) %>%
  summarize(
    across(where(is.numeric),
           list(
             mean = ~mean(.x, na.rm=T)
           )), .groups = "drop"
  )


summary2 <- tel.comp.fit %>%
  group_by(class,pilota,GP) %>%
  summarize(cont=n(), .groups = "drop"
  )

table(tel.comp.fit$pilota,tel.comp.fit$class)
table(tel.comp.fit$GP,tel.comp.fit$class)


