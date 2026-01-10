# F1 Telemetry Analysis & Clustering

Questo progetto è stato sviluppato per il corso di **Statistica Computazionale** (A.A. 2025-2026).
L'obiettivo è analizzare le telemetrie delle qualifiche di Formula 1 per individuare e classificare diversi stili di guida ("Driving Styles") utilizzando tecniche di Model-Based Clustering.

## 📋 Indice

- [Panoramica](#panoramica)
- [Funzionalità](#funzionalità)
- [Struttura del Progetto](#struttura-del-progetto)
- [Requisiti](#requisiti)
- [Installazione e Configurazione](#installazione-e-configurazione)
- [Utilizzo](#utilizzo)

## 🎯 Panoramica

Lo studio parte dall'analisi di file JSON contenenti i tempi sul giro e le telemetrie. Attraverso una pipeline di pre-processing e feature engineering, vengono estratte variabili significative (accelerazione longitudinale/laterale, uso dell'acceleratore/freno, variazioni nel tempo). Successivamente, si applica la PCA (Principal Component Analysis) per ridurre la dimensionalità e si utilizza `Mclust` per raggruppare i piloti in base al loro stile di guida.

## Funzionalità

- **Data Ingestion**: Caricamento e parsing di file JSON (laptimes e telemetry).
- **Pre-processing**: Pulizia dei dati, gestione dei valori mancanti e filtraggio dei giri non validi.
- **Feature Engineering**: 
  - Separazione dell'accelerazione longitudinale in `acc_x` (valori positivi) e `dec_x` (valori negativi in modulo)
  - Trasformazione di `acc_y` in valore assoluto per catturare l'intensità delle forze laterali
  - Calcolo di variazioni percentuali lag (breve/medio periodo) e Coefficienti di Variazione (CV)
- **Analisi Esplorativa**: Visualizzazione delle telemetrie e correlazioni.
- **PCA**: Riduzione delle variabili correlate in 4 componenti principali interpretabili.
- **Clustering**: Identificazione di cluster di stili di guida tramite Gaussian Mixture Models (Model-Based Clustering).
- **Classificazione**: Modelli per predire l'appartenenza di nuove osservazioni.

## 📁 Struttura del Progetto

```
.
├── .env                # File di configurazione (NON committare)
├── .env.example        # Template per il file .env
├── README.md           # Documentazione del progetto
├── pre-processing.R    # Script per unire e pulire i dati grezzi JSON
├── QualiCluster.Rmd    # R Markdown per analisi, PCA e Clustering
├── data/               # Cartella contenente i dati (input/output)
│   └── dataset_completo_best_tel.rds  # Dataset processato
├── findcorrelation     # File ausiliari (se presenti)
└── report.pdf          # Report finale (output del Rmd)
```

## 🚀 Requisiti

- **R** (versione 4.0 o superiore)
- **RStudio** (consigliato per eseguire i file .Rmd)
- Librerie R richieste:
  - `jsonlite`, `tidyverse`, `dotenv`
  - `ggplot2`, `RColorBrewer`, `scales`, `ggcorrplot`, `GGally`
  - `mclust`, `caret`, `factoextra`, `Rmixmod`

Puoi installarle eseguendo in R:
```r
install.packages(c("jsonlite", "tidyverse", "dotenv", "ggplot2", "RColorBrewer", 
                   "scales", "ggcorrplot", "mclust", "caret", "factoextra", 
                   "GGally", "Rmixmod"))
```

## 🛠️ Installazione e Configurazione

1. **Clona il repository**:
   ```bash
   git clone https://github.com/WBOnCedR/Statistica_Computazionale_Progetto.git
   cd Statistica_Computazionale_Progetto
   ```

2. **Configura le variabili d'ambiente**:
   Copia il file `.env.example` in un nuovo file `.env`:
   ```bash
   cp .env.example .env
   # Oppure su Windows copia e rinomina manualmente
   ```

   Apri il file `.env` e imposta i percorsi corretti:
   ```properties
   # Percorso della cartella contenente i file JSON (laptimes e telemetry)
   JSON_PATH="C:/percorso/ai/tuoi/dati/json"

   # Directory di lavoro del progetto (la cartella dove si trova questo README)
   WORK_DIR="C:/Users/TuoUtente/.../formula1"

   # Percorso dove salvare/leggere il file .rds processato
   DATA="C:/Users/TuoUtente/.../formula1/data/dataset_completo_best_tel.rds"
   ```

## 💻 Utilizzo

Il workflow dell'analisi si divide in due step principali:

### 1. Pre-processing
Esegui lo script `pre-processing.R`. Questo script:
- Legge i file JSON specificati in `JSON_PATH`.
- Estrae il miglior giro di qualifica per ogni pilota.
- Unisce i dati di telemetria.
- Salva il risultato in un file `.rds` (specificato nello script/env).

**Nota**: Assicurati che le cartelle di output esistano o verifica i percorsi nello script.

### 2. Analisi e Clustering
Apri il file `QualiCluster.Rmd` in RStudio.
Questo documento contiene l'intero flusso di analisi:
- Caricamento del dataset processato (variabile `DATA` nel .env).
- Data Cleaning e Feature Engineering avanzato.
- Visualizzazione dei dati (ggplot).
- Calcolo PCA e Clustering (Mclust).

Puoi eseguire i chunk singolarmente o generare il report finale cliccando su **Knit**.

---
**Autori**: Pietro Riva, Federico Maccianti, Nicola Rapacioli
