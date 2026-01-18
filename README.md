# Formula 1 - Analisi Stili di Guida

L'obiettivo di questo progetto è analizzare le telemetrie delle qualifiche di Formula 1 per individuare e classificare diversi stili di guida utilizzando tecniche di Model-Based Clustering.

## Indice

- [Panoramica](#-panoramica)
- [Funzionalità](#-funzionalità)
- [Struttura del Progetto](#-struttura-del-progetto)
- [Requisiti](#-requisiti)
- [Installazione e Configurazione](#️-installazione-e-configurazione)
- [Utilizzo](#-utilizzo)
- [Risultati](#-risultati)

## Panoramica

Lo studio parte dall'analisi di file JSON contenenti i tempi sul giro e le telemetrie delle qualifiche della stagione 2025 di Formula 1. Attraverso una pipeline di pre-processing e feature engineering, vengono estratte variabili significative (accelerazione longitudinale/laterale, uso dell'acceleratore/freno, variazioni nel tempo). Successivamente, si applica la PCA (Principal Component Analysis) per ridurre la dimensionalità e si utilizza un model based clustering per raggruppare i piloti in base al loro stile di guida.

## Funzionalità

- **Data Ingestion**: Caricamento e parsing di file JSON (laptimes e telemetry)
- **Pre-processing**: Pulizia dei dati, gestione dei valori mancanti e filtraggio dei giri non validi (regola 107% FIA)
- **Feature Engineering**: 
  - Separazione dell'accelerazione longitudinale in `acc_x` (valori positivi) e `dec_x` (valori negativi in modulo)
  - Trasformazione di `acc_y` in valore assoluto per catturare l'intensità delle forze laterali
  - Calcolo di variazioni percentuali lag1 e lag5 (breve/medio periodo)
  - Calcolo di Coefficienti di Variazione (CV) per normalizzare rispetto al setup della vettura
- **Analisi Esplorativa**: Visualizzazione delle telemetrie e analisi delle correlazioni
- **PCA**: Riduzione dimensionale a 4 componenti principali interpretabili (IN_OUT, C_SHAPE, TRANS, TRACK)
- **Model-Based Clustering**: Identificazione di 13 cluster basati sulla configurazione del tracciato
- **MEM (Mixture of Experts Models)**: Clustering con covariate per ridurre l'effetto della pista e identificare 4 stili di guida puri

## Struttura del Progetto

```
.
├── .env.example              # Template per il file .env
├── README.md                 # Documentazione del progetto (questo file)
├── Risultati.md              # Report dei risultati in formato Markdown
├── pre-processing.R          # Script per unire e pulire i dati grezzi JSON
├── script_final.R            # Script R completo per l'analisi
├── QualiCluster.Rmd          # R Markdown principale per analisi, PCA e Clustering
├── static/                   # Cartella contenente grafici e immagini generati
└── data/                     # Cartella contenente i dataset 
```


## Requisiti

- **R** (versione 4.0 o superiore)
- **RStudio** (consigliato per eseguire i file .Rmd)
- Librerie R richieste:
  - `jsonlite`, `tidyverse`, `dotenv`
  - `ggplot2`, `RColorBrewer`, `scales`, `ggcorrplot`, `GGally`
  - `mclust`, `caret`, `factoextra`, `Rmixmod`, `flexmix`, `plotly`

Puoi installarle eseguendo in R:
```r
install.packages(c("jsonlite", "tidyverse", "dotenv", "ggplot2", "RColorBrewer", 
                   "scales", "ggcorrplot", "mclust", "caret", "factoextra", 
                   "GGally", "Rmixmod", "flexmix", "plotly"))
```

## Installazione e Configurazione

### 1. Clona il repository
```bash
git clone https://github.com/WBOnCedR/Statistica_Computazionale_Progetto.git
cd Statistica_Computazionale_Progetto
```

### 2. Scarica i dati
I dati di telemetria sono disponibili nel repository [TracingInsights/2025](https://github.com/TracingInsights/2025):

```bash
git clone https://github.com/TracingInsights/2025.git
```

Assicurati di essere sul branch `main`.

### 3. Configura le variabili d'ambiente
Copia il file `.env.example` in un nuovo file `.env`:
```bash
cp .env.example .env
# Oppure su Windows copia e rinomina manualmente
```

Apri il file `.env` e imposta i percorsi corretti:
```properties
# Percorso della directory di lavoro del progetto
WORK_DIR="C:/percorso/alla/cartella/formula1"

# Percorso dove salvare/leggere il file .rds processato
DATA="C:/percorso/alla/cartella/formula1/data/dataset_completo_best_tel.rds"

# Percorso della cartella contenente i dati JSON clonati (TracingInsights)
JSON_PATH="C:/percorso/ai/dati/TracingInsights/2025"
```

## Utilizzo

Il workflow dell'analisi si divide in due step principali:

### 1. Pre-processing
Esegui lo script `pre-processing.R`. Questo script:
- Legge i file JSON specificati in `JSON_PATH`
- Estrae il miglior giro di qualifica per ogni pilota
- Unisce i dati di telemetria
- Salva il risultato in un file `.rds` nel percorso specificato da `DATA`

```r
source("pre-processing.R")
```

### 2. Analisi e Clustering
Apri il file `QualiCluster.Rmd` in RStudio. Questo documento contiene l'intero flusso di analisi:
- Caricamento del dataset processato (variabile `DATA` nel .env)
- Data Cleaning e Feature Engineering avanzato
- Visualizzazione dei dati (ggplot)
- PCA: Riduzione a 4 componenti (IN_OUT, C_SHAPE, TRANS, TRACK)
- **Model-Based Clustering**: Identificazione di 13 cluster basati sulla configurazione del tracciato (Mclust)
- **MEM (Mixture of Experts)**: Regressione con covariate per identificare 4 stili di guida puri, controllando per l'effetto della pista

L'analisi si articola in due fasi:
1. **Prima fase**: Clustering standard che identifica principalmente le caratteristiche dei tracciati
2. **Seconda fase**: MEM che condiziona le prime 3 componenti (IN_OUT, C_SHAPE, TRANS) sulla quarta (TRACK) per isolare gli stili di guida

Puoi eseguire i chunk singolarmente o generare il report completo cliccando su **Knit** in RStudio.

## Risultati

I risultati completi dell'analisi sono disponibili nel file [`Risultati.md`](Risultati.md), che include:
- Analisi esplorativa del dataset
- Interpretazione delle componenti principali
- Descrizione dei cluster identificati
- Visualizzazioni e grafici (salvati in `static/`)

---

**Autori**: Pietro Riva, Federico Maccianti, Nicola Rapacioli 

