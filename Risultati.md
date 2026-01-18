# F1: Telemetrie Qualifiche

**Gruppo CORES**  
*Statistica Computazionale - Report Finale*

**Membri:**  
- Maccianti Federico (909656)
- Rapacioli Nicola (915439)
- Riva Pietro (908813)

---

## Introduzione

Il seguente studio si pone come obiettivo quello di individuare i diversi stili di guida presenti nella Formula 1, tramite l'analisi dei dati di telemetria ([TracingInsights](https://github.com/TracingInsights/2025.git)) di ciascun pilota.

Lo studio si concentra sui dati relativi alle sessioni di qualifica della stagione di Formula 1 2025, selezionando per ciascun pilota il singolo giro migliore.

Il dataset originale include le seguenti variabili:

| Variabile | Unità | Supporto | Tipo |
|-----------|-------|----------|------|
| Gran premio | -- | {Nomi GP} | char |
| Pilota | -- | {Sigle} | char |
| Tempo dal via | s | ℝ⁺ | num |
| Distanza percorsa | m | ℝ⁺ | num |
| Distanza relativa | -- | [0, 1] | num |
| Velocità | km/h | ℕ | num |
| Regime motore | RPM | ℕ | num |
| Marcia | -- | {1, ..., 8} | int |
| Freno | -- | {0, 1} | fact |
| Acceleratore | % | [0, 100] | num |
| DRS | -- | {0, 1} | fact |
| Accelerazione lat. e long. | g | ℝ | num |
| Coordinate x,y,z | m | ℝ | num |
| Tempo Giro | s | ℝ⁺ | num |

Il dataset presenta le seguenti codifiche: le feature binarie assumono valore 0 in assenza dell'evento e 1 in sua presenza. La telemetria dell'acceleratore misura l'intensità dell'input del pilota, mentre per le distanze indicano la posizione progressiva rispetto allo start.

## Analisi Esplorativa

### Considerazioni sulle variabili

Poiché lo stile di guida non è riconducibile a variabili di tipo posizionale, le coordinate spaziali e le misure di distanza, sia assolute che relative, vengono escluse dall'analisi.

In questa fase preliminare, lo stile di guida viene descritto attraverso variabili dinamiche, quali l'utilizzo dell'acceleratore, del freno, la velocità e le seguenti accelerazioni longitudinali e laterali, che consentono, per esempio, di valutare rispettivamente le modalità di decelerazione in ingresso curva o anche l'intensità con cui la curva viene affrontata, tutte caratteristiche che fanno riferimento allo stile di guida.

A sostegno delle ipotesi sopra citate, si riporta la figura seguente che confronta nel Gran Premio degli USA le accelerazioni e velocità per i piloti: Charles Leclerc, Lando Norris, Max Verstappen e Franco Colapinto.

![Accelerazioni VER, LEC, NOR, COL](static/el_ex1.pdf)

Si nota infatti come nelle variazioni repentine si possano già individuare differenze significative tra piloti.

Dalle prime analisi descrittive sul dataset, viene osservato come per il pilota Russell al Gran Premio di Miami siano presenti degli `NA` nella variabile distanza relativa. Osservando i dati grezzi si può notare come ciò sia riconducibile al malfunzionamento dei sensori telemetrici - presenza di molti zeri in molte variabili e accelerazioni costanti durante l'intero giro. Per i motivi elencati sopra, si procede dunque ad eliminare i record.

![Confronto Australia Bearman](static/tel_ex2.pdf)

Inoltre, osservando la variabile tempo giro, alcuni piloti riportano alcuni valori `NA`. Osservando i dati, ciò si verifica per i piloti Tsunoda, Bearman e Hadjar rispettivamente nei Gran premi di Emilia Romagna, Australia e Stati Uniti.

Effettuando un'analisi di tipo qualitativo, si scopre che ciò è riconducibile ad un incidente durante il giro di qualifica. Le uniche telemetrie disponibili sono infatti riconducibili al giro di riscaldamento delle gomme. Al fine di eliminare rumore nell'analisi si rimuovono queste osservazioni.

Sempre per quanto riguarda la variabile tempo giro, considerando il regolamento della **FIA** ([39.4.b.i; PAG:47](https://www.fia.com/sites/default/files/fia_2025_formula_1_sporting_regulations_-_issue_1_-_2024-07-31.pdf)), secondo il quale se un pilota supera il 107% del tempo giro minore viene escluso dalle qualifiche, si uniforma il dataset. Incriminati risultano essere i piloti Hamilton, Tsunoda, Antonelli, Albon e Bortoleto nel Las Vegas Grand Prix e Stroll nel Dutch Grand Prix.

Analizzando il contesto, si nota che a Las Vegas i tempi più elevati sono imputabili alle condizioni meteo avverse (pioggia intensa), seguite da un progressivo asciugamento della pista che ha avvantaggiato gli altri piloti. Nel caso di Stroll, invece, il dato si riferisce al primo giro di lancio completo, poiché nel momento del giro di qualifica non è stato completato il giro per un incidente.

Di conseguenza per eliminare il rumore, si elimina soltanto l'occorrenza di Stroll, poiché il giro non rappresenta il "migliore" ma solamente quello di lancio.

#### Trasformazione e creazione di nuove variabili

Per quanto riguarda l'analisi della variabile di accelerazione laterale, viene trasformata tramite valore assoluto. Così da ottenere una misura dell'intensità complessiva (magnitudo).

Inoltre, l'accelerazione longitudinale viene suddivisa in:
- **Accelerazione**, che comprende tutti i valori positivi;
- **Decelerazione** che al contrario della precedente comprende tutti i valori negativi;

Successivamente viene applicato il valore assoluto e per le restanti variabili telemetriche si mantiene invece la forma originaria.

Si creano per le variabili velocità, acceleratore, decelerazione e accelerazioni, nuove variabili di variazione percentuali, per catturare l'intensità di variazione delle variabili che aiutano a distinguere lo stile di guida dei piloti, nei vari istanti di tempo:

- **_lag1**: variazione percentuale rispetto all'osservazione precedente;
- **_lag5**: variazione percentuale rispetto a cinque osservazioni precedenti;

che vengono calcolate come:

$$\Delta = \frac{x_t - x_{t-k}}{x_{t-k}}$$

indicando con $x_t$ l'unità statistica al tempo $t$ e $k=\{1,5\}$ numero di lag, per le misure di accelerazione e decelerazione. Nel caso in cui $x_{t-k}=0$ e $x_t=0$ non si applica la formula e la variabile _lag assume 0 di default, quando invece si osserva solamente $x_{t-k}=0$ si sostituisce con $x_{t-k}=0.01$ per riuscire a mantenere la validità del calcolo e continuità delle dinamiche telemetriche.

### Statistiche riassuntive

Vengono calcolate delle statistiche descrittive al fine di valutare in che modo le variabili possano essere associate allo stile di guida, considerando separatamente ciascun Gran Premio e Pilota.

Per le misure di accelerazione laterale, longitudinale, decelerazione e per gli input di frenata, accelerazione e velocità vengono calcolate media e deviazione standard.

Per le variabili di tipo _lag$k$ vengono calcolate medie e deviazioni standard distinguendo tra variazioni positive e negative, così da evidenziare eventuali asimmetrie nel comportamento dinamico del pilota.

Una volta calcolate media e deviazione standard, è possibile combinare questi valori per ottenere il coefficiente di variazione (CV):

$$\text{CV} = \frac{\text{sd}(x)}{\text{mean}(x)}$$

che misura la variabilità relativa di una variabile rispetto alla sua media. L'impiego di tale coefficiente permette di ridurre l'influenza del setup della vettura, rendendo confrontabili tra loro variabili che, in valore assoluto, dipendono dalle regolazioni meccaniche e aerodinamiche.

Si arriva così ad ottenere un dataset contenente 28 variabili, che forniscono informazione sullo stile di guida del pilota nella specifica gara.

Infine, per rimuovere l'influenza del tracciato, si riscalano tutte le variabili nell'intervallo [0,1].

Si procede dunque con una analisi delle correlazioni per ridurre la dimensionalità, e comprendere quali siano le variabili più significative nel fornire l'informazione.

### Analisi delle componenti principali

Al fine di limitare i fenomeni di multicollinearità ed eliminare il rumore, sono state eliminate le variabili fortemente correlate tra loro (cor(x,y)>0.9). Questo processo di scrematura permette di ridurre la dimensionalità a 26 variabili.

Nonostante tale riduzione, una dimensionalità pari a 26 risulta ancora essere impegnativa sia ai fini di interpretabilità che per costo computazionale per un Model Base Clustering.

Si effettua dunque un'analisi delle componenti principali (PCA) con l'obiettivo di ottenere un'ulteriore riduzione dimensionale. I risultati ottenuti tramite PCA mostrano come le prime 4 componenti spieghino più del 60% della varianza, come illustrato nella figura seguente.

![Varianza Spiegata Cumulata](static/tel_pca.pdf)

#### Interpretazione delle componenti

Attraverso l'analisi dei pesi associati alle componenti (figura seguente) le si interpretano:

1. **Prima componente** (IN_OUT): Separa stili di guida che hanno un'alta variabilità (molte correzioni) nell'erogazione della potenza da quelli che hanno un'alta variabilità nella fase di decelerazione. A valori elevati corrisponde uno stile di guida caratterizzato da una trazione più "sporca" o reattiva, con continue correzioni sul pedale dell'acceleratore. A questo si contrappone uno stile caratterizzato da una frenata molto modulata e dinamica, mantenendo però un'uscita di curva molto composta e lineare nell'erogazione del gas.

2. **Seconda componente** (C_SHAPE): distingue stili di guida che lavorano di più in termini di velocità pura (avanti/dietro) contro chi ha maggiore velocità di percorrenza (destra/sinistra). A valori elevati corrisponde uno stile di guida a "V", dove il pilota punta tutto sulla frenata profonda e sulla ripartenza rapida, variando molto l'accelerazione longitudinale. Invece, per valori bassi si contrappone uno stile di guida basato sulla velocità di percorrenza, che predilige la gestione del carico laterale.

3. **Terza componente** (TRANS): questa componente entra nel dettaglio delle fasi della curva. Discrimina stili di guida con bassa componente laterale in fase di percorrenza e alta componente di accelerazioni longitudinali e viceversa. In particolare, a valori alti corrisponde uno stile di guida caratterizzato da forti staccate, forti accelerazioni in uscita curva e bassa velocità di percorrenza. Mentre a valori bassi corrisponde uno stile caratterizzato da alte velocità di percorrenza e poca variazione longitudinale.

4. **Quarta componente** (TRACK): questa componente caratterizza la configurazione della pista. A valori elevati corrisponde una maggiore variabilità complessiva della velocità, mentre valori bassi indicano un andamento più regolare e uniforme.

**N.B.** Il circuito vincola il pilota a modificare il proprio stile di guida alla configurazione del tracciato, alternando fluidità e aggressività in base alle specifiche richieste aerodinamiche o meccaniche. Tale adattamento serve quindi esclusivamente a ottimizzare lo sfruttamento della massima aderenza disponibile in ogni circuito.

![Pesi componenti](static/Loadings.pdf)

## Model Based Clustering

L'analisi di Clustering Model-Based, condotta sulle prime quattro componenti, ha permesso di identificare un modello **VII** (*Volume variabile*, *forma sferica* e *orientamento identico*) a tredici classi, con un totale di 77 parametri stimati e BIC pari a -7108.435.

Il raggruppamento dei dati risulta già chiaramente distinguibile nella rappresentazione bidimensionale ottenuta dal confronto tra la prima e la seconda componente principale (figura seguente), sebbene sia ancora presente una certa sovrapposizione visiva, interpretabile come "rumore" grafico. Tale effetto è attribuibile principalmente all'informazione contenuta nelle componenti principali rimanenti. Ciò è ulteriormente evidenziato dalla visualizzazione 3D, in cui il confronto congiunto tra la quarta, la prima e la seconda componente consente di distinguere in modo più netto i cluster, migliorandone la separabilità rispetto alla rappresentazione bidimensionale.

![Componente 1 VS Componente 2](static/C1_C2.pdf)

![Componente 1 VS Componente 2 VS Componente 4](static/newplot.png)

Il valore della distanza di Kullback-Leibler (KL) del modello risulta essere pari a 89.622. Ciò dà sostegno a quanto già osservato graficamente, ulteriormente supportato dall'incertezza pari a 0.0796 che esclude una possibile situazione di sovrapposizione dei gruppi.

In particolare, i gruppi sembrano descrivere le seguenti situazioni:

- **Classe 13**: Questa classe identifica il Gran premio di Monaco. Il valore molto elevato della prima componente infatti si può associare ad un uso del gas ritmico.

- **Classi 5 e 11**: (Singapore, Messico, Ungheria) Queste classi raggruppano circuiti tortuosi ad alto carico. Condividono con Monaco la necessità di gestire l'acceleratore per gestire per esempio un uscita da una curva netta.

- **Classe 6**: (Baku) Mostra un'alta terza componente: le curve a 90° costringono a correzioni veloci dell'accelerazione laterale.

- **Classi 4 e 12**: (Las Vegas e Miami) Mostrano un'alta quarta componente: l'asfalto scivoloso soprattutto a Las Vegas per la pioggia che crea incertezza nel mantenere la velocità costante, costringendo a parzializzare il gas dove normalmente si andrebbe costanti.

- **Classi 8 e 9**: (Spa, Silverstone, Suzuka) Queste classi rappresentano i circuiti con prima componente fortemente negativa dovuta all'assenza di frenate decise. La seconda componente positiva distingue questi circuiti: indica che la variazione è dominata dalle forze longitudinali ad alta velocità.

- **Classe 10**: (Cina, USA, Brasile) Raggruppa circuiti completi dal punto di vista del tracciato: alternando tratti veloci a tratti tecnici. Non risulta esserci una componente con pesi notevoli.

- **Classe 2**: (Monza): La prima componente molto negativa. È una pista caratterizzata da staccate profonde con un'uscita di trazione pulita, inoltre la quarta componente abbastanza alta, dovuta alle elevate variazioni di velocità.

- **Classe 7**: (Bahrain e Canada): Sono circuiti caratterizzati da poche curve di alta velocità di percorrenza, evidenziati dal valore negativo della seconda componente.

- **Classe 1**: (Australian, Abu Dhabi, Qatar, Saudi, Spanish) È il gruppo più numeroso, caratterizzato da una prima componente negativa, quindi poche variazioni longitudinali e in generale fluidità nella percorrenza.

- **Classe 3**: (Austrian, Emilia Romagna) Molto simile alla classe 1, ma con curve più accentuate e maggiori variazioni longitudinali.

Come illustrato nella figura seguente, la distribuzione dei Gran Premi nelle prime due componenti principali evidenzia chiaramente la separazione tra le diverse classi identificate.

![Distribuzione gran premi nelle componenti](static/C_SHAPE__IN_OUT__GP.pdf)

## Model Based Clustering with Covariates

I risultati ottenuti dal precedente modello, sebbene chiari e definiti, non rispecchiano tuttavia l'obiettivo iniziale dell'analisi. Ciò è dato dal fatto che la quarta componente discrimina le classi per la tipologia di tracciato e non per lo stile di guida. Si prova quindi a ridurre l'effetto della pista, condizionando i parametri della distribuzione alla quarta componente.

Per far ciò si applica un **MEM** (*Mixture of Experts Models*), utilizzando una regressione normale multivariata con variabili:
- **Dipendente**: [IN_OUT, C_SHAPE, TRANS]
- **Esplicativa**: [TRACK]

La figura seguente mostra la relazione tra la componente TRACK e IN_OUT per ciascun cluster, evidenziando come il modello di regressione catturi l'influenza del tracciato sullo stile di guida.

Che rivela le seguenti classi:

1. **Cluster 1**: Rappresenta uno stile di guida reattivo, caratterizzato da frequenti correzioni sull'acceleratore (alto IN_OUT). Sebbene tenda a un approccio a "V" (C_SHAPE positivo), mantiene elevate velocità di percorrenza in curva (TRANS negativo);

2. **Cluster 2**: Definito da un approccio a "V" estremo (alto C_SHAPE) con staccate profonde e ripartenze rapide (alto TRANS). Il pilota è estremamente pulito nell'erogazione (basso IN_OUT), privilegiando la modulazione del freno e l'intensità longitudinale rispetto alla velocità pura a centro curva.

3. **Cluster 3**: Identifica uno stile focalizzato sulla velocità di percorrenza e sulla gestione del carico laterale (basso C_SHAPE). Nonostante forti fasi di accelerazione e frenata (alto TRANS), la trazione risulta "sporca" con diverse correzioni (IN_OUT positivo);

4. **Cluster 4**: È lo stile più fluido e composto, orientato alla massima percorrenza e stabilità laterale (bassi C_SHAPE e TRANS). L'erogazione del gas è lineare e la frenata molto modulata (basso IN_OUT), minimizzando le variazioni longitudinali a favore di una conduzione pulita e costante.

Nella figura seguente la divisione in classi sembra comunque molto sovrapposta, cosa che non si verifica invece nella visualizzazione tridimensionale che mostra le rette di regressione per ciascun cluster nello spazio delle componenti principali, permettendo di apprezzare la separazione tra i diversi stili di guida condizionati alla tipologia di tracciato.

![IN_OUT VS TRACK](static/C1_C4_R.pdf)

![IN_OUT VS C_SHAPE VS TRACK](static/newplot2.png)

I cluster riflettono quanto osservabile nella F1. Per esempio, il Cluster 2 identifica lo stile a "V" tipico di Verstappen e Leclerc, caratterizzato da forti staccate e riprese in uscita curva, mentre il Cluster 4 cattura la guida fluida a "U" di Norris e Russell, focalizzata sulla percorrenza.

Nonostante il tentativo di ridurre al minimo l'influenza della geometria del tracciato, anche questi cluster sembrano comunque dipendere molto da quest'ultima. Le motivazioni potrebbero essere:
- Le variabili utilizzate non discriminano realmente i diversi stili di guida dei piloti. Si potrebbe magari pensare di utilizzare altre variabili quali punti di staccata, di corda, angolo sterzo e altre;
- I piloti non hanno effettivamente uno stile di guida caratteristico, ma si adattano ai diversi circuiti seppur con le dovute differenze;

Si lascia la risposta a questi dubbi a futuri studi.
