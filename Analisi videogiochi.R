library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(gganimate)
library(gifski)
vgsales = read_csv("..\\Desktop\\Progetto fondamenti\\vgsales.csv")

# Dati nella storia (1980 - 2016)
# Questo dataframe raggruppa in base al nome del videogioco e ricava le vendite totali delle copie distribuite tra più console
# Es: GTA V compare 5 volte nel dataframe (PC, XBOX360, XBOX ONE, PS3, PS4)
vgsalesTot <- vgsales %>% 
              group_by(Name) %>% 
              summarise(Total_NA = sum(NA_Sales), Total_EU = sum(EU_Sales), 
                        Total_JP = sum(JP_Sales), Total_Other = sum(Other_Sales), Total_Sales = sum(Global_Sales)) 

# Il dataframe di prima contiene solo il nome e le vendite totali quindi facciamo il join tra quello e il dataframe originale in modo
# da avere le informazioni complete
vgsalesTot <- inner_join(vgsales, vgsalesTot) %>% 
              distinct(Name, .keep_all=TRUE)  %>%
              arrange(-Total_Sales) %>%
              select(Name, Genre, Platform, Year, Publisher, Total_NA, Total_EU, Total_JP, Total_Other, Total_Sales)
vgsalesTot %>% print(n=30)

# Domanda 1: noi europei pensiamo solo al calcio/sport e preferiamo la Playstation?

# Sottodomanda 1: qual'è il genere più venduto per continente
# Raggruppiamo i videogiochi in base al genere e calcoliamo la somma di tutte le vendite
# Vediamo le vendite totali per genere
vgsalesGenre <- vgsales %>% 
  group_by(Genre) %>% 
  summarise(Total_NA = sum(NA_Sales), Total_EU = sum(EU_Sales), 
            Total_JP = sum(JP_Sales), Total_Other = sum(Other_Sales), Total_Sales = sum(Global_Sales)) 

# Eliminiamo le righe ripetute (inutili) e ordiniamo in base al numero di vendite decrescente
vgsalesGenre <- inner_join(vgsales, vgsalesGenre) %>% 
  select(Genre,Total_NA, Total_EU, Total_JP, Total_Other, Total_Sales) %>% 
  distinct(Genre, .keep_all=TRUE) %>% 
  arrange(-Total_Sales)

# Attraverso il gather dividiamo le vendite per continente su più righe
vgRegion <- vgsalesGenre %>%
  gather("Total_NA","Total_EU","Total_JP","Total_Other", key = Market_Zone, value = Sales_Number)

# Calcoliamo la percentuale delle vendite di un continente rispetto al totale
vgRegion <- vgRegion %>% mutate(perc = Sales_Number/Total_Sales)

# Grafico che mostra gli interessi videoludici dei continenti
vgRegion %>%
  ggplot(aes(x=Genre, y=perc, fill = Market_Zone)) + geom_bar(stat="identity",position = "dodge")

# Sottodomanda 2: quanto influisce il calcio sul mercato europeo
# I giochi sportivi hanno venduto di più in America, questo però non risponde alla nostra domanda: è così importante il calcio in Europa?
# Mettiamo una variabile true se il gioco è di calcio (se contiene FIFA o PES nel nome)
vgAux <- vgsalesTot %>% mutate(Calcio = ifelse(str_detect(Name, 'FIFA') | str_detect(Name, 'Pro Evolution'),TRUE,FALSE))

# Raggruppiamo tutti i giochi sportivi e sommiamo le loro vendite totali in base all'essere un gioco di calcio o meno
vgAux <- vgAux%>%
  filter(Genre=="Sports") %>%
  group_by(Calcio) %>%
  summarise(Total_NA = sum(Total_NA), Total_EU = sum(Total_EU), 
            Total_JP = sum(Total_JP), Total_Other = sum(Total_Other), Total_Sales = sum(Total_Sales)) 

# Come prima dividiamo le vendite per continente su più righe
vgAux <- vgAux %>% 
  gather("Total_NA","Total_EU","Total_JP","Total_Other", key = Market_Zone, value = Sales_Number) %>%
  mutate(perc = Sales_Number/Total_Sales)

# Il grafico mostra che effettivamente quasi il 60% dei giochi di calcio sono venduti in Europa. Quindi si, siamo molto interessati al calcio
vgAux %>%
  ggplot(aes(x=Calcio, y=perc, fill = Market_Zone)) + geom_bar(stat="identity",position = "dodge")

# Sottodomanda 3: nella guerra tra Xbox e Playstation che console preferiscono gli europei?
# Calcoliamo le vendite delle console nei vari mercati
vgPS <- vgsales %>%
  group_by(Platform) %>%
  summarise(Total_NA = sum(NA_Sales), Total_EU = sum(EU_Sales), 
          Total_JP = sum(JP_Sales), Total_Other = sum(Other_Sales), Total_Sales = sum(Global_Sales))

# Analizziamo la situazione durante la generazione Xbox360 e PS3. Calcoliamo il totale delle vendite di entrambe le console e vediamo quanto
# un singolo mercato ha influito nel totale delle vendite
vgPS%>%
  filter(Platform == 'X360' | Platform == 'PS3') %>%
  mutate(Total_Sales_All = sum(Total_Sales)) %>%
  gather("Total_NA","Total_EU","Total_JP","Total_Other", key = Market_Zone, value = Sales_Number) %>%
  mutate(perc = Sales_Number/Total_Sales_All) %>%
  ggplot(aes(x=Platform, y=perc, fill = Market_Zone)) + geom_bar(stat="identity",position = "dodge")

# Stesso discorso ma per Xbox One e PS4
vgPS%>%
  filter(Platform == 'XOne' | Platform == 'PS4') %>%
  mutate(Total_Sales_All = sum(Total_Sales)) %>%
  gather("Total_NA","Total_EU","Total_JP","Total_Other", key = Market_Zone, value = Sales_Number) %>%
  mutate(perc = Sales_Number/Total_Sales_All) %>%
  ggplot(aes(x=Platform, y=perc, fill = Market_Zone)) + geom_bar(stat="identity",position = "dodge")

# Sottodomanda 4: i giocatori Playstation che genere di giochi preferiscono
# Da questo grafico possiamo vedere che il genere sportivo è il secondo più apprezzato dal mercato europeo
vgsales %>%
  filter(str_detect(Platform, 'PS')) %>%
  gather("NA_Sales","EU_Sales","JP_Sales","Other_Sales", key = Market_Zone, value = Sales_Number) %>%
  filter(Market_Zone == "EU_Sales") %>%
  ggplot(aes(x=Genre, y=Sales_Number)) + geom_bar(stat="identity",position = "dodge")

# Domanda 2: quali sono le serie che compaiono più volte tra i 1000 videogiochi più venduti
# Vediamo le serie
titles <- vgsalesTot %>% select(Name) %>% head(1000)

serieUnaParola <- titles %>% 
  unnest_tokens(bigram, Name, token = "ngrams", n = 1) %>% 
  select(bigram) %>% 
  count(bigram, sort=TRUE) %>% 
  filter(n>=2) %>%
  filter(!grepl("^[0-9]+$",bigram, perl = T)) # Tiriamo via le stringhe composte da soli numeri
serieUnaParola

serieDueParola <- titles %>% 
  unnest_tokens(bigram, Name, token = "ngrams", n = 2) %>% 
  select(bigram) %>% 
  count(bigram, sort=TRUE) %>% 
  filter(n>=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

serieDueParola 

serieTreParola <- titles %>% 
  unnest_tokens(bigram, Name, token = "ngrams", n = 3) %>% 
  select(bigram) %>% count(bigram, sort=TRUE) %>% 
  filter(n>=2) %>%
  separate(bigram, c("word1", "word2","word3"), sep = " ")
serieTreParola

serieTre <- serieTreParola %>%
  unite(bigram, word1, word2, word3, sep = " ")
serieTre

serieDue <- serieDueParola %>%
  filter(!(word1 %in% serieTreParola$word1 & word2 %in% serieTreParola$word2)) %>%
  filter(!(word1 %in% serieTreParola$word2 & word2 %in% serieTreParola$word3)) %>%
  unite(bigram, word1, word2, sep = " ")

serieDue

serieUna <- serieUnaParola %>%
  filter(!bigram %in% serieDueParola$word1) %>%
  filter(!bigram %in% serieDueParola$word2)

serieUna

# Parole da rimuovere dall'elenco delle serie in quanto non vogliono dire niente e compaiono tra i primi 30 risultati oppure sono doppioni
words <- c("adventures", "version", "iv","edition","king","ds","your","the legend of","smackdown vs raw", "woods pga tour")

# Otteniamo il grafico contenente le 20 serie più popolari 
rbind(serieUna, serieDue, serieTre) %>%
  filter(!bigram %in% words) %>%
  arrange(-n) %>%
  head(20) %>%
  ggplot(aes(x=n, y=reorder(bigram, n))) + geom_bar(stat="identity", position = "dodge")

# Mario compare 3 volte nel grafico (super mario bros, mario kart e mario pary). Se contassimo tutti nomi che lo contengono quanto sarebbe 
# il suo totale in relazione alle altre serie. Ci chiediamo inoltre chi è il vincitore della console war degli anni 90 tra Mario e Sonic
mario <- titles %>%
  filter(str_detect(Name, 'Mario')) %>%
  summarise(bigram = "mario", n=n())

sonic <- titles %>%
  filter(str_detect(Name, 'Sonic')) %>%
  summarise(bigram = "sonic", n=n())

rbind(serieUna, serieDue, serieTre) %>%
  filter(!str_detect(bigram, 'mario')) %>%
  filter(!str_detect(bigram, 'sonic')) %>%
  rbind(mario, sonic) %>%
  filter(!bigram %in% words) %>%
  arrange(-n) %>%
  head(20) %>%
  ggplot(aes(x=n, y=reorder(bigram, n))) + geom_bar(stat="identity", position = "dodge")
  

# Animazione delle vendite
vgsalesAnni <- vgsales %>%
  filter(Year <= 2016) %>%
  group_by(Platform, Year) %>%
  summarize(Global_Sales = sum(Global_Sales)) %>%
  spread(key = Year, Global_Sales)

vgsalesAnni[is.na(vgsalesAnni)] <- 0
vgsalesAnni

range <- 3:length(1981:2018)
for(anno in range)
  vgsalesAnni[,anno] <- vgsalesAnni[,anno] + vgsalesAnni[,anno-1]

vgsalesAnni <- vgsalesAnni %>%
  gather('1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016',key = 'Year', value = 'Global_Sales')

vgsalesAnniTemp <- vgsalesAnni %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Global_Sales),
                     Value_rel = Global_Sales/Global_Sales[Rank==1],
                     Value_lbl = paste0(" ",round(Global_Sales/1e9)))  %>%
  filter(Rank <= 5)

p <- vgsalesAnniTemp %>%
  ggplot(aes(x = Value_rel, y = -Rank, fill = Platform)) + 
  geom_bar(stat="identity") +
  transition_states(
    Year,
    transition_length = 4,
    state_length = 1
  ) +
  geom_text(aes(-Rank,x=0,label = Platform,hjust=0)) +
  geom_text(aes(-Rank,x=Value_rel,label = round(Global_Sales), hjust=0)) +
  theme(legend.position = "none",axis.title = element_blank()) +
  labs(title = 'Vendite nell\' anno {closest_state}')

animate(p,100, fps = 25, duration = 20, width = 800, height = 600)

# Chi è il leader tra USA e Giappone nella realizzazione di videogiochi
v_nazioni <- readLines('Fondamenti di scienza dei dati\\NazioniAziende.txt')
v_nazioni

v_nazioni <- c(v_nazioni, replicate(length(vgsalesPublisher$Publisher)-length(v_nazioni), "NA"))
v_nazioni

nazioni_df <- data.frame(Publisher = vgsalesPublisher$Publisher,
           Nation = v_nazioni
)

write_rds(nazioni_df, "companiesNations.rds")
nazioni_df <- read_rds("companiesNations.rds")

nazioni_df
nazioni_df <- inner_join(vgsalesPublisher, nazioni_df)

nazioni_df %>%
  filter(Nation != "NA") %>%
  group_by(Nation) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  arrange(-Total_Sales) %>%
  ggplot(aes(x=reorder(Nation,-Total_Sales), y=Total_Sales, fill = Nation)) + geom_bar(stat="identity",position = "dodge") +
  theme(legend.position = "none",axis.title = element_blank()) + 
  scale_fill_manual("legend", values = c("Japan" = "gray", "USA" = "lightblue", "France" = "darkblue", "UK" = "red", "Italy" = "darkgreen", "Germany" = "Black", "Canada" = "red", "Poland" = "red", "Sweden" = "blue", "Denmark" = "red"))

# La compagnia di maggiore successo

v_console = c('Wii', 'NES', 'GB', 'DS', 'X360', 'PS3', 'PS2', 'SNES', 'GBA',' 3DS', 'PS4', 'N64', 'PS', 'XB', 'PC', 
              '2600', 'PSP', 'XOne', 'GC', 'WiiU', 'GEN', 'DC', 'PSV', 'SAT', 'SCD', 'WS', 'NG', 'TG16', '3DO',
              'GG', 'PCFX')
V_companies = c('Nintendo', 'Nintendo', 'Nintendo','Nintendo', 'Microsoft', 'Sony', 'Sony', 'Nintendo', 'Nintendo', 'Nintendo', 'Sony',
                'Nintendo', 'Sony', 'Microsoft', 'PC', 'Atari', 'Sony', 'Microsoft', 'Nintendo', 'Nintendo', 'Sega', 'Sega', 'Sony',
                'Sega', 'Sega', 'Bandai', 'SNK', 'NEC', 'Panasonic', 'Sega', 'NEC')

consoleCompany_df <- data.frame(Platform = v_console, Company = V_companies)

write_rds(consoleCompany_df, "consoleCompanies.rds")
consoleCompany_df <- read_rds("consoleCompanies.rds")

vgsalesPlatform <- vgsales %>% 
                   group_by(Platform) %>% 
                   summarise(Total_Sales = sum(Global_Sales))

vgsalesPlatform <- inner_join(vgsales, vgsalesPlatform) %>% 
                   select(Platform,Total_Sales) %>% 
                   distinct(Platform, .keep_all=TRUE) %>% 
                   arrange(-Total_Sales)

inner_join(vgsalesPlatform, consoleCompany_df) %>%
  group_by(Company) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  arrange(-Total_Sales) #%>%
  #ggplot(aes(x=reorder(Company,-Total_Sales), y=Total_Sales, fill = Company)) + geom_bar(stat="identity",position = "dodge") +
  #theme(legend.position = "none",axis.title = element_blank())




# Vediamo qual'è il publisher che ha avuto più vendite
vgsalesPublisher <- vgsales %>% 
                    group_by(Publisher) %>% 
                    summarise(Total_Sales = sum(Global_Sales))

vgsalesPublisher <- inner_join(vgsales, vgsalesPublisher) %>% 
                    select(Publisher,Total_Sales) %>% 
                    distinct(Publisher, .keep_all=TRUE) %>% 
                    arrange(-Total_Sales)
vgsalesPublisher

# Vediamo qual'è il genere di più successo
vgsalesGenre <- vgsales %>% 
                group_by(Genre) %>% 
                summarise(Total_Sales = sum(Global_Sales))

vgsalesGenre <- inner_join(vgsales, vgsalesGenre) %>% 
                select(Genre,Total_Sales) %>% 
                distinct(Genre, .keep_all=TRUE) %>% 
                arrange(-Total_Sales)
vgsalesGenre

# Dati riguardanti le decadi

# Grafico che ci mostra l'andamento delle vendite
vgsalesTot %>% 
  ggplot(aes(x = id, y = Total_Sales)) + geom_point() 

# Il gioco di maggior successo per ogni genere
vgsalesKing <- vgsalesTot %>%
  distinct(Genre, .keep_all=TRUE)
vgsalesKing

# Si usa stat="identity" per far si che come altezza del grafico non venga utilizzato il count() delle righe ma venga utilizzato il valore dell'asse y
vgsalesKing %>%
  ggplot(aes(x=Name, y=Total_Sales, fill = Genre)) + geom_bar(stat="identity")

vgsalesKing %>%
  ggplot(aes(x=Publisher, y=Total_Sales, fill = Genre)) + geom_bar(stat="identity")

# Giochi in base al publisher
vgsalesPublisherAux <- vgsalesPublisher %>%
  head(10)

vgsalesPublisherAux

inner_join(vgsales,vgsalesPublisherAux) %>%
  distinct(Publisher, .keep_all=TRUE) %>%
  ggplot(aes(x=Publisher, y=Total_Sales, fill = Genre)) + geom_bar(stat="identity",position = "dodge")

vgsalesPublisherAux <- vgsalesPublisher %>%
  head(5)
vgsalesPublisherAux
