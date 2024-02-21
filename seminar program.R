### R - Programm                                               #########
### Für Seminararbeit
### in   "Algorithmische und statistische Methoden der Zeitreihenanalyse"
### von:       Frank Müller
### M-Nr:      9053395
### Betreuer:  Univ.-Prof. Dr. Robinson Kruse-Becher
### Thema:   Imputationsverfahren für fehlende Daten            #########

### Das Program wurde in R in einem Google Collab Notebook geschrieben und kompiliert 
### Der Compiler von R Studio 


### Installation der Bibliotheken
library(quantmod)
library(tidyverse)
library(xts)
library(ggcorrplot)
library(imputeTS)

### Download der Daten

## DJI- Symbole
dji_symbols = c("AAPL","AMGN","AXP","BA","CAT","CRM","CSCO","CVX","DIS","GS","HD","HON","IBM","INTC","JNJ","JPM","KO","MCD","MMM","MRK","MSFT","NKE","PG","TRV","UNH","VZ","WBA","WMT")
# Enthält alle Aktien, die schon vor 2018 im DJI enthalten waren. Neuzugänge wurden ausgeschlossen. Visa wurde auch ausgeschlossen, da der Aktienkurs zu stark mit Microsoft korrelliert.

# Herunterladen der Kurse
kurse = getSymbols(dji_symbols, from = "2018-01-01", to = "2019-12-31", warning=FALSE)

### Erzeugen eines Dataframes mit den Schlusskursen der Aktien

portfolio = data.frame(get("AAPL")[,4])
for (element in 2:28){
  portfolio = cbind(portfolio, data.frame(get(kurse[element])[,4]))
}

# Dataframe abspeichern:
#write.csv(portfolio, "\\content\\sample_data\\dj1819.csv", row.names=TRUE)
# Gespeicherten Dataframe laden:
#portfolio <- readr::read_csv("C:/.../dj1819.csv")

# Die Aktienkurse von Microsoft und Visa korrelieren zu 0,987. Daher wird Visa aus dem Dataframe entfernt, da sonst der Amelia- Algorithmus nicht funktioniert.

# Die Korrelationen zwischen den Aktienkursen in einer Heatmap:
ggcorrplot(round(cor(portfolio), 1),hc.order = TRUE)

# Der Kursverlauf von Apple im Beobachtungszeitraum zwischen 2018 und 2019
plot(portfolio[,1], type="l", ylim=c(20,80), main="Apple Inc 2018 und 2019", xlab="2018 - 2019", ylab="Kurs in $",  xaxt = "n")

# Der ACF und PCF von Apple
stats::acf(portfolio[,1])
stats::pacf(portfolio[,1])

# Alle Aktienkurse des Dataframes im Beobachtungszeitraum 2018 und 2019
plot(portfolio[,1], type="l", ylim=c(0,450), main=" Dow Jones Industrial Average", xlab="2018 - 2019", ylab="Kurs in $",  xaxt = "n")
for (i in 2:28){
  lines(portfolio[,i],, type = "l")
}

### Berechung der Log-Renditen/ stetige Rendite

# Die stetige Rendite ist die Differenz des Logarithmierten Kurses der Vortages mit 
# dem Logarithmierten Kurs des Folgetages (natürlicher Logarithmus).

h1 = portfolio[1:501,]
h2 = portfolio[2:502,]
portfolio_rendite <- log(h1/h2)

# Eine visuelle Aufbreitung der Korrelationen zwischen den Renditen des Datasets

ggcorrplot(round(cor(portfolio_rendite[,1:28]), 1),hc.order = TRUE)

# Die Entwicklung der Rendite, der ACF und PACF der Rendite von Apple

plot(portfolio_rendite[,1], type="l", main="Rendite von Apple Inc 2018 und 2019", xlab="2018 - 2019", ylab="Kurs in $",  xaxt = "n")

stats::acf(portfolio_rendite[,1])
stats::pacf(portfolio_rendite[,1])

### Erstellung eines Amputierten Datasets indem zufällige Wert gelöscht werden

df_mcar <- data.frame(portfolio)
random_NA_values <- function(df, gap_amount, gap_size){
  init_values_set_to_NA <- gap_amount*nrow(df)
  for (i in 1:ncol(df)){
    set.seed <- 42
    df[[i]][sample(nrow(df),init_values_set_to_NA)]<-NA
  }
  for (i in 1:ncol(df)){
    for (j in nrow(df):1){
      if(is.na(df[j,i])){
        gap <- gap_size
        for(k in 1:gap){
          if(j+k>nrow(df)){
            break
          }
          df[j+k,i]<- NA
        }
      }
    }
  }
  #summary(df)
  return(df)
}

### Erstellung eines Amputierten Datasets indem zufällige Wert gelöscht werden mit definiertem Seed

random_NA_values_with_seed <- function(df, gap_amount, gap_size, seed){
  init_values_set_to_NA <- gap_amount*nrow(df)
  for (i in 1:ncol(df)){
    set.seed <- seed
    df[[i]][sample(nrow(df),init_values_set_to_NA)]<-NA
  }
  for (i in 1:ncol(df)){
    for (j in nrow(df):1){
      if(is.na(df[j,i])){
        gap <- gap_size
        for(k in 1:gap){
          if(j+k>nrow(df)){
            break
          }
          df[j+k,i]<- NA
        }
      }
    }
  }
  #summary(df)
  return(df)
}

### Erzeugen eines Polts der die fehlenden Werte in einem amputierten Dataset darstellt
df_mcar <- random_NA_values(portfolio, 0.01, 2)
imputeTS::ggplot_na_distribution(df_mcar[3])
statsNA(df_mcar$AAPL.Close)

### Visualisierung der Renditen des Portfolios

# Histogramme der einzelnen Spalten
data_long <- portfolio_rendite %>%                          # Umwandeln in eine lange Pivotfunktion
  pivot_longer(colnames(portfolio_rendite)) %>% 
  as.data.frame()

p <- ggplot(data_long, aes(x=value)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  facet_wrap(~ name, scales = "free")+
  geom_density(alpha=.2, fill="#FF6666")
p

# Veranschaulichung der amputierten Werte
imputeTS::ggplot_na_imputations(df_mcar[10:500,3], portfolio[10:500,3]) 

### Hilfsfunktionen

# Funktion um Maske zu erschaffen, in der alle NA mit FALSE und alle gültigen Werte mit TRUE ersetzt werden
create_NA_mask <- function(dataframe_with_na){
  df_bool <- data.frame(dataframe_with_na)
  df_bool[!is.na(df_bool)] <- 0
  df_bool[is.na(df_bool)] <- 1
  return (df_bool)
}

# Evaluationsfunktion mit MSE
# Die Dimensionen der Datasets müssen gleich sein
calculate_MSE <- function(original_dataset, unimputed_dataset, imputed_dataset){
  mask_of_unimputed_dataset <- create_NA_mask(unimputed_dataset)
  #berechne für jedes imputiertes NA die jeweilige quadratische Abweichung vom Ursprungswert (M)SE  
  difference_sqrt_dataset <- (original_dataset - imputed_dataset)^2 * mask_of_unimputed_dataset
  #print(difference_sqrt_dataset)   
  MSE = sum(difference_sqrt_dataset)/sum(is.na(unimputed_dataset)) 
  return(MSE)
}

create_pooled_imp_df_amelia <- function(dataframe_with_na){
  dataframe_with_na <- cbind(date = as.Date(rownames(dataframe_with_na)), dataframe_with_na)
  rownames(dataframe_with_na) <- 1:nrow(dataframe_with_na)
  a.out <- Amelia::amelia(data.frame(dataframe_with_na),p2s = 0, m=5, ts="date",polytime=2)
  df_amelia <- portfolio
  for (i in 1:nrow(dataframe_with_na)){
    for (j in 2:ncol(dataframe_with_na)){
      df_amelia[i,j-1] <- a.out$imputations$imp1[i,j]+ a.out$imputations$imp2[i,j]+ a.out$imputations$imp3[i,j]+ a.out$imputations$imp4[i,j]+ a.out$imputations$imp5[i,j]
    }
  }
  df_amelia <- df_amelia / 5
  
  return (df_amelia)
}
###############################################################
########### Vergleich der Imputationsmethoden  ################
#### Es gibt jeweils fünf Simulationsrunden ###################

          #######  Aktienkurse  ########
          ##############################

### Analyse der Aktien bei einer Gapsize = 1 für verschiedene NA- Anteile ###
#####                                                                   #####

ggs1 <- matrix(0,nrow=10,ncol=7)
ggs1t <- matrix(0,nrow=10,ncol=7)

simulation_runs <- 5
for (seed in 1:simulation_runs){
  gs1 <- matrix(nrow=10,ncol=7)
  gs1t <- matrix(nrow=10,ncol=7)
  for (i in 1:10){
    df_mcar <- random_NA_values_with_seed (portfolio, i/100, 1, seed)
    gs1[i,1] <-  sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    gs1t[i,1] <- sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    start_time <- Sys.time()
    df_locf <- imputeTS::na_locf(df_mcar)
    end_time <- Sys.time()
    gs1t[i,2] <- as.numeric(end_time - start_time)
    gs1[i,2] <- calculate_MSE(portfolio, df_mcar, df_locf)
    start_time <- Sys.time()
    df_mean <- imputeTS::na_mean(df_mcar)
    end_time <- Sys.time()
    gs1t[i,3] <- as.numeric(end_time - start_time)
    gs1[i,3] <- calculate_MSE(portfolio, df_mcar, df_mean)
    start_time <- Sys.time()
    df_interpolate <- imputeTS::na_interpolation (df_mcar)
    end_time <- Sys.time()
    gs1t[i,4] <- as.numeric(end_time - start_time)
    gs1[i,4] <- calculate_MSE(portfolio, df_mcar, df_interpolate)
    start_time <- Sys.time()
    df_ma <- imputeTS::na_ma(df_mcar)
    end_time <- Sys.time()
    gs1t[i,5] <- as.numeric(end_time - start_time)
    gs1[i,5] <- calculate_MSE(portfolio, df_mcar, df_ma)
    start_time <- Sys.time()
    df_kalman <- imputeTS::na_kalman(df_mcar,model="StructTS",smooth=TRUE)
    end_time <- Sys.time()
    gs1t[i,6] <- as.numeric(end_time - start_time)
    gs1[i,6] <- calculate_MSE(portfolio, df_mcar, df_kalman)
    start_time <- Sys.time()
    df_amelia <- create_pooled_imp_df_amelia(df_mcar)
    end_time <- Sys.time()
    gs1t[i,7] <- as.numeric(end_time - start_time)
    gs1[i,7] <- calculate_MSE(portfolio[-c(1)], df_mcar[-c(1)], df_amelia[-c(1)])
  }
  ggs1 <- ggs1 + gs1
  ggs1t <- ggs1t + gs1t
}
gs1 <- ggs1/simulation_runs
gst1 <- ggs1t/simulation_runs
colnames(gs1) <- c("NA-Anteil in %","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")
colnames(gs1t) <- c("NA-Anteil in %","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")

# MSE der einzelnen Algorithmen
gs1

# Ausführungszeit der einzelnen Algorithmen
gs1t

# Visuelle Darstellung des MSE
plot(gs1[,1],gs1[,2], type = "b", col = "black", ylim=c(0,30), main="Vergleich versch. Imputationsverfahren bei einer Gapsize von 1", xlab="NA- Anteil in %", ylab="MSE")
lines(gs1[,1], gs1[,4], col = "blue", type = "b", lty = 2)
lines(gs1[,1], gs1[,5], col = "red", type = "b", lty = 3)
lines(gs1[,1], gs1[,6], col = "purple", type = "b", lty = 4)
lines(gs1[,1], gs1[,7], col = "brown", type = "b", lty = 5)
legend("topright", legend=c("Locf", "Interpolation","Moving Average","Kalman","Amelia"),
       col=c("black", "blue","red","purple","brown"), lty=1:5, cex=0.8)


### Analyse der Aktien bei einer Gapsize = 5 für verschiedene NA- Anteile ###
#####                                                                   #####

ggs5 <- matrix(0,nrow=10,ncol=7)
ggs5t <- matrix(0,nrow=10,ncol=7)

simulation_runs <- 5
for (seed in 1:simulation_runs){
  gs5 <- matrix(nrow=10,ncol=7)
  gs5t <- matrix(nrow=10,ncol=7)
  for (i in 1:10){
    df_mcar <- random_NA_values_with_seed(portfolio, i/200, 5,seed)
    gs5[i,1] <-  sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    gs5t[i,1] <- sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    start_time <- Sys.time()
    df_locf <- imputeTS::na_locf(df_mcar)
    end_time <- Sys.time()
    gs5t[i,2] <- as.numeric(end_time - start_time)
    gs5[i,2] <- calculate_MSE(portfolio, df_mcar, df_locf)
    start_time <- Sys.time()
    df_mean <- imputeTS::na_mean(df_mcar)
    end_time <- Sys.time()
    gs5t[i,3] <- as.numeric(end_time - start_time)
    gs5[i,3] <- calculate_MSE(portfolio, df_mcar, df_mean)
    start_time <- Sys.time()
    df_interpolate <- imputeTS::na_interpolation (df_mcar)
    end_time <- Sys.time()
    gs5t[i,4] <- as.numeric(end_time - start_time)
    gs5[i,4] <- calculate_MSE(portfolio, df_mcar, df_interpolate)
    start_time <- Sys.time()
    df_ma <- imputeTS::na_ma(df_mcar)
    end_time <- Sys.time()
    gs5t[i,5] <- as.numeric(end_time - start_time)
    gs5[i,5] <- calculate_MSE(portfolio, df_mcar, df_ma)
    start_time <- Sys.time()
    df_kalman <- imputeTS::na_kalman(df_mcar)
    end_time <- Sys.time()
    gs5t[i,6] <- as.numeric(end_time - start_time)
    gs5[i,6] <- calculate_MSE(portfolio, df_mcar, df_kalman)
    start_time <- Sys.time()
    df_amelia <- create_pooled_imp_df_amelia(df_mcar)
    end_time <- Sys.time()
    gs5t[i,7] <- as.numeric(end_time - start_time)
    gs5[i,7] <- calculate_MSE(portfolio[-c(1)], df_mcar[-c(1)], df_amelia[-c(1)])
  }
  ggs5 <- ggs5 + gs5
  ggs5t <- ggs5t + gs5t
}
gs5 <- ggs5/simulation_runs
gst5 <- ggs5t/simulation_runs
colnames(gs5) <- c("NA-Anteil in %","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")
colnames(gs5t) <- c("NA-Anteil in %","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")

# MSE der Verschiedenen Algorithmen für eine Gapsize von 5
gs5

# Ausführungszeit für eine Gapsize von 5
gst5

# Graphische Darstellung
plot(gs5[,1],gs5[,2], type = "b", lty = 1, col = "black", ylim=c(5,46), main="Vergleich versch. Imputationsverfahren bei einer Gapsize von 5", xlab="NA- Anteil in %", ylab="MSE")
lines(gs5[,1], gs5[,4], col = "blue", type = "b", lty = 2)
lines(gs5[,1], gs5[,5], col = "red", type = "b", lty = 3)
lines(gs5[,1], gs5[,6], col = "purple", type = "b", lty = 4)
lines(gs5[,1], gs5[,7], col = "brown", type = "b", lty = 5)
legend("topleft", legend=c("Locf", "Interpolation","Moving Average","Kalman","Amelia"),
       col=c("black", "blue","red","purple","brown"), lty=1:5, cex=1)


### Analyse der Aktienkurse bei einem niedrigen NA- Anteil bei verschiedenen Gapsizes ###
#####                                                                               #####

gna1 <- matrix(0,nrow=10,ncol=8)
gfreq <- 0.015

simulation_runs <- 5
for (seed in 1:simulation_runs){
  na1 <- matrix(nrow=10,ncol=8)
  freq <- gfreq
  for (i in 1:10){
    df_mcar <- random_NA_values_with_seed(portfolio,freq, i,seed)
    if(i>1){
      while(na1[1,1]<(sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100)){
        freq <- freq - 0.00003
        df_mcar <- random_NA_values_with_seed(portfolio, freq, i,seed)
      }
    }
    na1[i,1] <-  sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    na1[i,2] <- i
    df_locf <- imputeTS::na_locf(df_mcar)
    na1[i,3] <- calculate_MSE(portfolio, df_mcar, df_locf)
    df_mean <- imputeTS::na_mean(df_mcar)
    na1[i,4] <- calculate_MSE(portfolio, df_mcar, df_mean)
    df_interpolate <- imputeTS::na_interpolation (df_mcar)
    na1[i,5] <- calculate_MSE(portfolio, df_mcar, df_interpolate)
    df_ma <- imputeTS::na_ma(df_mcar)
    na1[i,6] <- calculate_MSE(portfolio, df_mcar, df_ma)
    df_kalman <- imputeTS::na_kalman(df_mcar)
    na1[i,7] <- calculate_MSE(portfolio, df_mcar, df_kalman)
    df_amelia <- create_pooled_imp_df_amelia(df_mcar)
    na1[i,8] <- calculate_MSE(portfolio[-c(1)], df_mcar[-c(1)], df_amelia[-c(1)])
  }
  gna1 <- gna1 + na1
}
na1 <- gna1/simulation_runs
colnames(na1) <- c("NA-Anteil in %", "Gapsize","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")

#  MSE der Verschiedenen Algorithmen bei einem niedrigen NA- Anteil
na1

# Visuelle Darstellung 
plot(na1[,2],na1[,3], type = "b", col = "black", ylim=c(0,37), main="Vergleich versch. Imputationsverfahren bei niedrigen NA- Anteil", xlab="Gapsize", ylab="MSE")
lines(na1[,2], na1[,5], col = "blue", type = "b", lty = 2)
lines(na1[,2], na1[,6], col = "red", type = "b", lty = 3)
lines(na1[,2], na1[,7], col = "purple", type = "b", lty = 4)
lines(na1[,2], na1[,8], col = "brown", type = "b", lty = 5)
legend("topleft", legend=c("Locf", "Interpolation","Moving Average","Kalman", "Amelia"),
       col=c("black", "blue","red","purple","brown"), lty=1:5, cex=0.8)


### Analyse der Aktienkurse bei einem hohen NA- Anteil bei verschiedenen Gapsizes ###
#####                                                                           #####

gna10 <- matrix(0,nrow=10,ncol=8)
gfreq <- 0.1

simulation_runs <- 5
for (seed in 1:simulation_runs){
  na10 <- matrix(nrow=10,ncol=8)
  freq <- gfreq
  for (i in 1:10){
    df_mcar <- random_NA_values_with_seed(portfolio,freq, i,seed)
    if(i>1){
      while(na10[1,1]<(sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100)){
        freq <- freq - 0.0002
        df_mcar <- random_NA_values_with_seed(portfolio, freq, i,seed)
      }
    }
    na10[i,1] <-  sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    na10[i,2] <- i
    df_locf <- imputeTS::na_locf(df_mcar)
    na10[i,3] <- calculate_MSE(portfolio, df_mcar, df_locf)
    df_mean <- imputeTS::na_mean(df_mcar)
    na10[i,4] <- calculate_MSE(portfolio, df_mcar, df_mean)
    df_interpolate <- imputeTS::na_interpolation (df_mcar)
    na10[i,5] <- calculate_MSE(portfolio, df_mcar, df_interpolate)
    df_ma <- imputeTS::na_ma(df_mcar)
    na10[i,6] <- calculate_MSE(portfolio, df_mcar, df_ma)
    df_kalman <- imputeTS::na_kalman(df_mcar)
    na10[i,7] <- calculate_MSE(portfolio, df_mcar, df_kalman)
    df_amelia <- create_pooled_imp_df_amelia(df_mcar)
    na10[i,8] <- calculate_MSE(portfolio[-c(1)], df_mcar[-c(1)], df_amelia[-c(1)])
  }
  gna10 <- gna10 + na10
}
na10 <- gna10/simulation_runs
colnames(na10) <- c("NA-Anteil in %", "Gapsize","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")

#  MSE der Verschiedenen Algorithmen bei einem hohen NA- Anteil
na10

# Visualle Darstellung
plot(na10[,2],na10[,3], type = "b", col = "black", ylim=c(0,40), main="Vergleich versch. Imputationsverfahren bei einer NA-Anteil von 18%", xlab="Gapsize", ylab="MSE")
lines(na10[,2], na10[,5], col = "blue", type = "b", lty = 2)
lines(na10[,2], na10[,6], col = "red", type = "b", lty = 3)
lines(na10[,2], na10[,7], col = "green", type = "l", lty = 4)
lines(na10[,2], na10[,8], col = "brown", type = "b", lty = 5)
legend("topleft", legend=c("Locf", "Interpolation","Moving Average","Kalman","Amelia"),
       col=c("black", "blue","red","purple","brown"), lty=1:5, cex=0.8)



############ LOg Renditen  #############################
########################################################


### Analyse der Log Rendite bei einer Gapsize= 1 ###
#####                                          #####

grgs1 <- matrix(0,nrow=10,ncol=7)
grgs1t <- matrix(0,nrow=10,ncol=7)

simulation_runs <- 5
for (seed in 1:simulation_runs){
  rgs1 <- matrix(nrow=10,ncol=7)
  rgs1t <- matrix(nrow=10,ncol=7)
  for (i in 1:10){
    df_mcar <- random_NA_values_with_seed(portfolio_rendite, i/100, 1,seed+15)
    rgs1[i,1] <-  sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    rgs1t[i,1] <- sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    start_time <- Sys.time()
    df_locf <- imputeTS::na_locf(df_mcar)
    end_time <- Sys.time()
    rgs1t[i,2] <- as.numeric(end_time - start_time)
    rgs1[i,2] <- calculate_MSE(portfolio_rendite, df_mcar, df_locf)
    start_time <- Sys.time()
    df_mean <- imputeTS::na_mean(df_mcar)
    end_time <- Sys.time()
    rgs1t[i,3] <- as.numeric(end_time - start_time)
    rgs1[i,3] <- calculate_MSE(portfolio_rendite, df_mcar, df_mean)
    start_time <- Sys.time()
    df_interpolate <- imputeTS::na_interpolation (df_mcar)
    end_time <- Sys.time()
    rgs1t[i,4] <- as.numeric(end_time - start_time)
    rgs1[i,4] <- calculate_MSE(portfolio_rendite, df_mcar, df_interpolate)
    start_time <- Sys.time()
    df_ma <- imputeTS::na_ma(df_mcar)
    end_time <- Sys.time()
    rgs1t[i,5] <- as.numeric(end_time - start_time)
    rgs1[i,5] <- calculate_MSE(portfolio_rendite, df_mcar, df_ma)
    start_time <- Sys.time()
    df_kalman <- imputeTS::na_kalman(df_mcar)
    end_time <- Sys.time()
    rgs1t[i,6] <- as.numeric(end_time - start_time)
    rgs1[i,6] <- calculate_MSE(portfolio_rendite, df_mcar, df_kalman)
    start_time <- Sys.time()
    df_amelia <- create_pooled_imp_df_amelia(df_mcar)
    end_time <- Sys.time()
    rgs1t[i,7] <- as.numeric(end_time - start_time)
    rgs1[i,7] <- calculate_MSE(portfolio_rendite[-c(1)], df_mcar[-c(1)], df_amelia[-502,-c(1)])
  }
  grgs1 <- grgs1 + rgs1
  grgs1t <- grgs1t + rgs1t
  
}
rgs1 <- grgs1/simulation_runs
rgs1t <-grgs1t/simulation_runs
colnames(rgs1) <- c("NA-Anteil in %","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")
colnames(rgs1t) <- c("NA-Anteil in %","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")

# MSE der verschiedenen Algorithmen bei deiner Gapsize von 1
rgs1

# Ausführungszeit der verschiedenen Algorithmen bei einer Gapsize von 1
grgs1t

# Visuelle Darstellung der verschiedenen Imputationsverfahren bei einer Gapsize von 1
plot(rgs1[,1],rgs1[,2], type = "b", col = "black", ylim=c(0.0001,0.0006), main="Vergleich versch. Imputationsverfahren bei einer Gapsize von 1", xlab="NA- Anteil in %", ylab="MSE")
lines(rgs1[,1], rgs1[,3], col = "darkcyan", type = "b", lty = 2)
lines(rgs1[,1], rgs1[,4], col = "blue", type = "b", lty = 3)
lines(rgs1[,1], rgs1[,5], col = "red", type = "b", lty = 4)
lines(rgs1[,1], rgs1[,6], col = "purple", type = "b", lty = 5)
lines(rgs1[,1], rgs1[,7], col = "brown", type = "b", lty = 6)
legend("topleft", legend=c("Locf","Mean" , "Interpolation","Moving Average","Kalman","Amelia"),
       col=c("black","darkcyan", "blue","red","purple","brown"), lty=1:6, cex=0.8)


### Analyse der Log Rendite bei einer Gapsize= 5 ###
#####                                          #####

grgs5 <- matrix(0,nrow=10,ncol=7)
grgs5t <- matrix(0,nrow=10,ncol=7)

simulation_runs <- 5
for (seed in 1:simulation_runs){
  rgs5 <- matrix(nrow=10,ncol=7)
  rgs5t <- matrix(nrow=10,ncol=7)
  for (i in 1:10){
    df_mcar <- random_NA_values_with_seed(portfolio_rendite, i/200, 5,seed)
    rgs5[i,1] <-  sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    rgs5t[i,1] <- sum(is.na(df_mcar))/(ncol(df_mcar)*nrow(df_mcar))*100
    start_time <- Sys.time()
    df_locf <- imputeTS::na_locf(df_mcar)
    end_time <- Sys.time()
    rgs5t[i,2] <- as.numeric(end_time - start_time)
    rgs5[i,2] <- calculate_MSE(portfolio_rendite, df_mcar, df_locf)
    start_time <- Sys.time()
    df_mean <- imputeTS::na_mean(df_mcar)
    end_time <- Sys.time()
    rgs5t[i,3] <- as.numeric(end_time - start_time)
    rgs5[i,3] <- calculate_MSE(portfolio_rendite, df_mcar, df_mean)
    start_time <- Sys.time()
    df_interpolate <- imputeTS::na_interpolation (df_mcar)
    end_time <- Sys.time()
    rgs5t[i,4] <- as.numeric(end_time - start_time)
    rgs5[i,4] <- calculate_MSE(portfolio_rendite, df_mcar, df_interpolate)
    start_time <- Sys.time()
    df_ma <- imputeTS::na_ma(df_mcar)
    end_time <- Sys.time()
    rgs5t[i,5] <- as.numeric(end_time - start_time)
    rgs5[i,5] <- calculate_MSE(portfolio_rendite, df_mcar, df_ma)
    start_time <- Sys.time()
    df_kalman <- imputeTS::na_kalman(df_mcar)
    end_time <- Sys.time()
    rgs5t[i,6] <- as.numeric(end_time - start_time)
    rgs5[i,6] <- calculate_MSE(portfolio_rendite, df_mcar, df_kalman)
    start_time <- Sys.time()
    df_amelia <- create_pooled_imp_df_amelia(df_mcar)
    end_time <- Sys.time()
    rgs5t[i,7] <- as.numeric(end_time - start_time)
    rgs5[i,7] <- calculate_MSE(portfolio_rendite[-c(1)], df_mcar[-c(1)], df_amelia[-502,-c(1)])
  }
  grgs5 <- grgs5 + rgs5
  grgs5t <- grgs5t + rgs5t
  
}
rgs5 <- grgs5/simulation_runs
rgs5t <-grgs5t/simulation_runs
colnames(rgs5) <- c("NA-Anteil in %","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")
colnames(rgs5t) <- c("NA-Anteil in %","Locf","Mean","Interpolation","Moving Average","Kalman","Amelia")

# MSE der verschiedenen Algorithmen bei deiner Gapsize von 5
rgs5

# Visuelle Darstellung der verschiedenen Imputationsverfahren bei einer Gapsize von 5
plot(rgs5[,1],rgs5[,2], type = "b", col = "black", ylim=c(0.0001,0.0006), main="Vergleich versch. Imputationsverfahren bei einer Gapsize von 5", xlab="NA- Anteil in %", ylab="MSE")
lines(rgs5[,1], rgs5[,3], col = "darkcyan", type = "b", lty = 2)
lines(rgs5[,1], rgs5[,4], col = "blue", type = "b", lty = 3)
lines(rgs5[,1], rgs5[,5], col = "red", type = "b", lty = 4)
lines(rgs5[,1], rgs5[,6], col = "purple", type = "b", lty =5)
lines(rgs5[,1], rgs5[,7], col = "brown", type = "b", lty = 6)
legend("topleft", legend=c("Locf","Mean", "Interpolation","Moving Average","Kalman","Amelia"),
       col=c("black","darkcyan", "blue","red","purple","brown"), lty=1:6, cex=0.8)

