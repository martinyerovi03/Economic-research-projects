# FLAR Essay Project - R Script
# Author: Martín Yerovi
# Purpose: Construct institutional coordination indices using network analysis
# and estimate their relationship with post-pandemic GDP growth in Latin America.

# setwd("path/to/project/folder")

library(tidyverse)
library(data.table)
library(readxl)
library(writexl)

base <- read_excel("Base1.xlsx", sheet = "Base")

# Compute the indices ----------------------------------------------- --
# Convert to "long"
base_L <- base %>% transpose() %>% as.data.frame() 
base_L <- base_L[-1,]
for(i in 1:dim(base_L)[2]){ #number format
  base_L[,i] <- as.numeric(unlist(base_L[,i]))
}
colnames(base_L) <- base$Pais
base_L$Q <- colnames(base)[-1]
base_L$Cat_Q <- substr(base_L$Q, 4,6)

# Aggregate (sum)
data <- aggregate(data = base_L[,-13], .~ Cat_Q, FUN = sum,  
                  na.action = na.pass, na.rm = T)

# Rounding
for(i in 2:dim(data)[2]){ #number format
  data[,i] <- round(data[,i], digits = 2)
}

# NaN's result from NA/NA
for(i in 2:dim(data)[2]){
  data[,i] <- ifelse(is.nan(data[,i]), NA, data[,i])
}

write_xlsx(data, path = "indices.xlsx")


# Adjacency matrices -------------------------------------------------------

# Dicctionary
institutions <- c("MF", "NC", "PRES", "MSD", "SSI", "TA", "FCE")
pairs <- data.frame(
  Code = paste0("P", sprintf("%02d", 1:21)),
  Inst1 = c(
    "MF", "MF", "MF", "MF", "MF", "MF",
    "NC", "NC", "NC", "NC", "NC",
    "PRES", "PRES", "PRES", "PRES",
    "MSD", "MSD", "MSD", 
    "SSI", "SSI", 
    "TA"
  ),
  Inst2 = c(
    "NC", "PRES", "MSD", "SSI", "TA", "FCE",
    "PRES", "MSD", "SSI", "TA", "FCE",
    "MSD", "SSI", "TA", "FCE",
    "SSI", "TA", "FCE", 
    "TA", "FCE", 
    "FCE"
  ),
  stringsAsFactors = FALSE
)


countries <- colnames(data)[-1]
matrices <- list()
for (country in countries) {
  mat <- matrix(0, nrow = length(institutions), ncol = length(institutions))
  rownames(mat) <- institutions
  colnames(mat) <- institutions
  
  for (i in 1:nrow(pairs)) {
    inst1 <- pairs$Inst1[i]
    inst2 <- pairs$Inst2[i]
    code  <- pairs$Code[i] 
    weight <- data[data$Cat_Q == code, country]
    mat[inst1, inst2] <- weight
    mat[inst2, inst1] <- weight
  }
 
  matrices[[country]] <- mat
}

# Se quita el nodo FCE si este no tiene ninguna conección
for (country in names(matrices)) {
  
  mat <- matrices[[country]]
  
  # Add rows and columns FCE
  if ("FCE" %in% rownames(mat)) {
    suma_fce <- sum(mat["FCE", ]) + sum(mat[, "FCE"])
    
    if (suma_fce == 0) {
      #Delete row and column "FCE" if there is no conections
      mat <- mat[rownames(mat) != "FCE", colnames(mat) != "FCE"]
    }
  }
  
  matrices[[country]] <- mat
}

#Export Excel  (Respaldo)
matrices_df <- lapply(matrices, function(x) {
  as.data.frame(x)
})

rm(base, base_L, mat, inst1, inst2, country, code, i)

write_xlsx(matrices_df, path = "matrices_paises.xlsx")


# Index --------------------------------------------------------------

# Matrix of reference
mat <- c(0,5,5,5,5,5,5,
         5,0,5,5,5,5,5,
         5,5,0,5,5,5,5,
         5,5,5,0,5,5,5,
         5,5,5,5,0,5,5,
         5,5,5,5,5,0,5,
         5,5,5,5,5,5,0)
mat_R <- matrix(data=mat, ncol=7, nrow=7)
rownames(mat_R) <- institutions
colnames(mat_R) <- institutions

#Adjacency (spectral) distance
d <- vector()
for(i in 1:length(matrices)){
  k = 0
  for(n in 1:7){
    l = 0
    for(m in 1:7){
      l = (matrices[[i]][m,n] - mat_R[m,n])^2
      k = k + l
    }
  }
  s = sqrt(k)
  d <- append(d, s)
  rm(k,l)
}
distances <- data.frame(Country = names(matrices), dist = d)
distances$Index <- 100*(distances$dist + 1)^{-1}

#Gromov-Hausdorff distance
library(gromovlab)

matrices_GH <- list()     #Calculate differently the adjacency matrices
for (country in countries) {
  mat_GH <- matrix(0, nrow = length(institutions), ncol = length(institutions))
  rownames(mat_GH) <- institutions
  colnames(mat_GH) <- institutions
  
  for (i in 1:nrow(pairs)) {
    inst1 <- pairs$Inst1[i]
    inst2 <- pairs$Inst2[i]
    code  <- pairs$Code[i] 
    weight <- data[data$Cat_Q == code, country]
    mat_GH[inst1, inst2] <- ifelse(weight>0, weight, 0.1)
    mat_GH[inst2, inst1] <- ifelse(weight>0, weight, 0.1)
  }
  
  matrices_GH[[country]] <- mat_GH
}

d <- vector()
for(i in 1:length(matrices)){
  k <- gromovdist(mat_R, matrices_GH[[i]], type = "l1", p = NULL)
  d <- append(d, k)
  rm(k)
}

distances_GH <- data.frame(Country = names(matrices), d_GH = d)
distances_GH$Index <- 100*(distances_GH$d_GH + 1)^{-1}

rm(mat_R, d, weight, i)

write_xlsx(distances, "Index.xlsx")
write_xlsx(distances_GH, "Index_GH.xlsx")

#FLAR Data ----------------------------------------------------------
flar <- read_excel("SIE.xlsx")
flar <- flar[, -c(14:21)]
vars_flar <- unique(flar$indicatorNameENShort)

vars <- c("Nominal Gross Domestic Product, National Currency  annual variation")

base_flar <- flar[flar$indicatorNameENShort %in% vars & flar$refAreaName %in% countries,
                  -c(1,2,5,7,8,9,10,12)]
base_flar$Year <- as.numeric(substr(base_flar$timePeriod, 1, 4))
base_flar <- base_flar[,-4]

# Aggregate (mean)
data_flar <- aggregate(data = base_flar,  obsValue ~., FUN = mean,  
                  na.action = na.pass, na.rm = T)
# Formatting
data_flar <- data_flar[data_flar$Year %in% c(2015:2024), ]
data_flar <- data_flar %>% spread(indicatorNameENShort, obsValue)
colnames(data_flar) <- c("ID", "Country", "Year", "GDP_v")

rm(base_flar, flar, vars_flar, vars)

write_xlsx(data_flar, "data_flar.xlsx")

# Full database ---------------------------------------------------------------
data_flar1 <- data_flar
data_flar1$covid <- ifelse(data_flar1$Year <= 2019, 0, 1)
data_Reg <- merge(data_flar1, distances, by.x = "Country", by.y = "Country")
#data_Reg[42, c(4,5)] <- c(NA, NA)  # Anomalous obs.

write_xlsx(data_Reg, "data_Reg.xlsx")

# Summary & Correlations -------------------------------------------------------

# Summaru statistics
data_Reg_preC <- data_Reg[data_Reg$covid == 0,]
data_Reg_postC <- data_Reg[data_Reg$covid == 1,]

#install.packages("stargazer")
library(stargazer)
stargazer(data_Reg_preC[,c(1,4,7)], summary = T, type = "latex", out = "sum_pre.tex")
stargazer(data_Reg_postC[,c(1,4,7)], summary = T, type = "latex", out = "sum_post.tex")

# Summary statistics by country
data_means_by_country <- aggregate(data = data_Reg[, -c(2, 3, 6, 7)],  .~ Country + covid, FUN = mean,  
                                   na.action = na.pass, na.rm = T)
data_means_by_country <- spread(data_means_by_country, covid, GDP_v)
colnames(data_means_by_country) <- c("Country", "vGDP_PreCovid", "vGDP_PostCovid")

data_sds_by_country <- aggregate(data = data_Reg[, -c(2, 3, 6, 7)],  .~ Country + covid, FUN = sd,  
                                   na.action = na.pass, na.rm = T)
data_sds_by_country <- spread(data_sds_by_country, covid, GDP_v)
colnames(data_sds_by_country) <- c("Country", "sdGDP_PreCovid", "sdGDP_PostCovid")

data_sum_by_country <- merge(data_means_by_country, data_sds_by_country)
data_sum_by_country <- merge(data_sum_by_country, distances[, c(1,3)])

write_xlsx(data_sum_by_country, "data_sum_by_country.xlsx")

rm(data_sds_by_country, data_means_by_country)

# Preliminary regressions
c1 <- lm(data = data_Reg, GDP_v ~ Index)
c2 <- lm(data = data_Reg, GDP_v ~ Index + covid)
c3 <- lm(data = data_Reg, GDP_v ~ Index*covid)

stargazer(c1, c2, c3, type = "text")
stargazer(c1, c2, c3, type = "latex", out = "regs.tex")

# Full Regression Data  -------------------------------------------------------

m1 <- lm(data = data_Reg, GDP_v ~ Index + factor(Country)) 
m2 <- lm(data = data_Reg, GDP_v ~ Index + factor(Country) + factor(Year)) 
m3 <- lm(data = data_Reg, GDP_v ~ Index*covid + factor(Country))
m4 <- lm(data = data_Reg, GDP_v ~ Index*covid + factor(Country) + factor(Year))

#Output tables
stargazer(m1, m2, m3, m4, type = "text")
stargazer(m1, m2, m3, m4, type = "latex", out = "regs_fe.tex")


#Networks graphics --------------------------------------------------------------

#ARGENTINA
library(igraph)

g <- graph_from_adjacency_matrix(matrices[["Argentina"]],
                                 mode = "undirected", weighted = TRUE, diag = FALSE)
pesos <- E(g)$weight

centralidad <- eigen_centrality(g)$vector  
tamaño_nodos <- centralidad * 30  

layout_fr <- layout_with_fr(g, niter = 5000)
layout_fr <- layout_fr * 1.5

colores_centralidad <- colorRampPalette(c("orange", "tomato"))(length(centralidad))
colores_nodos <- colores_centralidad[rank(centralidad)]

par(mar = c(0.9, 0.9, 1.5, 0.9))
plot(g,
     layout = layout_fr,
     vertex.color = colores_nodos,  
     vertex.size = tamaño_nodos,
     vertex.frame.color = "white",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     edge.width = 0.3,  
     edge.color = "darkgrey",
     edge.label = pesos,
     edge.label.family = "sans",
     edge.label.cex = 0.8,
     edge.label.color = "blue",
     edge.label.font = 2, 
     main = NULL
)
title("Argentina", 
      family = "sans", 
      cex.main = 1.5,       
      col.main = "black")


#BOLIVIA
library(igraph)

g <- graph_from_adjacency_matrix(matrices[["Bolivia"]],
                                 mode = "undirected", weighted = TRUE, diag = FALSE)
pesos <- E(g)$weight

centralidad <- eigen_centrality(g)$vector  
tamaño_nodos <- centralidad * 30 

layout_fr <- layout_with_fr(g, niter = 5000)
layout_fr <- layout_fr * 1.5

colores_centralidad <- colorRampPalette(c("orange", "tomato"))(length(centralidad))
colores_nodos <- colores_centralidad[rank(centralidad)]

par(mar = c(0.9, 0.9, 1.5, 0.9))

plot(g,
     layout = layout_fr,
     vertex.color = colores_nodos,  
     vertex.size = tamaño_nodos,
     vertex.frame.color = "white",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     edge.width = 0.3, 
     edge.color = "darkgrey",
     edge.label = pesos,
     edge.label.family = "sans",
     edge.label.cex = 0.8,
     edge.label.color = "blue",
     edge.label.font = 2, 
     main = NULL
)

title("Bolivia", 
      family = "sans", 
      cex.main = 1.5,       
      col.main = "black")


#BRAZIL
library(igraph)

g <- graph_from_adjacency_matrix(matrices[["Brazil"]],
                                 mode = "undirected", weighted = TRUE, diag = FALSE)
pesos <- E(g)$weight

centralidad <- eigen_centrality(g)$vector
tamaño_nodos <- centralidad * 30 

layout_fr <- layout_with_fr(g, niter = 5000)
layout_fr <- layout_fr * 1.5

colores_centralidad <- colorRampPalette(c("orange", "tomato"))(length(centralidad))
colores_nodos <- colores_centralidad[rank(centralidad)]

par(mar = c(0.9, 0.9, 1.5, 0.9))

plot(g,
     layout = layout_fr,
     vertex.color = colores_nodos, 
     vertex.size = tamaño_nodos,
     vertex.frame.color = "white",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     edge.width = 0.3, 
     edge.color = "darkgrey",
     edge.label = pesos,
     edge.label.family = "sans",
     edge.label.cex = 0.8,
     edge.label.color = "blue",
     edge.label.font = 2,  
     main = NULL
)
title("Brazil", 
      family = "sans", 
      cex.main = 1.5,       
      col.main = "black")



#CHILE
library(igraph)

g <- graph_from_adjacency_matrix(matrices[["Chile"]],
                                 mode = "undirected", weighted = TRUE, diag = FALSE)
pesos <- E(g)$weight

centralidad <- eigen_centrality(g)$vector
tamaño_nodos <- centralidad * 30 

layout_fr <- layout_with_fr(g, niter = 5000)
layout_fr <- layout_fr * 1.5

colores_centralidad <- colorRampPalette(c("orange", "tomato"))(length(centralidad))
colores_nodos <- colores_centralidad[rank(centralidad)]

par(mar = c(0.9, 0.9, 1.5, 0.9))

plot(g,
     layout = layout_fr,
     vertex.color = colores_nodos, 
     vertex.size = tamaño_nodos,
     vertex.frame.color = "white",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     edge.width = 0.3, 
     edge.color = "darkgrey",
     edge.label = pesos,
     edge.label.family = "sans",
     edge.label.cex = 0.8,
     edge.label.color = "blue",
     edge.label.font = 2,  
     main = NULL
)
title("Chile", 
      family = "sans", 
      cex.main = 1.5,       
      col.main = "black")


#COLOMBIA
library(igraph)

g <- graph_from_adjacency_matrix(matrices[["Colombia"]],
                                 mode = "undirected", weighted = TRUE, diag = FALSE)
pesos <- E(g)$weight

centralidad <- eigen_centrality(g)$vector
tamaño_nodos <- centralidad * 30 

layout_fr <- layout_with_fr(g, niter = 5000)
layout_fr <- layout_fr * 1.5

colores_centralidad <- colorRampPalette(c("orange", "tomato"))(length(centralidad))
colores_nodos <- colores_centralidad[rank(centralidad)]

par(mar = c(0.9, 0.9, 1.5, 0.9))

plot(g,
     layout = layout_fr,
     vertex.color = colores_nodos, 
     vertex.size = tamaño_nodos,
     vertex.frame.color = "white",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     edge.width = 0.3, 
     edge.color = "darkgrey",
     edge.label = pesos,
     edge.label.family = "sans",
     edge.label.cex = 0.8,
     edge.label.color = "blue",
     edge.label.font = 2,  
     main = NULL
)
title("Colombia", 
      family = "sans", 
      cex.main = 1.5,       
      col.main = "black")


#COSTA RICA
library(igraph)

g <- graph_from_adjacency_matrix(matrices[["Costa Rica"]],
                                 mode = "undirected", weighted = TRUE, diag = FALSE)
pesos <- E(g)$weight

centralidad <- eigen_centrality(g)$vector
tamaño_nodos <- centralidad * 30 

layout_fr <- layout_with_fr(g, niter = 5000)
layout_fr <- layout_fr * 1.5

colores_centralidad <- colorRampPalette(c("orange", "tomato"))(length(centralidad))
colores_nodos <- colores_centralidad[rank(centralidad)]

par(mar = c(0.9, 0.9, 1.5, 0.9))

plot(g,
     layout = layout_fr,
     vertex.color = colores_nodos, 
     vertex.size = tamaño_nodos,
     vertex.frame.color = "white",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     edge.width = 0.3, 
     edge.color = "darkgrey",
     edge.label = pesos,
     edge.label.family = "sans",
     edge.label.cex = 0.8,
     edge.label.color = "blue",
     edge.label.font = 2,  
     main = NULL
)
title("Costa Rica", 
      family = "sans", 
      cex.main = 1.5,       
      col.main = "black")


#PERÚ
library(igraph)

g <- graph_from_adjacency_matrix(matrices[["Peru"]],
                                 mode = "undirected", weighted = TRUE, diag = FALSE)
pesos <- E(g)$weight

centralidad <- eigen_centrality(g)$vector
tamaño_nodos <- centralidad * 30 

layout_fr <- layout_with_fr(g, niter = 5000)
layout_fr <- layout_fr * 1.5

colores_centralidad <- colorRampPalette(c("orange", "tomato"))(length(centralidad))
colores_nodos <- colores_centralidad[rank(centralidad)]

par(mar = c(0.9, 0.9, 1.5, 0.9))

plot(g,
     layout = layout_fr,
     vertex.color = colores_nodos, 
     vertex.size = tamaño_nodos,
     vertex.frame.color = "white",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     edge.width = 0.3, 
     edge.color = "darkgrey",
     edge.label = pesos,
     edge.label.family = "sans",
     edge.label.cex = 0.8,
     edge.label.color = "blue",
     edge.label.font = 2,  
     main = NULL
)
title("Perú", 
      family = "sans", 
      cex.main = 1.5,       
      col.main = "black")


#URUGUAY
library(igraph)

g <- graph_from_adjacency_matrix(matrices[["Uruguay"]],
                                 mode = "undirected", weighted = TRUE, diag = FALSE)
pesos <- E(g)$weight

centralidad <- eigen_centrality(g)$vector
tamaño_nodos <- centralidad * 30 

layout_fr <- layout_with_fr(g, niter = 5000)
layout_fr <- layout_fr * 1.5

colores_centralidad <- colorRampPalette(c("orange", "tomato"))(length(centralidad))
colores_nodos <- colores_centralidad[rank(centralidad)]

par(mar = c(0.9, 0.9, 1.5, 0.9))

plot(g,
     layout = layout_fr,
     vertex.color = colores_nodos, 
     vertex.size = tamaño_nodos,
     vertex.frame.color = "white",
     vertex.label = V(g)$name,
     vertex.label.cex = 1,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     edge.width = 0.3, 
     edge.color = "darkgrey",
     edge.label = pesos,
     edge.label.family = "sans",
     edge.label.cex = 0.8,
     edge.label.color = "blue",
     edge.label.font = 2,  
     main = NULL
)
title("Uruguay", 
      family = "sans", 
      cex.main = 1.5,       
      col.main = "black")




#METRICS-------------------

#Per Institution
lista_resultados <- list()
for (pais in names(matrices)) {
  g <- graph_from_adjacency_matrix(matrices[[pais]],
                                   mode = "undirected", weighted = TRUE, diag = FALSE)
  
  degree_c <- degree(g)
  eigen_c <- eigen_centrality(g)$vector
  
  g_inv <- g
  E(g_inv)$weight <- 1 / E(g)$weight
  betweenness_c <- betweenness(g_inv, weights = E(g_inv)$weight)
  
  df <- data.frame(
    Pais = pais,
    Institucion = V(g)$name,
    Degree = degree_c,
    Betweenness = round(betweenness_c, 2),
    Eigenvector = round(eigen_c, 3)
  )
  
  lista_resultados[[pais]] <- df
}

tabla_final <- do.call(rbind, lista_resultados)

print(tabla_final)

library(writexl)
write_xlsx(tabla_final, path = "Metrics_centralities.xlsx")


#Per country
library(igraph)
network_metrics <- data.frame()
for (country in names(matrices)) {
  g <- graph_from_adjacency_matrix(matrices[[country]],
                                   mode = "undirected", weighted = TRUE, diag = FALSE)
  
  density <- edge_density(g)
  avg_degree <- mean(degree(g))
  avg_path_length <- average.path.length(g)
  diameter_value <- diameter(g)
  clustering <- transitivity(g, type = "average")
  
  network_metrics <- rbind(network_metrics, data.frame(
    Country = country,
    Density = round(density, 3),
    Average_Degree = round(avg_degree, 2),
    Average_Path_Length = round(avg_path_length, 2),
    Diameter = diameter_value,
    Clustering_Coefficient = round(clustering, 3)
  ))
}
print(network_metrics)

library(writexl)
write_xlsx(network_metrics, "Metrics_networks.xlsx")

