# Trabalho de Estatística e Predição
# Filipe Ribeiro Rocha
# Victor Luiz de Souza

### 1. Instalar Libs
install.packages(c("ggplot2", "dplyr", "corrplot", "car", "caret"))
library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
library(caret)

### 2. importar e tratar os dados

base = read.csv("coleta_lagoa_peri.csv")

  # converter virgula por ponto
base = base %>%
  mutate(
    Temp. = as.numeric(gsub(",", ".", Temp.)),
    Cond. = as.numeric(gsub(",", ".", Cond.)),
    Oxigenio = as.numeric(gsub(",", ".", Oxigenio)),
    Profund. = as.numeric(gsub(",", ".", Profund.)),
    Vel = as.numeric(gsub(",", ".", Vel)),
    Rainfall = as.numeric(gsub(",", ".", Rainfall))
  )

  # adiciona coluna de estação unica 
base <- base %>%
  mutate(
    EstaçãoUnica = case_when(
      Estação %in% c("S1", "S2") ~ "SU",
      Estação %in% c("W1", "W2") ~ "WU",
      Estação %in% c("A1", "A2") ~ "AU",
      Estação %in% c("Sp1", "Sp2") ~ "SpU",
      TRUE ~ Estação  # mantem estação com os valores originais
    )
  )

  # Converte data em datetime
base$Data <- as.Date(base$Data, format = "%m/%d/%Y") 

str(base)
summary(base)


### 3. analise univariada

  # histograma para as variaveis quantitativas
ggplot(base, aes(x = Temp.)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Temperatura", x = "Teratura (C)", y = "Frequência")

ggplot(base, aes(x = Cond.)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Condutividade", x = "Condutividade (µS/cm)", y = "Frequência")

ggplot(base, aes(x = Oxigenio)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Oxigenio", x = "Oxigenio (medida)", y = "Frequência")

ggplot(base, aes(x = Profund.)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Profundidade", x = "Profundidade (m)", y = "Frequência")

ggplot(base, aes(x = Vel)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Velocidade", x = "Velocidade (m/s)", y = "Frequência")

ggplot(base, aes(x = Rainfall)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Pluviosidade", x = "Velocidade (mm)", y = "Frequência")


  # grafico de barras para as qualitativas

ggplot(base, aes(x = Estação)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Frequência de Estações", x = "Estação", y = "Frequência") +
  theme_minimal()

ggplot(base, aes(x = Ponto)) +
  geom_bar(fill = "orange") +
  labs(title = "Frequência por Ponto", x = "Ponto", y = "Frequência") +
  theme_minimal()

# Gráfico de barras para a variável "EstaçãoUnica" ordenado por frequência

# Ordenar a variável "EstaçãoUnica" pela frequência
freq_estacao <- table(base$EstaçãoUnica)
levels_ordered <- names(sort(freq_estacao, decreasing = TRUE))
base$EstaçãoUnica <- factor(base$EstaçãoUnica, levels = levels_ordered)

ggplot(base, aes(x = EstaçãoUnica)) +
  geom_bar(fill = "green") +
  labs(title = "Frequência por Estação Unica (Ordenado)", x = "EstaçãoUnica", y = "Frequência") +
  theme_minimal()


ggplot(base, aes(y = Cond.)) + 
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot da Condutividade", y = "Condutividade (µS/cm)")
summary(base$Cond.)
sd(base$Cond., na.rm = TRUE)

ggplot(base, aes(x = Temp., y = Cond.)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Condutividade vs. Temperatura", x = "Temperatura (°C)", y = "Condutividade (µS/cm)")

r = cor(Temp., Cond.)
cor.test(base$Temp., base$Cond.)
ggplot(base, aes(x = Cond.)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Condutividade", x = "Condutividade (µS/cm)", y = "Frequência")

ggplot(base, aes(x = Estação, y = Cond.)) + 
  geom_boxplot(fill = "lightblue") +
  labs(title = "Condutividade por Estação", x = "Estação", y = "Condutividade (µS/cm)")


ggplot(base, aes(x = Ponto, y = Cond.)) + 
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Condutividade por Ponto de Coleta", x = "Ponto de Coleta", y = "Condutividade (µS/cm)")

conductivity_over_time <- base %>%
  group_by(Data) %>%
  summarise(Condutividade_Media = mean(Cond., na.rm = TRUE))

ggplot(conductivity_over_time, aes(x = Data, y = Condutividade_Media)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Variação da Condutividade ao Longo do Tempo",
    x = "Data",
    y = "Condutividade Média (µS/cm)"
  ) +
  theme_minimal()

ggplot(base, aes(x = `Oxigenio`, y = `Cond.`)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "Oxigênio vs Condutividade", x = "Oxigênio", y = "Condutividade (µS/cm)")

# Analise bivariada
cor(base[, c("Temp.", "Cond.", "Oxigenio")]) para entender relações lineares.

# Correlação
  # Matriz gráfica

numeric_data <- base[, sapply(base, is.numeric)]  # Selecionar apenas variáveis numéricas
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle")

  # Matriz numérica
cor_matrix = cor(base %>% select(Temp., Cond., Oxigenio, Profund., Vel, Rainfall))
print(cor_matrix)


## Sandbox
ggplot(base, aes(x = Temp., y = Cond.)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ EstaçãoUnica) +
  labs(title = "Condutividade vs Temperatura por Estação Única", x = "Temperatura (°C)", y = "Condutividade (µS/cm)") +
  theme_minimal()

ggplot(base, aes(x = Oxigenio, y = Cond.)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ EstaçãoUnica) +
  labs(title = "Condutividade vs Oxigenio por Estação Única", x = "Oxigenio", y = "Condutividade (µS/cm)") +
  theme_minimal()



# Matriz de correlação para uma estação única 
stations <- unique(base$EstaçãoUnica)
filtered_data <- base %>% filter(EstaçãoUnica == "AU")
numeric_data <- filtered_data[, sapply(filtered_data, is.numeric)]  # Selecionar apenas variáveis numéricas
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle", main = paste("Estação:", "AU"))


## Matriz gráfica de correlação filtrada por ponto
filtered_data <- base %>% filter(Ponto == "P4")
numeric_data <- filtered_data[, sapply(filtered_data, is.numeric)]  # Selecionar apenas variáveis numéricas
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle", main = paste("Ponto:", "P4"))


## Analise inicial para ver se encontrariamos um resultado diferente para cada ponto de coleta

#Tentativa 1: 
# Calcular a média de condutividade por ponto
media_cond <- base %>%
  group_by(Ponto) %>%
  summarise(Media_Condutividade = mean(Cond., na.rm = TRUE))

# Gráfico de Barras
ggplot(media_cond, aes(x = Ponto, y = Media_Condutividade)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Média da Condutividade por Ponto de Coleta", x = "Ponto de Coleta", y = "Média da Condutividade (µS/cm)") +
  theme_minimal()

#Tentativa 2:
# Boxplot para Condutividade por Ponto de Coleta
ggplot(base, aes(x = Ponto, y = Cond.)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Condutividade por Ponto de Coleta", x = "Ponto de Coleta", y = "Condutividade (µS/cm)") +
  theme_minimal()

## Grafico de linha
# Calcular a média da condutividade por ponto
media_cond <- base %>%
  group_by(Ponto) %>%
  summarise(Media_Condutividade = mean(Cond., na.rm = TRUE))

# Gráfico de linha para média de condutividade por ponto
ggplot(media_cond, aes(x = Ponto, y = Media_Condutividade, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Média da Condutividade por Ponto de Coleta", x = "Ponto de Coleta", y = "Média da Condutividade (µS/cm)") +
  theme_minimal()

# Aplicar o teste ANOVA
anova_result <- aov(Cond. ~ Ponto, data = base)

# Resumo dos resultados
summary(anova_result)

## Analise inicial para ver se encontrariamos um resultado diferente para cada ponto de coleta
# Calcular a média da condutividade por estacao unica
media_cond <- base %>%
  group_by(EstaçãoUnica) %>%
  summarise(Media_Condutividade = mean(Cond., na.rm = TRUE))

# Gráfico de linha para média de condutividade por ponto
ggplot(media_cond, aes(x = EstaçãoUnica, y = Media_Condutividade, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Média da Condutividade por Estação Unica", x = "Estação Unica", y = "Média da Condutividade (µS/cm)") +
  theme_minimal()


conductivity_over_time <- base %>%
  group_by(Data) %>%
  summarise(Condutividade_Media = mean(Cond., na.rm = TRUE))

ggplot(conductivity_over_time, aes(x = Data, y = Condutividade_Media)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Variação da Condutividade ao Longo do Tempo",
    x = "Data",
    y = "Condutividade Média (µS/cm)"
  ) +
  theme_minimal()
base <- base %>%
  filter(!Cond. %in% c(49.7, 81.2, 78.7, 78.5, 73.8))

ggplot(base, aes(x = Data, y = Cond., color = Ponto)) +
  geom_line(size = 0.5) +   # Linhas coloridas por ponto
  geom_point(size = 0.5) +  # Pontos coloridos para cada linha
  labs(
    title = "Condutividade ao Longo do Tempo por Ponto de Coleta",
    x = "Data",
    y = "Condutividade (µS/cm)",
    color = "Ponto de Coleta"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Coloca a legenda na parte inferior


########### testes modelo
########### teste A
# Instalar e carregar o pacote forecast
install.packages("forecast")
library(forecast)

# Selecionar dados para um ponto específico (exemplo: P1)
dados_ponto <- base %>% filter(Ponto == "P1") %>% arrange(Data)

# Criar uma série temporal
serie_temporal <- ts(dados_ponto$Cond., start = c(as.numeric(format(min(dados_ponto$Data), "%Y")),
                                                  as.numeric(format(min(dados_ponto$Data), "%j"))), frequency = 365)

# Ajustar o modelo ARIMA
modelo_arima <- auto.arima(serie_temporal)

# Prever valores futuros
previsao <- forecast(modelo_arima, h = 30)
plot(previsao)

############## teste B
install.packages(c("forecast"))
library(forecast)

# Criar uma série temporal
serie_temporal <- ts(base$Cond., start = c(as.numeric(format(min(base$Data), "%Y")), as.numeric(format(min(base$Data), "%j"))), frequency = 365)

# Ajustar um modelo ARIMA
xreg_data <- as.matrix(base[, c("Oxigenio", "Rainfall")])
modelo_exog <- Arima(serie_temporal, xreg = xreg_data)
summary(modelo_exog)
checkresiduals(modelo_arima)

previsoes <- forecast(modelo_arima, h = 10)
print(previsoes)
