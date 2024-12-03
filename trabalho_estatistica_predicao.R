# Trabalho de Estatística e Predição
# Filipe Ribeiro Rocha
# Victor Luiz de Souza

### 1. Instalar Libs
install.packages(c("ggplot2", "dplyr", "corrplot", "car", "caret"))
install.packages(c("relaimpo"))
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

base$EstaçãoUnica = as.factor(base$EstaçãoUnica)
base$Ponto = as.factor(base$Ponto)
base <- base %>%
  filter(!Cond. %in% c(49.7, 81.2, 78.7, 78.5, 73.8))

  # Converte data em datetime
base$Data <- as.Date(base$Data, format = "%m/%d/%Y") 
base = base %>% select(-Data)
summary(base)    

### Histograma Condutividade
ggplot(base, aes(x = Cond.)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Condutividade", x = "Condutividade (µS/cm)", y = "Frequência")


### Boxplot da Condutividade
ggplot(base, aes(y = Cond.)) + 
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot da Condutividade", y = "Condutividade (µS/cm)")
summary(base$Cond.)
sd(base$Cond., na.rm = TRUE)

### Correlação
  # Matriz gráfica
numeric_data <- base[, sapply(base, is.numeric)]  # Selecionar apenas variáveis numéricas
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle")

  # Matriz numérica
cor_matrix <- cor(numeric_data, use = "complete.obs")
print(cor_matrix)


### Linha Condutividade ao Longo do Tempo por Ponto de Coleta
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
  theme(legend.position = "bottom") +  # Coloca a legenda na parte inferior
  scale_x_date(
    date_breaks = "8 weeks",  # Intervalo de 2 semanas
    date_labels = "%d/%m/%Y" # Formato do rótulo das datas
  )

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

ggplot(base, aes(x = Rainfall, y = Cond.)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ EstaçãoUnica) +
  labs(title = "Condutividade vs Pluviosidade por Estação Única", x = "Pluviosidade", y = "Condutividade (µS/cm)") +
  theme_minimal()

ggplot(base, aes(x = Profund., y = Cond.)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ EstaçãoUnica) +
  labs(title = "Condutividade vs Profundidade por Estação Única", x = "Profundidade", y = "Condutividade (µS/cm)") +
  theme_minimal()

### Modelo
# modelo <- lm(Cond. ~ Ponto + Temp. + Oxigenio  + Profund. + Vel + Rainfall + EstaçãoUnica, data=base) Backward-1
# modelo <- lm(Cond. ~ Temp. + Oxigenio  + Profund. + Vel + Rainfall + EstaçãoUnica, data=base) Backward-2
# modelo <- lm(Cond. ~ Temp. + Oxigenio  + Profund. + Rainfall + EstaçãoUnica, data=base) Backward-3
modelo <- lm(Cond. ~ + Oxigenio  + Profund. + EstaçãoUnica, data=base)
summary(modelo)

# Importancia de cada variavel no modelo
imp = calc.relimp(modelo)
var.exp = data.frame(round(imp$lmg*100,1))
colnames(var.exp) = "imp.lmg"
nome = rownames(var.exp)
var.exp = data.frame(nome,var.exp)
ggplot(var.exp,aes(nome,imp.lmg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = imp.lmg), vjust = 1.5, colour = "white")

# Analise de residuos
plot(fitted(modelo), rstandard(modelo))
abline(0,0)





