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
library(relaimpo)

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
# base = base %>% select(-Data)
summary(base)    

### Histograma Condutividade
ggplot(base, aes(x = Cond.)) + 
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Histograma da Condutividade", x = "Condutividade (µS/cm)", y = "Frequência")


### Verificar a distribuição normal da condutividade - Teste de Kolmogorov-Smirnov
ks.test(base$Cond., "pnorm", mean(base$Cond.), sd(base$Cond.))
qqnorm(base$Cond.)
qqline(base$Cond., col = "red")

## Verificar a distribuição normal da condutividade - Teste de Shapiro-Wilk
shapiro.test(base$Cond.)
ggplot(base, aes(x = Cond.)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(base$Cond.), sd = sd(base$Cond.)), color = "red") +
  labs(title = "Densidade de Condutividade", x = "Condutividade (µS/cm)", y = "Densidade") +
  theme_minimal()


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



# Criar um data frame com os períodos
periodos <- data.frame(
  start = as.Date(c("2012-01-21", "2012-03-28", "2012-07-01", "2012-10-01", "2013-01-01", "2013-04-08", "2013-07-01", "2013-09-29")),
  end = as.Date(c("2012-03-20", "2012-06-22", "2012-09-22", "2012-12-22", "2013-04-01", "2013-06-23", "2013-09-27", "2013-10-20")),
  label = c("Verão 2012", "Outono 2012", "Inverno 2012", "Privavera 2012", "Verão 2013", "Outono 2013", "Inverno 2013", "Privavera 2013")
)

# Plotando o gráfico com ggplot2 e destacando os períodos
ggplot(base, aes(x = Data, y = Cond., color = Ponto)) +
  # Adiciona áreas sombreadas para os períodos
  geom_rect(data = periodos, inherit.aes = FALSE, aes(
    xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label), alpha = 0.2) +
  geom_line(size = 0.5) +   # Linhas coloridas por ponto
  geom_point(size = 0.5) +  # Pontos coloridos para cada linha
  labs(
    title = "Condutividade ao Longo do Tempo por Ponto de Coleta",
    x = "Data",
    y = "Condutividade (µS/cm)",
    color = "Ponto de Coleta",
    fill = "Períodos"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Coloca a legenda na parte inferior
  scale_x_date(
    date_breaks = "8 weeks",  # Intervalo de 8 semanas
    date_labels = "%d/%m/%Y" # Formato do rótulo das datas
  )


### Linha Oxigênio ao Longo do Tempo por Ponto de Coleta
ggplot(base, aes(x = Data, y = Oxigenio, color = Ponto)) +
  geom_line(size = 0.5) +   # Linhas coloridas por ponto
  geom_point(size = 0.5) +  # Pontos coloridos para cada linha
  labs(
    title = "Oxigênio ao Longo do Tempo por Ponto de Coleta",
    x = "Data",
    y = "Oxigênio (mg/L)",
    color = "Ponto de Coleta"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Coloca a legenda na parte inferior
  scale_x_date(
    date_breaks = "8 weeks",  # Intervalo de 2 semanas
    date_labels = "%d/%m/%Y" # Formato do rótulo das datas
  )

### Linha Precipitação ao Longo do Tempo por Ponto de Coleta
ggplot(base, aes(x = Data, y = Rainfall, color = Ponto)) +
  # Adiciona áreas sombreadas para os períodos
  geom_rect(data = periodos, inherit.aes = FALSE, aes(
    xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label), alpha = 0.2) +
  geom_line(size = 0.5) +   # Linhas coloridas por ponto
  geom_point(size = 0.5) +  # Pontos coloridos para cada linha
  labs(
    title = "Precipitação ao Longo do Tempo por Ponto de Coleta",
    x = "Data",
    y = "Precipitação (mm)",
    color = "Ponto de Coleta",
    fill = "Períodos"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Coloca a legenda na parte inferior
  scale_x_date(
    date_breaks = "8 weeks",  # Intervalo de 8 semanas
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
  labs(title = "Condutividade vs Precipitação por Estação Única", x = "Precipitação", y = "Condutividade (µS/cm)") +
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





