# Trabalho de Estatística e Predição
# Filipe Ribeiro Rocha
# Victor Luiz de Souza

# Instalação dos pacotes
install.packages(c("ggplot2", "dplyr", "corrplot", "car", "caret"))

# Carregando os pacotes
library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
library(caret)

# Importação dos dados para análise
base = read.csv("coleta_lagoa_peri.csv")

# Converter virgula por ponto
base = base %>%
  mutate(
    Temp. = as.numeric(gsub(",", ".", Temp.)),
    Cond. = as.numeric(gsub(",", ".", Cond.)),
    Oxigenio = as.numeric(gsub(",", ".", Oxigenio)),
    Profund. = as.numeric(gsub(",", ".", Profund.)),
    Vel = as.numeric(gsub(",", ".", Vel)),
    Rainfall = as.numeric(gsub(",", ".", Rainfall))
  )

# Ajuste da base de dados - retirar a coluna Data
base = base %>% select(-Data)

# Resumo estatístico
summary(base)

# Visualização gráfica
ggplot(base, aes(x = Temp.)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribuição da Temperatura", x = "Temperatura (°C)", y = "Frequência")

ggplot(base, aes(x = Cond.)) + 
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot da Condutividade", x = "", y = "Condutividade (µS/cm)")

# Correlação
cor_matrix = cor(base %>% select(Temp., Cond., Oxigenio, Profund., Vel, Rainfall))
print(cor_matrix)
