library(tidyverse)
library(dplyr)
library(tokenizers)
library(ggplot2)

# Importando todas as palavras da língua portuguesa
bd <- read.table("dicio_port.txt", encoding="UTF-8", quote="\"", comment.char="")

# Alterando nome
colnames(bd) <- "palavras"

# Filtrando apenas palavras pois alguns termos da lista vem com decodificação errada
bd <- bd %>% 
  filter(!(str_detect(palavras, "\\W+")))

# Definindo palavras com apenas 5 letras
bd2 <- subset(bd, nchar(as.character(palavras)) == 5)

# Padronizando letras minúsculas
bd2$palavras <- str_to_lower(bd2$palavras)

# Substituindo acentos
bd2$palavras <- gsub("á", "a", bd2$palavras)
bd2$palavras <- gsub("ã", "a", bd2$palavras)
bd2$palavras <- gsub("â", "a", bd2$palavras)
bd2$palavras <- gsub("é", "e", bd2$palavras)
bd2$palavras <- gsub("ê", "e", bd2$palavras)
bd2$palavras <- gsub("í", "i", bd2$palavras)
bd2$palavras <- gsub("ó", "o", bd2$palavras)
bd2$palavras <- gsub("ô", "o", bd2$palavras)
bd2$palavras <- gsub("õ", "o", bd2$palavras)
bd2$palavras <- gsub("ú", "i", bd2$palavras)
bd2$palavras <- gsub("ç", "c", bd2$palavras)

# Removendo palavras que tornaram-se iguais (roerá e roera por exemplo)
bd2 <- distinct(bd2)

# Contagem das letras:
# Separando as letras para contagem 
letras <- as.character(bd2)

lista <- as.data.frame(unlist(strsplit(letras, "")))
colnames(lista) <- "palavras"

# Contando
lista_final <- count(lista, palavras, sort = T)

# Retirando tudo que não é uma letra
lista_final <- lista_final %>% 
  filter(!str_detect(palavras, "\\W"))

# Plot frequência letras
plot_letras <- ggplot(lista_final) +
  aes(x = reorder(palavras, -n), y = n) +
  geom_bar(stat = "identity", fill = "#10ACB8", width = 0.8) +
  geom_text(aes(label=n), color = "black", vjust = -0.5, size = 2.8) +
  theme_minimal() +
  labs(title = "Frequência das letras em palavras com 5 letras", 
        x = "",
        y = "Frequência\n",
       caption = "\nArtur Vidaurre de Almeida") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        plot.caption = element_text(face = "italic", size = 12),
        axis.text.x = element_text(size = 14, vjust = 4.5),
        axis.title.x = element_blank())

# Salvando
ggsave(filename = "frequencia_letras.jpeg", plot = plot_letras, width = 8, height = 5)
ggsave(filename = "frequencia_letras.png", plot = plot_letras, width = 8, height = 5)

# Removendo arquivos
rm(lista, letras, plot, lista_final)
# Buscando boas palavras para início:
# Definindo cada letra como uma nova coluna no Data Frame
top_palavras <- bd2 %>% 
                  mutate(letras = as.character(tokenize_characters(palavras)))

# Removendo resquícios do processo de tokens
top_palavras$letras <- str_remove(top_palavras$letras, pattern = "c")
top_palavras$letras <- str_remove(top_palavras$letras, pattern = "\\(")
top_palavras$letras <- str_remove(top_palavras$letras, pattern = "\\)")
top_palavras$letras <- str_remove_all(top_palavras$letras, pattern = '\\"')

# Separando cada letra em uma nova coluna
top_palavras <- separate(top_palavras, letras, into = c("letra_1", "letra_2", "letra_3", "letra_4", "letra_5", sep = ","))

# Novo df com apenas as letras
top_letras <- top_palavras %>% 
                  select(2:6)

# Eliminando palavras com letras repetidas
top_letras <- top_letras %>% 
              filter(letra_1 != letra_2 &
                     letra_1 != letra_3 &
                     letra_1 != letra_4 &
                     letra_1 != letra_5 &
                     letra_2 != letra_3 &
                     letra_2 != letra_4 &
                     letra_2 != letra_5 &
                     letra_3 != letra_4 &
                     letra_3 != letra_5 &
                     letra_4 != letra_5)

# Definindo df que servirá de base para unir os valores posteriormente
top_reserva <- top_letras

# Substituindo cada letra por sua frequência 
top_letras[top_letras == "a"] <- "4243"
top_letras[top_letras == "b"] <- "535"
top_letras[top_letras == "c"] <- "1087"
top_letras[top_letras == "d"] <- "752"
top_letras[top_letras == "e"] <- "2734"
top_letras[top_letras == "f"] <- "530"
top_letras[top_letras == "g"] <- "599"
top_letras[top_letras == "h"] <- "233"
top_letras[top_letras == "i"] <- "2188"
top_letras[top_letras == "j"] <- "205"
top_letras[top_letras == "k"] <- "4"
top_letras[top_letras == "l"] <- "1021"
top_letras[top_letras == "m"] <- "1293"
top_letras[top_letras == "n"] <- "865"
top_letras[top_letras == "o"] <- "2763"
top_letras[top_letras == "p"] <- "691"
top_letras[top_letras == "q"] <- "38"
top_letras[top_letras == "r"] <- "2090"
top_letras[top_letras == "s"] <- "1967"
top_letras[top_letras == "t"] <- "1096"
top_letras[top_letras == "u"] <- "1230"
top_letras[top_letras == "v"] <- "595"
top_letras[top_letras == "x"] <- "162"
top_letras[top_letras == "z"] <- "200"

# Alterando para numérico e somando o resultado em nova coluna
top_letras$letra_1 <- as.numeric(top_letras$letra_1)
top_letras$letra_2 <- as.numeric(top_letras$letra_2)
top_letras$letra_3 <- as.numeric(top_letras$letra_3)
top_letras$letra_4 <- as.numeric(top_letras$letra_4)
top_letras$letra_5 <- as.numeric(top_letras$letra_5)

top_letras <- top_letras %>% 
  mutate(pontuação = rowSums(top_letras[c(1:5)]))

# Reunindo as letras para determinar as palavras novamente
top_reserva$Palavras <- paste(top_reserva$letra_1, top_reserva$letra_2, 
                              top_reserva$letra_3, top_reserva$letra_4,
                              top_reserva$letra_5, sep = "")

# Separando apenas a soma das frequências (pontuação)
top_reserva <- top_reserva %>% select(6)

# Juntando df final com Palavra e Pontuação
top_final <- cbind(top_reserva, top_letras$pontuação)

colnames(top_final) <- c("Palavras", "Pontuação")

# Ordenando as melhores palavras para começar o jogo
top_final <- top_final %>% 
  arrange(-Pontuação)

# Plot melhores palavras
plot_palavras <- top_final %>% 
  head(n = 15) %>% 
  ggplot() +
  aes(x = reorder(Palavras, Pontuação), y = Pontuação) +
  coord_flip(ylim = c(12000, 14000)) +
  geom_bar(stat = "identity", fill = "#10ACB8", width = 0.8) +
  geom_text(aes(label=Pontuação), color = "white", hjust = 1.3, vjust = 0.4, size = 4) +
  theme_minimal() +
  labs(title = "Melhores palavras para começar o Termo", 
       subtitle = "A pontuação é definida pela soma das frequências das letras em cada palavra\n",
       x = "",
       y = "",
       caption = "\nArtur Vidaurre de Almeida") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        plot.caption = element_text(face = "italic", size = 12),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 13, face = "bold", hjust = 1))

# Salvando
ggsave(filename = "melhores_palavras.jpeg", plot = plot_palavras, width = 8, height = 5)
ggsave(filename = "melhores_palavras.png", plot = plot_palavras, width = 8, height = 5)
