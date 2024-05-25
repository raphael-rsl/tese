library(likert)
library(readxl)
library(dplyr)
library(table1)
library(flextable)
library(RColorBrewer)

dados_pesquisa = read_xlsx("results-survey386997_maio_2024_excel_completo.xlsx", sheet = "Original")
itens_pesquisa = read_excel("results-survey386997_maio_2024_excel_completo.xlsx", sheet = "Categoria")
ordem_pesquisa = read_excel("results-survey386997_maio_2024_excel_completo.xlsx", sheet = "Ordem")

glimpse(dados_pesquisa)

#####################################################################################
#                                                                                   #
#                                                                                   #
#                                 BLOCO 1                                           #  
#                                                                                   #
#                                                                                   #
#####################################################################################

dados_pesquisa[,c(8:26)] = lapply(dados_pesquisa[,c(8:26)], function(x){factor(x, levels = c("DiscordoTotalmente", 
                                                                         "DiscordoParcialmente",
                                                                         "ConcordoParcialmente",
                                                                         "ConcordoTotalmente"), labels = 
                                                             c("Discordo Totalmente",
                                                               "Discordo Parcialmente",
                                                               "Concordo Parcialmente",
                                                               "Concordo Totalmente"))})

colnames(dados_pesquisa)[8:26] <- itens_pesquisa$questao[1:19]

# #Descritiva Geral
# table1(~., bloco1, overall = "n (%)", decimal.mark = ",")
# #Descritiva por Idade
# table1(~. | dados_pesquisa$idade2, bloco1, overall = "n Total", decimal.mark = ",") 
# #Descritiva por Gênero
# table1(~. | dados_pesquisa$genero, bloco1, overall = "n Total", decimal.mark = ",")
# #Descritiva por Raça
# table1(~. | dados_pesquisa$raca, bloco1, overall = "n Total", decimal.mark = ",")
# #Descritiva por RA
# table1(~. | dados_pesquisa$ra2, bloco1, overall = "n Total", decimal.mark = ",")

graf_bloco1 <- likert::likert(as.data.frame(dados_pesquisa[8:26]))

likert::likert.bar.plot(graf_bloco1) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Discordo Totalmente",
                                                                  "Discordo Parcialmente",
                                                                  "Concordo Parcialmente",
                                                                  "Concordo Totalmente")) +
  theme(legend.position = "bottom", legend.justification = "right") +
  ggtitle("Bloco 1 - Grau de Concordância | n(94)")

ggsave("likert1.png", width = 10, height = 12)

#mapacalor
#plot(bloco1, type = "heat", wrap=60, text.size=3) + theme (axis.title.y = element_text(size="10"))

###Gênero

bloco1_genero = likert(as.data.frame(dados_pesquisa[8:26]), grouping = dados_pesquisa$genero)

likert::likert.bar.plot(bloco1_genero) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text=element_text(size=20),
        axis.text = element_text(size=15),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Discordo Totalmente",
                                                                  "Discordo Parcialmente",
                                                                  "Concordo Parcialmente",
                                                                  "Concordo Totalmente")) +
  theme(legend.position = "bottom") +
  ggtitle("Bloco 1 - Grau de Concordância por Gênero | n=94 \n Masculino = 56 | Feminino = 35 | Não-Binário = 3")


ggsave("likertgenero.png", width = 16, height = 30)


###Raça

bloco1_raca = likert(as.data.frame(dados_pesquisa[8:26]), grouping = dados_pesquisa$raca2)

likert::likert.bar.plot(bloco1_raca) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text=element_text(size=20),
        axis.text = element_text(size=15),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Discordo Totalmente",
                                                                  "Discordo Parcialmente",
                                                                  "Concordo Parcialmente",
                                                                  "Concordo Totalmente")) +
  theme(legend.position = "bottom") +
  ggtitle("Bloco 1 - Grau de Concordância por Raça | n=94 \n Branca (36) | Não-Branca (58) \n
          Não-Branca --> Parda (40) | Preta (15) | Indígena (1) | Amarela (1)  ")


ggsave("likertraca.png", width = 16, height = 26)


###Idade

bloco1_idade = likert(as.data.frame(dados_pesquisa[8:26]), grouping = dados_pesquisa$idade2)

likert::likert.bar.plot(bloco1_idade) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text=element_text(size=20),
        axis.text = element_text(size=15),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Discordo Totalmente",
                                                                  "Discordo Parcialmente",
                                                                  "Concordo Parcialmente",
                                                                  "Concordo Totalmente")) +
  theme(legend.position = "bottom") +
  ggtitle("Bloco 1 - Grau de Concordância por Idade | n=94 \n 20-30 (16) | 31-40 (29) |
          41-50 (29) | Acima de 50 (20)")


ggsave("likertidade.png", width = 16, height = 32)


###RA

dados_pesquisa$ra2 = factor (dados_pesquisa$ra2, levels = c("Outro",
                                                            "Renda Baixa",
                                                            "Renda Média-Baixa",
                                                            "Renda Média-Alta", 
                                                            "Renda Alta"),
                             ordered = T)

bloco1_ra = likert(as.data.frame(dados_pesquisa[8:26]), grouping = dados_pesquisa$ra2)

likert::likert.bar.plot(bloco1_ra) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text=element_text(size=20),
        axis.text = element_text(size=15),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Discordo Totalmente",
                                                                  "Discordo Parcialmente",
                                                                  "Concordo Parcialmente",
                                                                  "Concordo Totalmente")) +
  theme(legend.position = "bottom") +
  ggtitle("Bloco 1 - Grau de Concordância por Classe de RA | n=94 \n Renda Alta (30) | Renda Média-Alta (24) |
          Renda Média-Baixa (20) | Renda Baixa (18) | Outro (2)")


ggsave("likertra.png", width = 16, height = 36)


#####################################################################################
#                                                                                   #
#                                                                                   #
#                                 BLOCO 2                                           #  
#                                                                                   #
#                                                                                   #
#####################################################################################

dados_pesquisa[,c(27:32)] = lapply(dados_pesquisa[,c(27:32)], function(x){factor(x, levels = c("Nada Importante", 
                                                                                               "Pouco Importante",
                                                                                               "Importante",
                                                                                               "Muito Importante"), labels = 
                                                                                 c("Nada Importante", 
                                                                                   "Pouco Importante",
                                                                                   "Importante",
                                                                                   "Muito Importante"))})


# bloco2 = dados_pesquisa[,c(27:32)]
# bloco2 = lapply(bloco2, function(x){factor(x, levels = c("Nada Importante", 
#                                                                          "Pouco Importante",
#                                                                          "Importante",
#                                                                          "Muito Importante"), labels = 
#                                                              c("Nada Importante", 
#                                                                "Pouco Importante",
#                                                                "Importante",
#                                                                "Muito Importante"))})

dados_graf2 <- likert::likert(as.data.frame(dados_pesquisa[27:32]))

likert::likert.bar.plot(dados_graf2) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Nada Importante", 
                                                                  "Pouco Importante",
                                                                  "Importante",
                                                                  "Muito Importante")) +
  theme(legend.position = "bottom", legend.justification = "right") +
  ggtitle("Bloco 2 - Grau de Importância | n(94)")

ggsave("likert2.png", width = 10, height = 5)


###Gênero

bloco2_genero = likert(as.data.frame(dados_pesquisa[27:32]), grouping = dados_pesquisa$genero)

likert::likert.bar.plot(bloco2_genero) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text=element_text(size=16),
        axis.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Nada Importante", 
                                                                  "Pouco Importante",
                                                                  "Importante",
                                                                  "Muito Importante")) +
  theme(legend.position = "bottom") +
  ggtitle("Bloco 1 - Grau de Concordância por Gênero | n=94 \n Masculino = 56 | Feminino = 35 | Não-Binário = 3")


ggsave("likertgenero2.png", width = 12, height = 14)



###Raça

bloco2_raca = likert(as.data.frame(dados_pesquisa[27:32]), grouping = dados_pesquisa$raca2)

likert::likert.bar.plot(bloco2_raca) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text=element_text(size=16),
        axis.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Nada Importante", 
                                                                  "Pouco Importante",
                                                                  "Importante",
                                                                  "Muito Importante")) +
  theme(legend.position = "bottom") +
  ggtitle("Bloco 2 - Grau de Importância por Raça | n=94 \n Branca (36) | Não-Branca (58) \n
          Não-Branca --> Parda (40) | Preta (15) | Indígena (1) | Amarela (1)  ")


ggsave("likertraca2.png", width = 12, height = 14)


###Idade

bloco2_idade = likert(as.data.frame(dados_pesquisa[27:32]), grouping = dados_pesquisa$idade2)

likert::likert.bar.plot(bloco2_idade) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text=element_text(size=16),
        axis.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Nada Importante", 
                                                                  "Pouco Importante",
                                                                  "Importante",
                                                                  "Muito Importante")) +
  theme(legend.position = "bottom") +
  ggtitle("Bloco 2 - Grau de Importância por Idade | n=94 \n 20-30 (16) | 31-40 (29) |
          41-50 (29) | Acima de 50 (20)")


ggsave("likertidade2.png", width = 12, height = 14)


###RA

dados_pesquisa$ra2 = factor (dados_pesquisa$ra2, levels = c("Outro",
                                                            "Renda Baixa",
                                                            "Renda Média-Baixa",
                                                            "Renda Média-Alta", 
                                                            "Renda Alta"), ordered = T)

bloco2_ra = likert(as.data.frame(dados_pesquisa[27:32]), grouping = dados_pesquisa$ra2)

likert::likert.bar.plot(bloco2_ra) +
  labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        strip.text=element_text(size=20),
        axis.text = element_text(size=15),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = brewer.pal(n=4,"RdYlBu"), breaks = c("Nada Importante", 
                                                                  "Pouco Importante",
                                                                  "Importante",
                                                                  "Muito Importante")) +
  theme(legend.position = "bottom") +
  ggtitle("Bloco 2 - Grau de Importância por Classe de RA | n=94 \n Renda Alta (30) | Renda Média-Alta (24) |
          Renda Média-Baixa (20) | Renda Baixa (18) | Outro (2)")


ggsave("likertra2.png", width = 14, height = 20)


#####################################################################################
#                                                                                   #
#                                                                                   #
#                                 BLOCO 3     NO EXCEL (MAPA DE CALOR)              #  
#                                                                                   #
#                                                                                   #
#####################################################################################


dados_pesquisa[,c(33:38)] = lapply(dados_pesquisa[,c(33:38)], 
                                   function(x){factor(x, levels = c("Responsabilidade",
                                                                    "Motivação",
                                                                    "Mérito",
                                                                    "Liberdade",
                                                                    "Sensação de Justiça",
                                                                    "Individualismo"), 
                                                      labels = c("Responsabilidade",
                                                                 "Motivação",
                                                                 "Mérito",
                                                                 "Liberdade",
                                                                 "Sensação de Justiça",
                                                                 "Individualismo"))})

colnames(dados_pesquisa)[33:38] <- c("1º Lugar", "2º Lugar", "3º Lugar",
                                     "4º Lugar", "5º Lugar", "6º Lugar", ordered = T)



graf_bloco3 <- likert::likert(as.data.frame(dados_pesquisa[33:38]))

likert::likert.heat.plot(graf_bloco3) +
  #labs(y = "") + 
  guides(fill = guide_legend(title = "")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(legend.position = "bottom", legend.justification = "center") +
  ggtitle("Bloco 3 - Ordem de Importância | n(94)")

ggsave("likert3.png", width = 8, height = 6)
