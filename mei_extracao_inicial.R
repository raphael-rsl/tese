library(tidyverse)

setwd("C:/Users/raphael.rsl/Desktop/R/MEI")

#dados retirados das tabelas de Estabelecimento e Simples Nacional/MEI
#microdados disponíveis em: https://www.gov.br/receitafederal/pt-br/assuntos/orientacao-tributaria/cadastros/consultas/dados-publicos-cnpj
#dicionário disponível em: https://www.gov.br/receitafederal/pt-br/assuntos/orientacao-tributaria/cadastros/consultas/arquivos/novolayoutdosdadosabertosdocnpj-dez2021.pdf


#MEI_CNPJ.csv é o arquivo http://200.152.38.155/CNPJ/F.K03200$W.SIMPLES.CSV.D11113.zip
#O scraping feito pelo "download.file" é muito lento
cnpj = read.csv2("MEI_CNPJ.csv", header = FALSE)
colnames(cnpj) = c("cnpj", "simples", "inc_simp", "exc_simp", "mei", "inc_mei", "exc_mei")

mei = cnpj %>% 
  filter(mei == 'S')
#14.213.294 linhas

rm(cnpj)

mei = mei %>% 
  filter(exc_mei == 0)
#14.207.811

#uSA MUITA MEMÓRIA
for(i in 1:10) {
nome = paste0("ESTABELECIMENTO", i, ".csv")
estabelecimento = read.csv2(nome, header = FALSE)
colnames(estabelecimento) = c("cnpj", "ordem", "dv", "id", "fantasia", "situacao", "data_situacao",
                               "motivo_situacao", "cidade_ext", "pais", "data_inicio", "cnae",
                               "cnae_sec", "tipo_logra", "logra", "num", "comp", "bairro", "cep",
                               "uf", "municipio", "ddd", "tel", "ddd2", "tel2", "dddfax", "fax",
                               "email", "sit_esp", "data_sit_esp")
estabelecimento = estabelecimento %>% 
  select("cnpj", "fantasia", "situacao", "data_situacao", "motivo_situacao", "data_inicio", "cnae",
         "cnae_sec", "tipo_logra", "logra", "num", "bairro", "uf", "municipio", "ddd", "tel", "email") %>% 
  mutate(ddd = as.numeric(ddd), tel = as.numeric(tel))

if(i == 1){
  final_estabelecimento = estabelecimento
} 
else final_estabelecimento = bind_rows(final_estabelecimento, estabelecimento)
}

rm(estabelecimento)

mei_cruzamento = merge(final_estabelecimento, mei, by = "cnpj")

head(mei)


#EMAILS
mei_ativos_cruzamento_df = mei_cruzamento %>% 
  filter(uf == "DF", situacao == "2") %>% 
  filter(email != "")

write.csv(mei_ativos_cruzamento_df, "emails_meis_df.csv")

#################

mei_ra = mei_ativos_cruzamento_df %>% 
  group_by(bairro) %>% 
  summarise(total = n())


final_estabelecimento_df = final_estabelecimento %>% 
  filter(uf == "DF") %>% 
  filter(situacao == "2")
