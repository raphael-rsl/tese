library(tidyverse)

setwd("C:/Users/raphael.rsl/Desktop/R/MEI")

#dados retirados das tabelas de Estabelecimento e Simples Nacional/MEI
#disponível em https://www.gov.br/receitafederal/pt-br/assuntos/orientacao-tributaria/cadastros/consultas/dados-publicos-cnpj

#COLOCAR EM LOOP
#uSA MUITA MEMÓRIA
############## ESTABELECIMENTO ##############
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
           "cnae_sec", "bairro", "uf", "municipio")
  
  if(i == 1){
    final_estabelecimento = estabelecimento
  } 
  else final_estabelecimento = bind_rows(final_estabelecimento, estabelecimento)
}

rm(estabelecimento)

saveRDS(final_estabelecimento, file = "estabelecimento.rds")

teste = read.csv2("EMPRESA01.csv", header = FALSE)
colnames(teste)

############## EMPRESAS USAR EM SERVIDOR ##############

options(timeout = 10000)

for(i in 0:9) {
  url_rfb = paste0("http://200.152.38.155/CNPJ/K3241.K03200Y", i, ".D11113.EMPRECSV.zip")
  arquivo = paste0("C:/Users/raphael.rsl/Desktop/R/MEI/EMPRESA", i, ".zip")
  download.file(url_rfb, arquivo)
  unzip(arquivo, exdir = "C:/Users/raphael.rsl/Desktop/R/MEI")
  file.rename(paste0("C:/Users/raphael.rsl/Desktop/R/MEI/K3241.K03200Y", i, ".D11113.EMPRECSV"), 
              paste0("C:/Users/raphael.rsl/Desktop/R/MEI/EMPRESA0000" , i, ".csv"))
  empresa = read.csv2(paste0("C:/Users/raphael.rsl/Desktop/R/MEI/EMPRESA0000" , i, ".csv"), header = FALSE)
  colnames(empresa) = c("cnpj", "nome", "nat_jur", "qual_resp", "capital", "porte", "uf")
  
  if(i == 0){
    final_empresa = empresa
  } 
  else final_empresa = bind_rows(final_empresa, empresa)
} 

saveRDS(final_empresa, file = "empresa.rds")
