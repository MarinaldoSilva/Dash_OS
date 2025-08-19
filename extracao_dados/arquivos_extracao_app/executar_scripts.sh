#!/bin/bash
echo "Iniciando Script"
echo "executando 1_extracao_dados.R"
Rscript /mnt/googledrive/script/1_extracao_dados.R

echo "executando 2_correcao_tabela.R"
Rscript /mnt/googledrive/script/2_correcao_tabela.R

sleep 5

echo "executando 3_correcao_dados.R"
Rscript /mnt/googledrive/script/3_correcao_dados.R
#python3 /mnt/googledrive/script/3_finalizacao_dados.py

echo "executando 4_formatacao_datas.R"
Rscript /mnt/googledrive/script/4_formatacao_datas.R

#Rscript /mnt/googledrive/script/app1.R
Rscript /mnt/googledrive/script/app1.R
echo "Todos os scripts foram executados!"
echo "acesse 192.168.150.34:8788 para entrar na aplicacao"

