# Dashboard Monitoramento de Ordem de serviços (OS)

Esse dash foi criado para acompanhar as OS atenditas pelos colaboradores de informática e suporte do Procape
Os dados são disponibilizados são extraidos do bando e salvos em `.xlsx` e `.csv`, (tem um erro no banco que estava atrapalhando a extração, infelizmente o DBA não resolveu ainda :( - ).

### Extração de Dados (Rscript)
Os dados atualizados dos scripts estão em Python, mais só foi disponibilizado para os integrantes do projeto o arquivo em RScript, ordens do supervisor, de todo modo, está ótimo.
- Com nosso usuário de leitura, extraímos do BD os dados e salvamos em `.csv`.
- Mais esse processo é feito em 3 partes(devido ao erro de  extração no banco): as duas primeiras extrações são feitas e após isso são combinadas em um arquivo.

### Correção dos Dados com Dicionário

- Por algum motivo que o DBA não informou os dados após a extração não são retornados em UTF-8, mesmo no banco estando, e devido a isso, tivemos que fazer tudo manualmente, todas as correções de nomes, setores, horários e etc...

### Aplicação das Correções 

- Após a correção dos dados que vieram sem a formatação adequada, tivemos que fazer o "replace/mutate" dos dados seem utf-8 para utf-8, aalém disso também tivemos que corrigir nomes de colunas que vieram do nosso excel.

### Formatação das Datas 

- Os separadores das datas estavam variando muito, isso também devido aos dados já virem assim da aplicação que os pega e salva no banco, então tivemos que estudar e para fazer as trocas, deu trabalho, mas deu certo

### Aplicação de execução

- O App.r é onde a aplicação vai buscar os modulos e executar um servidor local para exibir ao usuário gráfico simples porém que mostram bem o que se pretendia, mostrar o quantitativo de OS por dia, de cada atentende.

## Ordem de execução dos Scripts

procurar o nosso shell `executar_scripts.sh` nesse shell temos os scripts na ordem correta de execução, se seguir a mesma ordem que está, vai dar certo.
