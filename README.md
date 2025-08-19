# Dashboard de Ordens de Serviço (OS) 📊

## Visão Geral 🌐

Criação de um dashboard para verificação dos atendimentos das ordens de serviço. 💡

O sistema tem como objetivo carregar as informações exportadas em arquivos `.xlsx` e `.csv`, disponibilizados pelo setor de DBA, para filtragem e análise dos dados.

## Funcionamento do Sistema 💻

Os dados são extraídos do banco de dados (os caminhos originais foram alterados para preservação do ambiente). Existem quatro arquivos utilizados para o tratamento dos dados em Excel:

### 1. Extração de Dados (Rscript) 👨‍💻

- Após a conexão com o banco, os dados são extraídos e salvos em `.csv`.
- O processo é dividido em três partes: primeira extração, segunda extração e combinação dos dados extraídos.
- Essa divisão ocorre devido a uma inconsistência no banco, que já está sendo tratada pelo pessoal do DBA 😁👍.

### 2. Correção dos Dados com Dicionário 📔

- Devido a inconsistências nos dados do banco relacionadas a caracteres especiais, foi utilizado um dicionário para padronizar os nomes dos setores e usuários.

### 3. Aplicação das Correções 👷‍♂️

- Com base nos dicionários corrigidos, utilizamos a função `mutate()` para substituir os nomes dos setores.
- Também é feita a correção das colunas de datas: início, fim, atendimento, entre outras.

### 4. Formatação das Datas 📅

- Os separadores de datas são padronizados para garantir consistência na visualização.

### 5. `app.R` 🚀👩‍🚀👨‍🚀

- Aqui é onde a aplicação é disponibilizada para os usuários via web.
- Os gráficos são gerados e há interações com o usuário, como aplicação de filtros e exibição dos dados tratados.

---

## Execução dos Scripts 🤓👨‍💻

Existe um arquivo chamado `executar_scripts.sh` que será executado no servidor e rodará todos os scripts de forma automática.

---

## Comandos Úteis (em caso de falha por uso de memória) ✨

```bash
# Recarrega os arquivos de configuração
sudo systemctl daemon-reload

# Reinicia a aplicação
sudo systemctl restart grafico_OS.service

# Verifica o status do serviço
sudo systemctl status grafico_OS.service
