import pandas as pd
import numpy as np
from pathlib import Path
import re

def dados_nao_preenchidos_ou_faltantes():

    base_dir = Path('/mnt/googledrive/')
    file_path = base_dir / 'dados-tratados.xlsx'
    df = pd.read_excel(file_path)
    print('Verificando colunas...')

    df.loc[
    (df['tipo_os'] == 'SEI') & (df['setor__corrigido'] == 'Teste DEV') & (df['executor'] == '-'), 'executor'] = 'Tesla'
    print('Corrigindo executor Teste Dev')

    df.loc[
        (df['executor'] == 'tesla'), 'executor'] = 'Tesla'
    print('Corrigindo executor tesla -> Tesla')

    df.loc[
        (df['solicitante'] == 'Marcelo Machado'), 'solicitante'] = 'Marcelo Rocha Machado'

    df.loc[
        (df['validador'] == '-'), 'validador'] = 'Paulo'
    print('Corrigindo validador')

    df.loc[
    (df['executor'] == '-') & (df['tipo_os'] == 'O.S') & (df['enviado'] == 'MV'), 'executor'] = 'MV'
    print('Corrigindo executor e enviado MV')

    df.loc[
        (df['executor'] == '-') & (df['tipo_os'] == 'CAD AD'), 'executor'] = 'Hugo'
    print('Corrigindo executor CAD AD -> Hugo')
 
    df.loc[
        (df['executor'] == 'Machado'), 'executor'] = 'Marcelo'
    print('Corrigindo executor Machado -> Marcelo')

    df.loc[(
        df['concluinte'] == 'Machado'), 'concluinte'] = 'Marcelo'
    print('Corrigindo concluinte Machado -> Marcelo')

    df.loc[
        (df['executor'] == '-, Hugo, Wagner'), 'executor'] = 'Hugo'
    print('Corrigindo executor - Hugo, Wagner -> Hugo')

    df['executor'] = df['executor'].where(df['executor'] != '-', 'Indefinido')
    print('Corrigindo executor indefinido')

    df.loc[
        (df['statusx'].isin([1, 6, 9])), 'executor'] = 'Pendente'
    
    df.loc[(df['statusx']==2), 'executor'] = 'Aguardando Tecnico'
    
    df['concluinte'] = df['concluinte'].where(df['concluinte'] != '-', df['executor'])
    print('Corrigindo executor -> concluinte')

    df.loc[
        (df['executor'] == 'Raphael'), 'executor'] = 'Ferista'
    print('Corrigindo executor Ferista')

    df.loc[
        (df['concluinte'] == 'Raphael'), 'concluinte'] = 'Ferista'
    print('Corrigindo concluinte Ferista')

    print('Corrigindo datas')

    df['data_ini'] = df['data_cad'].where(df['data_ini'] != '-', df['data_cad'])

    df['data_ini'] = df['data_ini'].where(df['data_ini'] != '-', df['data_cad'])

    df['data_fim'] = df['data_fim'].where(df['data_fim'] != '-', df['data_cad'])

    print('datas corrigidas')

    print('Salvando arquivo')
    df.to_excel(base_dir /'dados-tratados.xlsx', index=False)
    print('Aplicando alterações')

def corrigir_setor(caminho_entrada, caminho_saida, dicionario_setores):
    df = pd.read_excel(caminho_entrada, engine="openpyxl")
    df["setor__corrigido"] = df["nome_setor_corrigido"]

    setores_ordenados = sorted(dicionario_setores.items(), key=lambda x: -max(len(p) for p in x[1]))

    for setor, palavras in setores_ordenados:
        padrao = re.compile(r"^(?:" + "|".join([re.escape(p) for p in palavras]) + ")$", re.IGNORECASE)
        df.loc[df["nome_setor_corrigido"].str.match(padrao, na=False), "setor__corrigido"] = setor
      
    df.to_excel(caminho_saida, index=False)

caminho_entrada = "/mnt/googledrive/python-dados.xlsx" 
caminho_saida = "/mnt/googledrive/dados-tratados.xlsx"

def chamados_abertos_em_sequencia():
    base_dir = Path('/mnt/googledrive/')
    file_path = base_dir / 'dados-tratados.xlsx'
    
    df = pd.read_excel(file_path)

    try:
        df['hora_ini'] = df['hora_ini'].replace('-', np.nan)

        df['hora_ini'] = pd.to_datetime(df['hora_ini'], format='%H:%M', errors='coerce').dt.time

        #df['hora_dos_chamados_sequenciais'] = df['hora_ini'].apply(lambda x: 
            #x.time() if pd.notna(x) and pd.to_datetime('05:00').time() <= x.time() <= pd.to_datetime('07:00').time() else 'Não se aplica')
        df['hora_dos_chamados_sequenciais'] = df['hora_ini'].apply(lambda x: 
            x if pd.notna(x) and pd.to_datetime('05:00').time() <= x <= pd.to_datetime('07:00').time() else 'Não se aplica')

        print("Coluna 'hora_dos_chamados_sequenciais' ajustada!")

    except Exception as e:
        print(f"Erro ao processar os dados: {e}")

    df.to_excel(base_dir / 'dados-tratados.xlsx', index=False)

def atualizar_campos():
    base_dir = Path('/mnt/googledrive/')
    file_path = base_dir / 'dados-tratados.xlsx'
    df = pd.read_excel(file_path)

    try:
        df.loc[df['solicitante'] == 'Marcelo da Silva Firmino Junior', ['tipo_os', 'executor', 'concluinte']] = 'Indefinido'

    except Exception as e:
        print(f"Erro ao processar os dados: {e}")

    df.to_excel(base_dir / 'dados-tratados.xlsx', index=False)

dicionario_setores = {
    #Administrativo
    "Administrativo":["Recepção Administrativo"],
    
    #Ambulatório
    "Ambulatório": ["Ambulatório - Sala 01", "Ambulatório - Sala 02", "Ambulatório - Sala 03", "Ambulatório - Sala 04", "Ambulatório - Sala 05","Ambulatório - Sala 06", "Ambulatório - Sala 07", "Ambulatório - Sala 08", "Ambulatório - Sala 09", "Ambulatório - Sala 10", "Ambulatório - Sala 11", "Ambulatório - Sala 12", "Ambulatório - ECG 1", "Ambulatório - ECG 2", "Ambulatório - ECG 3", "Ambulatório Recepção" ],

    #Bloco Cirúrgico
    "Bloco Cirúrgico": ["Bloco Cirúrgico - Gerência de Enfermagem", "Bloco Cirúrgico - Gerência Médica", "Bloco Cirúrgico - Sala 01", "Bloco Cirúrgico - Sala 02", "Bloco Cirúrgico - Sala 03", "Bloco Cirúrgico - Sala 04"],

    #Casa de Chagas
    "Casa de Chagas":["Casa de Chagas - Associação", "Casa de Chagas - Arquivo", "Casa de Chagas - Gerência Médica", "Casa de Chagas - Gerência de Enfermagem", "Casa de Chagas - ECG 1", "Casa de Chagas - ECG 2", "Casa de Chagas - Sala 1", "Casa de Chagas - Sala 2", "Casa de Chagas - Sala 3", "Casa de Chagas - Recepção" ],

    #Coordenação de Enfermagem

    "Coordenação de Enfermagem":[ "Gerência de Enfermagem","Recepção Coordenação de Enfermagem", "Pacientes Externo"],

    #Centro de Estudos
    "Centro de Estudos" : ["Centro de Estudo - Sala de Aula 01", "Coordenação de Pesquisa"],

    #Direção
    "Direção" : ["Diretor Médico","Diretória"],

    #Emergência
    "Emergência":["Emergência - Gerência de Enfermagem", "Emergência - Gerência", "Emergência - ECG","Emergência - Posto 01", "Emergência - Posto 02","Emergência - Evolução", "Emergência - Posto de Coleta Laboratórial","Emergência - Recepção","Emergência - Sala Amarela","Emergência - Sala Vermelha","Emergência - Sala de Prontuário"],

    #Farmácia
    "Farmácia": ["Farmácia", "Farmácia do Ambulatório","Farmácia DI", "Farmácia do Bloco", "Farmácia CAF", "Farmácia Emergência", "Farmácia da Hemodinânica", "Farmácia de Suprimentos", "Gerência da Farmácia"],


    #Ergometria
    "Ergometria":["Ergometria - Gerência", "Ergometria - Laudos", "Ergometria - Esteiras"],

    #Evolução 
    "Evolução": ["Evolução - Sala Vermelha","Evolução - Sala Amarela"],

    #Enfermarias
    "Enfermaria":["Enfermaria 4º Andar","Enfermaria 5º Andar","Enfermaria 6º Andar", "Enfermaria 7º Andar"],

    #Hemodinâmica
    "Hemodinâmica":["Hemodinâmica - Digitação","Hemodinâmica - Gerência", "Hemodinâmica - Laudos","Hemodinâmica - Recepção", "Hemodinâmica - Sala A","Hemodinâmica - Sala B", "Hemodinâmica - Sala C", "Hemodinâmica - Sala Reunião"],

    #UCO
    "Coronária": ["Unidade Coronária", "Coronária I", "Coronária II"],
    "Laboratório de Análises Clínicas": ["Laboratório de Análises Clínicas"],

    #RECEPÇÃO E ADMISSÃO
    "Recepção e Admissão": ["Recepção", "Admissão", "Alta"],

    #Fisioterapia
    "Fisioterapia": ["Fisioterapia e Terapia Ocupacional"],

    #Laboratório
    "Laboratório": ["Laboratório - Almoxerifado","Laboratório - Recepção",],

   #Laboratório PROLAB
   "Laboratório PROLAB":["Laboratório Triagem","Coleta de Sangue"],

    #Manutenção
    "Manutenção":["Gerência da Manutenção","Manutenção Subsolo"],

    #Nutrição
    "Nutrição":["Gerência da Nutrição","Nutrição","Nutrição - Evolução", "Nutrição - Cozinha",],

    #Pesquisa Clinica
    "Pesquisa Clínica":["Pesquisa Clínica - Consultório"],

    #Raio X
    "Raio-X":["Raio-X - Administração", "Raio-X - Recepção","Raio-X - Laudo","Raio-X - Exame",],

    #Zeladoria e RM
    "Zeladoria":["Gerência da Zeladoria","Zeladoria (RM)"],

    #Tomografia

    "Tomografia":["Tomografia - Sala Exame", "Tomografia - Laudo", "Tomografia - Enfermagem"]


      # Agora "Ambulatório" não sobrescreve "Farmácia do Ambulatório"

    #
}



corrigir_setor(caminho_entrada, caminho_saida, dicionario_setores)
chamados_abertos_em_sequencia()
dados_nao_preenchidos_ou_faltantes()
atualizar_campos()

print('Finalizado')
