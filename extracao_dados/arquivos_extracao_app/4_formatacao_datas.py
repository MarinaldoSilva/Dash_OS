import pandas as pd
from pathlib import Path

base_dir = Path('/mnt/googledrive/')
file_path = base_dir / 'dados-tratados.xlsx'
df = pd.read_excel(file_path)

def trocar_separador_data(x):
 
    if pd.isna(x):
        return x
    return str(x).replace("-", "/") 

def substituir_por_na(x):

    if str(x).strip() in ["-", "/", "_", "__"]:  
        return pd.NA
    return x 

def padronizar_e_corrigir_datas(df, colunas):
 
    for coluna in colunas:
        df[coluna] = df[coluna].apply(trocar_separador_data)  
        df[coluna] = df[coluna].apply(substituir_por_na)
    return df
df = padronizar_e_corrigir_datas(df, ["data_ini", "data_fim", "data_cad"])
df.to_excel(base_dir / 'dados-tratados.xlsx', index=False)