from io import BytesIO
from shiny import App, ui, render
import pandas as pd
import plotly.express as px
from pathlib import Path
import os
from ftplib import FTP
import openpyxl

base_dir_lunix = Path("/tratar_dados")
base_dir_windows = Path("C:\tratar_dados")
FTP_HOST = "127.0.0.1"
FTP_USER = "tesla"
FTP_PASS = "tesla"
FTP_FILE_NAME = "dados-tratados.xlsx"
CAMINHO_ABSOLUTO_ARQUIVO = base_dir_lunix / FTP_FILE_NAME if (base_dir_lunix / FTP_FILE_NAME).exists() else base_dir_windows / FTP_FILE_NAME

def carregar_dados_local():
    df = pd.read_excel(CAMINHO_ABSOLUTO_ARQUIVO, engine="openpyxl")
    return df

def carregar_dados_ftp():
    try:
        ftp = FTP(FTP_HOST)
        ftp.login(user=FTP_USER, passwd=FTP_PASS)

        with BytesIO() as buffer:
            ftp.retrbinary(f"RETR {FTP_FILE_NAME}", buffer.write)
            buffer.seek(0)
            df = pd.read_excel(buffer, engine="openpyxl")
        ftp.quit()
        return df
    except Exception as e:
        print(f"Erro ao carregar dados do FTP: {e}")
        return carregar_dados_local()

df = carregar_dados_ftp()

app_ui = ui.page_fluid(
    ui.tags.style("""
        body { font-family: Arial, sans-serif; margin: 0; padding: 0; }
        .shiny-output-ui { max-width: 100%; overflow: auto; }
        .panel { padding: 15px; border-radius: 8px; background-color: #f8f9fa; }
    """),

    ui.panel_title("📊 Dashboard de OS"),

    ui.layout_columns(
        ui.panel_well(
            ui.input_select("executor", "Selecione um Executor", choices=df["executor"].dropna().unique().tolist())
        ),
        ui.panel_well(
            ui.output_text("total_os"),
            ui.output_ui("grafico_os")
        )
    )
)

def server(input, output, session):
    df = carregar_dados_ftp()

    @output
    @render.ui
    def grafico_os():
        if not input.executor():
            return ui.HTML("<p>Selecione um executor para ver o gráfico.</p>")

        df_filtrado = df[df["executor"] == input.executor()]
        df_agrupado = df_filtrado.groupby("tipo_os").size().reset_index(name="quantidade")

        fig = px.bar(
            df_agrupado,
            x="tipo_os",
            y="quantidade",
            title=f"Distribuição de OS - {input.executor()}",
        )

        fig.update_layout(bargap=0.4, autosize=True)

        return ui.HTML(fig.to_html())

    @output
    @render.text
    def total_os():
        if not input.executor():
            return "Selecione um Executor para ver o total de OS"
        total = df[df['executor'] == input.executor()].shape[0]
        return f"Total de OS para {input.executor()}: {total}"

app = App(app_ui, server)
app.run()
