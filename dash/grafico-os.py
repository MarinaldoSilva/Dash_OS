from io import BytesIO
from shiny import App, ui, render
import pandas as pd
import plotly.express as px
from pathlib import Path
import os
from ftplib import FTP
import openpyxl

#grafico de barras
def renderBarChart(title, data, x, y, theme, direction, grid_left):
    fig = px.bar(
        data, 
        x=x, 
        y=y,
        title=title,
        orientation='v' if direction == "vertical" else 'h',
        template='plotly_white' if theme == "infographic" else None
    )
    fig.update_layout(margin=dict(l=40, r=40, t=60, b=40), xaxis_title=x, yaxis_title=y)
    return fig

#caso seja no win ou linux, a barra muda, essa função é paliativa até a definição de hospedagem por Paulo
base_dir_linux = Path("/tratar_dados")
base_dir_windows = Path(r"C:\tratar_dados")

FTP_HOST = "127.0.0.1"
FTP_USER = "tesla" #user padão de teste que eu criei, após produção alterar de acorodo com as regras de segurança exigidas
FTP_PASS = "tesla"
FTP_FILE_NAME = "dados-tratados.xlsx"

#escolhe o caminho linux ou win de acordo com o s.o. utilizado.
CAMINHO_ABSOLUTO_ARQUIVO = (
    base_dir_linux / FTP_FILE_NAME if (base_dir_linux / FTP_FILE_NAME).exists()
    else base_dir_windows / FTP_FILE_NAME
)

def carregar_dados_local():
    if CAMINHO_ABSOLUTO_ARQUIVO.exists():
        return pd.read_excel(CAMINHO_ABSOLUTO_ARQUIVO, engine="openpyxl")
    else:
        print("Verifique o local do arquivo.")
        return pd.DataFrame()

def carregar_dados_ftp():
    #caso esteja no servidor ftp(não testando em ambiente de testes)
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
        print(f"Não é possível acessar os dados pelo FTP: {e}")
        return carregar_dados_local()

df = carregar_dados_ftp()

app_ui = ui.page_fluid(
    ui.tags.style("""
        body { font-family: Arial, sans-serif; margin: 0; padding: 0; }
        .shiny-output-ui { max-width: 100%; overflow: auto; }
        .panel { padding: 15px; border-radius: 8px; background-color: #f8f9fa; }
    """),

    ui.panel_title("📊 Dashboard de OS - PROCAPE"),

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
    @output
    @render.ui
    def grafico_os():
        if not input.executor():
            return ui.HTML("<p>Selecione um executor para ver o gráfico.</p>")

        df_filtrado = df[df["executor"] == input.executor()]
        df_agrupado = df_filtrado.groupby("tipo_os").size().reset_index(name="quantidade")

        fig = renderBarChart(
            title=f"Distribuição de OS - {input.executor()}",
            data=df_agrupado,
            x="tipo_os",
            y="quantidade",
            theme="infographic",
            direction="vertical",
            grid_left="10%"
        )

        return ui.HTML(fig.to_html(full_html=False))

    @output
    @render.text
    def total_os():
        if not input.executor():
            return "Selecione um Executor para ver o total de OS"
        total = df[df['executor'] == input.executor()].shape[0]
        return f"Total de OS para {input.executor()}: {total}"

#execução do app shiny/py 
app = App(app_ui, server)
app.run()
