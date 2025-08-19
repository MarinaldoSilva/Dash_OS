import pandas as pd
from pathlib import Path as pl
from shiny import App, reactive, render, ui        
import plotly.express as px
from shiny.express import input, ui
from shinywidgets import render_plotly

ui.page_opts(title="Dashboard", fillable=True)

ui.input_numeric("n", "Atendimentos", 10, min=1, max=100)

@reactive.calc
def arquivo():
    #caminho base do arquivo que será usado como banco de dados da aplicação
    arquivo = pl(__file__).parent / "dados-tratados.xlsx"
    return pd.read_csv(arquivo)
   
#tudo dentro desse bloco 'with' é parte de colunas
with ui.layout_columns():
    #esse bloco "renderiza" o DF e deixa os dados apresentaveis em um tabela que tem interação ocm o usuário
    @render_plotly
    #minha função de graficos
    def grafico_top_vendas():
        df = arquivo()
        top_vendas = df.groupby('product')['quantity_ordered'].sum().nlargest(input.n()).reset_index()
        px.bar 
        return px.bar(top_vendas, x='product', y= 'quantity_ordered')
        #fig = px.pie(df, names='product', values='quantity_ordered', title="Distribuição das Vendas por Produto")
        #return fig

    #@render.data_frame
    #def dados_csv():
    #    return arquivo()

