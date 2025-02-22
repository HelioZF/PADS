import matplotlib.pyplot as plt
from wordcloud import WordCloud

# Lista de palavras fornecidas
dados = """
PowerBI
PowerBI
ggplot
matplotlib
Looker Studio
PowerBI
Looker ggplot QlikSense
PowerBI Tableau Metabase matplotlib
Powerbi
Ggplot
PowerBI
python PowerBI Excel powerpoint
Tableau
PowerBI
PowerBI Aws Quicksight Google Studio python
python looker
Ggplot
PowerBI
"""

# Normalizar o texto para evitar duplicatas causadas por diferenças de maiúsculas/minúsculas
dados = dados.lower()

def processar_texto(texto):
    # Remover caracteres especiais e separar por vírgulas e espaços
    palavras = texto.replace("\n", ", ").replace("-", " ").split(", ")
    palavras = [p.strip() for p in palavras if p.strip()]
    return " ".join(palavras)

texto_processado = processar_texto(dados)

# Criar a nuvem de palavras
wordcloud = WordCloud(width=800, height=400, background_color='white', colormap='viridis').generate(texto_processado)

# Exibir a nuvem de palavras
plt.figure(figsize=(10, 5))
plt.imshow(wordcloud, interpolation='bilinear')
plt.axis("off")
plt.show()
