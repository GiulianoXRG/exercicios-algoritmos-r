# ==============================================================================
# ALGORITMO PARA CIÊNCIAS ATUARIAIS
# 1ª AVALIAÇÃO
# ==============================================================================

# Boas práticas da programação

rm(list = ls()) # Limpeza inicial do ambiente de trabalho
getwd() # Verificar em que diretório estamos
setwd("/home/usuario/Documentos/orientação/docencia/BEATRIZ") # ir para o diretório que desejamos estar

# Instruções

# Salvar em arquivo compactado
# - script (arquivo .R)
# - área de trabalho (arquivo .Rdata)
# - gráfico (imagem .jpeg)
# - relatório (arquivo .txt)

# o não envio dos arquivos prejudicará a correção e será contado como não entrega da respectiva questão
# e, em caso de não entrega do script e/ou da área de trabalho, será caracterizado como não entrega da prova !!!

# ==============================================================================
# Questão 1
# ==============================================================================

# Instruções: Entrar com as matrizes usando a função matrix() e calcular:
#   - Determinante: det()
#   - Matriz Inversa (se existir): solve()
#   - Matriz Transposta: t()

# Matriz 1 (3x3)
# Linha 1: 7, 2, 1
# Linha 2: 3, 9, 3
# Linha 3: 1, 8, 1

matriz1 <- matrix( # cria uma matriz
  c(7, 2, 1, 3, 9, 3, 1, 8, 1), # elementos da matriz
  nrow = 3, # números de linhas da matriz
  ncol = 3, # números de colunas da matriz
  byrow = TRUE) # será estruturada por linhas? TRUE se sim

print(matriz1)

det(matriz1) # determinante
# o determinante da matriz 1 é diferente de zero,
# a matriz PODE admite inversa !!!

solve(matriz1) # inversa

t(matriz1) # transposta

# Matriz 2 (3x3)
# Linha 1: 12, 9, 5
# Linha 2: 3, 11, 3
# Linha 3: 10, 8, 7

dados_m2 <- c(12,  9, 5, 3, 11, 3, 10,  8, 7) # elementos da matriz 2

matriz2 <- matrix( # cria uma matriz
  dados_m2, # elementos da matriz
  nrow = 3, # números de linhas da matriz
  ncol = 3, # números de colunas da matriz
  byrow = TRUE) # será estruturada por linhas? TRUE se sim

print(matriz2)

det(matriz2) # determinante
# o determinante da matriz 2 é diferente de zero,
# a matriz PODE admite inversa !!!

solve(matriz2) # inversa

t(matriz2) # transposta

# Matriz 3 (2x2)
# Linha 1: 2, 4
# Linha 2: 3, 6

matriz3 <- matrix( # cria uma matriz
  c(2, 4, 3, 6), # elementos da matriz
  nrow = 2, # números de linhas da matriz
  ncol = 2, # números de colunas da matriz
  byrow = TRUE) # será estruturada por linhas? TRUE se sim

print(matriz3)

det(matriz3) # determinante
# como o determinante da matriz 3 é zero,
# a matriz não admite inversa !!!

t(matriz3) # transposta

# Matriz 4 (2x2)
# Linha 1: 0, 3
# Linha 2: 1, 7

matriz4 <- matrix( # cria uma matriz
  c(0, 1, 3, 7), # elementos da matriz
  nrow = 2, # números de linhas da matriz
  ncol = 2, # números de colunas da matriz
  byrow = FALSE) # será estruturada por linhas? FALSE se não

print(matriz4)

det(matriz4) # determinante
# o determinante da matriz 4 é diferente de zero,
# a matriz PODE admite inversa !!!

solve(matriz4) # inversa

t(matriz4) # transposta

# ==============================================================================
# Questão 2
# ==============================================================================

# Baixamos o arquivo dados2-prova1.csv
# O arquivo dados2-prova1.csv deve estar no diretório de trabalho atual (que estamos trabalhando)
# getdw() - verifica onde estamos

# PRIMEIRO
# Vamos dar uma olhadinha no arquivo
# Ele tem cabeçalho?
# Está separado como?
# Qual o separador de decimal?
# Tem entrada vazia?

help(read.table) # verificar quais parâmetros vamos precisar

dados = read.table( # lê arquivo em forma de tabela
  "dados2-Prova1.csv", # nome do arquivo
  header = TRUE, # indica que o meu arquivo tem cabeçalho (as colunas tem nomes)
  sep = ";", # separador de colunas do arquivo
  na.strings = "NA" # campos em branco também são considerados valores ausentes (NA)
  )

is.data.frame(dados) # verifica se é um dataframe

print(head(dados))

# Descrição dos dados segundo a prova (OBS 1):
# COL 1. Renda média dos chefes
# COL 2. Sexo do chefe (1 = masculino, 2 = feminino)
# COL 3. Percentual escoamento sanitário adequado
# COL 4. Percentual coleta de lixo adequada
# COL 5. Percentual domicílios c/ mais de 5 habitantes

# OBS 2: gerar um arquivo com as respostas da questão 2 (letras B até D) com sink()

# =========
# Letra A
# =========
dados$sexo <- factor( # define como variável categórica
  dados$SEXO, # quem está sendo definido
  levels = c(1, 2), # níveis da variável
  labels = c("Masculino", "Feminino") # Rótulos da variável
  )

# Início do relatório para as letras B, C e D
# Todas as saídas de print() e cat() a partir daqui serão gravadas no arquivo texto.

sink( # gera um arquivo txt com as saídas
  "relatorio.txt" # nome que desejamos para o arquivo
  )

cat("============================================================\n") # comentários no arquivo txt
cat("RELATÓRIO DE RESULTADOS - QUESTÃO 2 (LETRAS B, C e D)\n")
cat("============================================================\n\n")

# =========
# Letra B
# =========
cat("LETRA B \n\n")

# 1ª FORMA: Cálculo manual/bruto com sum(), length(), sqrt() e potências
# Cmedia = sum(x)/length(x)
# Variância amostral = sum((x - media)^2) / (length(x) - 1)
# Desvio padrão = sqrt(variância)

n = length(dados[, 1]) # tamanho do vetor/número de observações

# Coluna 1 (Renda)
m_v1 = sum(dados[, 1], na.rm = TRUE) / n

# Coluna 3 (Escoamento sanitário)
m_v3 = sum(dados[, 3], na.rm = TRUE) / n

# Coluna 4 (Coleta de lixo)
m_v4 = sum(dados[, 4], na.rm = TRUE) / n

# Coluna 5 (Mais de 5 habitantes)
m_v5 = sum(dados[, 5], na.rm = TRUE) / n

media_forma1 = c(m_v1, m_v3, m_v4, m_v5)

# 2ª FORMA: Cálculo coluna a coluna com as funções diretas (mean, var, sd)

var_forma2 = c(var(dados$RENDMED, na.rm = TRUE),
               var(dados$ESGSANIT, na.rm = TRUE),
               var(dados$LIXO, na.rm = TRUE),
               var(dados$DOM5MORA, na.rm = TRUE))

# 3ª FORMA: Usando apply() sobre as colunas (margem 2)

# apply( # aplica algo em algum lugar
#   x, # o lugar
#   margin, # em que direção (1 para linha e 2 para coluna)
#   ação, # o que será aplicado
#   na.rm = TRUE # desconsidera NA e calcule as estatísticas com as observações válidas
# )

# Vamos separamos apenas as colunas quantitativas 
# (todas exceto sexo, ou seja, colunas 1, 3, 4 e 5)

dados_quant = dados[, c(1, 3, 4, 5)]

dp_forma3 = apply(dados_quant, 2, sd,   na.rm = TRUE)

obj_forma = cbind(media_forma1, var_forma2, dp_forma3)

cat("3 FORMAS:\n")
obj_forma
cat("\n")

# =========
# Letra C
# =========
cat("LETRA C \n\n")

cv_objeto <- (dp_forma3 / media_forma1) * 100

cat("Resultados do Coeficiente de Variação:\n")
cv_objeto
cat("\n------------------------------------------------------------\n\n")

# =========
# Letra D
# =========
cat("LETRA D \n\n")

cat("\nFrequência relativa:\n")
table(dados[, 2]) # Tabela de frequência bruta (sem rótulos)
cat("\n")

cat("\nFrequência relativa (%):\n")
prop.table(table(dados$sexo)) * 100
cat("\n")

# Fechando o sink para voltar a exibir saídas no console
sink()


# ==============================================================================
# QUESTÃO 3
# ==============================================================================

# Instruções:
# - Eixo x: escoamento sanitário adequado
# - Eixo y: coleta de lixo adequada
# - Abrir uma janela gráfica no R
# - Nomear os eixos
# - Por título principal
# - Salvar o gráfico em arquivo JPEG

dev.new() # Abre uma janela gráfica em qualquer sistema
          # win.graph - para windows

# Gráfico de dispersão
jpeg("grafico_questao3.jpeg") # Salva imagem da janela gráfica
plot(
  dados[, 3], # escoamento sanitário (x)
  dados[, 4], # coleta de lixo (y)
  xlab = "Percentual de Domicilios com Escoamento Sanitario Adequado", # Rótulo eixo x
  ylab = "Percentual de Domicilios com Coleta de Lixo Adequada", # Rótulo eixo y
  main = "Diagrama de Dispersao: Escoamento Sanitario vs Coleta de Lixo" # Título
)
dev.off() # Fecha a janela gráfica


# ==============================================================================
# FINALIZAÇÃO
# ==============================================================================

savehistory(file = "script_prova1.R") # Salva o arquivo .R
save.image("prova1.RData") # Salva o arquivo .RData

# Salvar imagem na janela plot (se não salvou como acima)
# Salvar script em File (.R)
# Salvar Área de trabalho em Environment(.RData)
# Por tudo numa pasta com o relatório (.txt)
# Compactar (zippar)
# Enviar 
