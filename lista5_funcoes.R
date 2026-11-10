# =====================================================================
# Lista 5 – Criando funções no R  (Algoritmos p/ Ciências Atuariais)
# Resolução comentada
# =====================================================================
# MOLDE DE TODA FUNÇÃO:
#   nome = function(argumentos){
#     ...contas...
#     return(resultado)        # ou list(a=..., b=...) para devolver vários
#   }
# =====================================================================


# ---------------------------------------------------------------------
# 1) Entrada dos dados
# ---------------------------------------------------------------------
setwd("C:/Users/DELL/Documents/Projetos/Giuliano/Exercicios")   # sua pasta

dados1 = read.table("dados1.csv", header = TRUE, sep = ";", na.strings = "NA")
names(dados1) = tolower(names(dados1))      # deixa os nomes minúsculos: altura, peso...

dados1 = read.table("dados1.csv", header = TRUE, sep = ";", dec = ",", na.strings = "NA")

names(dados1) = tolower(names(dados1))

# Se o read.table der problema, apague as linhas acima e use os dados digitados:
# dados1 = data.frame(
#   altura = c(1.80,1.62,1.70,1.55,1.89,1.66,1.69,1.74,1.72,1.78,1.81,1.64,1.85,1.77,1.70),
#   peso   = c(85,68,71,66,92,65,72,78,74,85,88,70,84,83,78),
#   idade  = c(25,40,45,38,26,27,28,36,32,35,41,26,33,37,30),
#   sexo   = c("M","F","F","F","M","M","F","M","F","M","M","F","M","M","F"))

str(dados1)


# ---------------------------------------------------------------------
# 2) Função de sumário
#    Ideia: uma função que resume UM vetor; depois aplica em cada coluna.
# ---------------------------------------------------------------------
sumario = function(x){
  media = mean(x)
  dp    = sd(x)
  vari  = var(x)
  cv    = dp / media * 100              # CV em %
  p5    = quantile(x, 0.05)
  p95   = quantile(x, 0.95)
  res = c(media = media, dp = dp, variancia = vari,
          cv = cv, p5 = unname(p5), p95 = unname(p95))
  return(res)
}

# aplicando só nas colunas numéricas (sexo fica de fora)
quant = dados1[, sapply(dados1, is.numeric)]
round(sapply(quant, sumario), 3)

# Versão "for" (o professor às vezes pede sem sapply):
sumario_df = function(df){
  df  = df[, sapply(df, is.numeric)]
  tab = matrix(NA, nrow = 6, ncol = ncol(df))
  for(j in 1:ncol(df)){
    tab[, j] = sumario(df[, j])
  }
  rownames(tab) = c("media","dp","variancia","cv(%)","p5","p95")
  colnames(tab) = names(df)
  return(tab)
}
round(sumario_df(dados1), 3)


# ---------------------------------------------------------------------
# 3) IMC = peso / altura^2
# ---------------------------------------------------------------------
imc = function(peso, altura){
  return(peso / altura^2)
}
dados1$imc = imc(dados1$peso, dados1$altura)   # funciona vetorizado
dados1


# ---------------------------------------------------------------------
# 4) Normalização:  (x - min) / (max - min)   -> resultado entre 0 e 1
# ---------------------------------------------------------------------
normaliza = function(x){
  (x - min(x)) / (max(x) - min(x))
}
sapply(dados1[, c("altura","peso","idade")], normaliza)


# ---------------------------------------------------------------------
# 5) Padronização:  Z = (x - média) / desvio padrão  -> média 0, dp 1
# ---------------------------------------------------------------------
padroniza = function(x){
  (x - mean(x)) / sd(x)
}
Z = sapply(dados1[, c("altura","peso","idade")], padroniza)
Z
round(colMeans(Z), 10)    # conferência: ~0
apply(Z, 2, sd)           # conferência: 1
# (o R já tem pronto: scale(x) — bom para conferir na prova)


# ---------------------------------------------------------------------
# 6) Função simula incrementada
#    Antes: devolvia só o vetor de médias.
#    Agora: guarda cada amostra numa linha de uma matriz N x n
#    e devolve as DUAS coisas numa lista.
# ---------------------------------------------------------------------
simula = function(x, N){
  # x é um vetor
  # N é o nº de amostras c/ reposição
  n   = length(x)
  am  = NULL
  mat = matrix(NA, nrow = N, ncol = n)     # linhas = simulações
  for(i in 1:N){
    s        = sample(x, n, replace = TRUE)
    mat[i, ] = s                           # guarda a amostra na linha i
    am[i]    = mean(s)
  }
  return(list(medias = am, amostras = mat))
}

set.seed(123)
r = simula(dados1$peso, 5)
r$medias
r$amostras
rowMeans(r$amostras)   # tem que bater com r$medias


# ---------------------------------------------------------------------
# 7) Bootstrap do IMC
#    ATENÇÃO: peso e altura são da MESMA pessoa -> sorteia os ÍNDICES
#    (as linhas), não peso e altura separadamente.
# ---------------------------------------------------------------------
simula_imc = function(peso, altura, N){
  n    = length(peso)
  imcs = NULL
  for(i in 1:N){
    id      = sample(1:n, n, replace = TRUE)       # sorteia pessoas
    imcs[i] = mean(peso[id] / altura[id]^2)        # IMC médio da amostra
  }
  hist(imcs, main = "Distribuição do IMC (bootstrap)",
       xlab = "IMC", col = "lightblue", border = "white")
  return(imcs)
}

set.seed(123)
v = simula_imc(dados1$peso, dados1$altura, 1000)
summary(v)


# ---------------------------------------------------------------------
# 8) Tábua de sobrevivência (vida)
#    Entrada: idades de início de cada faixa, óbitos e população.
#    Colunas:  nMx -> nqx -> lx -> ndx -> nLx -> Tx -> ex
# ---------------------------------------------------------------------
tabua = function(idade, obitos, pop, raiz = 100000){
  k  = length(idade)
  n  = c(diff(idade), NA)            # amplitude de cada faixa (última aberta)
  mx = obitos / pop                  # taxa central de mortalidade
  ax = n / 2                         # supõe óbitos no meio do intervalo
  
  qx = n * mx / (1 + (n - ax) * mx)  # prob. de morrer na faixa
  qx[k] = 1                          # última faixa: todos morrem
  
  lx = numeric(k)
  lx[1] = raiz
  for(i in 2:k){
    lx[i] = lx[i-1] * (1 - qx[i-1])  # sobreviventes
  }
  dx = lx * qx                       # óbitos da tábua
  
  Lx = numeric(k)
  for(i in 1:(k-1)){
    Lx[i] = n[i] * lx[i+1] + ax[i] * dx[i]   # anos vividos na faixa
  }
  Lx[k] = lx[k] / mx[k]              # faixa aberta
  
  Tx = rev(cumsum(rev(Lx)))          # soma de Lx daquela idade pra frente
  ex = Tx / lx                       # esperança de vida
  
  return(data.frame(idade, n, mx, qx, lx, dx, Lx, Tx, ex))
}

# Exemplo com dados fictícios (faixas quinquenais)
idade  = c(0, 1, seq(5, 80, 5))
pop    = c(1000, 4000, 5000, 5100, 5200, 5000, 4800, 4600, 4300, 4000,
           3600, 3200, 2800, 2300, 1800, 1300, 900, 900)
obitos = c(15, 2, 1, 1, 3, 5, 6, 7, 9, 12,
           16, 22, 30, 40, 52, 65, 75, 160)
tv = tabua(idade, obitos, pop)
print(round(tv, 4))
