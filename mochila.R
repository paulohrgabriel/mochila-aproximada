# Algoritmo de programação dinâmica para a Mochila
# n -> número de itens
# v -> valor dos itens
# p -> peso dos itens
# c -> capacidade da mochila
progdim <- function(n, v, p, c) {
  
  X = matrix(NA, n+1, c+1)
  
  X[1,] = 0
  X[,1] = 0
  
  for (i in 2:(n+1)) {
    
    k = i-1
    
    for (j in 2:(c+1)) {
      
      if (p[k] > j) {
        X[i,j] = X[i-1, j]
        
      } else {
        a = X[i-1, j]
        b = X[i-1, j-p[k]] + v[k]
        
        X[i,j] = max(a,b)
      }
    }
  }
  
  #print(X)
  
  return(X[n+1, c+1])
}

# Algoritmo guloso para a Mochila
# n -> número de itens
# v -> valor dos itens
# p -> peso dos itens
# c -> capacidade da mochila
# (1/2)-aproximação
guloso <- function(n, v, p, c) {
  
  # Usa a razão v/p como critério de avaliação
  # Empacotaremos os itens que apresentam a melhor proporção entre valor e peso
  # Itens muito pesados mas pouco valiosos ficam no fim da fila
  # Itens valiosos e leves vão para o começo da fila
  x = order(v/p, decreasing = TRUE)
  v = v[x]
  p = p[x]
  
  vm = 0    # valor total da mochila
  pm = 0    # peso total da mochila
  
  for (i in 1:n) {
    # Verifica se o peso do item está abaixo da capacidade da mochila
    # e se o peso acumulado da mochila + esse item também está abaixo de c
    if (p[i] <= c && (pm+p[i]) <= c) {
      pm = pm + p[i]
      vm = vm + v[i]
    }
  }
  
  return(vm)
}
  
# Experimento:
# - Geração aleatória de itens, com pesos e valores
# - Comparação da razão entre a mochila gulosa (G) com a mochila dinâmica (D)

R = c()    # Vetor que armazenará as razões
c = 500    # Capacidade da mochila
for (i in 1:30) {

  # sample - gera "size" valores no intervalo de 1 a x.
  v = sample(x = 100, size = 1000, replace = TRUE)
  p = sample(x = 100, size = 1000, replace = TRUE)

  D = progdim(n = length(v), v = v, p = p, c = c)
  G =  guloso(n = length(v), v = v, p = p, c = c)
  
  R = c(R, G/D)
}

mean(R) # média dos valores de G/D
sd(R)   # desvio-padrão dos valores de G/D

# https://people.sc.fsu.edu/~jburkardt/datasets/knapsack_01/knapsack_01.html
#c = 165
#v = c(92, 57, 49, 68, 60, 43, 67, 84, 87, 72)
#p = c(23, 31, 29, 44, 53, 38, 63, 85, 89, 82)
#sol = c(1, 2, 3, 4, 6)
#sol = sum(v[sol])
#
#c = 6404180
#
#v = c(825594, 1677009, 1676628, 1523970, 943972, 97426, 69666, 1296457, 1679693,
#      1902996, 1844992, 1049289, 1252836, 1319836, 953277, 2067538, 675367,
#      853655, 1826027, 65731, 901489, 577243, 466257, 369261)
#p = c(382745, 799601, 909247, 729069, 467902, 44328, 34610, 698150, 823460,
#      903959, 853665, 551830, 610856, 670702, 488960, 951111, 323046, 446298,
#      931161, 31385, 496951, 264724, 224916, 169684)
#sol = c(1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1)
#sol = sol %*% v
