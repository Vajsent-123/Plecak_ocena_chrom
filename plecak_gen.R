#funkcja plecak zmieniona na ocene najlepszych chromosomów
library(genalg)

przedmioty <- data.frame(ceny = c(10, 20, 15, 2, 30, 10, 30), 
                         wagi = c(1, 5, 10, 1, 7, 5, 1))
limit <- 20

print(przedmioty)

#przyklad chromosomu
chromosom = c(1, 0, 0, 1, 1, 0, 0)
przedmioty[chromosom == 1, ]
print(paste("Suma cen: ",chromosom %*% przedmioty$ceny))
print(paste("Suma wag: ",chromosom %*% przedmioty$wagi))

#funkcja oceny dla algorytmu genetycznego
fitness <- function(x) {
  suma_cen <- x %*% przedmioty$ceny
  suma_wag <- x %*% przedmioty$wagi
  
  if (suma_wag > limit) 
    return(0) else return(-suma_cen)
}

#jaka ocene dostanie nasz chromosom?
fitness(chromosom)

#uruchomienie algorytmu genetycznego
wielkosc_populacji = 100
liczba_pokolen = 70
alg_gen <- rbga.bin(size = 7, 
                    popSize = wielkosc_populacji, 
                    iters = liczba_pokolen, 
                    mutationChance = 0.01,  
                    elitism = T, 
                    evalFunc = fitness)

#przebieg alg.genetycznego na wykresie
plot(alg_gen)

#wydobycie najlepszego rozwiazania
cat(summary(alg_gen))
przedmioty[najlepsze == 1, ]
print(paste("Suma cen: ",najlepsze %*% przedmioty$ceny))
print(paste("Suma wag: ",najlepsze %*% przedmioty$wagi))