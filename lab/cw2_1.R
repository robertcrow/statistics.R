#wczytywanie datasetu
data(iris)

# dostep i wypisywanie
summary(iris)
iris$Petal.L
iris$Petal.Length
a<-iris[1:5, 2]
iris[1:5, 2, drop=F] # zapobiega konwersji na wektor 

b<-iris
b$Petal.Area<-.5*b$Petal.Length*b$Petal.Width
b$Petal.Area
head(b)
plot(iris)

#sprawdzanie pól spelniajacych dane wlasciwosci
table(iris$Species[iris$Sepal.Width > 3])

#unikanie petli w kodzie
#1) 






# IO w R
# dane mozna zapisac w R jako Rdata (czyli zrzut
# srodowiska obliczeniowego - jak save w matlabie)
# lub RDS czyli formacie, w którym wystepuje jeden
# obiekt
# Przyklady uzycia:

save(a,b, file = "nazwa_pliku.Rdata");
load("nazwa_pliku.Rdata")

saveRDS(a,b, "f.RDS");
readRDS("f.rds")->x;

# wypisywanie strumienia danych do niestandardowego
# wyjscia:
sink("a.out"); # aktywowanie
sink() # wylaczanie

# tworzenie pliku wykonywalnego

source("f.R")

# funkcje do 