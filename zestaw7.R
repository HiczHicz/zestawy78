miesiace <- c("III 22", "IV 22", "V 22", "VI 22", "VII 22", "VIII 22", "IX 22", "X 22", "XI 22", "XII 22", "I 23", "II 23",
              "III 23", "IV 23", "V 23", "VI 23", "VII 23", "VIII 23", "IX 23", "X 23", "XI 23", "XII 23", "I 24", "II 24",
              "III 24", "IV 24", "V 24", "VI 24", "VII 24")

produkcja <- c(52.10, 53.92, 49.71, 50.86, 51.64, 54.46, 64.07, 60.11, 66.82, 68.82, 64.33, 61.43,
                  60.78, 61.30, 59.07, 57.61, 63.38, 60.46, 63.73, 72.01, 73.73, 78.11, 71.59, 69.06,
                  69.02, 66.17, 65.50, 64.82, 64.53)

#A - przyrosty absolutne

#PRZYROST ABSOLUTNY MIESIĘCZNY:
przyrost_absolutny_m=c(0, diff(produkcja))

#PRZYROST ABSOLUTNY ROCZNY:
przyrost_absolutny_r=c(0,0,0,0,0,0,0,0,0,0,0,0,diff(produkcja, lag=12))


#B - przyrosty względne

#PRZYROST WZGLĘDNY MIESIĘCZNY 
przyrost_wzgledny_m=round(c(0, diff(produkcja)/head(produkcja,-1)*100),2)

#PRZYROST WZGLĘDNY ROCZNY
przyrost_wzgledny_r=round(c(0,0,0,0,0,0,0,0,0,0,0,0, diff(produkcja, lag=12)/head(produkcja,-12)*100),2)


zad1<-data.frame('Miesiące'=miesiace, 'Produkcja'=produkcja, 'Przyrost absolutny miesięczny'=przyrost_absolutny_m,
           'Przyrost absolutny roczny'=przyrost_absolutny_r, 'Przyrost względny miesięczny'=przyrost_wzgledny_m,
           'Przyrost względny roczny'=przyrost_wzgledny_r)

#write.csv(zad1, 'zad1')

#C - wykresy

#WYKRES PRODUKCJI NA PRZESTRZENI KOLEJNYCH MIESIĘCY
#plot(ts(produkcja, start=1, end=29))
plot(produkcja,type="o",main="Wartość produkcji na przestrzeni kolejnych miesięcy [w tys.]",
     xlab="Kolejne miesiące",ylab="Wartość produkcji",lwd=2,col="coral3", pch=16)

#WYKRES PRZYROSTÓW ABSOLUTNYCH MIESIECZNYCH
plot(diff(produkcja),type="o",main="Wartości bezwględnych przyrostów produkcji\nw kolejnych miesiącach [w tys.]",
     xlab="Kolejne miesiące", xlim=c(0,30), ylab="Przyrosty absolutne",lwd=2,col="coral3", pch=16)

#WYKRES PRZYROSTÓW WZGLĘDNYCH MIESIĘCZNYCH
plot(diff(produkcja)/head(produkcja,-1)*100,type="o",main="Wartości względnych przyrostów produkcji\nw kolejnych miesiącach [w procentach]",
     xlab="Kolejne miesiące", xlim=c(0,30), ylab="Przyrosty względne",lwd=2,col="coral3", pch=16)


#D WARTOŚCI

mean(produkcja)

min(produkcja) 
miesiac_min=miesiace[which.min(produkcja)]

max(produkcja)
miesiace[which.max(produkcja)]

min(diff(produkcja))
miesiace[which.min(diff(produkcja))+1]
    
max(diff(produkcja))
miesiace[which.max(diff(produkcja))+1]

min(diff(produkcja, lag=12))
miesiace[which.min(diff(produkcja, lag=12))+12]

max(diff(produkcja, lag=12))
miesiace[which.max(diff(produkcja, lag=12))+12]

############
# Poprawiona funkcja dla najdłuższego ciągu
najdluzszy_ciag <- function(wektor) {
  rle_wektor <- rle(wektor) # Rozkład długości powtarzających się wartości logicznych (TRUE/FALSE)
  dlugosc <- max(rle_wektor$lengths[rle_wektor$values]) # Znajdź maksymalną długość dla TRUE
  indeks_start <- which.max(rle_wektor$lengths * rle_wektor$values) # Indeks startowy dla TRUE
  
  # Wyznaczenie początku i końca ciągu
  start <- sum(rle_wektor$lengths[1:(indeks_start - 1)]) + 1
  koniec <- start + dlugosc - 1
  
  list(dlugosc = dlugosc, start = start, koniec = koniec)
}

# Analiza wzrostów (zmiany > 0)
ciag_wzrost <- najdluzszy_ciag(zmiany > 0)
okres_wzrost <- miesiace[ciag_wzrost$start:ciag_wzrost$koniec]

# Analiza spadków (zmiany < 0)
ciag_spadek <- najdluzszy_ciag(zmiany < 0)
okres_spadek <- miesiace[ciag_spadek$start:ciag_spadek$koniec]

# Wyniki
cat("Najdłuższy okres wzrostów: długość =", ciag_wzrost$dlugosc, 
    "miesiące:", paste(okres_wzrost, collapse = ", "), "\n")
cat("Najdłuższy okres spadków: długość =", ciag_spadek$dlugosc, 
    "miesiące:", paste(okres_spadek, collapse = ", "), "\n")


############# nie działa to jak coś ale mniej więcej



zmiany <- diff(produkcja) > 0
which.max(rle(zmiany)$lengths)




