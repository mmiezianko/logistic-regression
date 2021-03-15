# logistic_regression

---
title: "Wstęp do analizy danych"
author: "Majka Miezianko, Marek Czerwonka"
date: "22 01 2021"
output:
  html_document: default
  word_document: default
---

# Analiza budżetów gospodarstw domowych we Włoszech 

## 1. Wstęp

Ten projekt jest zapisem naszej rzeczywistej potyczki z tymi włoskimi danymi. Patrząc na to po jego zakończeniu, widzimy, że pewne rzeczy można było zrobić lepiej i sensowniej oraz bardziej spójnie. Nie będziemy jednak tego już teraz przerabiać. Mimo wszystko tę potyczkę uważamy za generalnie w miarę udaną, następnym razem już wielu błędów unikniemy.

W ramach naszego projektu postanowiliśmy najpierw wstępnie określić rzeczywistą jakość udostępnionych danych, a następnie podjąć próbę skonstruowania modelu predykcyjnego, który będzie umożliwiał przewidzenie wartości zmiennej zależnej, na podstawie wartości wybranych przez nas zmiennych objaśniających.

W związku z tym, że dla analiz o podłożu ekonomicznym, oprócz zmiennych o charakterze socjologiczno-metryczkowym kluczowy wydawał się nam poziom dochodu badanego, a taka dana wprost nie była dostępna, postanowiliśmy oszacować tę zmienną poprzez sumę wszystkich wydatków uwzględnionych w tych danych.  Zdajemy sobie sprawę, że nie odzwierciedla to dokładnie poziomu dochodów (część wydatków nie jest uwzględniona, część rzeczywistego dochodu może być przekazywana na oszczędności, z kolei część uwzględnionych wydatków może być finansowana kredytami lub pożyczkami).

Jako pierwszy etap usuwania z danych nietypowych wartości, eliminujemy z dalszej analizy po 5% z każdego z krańców rozkładu utworzonej przez nas zmiennej reprezentującej dochód.

Ze względu na dużą liczbę dostępnych danych, w przypadkach gdy jakiś rekord jest niekompletny dla danych, które używamy do analizy, eliminujemy ten rekord z analizy, nie stosujemy zabiegów zastępowania braków danych arbitralnie przyjętymi wartościami (np. medianą czy średnią).


## 2. Statystyka opisowa oraz hipotezy ekonomiczne 

```{r setup, include=FALSE, echo=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE,message=FALSE)
```


```{r, echo=FALSE, warning=FALSE,message=FALSE, results = FALSE}
library(readr)
library(dplyr)
library(Amelia)
library(corrplot)
library(ggplot2)
library(mice)
library (moments)
library(aod)
library(ggpubr)
library("FitCurve")
#install.packages("bbmle",repos = "http://cran.us.r-project.org")
#install.packages("minpack.lm")
#install.packages("devtools")
#devtools::install_github("krzysiektr/FitCurve")
install.packages("mice", repos = "http://cran.us.r-project.org")
library(devtools)
Dane <- read.delim("~/Downloads/HBS_Microdati_Anno_2011.txt")
wszystkie_wydatki<-Dane %>%
  select(starts_with("C_")) %>% rowSums(., na.rm = TRUE)
#Dodanie nr gospodarstwa
NR<-seq(1,nrow(Dane),1)
Dane<-cbind(NR,Dane)
#Nowa ramka danych z podziałem żywności na 4 kategorie
zywnosc <-Dane%>%
  mutate("Zboza"=C_1101+C_1102+C_1103+C_1104+C_1105+C_1106+C_1107)%>%
  mutate("Mieso"= C_1201+ C_1202+ C_1203+ C_1204+ C_1205+ C_1206+ C_1207+ C_1208+ C_1209+ C_1297)%>%
  mutate("Ryby"=C_1301+ C_1302+ C_1303+ C_1304)%>%
  mutate("Owoce"=C_1601+ C_1602+ C_1603+ C_1604+ C_1605+ C_1606+ C_1607+ C_1608+ C_1609)%>%
  mutate("Napoje"=C_1803+ C_1804+ C_1805+ C_1806+ C_1807+ C_1808)%>%
  mutate("Dochod"=wszystkie_wydatki)%>%
  mutate("Suma_wydatkow_zywnosc"= C_1101+C_1102+C_1103+C_1104+C_1105+C_1106+C_1107+C_1201+ C_1202+ C_1203+ C_1204+ C_1205+ C_1206+ C_1207+ C_1208+ C_1209+ C_1297+C_1301+ C_1302+ C_1303+ C_1304+C_1601+ C_1602+ C_1603+ C_1604+ C_1605+ C_1606+ C_1607+ C_1608+ C_1609+C_1803+ C_1804+ C_1805+ C_1806+ C_1807+ C_1808) %>% 
  select(NR,Mese,Regione,TIPFAM,NC,Zboza,Mieso, Ryby,Owoce,Napoje,Dochod,Suma_wydatkow_zywnosc)
```


<span style="color:blue">*Ramka danych zawierająca wydatki na produkty spożywcze.*</span>

```{r, echo=FALSE}
head(zywnosc)
```

<span style="color:blue">*Usuwamy dane odstające*</span>

```{r}
#Usuwamy 10% skrajnych obserwacji 
qnt1 <- quantile(zywnosc$Dochod, probs=c(.05, .95), na.rm = T)
small<-zywnosc$Dochod<qnt1[1]
Dane_clean<-zywnosc[!small,]
large<-Dane_clean$Dochod>qnt1[2]
Dane_clean<-Dane_clean[!large,]
```

<span style="color:blue">*Porównujemy wykresy pudełkowe przed i po usunięciu obserwacji odstajcych.*</span>

```{r, echo=FALSE}
par(mfrow=c(1,2))
boxplot(zywnosc$Dochod)
boxplot(Dane_clean$Dochod)
```


Porównując wygenerowane wykresy pudełkowe dla danych pełnych i po usunięciu obserwacji skrajnych, widzimy że osiągnęliśmy zamierzony cel, czyli zmniejszenie bardzo silnej prawostronnej asymetrii dochodów (która jest zjawiskiem naturalnym, ale jednak w pełnym zbiorze danych naszym zdaniem była zbyt silna).

W dalszej części analizy używamy już wyłącznie opisanego wcześniej podzbioru pełnego zestawu danych.

### Opis próby

<span style="color:blue">*Sprawdzamy jakich rodzin jest najwięcej w naszym zbiorze danych*</span>

```{r pressure, echo=FALSE}
Dane_clean %>% group_by(NC) %>% summarise(count = n()) %>%  
    ggplot(aes(x= reorder(NC,count),y=count)) + geom_bar(stat = "identity") + coord_flip() + xlab("Typ rodziny") + ylab("Ilość")
```

<span style="color:blue">*Zauważamy, że w danych zachodzą błędy, ponieważ pierwszy wektor wskazuje na występujące typy rodzin.*
*Maksymalną liczbę członków rodziny dla pierwszego wektroa przyjmuje typ 8 - para z 2 dziećmi (4 osoby). Drugi wektor przedstawia zaś liczbę osób w rodzinie - mamy tutaj wartość także 5 i 6.*</span>


Dostrzegamy lekką niespójność danych, niestety będzie trudno tę niespójność usunąć, gdyż nie wiemy na jakim etapie przetwarzania zbioru danych surowych przez jego dostawcę ona powstała. Decydujemy się zatem zignorować tę niespójność, ewentualnie uwzględnimy to w szczególnych przypadkach w dalszych analizach.

```{r}
sort(unique(Dane_clean$TIPFAM)) # (max 4 czlonkow rodziny)
sort(unique(Dane_clean$NC)) # tu wystepuja rodziny 5 i 6 osobwe
```

Dane, które będą podlegały analizie mają charakter ekonomiczny. Zastanawiając się nad zagadnieniem ich reprezentatywności, przy tak licznej próbie oraz przy bardzo szerokim przekroju terytorialnym, dochodowym oraz liczności gospodarstw domowych, uważamy, że można je uznać za potencjalnie reprezentatywne dla społeczeństwa włoskiego. Nie mając możliwości zweryfikowania w sposób szczegółowy tego w jaki sposób zostały zebrane, postanowiliśmy spróbować zweryfikować ich rzetelność testując na nich pewne ogólne działające zasady i prawa ekonomiczne.

Naszym celem jest tu przede wszystkim upewnienie się, że dane są na tyle rzetelne, aby był sens próbować zbudować na nich model przyczynowo-skutkowy. Przy takiej liczebności próby nie ma możliwości zasłonięcia się nietypowym wynikiem losowania respondentów, albo obciążonym doborem respondentów (np. wybierano tylko tych, którzy lubią badania statystyczne, albo udział w ankiecie był płatny więc badano głównie tych, którzy mieli niskie dochody i skusił ich dochód dodatkowy). Nawet jeśli mamy do czynienia z próbą obciążoną systematycznie, przy takiej jej liczebności ogólnie działające zasady ekonomiczne zgodne z teorią wyboru konsumenta powinny się ujawnić.

Jako, że jest to nasza wstępna analiza dotycząca jakości danych a i tak zmienna dochód to nasza estymata, a nie rzeczywiście uzyskany przez respondenta dochód, na tym etapie jako pojedynczy rekord naszych danych traktujemy gospodarstwo domowe (mimo iż rozumiemy, że ma to pewne wady). Wyznaczone wartości liczbowe nie będą uwzględniane w zasadniczym etapie analizy, gdzie dane zostaną przekształcone do nieco innej postaci.




### 2.1 Hipoteza 1: Produkty zbożowe są dobrami normalnymi - elastyczność dochodowa jest z przedziału [0-1]

<span style="color:blue">*Źródło użytego wzoru: https://rpubs.com/cyobero/elasticity*</span>

$$ coeff * \frac{mean(INCOME)}{mean(FOOD EXP)} $$
<span style="color:blue">Parametry modelu liniowego.</span>

```{r, echo=FALSE}
theme_set(theme_bw())
ggplot(data = Dane_clean, aes(y = Suma_wydatkow_zywnosc, x = Dochod))  + geom_point(col = 'blue') + geom_smooth(method = 'lm', col = 'red', size = 0.5) # fitted regression line
lm1<-lm(Dane_clean$Zboza~Dane_clean$Dochod,data=Dane_clean)
summary(lm1)
lm1$coefficients
```
<span style="color:blue">*Obliczamy elastyczność dochodową produktów zbożowych.*</span>



```{r}
ied_Zboza<-lm1$coefficients[2]*mean(Dane_clean$Dochod)/mean(Dane_clean$Zboza) #Obliczamy elastycznosc dochodowa
ied_Zboza
```

Jak widać uzyskaliśmy oszacowanie przeciętnej wartości elastyczności dochodowej z przedziału od 0 do 1, a zatem zgodną z teorią ekonomii. Interpretując wartość uzyskanej elastyczności, gdy nasza miara dochodu wzrośnie o 1 % wydatki na produkty zbożowe wzrosną o 0,57 %, czyli jest to dobro normalne. Hipoteza 1 została zatem na naszym zestawie danych potwierdzona.

Poniżej wyznaczymy też elastyczności dochodowe innych dóbr będących produktami spożywczymi.


```{r}
lm2<-lm(Dane_clean$Mieso~Dane_clean$Dochod,data=Dane_clean)
summary(lm2)
ied_Mieso<-lm2$coefficients[2]*mean(Dane_clean$Dochod)/mean(Dane_clean$Mieso)
ied_Mieso #mięso
```

Jak widać uzyskaliśmy oszacowanie przeciętnej wartości elastyczności dochodowej z przedziału od 0 do 1, a zatem zgodną z teorią ekonomii. Interpretując wartość uzyskanej elastyczności, gdy nasza miara dochodu wzrośnie o 1 % wydatki na produkty mięsne wzrosną o 0,57 %, czyli jest to dobro normalne.

```{r}
lm3<-lm(Dane_clean$Ryby~Dane_clean$Dochod,data=Dane_clean)
summary(lm3)
ied_Ryby<-lm3$coefficients[2]*mean(Dane_clean$Dochod)/mean(Dane_clean$Ryby)
ied_Ryby #ryby
```

Jak widać uzyskaliśmy oszacowanie przeciętnej wartości elastyczności dochodowej z przedziału od 0 do 1, a zatem zgodną z teorią ekonomii. Interpretując wartość uzyskanej elastyczności, gdy nasza miara dochodu wzrośnie o 1 % wydatki na produkty rybne wzrosną o 0,71%, czyli jest to dobro normalne. Zwróćmy uwagę, że elastyczność jest w tym przypadku wyższa. Produkty rybne jako kategoria mają (uśredniając) charakter nieco bardziej ekskluzywny niż zbożowe czy mięsne.

```{r}
lm4<-lm(Dane_clean$Owoce~Dane_clean$Dochod,data=Dane_clean)
summary(lm4)
ied_Owoce<-lm4$coefficients[2]*mean(Dane_clean$Dochod)/mean(Dane_clean$Owoce)
ied_Owoce #owoce
```

Jak widać uzyskaliśmy oszacowanie przeciętnej wartości elastyczności dochodowej z przedziału od 0 do 1, a zatem zgodną z teorią ekonomii. Interpretując wartość uzyskanej elastyczności, gdy nasza miara dochodu wzrośnie o 1 % wydatki na owoce wzrosną o 0,58%, czyli jest to dobro normalne.

```{r}
lm5<-lm(Dane_clean$Napoje~Dane_clean$Dochod,data=Dane_clean)
summary(lm5)
ied_Napoje<-lm5$coefficients[2]*mean(Dane_clean$Dochod)/mean(Dane_clean$Napoje)
ied_Napoje #napoje
```


W przypadku napojów elastyczność mieści się w typowym zakresie jej zmienności, choć intuicyjnie wydaje się wyższa niż można by oczekiwać, szczególnie w przypadku kraju o gorącym klimacie (powinno być to raczej dobro o niższej elastyczności). Być może mogłoby to być interesujące dla głębszej analizy, z podziałem na regiony i grupy dochodowe. Wróćmy także uwagę, że wino z pewnością nie jest we Włoszech napojem o charakterze ekskluzywnym.

Chętnie wyznaczylibyśmy także cenowe elastyczności popytu, ale niestety dane dotyczące cen nie są dla nas dostępne



### 2.2 Hipoteza 2: procentowy udział wydatków na żywność maleje wraz ze wzorstem dochodu - prawo Engla.

W przypadku dóbr podstawowych wraz ze wzrostem dochodu popyt powinien rosnąć, ale słabiej niż dochód. Zatem względny udział takiej kategorii wydatków w wydatkach ogółem powinien maleć. Zaprezentujemy graficznie działanie prawa Engla na podstawie naszych danych. Nie będzie to wprost krzywa Engla (która raczej prezentowana jest w układzie współrzędnych wydatki/dochody), ale od razu procentowy udział wydatków na żywność w stosunku do naszej estymaty dochodu.


<span style="color:blue">*Dzielimy dochody na 40 grup.*</span>
```{r}
qnt2 <- quantile(Dane_clean$Dochod, probs=seq(from=0.025,to=0.975,by=0.025), na.rm = T)
```

<span style="color:blue">*Ustalamy średni udział wydatków na żywność oraz średnie dochody dla poszczególnych grup.*</span>

```{r, echo=FALSE}
gr1<-Dane_clean[Dane_clean$Dochod<=qnt2[1],]
gr2<-Dane_clean[Dane_clean$Dochod>=qnt2[1] & Dane_clean$Dochod<qnt2[2],]
gr3<-Dane_clean[Dane_clean$Dochod>=qnt2[2] & Dane_clean$Dochod<qnt2[3],]
gr4<-Dane_clean[Dane_clean$Dochod>=qnt2[3] & Dane_clean$Dochod<qnt2[4],]
gr5<-Dane_clean[Dane_clean$Dochod>=qnt2[4] & Dane_clean$Dochod<qnt2[5],]
gr6<-Dane_clean[Dane_clean$Dochod>=qnt2[5] & Dane_clean$Dochod<qnt2[6],]
gr7<-Dane_clean[Dane_clean$Dochod>=qnt2[6] & Dane_clean$Dochod<qnt2[7],]
gr8<-Dane_clean[Dane_clean$Dochod>=qnt2[7] & Dane_clean$Dochod<qnt2[8],]
gr9<-Dane_clean[Dane_clean$Dochod>=qnt2[8] & Dane_clean$Dochod<qnt2[9],]
gr10<-Dane_clean[Dane_clean$Dochod>=qnt2[9] & Dane_clean$Dochod<qnt2[10],]
gr11<-Dane_clean[Dane_clean$Dochod>=qnt2[10] & Dane_clean$Dochod<qnt2[11],]
gr12<-Dane_clean[Dane_clean$Dochod>=qnt2[11] & Dane_clean$Dochod<qnt2[12],]
gr13<-Dane_clean[Dane_clean$Dochod>=qnt2[12] & Dane_clean$Dochod<qnt2[13],]
gr14<-Dane_clean[Dane_clean$Dochod>=qnt2[13] & Dane_clean$Dochod<qnt2[14],]
gr15<-Dane_clean[Dane_clean$Dochod>=qnt2[14] & Dane_clean$Dochod<qnt2[15],]
gr16<-Dane_clean[Dane_clean$Dochod>=qnt2[15] & Dane_clean$Dochod<qnt2[16],]
gr17<-Dane_clean[Dane_clean$Dochod>=qnt2[16] & Dane_clean$Dochod<qnt2[17],]
gr18<-Dane_clean[Dane_clean$Dochod>=qnt2[17] & Dane_clean$Dochod<qnt2[18],]
gr19<-Dane_clean[Dane_clean$Dochod>=qnt2[18] & Dane_clean$Dochod<qnt2[19],]
gr20<-Dane_clean[Dane_clean$Dochod>=qnt2[19] & Dane_clean$Dochod<qnt2[20],]
gr21<-Dane_clean[Dane_clean$Dochod>=qnt2[20] & Dane_clean$Dochod<qnt2[21],]
gr22<-Dane_clean[Dane_clean$Dochod>=qnt2[21] & Dane_clean$Dochod<qnt2[22],]
gr23<-Dane_clean[Dane_clean$Dochod>=qnt2[22] & Dane_clean$Dochod<qnt2[23],]
gr24<-Dane_clean[Dane_clean$Dochod>=qnt2[23] & Dane_clean$Dochod<qnt2[24],]
gr25<-Dane_clean[Dane_clean$Dochod>=qnt2[24] & Dane_clean$Dochod<qnt2[25],]
gr26<-Dane_clean[Dane_clean$Dochod>=qnt2[25] & Dane_clean$Dochod<qnt2[26],]
gr27<-Dane_clean[Dane_clean$Dochod>=qnt2[26] & Dane_clean$Dochod<qnt2[27],]
gr28<-Dane_clean[Dane_clean$Dochod>=qnt2[27] & Dane_clean$Dochod<qnt2[28],]
gr29<-Dane_clean[Dane_clean$Dochod>=qnt2[28] & Dane_clean$Dochod<qnt2[29],]
gr30<-Dane_clean[Dane_clean$Dochod>=qnt2[29] & Dane_clean$Dochod<qnt2[30],]
gr31<-Dane_clean[Dane_clean$Dochod>=qnt2[30] & Dane_clean$Dochod<qnt2[31],]
gr32<-Dane_clean[Dane_clean$Dochod>=qnt2[31] & Dane_clean$Dochod<qnt2[32],]
gr33<-Dane_clean[Dane_clean$Dochod>=qnt2[32] & Dane_clean$Dochod<qnt2[33],]
gr34<-Dane_clean[Dane_clean$Dochod>=qnt2[33] & Dane_clean$Dochod<qnt2[34],]
gr35<-Dane_clean[Dane_clean$Dochod>=qnt2[34] & Dane_clean$Dochod<qnt2[35],]
gr36<-Dane_clean[Dane_clean$Dochod>=qnt2[35] & Dane_clean$Dochod<qnt2[36],]
gr37<-Dane_clean[Dane_clean$Dochod>=qnt2[36] & Dane_clean$Dochod<qnt2[37],]
gr38<-Dane_clean[Dane_clean$Dochod>=qnt2[37] & Dane_clean$Dochod<qnt2[38],]
gr39<-Dane_clean[Dane_clean$Dochod>=qnt2[38] & Dane_clean$Dochod<qnt2[39],]
gr40<-Dane_clean[Dane_clean$Dochod>=qnt2[39],]
srednia_grupy_dochody <-c(mean(gr1$Dochod),
                          mean(gr2$Dochod),
                          mean(gr3$Dochod),
                          mean(gr4$Dochod),
                          mean(gr5$Dochod),
                          mean(gr6$Dochod),
                          mean(gr7$Dochod),
                          mean(gr8$Dochod),
                          mean(gr9$Dochod),
                          mean(gr10$Dochod),
                          mean(gr11$Dochod),
                          mean(gr12$Dochod),
                          mean(gr13$Dochod),
                          mean(gr14$Dochod),
                          mean(gr15$Dochod),
                          mean(gr16$Dochod),
                          mean(gr17$Dochod),
                          mean(gr18$Dochod),
                          mean(gr19$Dochod),
                          mean(gr20$Dochod),
                          mean(gr21$Dochod),
                          mean(gr22$Dochod),
                          mean(gr23$Dochod),
                          mean(gr24$Dochod),
                          mean(gr25$Dochod),
                          mean(gr26$Dochod),
                          mean(gr27$Dochod),
                          mean(gr28$Dochod),
                          mean(gr29$Dochod),
                          mean(gr30$Dochod),
                          mean(gr31$Dochod),
                          mean(gr32$Dochod),
                          mean(gr33$Dochod),
                          mean(gr34$Dochod),
                          mean(gr35$Dochod),
                          mean(gr36$Dochod),
                          mean(gr37$Dochod),
                          mean(gr38$Dochod),
                          mean(gr39$Dochod),
                          mean(gr40$Dochod))
srednia_grupy_suma_zywnosc <- c(mean(gr1$Suma_wydatkow_zywnosc)/mean(gr1$Dochod),
                                mean(gr2$Suma_wydatkow_zywnosc)/mean(gr2$Dochod),
                                mean(gr3$Suma_wydatkow_zywnosc)/mean(gr3$Dochod),
                                mean(gr4$Suma_wydatkow_zywnosc)/mean(gr4$Dochod),
                                mean(gr5$Suma_wydatkow_zywnosc)/mean(gr5$Dochod),
                                mean(gr6$Suma_wydatkow_zywnosc)/mean(gr6$Dochod),
                                mean(gr7$Suma_wydatkow_zywnosc)/mean(gr7$Dochod),
                                mean(gr8$Suma_wydatkow_zywnosc)/mean(gr8$Dochod),
                                mean(gr9$Suma_wydatkow_zywnosc)/mean(gr9$Dochod),
                                mean(gr10$Suma_wydatkow_zywnosc)/mean(gr10$Dochod),
                                mean(gr11$Suma_wydatkow_zywnosc)/mean(gr11$Dochod),
                                mean(gr12$Suma_wydatkow_zywnosc)/mean(gr12$Dochod),
                                mean(gr13$Suma_wydatkow_zywnosc)/mean(gr13$Dochod),
                                mean(gr14$Suma_wydatkow_zywnosc)/mean(gr14$Dochod),
                                mean(gr15$Suma_wydatkow_zywnosc)/mean(gr15$Dochod),
                                mean(gr16$Suma_wydatkow_zywnosc)/mean(gr16$Dochod),
                                mean(gr17$Suma_wydatkow_zywnosc)/mean(gr17$Dochod),
                                mean(gr18$Suma_wydatkow_zywnosc)/mean(gr18$Dochod),
                                mean(gr19$Suma_wydatkow_zywnosc)/mean(gr19$Dochod),
                                mean(gr20$Suma_wydatkow_zywnosc)/mean(gr20$Dochod),
                                mean(gr21$Suma_wydatkow_zywnosc)/mean(gr21$Dochod),
                                mean(gr22$Suma_wydatkow_zywnosc)/mean(gr22$Dochod),
                                mean(gr23$Suma_wydatkow_zywnosc)/mean(gr23$Dochod),
                                mean(gr24$Suma_wydatkow_zywnosc)/mean(gr24$Dochod),
                                mean(gr25$Suma_wydatkow_zywnosc)/mean(gr25$Dochod),
                                mean(gr26$Suma_wydatkow_zywnosc)/mean(gr26$Dochod),
                                mean(gr27$Suma_wydatkow_zywnosc)/mean(gr27$Dochod),
                                mean(gr28$Suma_wydatkow_zywnosc)/mean(gr28$Dochod),
                                mean(gr29$Suma_wydatkow_zywnosc)/mean(gr29$Dochod),
                                mean(gr30$Suma_wydatkow_zywnosc)/mean(gr30$Dochod),
                                mean(gr31$Suma_wydatkow_zywnosc)/mean(gr31$Dochod),
                                mean(gr32$Suma_wydatkow_zywnosc)/mean(gr32$Dochod),
                                mean(gr33$Suma_wydatkow_zywnosc)/mean(gr33$Dochod),
                                mean(gr34$Suma_wydatkow_zywnosc)/mean(gr34$Dochod),
                                mean(gr35$Suma_wydatkow_zywnosc)/mean(gr35$Dochod),
                                mean(gr36$Suma_wydatkow_zywnosc)/mean(gr36$Dochod),
                                mean(gr37$Suma_wydatkow_zywnosc)/mean(gr37$Dochod),
                                mean(gr38$Suma_wydatkow_zywnosc)/mean(gr38$Dochod),
                                mean(gr39$Suma_wydatkow_zywnosc)/mean(gr39$Dochod),
                                mean(gr40$Suma_wydatkow_zywnosc)/mean(gr40$Dochod)
                                )
```

<span style="color:blue">*Wizualizacja Prawa Engla. Na osi y znajduje się procentowy udział wydatków na jedzenie, na osi x średnie zarobki.*</span>

```{r, echo=FALSE}
srednia_grupy_suma_zywnosc #procentowy udział produktów spożywczych w dochodach
srednia_grupy_dochody #średnie dochody dla danych grup
par(mar = c(5, 4, 4, 2) + 0.1)
plot(srednia_grupy_dochody, srednia_grupy_suma_zywnosc, type = "l", main = "Prawo Engla - wizualizacja", xlab="Srednie zarobki w euro", ylab="Procentowy udział wydatków na żywność", axes=TRUE )
```

### 3.3 Hipoteza 3 Procentowy udział wydatków zbożowych jest większy dla mniej zamożnych gospodarstw


Z ciekawości postanowiliśmy dodatkowo zwizualizować jeszcze wydatki na produkty zbożowe, które miały najniższą elastyczność. To pewien analog prawa Engla tylko dla węższej grupy produktów, która wydaje się dobrem najniższego rzędu z tych dotychczas rozważanych. Dodatkowo o ile można sobie wyobrazić, że ktoś nagle zacznie jeść dwukrotnie więcej owoców niż wcześniej, to trudno sobie wyobrazić aby zaczął nagle jeść dwa razy więcej bułek albo chleba czy innych produktów zbożowych. Spodziewaliśmy się zatem nawet gładszej zależności niż w przypadku ogólnych wydatków na żywność i w zasadzie dostaliśmy gładszą zależność 


<span style="color:blue">*Ustalamy średni udział produktów zbożowych w średnich dochodach dla danej grupy.*</span>
```{r, echo=FALSE}
udzial_zbozowe <- c(mean(gr1$Zboza)/mean(gr1$Dochod),
                    mean(gr2$Zboza)/mean(gr2$Dochod),
                    mean(gr3$Zboza)/mean(gr3$Dochod),
                    mean(gr4$Zboza)/mean(gr4$Dochod),
                    mean(gr5$Zboza)/mean(gr5$Dochod),
                    mean(gr6$Zboza)/mean(gr6$Dochod),
                    mean(gr7$Zboza)/mean(gr7$Dochod),
                    mean(gr8$Zboza)/mean(gr8$Dochod),
                    mean(gr9$Zboza)/mean(gr9$Dochod),
                    mean(gr10$Zboza)/mean(gr10$Dochod),
                    mean(gr11$Zboza)/mean(gr11$Dochod),
                    mean(gr12$Zboza)/mean(gr12$Dochod),
                    mean(gr13$Zboza)/mean(gr13$Dochod),
                    mean(gr14$Zboza)/mean(gr14$Dochod),
                    mean(gr15$Zboza)/mean(gr15$Dochod),
                    mean(gr16$Zboza)/mean(gr16$Dochod),
                    mean(gr17$Zboza)/mean(gr17$Dochod),
                    mean(gr18$Zboza)/mean(gr18$Dochod),
                    mean(gr19$Zboza)/mean(gr19$Dochod),
                    mean(gr20$Zboza)/mean(gr20$Dochod),
                    mean(gr21$Zboza)/mean(gr21$Dochod),
                    mean(gr22$Zboza)/mean(gr22$Dochod),
                    mean(gr23$Zboza)/mean(gr23$Dochod),
                    mean(gr24$Zboza)/mean(gr24$Dochod),
                    mean(gr25$Zboza)/mean(gr25$Dochod),
                    mean(gr26$Zboza)/mean(gr26$Dochod),
                    mean(gr27$Zboza)/mean(gr27$Dochod),
                    mean(gr28$Zboza)/mean(gr28$Dochod),
                    mean(gr29$Zboza)/mean(gr29$Dochod),
                    mean(gr30$Zboza)/mean(gr30$Dochod),
                    mean(gr31$Zboza)/mean(gr31$Dochod),
                    mean(gr32$Zboza)/mean(gr32$Dochod),
                    mean(gr33$Zboza)/mean(gr33$Dochod),
                    mean(gr34$Zboza)/mean(gr34$Dochod),
                    mean(gr35$Zboza)/mean(gr35$Dochod),
                    mean(gr36$Zboza)/mean(gr36$Dochod),
                    mean(gr37$Zboza)/mean(gr37$Dochod),
                    mean(gr38$Zboza)/mean(gr38$Dochod),
                    mean(gr39$Zboza)/mean(gr39$Dochod),
                    mean(gr40$Zboza)/mean(gr40$Dochod)
)
```

```{r, echo=FALSE}
udzial_zbozowe #procentowy udział produktów zbożowych w dochodach
par(mar = c(5, 4, 4, 2) + 0.1)
barplot(udzial_zbozowe, main="Udzial produktów zbożowych dla poszczególnych grup dochodowych", 
        xlab="Grupy podzielone ze wzgledu na dochod", names.arg = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", '17', '18', '19', '20', '21', '22', '23', '24', '24', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40'))
```

### 4.4 Hipoteza 4: Rodziny z dziećmi spożywają wiecej ciast i wyrobów cukierniczych niż rodziny bezdzietne


Nasza hipoteza 4 wydaje nam się być pierwszą, wobec której nasze intuicyjne przypuszczenia nie są klarowne oraz zgodne z naszymi osobistymi przekonaniami i w ogóle może być nietrywialna. Znamy bowiem bardzo wiele osób, które nie mając dzieci zjadają bardzo dużo takich rzeczy. Z kolei osoby mające dzieci często są wyznawcami bardzo zdrowego trybu życia i dla zasady słodyczy nie kupują. Poświęcimy jej zatem odrobinę więcej uwagi, gdyż powszechna opinia jest taka jak w wypisanej hipotezie.

Zdajemy sobie sprawę, że przy takiej liczbie danych istotność statystyczna różnicy pomiędzy średnimi sama z siebie nie niesie ze sobą istotnej informacji (na pewno różnica wyjdzie istotna). Traktujemy to jednak jako trening w R, a sama wartość i kierunek różnicy tak czy inaczej jest dla nas interesujący.



```{r}
Dane_ciasta <- Dane %>% 
mutate("ciasta" = C_1107) %>% 
select(NR,Mese,Regione,TIPFAM,NC,ciasta)
boxplot.default(Dane_ciasta$ciasta)
```


Po jakościowym rzucie oka na dane, przy użyciu wykresu boxplot, zdecydowaliśmy się jednak na usunięcie po 5% skrajnych dolnych oraz górnych obserwacji w poziomie wydatków na ciasta i wyroby cukiernicze, aby zmniejszyć jego skośność.

<span style="color:blue">*Usuwamy 10% odstających obserwacji*</span>

```{r}
qnt4 <- quantile(Dane_ciasta$ciasta, probs=c(.05, .95), na.rm = T)
small1<-Dane_ciasta$ciasta<qnt4[1]
Dane_ciasta<-Dane_ciasta[!small1,]
large2<-Dane_ciasta$ciasta>qnt4[2]
Dane_ciasta<-Dane_ciasta[!large2,]
boxplot.default(Dane_ciasta$ciasta)
```

Po usunięciu części danych odstających ponownie generujemy wykres pudełkowy, który potwierdza celowość takiego zabiegu. Skośność jest tu immanentną cechą rozkładu tej zmiennej, tak więc nie jest naszym celem usunięcie skośności, a jedynie zwiększenie stopnia pewności, że z danych usunęliśmy potencjalne błędy grube (np. gdy respondenci wypełniali ankietę dla zabawy wpisując tam losowe dane) oraz mocno nietypowe przypadki.

```{r}
table(Dane_ciasta$TIPFAM) #poszczególne typy rodzin
split_ciasta_tipfam <- split(Dane_ciasta$ciasta, as.factor(Dane_ciasta$TIPFAM), drop = TRUE)
```

<span style="color:blue">*Tworzymy ramkę danych, która zawiera podział na rodziny z dziećmi lub bez dzieci oraz odpowiadające im wydatki na ciasta i wyroby cukiernicze. Przekształcamy typ rodziny w factor.*</span>

```{r, echo=FALSE}
ramka_ciasta <- data.frame('typ_rodziny_dzietnosc'=c(rep("bezdzietna",length(split_ciasta_tipfam$`1`)),rep("bezdzietna",length(split_ciasta_tipfam$`3`)),rep("bezdzietna",length(split_ciasta_tipfam$`4`)), rep("bezdzietna",length(split_ciasta_tipfam$`6`)),rep("dzieci",length(split_ciasta_tipfam$`7`)), rep("dzieci",length(split_ciasta_tipfam$`8`)), rep("dzieci",length(split_ciasta_tipfam$`10`))),
                           'wydatki_ciasta' = c(split_ciasta_tipfam$`1`,split_ciasta_tipfam$`3`, split_ciasta_tipfam$`4`, split_ciasta_tipfam$`6`, split_ciasta_tipfam$`7`, split_ciasta_tipfam$`8`, split_ciasta_tipfam$`10`))
        
ramka_ciasta$typ_rodziny_dzietnosc <- factor(ramka_ciasta$typ_rodziny_dzietnosc) # Przekształcam typ_rodziny w factor                  
```

```{r}
head(ramka_ciasta)
```
<span style="color:blue">*Sprawdzamy założenia testu Studenta.*

Na kilka sposobów graficznie zilustrujemy empiryczny rozkład wydatków na ciastka (bez podziału na dwie podgrupy) w naszych danych. Już patrząc na wykres pudełkowy widzimy, że odstępstwa od rozkładu normalnego będą znaczne, ale być może uda nam się to zobrazować w bardziej atrakcyjny sposób.

* Normalnosc rozkladu</span>

```{r, echo=FALSE}
ggdensity(ramka_ciasta$wydatki_ciasta[], 
          main = "Wykres gęstosci",
          xlab = "Wydatki na ciasta w euro")
options(repr.plot.width=10, repr.plot.height=5)
#Wykres QQ
qqnorm(ramka_ciasta$wydatki_ciasta, main = 'Wykres QQ')
qqline(ramka_ciasta$wydatki_ciasta, col = "firebrick")
```

Wykres kwantyl-kwantyl potwierdza ponownie wyraźne rozbieżności naszego rozkładu wobec rozkładu normalnego o takiej samej średniej i odchyleniu standardowym jak nasze dane.

```{r}
rozkl_norm_ciasta<- rnorm(11116, mean = mean(ramka_ciasta$wydatki_ciasta), sd = sd(ramka_ciasta$wydatki_ciasta))
plot(ecdf(ramka_ciasta$wydatki_ciasta), col='royalblue3', main ='Dystrybuanta empiryczna (EDF) oraz teoretyczna (CDF)', xlab = 'Wydatki na ciasta', ylab = 'F(x)') #tworzymy dystrybuante empiryczna
lines(ecdf(rozkl_norm_ciasta), col='violetred3') #dystrybuanta teoretyczna
legend("bottomright", legend=c("EDF", "CDF"),col=c("royalblue", "violetred3"), lty = 1:1, cex=0.8)
  
```


Porównanie dystrybuanty empirycznej (EDF) oraz teoretycznej (dla rozkładu normalnego, CDF) ponownie potwierdza wcześniejsze spostrzeżenie.

Następnie dla formalności przeprowadzimy test Kołmogorowa-Smirnowa dla dwóch podzbiorów danych wydzielonych ze względu na posiadanie dzieci, przewidując zasadniczo jego wynik (odrzucenie hipotezy o normalności rozkładu).



<span style="color:blue">*Test KS*</span>

```{r, echo=FALSE}
dzieci_rozklad <- with(ramka_ciasta, wydatki_ciasta[typ_rodziny_dzietnosc=="dzieci"])
rozkl_norm_dzieci<- rnorm(11116, mean = mean(dzieci_rozklad), sd = sd((dzieci_rozklad)))
colSums(ramka_ciasta != 0)
bezdzieci_rozklad <- with(ramka_ciasta, wydatki_ciasta[typ_rodziny_dzietnosc=="bezdzietna"])
ks.test(x=dzieci_rozklad,y=rozkl_norm_dzieci)
ks.test(x=bezdzieci_rozklad, y="pnorm")
```

<span style="color:blue">*Dane nie mają rozkładu normalnego.*

Nieco mniej oczywiste wydaje się jedno z założeń wielu testów parametrycznych, czyli jednorodność lub sferyczność wariancji. Tutaj wynik naszym zdaniem będzie miał jakąś realną wartość poznawczą. Mimo tego, że ponownie ze względu na liczebność próby odrzucenie hipotezy o jednorodności wariancji jest praktycznie pewne, interesującą informację może nieść ze sobą sam stosunek tych wariancji, czyli ocena rzeczywistej siły tego efektu zróżnicowania wariancji.

* Homogeniczność wariancji</span>

```{r, echo=FALSE}
summary(ramka_ciasta$typ_rodziny_dzietnosc)
md.pattern(ramka_ciasta)
res.ftest <- var.test(wydatki_ciasta ~ typ_rodziny_dzietnosc, data = ramka_ciasta)
res.ftest #wariancje nie są jednorodne
var.test(wydatki_ciasta ~ typ_rodziny_dzietnosc, alternative = "greater", data = ramka_ciasta)
```

<span style="color:blue">*Wariancje nie są jednorodne.*

Jak widzimy, ocena jednej z wariancji jest półtorakrotnie wyższa od oceny drugiej. Przy takiej liczebności próby, taki wynik wydaje nam się znaczący. Tak więc intuicyjna interpretacja jest tu zgodna z tą formalną: te wariancje faktycznie są od siebie wyraźnie różne, a konkretniej wariancja dla rodzin bezdzietnych jest wyraźnie niższa. 

Nie jest to zaskakujące, bo nie mieć dzieci można tylko na jeden sposób (brak dzieci) a mieć dzieci można w różnej liczbie i w przypadku brania jako rekordu danych rodziny, generuje to dodatkową pulę zmienności. 

Ponownie zachowując świadomość ograniczonej wartości informacyjnej testów istotności różnic użyjemy nieparametrycznej alternatywy testu t-Studenta dla oceny różnic w poziomie wydatków na ciasta pomiędzy tymi dwiema grupami rodzin, czyli testem U Manna-Whitneya w wariancie z jednostronnym obszarem krytycznym. Zwizualizujemy też te różnice na wykresie pudełkowym.


*Zalozenia testu nie są spełnione zatem wykonujemy test nieparametryczny.*</span>

```{r}
group_by(ramka_ciasta, typ_rodziny_dzietnosc) %>%
  summarise(
    count = n(),
    median = median(wydatki_ciasta, na.rm = TRUE),
    IQR = IQR(wydatki_ciasta, na.rm = TRUE)
  )
```

```{r, echo=FALSE}
ggboxplot(ramka_ciasta, x = "typ_rodziny_dzietnosc", y = "wydatki_ciasta", 
          color = "typ_rodziny_dzietnosc", palette = c("#00AFBB", "#E7B800"),
          ylab = "wydatki na ciasta w euro", xlab = "typ rodziny ze wzgledu na dzietnosc")
```

<span style="color:blue">*Wykonujemy test U-Manna-Whitney'a-Wilcoxona.Sprawdzamy, czy dla rodzin bezdzietnych wydatki na ciastka sa mniejsze*</span>


```{r}
wilcox.test(wydatki_ciasta ~ typ_rodziny_dzietnosc, data = ramka_ciasta,
            exact = FALSE, alternative = "less")
```

Jakościowa ocena wykresu nie umożliwia oceny zróżnicowania median (obie są równe zeru). Widzimy za to graficzną ilustrację różnic w wewnętrznym zróżnicowaniu tych dwóch grup (tu mierzonym rozstępem międzykwartylowym). 
Naszym zdaniem nasuwa to podejrzenie o braku pełnej koherencji tych danych. Nie wydaje się realne aby zarówno połowa rodzin z dziećmi jak i połowa rodzin bez dzieci wydawała zero euro na wyroby cukiernicze.

Jest to zatem kolejne miejsce, w którym można nabrać wątpliwości co do jakości danych surowych, niemniej jednak ze względu na bezradność w kwestii ustalenia czy te zera w danych w istocie były brakami danych, które kiedyś wcześniej zostały zastąpione zerami albo też respondent wypełniał inny kwestionariusz albo nie wypełnił go w całości a mimo to dane zostały włączone do zbioru, nie będziemy rozważali dalej tej kwestii.

Na około 22 000 obserwacji tylko około 9400 nie jest zerem. Nadmiernie wiarygodnie to naszym zdaniem nie wygląda.

### 2.5 Hipoteza 5: Dochody są prawostronnie skośne

Jak wiadomo z teorii ekonomii, rozkład dochodów jest generalnie prawoskośny. Ponieważ kwestia skośności kilkukrotnie przewijała się już w naszych wcześniejszych analizach, teraz spróbujemy dokonać zwizualizowania tej własności dochodów na przykładzie naszych danych, pamiętając ciągle, że dochód estymujemy jako sumę zgłoszonych w kwestionariuszu wydatków.

<span style="color:blue">*Obliczamy współczynnik skośności.*</span>

```{r}
skewness(Dane_clean$Dochod)
```

Histogram dochodów:

```{r, echo=FALSE}
hist(Dane_clean$Dochod, main = "Histogram")
abline(v = c(mean(Dane_clean$Dochod),median(Dane_clean$Dochod)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))
legend("topright", legend=c("srednia", "mediana"), col=c("green", "blue"), lty = 2:2)
```



Dla rozkladu prawostronnie asymetrycznego spodziewamy się dodatniego współczynnika asymetrii i taką też jego wartość uzyskujemy. Histogram oraz krzywa gęstości z naniesionymi wartościami średniej oraz mediany potwierdzają jednoznacznie prawoskośność naszego miernika dochodu.

<span style="color:blue">*Wykres gestosci dla dochodow.*</span>

```{r, echo=FALSE}
density_plot_dochody <- 
  Dane_clean %>% 
  ggplot(aes(x = Dochod)) +
  stat_density(geom = "line", alpha = 1, colour = "cornflowerblue")
shaded_area_data <- 
  ggplot_build(density_plot_dochody)$data[[1]] %>% 
  filter(x < mean(Dane_clean$Dochod))
density_plot_dochody_shaded <- 
  density_plot_dochody + 
  geom_area(data = shaded_area_data, aes(x = x, y = y), fill="pink", alpha = 0.5)
mediana_dochody <- median(Dane_clean$Dochod)
srednia_dochody <-mean(Dane_clean$Dochod)
median_line_data <- 
  ggplot_build(density_plot_dochody)$data[[1]] %>% 
  filter(x <= mediana_dochody)
density_plot_dochody_shaded  +
  geom_segment(data = shaded_area_data, aes(x = srednia_dochody, y = 0, xend = srednia_dochody, yend = density),
               color = "red", linetype = "dotted") +
  annotate(geom = "text", x = srednia_dochody, y = 0, label = "srednia", color = "red") +
  
  geom_segment(data = median_line_data, aes(x = mediana_dochody, y = 0, xend = mediana_dochody, yend = density),
               color = "black", linetype = "dotted") +
  annotate(geom = "text", x = mediana_dochody, y = 0.000020, label = "mediana") +
  
  
  ggtitle("Wykres gęstości ilustrujący prawostronna skośność dochodów")
```

Podsumowując część, w której generalnie przyglądaliśmy się danym, zauważyliśmy w nich dwie wyraźne niekonsystencje, niemniej jednak wszystkie potencjalnie przewidywane przez teorię ekonomii własności tych danych zostały potwierdzone.

Biorąc pod uwagę to, co się okazało w ostatnim kroku analizy (podejrzanie duża liczba zerowych wydatków dla wyrobów cukierniczych) zmienimy nasze pierwotne zamierzenie aby konstruować model dla wydatków na wyroby cukiernicze, a zamiast tego zajmiemy się wydatkami na owoce.

Szczególną grupą aproksymant wspomnianych już wcześniej krzywych Engla są funkcje Tornquista. Zmienną objaśnianą są wydatki, a objaśniającą dochód. W naszym przypadku (owoców) zdecydowanie mamy do czynienia z dobrami podstawowymi, zatem oszacujemy parametry strukturalne krzywej Tornquista pierwszego rodzaju. Jest to model linearyzowalny, oszacujemy go jednak bezpośrednio w postaci nieliniowej.


## 2.6 Krzywa Tornquista I rzędu

<span style="color:blue">*Obliczamy parametry a i b za pomocą funkcji nls()*</span>

```{r}
x0 <- Dane_clean$Dochod
y0 <- Dane_clean$Owoce
summary(Dane_clean$Owoce) #max 329.82 
 
dane123 = data.frame(dochod = x0, wydatek = y0)
 t1 <- nls(formula = y0 ~ a*x0/(b+x0),
          data = dane123,
          start = list(a=333,b=10))
summary(t1) #a  108.382  b 3447.095
x <- Dane_clean$Dochod
y <-  (108.4*x)/(x+ 3447.1) 
plot(x,y,pch=5)
```

Krzywą dopasowaliśmy do danych surowych (bez ich grupowania, co wydawało nam się niepotrzebną utratą części danych). Jej kształt jest zgodny z tym czego należało oczekiwać. Konwergencję do zakładanej zmiany oszacowań parametrów strukturalnych uzyskaliśmy już w szóstej iteracji, co przy tej liczbie danych wydaje się wynikiem nieoczekiwanie dobrym. Inna rzecz, że ten model jest bardzo prosty.

Przejdziemy teraz do części, którą uważamy za zasadniczą część analityczną naszego projektu. Oszacujemy model logistyczny przewidujący wielkość wydatków na owoce w funkcji wybranych zmiennych objaśniających.

W tym etapie dokonaliśmy dość znacznych przekształceń danych surowych, które szczegółowo opiszemy.
	W pierwszym kroku dokonaliśmy przeliczenia wielkości wszystkich wydatków z wydatków rodzin, na przeciętne wydatki na osobę w rodzinie. Tutaj pojawiła się pierwsza istotna kwestia stanowiąca przedmiot rozważań.
	
Zastępując bowiem sumę wydatków rodziny przeciętnymi wydatkami na osobę w rodzinie, usuwamy pewne obciążenie danych, ale także pewne wprowadzamy. Usuwamy obciążenie związane z tym, że rodziny o większej liczbie osób w naturalny sposób wydają więcej.
Powstało natomiast zagadnienie, czy po takim przekształceniu z jednego rekordu danych tworzyć także jeden rekord danych czy też tyle rekordów, ile było osób w rodzinie. 
Po dyskusji zdecydowaliśmy o tworzeniu nowych rekordów 1:1 ze świadomością, że w pewien sposób przypisuje to niższą wagę danym z rodzin o dużych licznościach, bo rodzina 1-osobowa jest reprezentowana przez pojedynczy rekord i rodzina 8-osobowa także.


## 3. Regresja logistyczna

<span style="color:blue">*Tworzymy data frame zawierający wydatki na żywność na osobę.*</span>

```{r, echo=FALSE}
Dane3<-Dane%>%
  mutate("Zboza"=C_1101+C_1102+C_1103+C_1104+C_1105+C_1106+C_1107)%>%
  mutate("Mieso"= C_1201+ C_1202+ C_1203+ C_1204+ C_1205+ C_1206+ C_1207+ C_1208+ C_1209+ C_1297)%>%
  mutate("Ryby"=C_1301+ C_1302+ C_1303+ C_1304)%>%
  mutate("Owoce"=C_1601+ C_1602+ C_1603+ C_1604+ C_1605+ C_1606+ C_1607+ C_1608+ C_1609)%>%
  mutate("Napoje"=C_1803+ C_1804+ C_1805+ C_1806+ C_1807+ C_1808)%>%
  mutate("Dochod"=wszystkie_wydatki)%>%
  select(NR,Mese,Regione,Conprof1,TIPFAM,NC,Zboza,Mieso, Ryby,Owoce,Napoje,Dochod)
Dane3_na_osobe <-Dane%>%
  mutate("Zboza_na_os"=Dane3$Zboza/Dane3$NC) %>%
  mutate("Mieso_na_os"= Dane3$Mieso/ Dane3$NC)%>%
  mutate("Ryby_na_os"=Dane3$Ryby/Dane3$NC) %>%
  mutate("Owoce_na_os"=Dane3$Owoce/Dane3$NC)%>%
  mutate("Napoje_na_os"=Dane3$Napoje/Dane3$NC)%>%
  mutate("Dochod_na_os"=Dane3$Dochod/Dane3$NC)%>%
  select(NR,Mese,Regione,Conprof1,TIPFAM,NC,Zboza_na_os,Mieso_na_os, Ryby_na_os,Owoce_na_os,Napoje_na_os,Dochod_na_os)
```

```{r}
head(Dane3_na_osobe)
```

<span style="color:blue">*Sprawdzamy, czy wystepuja braki danych.*</span>


```{r, echo=FALSE}
md.pattern(Dane3_na_osobe)
missmap(Dane3_na_osobe, main = "Brakujące dane vs zaobserwowane")
```


<span style="color:blue">*Brak NA.*

Ze względu na wcześniejsze doświadczenia z tym zbiorem danych przeprowadziliśmy poszukiwanie brakujących wartości.

W ramach walki z monotonią analizy w tym przypadku zamiast a’priorycznego ustalenia ile danych z każdego krańca rozkładu usuwamy (tak jak zrobiliśmy w początkowym etapie analizy) tutaj posłużyliśmy się teoretycznie lepszą i bardziej zobiektywizowaną metodą, czyli skorzystaliśmy z reguły trzech sigm po wystandaryzowaniu danych. Selekcji danych dokonaliśmy na podstawie wartości dochodu przeliczonego na jedną osobę.


*Tworzymy funkcję standaryzującą. Będzie nam ona potrzebna do drugiego wykorzystanego przez nas sposobu na uswanie danych odstających.*</span>

```{r}
#Funkcja standaryzująca
std.scale <- function(x)
{
  (x - mean(x))/sd(x)
}
```

<span style="color:blue">*Następnie usuwamy obserwacje odstające dla dochodów z wykorzystaniem reguły trzech sigm.*</span>

```{r}
#Usuwamy wartości odstające dla dochodów
wszystkie_wydatki_scaled <- std.scale(Dane3_na_osobe$Dochod_na_os)
outlier.dochody.index <- which(wszystkie_wydatki_scaled > 3)
wszystkie_wydatki_no_outlier <- Dane3_na_osobe$Dochod_na_os[-outlier.dochody.index]
```

Kolejne zagadnienie to dobór zmiennych objaśniających. Ogólnie zdajemy sobie sprawę, że nadmiernie duża liczba zmiennych objaśniających nie jest wysoce pożądaną cechą modelu. 

W przypadku zmiennych jakościowych w modelu chcemy uwzględnić: typ rodziny, wykształcenie i zamieszkanie. Kwestia uwzględnienia zamieszkania wymaga szerszego opisu. Uważamy, że ta zmienna przenosi informację pochodzącą od co najmniej trzech zmiennych ukrytych: rzeczywistego dochodu (spodziewamy się, że północ jest bogatsza niż południe), aspekty psychologiczne respondentów powiązane nie tyle z ich osobistą sytuacją, ile z sytuacją całego regionu (także czysto geofizyczną, ukształtowaniem terenu) oraz zmiennej reprezentującej warunki klimatyczne (na południu powinno być goręcej niż na północy Włoch, zatem tam być może powinna występować większa chęć do spożywania owoców). W związku z tym, że tych regionów jest dużo, na zmiennej tej dokonamy później dodatkowych operacji.



<span style="color:blue">*Zamieniamy zmienne kategoryczne na czynniki za pomocą funkcji as.factor()*</span>

```{r}
#zamieniam zmienne kategoryczne na czynniki
is.factor(Dane3_na_osobe$Regione) #FALSE
zamieszkanie <- factor(Dane3_na_osobe$Regione)
is.factor(zamieszkanie) #TRUE
typ_rodziny <- factor(Dane3_na_osobe$TIPFAM)
is.factor(typ_rodziny) #TRUE
wyksztalcenie <-factor(Dane3_na_osobe$Conprof1)
zamieszkanie %>% as.factor() %>% as_tibble()
```

Przekształcenie zmiennej zależnej:

Aby nieco odszumić dane, na tym etapie analizy usuniemy z nich 30% środkowych obserwacji w zmiennej wydatki na owoce. W pozostałych rekordach zmienną zależną rekodujemy w następujący sposób:

* 0 - nie lubi owoców (wydaje na kwotę niższą od 35 percentyla)
* 1 - lubi owoce (wydaje na nie kwotę wyższą od 65 percentyla)


```{r}
qnt <- quantile(Dane3_na_osobe$Owoce_na_os, probs=c(.35, .65), na.rm = T)
nie_lubi.index <- which(Dane3_na_osobe$Owoce_na_os<qnt[1])
lubi.index <-which(Dane3_na_osobe$Owoce_na_os>qnt[2])
do_wyrzucenia <- which(Dane3_na_osobe$Owoce_na_os>qnt[1] & Dane3_na_osobe$Owoce_na_os<qnt[2])
owoce <- Dane3_na_osobe$Owoce_na_os 
owoce_bi <- ifelse(owoce >= qnt[2], 1, 0) %>% as.factor() 
```

```{r, echo=FALSE}
#Tworze ramke bez danych odstających z dochodów oraz bez wydatkow na owoce pomiedzy 35 a 65 kwantylem, ale owoce sa przedstawione dalej jako wartosci numeryczne
dane.tib0 <- tibble("owoce" = owoce, 
                    "zboza" = Dane3_na_osobe$Zboza_na_os, 
                    "mieso" = Dane3_na_osobe$Mieso_na_os, 
                    "ryby" = Dane3_na_osobe$Ryby_na_os, 
                    "napoje"=Dane3_na_osobe$Napoje_na_os, 
                    "dochod"=Dane3_na_osobe$Dochod_na_os, typ_rodziny, zamieszkanie, wyksztalcenie)
dane.tib0 <- dane.tib0[-outlier.dochody.index, ]
dane.tib0 <- dane.tib0[-do_wyrzucenia, ]
#Tworze ramke bez danych odstających z dochodów oraz bez wydatkow na owoce pomiedzy 35 a 65 kwantylem, ale owoce sa przedstawione jako wartosci 0 1
dane.tib1 <- tibble("owoce" = owoce_bi, 
                    "owoce surowe"= Dane3_na_osobe$Owoce_na_os, 
                    "zboza" = Dane3_na_osobe$Zboza_na_os, 
                    "mieso" = Dane3_na_osobe$Mieso_na_os, 
                    "ryby" = Dane3_na_osobe$Ryby_na_os, 
                    "napoje"=Dane3_na_osobe$Napoje_na_os, 
                    "dochod"=Dane3_na_osobe$Dochod_na_os, typ_rodziny, zamieszkanie, wyksztalcenie) 
#dane przed odrzuceniem indeksow
dane.tib <- dane.tib1[-outlier.dochody.index, ]
dane.tib <- dane.tib[-do_wyrzucenia, ]
#dane.tib to tibble ktory bedziemy uzywac do regresji logistycznej
```

<span style="color:blue">*Ramka danych, która zawiera owoce w postaci wektora 0,1 oraz regresory - w postaci ilościowej oraz jakościowej. Zmienna owoce surowe jest wektorem przed przekształceniem na postać 0, 1.*</span>

```{r}
head(dane.tib)
```

Oszacujemy nasz model, który będzie naszym modelem wstępnym. Podzielimy zbiór danych na zbiór uczący i testowy. Możemy zauważyć, że ich statystyki są podobne.

<span style="color:blue">*Dzielimy zbiór na uczący i testowy.*</span>

```{r}
sp <- 0.7
len <- nrow(dane.tib)
#1:11150
train <- dane.tib[1:round(len*sp, 0), ]
# 11151:22694
test <- dane.tib[(round(len*sp, 0) + 1):len, ]
summary(train)
summary(test)
```

```{r}
head(train)
head(test)
```


W przypadku tak dużej liczby danych uzyskanie ich istotności statystycznej zapewne nie będzie żadnym problemem. Ponieważ to jednak pierwszy szacowany przez nas model logistyczny, zdecydowaliśmy o mechanicznej kwalifikacji zmiennych na skali ilorazowej, sprawdzimy tylko czy nie są one ze sobą wzajemnie zbyt silnie skorelowane (to znaczy czy nie przenoszą tej samej informacji) oraz czy wykazują jakąś wyraźną korelację ze zmienną objaśnianą (co jest sytuacją pożądaną). Na etapie analizy korelacji jest to nadal zmienna objaśniana w jej początkowym formacie, w dalszej analizie zostanie ona przekształcona a następnie zrekodowana.


<span style="color:blue">*Sprawdzamy zatem korelacje miedzy zmiennymi ilościowymi w zbiorze uczącym.*</span>

```{r}
correlations <- cor(train[,2:7])
correlations
corrplot(correlations, method="circle")
```

Korelacje zobrazowane w formie graficznej. Jak widzimy wszystkie współczynniki są dodatnie. Wśród potencjalnych zmiennych objaśniających najwyższy współczynnik korelacji obserwujemy dla pary mięso-zboża (około 0,47). Taki współczynnik korelacji odpowiada około 22% wspólnie przenoszonej informacji o zmienności, wg nas jest to poziom akceptowalny. Pewna redundancja występuje, natomiast nie jest ona silna. Zastanawialiśmy się czy jednej z tych dwóch zmiennych nie usunąć z analizy, ale ostatecznie zdecydowaliśmy o pozostawieniu ich obu.

<span style="color:blue">*Tworzymy model regresji logistycznej.*</span>

```{r}
model <- glm(owoce  ~  zboza + mieso + ryby + napoje + dochod + typ_rodziny +zamieszkanie + wyksztalcenie, data = train, family = "binomial")
summary(model)
```
Powyżej mamy oszacowanie naszego modelu wstępnego.

```{r}
anova(model, test = "Chisq")
```

Teraz dokonamy dość istotnego przekształcenia danych do dalszej analizy. Jak widzimy jako kategorie referencyjne zostały domyślnie przypisane kategorie kodowane liczbą 1. Np. w przypadku zamieszkania byłby to Piemont. Niemniej jednak zmienne odpowiadające zamieszkaniu w niektórych innych regionach nie uzyskały statystycznej istotności w naszym modelu wstępnym. Procedura usunięcia ich po prostu z modelu nie wydaje się nam prawidłowa, bo wtedy nadal kategorią referencyjną pozostanie Piemont.

Zdecydowaliśmy zatem o połączeniu ze sobą wszystkich nieistotnych w tym modelu kategorii zamieszkania w jedną, która stanie się nową kategorią referencyjna, która możemy opisać jako: region inny niż te uwzględnione w modelu. Zatem zamieszkiwanie w regionach explicite uwzględnionych w modelu będzie powodowało istotną modyfikację ilorazu szans, wobec zamieszkiwania w dowolnym regionie innym niż te wymienione.
	
Analogicznie postępujemy z pozostałymi zmiennymi o charakterze czynnikowym.

Modyfikujemy czynniki modelu w zależności od ich istotności w modelu wstpnym:
*	Modyfikacja regionów zamieszkania
*	Modyfikujemy typ rodziny w zależności od istotności - czyli typ_rodziny4 i typ_rodziny6 przypisujemy do 1 - zmiennej referencyjnej
*	Modyfikujemy typ wyksztalcenia w zależności od ich istotności - czyli wyksztalcenie2 przypisujemy do 1 - zmiennej referencyjnej






```{r, echo=FALSE, message=FALSE}
#MODYFIKACJA CZYNNIKOW MODELU
#modyfikuje regiony zamieszkania w zaleznosci od ich istotnosci
check <-function(x){
  if(x==3||x==1||x==15||x==14||x==2||x==4||x==5||x==6||x==7||x==8||x==9||x==17||x==19)
    {
    return(1)
  }
  else{
    return(as.numeric(x)+1)
  }
} 
transformed_zamieszkanie<-train$zamieszkanie
transformed_zamieszkanie <- lapply(transformed_zamieszkanie,check) 
nz <- (matrix(unlist(transformed_zamieszkanie), nrow=length(transformed_zamieszkanie), byrow=TRUE)) %>% as.factor() %>%  as_tibble()
nowe_zamieszkanie <- nz$value %>% as.factor() 
#is.factor(nowe_zamieszkanie)
transformed_zamieszkanie_test<-test$zamieszkanie
transformed_zamieszkanie_test <- lapply(transformed_zamieszkanie_test,check) 
nz_test <- (matrix(unlist(transformed_zamieszkanie_test), nrow=length(transformed_zamieszkanie_test), byrow=TRUE)) %>% as.factor() %>%  as_tibble()
nowe_zamieszkanie_test <- nz_test$value %>% as.factor() 
#is.factor(nowe_zamieszkanie_test)
```

```{r, echo=FALSE, message=FALSE}
check.rodzina <-function(x){
  if(x==4||x==6){
    return(1)
  }
  if(x==10)
  {return(10)}
  if(x==7)
  {return(7)}
  if(x==8)
  {return(8)}
  if(x==3)
  {return(3)}
  else{
    return(as.numeric(x))
  }
} 
transformed_rodzina<-train$typ_rodziny
transformed_rodzina<- lapply(transformed_rodzina,check.rodzina) 
rodzina <- (matrix(unlist(transformed_rodzina), nrow=length(transformed_rodzina), byrow=TRUE)) %>% as.factor() %>%  as_tibble()
nowe_rodzina <- rodzina$value %>% as.factor() 
#is.factor(nowe_rodzina)
transformed_rodzina_test<-test$typ_rodziny
transformed_rodzina_test<- lapply(transformed_rodzina_test,check.rodzina) 
rodzina_test <- (matrix(unlist(transformed_rodzina_test), nrow=length(transformed_rodzina_test), byrow=TRUE)) %>% as.factor() %>%  as_tibble()
nowe_rodzina_test <- rodzina_test$value %>% as.factor() 
#is.factor(nowe_rodzina_test)
```



```{r, echo=FALSE, message=FALSE}
check.wykszt <-function(x){
  if(x==2){
    return(1)
  }
  if(x==8)
  {return(8)}
  
  if(x==7)
  {return(7)}
  
  if(x==4)
  {return(4)}
  
  else{
    return(as.numeric(x))
  }
} 
transformed_wykszt<-train$wyksztalcenie
transformed_wykszt<- lapply(transformed_wykszt,check.wykszt) 
nowe_wyksztalcenie <- (matrix(unlist(transformed_wykszt), nrow=length(transformed_wykszt), byrow=TRUE)) %>% as.factor() %>%  as_tibble()
nowe_wyksztalcenie<- nowe_wyksztalcenie$value %>% as.factor() 
#is.factor(nowe_wyksztalcenie)
transformed_wykszt_test <-test$wyksztalcenie
transformed_wykszt_test<- lapply(transformed_wykszt_test,check.wykszt) 
nowe_wyksztalcenie_test <- (matrix(unlist(transformed_wykszt_test), nrow=length(transformed_wykszt_test), byrow=TRUE)) %>% as.factor() %>%  as_tibble()
nowe_wyksztalcenie_test<- nowe_wyksztalcenie_test$value %>% as.factor() 
#is.factor(nowe_wyksztalcenie_test)
```

<span style="color:blue">*Tworzymy nowy zbior testowy i uczacy ze zmodyfikowanymi czynnikami*</span>

```{r}
test1 <-test %>% 
  mutate("nowe_rodzina"= nowe_rodzina_test) %>% 
  mutate("nowe_zamieszkanie"= nowe_zamieszkanie_test) %>% 
  mutate("nowe_wyksztalcenie"=nowe_wyksztalcenie_test)
head(test1)
  
train1 <-train %>% 
  mutate("nowe_rodzina"= nowe_rodzina) %>% 
  mutate("nowe_zamieszkanie"= nowe_zamieszkanie) %>% 
  mutate("nowe_wyksztalcenie"=nowe_wyksztalcenie)
head(train1)
```

<span style="color:blue">*Tworzymy model regresji logistycznej ze zmodyfikowanymi w opisany wcześniej sposób regresorami.*</span>

```{r}
model2 <- glm(owoce  ~  zboza + mieso + ryby + napoje + dochod + nowe_rodzina + nowe_zamieszkanie+ nowe_wyksztalcenie, data = train1, family = "binomial")
summary(model2)
```

Tak oszacowany model uznajemy za nasz model ostateczny. Wprawdzie niektóre zmienne są istotne na zastanawiająco niskim poziomie (jednak ciągle bardziej wymagającym niż umowne 0,05) i przy głębszej analizie można by wziąć pod uwagę ich usunięcie z modelu, przez co model stałby się bardziej czytelny i wyraźniej wskazywałby na wyróżnione pod badanym względem regiony lub poziomy wykształcenia. Jednak formalnie zmienne nie powinny być z niego usuwane, zatem uznajemy tę postać modelu za końcową.
Na podstawie oszacowanych współczynników modelu wyznaczamy i interpretujemy ilorazy szans. Można wyznaczyć przedziały ufności dla ilorazów szans, ale przy tak wysokiej uzyskanej istotności statystycznej jest jasne, że 95%CI będą bardzo wąskie.



```{r}
szansy <- c(exp(2.139e-01), exp(2.888e-01), exp(-7.811e-01 ), exp(-3.038e-01), exp(-4.388e-01), exp(2.531e-01), exp(2.411e-01), exp(4.858e-01), exp(3.568e-01), exp(2.821e-01), exp(2.559e-01), exp(5.088e-01), exp(4.772e-01), exp(5.207e-01))
szansy
```

Przykładowa interpretacja dla zmiennej czynnikowej nowe_rodzina3: W przypadku osoby samotnej w wieku 65 lat lub starszej, szansa, że będzie ona kupować dużo owoców jest około 1,24 razy większa ceteris paribus wobec takiej szansy u osób z połączonych pod względem typu rodziny grup nie uwzględnionych wprost w modelu.

Przykładowa interpretacja dla zmiennej na skali ilorazowej dotyczącej wydatków na produkty zbożowe: przeciętnie rzecz biorąc, z każdym 1 euro więcej wydawanym przez respondenta na produkty zbożowe szansa (nie prawdopodobieństwo) na to, że okaże się on osobą lubiącą owoce (w znaczeniu zdefiniowanym wcześniej) wzrasta ceteris paribus o nieco ponad 1,0185 razy.

Pozostałe parametry modelu interpretujemy w sposób analogiczny.

W kolejnym kroku zastosujemy nasz oszacowany model do zbioru testowego i sprawdzimy trafność predykcji.


<span style="color:blue">*Predykcja*</span>

```{r}
pred <- predict(model2, newdata = test1 %>% select(zboza, mieso, ryby, napoje, dochod,  nowe_rodzina, nowe_zamieszkanie, nowe_wyksztalcenie), type = "response")
pred.values <- ifelse(pred >= 0.5, 1, 0)
table(pred.values)
```

**Poniżej prezentujemy ideę tablicy błędów.** 

![Źródło: Codeproject.com](https://i0.wp.com/lh3.ggpht.com/_qIDcOEX659I/SzjW6wGbmyI/AAAAAAAAAtY/Nls9tSN6DgU/contingency_thumb%5B3%5D.png?zoom=2 "Logo Title Text 1")


```{r}
pred.table <- table(pred.values, test1$owoce)
pred.table
sum(diag(pred.table))/sum(pred.table)
```

Dla naszego zbioru testowego uzyskaliśmy ogólną trafność predykcji nieco powyżej 75%. Musimy niestety sobie uświadomić, że nie jest to tak dobry wynik, na jaki może nominalnie wyglądać. W naszym zbiorze testowym było 1189 „jedynek” oraz 3590 „zer”. Zatem bez używania żadnego modelu, a po prostu strzelając za każdym razem „zero” uzyskalibyśmy nadal około 75% trafności.

Nasz oszacowany model trafniej wprawdzie przewiduje „zera” niż taki sztywny wybór, natomiast „jedynki” przewiduje ze słabszą trafnością. Podsumowując, zdolność predykcyjna modelu określamy jako słabą, wobec naiwnego przypisywania każdemu rekordowi wartości równej „zero”.

Na zbiorze uczącym te trafności były wprawdzie nieco lepsze, ale nie przełożyło się to na efektywność modelu po sprawdzeniu go na nowym zbiorze danych.
	


<span style="color:blue">*Test Walda*</span>

```{r}
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 3:20)
```

Jako ostatni krok w analizie dokonaliśmy weryfikacji istotności modelu jako całości testem Walda. Naturalnie jak należało się spodziewać hipoteza zerowa została odrzucona z bardzo niską wartością p_value, a wcześniejsza weryfikacja zdolności predykcyjnej modelu na zbiorze testowym potwierdza, że w przypadku tak dużych zbiorów danych nawet wysoka istotność statystyczna w żadnym razie nie musi iść w parze z dobrymi cechami predykcyjnymi modelu.

Model może nie wyszedł zbyt idealnie, ale frajda przy jego budowaniu była, a i doświadczenie zostało zebrane.

## 4. Zakończenie i podsumowanie

W projekcie przeprowadziliśmy analizę wiarygodności danych poprzez sprawdzenie na nich prostych zależności ekonomicznych. Wszystkie postulowane przez teorię ekonomii zależności zostały na tym zbiorze danych potwierdzone. Niemniej jednak udało nam się wychwycić pewne niedoskonałości i niespójności tych danych.

Znaczną ilość pracy poświęciliśmy na przekształcenia, selekcję oraz rekodowanie danych surowych, aby przynajmniej częściowo dostosować je do analiz jakie chcieliśmy wykonać.

Następnie wyestymowaliśmy relatywnie prosty choć nie całkowicie banalny model regresji logistycznej bez uwzględniania w nim efektów interakcyjnych pomiędzy zmiennymi, nie przeprowadzaliśmy też w pełnym zakresie zaawansowanej diagnostyki właściwości stochastycznych modelu, np. zagrożenia efektem współliniowości (poza elementarną oceną skorelowania zmiennych ilościowych).

Jako punkt odcięcia dla predykcji zmiennej zależnej w modelu logistycznym zaakceptowaliśmy domyślną wartość równą 0,5, co z perspektywy oceniamy jako decyzję nietrafną. Należało raczej ze względu na dość silne niezrównoważenie zmiennej zależnej w zbiorze testowym, wybrać inną wartość, bardziej zbliżoną do średniego udziału wartości 1, w przekodowanej przez nas zmiennej zależnej. Na pewno dałoby się dobrać taką wartość punktu odcięcia, aby polepszyć parametr trafności predykcji na zbiorze testowym (do poziomu nieco powyżej 80%), pytanie tylko czy taki w pełni dowolny wybór byłby poprawny metodologicznie i czy na jeszcze innym zbiorze testowym znowu nie należałoby zmieniać punktu odcięcia, tego nie jesteśmy pewni.

Uważamy, że przynajmniej częściowo tłumaczy nas także niejasna jakość danych surowych oraz wiele przekształceń jakim je poddaliśmy, co też mogło wprowadzić pewien szum. Obecnie uważamy, że estymacja tego rodzaju rozsądnego modelu wymagałaby zapewne kilku tygodni pracy nad tym*.

*a na tym zbiorze danych być może nawet miesięcy :)


## 4.1 Oszacowanie proporcji udziału w projekcie:

* *pomysł / koncepcja analizy:* Majka 50 – Marek 50
* *kod / programowanie:* Majka 70 – Marek 30
* *interpretacja wyników / wnioski:* Majka 30 – Marek 70.
