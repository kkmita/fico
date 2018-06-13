# opis (13.06.2018)

### uwaga: kodowanie `RiskPerformance` jest takie: 1 == Bad, 0 == Good
### wynika to z tego, że na excelu z opisem (https://docs.google.com/spreadsheets/d/1KTRVj1uLrObw87YZBlC1zicTYEU69Kago-RnpKbtzK0/edit#gid=1364369573)[link do excela na drive] mamy przy informacji o monotoniczności 'with respect to probab. of bad = 1').

zmieniłem przygotowanie danych - sposób traktowania braków danych itd. Nie ma to 'dużego' wpływu na samo dopasowanie algorytmów, oprócz tego, że kilka zmiennych ciągłych stało się zmiennymi porządkowymi (np. `PercentTradeBalanceNeverDelq` podzieliłem wg. przyjętych progów na zmienną o czterech wartościach __1, 2, 3, 4__, które można traktować jako wartości zmiennej porządkowej (4 jest najgorsza lub najlepsza, nie pamiętam).

## 1) (to jest istotne dla opowieści o danych i o założeniach konkursu)
zrobić PDP dla każdej zmiennej i sprawdzić, czy spełnione są założenia z excela co do monotoniczności danej zmiennej (pamiętajcie: kodowanie jest takie, że 1==Bad)

## 2)
Feature Importance jakiś - zobaczyć jak są istotne

## 3)
Globalne wyjaśnienie modelu - czyli na scorach $\hat{y}$ zbudować proste drzewo/regresję logistyczną z karą czy coś

## 4)
LIME?
