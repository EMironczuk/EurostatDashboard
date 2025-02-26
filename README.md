# Dashboard demograficzny wybranych krajów europejskich

## Charakterystyka oprogramowania
Celem dashboardu jest dostarczenie intuicyjnego narzędzia do analizy danych demograficznych wybranych krajów europejskich, umożliwiającego wizualne porównanie wskaźników demograficznych, takich jak wielkość populacji, współczynnik dzietności, średnia długość życia i śmiertelność niemowląt. Wybór badanych jednostek został zdeterminowany dostępnością danych na podstawie Eurostatu.

Dashboard jest przeznaczony dla analityków danych, ekonomistów, badaczy polityki publicznej oraz decydentów, którzy potrzebują szybkiego dostępu do wskaźników demograficznych. Użytkownicy będą mogli w prosty sposób zidentyfikować kluczowe trendy oraz porównać poszczególne kraje, a także analizować zmiany populacji w czasie.

**Przykłady użycia:**

* Generowanie wykresu populacji dla krajów UE w różnych latach.
* Generowanie wykresu piramidy płci w wybranych krajach i latach.
* Wyświetlanie wybranych wskaźników dla zadanych krajów w wybranych latach.

## Prawa autorskie
**Autorzy:** Anna Sołtys, Wiktoria Wróbel, Elżbieta Mirończuk

**Warunki licencyjne do oprogramowania wytworzonego przez grupę:** 

The MIT License (MIT)

Copyright (c) 2025 Anna Sołtys, Wiktoria Wróbel, Elżbieta Mirończuk

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


## Specyfikacja wymagań

![x1](https://github.com/user-attachments/assets/471019c1-329c-4815-a6f0-25621548b946)

## Architektura systemu/oprogramowania
Dashboard opiera się na architekturze klient-serwer z trzema głównymi komponentami: frontendem (Shiny, Leaflet, DataTables), backendem (Shiny Server) i zewnętrznym API Eurostat. Frontend umożliwia interakcję użytkownika z danymi (mapa, tabela, wskaźniki). Backend przetwarza dane i komunikuje się z API. Dane są pobierane z Eurostat w czasie rzeczywistym lub z lokalnego cache. Dashboard obsługuje filtry (np. rok), wyświetla dane na mapie oraz w tabeli z wyszukiwaniem i sortowaniem. Architektura zapewnia responsywność i możliwość skalowania.

**Nawigacja po dashboardzie:**

* Menu boczne pozwala przełączać się między sekcjami: 'Population', 'Population by country', ‘Country’, oraz ‘Info’. 
* Zakładka ‘Population’ wyświetla interaktywną mapę i tabelę zawierającą dane o wielkości polulacji poszczegółnyk krajów oraz główne wskaźniki populacyjne dla UE. Występuje możliwość filtrowania wyników po zadanym roku. 
* Zakładka ‘Population by country’ wyświetla wykresy oraz wskaźniki dla jednego kraju w danym roku. Występuje możliwość filtrowania wyników po zadanym roku i kraju. 
* Zakładka ‘Country comparison’ wyświetla wykresy oraz wskaźniki dla dwóch wybranych krajów w danym roku. Występuje możliwość filtrowania wyników po zadanym roku oraz zniezależny wybór dwóch państw.
* Zakładka ‘Info’ wyświetla definicje wskaźników.

**Wymagania systemowe:** System operacyjny (Windows/Linux/Mac).

**Wymagania oprogramowania:** Wersja języka programowania R 4.2.3 (lub nowsza, wtedy z dospasowanymi wersjami bibliotek).

**Biblioteki użyte w aplikacji:**

![Zrzut ekranu 2025-01-01 135946](https://github.com/user-attachments/assets/eb055492-4f5e-4a95-a8c4-2cfbea97180c)
![Zrzut ekranu 2025-01-01 140134](https://github.com/user-attachments/assets/23ae87dc-de59-44bf-9c51-93f482b0a025)
![Zrzut ekranu 2025-01-01 140431](https://github.com/user-attachments/assets/ca3449b3-653c-4b74-8d12-a1ec499fdc4b)


## Testy
**Scenariusze testów:**

*a)	Sprawdzenie funkcjonalności zakładki „Population”*

Opis: Użytkownik sprawdza, czy aplikacja wyświetla dane populacji, wskaźników, kod kraju oraz sprawdza funkcjonalność mapy.

1.	Wybranie dowolnego roku w zakładce „Year”.
2.	Sprawdzenie czy aplikacja pokazuje wartości dla wyświetlanych wskaźników.
3.	Sprawdzenie czy aplikacja wyświetla kod kraju po najechaniu na niego kursorem myszy.
4.	Sprawdzenie czy aplikacja wyświetla liczbę populacji dla wybranego kraju po użyciu lewego przycisku myszy.
5.	Sprawdzenie czy wyszukiwarka nad listą krajów działa poprawnie.
6.	Zaznaczenie dowolnej liczby krajów z listy w celu sprawdzenia czy granice zaznaczonych państw zostaną zaznaczone na czerwono.


Wynik: Aplikacja wyświetla wartości dla populacji i kody kraju oraz wartości wskaźników w danym roku. Wyszukiwarka działa poprawnie. Wybrane państwa z listy są podświetlone na czerwono na mapie.

*b)	Sprawdzenie funkcjonalności zakładki „Population by country”*

Opis: Użytkownik sprawdza, czy aplikacja wyświetla dane dla wybranego kraju w wybranym roku.
1.	Wybranie dowolnego kraju w zakładce „Country” oraz roku w zakładce „Year”.
2.	Sprawdzenie czy aplikacja wyświetla wykresy liniowe dla populacji oraz wartość środków przeznaczanych na wsparcie socjalne.
3.	Sprawdzenie czy aplikacja wyświetla wykres piramidy wieku.
4.	Sprawdzenie czy aplikacja wyświetla wartości wskaźników.

Wynik: Aplikacja wyświetla wykresy oraz wartości wskaźników dla wybranego kraju w wybranym roku.

*c)	Sprawdzenie funkcjonalności zakładki „Country comparison”*

Opis: Użytkownik sprawdza czy aplikacja wyświetla dane dla dwóch wybranych krajów Unii Europejskiej w wybranym roku.

1.	Wybranie dwóch różnych krajów w zakładce „Country 1”, „Country 2” oraz roku w zakładce „Year”.
2.	Sprawdzenie czy aplikacja wyświetla wykresy liniowe dla populacji oraz wartość środków przeznaczanych na wsparcie socjalne.
3.	Sprawdzenie czy aplikacja wyświetla wykres piramidy wieku.
4.	Sprawdzenie czy aplikacja wyświetla wartości wskaźników.

Wynik: Aplikacja wyświetla wykresy oraz wartości wskaźników dla obu wybranych krajów w wybranym roku.

*d)	Sprawdzenie funkcjonalności zakładki „Info”*

Opis: Użytkownik sprawdza, czy aplikacja wyświetla definicje wybranych wskaźników.
1.	Rozwinięcie panelu z nazwą wskaźnika.
2.	Sprawdzenie czy definicja pasuje do wskaźnika.

Wynik: Aplikacja wyświetla definicje.

**Sprawozdanie z wykonania scenariuszy testów**
![x2](https://github.com/user-attachments/assets/a6a71085-a3a1-4582-be9d-248b081f5765)

## Uwagi
Więcej na temat API eurostatu oraz wykorzystania go za pomocą bibliotek R można znaleźć na:
* https://ec.europa.eu/eurostat/web/user-guides/data-browser/api-data-access/api-getting-started/api
* https://ropengov.github.io/eurostat/articles/eurostat_tutorial.html

