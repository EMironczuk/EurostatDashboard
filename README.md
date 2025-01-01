# Dashboard demograficzny wybranych krajów europejskich

## Charakterystyka oprogramowania
Celem dashboardu jest dostarczenie intuicyjnego narzędzia do analizy danych demograficznych wybranych krajów europejskich, umożliwiającego wizualne porównanie wskaźników demograficznych, takich jak wielkość populacji, współczynnik dzietności, średnia długość życia i śmiertelność niemowląt. Wybór badanych jednostek został zdeterminowany dostępnością danych na podstawie Eurostatu.

Dashboard jest przeznaczony dla analityków danych, ekonomistów, badaczy polityki publicznej oraz decydentów, którzy potrzebują szybkiego dostępu do wskaźników demograficznych. Użytkownicy będą mogli w prosty sposób zidentyfikować kluczowe trendy oraz porównać poszczególne kraje, a także analizować zmiany populacji w czasie.


## Prawa autorskie
...

## Specyfikacja wymagań

![Zrzut ekranu 2025-01-01 134931](https://github.com/user-attachments/assets/0155a994-7470-4404-ae2c-313093cb2b46)

## Architektura systemu/oprogramowania
Dashboard opiera się na architekturze klient-serwer z trzema głównymi komponentami: frontendem (Shiny, Leaflet, DataTables), backendem (Shiny Server) i zewnętrznym API Eurostat. Frontend umożliwia interakcję użytkownika z danymi (mapa, tabela, wskaźniki). Backend przetwarza dane i komunikuje się z API. Dane są pobierane z Eurostat w czasie rzeczywistym lub z lokalnego cache. Dashboard obsługuje filtry (np. rok), wyświetla dane na mapie oraz w tabeli z wyszukiwaniem i sortowaniem. Architektura zapewnia responsywność i możliwość skalowania.

Wymagania systemowe: System operacyjny (Windows/Linux/Mac).
Wymagania oprogramowania: Wersja języka programowania R 4.2.3 (lub nowsza, wtedy z dospasowanymi wersjami bibliotek).

Biblioteki użyte w aplikacji:

![Zrzut ekranu 2025-01-01 135946](https://github.com/user-attachments/assets/eb055492-4f5e-4a95-a8c4-2cfbea97180c)
![Zrzut ekranu 2025-01-01 140134](https://github.com/user-attachments/assets/23ae87dc-de59-44bf-9c51-93f482b0a025)
![Zrzut ekranu 2025-01-01 140431](https://github.com/user-attachments/assets/ca3449b3-653c-4b74-8d12-a1ec499fdc4b)


## Testy
