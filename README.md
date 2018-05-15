# Interpreter
Autor: Marian Dziubiak

## Budowanie
Jeśli chcemy modyfikować kod to należy używać `make interpreter`, który nie czyści plików pośrednich.
W przeciwnym przypadku wystarczy `make`

## Flow
Program zaczynamy w pliku `Main.hs` skąd uruchamiamy `Parser` na pliku źródłowym, dostarczonym w argumencie. Następnie otrzymane AST jest przekazywane to modułu `Translator`, gdzie dokonywane jest sprawdzanie typów oraz konwersja drzewa AST do uproszczonego modelu z `ProgramState`. Na tym etapie obliczane są też globalne wartości. Na koniec uruchamiamy ewaluację funkcji `main` opisanej w pliku źródłowym.

## Opis języka
W folderze `opis języka` znajdziemy plik `README` ze słownym opisem języka i plik gramatyki języka w formacie LBNF.

## Przykładowe programy
W folderach `good` i `bad` znajdują się przykładowe programy. Te w folderze `bad` sie nie uruchamiają ze względu na pewne błędy.