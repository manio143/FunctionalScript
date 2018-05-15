# Opis języka
Zdecydowałem się na język funkcyjny, którego składnia jest niejako połączeniem języków Haskell i F# (który jest z rodziny ML). Dodatkowo mamy rekordy, które są trochę jak te w JavaSciptcie. W pliku `Specification.cf` znajduje się gramatyka języka w postaci LBNF, jakiej używa BNFC. Natomiast ze względu na ograniczenia tego konwertera musiałem sam zaimplementować parser języka w związku z czym nie jestem pewny, czy gramatyka opisuje w 100% mój język. Ale starałem się jak najlepiej ją napisać. Poniżej przejdziemy przez wszystkie elementy języka.

## Typy
Z założenia język jest silnie, statycznie typowany. Doczynienia mamy z typami prostymi, listami, krotkami, funkcjami, uniami i rekordami.

### Typy bazowe
Mamy dwa typy bazowe: liczby (zarówno całkowite jak i zmiennoprecinkowe) `Number` oraz znaki `Char`.

Dodatkowo mamy typ `String`, będący listą znaków.

### Listy
Standardowe listy, które można by samemu zadeklarować w taki sposób:

    data List<a> = Empty | Head of (a * List<a>)
    let (:) h t = Head((h,t))

Listy są specjalnym typem z cukrem syntaktycznym. Nie da się samemu odwzorować ich pełnej funkcjonalności w języku.

Typ listy o elementach typu `a` to `[a]`.

Wartość listy pustej oznaczamy przez `[]`, a znajdujące się w niej elementy oddzielamy przecinkami - `[1,2,3]`. Do tego mamy deklarację zakresu `[1..10]` oraz bardziej zaawansowane generatory `[(x,y) | x <- [1..3], y <- ['a','b','c']]`.

Podczas przypisania _let_ możemy użyć formy

    let [a,b] = lista

aby uzyskać pierwsze dwa elementy listy `lista` i przypisać ich wartości do stałych `a` i `b`. Albo

    let (h:t) = lista

aby wyciągnąć pierwszy element i listę pozostałych, odpowiednio do stałych `h` i `t`.

Powyższych dwóch wzorców można również użyć w wyrażeniach _match-with_. Dodatkowo jest jeszcze jeden wzorzec _match-with_ dla list "zawieracz":

    match lista with
    | [... 10 ...] -> ()
    | [... f(11) ...] -> ()

Zawieracz sprawdza czy lista zawiera wartość podanego wyrażenia i jeśli tak to zwraca wartość wyrażenia po prawej stronie strzałki, a jeśli nie to przechodzimy do sprawdzania kolejnego wzorca.

### Krotki
Pary, trójki, itd, czyli krotki. Muszą być otoczone nawiasami, oddzielone przecinkami.

    @k :: Integer * String * Char
    let k = (1, "abc", 'x')

Typ krotki to lista typów oddzielonych przez znak '*'.

Podobnie jak listy możemy używać wzorcu krotki w przypisaniach i wyrażeniach _match-with_. Lista przypisań musi być nie większa niż typ krotki:

    let (i, s) = k      // i = 1, s = "abc"
    let (i, s, c, d) = k //błąd

    match k with
    | (1, _) -> ()
    | (a, b, 'x') -> ()

### Funkcje
Żeby można było uzyskać częściową aplikację to każda funkcja jest w teorii jedno argumentowa. Typ funkcji opisujemy jako `a -> b`. Mamy dwa sposoby na deklarację funkcji

    let f x = x
    let f = \x -> x

Ten drugi sposób korzysta z wyrażenia lambda.

Funkcje wołamy przy użyciu nawiasów, oddzielając argumenty przecinkami. Jeśli nie dostarczymy wystarczającej ilości argumentów to dostaniemy funkcję.

    let f x y z = ()
    let h = f(1,2,3)
    let g = f(1) //g :: Integer -> Integer -> Unit

Funkcje są rekurencyjne.

#### Operatory
W języku są tylko operatory binarne, w związku z czym są funkcjami o dwóch argumentach.

    let (|>) v f = f(v)

Na chwilę obecną gramatyka nie pozwala na tworzenie adnotacji typów dla operatorów, więc obejściem jest

    @myCustomPipe :: a -> (a -> a -> b) -> b
    let myCustomPipe v f = f(v)(v)
    let ($>) = myCustomPipe

Użytkownik może tworzyć swoje własne operatory, które mają najmniejszy priorytet lub korzystać z wbudowanych operatorów

    ["<|","<||","<|||","|>","||>","|||>","<$>", "<$", "$>"],
    ["<", "<=", "==", "===", ">", ">=",
      "!=", "/=", "=/=", "!==", "/=="],
    ["||", "<<", ">>", "<=>", "<==", "==>", ">=>",
     "<=<", "~>", "<~", "<<=", "=>>", "=<<", ">>="],
    ["+", "-","&&"],
    ["*", "/"],
    ["**", "***", "%", "^", "&&&", "&", "|||", "++", "--"]

Powyższa lista operatorów zwiększa priorytet w dół.

Dodatkowo operatory skierowane w lewo (poza porównaniem) oraz `:`, `**` i `***` są operatorami prawostronnymi, a pozostałe (wraz z operatorami użytkownika) są lewo stronne.

### Unie
Unia to inaczej mówiąc typ algebraiczny. Unie mają konstruktory zero lub jedno argumentowe. Unię deklarujemy w następujący sposób

    data Nazwa<(lista generycznych typów)> = lista konstruktorów

Czyli np.

    data Tree<a> = Leaf | Node of Tree<a> * a * Tree<a>
    //Leaf :: Tree<a>
    //Node :: (Tree<a> * a * T) -> Tree<a>

Albo (bez dodatkowych typów i rozpoczynając od nowej linii)

    data Enum =
        | E1
        | E2
        | E3

Unie nie podpadają pod wzorce w przypisaniach _let_. Więc dekonstrukcja wartości unii musi odbyć się przez wyrażenie _match-with_:

    let depth t = match t with
                  | Empty -> 0
                  | Node((tl, _, tr)) -> max(depth tl, depth tr) + 1

### Rekordy
Rekord to taki statyczny słownik. Poniżej zadeklarujemy typ rekordowy

    type Person = {Name :: String; Age :: Integer}

Następnie utworzymy jego element

    @p :: Person
    let p = {Name = "Jan"; Age = 30}

Typy rekordowe są porównywane pod względem pól jakie zawierają i ich typów. Dodatkowo możemy tworzyć nowe typy w oparciu o stare.

    type Teacher extends Person with {Pupils :: [Person]}

I jeśli funkcja przyjmuje obiekt typu `Person` to przyjmie również obiekt typu `Teacher` ponieważ zawiera on wszelkie niezbędne pola o takich samych typach.

Tak jak w przypadku rozszerzania typów istnieje podobna konstrukcja do rozszerzania i modyfikacji wartości

    let teach = {p with Name = "Adrian"; Pupils = []}

Rekordów można użyć do symulacji programowania obiektowego, np:

    //interfejs
    type IEquatable<a> =
        {
            equals :: a -> a -> Bool
        }

    @intEq :: IEquatable<Integer>
    let intEq = {equals = \x y -> x == y}

    @elem :: IEquatable<a> -> a -> [a] -> Bool
    let elem eq x l = //...
    let intElem = elem(intEq)

Funkcja, która przyjmuje rekord danego typu, może przyjąć również rekord o innymi typie, który zawiera potrzebne pola o tych samych typach

    @fr :: {A :: String; B :: Char} -> ()
    
    type R = {A :: String; B :: Char; C :: Integer}
    type Rb = {A :: Integer; B :: Integer}
    
    let _ = fr({A = ""; B = 'c'; C = 10} :: R)
    let _ = fr({A = 1; B = 2} :: Rb) // error: type mismatch

## Struktura programu
Program składa się listy deklaracji. Deklaracją jest definicja typu (rekordu, aliasu lub unii), adnotacja typu i deklaracja wartości. Deklaracje są globalne, a ich kolejność nieistotna. Powoduje to, że nie można redeklarować elementów o tej samej nazwie.

Najważniejszą funkcją w programie jest funkcja

    @main :: [String] -> Integer

która musi być obecna i jest punktem wejścia do programu. Przyjmuje ona listę argumentów oraz zwraca liczbę, z którą program interpretera zakończy działanie.

### Deklaracje typów i adnotacje
W poprzednim rozdziale dotyczącym typów przedstawiłem już jak się deklaruje, ale dla przypomnienia zrobię to raz jeszcze. Każdy typ może mieć listę generycznych argumentów.

    type Alias = String * Int
    type GenericAlias<a> = IEquatable<a> -> a -> a -> Integer
    type Rekord = {A :: T1; B :: T2}
    type RekordRozszerzony extends Rekord with {C :: T3}
    data Unia = E | X of Char

Do tego mamy adnotacje typów, które mają pomóc interpreterowi jeśli nie będzie w stanie domyślić się typu, oraz które mogą posłużyć jako upewnienie się, że wartość ma typ jaki oczekujemy.

    @identyfikator :: typ

### Deklaracje wartości
Wartości globalne są obliczane statycznie przed wywołaniem funkcji `main`, niekoniecznie w kolejności deklaracji w pliku. Wartości globalne (poza funkcjami) nie są rekurencyjne, natomiast wartości lokalne, w wyrażeniach _let_ mogą być rekurencyjne.

Każda deklaracja zaczyna się od słowa kluczowego `let`, następnie mamy wzór wiązania, znak "=" i wyrażenie wartości.

Wzory wiązania to zmienne, funkcje z argumentami (również operatory), dekonstrukcje rekordów, list, tupli lub wildcard ('_'), który służy do pomijania wartości.

Wyrażenia wartości to: podwyrażenia `let`, warunki `if-then-else`, sprawdzanie wzorców `match-with`, wyrażenia wartości, wyrażenia z dopiskiem typu, wywołania funkcji, wyrażenia z operatorem, wyrażenia w których rolę grają skutki uboczne (`do` i `if-do`).

Ponieważ długo by to wszystko tu opisywać, to kieruję czytelnika do pliku `../good/syntaxExamples.fsc`, gdzie przedstawione są wszystkie konstrukcje w języku.

#### Pattern matching
Ponieważ jest to jeden z punktów oceny, to warto wspomnieć, że nie będę ostrzegał użytkownika przed niepełną listą wzorców jakie są sprawdzane. Kiedy programowi skończą się wzory i użytkownik nie zadbał o wildcard to program skończy się z błędem.

Nie będę ostrzegać użytkownika jeśli nie sprawdza wszystkich wzorców. Ta decyzja jest spowodowana tym, że domyślnie Haskell nie ostrzega, co pozwala na nietworzenie "pustych" przypadków, kiedy wiemy coś o tym jakie nasza funkcja będzie otrzymywać wartości

    match value with
    | //important stuff
    | _ -> die("This cannot happen")
