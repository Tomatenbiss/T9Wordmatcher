-module(bel2).
-compile(export_all).

%%% Type occurenceList repraesentiert eine Menge von Buchstaben, die in einem Wort vorkommen.
%%% Die Buchstabenvorkommen werden als Tupel repraesentiert, bei denen der erste Wert des Characters ist
%%% und der zweite Wert die Anzahl der Vorkommen.
%%% occurenceLists sind immer nach der alphabetischen Reihenfolge der Buchstaben aufsteigend sortiert.
-type occurrenceList() :: list({char(),non_neg_integer()}).

%%%%%%%%%%%%%%%%
%%%
%%% extractLetters bildet eine Kette von Zahlen auf die moeglichen Buchstabenkombinationen ab.
%%% extractLetters bekommt als Parameter die Zahlenkette und liefert als Ergebnis die Buchstabenkombinationen,
%%% die sich gemaess der Zuordnung bilden lassen
%%%
%%%%%%%%%%%%%%%%
-spec extractLetters(list(non_neg_integer()))->list(list(char())).
extractLetters(XYZ) -> toBeDefined.

%%%%%%%%%%%%%%%%
%%%
%%% letterOccurences errechnet die Haeufigkeit der Buchstabenvorkommen.
%%% letterOccurences bekommt als Parameter eine Zeichenkette und berechnet, welcher Buchstabe wie haeufig vorkommt.
%%% Ergebnis ist eine Liste von Tupeln bestehend aus dem jeweiligen Buchstaben und der Anzahl der Vorkommen.
%%% Die Tupelliste muss alphabetisch nach dem Buchstaben geordnet sein.
%%% Vervollstaendigen Sie die Funktion splitter, die in der Faltungsfunktion angewendet wird.  
%%%
%%%%%%%%%%%%%%%%

-spec splitter(char(),list({char(),non_neg_integer()}))->list({char(), non_neg_integer()}).
splitter(X,Y) -> toBeDefined.

-spec letterOccurences(list(char()))->occurrenceList().  
letterOccurences(Word)-> SList= lists:sort(Word),
					OccList= lists:foldl(fun splitter/2,"",SList),
					lists:reverse(OccList).

%%%%%%%%%%%%%%%%
%%%
%%% groupBy indexiert eine Liste von beliebigen Elementen mit Hilfe einer zu uebergebenden Indexierungsfunktion.
%%% Die Funktion groupBy bekommt als Parameter die Liste sowie die Indexierungsfunktion.  
%%% Bei der Gruppierung werden alle Elemente, die den selben Wert bei der Anwendung der Gruppierungsfunktion
%%% produzieren, in einer Liste zusammengefasst und dem Funktionswert als Schluessel zugeordnet.
%%% So soll bspw. der Aufruf von groupBy(fun(X)->length(X) end, ["Hallo", "das", "ist", "ein", "Test"]) 
%%% die Liste nach der Laenge der Woerter zusammenfassen. Das Ergebnis ist also:
%%% [{3->["das","ist","ein"],{4->"Test"},{5->"Hallo"}].
%%% Die Map soll in einer Datenstruktur namens dict (siehe Erlang-Dokumentation) gespeichert werden.
%%%
%%%%%%%%%%%%%%%%					

-spec groupBy(fun((A) -> B), list(A)) -> dict:dict(B,A).
groupBy(GBFun, List)-> toBeDefined.

%%%%%%%%%%%%%%%%
%%%
%%% dictionaryOccurences soll die Liste der Woerter laden und nach den Buchstabenvorkommen indexieren.
%%% Fuer das Laden des Files kann die Funktion loadDictionary (am Ende der Aufgabenstellung) verwendet werden.  
%%% Die Gruppierung der Woerter soll ueber die vorausgehende Funktion groupBy erfolgen. Dabei
%%% muss die Funktion letterOccurences eingesetzt werden.
%%% Weiterhin muessen - um Gross- und Kleinschreibung zusammenzufuehren - die zu indizierenden Woerter in   
%%% Kleinbuchstaben umgewandelt werden, so dass bspw. die Buchstabenkombination [{$i,1,{$l,1},{$n,1}] sowohl die Woerter
%%% "Lin" als auch "nil" ergibt.
%%%
%%%%%%%%%%%%%%%%
		
-spec dictionaryOccurences()-> dict:dict() | {error,atom()}.
dictionaryOccurences() -> toBeDefined.

%%%%%%%%%%%%%%%%
%%%
%%% cominations soll alle moeglichen Buchstabenteilmengen, die durch die uebergebene occurrenceList
%%% gebildet werden koennen, berechnet werden. So soll bspw. der Aufruf von combinations([{$a,2},{$b,2}])
%%% folgende Kombinationen bilden:
%%% [{97,1}],
%%% [{97,1},{98,1}],
%%% [{97,1},{98,2}],
%%% [{97,2}],[{97,2},
%%% {98,1}],[{97,2},
%%% {98,2}],[{98,1}],
%%% [{98,2}]]
%%% Achtung: Die Anzahl der Buchstabenvorkommen (zweiter Wert des Tupels) muessen immer groesser 0 sein. 

-spec combinations(occurrenceList())->list(occurrenceList()).
combinations([]) -> [ [] ];
combinations([{Letter,Occ}|XS]) -> [ removeZero(Y,{Letter,Q}) || Y<-combinations(XS), Q<-lists:seq(0,Occ)].

%%%%%%%%%%%%%%%%
%%%
%%% Subtract bekommt als Parameter zwei Listen von Buchstabenvorkommen (occurrenceList) und soll die erste von der
%%% zweiten Abziehen. So ergibt bspw. der Aufruf: subtract([{$a,3},{$b,2},{$c,5}],[{$b,7},{$a,6},{$d,8},{$c,5}])
%%% das Ergebnis [{$a,3},{$b,5},{$d,8}].

-spec subtract(occurrenceList(), occurrenceList())-> occurrenceList().
subtract(Occ1, Occ2)-> toBeDefined.

%%%%%%%%%%%%%%%%
%%%	
%%% getWordLists soll aus einer beliebigen occurenceList und einem Dictionary, die Listen von Woertern bilden, die
%%% durch die occurrenceList repraesentiert werden koennen.
%%% So soll bspw. der Aufruf: 
%%% getWordLists([{$e,1},{$i,1},{$l,2},{$n,1},{$r,1},{$u,2},{$x,1},{$z,1}], dictionaryOccurences()).
%%% folgende Liste von Woertern ergeben:
%%%[["Zulu","Rex","nil"],
%%% ["Zulu","Rex","Lin"],
%%% ["Rex","Zulu","nil"],
%%% ["Rex","Zulu","Lin"],
%%% ["Uzi","Rex","null"],
%%% ["Rex","Uzi","null"],
%%% ["Zulu","nil","Rex"],
%%% ["Zulu","Lin","Rex"],
%%% ["Uzi","null","Rex"],
%%% ["null","Uzi","Rex"],
%%% ["nil","Zulu","Rex"],
%%% ["Lin","Zulu","Rex"],
%%% ["rulez","Linux"],
%%% ["Rex","null","Uzi"],
%%% ["null","Rex","Uzi"],
%%% ["Linux","rulez"],
%%% ["Rex","nil","Zulu"],
%%% ["Rex","Lin","Zulu"],
%%% ["nil","Rex","Zulu"],
%%% ["Lin","Rex","Zulu"]]

-spec getWordLists(occurrenceList(), dict:dict())->list(list(list(char()))).
getWordLists(OccList, Dict) -> toBeDefined.	

%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% FilterWords bekommt eine Liste von Zahlen und eine Liste von Saetzen und ermittelt die Saetze
%%% deren Buchstabenfolge sich durch die Zahlfolge repraesentieren laesst (richtige Reihenfolge).
%%%

-spec filterWords(list(char()), list(list(char()))) -> list(list(char)).
filterWords(NumList, WordList)-> toBeDefined.

%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% getSentences fuegt die einzelnen bisher geschriebenen Funktionen zusammen.
%%%	getSentences bekommt eine Nummernliste und erzeugt daraus die Buchstabenkombinationen, die sich daraus
%%% bilden lassen. Aus den Buchstabenkombinationen werden dann die Saetze ermittelt, die sich bilden lassen.
%%%
-spec getSentences(list(char()))-> list(list(list(char()))).  
getSentences(NumberList)->
		PossWords= extractLetters(NumberList),
		OccListWords= lists:map(fun(X)->letterOccurences(X) end,PossWords),
		Dict= dictionaryOccurences(),
		lists:flatmap(fun(X)->getWordLists(X,Dict) end, OccListWords).

%%%%%%%
%%%%%%% Helper Functions
%%%%%%%
%%%%%%% Load Words from Dictionary
%%% loadDictionary laedt das Woerterbuch in eine Liste von Strings.
%%% Achtung: Das Woerterbuch ist ueber die Linux-Manpages generiert - manche Woerter
%%% ergeben nicht unbedingt augenscheinlichen Sinn. 
-spec frname()->list(char()).		
frname()-> "words_eng.txt".

-spec loadDictionary()->{ok, {list(list(char)),integer()}} | {error, atom()}.
loadDictionary() ->    
	case file:open(frname(), [read]) of
		{'ok',S} ->  Content=reader(S,0,[]),
		   file:close(S),
		   {ok,Content};
		{'error', Why} -> {error, Why}
   end.

-spec reader(any(),integer(),list(list(char)))-> {list(list(char())),integer()}.
reader (File,N, Akku) ->
   case io:get_line(File,'') of  
		eof	  -> {lists:reverse(Akku),N};
		{error, Reason}     -> Reason;
		Line -> reader(File, N+1,[lists:filter(fun(X)->X/=$\n end, Line)| Akku])
	   end.

-spec assignChar(char())->list(char()).
assignChar($2)->[$a,$b,$c];
assignChar($3)->[$d,$e,$f];
assignChar($4)->[$g,$h,$i];
assignChar($5)->[$j,$k,$l];
assignChar($6)->[$m,$n,$o];
assignChar($7)->[$p,$q,$r,$s];
assignChar($8)->[$t,$u,$v];
assignChar($9)->[$w,$x,$y,$z].

-spec assignNum(char())->char().
assignNum(X) when X==$a; X==$b; X==$c; X==$A; X==$B; X==$C -> $2;
assignNum(X) when X==$d; X==$e; X==$f; X==$D; X==$E; X==$F -> $3;
assignNum(X) when X==$g; X==$h; X==$i; X==$G; X==$H; X==$I -> $4;
assignNum(X) when X==$j; X==$k; X==$l; X==$J; X==$K; X==$L -> $5;
assignNum(X) when X==$m; X==$n; X==$o; X==$M; X==$N; X==$O -> $6;
assignNum(X) when X==$p; X==$q; X==$r; X==$s; X==$P; X==$Q; X==$R; X==$S -> $7;
assignNum(X) when X==$t; X==$u; X==$v; X==$T; X==$U; X==$V -> $8;
assignNum(X) when X==$w; X==$x; X==$y; X==$z; X==$W; X==$X; X==$Y; X==$Z -> $9.

