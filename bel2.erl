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

extractLetters([]) -> [[]];
extractLetters([S|LS]) -> [Res++[C]||C<-assignChar(S),Res<-extractLetters(LS)].


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
%% splitter(X,Y) -> .

%% searchT([],Key) -> [{Key,1}];
%% searchT([{K,V}|LS],Key) when K==Key ->
%% searchT(L,Key) -> lists:delete({K,V})++{Key,V+1};

elementForKey([],_) -> false;
elementForKey([{K,V}|LS],Key) when K == Key -> {Key,V};
elementForKey([L|LS],Key) -> elementForKey(LS,Key);
elementForKey(X,Y) -> X.

splitter(X,[]) -> [{X,1}];
splitter(X,L) -> case elementForKey(L,X) of
									 false -> [{X,1}|L];
									 {Key,V} -> [{Key,V+1}|lists:delete({Key,V},L)]
								 end.

-spec letterOccurences(list(char()))->occurrenceList().  
letterOccurences(Word)-> SList= lists:sort(Word),		% nicht mehr original, da to_lower
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
groupBy(GBFun, List)-> groupBy(GBFun,List,dict:new()).
groupBy(GBFun,[],Acc) -> Acc;
groupBy(GBFun, [L|LS],Acc) -> groupBy(GBFun,LS,dict:append(GBFun(L),L,Acc)).

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
dictionaryOccurences() ->
	{_,{L,_}} = loadDictionary(),
	groupBy(fun letterOccurences/1,L).

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

removeZero(L,{Letter,Occ}) when Occ >0 -> [{Letter,Occ}|L];
removeZero(L,_) -> L.

-spec combinations(occurrenceList())->list(occurrenceList()).
combinations([])->[[]];
combinations([{Letter,Occ}|XS])->[removeZero(Y,{Letter,Q})||Y<-combinations(XS),Q<-lists:seq(0,Occ)].

%%%%%%%%%%%%%%%%
%%%
%%% Subtract bekommt als Parameter zwei Listen von Buchstabenvorkommen (occurrenceList) und soll die erste von der
%%% zweiten Abziehen. So ergibt bspw. der Aufruf: subtract([{$a,3},{$b,2},{$c,5}],[{$b,7},{$a,6},{$d,8},{$c,5}])
%%% das Ergebnis [{$a,3},{$b,5},{$d,8}].

minus(El,[]) -> El;
minus({Letter1,Number1},[{Letter2,Number2}|LS]) when (Letter1==Letter2) -> {Letter1,Number1-Number2};
minus(El, [L|LS]) -> minus(El,LS).

-spec subtract(occurrenceList(), occurrenceList())-> occurrenceList().
subtract(Occ1, Occ2)-> subtract(Occ1,Occ2,[]).
subtract(Occ1, [], Acc) -> Acc;
subtract(Occ1,[L|LS],Acc) -> subtract(Occ1,LS,removeZero(Acc,minus(L,Occ1))).

%%%%%%%%%%%%%%%%
%%%	
%%% getWordLists soll aus einer beliebigen occurenceList und einem Dictionary, die Listen von Woertern bilden, die
%%% durch die occurrenceList repraesentiert werden koennen.
%%% So soll bspw. der Aufruf: 
%%% getWordLists([{$e,1},{$i,1},{$l,2},{$n,1},{$r,1},{$u,2},{$x,1},{$z,1}], dictionaryOccurences()).
%%% folgende Liste von Woertern ergeben:
%%%[["Zulu","Rex","nil"], 1
%%% ["Zulu","Rex","Lin"],	2
%%% ["Rex","Zulu","nil"],	1
%%% ["Rex","Zulu","Lin"],	2
%%% ["Uzi","Rex","null"],	3
%%% ["Rex","Uzi","null"],	3
%%% ["Zulu","nil","Rex"],	1
%%% ["Zulu","Lin","Rex"],	2
%%% ["Uzi","null","Rex"],	3
%%% ["null","Uzi","Rex"],	3
%%% ["nil","Zulu","Rex"],	1
%%% ["Lin","Zulu","Rex"],	2
%%% ["rulez","Linux"],		4
%%% ["Rex","null","Uzi"],	3
%%% ["null","Rex","Uzi"],	3
%%% ["Linux","rulez"],		4
%%% ["Rex","nil","Zulu"],	1
%%% ["Rex","Lin","Zulu"],	2
%%% ["nil","Rex","Zulu"],	1
%%% ["Lin","Rex","Zulu"]]	2


%%%[["Zulu","Rex","nil"], 1
%%% ["Rex","Zulu","nil"],	1
%%% ["Zulu","nil","Rex"],	1
%%% ["nil","Zulu","Rex"],	1
%%% ["Rex","nil","Zulu"],	1
%%% ["nil","Rex","Zulu"],	1

%%% ["Zulu","Rex","Lin"],	2
%%% ["Lin","Rex","Zulu"]]	2
%%% ["Rex","Zulu","Lin"],	2
%%% ["Lin","Zulu","Rex"],	2
%%% ["Zulu","Lin","Rex"],	2
%%% ["Rex","Lin","Zulu"],	2

%%% ["Uzi","Rex","null"],	3
%%% ["Rex","Uzi","null"],	3
%%% ["Uzi","null","Rex"],	3
%%% ["null","Uzi","Rex"],	3
%%% ["Rex","null","Uzi"],	3
%%% ["null","Rex","Uzi"],	3

%%% ["rulez","Linux"],		4
%%% ["Linux","rulez"],		4



%% getWords(Combs,Dict) -> getWords(Combs,Dict,[]).
%% getWords([],Dict,Acc) -> Acc;
%% getWords([Word|List],Dict,Acc) ->
%% 	case dict:find(Word,Dict) of
%% 		error -> getWords(List,Dict,Acc);
%% 		{ok,L}-> getWords(List,Dict,lists:append(L,Acc))
%% 	end.

combinationsExceptEmpty(OccList) -> [A || A<-combinations(OccList), A /= []].

search(error)->[];
search({_,X})->X.

getWords(Occ,Dict) -> case dict:find(Occ,Dict) of
												error -> [error];
												{ok,L} -> L
											end.

isKey(Occ,Dict) -> case dict:find(Occ,Dict) of
										 error -> [error];
										 _ -> Occ
									 end.


%% getWords([X|[]],Dict,Acc)->search(dict:find(X,Dict))++Acc;
%% getWords([X| Combs],Dict,Acc)-> getWords(Combs,Dict,search(dict:find(X,Dict))++Acc).








-spec getWordLists(occurrenceList(), dict:dict())->list(list(list(char()))).
%% getWordLists(OccList,Dict)->getWordLists(OccList,Dict,[]).
%% getWordLists([],_,Result)->[Result];
%% getWordLists(OccList,Dict,Result)->lists:concat(lists:filter(fun(Y)->Y/=[]end,[getWordLists(subtract(letterOccurences(string:to_lower(A)),OccList),Dict,Result++[A])||A<- getWords(combinationsExceptEmpty(OccList),Dict,[])])).



%% createWordLists([],Dict) -> [];
%% createWordLists(Keys,Dict) -> [[Words]++createWordLists(Keys--[Key],Dict)||Key<-Keys,Words<-getWords(Key,Dict)].

%% getWordLists(Occ,Dict) ->

%% getWordLists(Occ,Dict) -> [{Word,C}||C<-combinations(Occ),Word<-getWords(C,Dict),Word /= error].

getWordLists(Occ,Dict) -> getWordLists(Occ,Dict,[]).
getWordLists([],Dict,Acc) -> Acc;
getWordLists(Occ,Dict,Acc) -> [getWordLists(subtract(C,Occ),Dict,Acc++[Word])||C<-combinations(Occ),Word<-getWords(C,Dict),Word /= error].

%% getWordLists(Occ,Dict) ->
%% 	Keys = lists:filter(fun(X)->X/=[error] end,lists:map(fun(X)->isKey(X,Dict) end,combinations(Occ))),
%% 	Res = createWordLists(Keys,Dict),
%% 	pass.

%% getWordLists([],Dict) -> [];
%% getWordLists(Occ,Dict) -> [[Words]++getWordLists(subtract(C,Occ),Dict)||C<-combinations(Occ),Words<-getWords(C,Dict),Words /= error].

%% getWordLists([],Combinations,Dict) -> [];
%% getWordLists(Occ,[C|CS], Dict) ->
%% 	C = combinations(Occ),
%% 	case getWords(C,Dict) of
%% 		error -> getWordLists(Occ,CS,Dict);
%% 		L ->
%% 	end.

%% getWordLists(OccList, Dict) ->
%% 	Combinations = combinations(OccList),
%% 	Res = lists:filter(fun(X) -> X /= error end,lists:map(fun(X)-> getWords(X,Dict) end ,Combinations)),
%% 	pass.

%% getWordLists(OccList, Dict) ->
%% 	Keys = dict:fetch_keys(Dict),
%% 	C = combinations(OccList),
%% 	Wat = dict:find([{105,1},{108,1},{110,1}],Dict),
%% 	Words = getWords(C,Dict),
%% 	pass.

%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% FilterWords bekommt eine Liste von Zahlen und eine Liste von Saetzen und ermittelt die Saetze
%%% deren Buchstabenfolge sich durch die Zahlfolge repraesentieren laesst (richtige Reihenfolge).
%%%

charsFromWordList(Stelle, X) -> lists:nth(Stelle, string:to_lower(X)).

-spec filterWords(list(char()), list(list(char()))) -> list(list(char)).
filterWords(NumList, WordList) -> filterWords(NumList, WordList, 1).
filterWords([X|[]], WordList, Y) -> [ A || D<-assignChar(X), A<-WordList, D == charsFromWordList(Y, lists:concat(A))];
filterWords([X|XS], WordList, Y) -> filterWords(XS, [ A || D<-assignChar(X), A<-WordList, D == charsFromWordList(Y, lists:concat(A))], Y + 1).


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
%% frname()-> "words_eng.txt".
frname()-> "/Users/pascal/Developer/erlang/HTWFunctionalProgramming/Assignment2/src/words_eng.txt".

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%
%%%%%   Helper Functions
%%%%%
%%%%%	Prepare Dictionary for Comparisons

prepDict(Dict) -> L= dict:to_list(Dict),
	SortedV= lists:map(fun({Key,Value})->{Key, lists:sort(Value)} end, L),
	lists:keysort(1,SortedV).

getWordsByOccurences(Word, DictOcc) ->
	dict:find(bel2:letterOccurences(Word),DictOcc).

%%%%%
%%%%%


main()->
	Occ = [{$e,1},{$i,1},{$l,2},{$n,1},{$r,1},{$u,2},{$x,1},{$z,1}],
	Res = bel2:getWordLists(Occ, bel2:dictionaryOccurences()),
%% 	Com = combinations(Occ),
%% 	C1 = [{101,1}],
%% 	Wat = subtract(C1,Occ),
	pass.