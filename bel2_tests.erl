-module(bel2_tests).
-include_lib("eunit/include/eunit.hrl").

extractLetters_test_()->
	[?_assertEqual(["gda","hda","ida","gea","hea","iea","gfa","hfa","ifa","gdb","hdb","idb","geb","heb",
		"ieb","gfb","hfb","ifb","gdc","hdc","idc","gec","hec","iec","gfc","hfc","ifc"],
		bel2:extractLetters("234")),
		?_assertEqual(81, length(bel2:extractLetters("2342"))),
		?_assertEqual(236196, length(bel2:extractLetters("23478432256")))
	].

letterOccurences_test_()->
	?_assertEqual([{$a,3},{$b,2},{$c,2},{$d,1},{$e,1},{$f,2},{$g,1}], bel2:letterOccurences("abccbadaeffg")).

groupBy_test_()->
	?_assertEqual([{3,["ein","ist","ist"]},{4,["Dies","Test"]},
		{6,["spitze"]},{11,["Funktionale"]},{14,["Programmierung"]}],
		prepDict(bel2:groupBy(fun(X)->length(X) end,
			["Dies","ist","ein","Test","Funktionale","Programmierung","ist","spitze"]))).
%%
getWordsByOccurences_test_()->
	?_assertEqual({ok,["ate","eat","tea"]},
		getWordsByOccurences("eat",bel2:dictionaryOccurences())).

subsetsOccurences_test_()->
	[?_assertEqual([[],[{97,1}],[{97,1},{98,1}],[{97,1},{98,2}],[{97,2}],[{97,2},{98,1}],[{97,2},
		{98,2}],[{98,1}],[{98,2}]], begin
																	L=bel2:combinations([{$a,2},{$b,2}]),
																	LS= lists:map(fun(X)->lists:keysort(1,X) end, L),
																	lists:sort(LS)
																end)].

subtract_test_()->
	[?_assertEqual([{97,3},{98,5},{100,8}],lists:keysort(1,bel2:subtract([{$a,3},{$b,2},{$c,5}],[{$b,7},{$a,6},{$d,8},{$c,5}])))].

%% getWordLists_test_()->
%% 	[?_assertEqual(lists:sort([["Zulu","Rex","nil"],["Zulu","Rex","Lin"],["Rex","Zulu","nil"],
%% 		["Rex","Zulu","Lin"],["Uzi","Rex","null"],["Rex","Uzi","null"],["Zulu","nil","Rex"],
%% 		["Zulu","Lin","Rex"],["Uzi","null","Rex"],["null","Uzi","Rex"],["nil","Zulu","Rex"],
%% 		["Lin","Zulu","Rex"],["rulez","Linux"],["Rex","null","Uzi"],["null","Rex","Uzi"],
%% 		["Linux","rulez"],["Rex","nil","Zulu"],["Rex","Lin","Zulu"],["nil","Rex","Zulu"],
%% 		["Lin","Rex","Zulu"]]), lists:sort(bel2:getWordLists([{$e,1},{$i,1},{$l,2},{$n,1},{$r,1},{$u,2},{$x,1},{$z,1}],
%% 		bel2:dictionaryOccurences())))].

%% getSentences_test_()->
%% 	Erg=bel2:getSentences("375264"),
%% 	?assert(length(Erg)==2220),
%% 	?assert(length(lists:filter(fun(X)-> length(X)==1 end, Erg))==44),
%% 	?assert(length(lists:filter(fun(X)-> length(X)==2 end, Erg))==2008),
%% 	?assert(length(lists:filter(fun(X)-> length(X)==3 end, Erg))==168),
%% 	?assert(length(lists:filter(fun(X)-> length(X)==4 end, Erg))==0),
%% 	?assert(bel2:filterWords("375264",Erg)==[["Erlang"]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%
%%%%%   Helper Functions
%%%%%
%%%%%	Prepare Dictionary for Comparisons

prepDict(Dict) -> L= dict:to_list(Dict),
	SortedV= lists:map(fun({Key,Value})->{Key, lists:sort(Value)} end, L),
	lists:keysort(1,SortedV).

getWordsByOccurences(Word, DictOcc) -> dict:find(bel2:letterOccurences(Word),DictOcc).

%%%%%
%%%%%
