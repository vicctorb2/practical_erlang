-module(task_4).

-export([dropwhile/2, takewhile/2]).

-include_lib("eunit/include/eunit.hrl").


%% implement lists:dropwhile/2
%% http://www.erlang.org/doc/man/lists.html#dropwhile-2
dropwhile(Pred,List) -> dropwhile(Pred,List,[],false).

dropwhile(Pred,[],Acc,_) -> task_2:reverse(Acc); %using our task2 module to reverse
dropwhile(Pred, List, Acc, FoundedFlag) ->  
    [Head | Tail] = List,
    if
        FoundedFlag ->                              %this flag signals that the first non-predicate element was found
            dropwhile(Pred,Tail,[Head | Acc],true); 
        true ->
            case Pred(Head) of                      
                true -> dropwhile(Pred,Tail,Acc,false);  %%founded by predicate -> skip this element  
                false -> dropwhile(Pred, Tail, [Head | Acc],true) %%not founded -> add it to Acc, go to next element 
            end
    end.


dropwhile_test() ->
    F = fun(Val) -> Val =:= 32 end,
    ?assertEqual("hello", dropwhile(F, "   hello")),
    ?assertEqual([], dropwhile(F, [])),
    ?assertEqual([1,2,3], dropwhile(F, [1,2,3])),
    ?assertEqual([3,4], dropwhile(F, [32,3,4])),
    ?assertEqual([3,4], dropwhile(F, [32,32,3,4])),
    ?assertEqual([3,32,4,32], dropwhile(F, [32,32,32,32,32,32,3,32,4,32])),
    ok.


%% implement lists:takewhile/2
%% http://www.erlang.org/doc/man/lists.html#takewhile-2
takewhile(Pred, List) -> 
    if
        List/=[] ->
            takewhile(Pred, List, []);
        true -> 
            [] 
    end.


takewhile(Pred, List, Acc) ->
    [Head | Tail] = List,
    case Pred(Head) of
        true -> takewhile(Pred, Tail, [Head | Acc]);
        false -> task_2:reverse(Acc)
    end.


takewhile_test() ->
    F = fun(Val) -> Val =:= 32 end,
    ?assertEqual("   ", takewhile(F, "   hello")),
    ?assertEqual([], takewhile(F, [])),
    ?assertEqual([], takewhile(F, [1,2,3])),
    ?assertEqual([32], takewhile(F, [32,3,4])),
    ?assertEqual([32,32], takewhile(F, [32,32,3,4])),
    ?assertEqual([32,32,32,32,32,32], takewhile(F, [32,32,32,32,32,32,3,32,4,32])),
    F2 = fun(Val) -> Val < 5 end,
    ?assertEqual([1,2,3,4], takewhile(F2, [1,2,3,4,5,6,7,8])),
    ok.
