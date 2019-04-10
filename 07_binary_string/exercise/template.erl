-module(template).

-export([parse/2, init/0]).


init() -> 
	In = <<"User {{name}} won {{wins}} games and got {{points}} points">>,
	Data = #{<<"name">> => "Kate",
             <<"wins">> => 55,
             <<"points">> => 777},
    {In, Data}.

parse(Str, Data) when is_binary(Str) ->
	Keys = maps:fold(fun(K, _V, Acc) -> [K | Acc] end, [], Data),
	Values = maps:fold(fun(_K, V, Acc) -> [V | Acc] end, [], Data),
	binary:replace(In, <<"{{" ++ [K | KeysRest] = Keys} ++ "}}">>, << [Value | ValueRest] = Values).

