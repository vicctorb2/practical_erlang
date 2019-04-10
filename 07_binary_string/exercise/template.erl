-module(template).

-export([parse/2, init/0]).


init() -> 
	In = <<"User {{name}} won {{wins}} games and got {{points}} points">>,
	Data = #{<<"name">> => "Kate",
             <<"wins">> => 55,
             <<"points">> => 777},
    {In, Data}.

parse(Str, Data) when is_binary(Str) ->
	SplittedList = binary:split(Str, [<<"{{">>], [global]),
	ListWithReplaced = lists:map(
		fun(SplittedVal) ->
			case binary:split(SplittedVal, [<<"}}">>]) of
				 [WithoutParam] -> WithoutParam;
				 [Param | Rest] -> 
				 	case maps:find(Param, Data) of
				 		error -> Rest;
				 		{ok, Value} when is_binary(Value) orelse is_list(Value) -> [Value, Rest];
				 		{ok, Value} when is_integer(Value) -> [integer_to_binary(Value), Rest]
				 	end
			end
		end,
		SplittedList),
	unicode:characters_to_binary(ListWithReplaced).

