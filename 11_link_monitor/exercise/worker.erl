-module(worker).
-export([parser/2, parse_strings/1]).

parser(File, ParentPid) ->
    {ok, Bin} = file:read_file(File),
    Strings = binary:split(Bin, <<"\n">>, [global]),
    ParsedResult = parse_strings(Strings),
    ParentPid ! {result, ParsedResult}.

parse_strings(Strings) ->
    lists:foldl(
    fun
        (<<>>, Acc) -> Acc;
        (String, Acc) ->
            [_Id, Name, BinaryQuantity, _Price]= binary:split(String, <<",">>, [global]),
            IntegerQuantity = list_to_integer(binary_to_list(BinaryQuantity)),
            Acc#{Name => IntegerQuantity}
    end,
    #{},
    Strings).
    