-module(url_parser).

-export([parse/1]).

-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    try
        {ok, Protocol, WithoutProtocolURI} = parse_protocol(URL),
        {ok, Domain, WithoutDomainURI} = parse_domain(WithoutProtocolURI),
        {ok, WithoutQueryURI, Query} = parse_query(WithoutDomainURI),
        {Path, Date} = parse_path(WithoutQueryURI),
        ResultMap = #{
            date => Date,
            query => Query,
            path => Path,
            domain => Domain,
            protocol => Protocol
        },
        {ok, ResultMap}
    catch
        throw:invalid_protocol -> {error, invalid_protocol};
        throw:invalid_domain -> {error, invalid_domain};
        throw:invalid_date -> {error, invalid_date}
    end.
    

parse_protocol(URL) ->
    case binary:split(URL, <<"://">>) of
        [_OneTerm] ->
            Reason = invalid_protocol,
            throw(Reason);
        [Protocol, RestURI] -> 
            {ok, Protocol, RestURI}
    end.

parse_domain(URI) ->
    case binary:split(URI, <<"/">>) of
        [<<>>] -> throw(invalid_domain);
        [Domain, RestURI] -> {ok, Domain, RestURI}
    end.

parse_path(URI) ->
    PathList = binary:split(URI, <<"/">>, [global]),
    case PathList of
        [_SingleBin] ->
            Date = undefined,
            Result = PathList;
        [OneTerm, <<>>] -> 
            Date = undefined,
            Result = [OneTerm];
        [YearBinary | [MounthBinary | [DayBinary | _PathRest]]] ->
            Date = parse_date(YearBinary, MounthBinary, DayBinary),
            Result = lists:filter(fun(Bin) -> Bin /= <<>> end, PathList)
    end,
    {Result, Date}.

parse_query(URI) ->
    case binary:split(URI, <<"?">>) of
        [_OneElement] -> {ok, URI, <<>>};
        [Rest, Query] -> {ok, Rest, Query}
    end.
            

parse_date(YearBinary, MounthBinary, DayBinary) ->
        YearTemp = string:to_integer(binary_to_list(YearBinary)),
        MounthTemp = string:to_integer(binary_to_list(MounthBinary)),
        DayTemp = string:to_integer(binary_to_list(DayBinary)),
        case {YearTemp, MounthTemp, DayTemp} of
            { {YearNumber, []} , {MounthNumber, []} , {DayNumber, []} } ->
                case check_date(MounthNumber, DayNumber) of
                    correct -> {YearNumber, MounthNumber, DayNumber};
                    incorrect -> undefined
                end;
            _ -> undefined
        end.

check_date(MounthNumber, DayNumber) ->
    case (MounthNumber >= 1 andalso MounthNumber =< 12) andalso (DayNumber >= 1 andalso DayNumber =< 31) of
        true -> correct;
        false -> incorrect
    end.

        

