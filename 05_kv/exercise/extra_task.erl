-module(extra_task).

-export([
	get_users/0,
	group_by_gender/1,
	criteria_func_gender/1,
	criteria_func_age/1,
	group_by/2]).


get_users() ->
	[
 		{user, "Bob", 21, male},
 		{user, "Bill", 23, male},
 		{user, "Helen", 17, female},
 		{user, "Kate", 25, female},
 		{user, "John", 20, male},
 		{user,"Arnold", 80, male},
 		{user, "Ann", 3, female}
	].


%--------------Task 1 --------------------------------

group_by_gender(Users) ->
	Map = lists:foldr(
		fun(User, Map) ->
			{user, _, _, Gender} = User,
			Males = maps:get(male,Map),
			Females = maps:get(female,Map),
			case Gender of
				male -> maps:update(male, [User | Males], Map);
				female -> maps:update(female, [User | Females], Map)
			end
		end,
		#{male => [], female => []},
		Users),
	Map.


%--------------Task 2 --------------------------------

criteria_func_gender(User) ->
	{user, _, _, Gender} = User,
	Gender.

criteria_func_age(User) ->
	{user, _, Age, _} = User,
	if
		Age > 0 andalso Age =< 12 -> child;
		Age > 12 andalso Age =< 18 -> teenage;
		Age > 18 andalso Age =< 25 -> young;
		Age > 25 andalso Age =< 60 -> adult;
		Age > 60 -> old;
		true -> undefined
	end.

group_by(CriteriaFun, Users) ->
	lists:foldl(
		fun(User, MapAcc) ->
			CurrCategory = CriteriaFun(User),
			case maps:find(CurrCategory,MapAcc) of 
				error -> maps:put(CurrCategory, [User], MapAcc);
				{ok,_} -> maps:put(CurrCategory,[User | maps:get(CurrCategory,MapAcc)], MapAcc)
			end
		end,
		#{},
		Users).