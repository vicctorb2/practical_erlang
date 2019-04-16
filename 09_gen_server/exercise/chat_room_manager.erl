-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).

-export([loop/1, handle_call/2]).

-record(room, {
        room_id,
        room_name,
        users = #{},
        history = []
    }).

-record(state, {
        max_rooms_count = 5,
        current_rooms_count = 0,
        rooms = #{}
    }).


start() ->
    InitialState = #state{},
    spawn(?MODULE, loop, [InitialState]).

%%user API
create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).

remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).

get_rooms(Server) ->
    call(Server, {get_rooms}).

add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).

remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).

get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).

send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).

get_messages_history(Server, RoomId) ->
    call(Server, {get_message_history, RoomId}).

call(Server, Msg) ->
    Server ! {Msg, self()},
    receive
        {reply, Reply} -> Reply
    after 5000 -> no_reply
    end.

loop(CurrentState) ->
    receive
        {Msg, From} -> 
            {NewState, Reply} = ?MODULE:handle_call(Msg, CurrentState),
            From ! Reply,
            ?MODULE:loop(NewState);
        stop -> ok
    end. 


%calls hadlers

handle_call({create_room, RoomName}, CurrentState) ->
    CurrentRoomsCount = CurrentState#state.current_rooms_count,
    MaxRoomsCount = CurrentState#state.max_rooms_count,
    case CurrentRoomsCount >= MaxRoomsCount of
        true -> 
            NewState = CurrentState,
            Reply = {error, room_limit};
        false ->
            NewRoom = #room{room_id = make_ref(), room_name = RoomName},
            NewState = CurrentState#state{rooms = maps:put(NewRoom#room.room_id, NewRoom, CurrentState#state.rooms), current_rooms_count = CurrentRoomsCount + 1},
            Reply = {ok, NewRoom#room.room_id}
    end,
    {NewState, {reply, Reply}};

handle_call({remove_room, RoomId}, CurrentState) ->
    CurrentRoomsCount = CurrentState#state.current_rooms_count,
    RoomsMap = CurrentState#state.rooms,
    case maps:find(RoomId, RoomsMap) of
        {ok, _Value} ->
            NewMap = maps:remove(RoomId, RoomsMap),
            NewState = CurrentState#state{rooms = NewMap, current_rooms_count = CurrentRoomsCount - 1},
            Reply = ok;
        error -> 
            Reply = {error, room_not_found},
            NewState = CurrentState
    end,
    {NewState, {reply, Reply}};

handle_call({get_rooms}, CurrentState) ->
    RoomsMap = CurrentState#state.rooms,
    RoomsListTmp = maps:to_list(RoomsMap),
    Reply = lists:foldl(fun({RoomId, {room, _, RoomName, _, _}}, Acc) ->
                                [{RoomId,RoomName} | Acc]
                            end,
                            [],
                            RoomsListTmp),
    {CurrentState, {reply, Reply}};

handle_call({add_user, RoomId, UserName}, CurrentState) ->
    RoomsMap = CurrentState#state.rooms,
    case maps:find(RoomId, RoomsMap) of
        {ok, Room} ->
                CurrentUsersMap = Room#room.users,
                case maps:find(UserName, CurrentUsersMap) of
                    {ok, _Value} -> 
                            Reply = {error, user_is_in_room},
                            {CurrentState, {reply, Reply}};
                    error -> 
                            NewRoom = Room#room{users = maps:put(UserName, make_ref(), CurrentUsersMap)},
                            NewRoomsMap = maps:update(RoomId, NewRoom, RoomsMap),
                            NewState = CurrentState#state{rooms = NewRoomsMap},
                            Reply = ok,
                            {NewState, {reply, Reply}}
                end;
        error -> 
                Reply = {error, room_not_found},
                {CurrentState, {reply, Reply}}
    end;

handle_call({remove_user, RoomId, UserName}, CurrentState) -> 
    RoomsMap = CurrentState#state.rooms,
    case maps:find(RoomId, RoomsMap) of
        {ok, Room} -> 
            UsersInRoomMap = Room#room.users,
            case maps:find(UserName, UsersInRoomMap) of
                {ok, _User} ->
                    NewRoomUsers = maps:remove(UserName, UsersInRoomMap),
                    NewRoom = Room#room{users = NewRoomUsers},
                    NewRoomsMap = maps:update(RoomId,NewRoom, RoomsMap),
                    NewState = CurrentState#state{rooms = NewRoomsMap},
                    {NewState, {reply, ok}};
                error -> 
                    Reply = {error, user_not_in_room},
                    {CurrentState, {reply, Reply}}
            end;
        error ->
            Reply = {error, room_not_found},
            {CurrentState, {reply, Reply}}
    end;


handle_call({get_users_list, RoomId}, CurrentState) ->
    RoomsMap = CurrentState#state.rooms,
    case maps:find(RoomId, RoomsMap) of
        {ok, Room} ->
            UsersInRoom = Room#room.users,
            UsersListTmp = maps:to_list(UsersInRoom),
            Reply = lists:foldl(fun({UserName, _}, Acc) ->
                                    [UserName | Acc]
                                end,
                                [],
                                UsersListTmp),
            {CurrentState, {reply, {ok,Reply}}};
        error -> 
            Reply = {error, room_not_found},
            {CurrentState, {reply, Reply}}
    end;

handle_call({send_message, RoomId, UserName, Message}, CurrentState) ->
    RoomsMap = CurrentState#state.rooms,
    case maps:find(RoomId, RoomsMap) of
        {ok, Room} ->
            UsersInRoom = Room#room.users,
            case maps:find(UserName, UsersInRoom) of
                {ok, _User} -> 
                    NewRoom = Room#room{history = [{UserName, Message} | Room#room.history]},
                    NewRooms = maps:update(RoomId, NewRoom, RoomsMap),
                    NewState = CurrentState#state{rooms = NewRooms},
                    Reply = ok,
                    {NewState, {reply, Reply}};
                error -> 
                    Reply = {error, user_not_in_room},
                    {CurrentState, {reply, Reply}}
            end;
        error -> 
            Reply = {error, room_not_found},
            {CurrentState, {reply, Reply}}
    end;

handle_call({get_message_history, RoomId}, CurrentState) ->
    RoomsMap = CurrentState#state.rooms,
    case maps:find(RoomId,RoomsMap) of
        {ok, Room} ->
            HistoryList = Room#room.history,
            Reply = {ok, HistoryList},
            {CurrentState, {reply, Reply}};
        error ->
            Reply = {error, room_not_found},
            {CurrentState, {reply, Reply}}
    end.