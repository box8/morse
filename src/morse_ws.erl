%% Copyright (c) 2018, Alexei Uskov <al@box8.ru>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(morse_ws).
-export([do/2]).
-include_lib("inets/include/httpd.hrl").

-record(state, {socket, rfc6455, channel}).

do(ModData, Channel) ->
	{ok, State} = ws_upgrade(ModData),
	start(State#state{rfc6455 = rfc6455:init(), channel = Channel}).

ws_upgrade(#mod{socket = Socket, http_version = Version, parsed_header = Header}) ->
	true = string:str(proplists:get_value("connection", Header), "pgrade") > 0,
	"websocket" = string:to_lower(proplists:get_value("upgrade", Header)),
	{_, Key} = lists:keyfind("sec-websocket-key", 1, Header),
	C = base64:encode(crypto:hash(sha, <<(list_to_binary(Key))/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
	process_flag(trap_exit, true),
	ssl:controlling_process(Socket, self()),
	ssl:setopts(Socket, [{active, once}]),
	ok = ssl:send(Socket, [Version, " 101 Switching Protocols", 13, 10,
		"connection: Upgrade", 13, 10,
		"sec-websocket-accept: ", C, 13, 10,
		"upgrade: websocket", 13, 10, 13, 10]),
	{ok, #state{socket = Socket}}.

start(#state{channel = Channel} = State) ->
	Pg = {?MODULE, Channel},
	pg2:create(Pg),
	pg2:join(Pg, self()),
	notify(Pg),
	self() ! {send, {text, [<<"{\"channel\":\"">>, Channel, <<"\"}">>]}},
	loop(State).

loop(State) ->
	receive
		{rfc6455, 8, Data} -> % close
			ssl:send(State#state.socket, rfc6455:serialize(8, 0, Data)),
			close(State);
		{rfc6455, 9, Data} -> % ping
			ssl:send(State#state.socket, rfc6455:serialize(10, 0, Data)),
			loop(State);
		{rfc6455, 10, _} -> loop(State); % pong
		{rfc6455, 2, Data} -> % expecting only binary
			Pg = {?MODULE, State#state.channel},
			lists:foreach(fun
				(Pid) when Pid =:= self() -> ok;
				(Pid) -> Pid ! {send, {binary, Data}}
			end, pg2:get_local_members(Pg)),
			loop(State);
		{rfc6455, _Opcode, _Data} -> loop(State);
		{send, {binary, Data}} ->
			ssl:send(State#state.socket, rfc6455:serialize(2, 0, Data)),
			loop(State);
		{send, {text, Data}} ->
			ssl:send(State#state.socket, rfc6455:serialize(1, 0, Data)),
			loop(State);
		{ssl, _Socket, Data} ->
			State2 = State#state{rfc6455 = rfc6455:read(Data, State#state.rfc6455)},
			ssl:setopts(State#state.socket, [{active, once}]),
			loop(State2);
		{ssl_closed, _Socket} -> close(State);
		{ssl_error, _Socket, _Reason} -> close(State);
		{'EXIT', _Pid, _Reason} -> close(State);
		_ -> loop(State)
	end.

close(#state{socket = Socket, channel = Channel}) ->
	ssl:close(Socket),
	Pg = {?MODULE, Channel},
	pg2:leave(Pg, self()),
	case pg2:get_local_members(Pg) of
		[] -> pg2:delete(Pg);
		_ -> notify(Pg)
	end,
	exit(normal).

notify(Pg) ->
	Pids = pg2:get_local_members(Pg),
	Data = [<<"{\"count\":">>, integer_to_binary(length(Pids)), $}],
	lists:foreach(fun(Pid) -> Pid ! {send, {text, Data}} end, Pids).
