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

-module(morse_handler_ws).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% exports

init(Req, _State) ->
	Channel = cowboy_req:qs(Req),
	{cowboy_websocket, Req, Channel, #{idle_timeout => infinity}}.

websocket_init(Channel) ->
	Pg = {?MODULE, Channel},
	pg2:create(Pg),
	pg2:join(Pg, self()),
	notify(Pg),
	Data = iolist_to_binary([<<"{\"channel\":\"">>, Channel, <<"\"}">>]),
	{reply, {text, Data}, Pg}.

websocket_handle({binary, Data}, Pg) ->
	lists:foreach(fun
		(Pid) when Pid =:= self() -> ok;
		(Pid) -> Pid ! {binary, Data}
	end, pg2:get_local_members(Pg)),
	{ok, Pg};
websocket_handle(_Data, Pg) -> {ok, Pg}.

websocket_info({binary, Data}, Pg) -> {reply, {binary, Data}, Pg};
websocket_info({text, Data}, Pg) -> {reply, {text, Data}, Pg};
websocket_info(_Info, Pg) -> {ok, Pg}.

terminate(_Reason, _PartialReq, Pg) ->
	pg2:leave(Pg, self()),
	case pg2:get_local_members(Pg) of
		[] -> pg2:delete(Pg);
		_ -> notify(Pg)
	end,
	ok.

%% internal

notify(Pg) ->
	Pids = pg2:get_local_members(Pg),
	Data = iolist_to_binary([<<"{\"count\":">>, integer_to_binary(length(Pids)), $}]),
	lists:foreach(fun(Pid) -> Pid ! {text, Data} end, Pids).
