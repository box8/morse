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

init(Req, State) -> {cowboy_websocket, Req, State, #{idle_timeout => infinity}}.

websocket_init(State) ->
	pg2:join(?MODULE, self()),
	notify_count(),
	{ok, State}.

websocket_handle({binary, Data}, State) ->
	lists:foreach(fun
		(Pid) when Pid =:= self() -> ok;
		(Pid) -> Pid ! {binary, Data}
	end, pg2:get_local_members(?MODULE)),
	{ok, State};
websocket_handle(_Data, State) -> {ok, State}.

websocket_info({binary, Data}, State) -> {reply, {binary, Data}, State};
websocket_info({text, Data}, State) -> {reply, {text, Data}, State};
websocket_info(_Info, State) -> {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
	pg2:leave(?MODULE, self()),
	notify_count(),
	ok.

%% internal

notify(Data) ->
	lists:foreach(fun(Pid) ->
		Pid ! {text, Data}
	end, pg2:get_local_members(?MODULE)).

notify_count() ->
	Data = iolist_to_binary([
		<<"{\"count\":">>,
		integer_to_binary(length(pg2:get_local_members(?MODULE))),
		$}
	]),
	notify(Data).
