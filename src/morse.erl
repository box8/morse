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

-module(morse).
-behaviour(application).
-behaviour(supervisor).
-export([start/0, start/2, stop/1, init/1, init/2]).

start() ->
	ok = application:start(inets, permanent),
	ok = application:start(crypto, permanent),
	ok = application:start(asn1, permanent),
	ok = application:start(public_key, permanent),
	ok = application:start(ssl, permanent),
	ok = application:start(ranch, permanent),
	ok = application:start(cowlib, permanent),
	ok = application:start(cowboy, permanent),
	ok = application:start(?MODULE, permanent).

%% application

start(_Type, _Args) ->
	{ok, Config} = application:get_env(?MODULE, config),
	{_, Addr} = proplists:lookup(addr, Config),
	{_, Port} = proplists:lookup(port, Config),
	{_, CaCertFile} = proplists:lookup(cacertfile, Config),
	{_, CertFile} = proplists:lookup(certfile, Config),
	{_, KeyFile} = proplists:lookup(keyfile, Config),
	{_, Secret} = proplists:lookup(secret_prefix, Config),
	AppPath = filename:dirname(filename:dirname(code:which(?MODULE))),
	PrivDir = filename:join(AppPath, "priv"),
	Dispatch = cowboy_router:compile([
		{"127.0.0.1", [{"/halt", ?MODULE, []}]},
		{'_', [
			{Secret ++ "/", cowboy_static, {file, filename:join(PrivDir, "index.html"), [{mimetypes, cow_mimetypes, web}]}},
			{Secret ++ "/ws", morse_handler_ws, []}
		]}
	]),
	{ok, _} = cowboy:start_tls(https, [
		{cacertfile, CaCertFile},
		{certfile, CertFile},
		{ip, Addr},
		{keyfile, KeyFile},
		{port, Port}
	], #{env => #{dispatch => Dispatch}}),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

%% supervisor

init(_) -> {ok, {{one_for_one, 5, 20}, []}}.

%% halt handler

init(Req, State) ->
	case cowboy_req:peer(Req) of
		{{127, 0, 0, 1}, _} -> erlang:halt(0, [{flush, false}]);
		_ -> ok
	end,
	Req2 = cowboy_req:reply(200, Req),
	{ok, Req2, State}.
