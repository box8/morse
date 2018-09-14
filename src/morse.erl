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
-export([start/0, start/2, stop/1, init/1, do/1]).

-include_lib("inets/include/httpd.hrl").

start() ->
	ok = application:start(inets, permanent),
	ok = application:start(crypto, permanent),
	ok = application:start(asn1, permanent),
	ok = application:start(public_key, permanent),
	ok = application:start(ssl, permanent),
	ok = application:start(?MODULE, permanent).

%% application

start(_Type, _Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

%% supervisor

init(_) ->
	ets:new(config, [public, named_table]),
	{ok, Config} = application:get_env(?MODULE, config),
	{_, Addr} = proplists:lookup(addr, Config),
	{_, Port} = proplists:lookup(port, Config),
	{_, CaCertFile} = proplists:lookup(cacertfile, Config),
	{_, CertFile} = proplists:lookup(certfile, Config),
	{_, KeyFile} = proplists:lookup(keyfile, Config),
	{_, Secret} = proplists:lookup(secret_prefix, Config),
	ets:insert(config, {secret, Secret}),
	AppPath = filename:dirname(filename:dirname(code:which(?MODULE))),
	PrivDir = filename:join(AppPath, "priv"),
	ets:insert(config, {index_html, filename:join(PrivDir, "index.html")}),
	inets:start(httpd, [
		{bind_address, Addr},
		{document_root, PrivDir},
		{modules, [?MODULE]},
		{port, Port},
		{server_name, []},
		{server_root, PrivDir},
		{server_tokens, none},
		{socket_type, {essl, [
			{cacertfile, CaCertFile},
			{certfile, CertFile},
			{keyfile, KeyFile},
			{verify, verify_none}
		]}}
	]),
	{ok, {{one_for_one, 5, 20}, []}}.

%% inetd module

do(#mod{request_uri = Uri} = ModData) ->
	Secret = ets:lookup_element(config, secret, 2),
	case Uri of
		"/halt" ->
			case ssl:peername(ModData#mod.socket) of
				{ok, {{127, 0, 0, 1}, _Port}} ->
					erlang:halt(0, [{flush, false}]),
					{break, [{response, {204, []}}]};
				_ -> {break, [{response, {404, []}}]}
			end;
		_ ->
			case string:prefix(Uri, Secret) of
				nomatch -> {break, [{response, {404, []}}]};
				Tail ->
					{Path, Channel} = case string:chr(Tail, $?) of
						0 -> {Tail, []};
						N -> {string:substr(Tail, 1, N - 1), string:substr(Tail, N + 1)}
					end,
					case Path of
						"/ws" -> morse_ws:do(ModData, Channel);
						"" -> index_html(ModData#mod.parsed_header);
						_ -> {break, [{response, {404, []}}]}
					end
			end
	end.

%% internal

index_html(Headers) ->
	Index = ets:lookup_element(config, index_html, 2),
	Etag = proplists:get_value("if-none-match", Headers, undefined),
	{ok, FileInfo} = file:read_file_info(Index, [{time, universal}]),
	Etag2 = httpd_util:create_etag(FileInfo),
	if
		Etag =:= Etag2 ->
			{break, [{response, {response, [
				{code, 304},
				{content_type, "text/html"},
				{"Etag", Etag2}
			], []}}]};
		true ->
			{ok, Data} = file:read_file(Index),
			{break, [{response, {response, [
				{code, 200},
				{content_type, "text/html"},
				{content_length, integer_to_list(size(Data))},
				{"Etag", Etag2}
			], [Data]}}]}
	end.
