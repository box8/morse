%% Copyright (c) 2015, Alexei Uskov <al@box8.ru>
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


-module(rfc6455).
-export([init/0, read/2, serialize/3]).

-record(state, {fin, opcode, mask, data, tail, prev}).

-spec init() -> State when State::#state{}.
init() ->
	#state{data = [], tail = 0, prev = false}.

-spec read(binary(), State) -> State when State::#state{}.
read(<<>>, State) -> State;
read(Data, State) when State#state.tail > 0 ->
	N = State#state.tail,
	case Data of
		<<Head:N/binary, Tail/binary>> ->
			State2 = State#state{data = [mask(Head, State#state.mask) | State#state.data], tail = 0},
			read(Tail, case State#state.fin of
				1 -> full_packet(State2);
				_ -> State2
			end);
		_ ->
			State#state{
				mask = mask_shift(State#state.mask, byte_size(Data) rem 4),
				data = [mask(Data, State#state.mask) | State#state.data],
				tail = State#state.tail - byte_size(Data)}
	end;
read(<<Fin:1, 0:3, Opcode:4, 0:1, 127:7, Length:64, Data/binary>>, State) ->
	read_frame(Fin, Opcode, Length, 0, Data, State);
read(<<Fin:1, 0:3, Opcode:4, 1:1, 127:7, Length:64, Mask:32, Data/binary>>, State) ->
	read_frame(Fin, Opcode, Length, Mask, Data, State);
read(<<Fin:1, 0:3, Opcode:4, 0:1, 126:7, Length:16, Data/binary>>, State) ->
	read_frame(Fin, Opcode, Length, 0, Data, State);
read(<<Fin:1, 0:3, Opcode:4, 1:1, 126:7, Length:16, Mask:32, Data/binary>>, State) ->
	read_frame(Fin, Opcode, Length, Mask, Data, State);
read(<<Fin:1, 0:3, Opcode:4, 0:1, Length:7, Data/binary>>, State) ->
	read_frame(Fin, Opcode, Length, 0, Data, State);
read(<<Fin:1, 0:3, Opcode:4, 1:1, Length:7, Mask:32, Data/binary>>, State) ->
	read_frame(Fin, Opcode, Length, Mask, Data, State).

-spec serialize(int, int, iodata()) -> iodata().
serialize(Opcode, Mask, Data) ->
	N = iolist_size(Data),
	L = if
		N < 126 -> <<N:7>>;
		N < 16#FFFF -> <<126:7, N:16>>;
		N < 16#FFFFFFFFFFFFFFFF -> <<127:7, N:64>>
	end,
	case Mask of
		0 -> [<<1:1, 0:3, Opcode:4, 0:1, L/bitstring>>, Data];
		_ -> [<<1:1, 0:3, Opcode:4, 1:1, L/bitstring, Mask:32>>, mask(Data, Mask)]
	end.

%% internal

-spec read_frame(int, int, int, int, binary(), State) -> State when State::#state{}.
read_frame(Fin, 0, Length, Mask, Data, State) when State#state.opcode < 8
		andalso State#state.data =/= [] andalso State#state.prev =:= false ->
	case Data of
		<<Head:Length/binary, Tail/binary>> ->
			State2 = State#state{data = [mask(Head, Mask) | State#state.data]},
			read(Tail, case Fin of
				1 -> full_packet(State2);
				_ -> State2
			end);
		_ ->
			State#state{
				fin = Fin,
				mask = mask_shift(Mask, byte_size(Data) rem 4),
				data = [mask(Data, Mask) | Data],
				tail = Length - byte_size(Data)}
	end;
read_frame(0, Opcode, Length, Mask, Data, State) when Opcode < 8 andalso State#state.prev =:= false ->
	Opcode2 = case Opcode of
		0 -> State#state.opcode;
		_ -> Opcode
	end,
	case Data of
		<<Head:Length/binary, Tail/binary>> ->
			read(Tail, State#state{opcode = Opcode2, data = [mask(Head, Mask)], tail = 0});
		_ ->
			State#state{
				fin = 0,
				opcode = Opcode2,
				mask = mask_shift(Mask, byte_size(Data) rem 4),
				data = [mask(Data, Mask)],
				tail = Length - byte_size(Data)}
	end;
read_frame(1, Opcode, Length, Mask, Data, State) when Opcode > 7 orelse State#state.prev =:= false ->
	Opcode2 = case Opcode of
		0 -> State#state.opcode;
		_ -> Opcode
	end,
	Prev = case State#state.data of
		[] -> false;
		_ -> State
	end,
	case Data of
		<<Head:Length/binary, Tail/binary>> ->
			read(Tail, full_packet(State#state{opcode = Opcode2, data = [mask(Head, Mask)], tail = 0, prev = Prev}));
		_ ->
			State#state{
				fin = 1,
				opcode = Opcode2,
				mask = mask_shift(Mask, byte_size(Data) rem 4),
				data = [mask(Data, Mask)],
				tail = Length - byte_size(Data),
				prev = Prev}
	end.

-spec full_packet(State) -> State when State::#state{}.
full_packet(#state{opcode = Opcode, data = Data, prev = Prev} = State) ->
	self() ! {rfc6455, Opcode, lists:reverse(Data)},
	case Prev of
		false -> State#state{data = []};
		_ -> Prev
	end.

-spec mask(iodata(), int) -> list().
mask(Data, 0) -> Data;
mask(Data, Mask) ->
	{_M, L} = mask_rev(Data, Mask, []),
	lists:reverse(L).

-spec mask_rev(iodata(), int, list()) -> {int, list()}.
mask_rev([], M, L) -> {M, L};
mask_rev([A | B], M, L) ->
	{M2, L2} = mask_rev(A, M, L),
	mask_rev(B, M2, L2);
mask_rev(<<>>, M, L) -> {M, L};
mask_rev(<<A:8>>, M, L) -> {mask_shift(M, 1), [<<(((A bsl 24) bxor M) bsr 24):8>> | L]};
mask_rev(<<A:16>>, M, L) -> {mask_shift(M, 2), [<<(((A bsl 16) bxor M) bsr 16):16>> | L]};
mask_rev(<<A:24>>, M, L) -> {mask_shift(M, 3), [<<(((A bsl 8) bxor M) bsr 8):24>> | L]};
mask_rev(<<A:32, B/binary>>, M, L) -> mask_rev(B, M, [<<(A bxor M):32>> | L]).

-spec mask_shift(int, int) -> int.
mask_shift(M, 0) -> M;
mask_shift(M, 1) -> ((M band 16#FFFFFF) bsl 8) + (M bsr 24);
mask_shift(M, 2) -> ((M band 16#FFFF) bsl 16) + (M bsr 16);
mask_shift(M, 3) -> ((M band 16#FF) bsl 24) + (M bsr 8).
