%%%-------------------------------------------------------------------
%%% @author Gustavo Feuerstein
%%% @copyright (C) 2019, Robot Genesis
%%% @doc
%%%
%%% @end
%%% Created : 22. mai 2019 15:33
%%%-------------------------------------------------------------------
-module(mappy).

%% API
-export([encode/1, decode/1, test/0]).

encode(Map) ->
  BinaryMap = encode_map(Map),
  jiffy:encode(BinaryMap, [pretty]).

decode(JSONText) ->
  RawMap = jiffy:decode(JSONText),
  clean_value(RawMap).

%%%===================================================================
%%% Internal functions
%%%===================================================================

keys_to_atom(PropList) ->
  keys_to_atom(PropList, []).
keys_to_atom([], Acc) -> Acc;
keys_to_atom([{Key, RawValue} |T], Acc) ->
  Atom = binary_to_atom(Key, utf8),
  Value = clean_value(RawValue),
  keys_to_atom(T, Acc ++ [{Atom, Value}]).

clean_value(RawValue) when is_bitstring(RawValue) ->
  binary_to_list(RawValue);
clean_value(RawValue) when is_list(RawValue) ->
  handle_list(RawValue);
clean_value(RawValue) when is_tuple(RawValue) ->
  handle_raw_map(RawValue);
clean_value(RawValue) ->
  RawValue.

handle_list(List) ->
  handle_list(List, []).
handle_list([], Acc) -> Acc;
handle_list([RawValue|T], Acc) ->
  Value = clean_value(RawValue),
  handle_list(T, Acc ++ [Value]).

handle_raw_map({PropList}) ->
  List = keys_to_atom(PropList),
  maps:from_list(List);
handle_raw_map(RawMap) ->
  RawMap.

%%--------------------------------------------------------------------

encode_map(Map) ->
  maps:map(fun map_binary/2, Map).

make_binary(Value) when is_map(Value) ->
  encode_map(Value);
make_binary(Value) when is_list(Value) ->
  make_binary(Value, []);
make_binary(Value) ->
  Value.

make_binary([], Acc) -> Acc;
make_binary([H|T], Acc) ->
  make_binary(T, Acc ++ [make_binary(H)]).

map_binary(_, Value) when is_list(Value) ->
  case io_lib:printable_unicode_list(Value) of
    true  -> list_to_binary(Value);
    false -> make_binary(Value)
  end;

map_binary(_, Value) when is_map(Value) ->
  encode_map(Value);

map_binary(_, Value) ->
  Value.

%%--------------------------------------------------------------------

test() ->
  application:ensure_all_started(jiffy),
  M = #{entry => [#{id => "367473100727583",messaging => [#{sender => #{id => "2078416445552294"},timestamp => 1557512681063}],time => 1557512681063}],object => "page"},
  J = encode(M),
  M2 = decode(J),
  M == M2.
