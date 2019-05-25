%%%-------------------------------------------------------------------
%%% @author Gustavo Feuerstein
%%% @copyright (C) 2019, Robot Genesis
%%% @doc
%%%
%%% @end
%%% Created : 17. mai 2019 20:31
%%%-------------------------------------------------------------------
-module(env).

%% API
-export([get_value/1]).
-export([get_number/1]).
-export([get_atom/1, get_atoms/1]).
-export([get_map/1, get_maps/1]).
-export([merge/1]).

%%%===================================================================
%%% API
%%%===================================================================

get_value(VarName) when is_atom(VarName) ->
  get_value(string:to_upper(atom_to_list(VarName)));

get_value(VarName) ->
  VarValue = os:getenv(VarName),
  if
    VarValue /= false -> VarValue;
    true -> error({env_not_set, VarName})
  end.

%%--------------------------------------------------------------------

get_number(VarName) ->
  VarValue = get_value(VarName),
  try
    list_to_float(VarValue)
  catch
    error:badarg ->
      try
        list_to_integer(VarValue)
      catch
        error:badarg -> error({env_not_a_number, VarName, VarValue})
      end
  end.

%%--------------------------------------------------------------------

get_atom(VarName) ->
  VarValue = get_value(VarName),
  list_to_atom(VarValue).

%%--------------------------------------------------------------------

get_atoms(VarName) ->
  VarValue = get_value(VarName),
  Values = string:split(VarValue, ",", all),
  TValues = [string:trim(Value) || Value <- Values],
  [list_to_atom(TValue) || TValue <- TValues].

%%--------------------------------------------------------------------

get_map(VarName) ->
  VarValue = get_value(VarName),
  mappy:decode(VarValue).

%%--------------------------------------------------------------------

get_maps(VarName) ->
  Names = get_atoms(VarName),
  Maps = [#{Name => get_map(Name)} || Name <- Names],
  merge(Maps).

%%%===================================================================

merge(Maps) ->
  merge(Maps, #{}).
merge([], Map) -> Map;
merge([H|T], Map) ->
  Map2 = maps:merge(H, Map),
  merge(T, Map2).

%%%===================================================================
%%% Internal functions
%%%===================================================================


