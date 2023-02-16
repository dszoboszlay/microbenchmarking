%%% @doc Code snippets to be benchmarked.
%%% @copyright 2023 Klarna Bank AB
-module(mb_tests).

%%% API
-export([ scenario/1
        , empty/1
        ]).

%%% Code snippets to be benchmarked
-export([ merge_sort/1
        , bubble_sort/1
        ]).

-include("testing.hrl").

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

scenario(sorting) ->
  #{ args => [[rand:uniform(90) || _ <- lists:seq(1, 1000)]]
   , snippets => #{ <<"lists:sort/1">> => fun lists:sort/1
                  , <<"merge sort">> => fun merge_sort/1
                  , <<"bubble sort">> => fun bubble_sort/1
                  , <<"empty">> => fun empty/1
                  }
   }.

%% @doc The empty test (does nothing). Useful for measuring the
%% test runner's overhead.
empty(X) -> X.

%%%-----------------------------------------------------------------------------
%%% Code snippets to be benchmarked
%%%-----------------------------------------------------------------------------

%% @doc A merge-sort algorithm to benchmark.
merge_sort([]) ->
  [];
merge_sort([X | L]) ->
  element(1, merge_sort([X], L, 1, infinity)).

merge_sort(Sorted, [], _, _) ->
  {Sorted, []};
merge_sort(Sorted, L, Length, Length) ->
  {Sorted, L};
merge_sort(SortedA, [X | L], CurrentLength, NeededLength) when CurrentLength < NeededLength ->
  {SortedB, Rest} = merge_sort([X], L, 1, CurrentLength),
  merge_sort(lists:merge(SortedA, SortedB), Rest, CurrentLength bsl 1, NeededLength).

%% @doc A bubble-sort algorithm to benchmark.
%% It's a rather inefficient way to sort a list.
bubble_sort(L) ->
  case lists:foldr(fun bubble_sort/2, {true, []}, L) of
    {true, SortedL} -> SortedL;
    {false, PartiallySortedL} -> bubble_sort(PartiallySortedL)
  end.

bubble_sort(A, {_, [B | L]}) when A > B ->
  {false, [B, A | L]};
bubble_sort(A, {Sorted, L}) ->
  {Sorted, [A | L]}.

%%%-----------------------------------------------------------------------------
%%% Unit tests
%%%-----------------------------------------------------------------------------
-ifdef(TEST).

bubble_sort_test_() ->
  [ ?_assertEqual("", bubble_sort(""))
  , ?_assertEqual("x", bubble_sort("x"))
  , ?_assertEqual("abc", bubble_sort("abc"))
  , ?_assertEqual("abc", bubble_sort("acb"))
  , ?_assertEqual("abc", bubble_sort("bac"))
  , ?_assertEqual("abc", bubble_sort("bca"))
  , ?_assertEqual("abc", bubble_sort("cab"))
  , ?_assertEqual("abc", bubble_sort("cba"))
  , ?_assertEqual("aaaaaaaccccddddddefffhhhjjjjkkkklllnssssssvw",
                  bubble_sort("asdfafjalkdhvcascdjkhwesadncjklsdcjasklfhasd"))
  ].

merge_sort_test_() ->
  [ ?_assertEqual("", merge_sort(""))
  , ?_assertEqual("x", merge_sort("x"))
  , ?_assertEqual("abc", merge_sort("abc"))
  , ?_assertEqual("abc", merge_sort("acb"))
  , ?_assertEqual("abc", merge_sort("bac"))
  , ?_assertEqual("abc", merge_sort("bca"))
  , ?_assertEqual("abc", merge_sort("cab"))
  , ?_assertEqual("abc", merge_sort("cba"))
  , ?_assertEqual("aaaaaaaccccddddddefffhhhjjjjkkkklllnssssssvw",
                  merge_sort("asdfafjalkdhvcascdjkhwesadncjklsdcjasklfhasd"))
  ].

-endif.
