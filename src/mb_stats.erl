%%% @doc Statistics from benchmark results.
%%% @copyright 2023 Klarna Bank AB
-module(mb_stats).

%%% API
-export([ add_stats/1
        , add_histogram/1
        ]).

-include("testing.hrl").

-define(HISTOGRAM_GROUPS, 5).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

add_stats(#{times := [Time]} = Result) ->
  Result#{ sample_size => 1
         , expected_value => Time
         , std_deviation => 0.0
         , min => Time
         , max => Time
         , p25 => Time
         , p50 => Time
         , p75 => Time
         };
add_stats(#{times := Times} = Result) ->
  SampleSize = length(Times),
  ExpectedValue = lists:sum(Times) / SampleSize,
  Variance = lists:sum([(T - ExpectedValue) * (T - ExpectedValue) || T <- Times]) / (SampleSize - 1),
  StdDeviation = math:sqrt(Variance),
  SortedTimes = lists:sort(Times),
  Min = hd(SortedTimes),
  Max = lists:last(SortedTimes),
  Result#{ sample_size => SampleSize
         , expected_value => ExpectedValue
         , std_deviation => StdDeviation
         , min => Min
         , max => Max
         , p25 => percentile(0.25, SortedTimes, SampleSize)
         , p50 => percentile(0.50, SortedTimes, SampleSize)
         , p75 => percentile(0.75, SortedTimes, SampleSize)
         }.

add_histogram(Stats) when is_list(Stats) ->
  AllTimesSorted = lists:sort(lists:flatmap(fun (#{times := Times}) -> Times end, Stats)),
  SampleSize = length(AllTimesSorted),
  P25 = percentile(0.25, AllTimesSorted, SampleSize),
  P75 = percentile(0.75, AllTimesSorted, SampleSize),
  GroupSpan = group_span(P25, P75),
  [add_histogram(Stat, GroupSpan) || Stat <- Stats];
add_histogram(Stat = #{p25 := P25, p75 := P75}) ->
  add_histogram(Stat, group_span(P25, P75)).

%%%-----------------------------------------------------------------------------
%%% Helper functions
%%%-----------------------------------------------------------------------------

percentile(Percentile, Samples, N) ->
  Rank = Percentile * (N + 1),
  LoRank = max(floor(Rank), 1),
  HiRank = min(ceil(Rank), N),
  Lo = lists:nth(LoRank, Samples),
  Hi = lists:nth(HiRank, Samples),
  Lo + (Hi - Lo) * (Rank - LoRank).

group_span(Lo, Hi) ->
  fancy_round((Hi - Lo) / ?HISTOGRAM_GROUPS).

fancy_round(N) when is_float(N) ->
  fancy_round(ceil(N));
fancy_round(0) ->
  1;
fancy_round(N) when is_integer(N), N =< 10 ->
  N;
fancy_round(N) when is_integer(N) ->
  Log = floor(math:log10(N)),
  Unit = floor(math:pow(10.0, math:floor(Log))),
  HalfUnit = Unit div 2,
  Major = (N div Unit) * Unit,
  if
    Major =:= N -> Major;
    (N - Major) > HalfUnit -> Major + Unit;
    true -> Major + HalfUnit
  end.

add_histogram(Stat = #{times := Times}, GroupSpan) ->
  Histogram =
    lists:foldl(
      fun (Time, Histogram) ->
        Group = sample_to_group(Time, GroupSpan),
        maps:update_with(Group, fun (X) -> X + 1 end, 1, Histogram)
      end,
      #{},
      Times),
  Stat#{ group_span => GroupSpan
       , histogram => Histogram
       }.

sample_to_group(Sample, GroupSpan) ->
  Sample - Sample rem GroupSpan + GroupSpan.

%%%-----------------------------------------------------------------------------
%%% Unit tests
%%%-----------------------------------------------------------------------------
-ifdef(TEST).

-define(_assertCloseEnough(Expect, Expr), ?_assertMatch(Res when abs(Res - (Expect)) < 1.0e-8, Expr)).

percentile_test_() ->
  Samples = [1, 2, 3, 3, 3, 4, 4, 5, 5, 7],
  N = length(Samples),
  [ ?_assertCloseEnough(1.10, percentile(0.10, Samples, N))
  , ?_assertCloseEnough(2.75, percentile(0.25, Samples, N))
  , ?_assertCloseEnough(3.50, percentile(0.50, Samples, N))
  , ?_assertCloseEnough(5.00, percentile(0.75, Samples, N))
  , ?_assertCloseEnough(6.80, percentile(0.90, Samples, N))
  ].

fancy_round_test_() ->
  [ ?_assertEqual(7, fancy_round(7))
  , ?_assertEqual(10, fancy_round(10))
  , ?_assertEqual(15, fancy_round(11))
  , ?_assertEqual(15, fancy_round(14))
  , ?_assertEqual(15, fancy_round(15))
  , ?_assertEqual(20, fancy_round(16))
  , ?_assertEqual(20, fancy_round(19))
  , ?_assertEqual(20, fancy_round(20))
  , ?_assertEqual(25, fancy_round(21))
  , ?_assertEqual(65, fancy_round(62))
  , ?_assertEqual(150000, fancy_round(137248))
  ].

-endif.
