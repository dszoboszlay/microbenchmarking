%%% @doc Escript entry point
%%% @copyright 2023 Klarna Bank AB
-module(microbenchmarking).

%%% API
-export([ main/1
        ]).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

main([]) ->
  main(["default"]);
main([Prefix]) ->
  io:format("= ~s~n", [Prefix]),
  OverallF = open_csv(filename:join(Prefix, "overall.csv")),
  write_overall_csv(OverallF,
                    #{ prefix => prefix
                     , scenario => scenario
                     , snippet => snippet
                     , loops => {loops, loops}
                     , yield => yield
                     , warmup => warmup
                     , elevate_priority => elevate_priority
                     , pre_allocate => pre_allocate
                     , scheduler => scheduler
                     , options => options
                     , gcs => gcs
                     , sample_size => sample_size
                     , expected_value => expected_value
                     , std_deviation => std_deviation
                     , min => min
                     , max => max
                     , p25 => p25
                     , p50 => p50
                     , p75 => p75
                     }),
  [ benchmark_snippet(#{ prefix => Prefix
                       , scenario => ScenarioName
                       , snippet => Snippet
                       },
                       OverallF,
                       Fun,
                       Args)
   || ScenarioName <- [sorting],
      #{args := Args,  snippets := Snippets} <- [mb_tests:scenario(ScenarioName)],
      {Snippet, Fun} <- maps:to_list(Snippets)
  ],
  file:close(OverallF).

%%%-----------------------------------------------------------------------------
%%% Helper functions
%%%-----------------------------------------------------------------------------

benchmark_snippet(BaseConfig = #{snippet := Snippet}, OverallF, Fun, Args) ->
  io:format("== ~s~n", [Snippet]),
  Estimate = mb_runner:estimate(Fun, Args),
  Stats = [ begin
              Options = options_name(Loops, Yield, Warmup, ElevatePriority, PreAllocate, Scheduler),
              Config = BaseConfig#{ loops => Loops
                                  , yield => Yield
                                  , warmup => Warmup
                                  , elevate_priority => ElevatePriority
                                  , pre_allocate => PreAllocate
                                  , scheduler => Scheduler
                                  , options => Options
                                  },
              io:put_chars("."),
              Result = mb_stats:add_stats(mb_runner:benchmark(Fun, Args, Config, Estimate)),
              write_overall_csv(OverallF, Result),
              Result
            end
          || Loops <- loops_for_snippet(Snippet),
             Yield <- [false, erlang, os, both],
             Warmup <- [false, true],
             ElevatePriority <- [false, true],
             PreAllocate <- [false, true],
             Scheduler <- lists:seq(0, erlang:system_info(schedulers))
          ],
  lists:map(fun write_histogram_csv/1, mb_stats:add_histogram(Stats)),
  io:format("~n", []).

options_name(Loops, Yield, Warmup, ElevatePriority, PreAllocate, Scheduler) ->
  Options = [ loops_name(Loops)
            , yield_name(Yield)
            , Warmup andalso "warmup"
            , ElevatePriority andalso "high priority"
            , PreAllocate andalso "no gc"
            , scheduler_name(Scheduler)
            ],
  string:join([Option || Option <- Options, Option =/= false], "+").

write_overall_csv(FD,
                  #{ prefix := Prefix
                   , scenario := Scenario
                   , snippet := Snippet
                   , loops := {Loops, _}
                   , yield := Yield
                   , warmup := Warmup
                   , elevate_priority := ElevatePriority
                   , pre_allocate := PreAllocate
                   , scheduler := Scheduler
                   , options := Options
                   , gcs := GCs
                   , sample_size := SampleSize
                   , expected_value := ExpectedValue
                   , std_deviation := StdDeviation
                   , min := Min
                   , max := Max
                   , p25 := P25
                   , p50 := P50
                   , p75 := P75
                   }) ->
  io:format(FD,
            "~s ~s ~s ~s,~s,~s,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w~n",
            [ Prefix, Scenario, Snippet, Options
            , Prefix
            , Scenario
            , Snippet
            , Yield
            , Warmup
            , ElevatePriority
            , PreAllocate
            , Scheduler
            , SampleSize
            , Loops
            , GCs
            , ExpectedValue
            , StdDeviation
            , Min
            , Max
            , P25
            , P50
            , P75
            ]).

write_histogram_csv(#{ prefix := Prefix
                     , histogram := Histogram
                     } = Run) ->
  FD = open_csv(filename:join(Prefix, csv_name(Run))),
  io:format(FD, "time,count~n", []),
  [io:format(FD, "~w,~w~n", [Time, Count])
   || {Time, Count} <- lists:sort(maps:to_list(Histogram))
  ],
  file:close(FD).

open_csv(FileName) ->
  ok = filelib:ensure_dir(FileName),
  {ok, FD} = file:open(FileName, [write, {encoding, utf8}]),
  FD.

loops_for_snippet(<<"bubble sort">>) -> [{count, 100}, {ms, 200}];
loops_for_snippet(<<"empty">>) -> [{count, 2000}];
loops_for_snippet(_) -> [{count, 2000}, {ms, 200}].

loops_name({count, _}) -> "counted";
loops_name({ms, _}) -> "timed".

yield_name(false) -> false;
yield_name(erlang) -> "erlang:yield/1";
yield_name(os) -> "sched_yield()";
yield_name(both) -> "dual yield".

scheduler_name(0) -> false;
scheduler_name(N) -> "scheduler " ++ integer_to_list(N).

csv_name(#{ scenario := Scenario
          , snippet := Snippet
          , options := Options
          }) ->
  UnsafeName = lists:flatten(io_lib:format("~s_~s_~s.csv", [Scenario, Snippet, Options])),
  lists:filtermap(fun ($\s) -> {true, $-};
                      ($:)  -> {true, $-};
                      ($/)  -> {true, $-};
                      ($()  -> false;
                      ($))  -> false;
                      (C)   -> {true, C}
                  end,
                  UnsafeName).
