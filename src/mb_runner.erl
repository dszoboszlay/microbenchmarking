%%% @doc Test runner implementations.
%%% @copyright 2023 Klarna Bank AB
-module(mb_runner).

%%% API
-export([ benchmark/4
        , estimate/2
        ]).

%%% NIF-related
-export([ init/0
        ]).

-on_load(init/0).

-define(WARMUP_LOOPS, 50).
-define(ESTIMATE_LOOPS, 500).
-define(REDUCTIONS_PER_SCHEDULE, 4000).
-define(INNER_LOOP_REDUCTION_OVERHEAD, 2).   % Updating the counter & recursion
-define(OUTER_LOOP_REDUCTION_OVERHEAD, 100). % This is just an ad-hoc, but safe estimate
-define(LOOPING_MEMORY_OVERHEAD, 100).       % This is just an ad-hoc, but safe estimate
-define(DEFAULT_CONFIG, #{ loops => {ms, 200}
                         , yield => false
                         , warmup => false
                         , elevate_priority => false
                         , pre_allocate => false
                         , scheduler => 0
                         }).

-compile({inline, [ warmup/3
                  , benchmark/5
                  , erlang_yield/0
                  , double_yield/0
                  ]}).
-compile(nowarn_nif_inline).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

benchmark(Fun, Args, Config,
          #{ inner_loops := InnerLoops
           , mean_time := MeanTime
           , memory := Memory
           }) ->
  %% Ensure we have a full configuration
  FullConfig = maps:merge(?DEFAULT_CONFIG, Config),
  #{ loops := Loops                      % {count, pos_integer()} | {ms, pos_integer()}
   , yield := Yield                      % false | erlang | os | both
   , warmup := Warmup                    % boolean()
   , elevate_priority := ElevatePriority % boolean()
   , pre_allocate := PreAllocate         % boolean()
   , scheduler := Scheduler              % non_neg_integer()
   } = FullConfig,

  %% Estimate number of loops and memory requirement
  EstimatedLoops = test_loops(Loops, MeanTime) + warmup_loops(Warmup),
  EstimatedMemory = EstimatedLoops * (Memory + 1) + ?LOOPING_MEMORY_OVERHEAD,

  %% Determine parameters of the function
  SpawnOpts = [ monitor
              , {priority, priority(ElevatePriority)}
              , {min_heap_size, heap_size(PreAllocate, EstimatedMemory)}
              ],

  %% Do the measurement from a dedicated worker process
  WorkerFun =
    fun () ->
      process_flag(scheduler, Scheduler),
      warmup(Fun, Args, Warmup),
      Result = benchmark(Loops, Yield, InnerLoops, Fun, Args),
      exit({ok, Result})
    end,
  {Pid, Mon} = spawn_opt(WorkerFun, SpawnOpts),
  receive
    {'DOWN', Mon, process, Pid, Info} ->
      {ok, Result} = Info,
      maps:merge(FullConfig, Result)
  end.

estimate(Fun, Args) ->
  Self = self(),
  %% Measure number of reductions and determine inner loop length
  R0 = process_info(Self, reductions),
  counted_inner_loop(Fun, Args, [], 1),
  R1 = process_info(Self, reductions),
  ReductionsPerLoop = element(2, R1) - element(2, R0) + ?INNER_LOOP_REDUCTION_OVERHEAD,

  %% Measure average execution time
  T0 = erlang:monotonic_time(),
  warmup_loop(Fun, Args, ?ESTIMATE_LOOPS),
  T1 = erlang:monotonic_time(),

  %% Estimates are ready
  AvailableReductionsPerLoop = ?REDUCTIONS_PER_SCHEDULE - ?OUTER_LOOP_REDUCTION_OVERHEAD,
  #{ inner_loops => max(AvailableReductionsPerLoop div ReductionsPerLoop, 1)
   , mean_time => (T1 - T0) div ?ESTIMATE_LOOPS
   , memory => get_heap_size(Fun, Args)
   }.

%%%-----------------------------------------------------------------------------
%%% NIF-related
%%%-----------------------------------------------------------------------------

init() ->
  %% FIXME: make this work reliably from an escript
  erlang:load_nif("priv/mb_yield", 0).

%%%-----------------------------------------------------------------------------
%%% Helper functions
%%%-----------------------------------------------------------------------------

benchmark({count, Cnt}, false, _InnerLoops, Fun, Args) ->
  Ts = counted_inner_loop(Fun, Args, [erlang:monotonic_time()], Cnt),
  GCs = gcs(),
  #{ times => timestamps_to_durations(Ts)
   , gcs => GCs
   };
benchmark({count, Cnt}, Yield, InnerLoops, Fun, Args) ->
  OuterLoops = (Cnt + (InnerLoops - 1)) div InnerLoops,
  case Yield of
    erlang -> counted_outer_loop_erlang_yield(Fun, Args, [], OuterLoops, InnerLoops);
    os ->  counted_outer_loop_os_yield(Fun, Args, [], OuterLoops, InnerLoops);
    both ->  counted_outer_loop_double_yield(Fun, Args, [], OuterLoops, InnerLoops)
  end;
benchmark({ms, Duration}, false, _InnerLoops, Fun, Args) ->
  DeltaT = erlang:convert_time_unit(Duration, millisecond, native),
  T0 = erlang:monotonic_time(),
  Ts = timed_inner_loop(Fun, Args, [T0], T0 + DeltaT),
  GCs = gcs(),
  #{ times => timestamps_to_durations(Ts)
   , gcs => GCs
   };
benchmark({ms, Duration}, Yield, InnerLoops, Fun, Args) ->
  DeltaT = erlang:convert_time_unit(Duration, millisecond, native),
  EndT = erlang:monotonic_time() + DeltaT,
  case Yield of
    erlang -> timed_outer_loop_erlang_yield(Fun, Args, [], EndT, InnerLoops);
    os ->  timed_outer_loop_os_yield(Fun, Args, [], EndT, InnerLoops);
    both ->  timed_outer_loop_double_yield(Fun, Args, [], EndT, InnerLoops)
  end.

-define(gen_outer_loop(Name, Yield, LoopTest, NewLoopArg),
Name(Fun, Args, Durations, LoopArg, InnerLoops) ->
  (Yield),
  Ds = counted_inner_loop(Fun, Args, [erlang:monotonic_time()], InnerLoops),
  case LoopTest of
    true ->
      ?FUNCTION_NAME(Fun, Args, [Ds | Durations], NewLoopArg, InnerLoops);
    false ->
      GCs = gcs(),
      #{ times => lists:foldl(fun timestamps_to_durations/2, [], [Ds | Durations])
       , gcs => GCs
       }
  end
).

-define(counted_outer_loop(Name, Yield), ?gen_outer_loop(Name, Yield, LoopArg > 1, LoopArg - 1)).
?counted_outer_loop(counted_outer_loop_erlang_yield, erlang_yield()).
?counted_outer_loop(counted_outer_loop_os_yield, os_yield()).
?counted_outer_loop(counted_outer_loop_double_yield, double_yield()).

-define(timed_outer_loop(Name, Yield), ?gen_outer_loop(Name, Yield, hd(Ds) < LoopArg, LoopArg)).
?timed_outer_loop(timed_outer_loop_erlang_yield, erlang_yield()).
?timed_outer_loop(timed_outer_loop_os_yield, os_yield()).
?timed_outer_loop(timed_outer_loop_double_yield, double_yield()).

counted_inner_loop(Fun, Args, Ts, Cnt) ->
  apply(Fun, Args),
  T = erlang:monotonic_time(),
  if
    Cnt > 1 -> counted_inner_loop(Fun, Args, [T | Ts], Cnt - 1);
    true -> [T | Ts]
  end.

timed_inner_loop(Fun, Args, Ts, EndTime) ->
  apply(Fun, Args),
  T = erlang:monotonic_time(),
  if
    T < EndTime -> timed_inner_loop(Fun, Args, [T | Ts], EndTime);
    true -> [T | Ts]
  end.

warmup(Fun, Args, true) -> warmup_loop(Fun, Args, ?WARMUP_LOOPS);
warmup(_, _, false) -> ok.

warmup_loop(Fun, Args, Cnt) ->
  apply(Fun, Args),
  if
    Cnt > 1 -> warmup_loop(Fun, Args, Cnt - 1);
    true -> ok
  end.

gcs() ->
  {garbage_collection, GC} = process_info(self(), garbage_collection),
  proplists:get_value(minor_gcs, GC).

get_heap_size(Fun, Args) ->
  {min_heap_size, HeapSize} = erlang:system_info(min_heap_size),
  get_heap_size(Fun, Args, HeapSize).

get_heap_size(Fun, Args, TestHeapSize) ->
  Opts = [ monitor
         , {min_heap_size, TestHeapSize}
         ],
  {Pid, Mon} = spawn_opt(fun () -> heap_size_worker(Fun, Args) end, Opts),
  receive
    {'DOWN', Mon, process, Pid, {ok, HeapSize}} ->
      HeapSize;
    {'DOWN', Mon, process, Pid, Result} ->
      {retry, HeapSize} = Result,
      get_heap_size(Fun, Args, HeapSize + 1)
  end.

heap_size_worker(Fun, Args) ->
  apply(Fun, Args),
  [{heap_size, HeapSize}, {garbage_collection, GC}] =
    process_info(self(), [heap_size, garbage_collection]),
  case proplists:get_value(minor_gcs, GC) of
    0 -> exit({ok, HeapSize});
    _ -> exit({retry, HeapSize})
  end.

timestamps_to_durations(Ts) ->
  timestamps_to_durations(Ts, []).

timestamps_to_durations([Tn | Ts], Times) ->
  element(2, lists:foldl(fun (T0, {T1, Acc}) -> {T0, [T1 - T0 | Acc]} end, {Tn, Times}, Ts)).

test_loops({count, Loops}, _MeanTime) ->
  Loops;
test_loops({ms, Duration}, MeanTime) ->
  erlang:convert_time_unit(Duration, millisecond, native) div MeanTime.

warmup_loops(false) -> 0;
warmup_loops(true) -> ?WARMUP_LOOPS.

priority(true) -> high;
priority(false) -> normal.

heap_size(false, _Memory) ->
  {min_heap_size, HeapSize} = erlang:system_info(min_heap_size),
  HeapSize;
heap_size(true, Memory) ->
  Memory.

erlang_yield() -> erlang:yield().

os_yield() -> erlang:nif_error(?FUNCTION_NAME).

double_yield() -> os_yield(), erlang_yield().
