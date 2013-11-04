-module(synapse).
-export([get_traces/2,get_live_traces/2,learn/3,learn/2,diff/3,supported_learners/0]).

-export([trace_server/4]).

-include("synapse.hrl").

%% @doc Get events from an event source and split into traces, using the supplied TraceEnd function.
-spec get_traces(
	EventSource :: fun(() -> event()), 
	TraceEnd :: fun((event()) -> false | include | exclude)
		    ) -> list(trace()).
get_traces(_EventSource, _TraceEnd) ->
    %% FIXME content
    [].

%% @doc Register a trace receiver server that will direct new {trace, Trace} messages to the calling process as traces are detected.
%% This is used for tracing continous systems, such as network traffic. The trace server is spawned with the specified event source function and trace end
%% identifier function. It collates events until TraceEnd identifies the end of a trace, then the calling process is informed and it continues
%% monitoring the event source.
-spec get_live_traces(
	EventSource :: fun(() -> event()), 
	TraceEnd :: fun((event()) -> false | include | exclude)
		    ) -> ok | {error, string()}.
get_live_traces(_EventSource,_TraceEnd) ->
    %% FIXME content
    ok.

%% @doc Learn from a set of traces, using the specified learner backend.
-spec learn(
	Learner :: learner_backend(),
	Traces :: list(trace()),
	MetaInfo :: learner_metainfo()
		    ) -> statemachine().
learn(Learner,Traces,MetaInfo) ->
    run_learner_function(Learner,learn,[Traces,MetaInfo]).

%% @doc Learn from a set of traces, using the default learner backend.
-spec learn(
	Traces :: list(trace()),
	MetaInfo :: learner_metainfo()
		    ) -> statemachine().
learn(Traces,MetaInfo) ->
    learn(default_learner(),Traces,MetaInfo).

%% @doc Determine the difference between two state machines.
-spec diff(
	SM1 :: statemachine(),
	SM2 :: statemachine(),
	MetaInfo :: learner_metainfo()
		  ) -> statemachinedifference().
diff(SM1, SM2, MetaInfo) ->
    diff(default_learner(),SM1,SM2,MetaInfo).

-spec diff(
	Learner :: learner_backend(),
	SM1 :: statemachine(),
	SM2 :: statemachine(),
	MetaInfo :: learner_metainfo()
		    ) -> statemachinedifference().
diff(Learner, SM1, SM2, MetaInfo) ->
    run_learner_function(Learner,diff,[SM1,SM2,MetaInfo]).

%% Internal functions and server functions
%% @private
%% @doc Loops around, running the event source function and either extending the current trace, or reporting and reseting the trace. 
-spec trace_server(
	EventSource :: fun(() -> event()), 
	TraceEnd :: fun((event()) -> false | include | exclude),
	Receiver :: pid(),
	CurrentTrace :: list(event())
		    ) -> ok | {error, string()}.
trace_server(EventSource, TraceEnd, Receiver, CurrentTrace) ->
    Event = apply(EventSource,[]),
    case apply(TraceEnd, [Event]) of
	exclude ->
	    Receiver ! {trace, lists:reverse(CurrentTrace)},
	    trace_server(EventSource, TraceEnd, Receiver, []);
	include ->
	    Receiver ! {trace, lists:reverse([Event | CurrentTrace])},
	    trace_server(EventSource, TraceEnd, Receiver, []);
	_ ->
	    trace_server(EventSource, TraceEnd, Receiver, [Event | CurrentTrace])
    end.



supported_learners() ->
    [statechum].

is_supported(L) ->
    is_supported(L,supported_learners()).

is_supported(_L,[]) ->
    false;
is_supported(L,[L | _]) ->
    true;
is_supported(L,[_ | Ls]) ->
    is_supported(L,Ls).

default_learner() ->
    statechum.

run_learner_function(Learner,Function,Args) ->
    case is_supported(Learner) of
	false ->
	    {error,"Learner not supported",Learner,{"Supported Learners",supported_learners()}};
	_ ->
	    erlang:apply(list_to_atom("synapse_" ++ atom_to_list(Learner)),Function,Args)
    end.
