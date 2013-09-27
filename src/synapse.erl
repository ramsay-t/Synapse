-module(synapse).
-export([get_traces/2,get_live_traces/2,learn/2,diff/2]).

-export([trace_server/4]).

-include("synapse.hrl").

%% @doc Get events from an event source and split into traces, using the supplied TraceEnd function.
-spec get_traces(
	EventSource :: fun(() -> event()), 
	TraceEnd :: fun((event()) -> false | include | exclude)
		    ) -> list(trace()).
get_traces(EventSource, TraceEnd) ->
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
get_live_traces(EventSource,TraceEnd) ->
    %% FIXME content
    ok.

%% @doc Learn from a set of traces, using the specified learner backend.
%% Currently only statechum is supported as a backend.
-spec learn(
	Learner :: learner_backend(),
	MetaInfo :: learner_metainfo()
		    ) -> statemachine().
learn(Learner,MetaInfo) ->
    %% FIXME content
    #statemachine{
       states = [],
       transitions = [],
       initial_state = 1,
       alphabet = []
      }.

%% @doc Determine the difference between two state machines.
-spec diff(
	First :: statemachine(),
	Second :: statemachine()
		  ) -> statemachinedifference().
diff(First, Second) ->
    %% FIXME content
    #statemachinedifference{
       	  added_transitions = [],
	  deleted_transitions = [],
	  added_states = [],
	  deleted_states = [],
	  name_mapping_1 = [],
	  name_mapping_2 = []
      }.
		   


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
