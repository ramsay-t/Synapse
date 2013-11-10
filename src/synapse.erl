-module(synapse).
-export([
	 get_traces/2,get_live_traces/2
	 ,learn/2,learn/3
	 ,passive_learn/1,passive_learn/2,passive_learn/3
	 ,diff/3,diff/4
	 ,supported_learners/0
	 ,visualise/1,visualise/3,visualise/4
	 ,visualise_diff/2,visualise_diff/4,visualise_diff/5
	 ,load_config/1
	]).

-export([trace_server/4,conf_server/1,get_config/1]).

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

%% @doc Learn from a set of traces, using the default learner backend.
-spec learn(
	Traces :: list(trace()),
	MetaInfo :: learner_metainfo()
		    ) -> statemachine().
learn(Traces,MetaInfo) ->
    learn(default_learner(),Traces,MetaInfo).

%% @doc Learn from a set of traces, using the specified learner backend.
-spec learn(
	Learner :: learner_backend(),
	Traces :: list(trace()),
	MetaInfo :: learner_metainfo()
		    ) -> statemachine().
learn(Learner,Traces,MetaInfo) ->
    run_learner_function(Learner,learn,[Traces,MetaInfo]).


%% @doc Learn from a set of traces, without queries, 
%% using the default learner backend and the default meta_info.
-spec passive_learn(
	Traces :: list(trace())
       ) -> statemachine().
passive_learn(Traces) ->
    passive_learn(default_learner(),Traces,[]).

%% @doc Learn from a set of traces, without queries, 
%% using the default learner backend.
-spec passive_learn(
	Traces :: list(trace()),
	MetaInfo :: learner_metainfo()
       ) -> statemachine().
passive_learn(Traces,MetaInfo) ->
    passive_learn(default_learner(),Traces,MetaInfo).

%% @doc Learn from a set of traces, without queries.
-spec passive_learn(
	Learner :: learner_backend(),
	Traces :: list(trace()),
	MetaInfo :: learner_metainfo()
       ) -> statemachine().
passive_learn(Learner,Traces,MetaInfo) ->
    run_learner_function(Learner,passive_learn,[Traces,MetaInfo]).

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

-spec visualise(
	SM :: statemachine()
	      ) -> ok.		       
visualise(SM) ->
    visualise(default_learner(), SM, [], 'FSM').

-spec visualise(
	SM :: statemachine(),
	MetaInfo :: learner_metainfo(),
	WindowName :: atom()
		      ) -> ok.		       
visualise(SM, MetaInfo, WindowName) ->
    visualise(default_learner(), SM, MetaInfo, WindowName).

-spec visualise(
	Learner :: learner_backend(),
	SM :: statemachine(),
	MetaInfo :: learner_metainfo(),
	WindowName :: atom()
		      ) -> ok.		       
visualise(Learner, SM, MetaInfo, WindowName) ->
    run_learner_function(Learner,visualise,[SM,MetaInfo,WindowName]).

-spec visualise_diff(
	Orig :: statemachine(),
	Diff :: statemachinedifference()
	      ) -> ok.		       
visualise_diff(Orig, Diff) ->
    visualise_diff(default_learner(),Orig, Diff,[],'Difference').

-spec visualise_diff(
	Orig :: statemachine(),
	Diff :: statemachinedifference(),
	MetaInfo :: learner_metainfo(),
	WindowName :: atom()
	      ) -> ok.		       
visualise_diff(Orig, Diff, MetaInfo, WindowName) ->
    visualise_diff(default_learner(), Orig, Diff, MetaInfo, WindowName).

-spec visualise_diff(
	Learner :: learner_backend(),
	Orig :: statemachine(),
	Diff :: statemachinedifference(),
	MetaInfo :: learner_metainfo(),
	WindowName :: atom()
	      ) -> ok.		       
visualise_diff(Learner, Orig, Diff, MetaInfo, WindowName) ->
    run_learner_function(Learner,visualise_diff,[Orig,Diff,MetaInfo,WindowName]).

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


conf_server(Conf) ->
    receive
	{From, get, Key} ->
	    case lists:keyfind(Key,1,Conf) of
		{Key, Value} ->
		    From ! {conf, Key, Value},
		    conf_server(Conf);
		false ->
		    From ! {conf, error, not_found},
		    conf_server(Conf);
		Other ->
		    From ! {conf, error, Other},
		    conf_server(Conf)
	    end;
	terminate ->
	    ok
    end.

get_config(Key) ->
    find_conf_server() ! {self(),get,Key},
    receive 
	{conf, Key, Value} ->
	    Value;
	{conf, error, Error} ->
	    {error, Error}
    end.

find_conf_server() ->
    case global:whereis_name(synapse_conf_server) of
	undefined ->
	    exit("You must load a Synapse config file with the load_config function.");
	PID ->
	    PID
    end.

load_config(File) ->
    case global:whereis_name(synapse_conf_server) of
	undefined ->
	    {ok, Conf} = file:consult(File),
	    PID = spawn_link(?MODULE,conf_server,[Conf]),
	    global:register_name(synapse_conf_server,PID);
	Server ->
	    Server ! terminate,
	    load_config(File)
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
