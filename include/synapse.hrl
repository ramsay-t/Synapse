%% @doc Currently, events can be any Erlang element.
-type event() :: atom().

%% @doc Currently, only atoms are supported as sttae names.
-type state() :: atom().

%% @doc A trace is simply a list of events.
-type trace() :: list(event()).

%% @doc A transition maps from a start state and an event label to an end state.
%% This may be subject to change if Synapse moves to EFSM models
-type transition() :: {state(),event(),state()}.

%% @doc The possible learner backends.
%% Currently only statechum is supported.
-type learner_backend() :: statechum.

%% @doc The state machine representation.
%% This is a conventional FSM representation.
-record(statemachine,{
	  states :: list(state()),
	  transitions :: list(transition()),
	  initial_state :: state(),
	  alphabet :: list(event())
	 }).

%% @doc The difference between two state machines.
%% This is in diff form, so the minumum set of changes to transform the first FSM into the second.
%% The name_mapping_1 and name_mapping_2 elements are lists of pairs that map from state names in the 
%% first FSM to state names in the diff, and from state names in the second FSM to state names in the diff
%% respectively.
-record(statemachinedifference,{
	  added_transitions :: list(transition()),
	  deleted_transitions :: list(transition()),
	  added_states :: list(state()),
	  deleted_states :: list(state()),
	  name_mapping_1 :: list({state(),state()}),
	  name_mapping_2 :: list({state(),state()})
	 }).

%% @doc Type synonym for the state machine record.
-type statemachine() :: #statemachine{}.
%% @doc Type synonym for the state machine difference record.
-type statemachinedifference() :: #statemachinedifference{}.

%% @doc Learner metainfo consists of key,value pairs.
-type learner_metainfo() :: list({atom(), any()}).
