-module(synapse_sm).

-include("synapse.hrl").

-export([walk/3,walk/2,sanity_check/1,get_states/1,get_alphabet/1]).

%% @doc Walk a trace on a state machine.
%% Begins at the initial state of the state machine and return the 
%% final state of the machine, or report the first point of failure. 
-spec walk(
	SM :: statemachine(),
	Trace :: trace()
		 ) -> state().
walk(SM,Trace) ->
    walk(SM,SM#statemachine.initial_state,Trace).

%% @doc Walk a trace on a state machine, starting from the specified state.
-spec walk(
	SM :: statemachine(),
	Start :: state(),
	Trace :: trace()
		 ) -> state().
walk(_SM,Start,[]) ->
    {ok, Start};
walk(SM,Start,[E | Trace]) ->
    case find_transition(SM,Start,E) of
	not_found ->
	    {failed_at,[E]};
	{Start,E,Next} ->
	    case walk(SM,Next,Trace) of
		{failed_at,F} ->
		    {failed_at,[E | F]};
		{ok, End} ->
		    {ok, End}
	    end
    end.

find_transition(#statemachine{transitions=Trans},Start,Event) ->
    find_transition(Trans,Start,Event);
find_transition([],_Start,_Event) ->
    not_found;
find_transition([{Start,Event,Next} | _T], Start, Event) ->
    {Start,Event,Next};
find_transition([_ | T], Start, Event) ->
    find_transition(T,Start,Event).

%% @doc Applies various sanity checks to a state machine
%% Checks that the alphabet is consistent with the transitions, 
%% and that the list of states is a superset of the states used in
%% transitions.
-spec sanity_check(
	SM :: statemachine()
	      ) -> boolean().
sanity_check(#statemachine{
		states=States,
		transitions=Trans,
		initial_state=IS,
		alphabet=Alphabet
	       }) ->
    try
	SubStates = get_states(Trans),
	lists:map(fun(S) -> 
			  [S] = lists:filter(fun(SS) -> 
						     SS =:= S 
					     end,
					     States) 
		  end, 
		  SubStates),
	io:format("~p vs ~p~n",[Alphabet,get_alphabet(Trans)]),
	Alphabet = get_alphabet(Trans),
	[IS] = lists:filter(fun(E) -> E =:= IS end, States),
	true
    catch error:{badmatch,V} ->
	    false
    end.
	    
%% @doc Get states from a list of transitions
-spec get_states(Trans :: list(transition()) ) -> list(state()).
get_states([]) ->
    [];
get_states([{S,_,E} | Ts]) ->
    lists:usort([S, E | get_states(Ts)]).

%% @doc Get the alphabet from a list of transitions
-spec get_alphabet(Trans :: list(transition())) -> list(event()).
get_alphabet([]) ->
    [];
get_alphabet([{_,E,_} | Ts]) ->
    lists:usort([E | get_alphabet(Ts)]).
