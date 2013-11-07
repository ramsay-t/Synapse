-module(synapse_statechum).

-export([learn/2,diff/3,visualise/2,visualise_diff/2]).

-include("synapse.hrl").

-spec learn(
	Traces :: list(trace()),
	MetaInfo :: learner_metainfo()
		    ) -> statemachine().
learn(Traces, MetaInfo) ->
    StateChum = find_statechum(),
    StateChum ! {learn, Traces, MetaInfo, self()},
    display_progress().

diff(SM1, SM2, MetaInfo) ->
    StateChum = find_statechum(),
    StateChum ! {diff, SM1, SM2, MetaInfo, self()},
    display_progress().

visualise(SM, MetaInfo) ->
    StateChum = find_statechum(),
    StateChum ! {visualise, SM, MetaInfo, self()},
    display_progress().
 
visualise_diff(Diff, MetaInfo) ->
    StateChum = find_statechum(),
    StateChum ! {visualise_diff, Diff, MetaInfo, self()},
    display_progress().

display_progress() ->
    receive
	{status, Msg} ->
	    io:format("Progress: ~p~n", [Msg]),
	    display_progress();
	{complete, Result} ->
	    Result;
	Other ->
	    io:format("Unexpected message: ~p~n",[Other])
    end.

find_statechum() ->
    case synapselauncher:find_statechum() of
	not_started ->
	    {ok,StateChumOpts} = file:consult("statechum.conf"),
	    synapselauncher:startStatechum(StateChumOpts);
	PID ->
	    PID
    end.

	    


				
