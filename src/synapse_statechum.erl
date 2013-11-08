-module(synapse_statechum).

-export([learn/2,diff/3,visualise/3,visualise_diff/4]).

-include("synapse.hrl").

-spec learn(
	Traces :: list(trace()),
	MetaInfo :: learner_metainfo()
		    ) -> statemachine().
learn(Traces, MetaInfo) ->
    {Ref, StateChum} = get_worker(),
    StateChum ! {Ref,updateConfiguration,MetaInfo},
    ok = display_progress(Ref),
    StateChum ! {Ref,traces,Traces},
    ok = display_progress(Ref),
    StateChum ! {Ref,learn,self()},
    get_final_progress(StateChum,Ref).

diff(SM1, SM2, MetaInfo) ->
    {Ref, StateChum} = get_worker(),
    StateChum ! {Ref,updateConfiguration,MetaInfo},
    ok = display_progress(Ref),
    StateChum ! {Ref, computeDiff, SM1, SM2},
    get_final_progress(StateChum,Ref).

visualise(SM, MetaInfo, Name) ->
    {Ref, StateChum} = get_worker(),
    StateChum ! {Ref,updateConfiguration,MetaInfo},
    ok = display_progress(Ref),
    StateChum ! {Ref, displayFSM, SM, Name},
    get_final_progress(StateChum,Ref).
 
visualise_diff(Orig, Diff, MetaInfo, Name) ->
    {Ref, StateChum} = get_worker(),
    StateChum ! {Ref,updateConfiguration,MetaInfo},
    ok = display_progress(Ref),
    StateChum ! {Ref,displayDiff,Orig,Diff,Name},
    get_final_progress(StateChum,Ref).

get_final_progress(StateChum, Ref) ->
    Result = display_progress(Ref),
    StateChum ! {Ref,terminate},
    Result.

display_progress(Ref) ->
    receive
	{Ref, status, Msg} ->
	    io:format("Progress: ~p~n", [Msg]),
	    display_progress(Ref);
	{Ref, ok, Result} ->
	    Result;
	{Ref, Result} ->
	    Result;
	Other ->
	    io:format("Unexpected message: ~p~n",[Other])
    end.

find_statechum() ->
    OFile = "../statechum.conf",
    case erlang:module_loaded(synapselauncher) of
	true ->
	    case synapselauncher:find_statechum() of
		not_started ->
		    {ok,StateChumOpts} = file:consult(OFile),
		    synapselauncher:startStatechum(StateChumOpts);
		PID ->
		    PID
	    end;
	_ ->
	    {ok,StateChumOpts} = file:consult(OFile),
	    case lists:keyfind('StatechumDir', 1, StateChumOpts) of
		{'StatechumDir', SCDir} ->
		    code:add_path(atom_to_list(SCDir) ++ "/lib/synapse"),
		    synapselauncher:startStatechum(StateChumOpts);
		_ ->
		    exit({"Statechum Dir not set in options file.",OFile}) 
	    end	
    end.

get_worker() ->	    
    Launcher = find_statechum(),
    Ref = make_ref(),
    Launcher ! {self(), Ref, getStatechumWorker},
    receive
	{Ref,PID} ->
	    {Ref,PID}
    end.



				
