-module(synapse_statechum).

-export([learn/2,passive_learn/2,diff/3,visualise/3,visualise/4,visualise_diff/4,learn_erlang/1,learn_erlang/2]).

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

passive_learn(Traces,MetaInfo) ->
    learn(Traces,[{'askQuestions','false'} | MetaInfo]).

learn_erlang(Module) ->
    learn_erlang(Module, [{'erlangInitialTraceLength','5'},{'erlangAlphabetAnyElements','ANY_WIBBLE'}]).

learn_erlang(Module,MetaInfo) ->
    {Ref, StateChum} = get_worker(),
    StateChum ! {Ref,updateConfiguration,MetaInfo},
    ok = display_progress(Ref),
    StateChum ! {Ref,learnErlang,Module,self()},
    get_final_progress(StateChum,Ref).

diff(SM1, SM2, MetaInfo) ->
    {Ref, StateChum} = get_worker(),
    StateChum ! {Ref,updateConfiguration,MetaInfo},
    ok = display_progress(Ref),
    StateChum ! {Ref, computeDiff, SM1, SM2},
    get_final_progress(StateChum,Ref).

visualise(SM, MetaInfo, Name) ->
    visualise(SM,MetaInfo,Name,['N1000']).

visualise(SM, MetaInfo, Name, Ignore) ->
    {Ref, StateChum} = get_worker(),
    StateChum ! {Ref,updateConfiguration,MetaInfo},
    ok = display_progress(Ref),
    StateChum ! {Ref, displayFSM, SM, Name, Ignore},
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
	    io:format("Status: ~p~n", [Msg]),
	    display_progress(Ref);
	{Ref, status, step, Info} ->
	    Msg = case Info of
		      {N} -> io_lib:format("~p states",[N]);
		      {S,_FSM,Reds} -> io_lib:format("~p states, ~p red states",[S,length(Reds)]);
		      M -> M
		      end,
	    io:format("Progress: ~s~n", [Msg]),
	    display_progress(Ref);
	{Ref, ok, Result} ->
	    Result;
	{Ref, Result} ->
	    Result;
	Other ->
	    io:format("Unexpected message <ref:~p>: ~p~n",[Ref,Other])
    end.

find_statechum() ->
    OFile = case synapse:get_config(statechum_conf) of
		{error, not_found} ->
		    exit("You must specify a statechum.conf file in your synapse.conf to use statechum.");
		{error, Other} ->
		    exit({"There was an error reading the synapse configuration.",Other});
		File ->
		    File
	    end,
    case erlang:module_loaded(synapselauncher) of
	true ->
	    case synapselauncher:find_statechum() of
		not_started ->
		    {ok,StateChumOpts} = file:consult(OFile),
		    synapselauncher:startStatechum(StateChumOpts),
		    find_statechum();
		PID ->
		    io:format("StateChum at ~p~n",[PID]),
		    PID
	    end;
	_ ->
	    {ok,StateChumOpts} = file:consult(OFile),
	    case lists:keyfind('StatechumDir', 1, StateChumOpts) of
		{'StatechumDir', SCDir} ->
		    code:add_path(atom_to_list(SCDir) ++ "/lib/synapse"),
		    synapselauncher:startStatechum(StateChumOpts),
		    find_statechum();
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



				
