-module(synapse_sm_eunit).

-include("synapse.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

sanity_check_test_() ->
    {"Apply the sanity_check function",
     {inorder,
      [
       {test, ?MODULE, sanity_check_1}
       ,{test, ?MODULE, sanity_check_2}
       ,{test, ?MODULE, sanity_check_3}
       ,{test, ?MODULE, sanity_check_4}
       ,{test, ?MODULE, sanity_check_5}
       ,{test, ?MODULE, sanity_check_6}
      ]}
    }.


walk_test_() ->
        {"Walk traces on some state machines",
     {inorder,
      [
       {test, ?MODULE, walk_test_1}
       ,{test, ?MODULE, walk_test_2}
       ,{test, ?MODULE, walk_test_3}
       ,{test, ?MODULE, walk_test_4}
       ,{test, ?MODULE, walk_test_5}
      ]}
    }.

get_content_test_() ->
    {"Get states and alphabets from transitions",
     {inorder,
      [
       {test, ?MODULE, get_states_1}
       ,{test, ?MODULE, get_alphabet_1}
       ]}
     }.

sanity_check_1() ->
    ?assert(synapse_sm:sanity_check(wibble_machine())).

sanity_check_2() ->
    ?assertNot(synapse_sm:sanity_check(#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=d
					  ,alphabet=[wibble,wobble]
					 })).
sanity_check_3() ->
    ?assertNot(synapse_sm:sanity_check(#statemachine{
					  states=[a]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 })).
sanity_check_4() ->
    ?assertNot(synapse_sm:sanity_check(#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble]
					 })).
sanity_check_5() ->
    %% Alphabet can be a superset
    ?assert(synapse_sm:sanity_check(#statemachine{
					  states=[a,b,c]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble,waggle]
					 })).
sanity_check_6() ->
    %% States can be a superset
    ?assert(synapse_sm:sanity_check(#statemachine{
					  states=[a,b,c,d,e]
					  ,transitions=[{a,wibble,b},{b,wobble,c}]
					  ,initial_state=a
					  ,alphabet=[wibble,wobble]
					 })).

walk_test_1() ->
    F = synapse_sm:walk(wibble_machine(),[init]),
    ?assertEqual(F,{ok, '2'}).

walk_test_2() ->
    F = synapse_sm:walk(wibble_machine(),[init,'read/0']),
    ?assertEqual(F,{ok, '2'}).

walk_test_3() ->
    F = synapse_sm:walk(wibble_machine(),[init,'read/wibble']),
    ?assertEqual(F,{failed_at,[init,'read/wibble']}).

walk_test_4() ->
    F = synapse_sm:walk(wibble_machine(),[init,lock,'write(wibble)',unlock]),
    ?assertEqual(F,{ok, '5'}).

walk_test_5() ->
    F = synapse_sm:walk(wibble_machine(),[init,lock,'write(wibble)',unlock,lock,'write(0)',unlock]),
    ?assertEqual(F,{ok, '2'}).


get_states_1() ->
    S = synapse_sm:get_states((wibble_machine())#statemachine.transitions),
    ?assertEqual(S,lists:sort((wibble_machine())#statemachine.states)).

get_alphabet_1() ->
    S = synapse_sm:get_alphabet((wibble_machine())#statemachine.transitions),
    ?assertEqual(S,lists:sort((wibble_machine())#statemachine.alphabet)).



wibble_machine() ->
    #statemachine{
		states = ['1','2','3','4','5'],
		transitions = [
				{'1',init,'2'}
				,{'2','read/0','2'}
				,{'2',lock,'3'}
				,{'3',unlock,'2'}
				,{'3','read/0','3'}
				,{'3','write(wibble)','4'}
				,{'4','write(0)','3'}
				,{'4','read/wibble','4'}
				,{'4',unlock,'5'}
				,{'5',lock,'4'}
				,{'5','read/wibble','5'}
			       ],
		initial_state = '1',
		alphabet = lists:sort([init,lock,unlock,'read/0','read/wibble','write(0)','write(wibble)'])
	       }.
