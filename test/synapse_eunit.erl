-module(synapse_eunit).

-include("synapse.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

learn_test_() ->
    {setup,
     fun() ->
	     synapse:load_config("../synapse.conf")
     end,
     {"Test learning of a simple example",
      {inorder,
       [
	{test, ?MODULE, learn1}
	,{test, ?MODULE, learn1}
       ]}
     }
    }.

unsupported_learner_test_() ->
    {setup,
     fun() ->
	     synapse:load_config("../synapse.conf")
     end,
     {"Check learner support",
      {inorder,
       [
	{test,?MODULE,unsupported}
       ]}
     }
    }.

unsupported() ->
    Res = synapse:learn(fake_learner,[],[]),
    ?assertEqual({error,"Learner not supported",fake_learner,{"Supported Learners",[statechum]}}, Res).

learn1() ->
    Traces = synapse_stamina:read_trace_file("../test/test1.traces"),
    Meta = [{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}],
    SM = synapse:learn(Traces,Meta),
    ?assert(synapse_sm:sanity_check(SM)).

learn2() ->
    Traces = synapse_stamina:read_trace_file("../test/test2.traces"),
    Meta = [{'askQuestions','false'},{'gdFailOnDuplicateNames','false'}],
    SM = synapse:learn(Traces,Meta),
    check2(SM).

check2(SM) ->
    ?assert(synapse_sm:sanity_check(SM)),
    %% State names are not fixed, but they can be defined based on
    %% walks.
    ?assertEqual(
       synapse_sm:walk(SM,[init])
       ,synapse_sm:walk(SM,[init,lock,unlock])
      ),
    ?assertEqual(
       synapse_sm:walk(SM,[init,lock])
       ,synapse_sm:walk(SM,[init,lock,unlock,lock])
      ),
    ?assertEqual(
       synapse_sm:walk(SM,[init])
       ,synapse_sm:walk(SM,[init,'read/0'])
      ),
    ?assertEqual(
       synapse_sm:walk(SM,[init,lock,'write(wibble)'])
       ,synapse_sm:walk(SM,[init,lock,'write(wibble)',unlock,lock])
      ),
    ?assertEqual(
       synapse_sm:walk(SM,[init])
       ,synapse_sm:walk(SM,[init,lock,'write(wibble)','write(0)',unlock])
      ).

