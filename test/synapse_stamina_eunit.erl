-module(synapse_stamina_eunit).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

read_trace_file_test_() ->
    {"Read STAMINA format trace files",
     {inorder,
      [
       {test, ?MODULE, read_test1}
       ,{test, ?MODULE, read_test2}
       ,{test, ?MODULE, read_test_fail}
       ,{test, ?MODULE, read_test_error}
      ]}
    }.

read_test1() ->
    Traces = synapse_stamina:read_trace_file("../test/test1.traces"),
    Expected = [
		{pos, ['x','x','x','y','y','x','x']}
		,{pos, ['x','x','x','x','x']}
		,{neg, ['x','y']}
		,{pos, ['x','x','x','y','x','x']}
		,{pos, ['x','x','x','y','y','y','y','y','x','x','x','y','y','y']}
		,{neg, ['x','x','x','y','y','x','x','x','y','y','x','y']}
	       ],
    ?assert(Traces =:= Expected).

read_test2() ->
    Traces = synapse_stamina:read_trace_file("../test/test2.traces"),
    Expected = [{pos,[init,lock,'read/0']},
		{pos,[init,lock,'write(wibble)','read/wibble']},
		{neg,[init,lock,'write(wibble)','read/0']},
		{pos,[init,lock,unlock,'read/0']},
		{neg,[init,'write(wibble)']},
		{pos,[init,lock,'write(wibble)',unlock,'read/wibble']},
		{pos,[init,lock,'write(wibble)',unlock,lock,'read/wibble']},
		{pos,[init,lock,'write(wibble)',unlock,lock,'write(0)',unlock,'read/0']}
	       ],
    io:format("Traces: ~n~p~n",[Traces]),
    ?assert(Traces =:= Expected).

read_test_fail() ->
    Traces = synapse_stamina:read_trace_file("../test/test-fail.traces"),
    Expected = [
		{error,"Cannot parse STAMINA line","wibble\n"}
		,{error,"Cannot parse STAMINA line","This is not stamina\n"}
	       ],
    ?assert(Traces =:= Expected).

read_test_error() ->
    E = synapse_stamina:read_trace_file("nonsene.traces"),
    ?assert(E =:= {error, not_found, "nonsene.traces"}).
