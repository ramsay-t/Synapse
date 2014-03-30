-module(synapse_stamina).

-export([read_trace_file/1,parse_line/1]).

-include("synapse.hrl").

read_trace_file(Filename) ->
    case file:open(Filename,[read]) of
	{ok, IO} ->
	    Traces = read_lines(IO,[]),
	    file:close(IO),
	    Traces;
	{error, enoent} ->
	    {error, not_found, Filename};
	{error, Reason} ->
	    {error,Reason};
	R ->
	    exit({"Impossible response from file:open",R})
    end.

read_lines(IO,Traces) ->
    case file:read_line(IO) of
	{ok, Data} ->
	    read_lines(IO,[parse_line(Data) | Traces]);
	eof ->
	    lists:reverse(Traces);
	{error, Reason} ->
	    {error,Reason};
	R ->
	    exit({"Impossible response from file:read_line",R})
    end.

parse_line([$+ | Line]) ->
    {pos, parse_events(Line)};
parse_line([$- | Line]) ->
    {neg, parse_events(Line)};
parse_line(Line) ->
    {error, "Cannot parse STAMINA line", Line}.

parse_events([]) ->
    [];
parse_events(Line) ->
    {Ev, NewLine} = parse_event(Line),
    case Ev of
	'' ->
	    parse_events(NewLine);
	_ ->
	    [Ev | parse_events(NewLine)]
    end.

parse_event([]) ->
    {list_to_atom([]),[]};
parse_event([$\ | Line]) ->
    parse_event(Line);
parse_event([$\n| Line]) ->
    parse_event(Line);
parse_event([C]) ->
    {list_to_atom([C]), []};
parse_event([C, $\  | Line]) ->
    {list_to_atom([C]), Line};
parse_event([C, $\n | Line]) ->
    {list_to_atom([C]), Line};
parse_event([C | Line]) ->
    {Tail,NewLine} = parse_event(Line),
    {list_to_atom([C] ++ atom_to_list(Tail)), NewLine}.

