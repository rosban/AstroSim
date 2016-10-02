%%%
%
%	A "Linda" based many body astrophysics simulator.
%	
%	
%%%

-module(astrosim).
-export([newBody/6, loopBody/2, new/0, readSpace/1, updateSpace/2, loopSpace/1]).

-record(space_record, {space, requests}).
-record(body_record, {ref, mass, radius, position, velocity, acceleration}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newBody(Space_Pid, Mass, Radius, Position, Velocity, Acceleration) ->
	spawn_link(astrosim, loopBody, [Space_Pid, #body_record{ref = make_ref(), 
		mass = Mass, radius = Radius, position = Position, velocity = Velocity, 
		acceleration = Acceleration}]).
	
loopBody(Space_Pid, Body_Record) ->
	
	timer:sleep(1000),
	
	Updated_Body_Record = Body_Record#body_record{
		position = pos(Body_Record#body_record.position, 
			Body_Record#body_record.velocity, 
			Body_Record#body_record.acceleration),
		velocity = vel(Body_Record#body_record.velocity, 
			Body_Record#body_record.acceleration),
		acceleration = acc(Body_Record#body_record.position,
			readSpace(Space_Pid))},

	updateSpace(Space_Pid, Updated_Body_Record),
	loopBody(Space_Pid, Updated_Body_Record).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
pos(Pos, _Vel, _Acc) ->
	Pos.
	
vel(Vel, _Acc) ->
	Vel.

acc(_Pos, _Body_Records) ->
	{0,0,0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() ->
	spawn_link(astrosim, loopSpace, [#space_record{space=[], requests=[]}]).

%inSpace(TS, Pattern) ->
%	Tag = make_ref(),
%	TS ! {in, {Tag, self(), Pattern}},
%	receive 
%		{Tag, Response} ->
%			Response
%	end.

% Returns the whole space
readSpace(Space_Pid) ->
	%Tag = make_ref(),
	Space_Pid ! {read, self()},
	receive
		Body_Records ->
			Body_Records
	end.

% Puts Body_record in space and removes old Body_record matching ref.
updateSpace(Space_Pid, Body_Record) ->
	Space_Pid ! {update, Body_Record}.
	
loopSpace(Space_Record) ->
	
	case Space_Record#space_record.requests of
		[H|_] ->
			H ! Space_Record#space_record.space;
		H ->
			H
	end, 
	
	%AllReqTupCombs = [{Req, Tup} ||
	%	Req <- Space_Record#space_record.requests,
	%	Tup <- Space_Record#space_record.space],
	%case findMatch(AllReqTupCombs) of 
	%	{Req, Tup} ->
	%		{Tag, Pid, _} = Req,
	%		Pid ! {Tag, Tup},
	%		loopSpace(Space_Record#space_record{
	%			space = Space_Record#space_record.space -- [Tup],
	%			requests = Space_Record#space_record.requests -- [Req]
	%		});
	%	false ->
	%		false
	%end,		
		
	receive 
		{read, Pid} ->
			Pid ! Space_Record#space_record.space,
			loopSpace(Space_Record#space_record{ 
				requests = Space_Record#space_record.requests 
				-- [H] ++ [Pid]});            
		{update, Body_Record} ->
			case findBody(Space_Record#space_record.space, Body_Record#body_record.ref) of
				false ->
					loopSpace(Space_Record#space_record{
						space = Space_Record#space_record.space 
						++ [Body_Record], 
						requests = Space_Record#space_record.requests 
						-- [H]});
				Old_Body_Record ->
					loopSpace(Space_Record#space_record{
						space = Space_Record#space_record.space 
						-- [Old_Body_Record] ++ [Body_Record],
						requests = Space_Record#space_record.requests 
						-- [H]})
			end;
		quit  ->
			true
	end.

findBody([], _) ->
	false;
findBody([H|T], Ref) ->
	case match(H#body_record.ref, Ref) of
		true ->
			H;
		false ->
			findBody(T, Ref)	
	end.
	
%findMatch([]) ->
%	false;
%findMatch([{Req, Tup} | T])	->
%	case match(lists:last(tuple_to_list(Req)), Tup) of 
%		true ->
%			{Req, Tup};
%		false ->
%			findMatch(T)
%	end.
	
	
%match(any, _) -> true;
%match(P, Q) when is_tuple(P), is_tuple(Q) -> 
%	match(tuple_to_list(P),tuple_to_list(Q));
%match([P|PS], [L|LS]) -> 
%	case match(P, L) of 
%			match(PS, LS);
%		false -> 
%			false
%	end;
match(_P, _P) -> true;
match(_P, _Q) -> false.	
