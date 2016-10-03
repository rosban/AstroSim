%%%
%
%	A "Linda" based many body astrophysics simulator.
%	
%	new() - Create a new space, returns pid
%	newBody(Space_Pid, Mass, Radius, 
%		[Position_x, Position_y, Position_z], 
%		[Velocity_x, Velocity_y, Velocity_z],
%		[Acceleration_x, Acceleration_y, Acceleration_z]) 
%			- Create a new body, returns pid 
%	readSpace(Space_Pid) - Returns the space of bodies
%
%%%

-module(astrosim).
-export([newBody/6, loopBody/2, new/0, readSpace/1, loopSpace/1]).

-record(space_record, {space, requests}).
-record(body_record, {ref, mass, radius, position, velocity, acceleration}).

newBody(Space_Pid, Mass, Radius, Position, Velocity, Acceleration) ->
	spawn_link(astrosim, loopBody, [Space_Pid, #body_record{ref = make_ref(), 
		mass = Mass, radius = Radius, position = Position, velocity = Velocity, 
		acceleration = Acceleration}]).
	
loopBody(Space_Pid, Body_Record) ->
	
	%%%%%%%%%%%%%%%%%%%%%
	
	DT=1,
	timer:sleep(1000),
	
	%%%%%%%%%%%%%%%%%%%%%
	
	Updated_Body_Record = Body_Record#body_record{
		position = pos(DT, Body_Record#body_record.position, 
			Body_Record#body_record.velocity, 
			Body_Record#body_record.acceleration),
		velocity = vel(DT, Body_Record#body_record.velocity, 
			Body_Record#body_record.acceleration),
		acceleration = acc(Body_Record#body_record.position,
			readSpace(Space_Pid))},

	updateSpace(Space_Pid, Updated_Body_Record),
	loopBody(Space_Pid, Updated_Body_Record).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pos(_, [], [], []) ->
	[];	
pos(DT, [Hp|Tp], [Hv|Tv], [Ha|Ta]) ->
	[Hp + Hv*DT + Ha*math:pow(DT,DT)/2] ++ pos(DT, Tp, Tv, Ta). 
	
vel(_, [], []) ->
	[];
vel(DT, [Hv|Tv], [Ha|Ta]) ->
	[Hv + Ha*DT] ++ vel(DT, Tv, Ta).

acc(Pos, Body_Records) ->
	acc(Pos, Pos, Body_Records).
acc(_, [], _) ->
	[];
acc(_Base_Pos, _Pos, [_Hbr|_Tbr]) ->
	%G=1,
	%[G*Hbr#
	[0,0,0].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() ->
	spawn_link(astrosim, loopSpace, [#space_record{space=[], requests=[]}]).

% Returns the whole space
readSpace(Space_Pid) ->
	%Tag = make_ref(),
	Space_Pid ! {read, self()},
	receive
		Body_Records ->
			Body_Records
	end.

updateSpace(Space_Pid, Body_Record) ->
	Space_Pid ! {update, Body_Record}.
	
loopSpace(Space_Record) ->
	
	case Space_Record#space_record.requests of
		[H|_] ->
			H ! Space_Record#space_record.space;
		H ->
			H
	end, 
		
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
						space = (Space_Record#space_record.space 
						-- [Old_Body_Record]) ++ [Body_Record],
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
	
match(_P, _P) -> true;
match(_P, _Q) -> false.	
