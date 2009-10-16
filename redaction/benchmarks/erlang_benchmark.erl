#!/usr/bin/env /home/dave/projet/maitrise/memoire/redaction/benchmarks/otp_src_R13B02-1/bin/escript

-mode(compile).

pingpong(Pid2) ->
	Pid2 ! {self(), ping},
	receive 
		{Pid2, pong} -> ok
	end.

pingloop(_, 0) -> stop;
pingloop(Pid2, I) -> pingpong(Pid2), pingloop(Pid2, I-1).

pongloop() ->
	receive
		{From, ping} -> 
			From ! {self(), pong},
			pongloop();
		stop ->
			true
	end.

main(_) ->
	Pid2 = spawn(erlang_benchmark, pongloop, []),
  {_, T1_s, T1_us} = erlang:now(),
  pingloop(Pid2, 500000),
  {_, T2_s, T2_us} = erlang:now(),
  io:format("(erlang recv: ~w)~n",[T2_s - T1_s + (T2_us - T1_us)/1000000]).

