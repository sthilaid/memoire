#!/usr/bin/env /home/dave/projet/maitrise/memoire/redaction/benchmarks/otp_src_R13B02-1/bin/escript

-mode(compile).

%-module(erlang_benchmark).
%-export([pongloop/0, benchrecv/0, benchrecvtimeoutloop/1, main/1]).

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

benchrecvloop(0) -> stop;
benchrecvloop(Index) ->
  receive
    allo -> benchrecvloop(Index-1)
  end.

send(0) ->  ok;
send(N) ->  self() ! allo, send(N-1).

benchrecv() ->
  send(1000000),
  {_, T1_s, T1_us} = erlang:now(),
  benchrecvloop(1000000),
  {_, T2_s, T2_us} = erlang:now(),
  receive
    {result, To} -> To ! T2_s - T1_s + (T2_us - T1_us)/1000000
  end.

benchrecvtimeoutloop(0) -> stop;
benchrecvtimeoutloop(Index) ->
  receive
    after 0 -> benchrecvtimeoutloop(Index-1)
  end.

main(_) ->
  RecvBenchPid = spawn(erlang_benchmark, benchrecv, []),
  RecvBenchPid ! {result, self()},
  receive
    RecvBenchValue -> io:format("(erlang recv: ~w)~n",[RecvBenchValue])
  end,
  {_, T1_s_a, T1_us_a} = erlang:now(),
  benchrecvtimeoutloop(1000000),
  {_, T2_s_a, T2_us_a} = erlang:now(),
  io:format("(erlang recv-timeout: ~w)~n",[T2_s_a - T1_s_a + (T2_us_a - T1_us_a)/1000000]),
	Pid2 = spawn(erlang_benchmark, pongloop, []),
  {_, T1_s, T1_us} = erlang:now(),
  pingloop(Pid2, 1000000),
  {_, T2_s, T2_us} = erlang:now(),
  io:format("(erlang ping-server: ~w)~n",[T2_s - T1_s + (T2_us - T1_us)/1000000]).

