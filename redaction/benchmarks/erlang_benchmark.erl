#!/usr/bin/env /home/dave/projet/maitrise/memoire/redaction/benchmarks/otp_src_R13B02-1/bin/escript

-mode(compile).

%-module(erlang_benchmark).
%-export([pongloop/0, send_recv_r/0, bench_send/0, bench_simple_recpt/0, bench_to_recpt/0, benchrecv/0, benchrecvto/0, main/1]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(0) ->  ok;
send(N) ->  self() ! {allo, 12}, send(N-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bench_send() ->
  {_, T1_s, T1_us} = erlang:now(),
  send(1000000),
  {_, T2_s, T2_us} = erlang:now(),
  receive
    {result, To} -> To ! T2_s - T1_s + (T2_us - T1_us)/1000000
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bench_simple_recpt_loop(0) -> stop;
bench_simple_recpt_loop(Index) ->
  receive
    {allo, 12} -> benchrecvloop(Index-1)
  end.

bench_simple_recpt() ->
  send(1000000),
  {_, T1_s, T1_us} = erlang:now(),
  bench_simple_recpt_loop(1000000),
  {_, T2_s, T2_us} = erlang:now(),
  receive
    {result, To} -> To ! T2_s - T1_s + (T2_us - T1_us)/1000000
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_recv_r() -> 
  receive 
		ping -> 
           send_recv_r();
    stop ->
           ok
	end.

send_recv_loop_s(R, 0) -> R ! stop;
send_recv_loop_s(R, I) ->
  R ! ping,
  send_recv_loop_s(R, I-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bench_to_recpt_loop(0) -> stop;
bench_to_recpt_loop(Index) ->
  receive
    {allo, 12} -> benchrecvloop(Index-1)
    after 2 -> bench_to_recpt_loop(Index-1)
  end.

bench_to_recpt() ->
  send(1000000),
  {_, T1_s, T1_us} = erlang:now(),
  bench_to_recpt_loop(1000000),
  {_, T2_s, T2_us} = erlang:now(),
  receive
    {result, To} -> To ! T2_s - T1_s + (T2_us - T1_us)/1000000
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

benchrecvloop(0) -> stop;
benchrecvloop(Index) ->
  receive
    salut -> benchrecvloop(Index-1) ;
    {allo, 12} -> benchrecvloop(Index-1)
  end.

benchrecv() ->
  send(1000000),
  {_, T1_s, T1_us} = erlang:now(),
  benchrecvloop(1000000),
  {_, T2_s, T2_us} = erlang:now(),
  receive
    {result, To} -> To ! T2_s - T1_s + (T2_us - T1_us)/1000000
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

benchrecvtoloop(0) -> stop;
benchrecvtoloop(Index) ->
  receive
    salut -> benchrecvtoloop(Index-1) ;
    {allo, 12} -> benchrecvtoloop(Index-1)
    after 2 -> benchrecvtoloop(Index-1)
  end.

benchrecvto() ->
  send(1000000),
  {_, T1_s, T1_us} = erlang:now(),
  benchrecvtoloop(1000000),
  {_, T2_s, T2_us} = erlang:now(),
  receive
    {result, To} -> To ! T2_s - T1_s + (T2_us - T1_us)/1000000
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main(_) ->

  %% Message sending benchmark %%
  SendBenchPid = spawn(erlang_benchmark, bench_send, []),
  SendBenchPid ! {result, self()},
  receive
    SendBenchValue -> io:format("(erlang !: ~w)~n",[SendBenchValue])
  end,

  %% Simple reception benchmark %%
  SimpleRecptBenchPid = spawn(erlang_benchmark, bench_simple_recpt, []),
  SimpleRecptBenchPid ! {result, self()},
  receive
    SimpleRecptBenchValue -> io:format("(erlang ?: ~w)~n",[SimpleRecptBenchValue])
  end,

  %% Simple reception with timeout benchmark %%
  ToRecptBenchPid = spawn(erlang_benchmark, bench_to_recpt, []),
  ToRecptBenchPid ! {result, self()},
  receive
    ToRecptBenchValue -> io:format("(erlang ?-timeout: ~w)~n",[ToRecptBenchValue])
  end,

  %% Send / Receive benchmark %%
	Pid2_SR = spawn(erlang_benchmark, send_recv_r, []),
  {_, T1_s_sr, T1_us_sr} = erlang:now(),
  send_recv_loop_s(Pid2_SR, 1000000),
  {_, T2_s_sr, T2_us_sr} = erlang:now(),
  io:format("(erlang !+?: ~w)~n",[T2_s_sr - T1_s_sr + (T2_us_sr - T1_us_sr)/1000000]),

  %% Recv benchmark %%
  RecvBenchPid = spawn(erlang_benchmark, benchrecv, []),
  RecvBenchPid ! {result, self()},
  receive
    RecvBenchValue -> io:format("(erlang recv: ~w)~n",[RecvBenchValue])
  end,

  %% Recv with timeout benchmark %%
  RecvToBenchPid = spawn(erlang_benchmark, benchrecvto, []),
  RecvToBenchPid ! {result, self()},
  receive
    RecvToBenchValue -> io:format("(erlang recv-timeout: ~w)~n",[RecvToBenchValue])
  end,

  %% Ping server benchmark %%
	Pid2 = spawn(erlang_benchmark, pongloop, []),
  {_, T1_s, T1_us} = erlang:now(),
  pingloop(Pid2, 1000000),
  {_, T2_s, T2_us} = erlang:now(),
  io:format("(erlang ping-server: ~w)~n",[T2_s - T1_s + (T2_us - T1_us)/1000000]).

