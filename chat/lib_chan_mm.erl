%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
%% Protocol
%%   To the controlling process
%%      {chan, MM, Term}
%%      {chan_closed, MM}
%%   From any process
%%      {send, Term}
%%      close

-module(lib_chan_mm).
%% TCP Middle man
%%   Models the interface to gen_tcp

-export([loop/2, send/2, close/1, controller/2, set_trace/2, trace_with_tag/2]).

%대기하는 프로세스에 각 메시지를 보내는 함수 .
send(Pid, Term)       -> Pid ! {send, Term}.
close(Pid)            -> Pid ! close.
controller(Pid, Pid1) -> Pid ! {setController, Pid1}.
set_trace(Pid, X)     -> Pid ! {trace, X}.

trace_with_tag(Pid, Tag) ->
    set_trace(Pid, {true,
		    fun(Msg) ->
			    io:format("MM:~p ~p~n",[Tag, Msg])
		    end}).
  %calling lib.chan.erl
  %     lib_chan_mm:loop(Socket, Controller).
  % Socket 은 listen, accept 된 소켓.
  %  Pid 는 Controller 이며 lib_chan:start_erl_port_server임.
  % 종료 옵션 허용 후 loop1 excute
  % 정리 - Socket을 대기상태로 한다.
loop(Socket, Pid) ->
    %% trace_with_tag(self(), trace),
    process_flag(trap_exit, true),
    % loop1 메세지 대기 상태. // client에서 massage를 보내는듯 하다.
    loop1(Socket, Pid, false).
%Socket 의 상태를 확인한다.
loop1(Socket, Pid, Trace) ->
    receive
	{tcp, Socket, Bin} ->
	    Term = binary_to_term(Bin),
	    trace_it(Trace,{socketReceived, Term}),
	    Pid ! {chan, self(), Term},
	    loop1(Socket, Pid, Trace);
	{tcp_closed, Socket} ->
	    trace_it(Trace, socketClosed),
	    Pid ! {chan_closed, self()};
	{'EXIT', Pid, Why} ->
	    trace_it(Trace,{controllingProcessExit, Why}),
	    gen_tcp:close(Socket);
	{setController, Pid1} ->
	    trace_it(Trace, {changedController, Pid}),
	    loop1(Socket, Pid1, Trace);
	{trace, Trace1} ->
	    trace_it(Trace, {setTrace, Trace1}),
	    loop1(Socket, Pid, Trace1);
	close ->
	    trace_it(Trace, closedByClient),
	    gen_tcp:close(Socket);
	{send, Term}  ->
	    trace_it(Trace, {sendingMessage, Term}),
      %해당 소캣에 term 데이터를 send 한다 
	    gen_tcp:send(Socket, term_to_binary(Term)),
	    loop1(Socket, Pid, Trace);
	UUg ->
	    io:format("lib_chan_mm: protocol error:~p~n",[UUg]),
	    loop1(Socket, Pid, Trace)
    end.
trace_it(false, _)     -> void;
trace_it({true, F}, M) -> F(M).
