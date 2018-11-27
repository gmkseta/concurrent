%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(chat_client).

-import(io_widget,
	[get_state/1, insert_str/2, set_prompt/2, set_state/2,
	 set_title/2, set_handler/2, update_state/3]).

-export([start/0, test/0, connect/5]).


start() ->
    connect("localhost", 2223, "AsDT67aQ", "general", "joe").

% make chat_client 실행시 가장 먼저 실행됨
test() ->
    connect("localhost", 2223, "AsDT67aQ", "general", "joe"),
    connect("localhost", 2223, "AsDT67aQ", "general", "jane"),
    connect("localhost", 2223, "AsDT67aQ", "general", "jim"),
    connect("localhost", 2223, "AsDT67aQ", "general", "sue").

% connect spawn 한다.
connect(Host, Port, HostPsw, Group, Nick) ->
    spawn(fun() -> handler(Host, Port, HostPsw, Group, Nick) end).

handler(Host, Port, HostPsw, Group, Nick) ->
    process_flag(trap_exit, true),
		% IO Widget 실행 .  UI 설정
    Widget = io_widget:start(self()),
    set_title(Widget, Nick),
    set_state(Widget, Nick),
    set_prompt(Widget, [Nick, " > "]),
		%data parse
    set_handler(Widget, fun parse_command/1),
			% connector //connect("localhost", 2223, "AsDT67aQ"
    start_connector(Host, Port, HostPsw),
    disconnected(Widget, Group, Nick).



disconnected(Widget, Group, Nick) ->
    receive
			%{connected pid 를 받음 .
	{connected, MM} ->
	    insert_str(Widget, "connected to server\nsending data\n"),
			% send(Pid, Term)       -> Pid ! {send, Term}. 하여
			% {send, Term}  ->
			%	 	trace_it(Trace, {sendingMessage, Term}),
			%		gen_tcp:send(Socket, term_to_binary(Term)),
			%		loop1(Socket, Pid, Trace);
			%   {login, Group, Nick} data를 Sock에 전속한다.
	    lib_chan_mm:send(MM, {login, Group, Nick}),
			%wait_login_response를  메세지 대기및 에러를 처리한다.
	    wait_login_response(Widget, MM);
	{Widget, destroyed} ->
	    exit(died);
	{status, S} ->
	    insert_str(Widget, to_str(S)),
	    disconnected(Widget, Group, Nick);
	Other ->
	    io:format("chat_client disconnected unexpected:~p~n",[Other]),
	    disconnected(Widget, Group, Nick)
    end.



wait_login_response(Widget, MM) ->
    receive
	{chan, MM, ack} ->
	    active(Widget, MM);
	Other ->
	    io:format("chat_client login unexpected:~p~n",[Other]),
	    wait_login_response(Widget, MM)
    end.



active(Widget, MM) ->
     receive
	 {Widget, Nick, Str} ->
	     lib_chan_mm:send(MM, {relay, Nick, Str}),
	     active(Widget, MM);
	 {chan, MM, {msg, From, Pid, Str}} ->
	     insert_str(Widget, [From,"@",pid_to_list(Pid)," ", Str, "\n"]),
	     active(Widget, MM);
	 {'EXIT',Widget,windowDestroyed} ->
	     lib_chan_mm:close(MM);
	 {close, MM} ->
	     exit(serverDied);
	 Other ->
	     io:format("chat_client active unexpected:~p~n",[Other]),
	     active(Widget, MM)
     end.


%  //connect("localhost", 2223, "AsDT67aQ"
start_connector(Host, Port, Pwd) ->
    S = self(),
    spawn_link(fun() -> try_to_connect(S, Host, Port, Pwd) end).

try_to_connect(Parent, Host, Port, Pwd) ->
    %% Parent is the Pid of the process that spawned this process
		%프로세스 생성후 대기 한다.
    case lib_chan:connect(Host, Port, chat, Pwd, []) of
	{error, _Why} ->
	    Parent ! {status, {cannot, connect, Host, Port}},
	    sleep(2000),
	    try_to_connect(Parent, Host, Port, Pwd);
		%% MM - 생성된 process
	{ok, MM} ->
		% controller(Pid, Pid1) -> Pid ! {setController, Pid1}. 실행하여
		% MM 에 {setController, Pid1} 전송
		%
		%	{setController, Pid1} ->
		%	    trace_it(Trace, {changedController, Pid}),
		%	    loop1(Socket, Pid1, Trace);
		%  후 에 trace_it(false, _)     -> void; 실행
	    lib_chan_mm:controller(MM, Parent),
   % {connected 메세지를 보냄
	    Parent ! {connected, MM},
	    exit(connectorFinished)
    end.


sleep(T) ->
    receive
    after T -> true
    end.

to_str(Term) ->
    io_lib:format("~p~n",[Term]).

% data parese
parse_command(Str) -> skip_to_gt(Str).

skip_to_gt(">" ++ T) -> T;
skip_to_gt([_|T])    -> skip_to_gt(T);
skip_to_gt([])       -> exit("no >").
