%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(lib_chan).
-export([cast/2, start_server/0, start_server/1,
	 connect/5, disconnect/1, rpc/2]).
-import(lists, [map/2, member/2, foreach/2]).
-import(lib_chan_mm, [send/2, close/1]).

%%----------------------------------------------------------------------
%% Server code


% getenv("HOME")의 환경변수 값을 확인
% start_server($HOME + "/.erlang_config/lib_chan.conf" )
%으로 실행한다. connf file 로  start_server 실행
start_server() ->
    case os:getenv("HOME") of
	false ->
	    exit({ebadEnv, "HOME"});
	Home ->
	    start_server(Home ++ "/.erlang_config/lib_chan.conf")
    end.
% chat_server.erl 에서 lib_chan:start_server("chat.conf") 로 calling
% chat.conf 출력
%  file:consult(ConfigFile) -
%     chat.conf file 을 하나의[]로 만드며 {} 단위로 데이터를 나눔
%      case check_terms(ConfigData)  data 패턴 매칭
%    conf file 이 error인지 확인지 확인 후
%  		에러가 없으면 . start_server1 에 {}단위로 입력
start_server(ConfigFile) ->
    io:format("lib_chan starting:~p~n",[ConfigFile]),
    case file:consult(ConfigFile) of
	{ok, ConfigData} ->
	    io:format("ConfigData=~p~n",[ConfigData]),
			case check_terms(ConfigData) of
		[] ->
		    start_server1(ConfigData);
		Errors ->
		    exit({eDaemonConfig, Errors})[]
	    end;
	{error, Why} ->
	    exit({eDaemonConfig, Why})
    end.

% check_terms() -> [Error]
% map(fun check_term/1, ConfigData) - check_term/1 을 ConfigData 매핑
%  data가 해당 형식에 맞는지 확인
check_terms(ConfigData) ->
    L = map(fun check_term/1, ConfigData),
    [X || {error, X} <- L].

check_term({port, P}) when is_integer(P)     -> ok;
check_term({service,_,password,_,mfa,_,_,_}) -> ok;
check_term(X) -> {error, {badTerm, X}}.

% start_server1 - lib_chan 으로 process name사용
% 추후에 lib_chan ! message 형식으로 사용 가능.
%  Start_server1 의 data  ->
%		 {port, P} ,  {service,_,password,_,mfa,_,_,_} 형태로 입력된 data 삽입
% lib_chan 이름으로  spawn된 start_server2  입력받은 데이터 삽입.
start_server1(ConfigData) ->
    register(lib_chan, spawn(fun() -> start_server2(ConfigData) end)).

%start_server1 으로부터 온 data 패턴 매칭
% ConfigDta 가 {Port,P} 인 경우 P 을 Port에 저장.
% Port 에 관한 data인지 확인후 start_port_server로 포트번호
start_server2(ConfigData) ->
    [Port] = [ P || {port,P} <- ConfigData],
    start_port_server(Port, ConfigData).
%lib_chan_cs:start_raw_server 에 prot 번호를 삽입. 함수 생성,
%start_port_instance 으로 생성,
%start_port_instance - listen 이 이루어지게될 process
start_port_server(Port, ConfigData) ->
    lib_chan_cs:start_raw_server(Port,
				fun(Socket) ->
					start_port_instance(Socket,
							    ConfigData) end,
				100,
				4).

start_port_instance(Socket, ConfigData) ->
	% start_erl_port_server(S, ConfigData) 으로
	%  listen. accept한 Socket 설정 후 , 할당.
	%   lib_chan_mm:loop(Socket, Controller) -
    S = self(),
    Controller = spawn_link(fun() -> start_erl_port_server(S, ConfigData) end),
    lib_chan_mm:loop(Socket, Controller).

% start_erl_port_server(S, ConfigData)  S는 Pid configData는{ port,protnum} 임.
start_erl_port_server(MM, ConfigData) ->
    receive
	{chan, MM, {startService, Mod, ArgC}} ->
	    case get_service_definition(Mod, ConfigData) of

		{yes, Pwd, MFA} ->
		    case Pwd of
			none ->
			    send(MM, ack),
			    really_start(MM, ArgC, MFA);
			_ ->
			    do_authentication(Pwd, MM, ArgC, MFA)
		    end;

		no ->
		    io:format("sending bad service~n"),
		    send(MM, badService),
		    close(MM)
	    end;

	Any ->
	    io:format("*** ErL port server got:~p ~p~n",[MM, Any]),
	    exit({protocolViolation, Any})
    end.

do_authentication(Pwd, MM, ArgC, MFA) ->
    C = lib_chan_auth:make_challenge(),
    send(MM, {challenge, C}),
    receive
	{chan, MM, {response, R}} ->
	    case lib_chan_auth:is_response_correct(C, R, Pwd) of
		true ->
		    send(MM, ack),
		    really_start(MM, ArgC, MFA);
		false ->
		    send(MM, authFail),
		    close(MM)
	    end
    end.


%% MM is the middle man
%% Mod is the Module we want to execute ArgC and ArgS come from the client and
%% server respectively

really_start(MM, ArgC, {Mod, Func, ArgS}) ->
    %% authentication worked so now we're off
    case (catch apply(Mod,Func,[MM,ArgC,ArgS])) of
	{'EXIT', normal} ->
	    true;
	{'EXIT', Why} ->
	    io:format("server error:~p~n",[Why]);
	Why ->
	    io:format("server error should die with exit(normal) was:~p~n",
		      [Why])
    end.

%% get_service_definition(Name, ConfigData)

get_service_definition(Mod, [{service, Mod, password, Pwd, mfa, M, F, A}|_]) ->
    {yes, Pwd, {M, F, A}};
get_service_definition(Name, [_|T]) ->
    get_service_definition(Name, T);
get_service_definition(_, []) ->
    no.

%%----------------------------------------------------------------------
%% Client connection code
%% connect(...) -> {ok, MM} | Error
%------------------------------------------------------------
%try_to_connect(Parent, Host, Port, Pwd) ->
%    %% Parent is the Pid of the process that spawned this process
%    case lib_chan:connect(Host, Port, chat, Pwd, []) 으로 불린다.
%
% 함수 생성후 Pid ! Message 대기 .
connect(Host, Port, Service, Secret, ArgC) ->
    S = self(),
    MM = spawn(fun() -> connect(S, Host, Port) end),
    receive
	{MM, ok} ->
	    case authenticate(MM, Service, Secret, ArgC) of
		ok    -> {ok, MM};
		Error -> Error
	    end;
	{MM, Error} ->
	    Error
    end.
% lib_chan_cs:start_raw_client(Host, Port, 4)- connect 하는 socket 생성
connect(Parent, Host, Port) ->
	% Sock 생성 .
    case lib_chan_cs:start_raw_client(Host, Port, 4) of
	{ok, Socket} ->
		% socke 생성 완료 ok message 를 보냄
	    Parent ! {self(), ok},
			% 대기 상태로 한다 .
	    lib_chan_mm:loop(Socket, Parent);
	Error ->
	    Parent ! {self(),  Error}
    end.

authenticate(MM, Service, Secret, ArgC) ->
    send(MM, {startService, Service, ArgC}),
    %% we should get back a challenge or a ack or closed socket
    receive
	{chan, MM, ack} ->
	    ok;
	{chan, MM, {challenge, C}} ->
	    R = lib_chan_auth:make_response(C, Secret),
	    send(MM, {response, R}),
	    receive
		{chan, MM, ack} ->
		    ok;
		{chan, MM, authFail} ->
		    wait_close(MM),
		    {error, authFail};
		Other ->
		    {error, Other}
	    end;
	{chan, MM, badService} ->
	    wait_close(MM),
	    {error, badService};
	Other ->
	    {error, Other}
    end.

wait_close(MM) ->
    receive
	{chan_closed, MM} ->
	    true
    after 5000 ->
	    io:format("**error lib_chan~n"),
	    true
    end.

disconnect(MM) -> close(MM).

rpc(MM, Q) ->
    send(MM, Q),
    receive
	{chan, MM, Reply} ->
	    Reply
    end.

cast(MM, Q) ->
    send(MM, Q).
