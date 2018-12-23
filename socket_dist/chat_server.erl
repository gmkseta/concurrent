%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_server).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [delete/2, foreach/2, map/2, member/2,reverse/2]).

-compile(export_all).

% make chat_server 실행시 가장 먼저 실행됨.
%start_server() 와 lib_chan:start_server("chat.conf") 실행하게 된다 .
start() ->
    start_server(),
    lib_chan:start_server("chat.conf").

start_server() ->
  % register - chat_server 이 spawn 된 process id
  % process_flag - process가 exit signal을 받으면 종료되는 옵션
  % server_loop 실행  후 io:format 으로 출력
    register(chat_server,
	     spawn(fun() ->
			   process_flag(trap_exit, true),
			   Val= (catch server_loop([])),
			   io:format("Server terminated with:~p~n",[Val])
		   end)).


% 받아온 L 의 pattern 매칭.
server_loop(L) ->
    receive
	{mm, Channel, {login, Group, Nick}} ->
	    case lookup(Group, L) of
		{ok, Pid} ->
		    Pid ! {login, Channel, Nick},
		    server_loop(L);
		error ->
		    Pid = spawn_link(fun() ->
					     chat_group:start(Channel, Nick)
				     end),
		    server_loop([{Group,Pid}|L])
	    end;
	{mm_closed, _} ->
	    server_loop(L);
	{'EXIT', Pid, allGone} ->
	    L1 = remove_group(Pid, L),
	    server_loop(L1);
	Msg ->
	    io:format("Server received Msg=~p~n",
		      [Msg]),
	    server_loop(L)
    end.




lookup(G, [{G,Pid}|_]) -> {ok, Pid};
lookup(G, [_|T])       -> lookup(G, T);
lookup(_,[])           -> error.

remove_group(Pid, [{G,Pid}|T]) -> io:format("~p removed~n",[G]), T;
remove_group(Pid, [H|T])       -> [H|remove_group(Pid, T)];
remove_group(_, [])            -> [].
