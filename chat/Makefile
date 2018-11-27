.SUFFIXES: .erl .beam .yrl


.erl.beam:
	erlc -W $<

## to test lib_chan
##    make server in one window
##    make client in the other window

MODS =  any_apply \
        chat_client chat_group chat_server\
	io_widget kvs\
	lib_chan lib_chan_mm lib_chan_cs lib_chan_test\
        lib_chan_auth \
        mod_echo mod_math mod_chat_controller mod_name_server\
        test1 test_name_server mod_srpc


ERL = erl -boot start_clean

all:	compile
	@echo "To run the chat test program"
	@echo "  1) start two xterms"
	@echo "  2) in xterm one type -- make chat_server"
	@echo "  3) in xterm two type -- make chat_client"
	@echo "  Note: remember to type make in the directory above this"
	@echo "        before running make in this directory"
	@echo "To run the lib_chan test program"
	@echo "  1) start two xterms"
	@echo "  2) in xterm one type -- make server"
	@echo "  3) in xterm two type -- make client"
	@echo "  Ignore any output in the server window"
	@echo "  The results of the test are printed in the client window"

# .erl file to .beam file  at MODS:
# mkdir -p 계층적인 directory  생성
# conf file을 ${HOME}/.erlang_config/lib_chan.conf으로 저장
compile: ${MODS:%=%.beam}
	mkdir -p ${HOME}/.erlang_config/
	cp conf ${HOME}/.erlang_config/lib_chan.conf
	@echo "make clean - clean up"

#compile: 실행
# -pa option directory 설정 .
# -s option chat_client의 test 실행
chat_client: compile
	erl -pa ../ -s chat_client test

test: compile
	erl -s chat_tests start

#compile: 실행
# -pa option directory 설정 .
# -s option chat_server의 start 실행
chat_server: compile
	erl -pa ../ -s chat_server start

server: compile
	erl -pa ../ -boot start_clean -pa '.' -s lib_chan_test start_server

client: compile
	erl -pa ../ -boot start_clean -pa '.' -s lib_chan_test start_client

clean:
	rm -rf *.beam erl_crash.dump
