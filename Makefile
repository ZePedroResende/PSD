build: frontend
current_dir = $(shell pwd)

frontend:
	dependencies/gpb/bin/protoc-erl -I. -maps -o Frontend/ Protos/protocol.proto
	erlc -I dependencies/gpb/include -o Frontend/ Frontend/protocol.erl
	erlc -I dependencies/erlzmq2/include -o Frontend/ Frontend/erlzmq.erl
	erlc -I dependencies/erlzmq2/include -o Frontend/ Frontend/erlzmq_nif.erl
	erlc -o Frontend/ Frontend/erlzmq.erl Frontend/exchange*.erl Frontend/mochijson.erl Frontend/frontend.erl Frontend/login_manager.erl Frontend/user_manager.erl


run-frontend:
	cd dependencies/erlzmq2/ebin && ./fe.sh


.PHONY: frontend

clean:
	-@rm Frontend/*.beam
	-@rm Frontend/protocol.erl
