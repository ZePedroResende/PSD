build: client frontend exchange
current_dir = $(shell pwd)

client:
	protoc --java_out=. Protos/protocol.proto
	javac -cp dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:dependencies/jar/java-json.jar Protos/Protocol.java Client/src/main/java/*.java

frontend:
	dependencies/gpd/bin/protoc-erl -I. -maps -o Frontend/ Protos/protocol.proto
	erlc -I dependencies/gpb/include -o Frontend/ Frontend/protocol.erl
	erlc -I dependencies/erlzmq2/include -o Frontend/ Frontend/erlzmq.erl
	erlc -I dependencies/erlzmq2/include -o Frontend/ Frontend/erlzmq_nif.erl
	erlc -o Frontend/ Frontend/erlzmq.erl Frontend/exchange*.erl Frontend/mochijson.erl Frontend/frontend.erl Frontend/login_manager.erl Frontend/user_manager.erl

exchange:
	protoc --java_out=. Protos/protocol.proto
	javac -cp dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:dependencies/jar/gson-2.6.2.jar Protos/Protocol.java Exchange/src/main/java/*.java

pub-broker:
	javac -cp dependencies/jar/jeromq-0.4.3.jar Exchange/src/main/java/NotificationBroker.java

push-broker:
	javac -cp dependencies/jar/jeromq-0.4.3.jar Exchange/src/main/java/Broker.java


# 7000 -&gt; frontend (mudar consoante cliente) | 4442 XPUB
runcli:
	java -cp .:dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:. client.src.main.java.Client localhost 7000 4442

# 3331 -&gt; push para broker | 5000 pull do ator (mudar consoante exchange) | XSUB 4441
run-exchange:
	java -cp .:dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:dependencies/jar/gson-2.6.2.jar Exchange.src.main.java.Exchange 3331 5000 4441 1

run-pub-broker:
	java -cp .:dependencies/jar/jeromq-0.4.3.jar Exchange.src.main.java.NotificationBroker 4441 4442

push-pull-broker:
	java -cp .:dependencies/jar/jeromq-0.4.3.jar Exchange.src.main.java.Broker 3331 3332 


run-dir:
	gradle run
#	java -jar directory/target/stockdirectory-1.0-SNAPSHOT.jar server directory/conf.yml

run-fe:
	cd dependencies/erlzmq2/ebin && ./fe.sh


.PHONY: exchange frontend client 

clean:
	-@rm Client/*.class
	-@rm Frontend/*.beam
	-@rm Frontend/protocol.erl
	-@rm Client/Protocol2.java
	-@rm Exchange/Protocol.java
	-@rm Exchange/*.class
