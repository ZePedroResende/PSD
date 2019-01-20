build: stock-directory client frontend exchange

stock-directory:
	mvn -f stock-directory/pom.xml compile
	mvn -f stock-directory/pom.xml package

client:
	protoc --java_out=. protos/protocol.proto
	javac -cp dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:dependencies/jar/java-json.jar client/*.java

frontend:
	dependency/gpb/bin/protoc-erl -I. -maps -o Frontend/ Protos/protocol.proto
	erlc -I dependency/gpb/include -o Frontend/ Frontend/protocol.erl
	erlc -I dependency/erlzmq2/include -o Frontend/ Frontend/erlzmq.erl
	erlc -I dependency/erlzmq2/include -o Frontend/ Frontend/erlzmq_nif.erl
	erlc -o Frontend/ Frontend/exchange*.erl Frontend/mochijson.erl Frontend/frontend.erl Frontend/login_manager.erl Frontend/user_manager.erl

exchange:
	protoc --java_out=. protos/protocol2.proto
	javac -cp dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:dependencies/jar/gson-2.6.2.jar exchange/*.java

pub-broker:
	javac -cp dependencies/jar/jeromq-0.4.3.jar exchange/PubSubBroker.java

push-broker:
	javac -cp dependencies/jar/jeromq-0.4.3.jar exchange/Broker.java


# 7000 -&gt; frontend (mudar consoante cliente) | 4442 XPUB
runcli:
	java -cp .:dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:. client.Client localhost 7000 4442

# 3331 -&gt; push para broker | 5000 pull do ator (mudar consoante exchange) | XSUB 4441
run-exchange:
	java -cp .:dependencies/jar/protobuf-java-3.4.1.jar:dependencies/jar/jeromq-0.4.3.jar:dependencies/jar/gson-2.6.2.jar exchange.Exchange 3331 5000 4441 1

run-pub-broker:
	java -cp .:dependencies/jar/jeromq-0.4.3.jar exchange.PubSubBroker 4441 4442

push-pull-broker:
	java -cp .:dependencies/jar/jeromq-0.4.3.jar exchange.Broker 3331 3332 


run-stock-directody:
	java -jar stock-directory/target/stockdirectory-1.0-SNAPSHOT.jar server stock-directory/conf.yml

.PHONY: stock-directory exchange frontend client 

clean:
	-@rm client/*.class
	-@rm frontend/*.beam
	-@rm frontend/protocol.erl
	-@rm client/Protocol.java
	-@rm exchange/Protocol.java
	-@mvn -f stock-directory/pom.xml clean
	-@rm exchange/*.class
