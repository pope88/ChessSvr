.PHONY: all clean

LIB_PB_HOME=../dep/erlang_protobuffs
INCLUDE_HOME=../include

PROTO =  gate_cli.proto gate_svr.proto game.proto
INC = $(PROTO:.proto=_pb.hrl)

# all: compile

compile: $(INC)

%_pb.hrl:%.proto game.proto
	#erl -noshell -pa $(LIB_PB_HOME)/ebin -s protobuffs_compile generate_source gate_svr.proto -s init stop
	erl -noshell -pa $(LIB_PB_HOME)/ebin -s protobuffs_compile scan_file $< -s init stop

clean:
	rm -f *.beam *.dump *.hrl *.erl



