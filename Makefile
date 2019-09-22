SSL_OBJ ?= openssl_md5.o

ifdef DEBUG
DOPTS = -g -O0
else
DOPTS = -O2 -DNDEBUG
endif

LDFLAGS ?=  -L/usr/pkg/lib -L/usr/local/ssl/lib -lssl -lcrypto

CXXFLAGS += ${DOPTS} -W -Wall -Werror -I/usr/pkg/include -I/usr/local/ssl/include
OBJS = main.o parser.o lexer.o gencpp.o genjava.o generlang.o genobjc.o genpython.o genocaml.o js_src.o genjavascript.o gengwtjava.o genrust.o ${SSL_OBJ} hash.o

ERLC ?= erlc
CXX ?= g++
RM ?= rm

pc : ${OBJS}
	${CXX} ${CXXFLAGS} ${LDFLAGS} -g -o pc ${OBJS}

${OBJS} : pc.h

parser.o lexer.o : lexer.h

clean :
	${RM} -rf *.o pc *.class *.beam test_protocol.erl js_src.cpp \
		test_protocol.hrl test_protocol.py test_protocol.ml*

test.cpp test.h : test.proto pc
	./pc -a -W -l c++ test.proto

test_protocol.py : test.proto pc
	./pc -W -a -l python test.proto

test.java : test.proto pc
	./pc -a -W -l java -p gov.fnal.controls test.proto

test.class : test.java
	javac -classpath . test.java

test.o : test.cpp test.h

test.m : test.proto pc
	./pc -W -l objc test.proto

js_src.cpp : proto_lib.js Makefile
	echo "#include <sys/types.h>" > js_src.cpp
	echo "#include \"pc.h\"" >> js_src.cpp
	echo "byte const js_lib[] = {" >> js_src.cpp
	hexdump -e "16/1 \"%3d, \" \"\\n\"" proto_lib.js | sed 's/^/    /' | sed 's/[, ]*$$/,/' >> js_src.cpp
	echo "};" >> js_src.cpp
	echo "size_t const js_lib_size = sizeof(js_lib);" >> js_src.cpp

genjavascript.o : js_src.o

regression.o : test.h

demo.cpp demo.h : demo.proto pc
	./pc -W -l c++ demo.proto

regression : regression.o test.o hash.o
	${CXX} ${CXXFLAGS} -o $@ $^ -lssl -lcrypto

proto_test.o : demo.h

proto_test : proto_test.o demo.o
	${CXX} ${CXXFLAGS} -o $@ $^

regression.beam : regression.erl test_protocol.hrl
	${ERLC} -Wall +debug_info regression.erl

test_protocol.beam : test_protocol.erl test_protocol.hrl
	${ERLC} -Wall +debug_info +export_all test_protocol.erl

test_protocol.erl test_protocol.hrl : test.proto pc
	./pc -a -W -l erlang test.proto

erl-regress : regression.beam test_protocol.beam erl-hdr
	@echo -n Building 'erl-regress' '... '
	@zip -q9 temp.zip regression.beam test_protocol.beam;\
	cat erl-hdr temp.zip > erl-regress;\
	${RM} temp.zip;\
	chmod +x erl-regress
	@echo done.
