// Fermilab Software Legal Information (BSD License)
// Copyright (c) 2008-2019, Fermi Research Alliance, LLC
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the
// distribution.
//
// Neither the name of the FERMI NATIONAL ACCELERATOR LABORATORY, nor
// the names of its contributors may be used to endorse or promote
// products derived from this software without specific prior written
// permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.
//

#include <unistd.h>
#include <cstring>
#include <cstdlib>
#include <cctype>
#include <iostream>
#include <fstream>
#include "pc.h"

#define MAX_SDD_VERSION		3
#define DEFAULT_SDD_VERSION	2

char const pcVersion[] = "1.3.8";

enum Target { tgtNone, tgtCPP, tgtJava, tgtPython,
	      tgtErlang, tgtPerl, tgtObjc, tgtOCaml,
	      tgtJavascript, tgtRust };

bool verbose = false;
bool allStructs = false;
bool warnings = false;
CppLevel cppLevel = cppLegacy;
CppGenFiles cppGenFiles = cppBoth;
bool checkReturns = false;
TargetType target = Both;
bool erlangStringsAsBinaries = false;
bool erlangNative = true;
bool ocamlPolyVT = false;
bool pyV3 = false;
bool objcUseARC = true;
bool jsEmitLib = false;
bool jsEmitDts = false;
bool jsSingletons = false;
bool javaGwt = false;
bool javaStreams = false;
bool javaBuffers = false;
std::string javaPkg;
std::string targetPath(".");
std::string rustAllPublic = "";
unsigned sddVersion = DEFAULT_SDD_VERSION;

static void usage();

static Target tgt = tgtNone;
static std::list<std::string> paths;

static void setLang(char const* l)
{
    if (tgt != tgtNone)
	throw std::runtime_error("a language was already specified");

    if (!strcmp(l, "c++") ||
	!strcmp(l, "cpp") ||
	!strcmp(l, "cxx"))
	tgt = tgtCPP;
    else if (!strcmp(l, "java"))
	tgt = tgtJava;
    else if (!strcmp(l, "python") ||
	     !strcmp(l, "py"))
	tgt = tgtPython;
    else if (!strcmp(l, "perl") ||
	     !strcmp(l, "pl"))
	tgt = tgtPerl;
    else if (!strcmp(l, "erlang") ||
	     !strcmp(l, "erl"))
	tgt = tgtErlang;
    else if (!strcmp(l, "objective-c") ||
	     !strcmp(l, "objc"))
	tgt = tgtObjc;
    else if (!strcmp(l, "ocaml"))
	tgt = tgtOCaml;
    else if (!strcmp(l, "javascript") ||
	     !strcmp(l, "js")) {
	tgt = tgtJavascript;
	target = Client;
    } else if (!strcmp(l, "rust"))
	tgt = tgtRust;
    else
	throw std::runtime_error("unknown language specified");
}

static std::string const& langToStr(Target tgt)
{
    static std::string const name[] = {
	"none", "C++", "Java", "Python", "Erlang",
	"Perl", "Objective-C", "OCaml", "Javascript", "Rust"
    };

    return name[tgt];
}

static bool checkOption(char const* const arg, std::string const& option,
			Target const lang)
{
    if (option == arg + 2) {
	if (tgt == tgtNone)
	    tgt = lang;
	else if (tgt != lang) {
	    std::string const msg =
		"the --" + option + " option is for " + langToStr(lang) +
		" but the current language is set to " + langToStr(tgt);

	    throw std::runtime_error(msg);
	}
	return true;
    } else
	return false;
}

static void usage()
{
    std::cerr << "usage: pc [-V] [-I PATH] [-a] [-v] [-q] [-W] [-r] \\\n"
	"          [-l LANGUAGE] [...language modifiers...] file [...]\n\n"
	"    -a           generate code for all structs -- even if not used\n"
	"    -c, --client\n"
	"                 only generate code for protocol clients\n"
	"    -I PATH      adds a search path to find proto files\n"
	"    -l LANGUAGE  generates source code for a given language\n"
	"                 (supported languages are c++, java, erlang, objc,\n"
	"                 ocaml, javascript, python, and rust)\n"
	"    -q           removes verbosity from the output\n"
	"    -r           validate 'returns' statements\n"
	"    -s, --server\n"
	"                 only generate code for protocol servers\n"
	"    -t PATH      output target path\n"
	"    -v           adds verbosity to the output\n"
	"    -V           print the version numbers\n"
	"    -W           turns on more warnings\n"
	"    file         the proto file containing the protocol description\n\n"
	"  Language modifiers:\n"
	"    --c++-legacy\n"
	"                 use pre-C++11 language features -- only for use with\n"
	"                 older, outdated compilers. THIS IS THE DEFAULT AT\n"
	"                 FERMILAB.\n\n"
	"    --c++-11\n"
	"                 use C++11 language features: std::unique_ptr<> is used\n"
	"                 instead of std::auto_ptr<> for optional fields, range-based\n"
	"                 for-loops are used when iterating through containers, and\n"
	"                 strongly typed enumerations are used for enumerations.\n\n"
	"    --c++-14\n"
	"                 use C++11 features along with new C++14 features:\n"
	"                 AT THIS POINT, NO EXTRA FEATURES ARE INCLUDED.\n\n"
	"    --c++-17\n"
	"                 use C++14 features along with new C++17 features:\n"
	"                 AT THIS POINT, NO EXTRA FEATURES ARE INCLUDED.\n\n"
	"    --c++-exp\n"
	"                 use C++17 features along with experimental features:\n"
	"                 Optional fields use std::experimental::optional<T>.\n\n"
	"    --c++-header-only\n"
	"                 only the C++ header is generated\n\n"
	"    --c++-source-only\n"
	"                 only the C++ source file is generated\n\n"
	"    --erl-bin-str\n"
	"                 the Erlang generator will encode/decode strings in\n"
	"                 the protocol as binaries\n\n"
	"    --erl-no-native\n"
	"                 allows the generated Erlang code to be compiled as\n"
	"                 byte-code (this option is not recommended as it\n"
	"                 will greatly slow down serialization)\n\n"
	"    --java-use-pkg PACKAGE\n"
	"                 place generated Java classes in the specified package\n\n"
	"    --java-use-streams\n"
	"                 generate code using the Java I/O streams interface\n\n"
	"    --java-use-bytebuffers\n"
	"                 generate code using the Java ByteBuffers interface\n\n"
	"    --java-gwt\n"
	"                 creates Java overlay classes for use in Google\n"
	"                 Web Toolkit (GWT) based applications\n\n"
	"    --js-emit-library\n"
	"                 writes a source file containing functions common to\n"
	"                 all protocols. The created file is called 'proto_lib.js'\n\n"
	"    --js-dts\n"
	"                 generates a '.d.ts' file so protocols can be used in\n"
	"                 Typescript applications\n\n"
	"    --js-singletons\n"
	"                 messages are unmarshalled into a reused instance of\n"
	"                 the message.\n\n"
	"    --objc-no-arc\n"
	"                 generate objective-c code using older retain/release\n"
	"                 memory model\n\n"
	"    --ocaml-poly-vt\n"
	"                 requests and reply messages are specified using\n"
	"                 polymorphic variant types rather than just variant\n"
	"                 types\n\n"
	"    --python-v3\n"
	"                 target Python v3.x\n\n"
	"    --rust-all-public\n"
	"                 marks every function 'public' -- should only be used\n"
	"                 when running unit tests on the generated code\n";
    exit(1);
}

size_t countRequired(Struct const& s)
{
    size_t total = 0;

    for (FieldList::const_iterator ii = s.fields.begin(); ii != s.fields.end(); ++ii)
	if (!ii->optional)
	    ++total;
    return total;
}

int main(int argc, char** argv)
{
    std::string input;

    if (argc == 1)
	usage();

    try {
	const char* exePath = argv[0];

	paths.push_back("");
	paths.push_back(".");

	while (--argc) {
	    if (**++argv == '-')
		switch (argv[0][1]) {
		 case 'I':
		    if (argv[0][2])
			paths.push_front(*argv + 2);
		    else if (--argc)
			paths.push_front(*++argv);
		    else
			throw std::runtime_error("-I option needs a path");
		    if (verbose)
			std::cout << "Added \"" << *paths.begin() << "\" to search path\n";
		    break;

		 case 'h':
		 case '?':
		    usage();
            // fall through    
		 case 'a':
		    allStructs = true;
		    break;

		 case 'c':
		    target = Client;
		    break;

		 case 's':
		    target = Server;
		    break;

		 case 'W':
		    warnings = true;
		    break;

		 case 'r':
		    checkReturns = true;
		    break;

		 case 'v':
		    verbose = true;
		    break;

		 case 'q':
		    verbose = false;
		    break;

		 case 'l':
		    if (argv[0][2])
			setLang(*argv + 2);
		    else if (--argc)
			setLang(*++argv);
		    else
			throw std::runtime_error("-l option needs a language");
		    break;

		 case 't':
		    if (argv[0][2])
			targetPath = *argv + 2;
		    else if (--argc)
			targetPath = *++argv;
		    else
			throw std::runtime_error("-t option needs a path");

		    for (std::string::reverse_iterator ii = targetPath.rbegin();
			 ii != targetPath.rend(); ++ii)
			if (*ii != '/' && *ii != '\\') {
			    targetPath.erase(targetPath.rend() - ii);
			    break;
			}

		    if (access(targetPath.c_str(), W_OK) != 0)
			throw std::runtime_error("invalid target path:'" + targetPath + "'");

		    if (verbose)
			std::cout << "generated files will be written to '" <<
			    targetPath << "'\n";
		    break;

		 case 'V':
		    std::cout << exePath << " version: " << pcVersion <<
			"\nmax SDD version: " << MAX_SDD_VERSION <<
			"\ndefault SDD version: " << DEFAULT_SDD_VERSION <<
			'\n';
		    break;

		 case '-':
		    if (checkOption(*argv, "c++-legacy", tgtCPP))
			cppLevel = cppLegacy;
		    else if (checkOption(*argv, "c++-11", tgtCPP))
			cppLevel = cpp11;
		    else if (checkOption(*argv, "c++-14", tgtCPP))
			cppLevel = cpp14;
		    else if (checkOption(*argv, "c++-17", tgtCPP))
			cppLevel = cpp17;
		    else if (checkOption(*argv, "c++-17exp", tgtCPP)) {
		        std::cerr << "warning: the '--c++-17exp' option is deprecated -- use '--c++-exp' instead\n";
			cppLevel = cpp_exp;
		    } else if (checkOption(*argv, "c++-exp", tgtCPP))
			cppLevel = cpp_exp;
		    else if (checkOption(*argv, "c++-header-only", tgtCPP))
			cppGenFiles = cppHeader;
		    else if (checkOption(*argv, "c++-source-only", tgtCPP))
			cppGenFiles = cppSource;
		    else if (checkOption(*argv, "erl-bin-str", tgtErlang))
			erlangStringsAsBinaries = true;
		    else if (checkOption(*argv, "erl-no-native", tgtErlang))
			erlangNative = false;
		    else if (checkOption(*argv, "ocaml-poly-vt", tgtOCaml))
			ocamlPolyVT = true;
		    else if (checkOption(*argv, "js-emit-library", tgtJavascript)) {
			Protocol p;

			jsEmitLib = true;
			generateJS(p);
			return 0;
		    } else if (checkOption(*argv, "js-singletons", tgtJavascript))
			jsSingletons = true;
		    else if (checkOption(*argv, "js-dts", tgtJavascript))
			jsEmitDts = true;
		    else if (checkOption(*argv, "java-use-pkg", tgtJava)) {
			if (--argc && isalpha(argv[1][0]))
			    javaPkg = *++argv;
			else
			    throw std::runtime_error("java-use-pkg option needs a java package name");
		    } else if (checkOption(*argv, "java-use-streams", tgtJava))
			javaStreams = true;
		    else if (checkOption(*argv, "java-use-bytebuffers", tgtJava))
			javaBuffers = true;
		    else if (checkOption(*argv, "java-gwt", tgtJava))
			javaGwt = true;
		    else if (checkOption(*argv, "objc-no-arc", tgtObjc))
			objcUseARC = false;
		    else if (checkOption(*argv, "python-v3", tgtPython))
			pyV3 = true;
		    else if (checkOption(*argv, "rust-all-public", tgtRust))
			rustAllPublic = "pub ";
		    else if (!strcmp(argv[0] + 2, "client"))
			target = Client;
		    else if (!strcmp(argv[0] + 2, "server"))
			target = Server;
		    else
			throw std::runtime_error("unknown option");
		    break;

		 default:
		    throw std::runtime_error("unknown option");
		}
	    else {
		for (std::list<std::string>::const_iterator ii = paths.begin();
		     ii != paths.end(); ++ii) {
		    input = *ii + "/" + *argv;

		    if (verbose)
			std::cout << "Checking for '" << input << "'\n";

		    if (!access(input.c_str(), R_OK)) {
			if (verbose)
			    std::cout << "Protocol Compiler Version " << pcVersion <<
				"\n" << "Found \"" << input << "\" ... compiling\n";

			if (javaPkg.size() && tgt != tgtJava)
			    std::cout << "Ignoring java package option\n";

			switch (tgt) {
			 case tgtNone:
			    std::cerr << "warning: no language specified ... checking syntax\n";
			    parse(input);
			    break;

			 case tgtCPP:
			    if (target != Both)
				std::cerr << "warning: the -c/-s options aren't yet supported for this language\n";
			    generateCPlusPlus(parse(input));
			    break;

			 case tgtErlang:
			    if (target != Both)
				std::cerr << "warning: the -c/-s options aren't yet supported for this language\n";
			    generateErlang(parse(input));
			    break;

			 case tgtJava:
			    if (target != Both)
				std::cerr << "warning: the -c/-s options aren't yet supported for this language\n";
			    if (javaGwt)
				generateGwtJava(parse(input));
			    else
				generateJava(parse(input));
			    break;

			 case tgtObjc:
			    if (target != Both)
				std::cerr << "warning: the -c/-s options aren't yet supported for this language\n";
			    generateObjC(parse(input));
			    break;

			 case tgtOCaml:
			    generateOCaml(parse(input));
			    break;

			 case tgtJavascript:
			    generateJS(parse(input));
			    break;

			 case tgtPython:
			    if (target != Both)
				std::cerr << "warning: the -c/-s options aren't yet supported for this language\n";
			    generatePython(parse(input));
			    break;

			 case tgtRust:
			    if (target != Both)
				std::cerr << "warning: the -c/-s options aren't yet supported for this language\n";
			    generateRust(parse(input));
			    break;

			 default:
			    throw std::runtime_error("target language not supported yet");
			}
			goto loop;
		    }
		}
		throw std::runtime_error("can't find source file");
	    }
	loop:
	    ;
	}
    }
    catch (std::logic_error& e) {
	std::cerr << "Caught program bug: " << e.what() << std::endl;
	return 1;
    }
    catch (parse_error& e) {
	std::cerr << input << e.what() << std::endl;
	return 1;
    }
    catch (std::runtime_error& e) {
	std::cerr << "Error: " << e.what() << std::endl;
	return 1;
    }
    catch (std::exception& e) {
	std::cerr << "Caught exception: " << e.what() << std::endl;
	return 1;
    }
    return 0;
}
