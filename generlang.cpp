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
// This module generates Erlang files that can communicate using a
// specified protocol. The .proto grammar gets mapped to Erlang in the
// following way:
//
// The fields of messages and structures get mapped to the following
// types:
//
//    bool       -> atoms 'true' and 'false'
//    int16      -> integer
//    int32      -> integer
//    int64      -> integer
//    double     -> number | '+Inf' | '-Inf' | 'NaN'
//    string     -> string
//    binary     -> binary
//    array of T -> list
//    T          -> type T = { *record notation* }
//    optional T -> either the type T or the atom 'nil'
//    enums      -> the values get mapped to atoms
//
// Request and reply messages and structures are mapped to Erlang in a
// similar way, as record definitions. The tag field will be the name
// of the data type (in lowercase) with one of the following prefixes:
// request_, reply_ or struct_.
//
// For instance, the protocol definition
//
//    struct Time {
//        int sec;
//        int msec;
//    }
//
// will be defined in Erlang as
//
//    -record(struct_time, {sec, msec}).
//
// The name of the .proto file will generate two files, a header
// containing the record definitions and a source file which marshals
// and unmarshals the various types. The base name of the files will
// have "_protocol" appended and the module directive will match the
// file's base name.
//
// Marshal and unmarshal routines will throw an exception if an error
// condition is detected. The term provided in the exception has the
// form
//
//     {protocol_error, Reason}
//
// where 'Reason' is a string description of the protocol failure.

#include <inttypes.h>
#include <sstream>
#include <fstream>
#include <numeric>
#include <cctype>
#include "pc.h"

// Local prototypes

static void dumpEachMarshaller(Protocol const&, Struct const&,
			       std::ostream&);
static void dumpEachUnmarshaller(Protocol const&, Struct const&,
				 std::ostream&);
static void dumpHeaderFile(Protocol const&, std::ostream&);
static void dumpMarshaller(Protocol const&, Struct const&, std::ostream&);
static void dumpProtocol(Protocol const&);
static void dumpSourceFile(Protocol const&, std::ostream&);
static void dumpUnmarshaller(Protocol const&, Struct const&, std::ostream&);
static void emit_record(Protocol const&, Struct const&, std::ostream&);
static std::string int_to_bin(int64_t, bool = true, uint8_t = 0x10);

class EmptyRecEmitter {
    std::string const join;
    bool first;
    Struct::Usage const usage;

 public:
    EmptyRecEmitter(size_t const sz, Struct::Usage const u) :
	join(std::string(" |\n") + std::string(sz, ' ')), first(true), usage(u)
    {}

    void operator()(Protocol const& p, Struct const& s, std::ostream& os)
    {
	if (s.isUsed() && s.use == usage) {
	    if (!first)
		os << join;
	    os <<
		'#' << toLower(p.name) << '_' << toLower(s.name) << '_' <<
		::usage(s) << "{}";
	    first = false;
	}
    }
};

std::string usage(Struct const& s)
{
    switch (s.use) {
     case Struct::asRequest:
	return "request";

     case Struct::asReply:
	return "reply";

     case Struct::asEnum:
	return "enum";

     case Struct::asStruct:
	return "struct";

     default:
	throw std::logic_error("bad usage call");
    }
}

static std::string int_to_bin(int64_t const val, bool const brackets,
			      uint8_t const tag)
{
    std::ostringstream tmp;

    emitRawInt(tmp, tag, val);

    std::ostringstream os;
    std::string const s = tmp.str();

    if (brackets)
	os << "<<";
    for (std::string::const_iterator ii = s.begin(); ii != s.end(); ++ii) {
	if (ii != s.begin())
	    os << ',';
	os << "16#" << std::hex << (int) (uint8_t) *ii;
    }
    if (brackets)
	os << ">>";
    return os.str();
}

static std::string xlatType(Protocol const& p, std::string const& type)
{
    if (type == "int16" || type == "int32" || type == "int64" ||
	type == "double" || type == "string" || type == "binary" ||
	type == "bool")
	return type;
    else
	return toLower(p.name) + '_' + toLower(type) +
	    (isEnum(p, type) ? "_enum" : "_struct");
}

// Emits the contents of the header file.

static void dumpHeaderFile(Protocol const& p, std::ostream& os)
{
    os <<
	"% Generated by the protocol compiler version " << pcVersion << "\n"
	"% DO NOT EDIT THIS FILE DIRECTLY!\n\n";

    apply(p, os, p.types.begin(), p.types.end(), emit_record);
    apply(p, os, p.requests.begin(), p.requests.end(), emit_record);
    apply(p, os, p.replies.begin(), p.replies.end(), emit_record);
}

// Emits the contents of the source file.

static void dumpSourceFile(Protocol const& p, std::ostream& os)
{
    os <<
	"% Generated by the protocol compiler version " << pcVersion << "\n"
	"% DO NOT EDIT THIS FILE DIRECTLY!\n\n"
	"-module(" << toLower(p.name) << "_protocol).\n"
	"-include(\"" << toLower(p.name) << "_protocol.hrl\").\n\n";

    if (erlangNative)
	os << "-compile(native).\n\n";

    for (StructList::const_iterator ii = p.types.begin();
	 ii != p.types.end(); ++ii)
	if (ii->use == Struct::asEnum && p.needed(ii->name)) {
	    std::string const enumName =
		toLower(p.name) + '_' + ii->name + "_enum";

	    os <<
		"-type " << enumName << "() :: ";
	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		if (jj != ii->fields.begin())
		    os << " | ";
		os << '\'' << jj->name << '\'';
	    }
	    os <<
		".\n\n"
		"-export_type([" << enumName << "/0]).\n\n";
	}

    if (p.requests.begin() != p.requests.end()) {
	os <<
	    "-type request() :: ";
	apply(p, os, p.requests.begin(), p.requests.end(),
	      EmptyRecEmitter(19, Struct::asRequest));
	os <<
	    ".\n\n"
	    "-export_type([request/0]).\n"
	    "-export([marshal_request/1, unmarshal_request/1]).\n\n";
    }

    if (p.replies.begin() != p.replies.end()) {
	os <<
	    "-type reply() :: ";
	apply(p, os, p.replies.begin(), p.replies.end(),
	      EmptyRecEmitter(17, Struct::asReply));
	os <<
	    ".\n\n"
	    "-export_type([reply/0]).\n"
	    "-export([marshal_reply/1, unmarshal_reply/1]).\n\n";
    }

    if (p.needed("bool"))
	os <<
	    "-spec m_bool(boolean()) -> binary().\n\n"
	    "-compile({inline, [m_bool/1]}).\n"
	    "m_bool(false) -> <<16#70>>;\n"
	    "m_bool(true) -> <<16#71>>.\n\n";

    os <<
	"-spec m_int(integer(), 0 .. 16#f0) -> binary().\n\n"
	"m_int(X, Tag) when is_integer(X) ->\n"
	"  if\n"
	"    X >= -16#80 andalso X =< 16#7f ->\n"
	"      <<(16#01 bor Tag), X:8/signed>>;\n"
	"    X >= -16#8000 andalso X =< 16#7fff ->\n"
	"      <<(16#02 bor Tag), X:16/signed>>;\n"
	"    X >= -16#800000 andalso X =< 16#7fffff ->\n"
	"      <<(16#03 bor Tag), X:24/signed>>;\n"
	"    X >= -16#80000000 andalso X =< 16#7fffffff ->\n"
	"      <<(16#04 bor Tag), X:32/signed>>;\n"
	"    X >= -16#8000000000 andalso X =< 16#7fffffffff ->\n"
	"      <<(16#05 bor Tag), X:40/signed>>;\n"
	"    X >= -16#800000000000 andalso X =< 16#7fffffffffff ->\n"
	"      <<(16#06 bor Tag), X:48/signed>>;\n"
	"    X >= -16#80000000000000 andalso X =< 16#7fffffffffffff ->\n"
	"      <<(16#07 bor Tag), X:56/signed>>;\n"
	"    X >= -16#8000000000000000 andalso X =< 16#7fffffffffffffff ->\n"
	"      <<(16#08 bor Tag), X:64/signed>>\n"
	"  end.\n\n";

    if (p.needed("int16"))
	os <<
	    "-spec m_int16(-16#8000 .. 16#7fff) -> binary().\n\n"
	    "-compile({inline, [m_int16/1]}).\n"
	    "m_int16(X) when X >= -16#8000 andalso X =< 16#7fff ->\n"
	    "  m_int(X, 16#10).\n\n";

    if (p.needed("int32"))
	os <<
	    "-spec m_int32(-16#80000000 .. 16#7fffffff) -> binary().\n\n"
	    "-compile({inline, [m_int32/1]}).\n"
	    "m_int32(X) when X >= -16#80000000 andalso X =< 16#7fffffff ->\n"
	    "  m_int(X, 16#10).\n\n";

    if (p.needed("int64"))
	os <<
	    "-compile({inline, [m_int64/1]}).\n"
	    "m_int64(X) -> m_int(X, 16#10).\n\n";

    if (p.needed("double"))
	os <<
	    "-compile({inline, [m_double/1]}).\n"
	    "m_double(X) when is_number(X) ->\n"
	    "    <<16#28,X:64/float-big>>;\n"
	    "m_double('+Inf') ->\n"
	    "    <<16#28,16#7ff:12/big,0:52>>;\n"
	    "m_double('-Inf') ->\n"
	    "    <<16#28,16#fff:12/big,0:52>>;\n"
	    "m_double('NaN') ->\n"
	    "    <<16#28,16#7ff:12/big,1:1,0:51/big>>.\n\n";

    if (p.needed("string")) {
	if (erlangStringsAsBinaries)
	    os <<
		"-spec m_string(binary()) -> binary().\n\n"
		"-compile({inline, [m_string/1]}).\n"
		"m_string(B) ->\n"
		"  <<(m_int(size(B), 16#40))/binary, B/binary>>.\n\n";
	else
	    os <<
		"-spec m_string(string()) -> binary().\n\n"
		"-compile({inline, [m_string/1]}).\n"
		"m_string(S) ->\n"
		"  Bin = list_to_binary(S),\n"
		"  <<(m_int(size(Bin), 16#40))/binary, Bin/binary>>.\n\n";
    }

    if (p.needed("binary"))
	os <<
	    "-spec m_binary(binary()) -> binary().\n\n"
	    "-compile({inline, [m_binary/1]}).\n"
	    "m_binary(B) ->\n"
	    "  <<(m_int(size(B), 16#30))/binary, B/binary>>.\n\n";

    if (p.anyOptionals() || p.anyOptionalArrays())
	os <<
	    "count_nil_hlp(nil, A) -> 1 + A;\n"
	    "count_nil_hlp(_, A) -> A.\n\n"
	    "-define(count_nil(X), lists:foldl(fun count_nil_hlp/2, 0, X)).\n\n";

    if (p.anyOptionals())
	os <<
	    "-define(m_optional(Tag, X, F),\n"
	    "        (if\n"
	    "            X =:= nil -> <<>>;\n"
	    "            true -> <<Tag/binary, (F(X))/binary>>\n"
	    "         end)).\n\n";
    if (p.anyOptionalArrays())
	os <<
	    "-define(m_optional_array(Tag, XS, F),\n"
	    "        (if\n"
	    "            XS =:= nil -> <<>>;\n"
	    "            true ->\n"
	    "              <<Tag/binary,(m_int(length(XS), 16#50))/binary,\n"
	    "                (<< <<(F(__X))/binary>> || __X <- XS>>)/binary >>\n"
	    "         end)).\n\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpEachMarshaller);

    apply(p, os, p.requests.begin(), p.requests.end(), dumpEachMarshaller);

    if (p.requests.begin() != p.requests.end()) {
	os <<
	    "-spec marshal_request(request()) -> binary() | no_return().\n\n"
	    "marshal_request(X) ->\n"
	    "  <<$S,$D,$D,2,16#51,3," <<
	    int_to_bin(p.longHash(p.name), false) <<
	    ", \n"
	    "   (case X of\n";
	apply(p, os, p.requests.begin(), p.requests.end(), dumpMarshaller);
	os <<
	    "       _ -> throw({protocol_error, \"bad request value\"})\n"
	    "   end)/binary>>.\n\n";
    }

    apply(p, os, p.replies.begin(), p.replies.end(), dumpEachMarshaller);

    if (p.replies.begin() != p.replies.end()) {
	os <<
	    "-spec marshal_reply(reply()) -> binary() | no_return().\n\n"
	    "marshal_reply(X) ->\n"
	    "  <<$S,$D,$D,2,16#51,3," <<
	    int_to_bin(p.longHash(p.name), false) <<
	    ",\n"
	    "    (case X of\n";
	apply(p, os, p.replies.begin(), p.replies.end(), dumpMarshaller);
	os <<
	    "       _ -> throw({protocol_error, \"bad reply value\"})\n"
	    "    end)/binary>>.\n\n";
    }

    if (p.needed("bool"))
	os <<
	    "-compile({inline, [u_bool/1]}).\n"
	    "u_bool(<<Tag, Rest/binary>>) ->\n"
	    "  case Tag of\n"
	    "    16#70 -> {Rest, false};\n"
	    "    16#71 -> {Rest, true}\n"
	    "  end.\n\n";

    os <<
	"u_tagged_int(<<Tag:4, Len:4, Val:Len/signed-unit:8, Rest/binary>>, Tag)\n"
	"  when Len =< 8 ->\n"
	"    {Rest, Val}.\n\n";

    if (p.needed("int16")) {
	Struct const& s = p.findType("int16");

	if (s.usedInArray)
	    os <<
		"u_int16(<<1:4, Len:4, Val:Len/signed-unit:8, Rest/binary>>)\n"
		"  when Len =< 2 ->\n"
		"    {Rest, Val}.\n\n";
    }

    if (p.needed("int32")) {
	Struct const& s = p.findType("int32");

	if (s.usedInArray)
	    os <<
		"u_int32(<<1:4, Len:4, Val:Len/signed-unit:8, Rest/binary>>)\n"
		"  when Len =< 4 ->\n"
		"    {Rest, Val}.\n\n";
    }

    if (p.needed("int64"))
	os <<
	    "-compile({inline, [u_int64/1]}).\n"
	    "u_int64(Bin) -> u_tagged_int(Bin, 1).\n\n";

    if (p.needed("double"))
	os <<
	    "u_double(<<16#28, 16#7ff:12/big, 0:52/big, Rest/binary>>) ->\n"
	    "    {Rest, '+Inf'};\n"
	    "u_double(<<16#28, 16#fff:12/big, 0:52/big, Rest/binary>>) ->\n"
	    "    {Rest, '-Inf'};\n"
	    "u_double(<<16#28, _:1, 16#7ff:11/big, _:52, Rest/binary>>) ->\n"
	    "    {Rest, 'NaN'};\n"
	    "u_double(<<16#28, Val:64/float-big, Rest/binary>>) ->\n"
	    "    {Rest, Val}.\n\n";

    if (p.needed("string")) {
	Struct const& s = p.findType("string");

	if (s.usedInArray) {
	    os <<
		"-compile({inline, [u_string/1]}).\n"
		"u_string(<<4:4,N:4/unsigned,L:N/signed-unit:8,S:L/binary,Rest/binary>>) ->\n";
	    if (erlangStringsAsBinaries)
		os << "  {Rest, S}.\n\n";
	    else
		os << "  {Rest, binary_to_list(S)}.\n\n";
	}
    }

    if (p.needed("binary")) {
	Struct const& s = p.findType("binary");

	if (s.usedInArray) {
	    os <<
		"-compile({inline, [u_binary/1]}).\n"
		"u_binary(<<3:4,N:4/unsigned,L:N/signed-unit:8,B:L/binary,Rest/binary>>) ->\n"
		"  {Rest, B}.\n\n";
	}
    }

    os <<
	"-compile({inline, [u_array_/3]}).\n"
	"u_array_(Rest, _, 0) -> {Rest, []};\n"
	"u_array_(Rest, UFunc, Len) ->\n"
	"  {NRest, Val} = UFunc(Rest),\n"
	"  {Final, Retval} = u_array_(NRest, UFunc, Len - 1),\n"
	"  {Final, [Val | Retval]}.\n\n"
	"u_array(Bin, UFunc) ->\n"
	"  {Rest, Len} = u_tagged_int(Bin, 5),\n"
	"  u_array_(Rest, UFunc, Len).\n\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpEachUnmarshaller);

    apply(p, os, p.requests.begin(), p.requests.end(), dumpEachUnmarshaller);

    if (p.requests.begin() != p.requests.end()) {
	os <<
	    "unmarshal_request_(Bin) ->\n"
	    "    case u_tagged_int(Bin, 1) of\n";
	apply(p, os, p.requests.begin(), p.requests.end(), dumpUnmarshaller);
	os <<
	    "        _ ->\n"
	    "            throw({protocol_error, \"unknown request type\"})\n"
	    "    end.\n\n";

	os <<
	    "-spec unmarshal_request(binary()) -> request().\n\n"
	    "unmarshal_request(<<$S,$D,$D,2,16#51,3,Proto/binary>>) ->\n"
	    "    {Rest, " << std::dec << p.longHash(p.name) <<
	    "} = u_tagged_int(Proto, 1),\n"
	    "    element(2, unmarshal_request_(Rest)).\n\n";
    }

    apply(p, os, p.replies.begin(), p.replies.end(), dumpEachUnmarshaller);

    if (p.replies.begin() != p.replies.end()) {
	os <<
	    "unmarshal_reply_(Bin) ->\n"
	    "    case u_tagged_int(Bin, 1) of\n";
	apply(p, os, p.replies.begin(), p.replies.end(), dumpUnmarshaller);
	os <<
	    "        _ ->\n"
	    "            throw({protocol_error, \"unknown reply type\"})\n"
	    "    end.\n\n";

	os <<
	    "-spec unmarshal_reply(binary()) -> reply().\n\n"
	    "unmarshal_reply(<<$S,$D,$D,2,16#51,3,Proto/binary>>) ->\n"
	    "    {Rest, " << std::dec << p.longHash(p.name) <<
	    "} = u_tagged_int(Proto, 1),\n"
	    "    element(2, unmarshal_reply_(Rest)).\n\n";
    }
}

struct PFState {
    int& fieldNo;
    bool& first;
    std::string const indent;
};

static void printField(Protocol const&, Field const& f, std::ostream& os,
		       PFState const& state)
{
    if (!state.first)
	os << ",\n" << state.indent;
    os << toLower(f.name) << "=Fld" << std::dec << ++state.fieldNo;
    state.first = false;
}

static void nilField(Protocol const&, Field const& f, std::ostream& os,
		     PFState const& state)
{
    ++state.fieldNo;
    if (f.optional) {
	if (!state.first)
	    os << ", ";
	os << "Fld" << std::dec << state.fieldNo;
	state.first = false;
    }
}

static void marshalField(Protocol const& p, Field const& f, std::ostream& os,
			 PFState const& state)
{
    os << ",\n" << state.indent;
    ++state.fieldNo;
    if (f.optional)
	os << "(?m_optional" << (f.array ? "_array(" : "(") <<
	    int_to_bin(f.hash) << ", Fld" << state.fieldNo <<
	    ", fun m_" << xlatType(p, f.type) << "/1))/binary";
    else {
	os <<
	    int_to_bin(f.hash, false) << ", ";
	if (f.array)
	    os <<
		"(m_int(length(Fld" << state.fieldNo << "), 16#50))/binary,"
		"(<< <<(m_" << xlatType(p, f.type) <<
		"(X))/binary>> || X <- Fld" << state.fieldNo << ">>)/binary";
	else
	    os <<
		"(m_" << xlatType(p, f.type) << "(Fld" << state.fieldNo <<
		"))/binary";
    }
}

static void dumpEachMarshaller(Protocol const& p, Struct const& s,
			       std::ostream& os)
{
    if (s.isUserDefined() && s.isUsed()) {
	if (s.use == Struct::asEnum) {
	    std::string const fName = "m_" + toLower(p.name) + '_' +
		toLower(s.name) + "_enum";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii) {
		if (ii != s.fields.begin())
		    os << ";\n";
		os <<
		    fName << "('" << ii->name << "') ->\n    " <<
		    int_to_bin(ii->hash, true, 0x80);
	    }
	    os << ".\n\n";
	} else {
	    std::string const recName =
		toLower(p.name) + '_' + toLower(s.name) + '_' + usage(s);

	    if (s.fields.size() > 0) {
		std::string const text = "m_" + recName + "(#" + recName;
		int fldno = 0;
		bool first = true;

		os << text << "{";
		{
		    PFState state = { fldno, first,
				      std::string(text.size() + 1, ' ') };

		    apply(p, os, s.fields.begin(), s.fields.end(), printField,
			  state);
		}
		os <<
		    "}) ->\n";

		size_t const required = countRequired(s);

		if (required == s.fields.size())
		    os <<
			"  <<" << int_to_bin(required * 2, false, 0x50);
		else {
		    os <<
			"  N = " << s.fields.size() << " - ?count_nil([";
		    fldno = 0;
		    first = true;
		    {
			PFState state = { fldno, first, "" };

			apply(p, os, s.fields.begin(), s.fields.end(),
			      nilField, state);
		    }
		    os <<
			"]),\n"
			"  <<(m_int(N * 2, 16#50))/binary";
		}

		fldno = 0;
		first = true;
		{
		    PFState state = { fldno, first, "   " };

		    apply(p, os, s.fields.begin(), s.fields.end(),
			  marshalField, state);
		}
		os << ">>.\n\n";
	    } else
		os << "m_" << recName << "(#" << recName << "{}) ->\n"
		    "  <<16#51,0>>.\n\n";
	}
    }
}

static void dumpMarshaller(Protocol const& p, Struct const& s,
			   std::ostream& os)
{
    std::string const recName =
	toLower(p.name) + '_' + toLower(s.name) + '_' + usage(s);

    os <<
	"       #" << recName << "{} ->\n"
	"         << " << int_to_bin(s.hash, false) <<
	", (m_" << recName << "(X))/binary>>;\n";
}

struct UFState {
    bool first;
    size_t& current;
    Struct const& s;
    std::string const& recName;
};

static void dumpBit(std::ostream& os, size_t const bit)
{
    static char const* const leading[] = {
	"16#1", "16#2", "16#4", "16#8"
    };

    os << leading[bit % 4] << std::string(bit / 4, '0');
}

static void unmarshalFieldCore(Protocol const& p, Field const& f,
			       std::ostream& os, UFState& state)
{
    std::string const& fName = toLower(f.name);

    if (f.array)
	os <<
	    "    {Rest, Val} = u_array(FV, fun u_" <<
	    xlatType(p, f.type) << "/1),\n";
    else
	os <<
	    "    {Rest, Val} = u_" << xlatType(p, f.type) <<
	    "(FV),\n";

    os <<
	"    u_" << state.recName << "_(Rest, setelement(#" <<
	state.recName << "." << fName << ", Out, Val), ";

    if (!f.optional) {
	os <<
	    "Mask bor ";
	dumpBit(os, state.current++);
    } else
	os << "Mask";

    os <<
	", N - 2)";
}

static void unmarshalField(Protocol const& p, Field const& f, std::ostream& os,
			   UFState& state)
{
    os <<
	(state.first ? "" : ";\n") <<
	"u_" << state.recName << "_(<<" <<
	int_to_bin(f.hash, false, 0x10) <<
	",";

    if (!f.array && f.type == "int16") {
	os <<
	    "1:4,Sz:4,Val:Sz/signed-unit:8,Rest/binary>>, Out, Mask, N)\n"
	    "  when N > 1, Sz =< 2 ->\n"
	    "    u_" << state.recName << "_(Rest, setelement(#" <<
	    state.recName << "." << toLower(f.name) << ", Out, Val), "
	    "Mask";
	if (!f.optional) {
	    os << " bor ";
	    dumpBit(os, state.current++);
	}
	os <<
	    ", N - 2)";
    } else if (!f.array && f.type == "int32") {
	os <<
	    "1:4,Sz:4,Val:Sz/signed-unit:8,Rest/binary>>, Out, Mask, N)\n"
	    "  when N > 1, Sz =< 4 ->\n"
	    "    u_" << state.recName << "_(Rest, setelement(#" <<
	    state.recName << "." << toLower(f.name) << ", Out, Val), "
	    "Mask";
	if (!f.optional) {
	    os << " bor ";
	    dumpBit(os, state.current++);
	}
	os <<
	    ", N - 2)";
    } else if (!f.array && f.type == "int64") {
	os <<
	    "1:4,Sz:4,Val:Sz/signed-unit:8,Rest/binary>>, Out, Mask, N)\n"
	    "  when N > 1, Sz =< 8 ->\n"
	    "    u_" << state.recName << "_(Rest, setelement(#" <<
	    state.recName << "." << toLower(f.name) << ", Out, Val), "
	    "Mask";
	if (!f.optional) {
	    os << " bor ";
	    dumpBit(os, state.current++);
	}
	os <<
	    ", N - 2)";
    } else if (!f.array && f.type == "string") {
	os <<
	    "4:4,Sz:4/unsigned,L:Sz/signed-unit:8,S:L/binary,Rest/binary>>, Out, Mask, N) when N > 1 ->\n"
	    "    u_" << state.recName << "_(Rest, setelement(#" <<
	    state.recName << "." << toLower(f.name) << ", Out, " <<
	    (erlangStringsAsBinaries ? "S" : "binary_to_list(S)") <<
	    "), Mask";
	if (!f.optional) {
	    os << " bor ";
	    dumpBit(os, state.current++);
	}
	os <<
	    ", N - 2)";
    } else if (!f.array && f.type == "binary") {
	os <<
	    "3:4,Sz:4/unsigned,L:Sz/signed-unit:8,B:L/binary,Rest/binary>>, Out, Mask, N) when N > 1 ->\n"
	    "    u_" << state.recName << "_(Rest, setelement(#" <<
	    state.recName << "." << toLower(f.name) << ", Out, B), "
	    "Mask";
	if (!f.optional) {
	    os << " bor ";
	    dumpBit(os, state.current++);
	}
	os <<
	    ", N - 2)";
    } else {
	os <<
	    "FV/binary>>, Out, Mask, N) when N > 1 ->\n";
	unmarshalFieldCore(p, f, os, state);
    }
    state.first = false;
}

static void dumpMask(std::ostream& os, size_t const bit)
{
    static char const* const leading[] = {
	"16#1", "16#3", "16#7", "16#f"
    };

    os << leading[bit % 4] << std::string(bit / 4, 'f');
}

static void dumpEachUnmarshaller(Protocol const& p, Struct const& s,
				 std::ostream& os)
{
    if (s.isUserDefined() && s.isUsed()) {
	if (s.use == Struct::asEnum) {
	    std::string const enumName =
		toLower(p.name) + '_' + toLower(s.name) + "_enum";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii)
		os <<
		    (ii == s.fields.begin() ? "" : ";\n") <<
		    "u_" << enumName << "_(" << ii->hash << ") -> '" <<
		    ii->name << '\'';

	    os <<
		".\n\n"
		"u_" << enumName << "(Bin) ->\n"
		"    {Rest, Val} = u_tagged_int(Bin, 8),\n"
		"    {Rest, u_" << enumName << "_(Val)}.\n\n";
	} else {
	    size_t const nReq = countRequired(s);
	    std::string const recName =
		toLower(p.name) + '_' + toLower(s.name) + '_' + usage(s);

	    if (s.fields.size() == 0)
		os <<
		    "u_" << recName << "(<<16#51, 16#00, Rest/binary>>) ->\n"
		    "  {Rest, #" << recName << "{}}.\n\n";
	    else {
		size_t curr = 0;
		UFState state = { true, curr, s, recName };

		apply(p, os, s.fields.begin(), s.fields.end(),
		      unmarshalField, state);

		if (nReq > 0) {
		    os <<
			";\n"
			"u_" << recName << "_(Bin, Out, ";
		    dumpMask(os, nReq - 1);
		    os <<
			", 0) -> {Bin, Out}.\n\n";
		} else
		    os <<
			";\n"
			"u_" << recName << "_(Bin, Out, 0, 0) -> {Bin, Out}.\n\n";

		os <<
		    "u_" << recName << "(<<5:4, Len:4, N:Len/unsigned-unit:8, Rest/binary>>) ->\n"
		    "    u_" << recName << "_(Rest, #" <<
		    recName << "{}, 0, N).\n\n";
	    }
	}
    }
}

static void dumpUnmarshaller(Protocol const& p, Struct const& s,
			     std::ostream& os)
{
    if (s.isUserDefined() && s.isUsed()) {
	std::string const recName =
	    toLower(p.name) + '_' + toLower(s.name) + '_' + usage(s);

	os <<
	    "        {Rest, " << std::dec << s.hash << "} ->\n"
	    "            u_" << recName << "(Rest);\n";
    }
}

// This function generates the .hrl and .erl files for a single
// protocol. It does this by opening the two output files and then
// calling the two functions that actually generate the content for
// each file type.

static void dumpProtocol(Protocol const& p)
{
    std::string hdrName = targetPath + "/" + toLower(p.name) + "_protocol.hrl";

    // Open the two files.

    std::ofstream hdr(hdrName.c_str());
    std::ofstream src((targetPath + "/" + toLower(p.name) + "_protocol.erl").c_str());

    dumpHeaderFile(p, hdr);
    dumpSourceFile(p, src);
}

class AddRecField {
    std::string join;
    Protocol const& p;

 public:
    AddRecField(Protocol const& _p, size_t const sz) :
	join(std::string(",\n") + std::string(sz, ' ')), p(_p)
    {
    }

    std::string operator()(std::string const& acc, Field const& elem)
    {
	std::string tmp = acc.size() ? acc + join : "";

	tmp += toLower(elem.name);
	if (elem.optional)
	    tmp += "=nil";
	else if (elem.array)
	    tmp += "=[]";
	else if (elem.type == "string") {
	    if (erlangStringsAsBinaries)
		tmp += "= <<>>";
	    else
		tmp += "=[]";
	} else if (elem.type == "int16" || elem.type == "int32" ||
		   elem.type == "int64")
	    tmp += "=0";
	else if (elem.type == "binary")
	    tmp += "= <<>>";
	else if (elem.type == "bool")
	    tmp += "=false";
	else if (elem.type == "double")
	    tmp += "=0.0";
	else if (isEnum(p, elem.type)) {
	    if (elem.optional)
		tmp += "=nil";
	    else {
		Struct const& s = p.findType(elem.type);

		tmp += "='" + s.fields.begin()->name + '\'';
	    }
	} else
	    tmp += "=#" + toLower(p.name) + '_' + toLower(elem.type) +
		"_struct{}";

	tmp += " :: ";

	if (elem.optional)
	    tmp += "'nil' | ";

	std::string type;

	if (elem.type == "string") {
	    if (erlangStringsAsBinaries)
		type = "unicode:unicode_binary()";
	    else
		type = "string()";
	} else if (elem.type == "int16" || elem.type == "int32" ||
		   elem.type == "int64")
	    type = "integer()";
	else if (elem.type == "binary")
	    type = "binary()";
	else if (elem.type == "bool")
	    type = "boolean()";
	else if (elem.type == "double")
	    type = "float() | 'NaN' | '+Inf' | '-Inf'";
	else
	    for (StructList::const_iterator ii = p.types.begin();
		 ii != p.types.end(); ++ii)
		if (ii->name == elem.type) {
		    if (ii->use == Struct::asEnum)
			type = toLower(p.name) + "_protocol:" +
			    toLower(p.name) + '_' + ii->name + "_enum()";
		    else
			type = "#" + toLower(p.name) + '_' +
			    toLower(elem.type) + "_struct{}";
		}

	return tmp + (elem.array ? "[" + type + ']' : type);
    }
};

static void emit_record(Protocol const& p, Struct const& s, std::ostream& os)
{
    if (s.isUserDefined() && s.use != Struct::asEnum && s.isUsed()) {
	std::string init = std::string("-record(") + toLower(p.name) + "_" +
	    toLower(s.name) + "_" + usage(s) + ", {";

	os <<
	    init <<
	    accumulate(s.fields.begin(), s.fields.end(), std::string(),
		       AddRecField(p, init.size())) << "}).\n\n";
    }
}

std::string toLower(std::string const& s)
{
    std::string d;

    for (size_t ii = 0; ii < s.size(); ii++) {
	char c = tolower(s[ii]);
	d.append(&c, 1);
    }
    return d;
}

// The only externally visible function from this module. This is the
// entry point to generate C++ files that handle the protocol.

void generateErlang(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("Erlang generator only supports SDDv2");

    dumpProtocol(p);
}
