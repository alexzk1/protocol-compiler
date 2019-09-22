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

// This module generates OCaml files that can communicate using a
// specified protocol. The .proto grammar gets mapped to OCaml in the
// following way:
//
// The fields of messages and structures get mapped to the following
// types:
//
//    bool       -> bool
//    int16      -> int
//    int32      -> int32
//    int64      -> int64
//    double     -> float
//    string     -> string
//    binary     -> Bytes.t
//    array of T -> 't array
//    enum T     -> type T_e = Constructors
//    T          -> type T = { *record notation* }
//    optional T -> 't option

#include <iostream>
#include <fstream>
#include <iomanip>
#include <assert.h>
#include <sstream>
#include "pc.h"

struct enum_state {
    bool* flags;
    UseType mTyp;
    UseType uTyp;
};

static inline std::string indent(size_t const indent)
{
    return std::string(indent / 8, '\t') + std::string(indent % 8, ' ');
}

static std::string mk_constInt(uint8_t tag, int64_t const val)
{
    std::ostringstream tmp;

    emitRawInt(tmp, tag, val);

    std::string const s = tmp.str();
    std::ostringstream os;

    os << '"';
    for (std::string::const_iterator ii = s.begin(); ii != s.end(); ++ii)
	os <<
	    "\\x" <<
	    std::setw(2) <<
	    std::setfill('0') <<
	    std::hex <<
	    ((int) *ii & 0xff);
    os << '"';
    return os.str();
}

static void emit_ConstInt(std::ostream& os, uint8_t tag, int64_t const val)
{
    std::string const s = mk_constInt(tag, val);

    os << "emit_str f " << mk_constInt(tag, val);
}

static Struct const& findStruct(Protocol const&p, std::string const& type)
{
    return p.findType(type);
}

static std::string to_enum_name(std::string const& ename)
{
    std::string tmp(ename);

    tmp[0] = tolower(tmp[0]);
    return tmp + "_e";
}

static std::string to_ctor(bool const poly, std::string const& ename)
{
    std::string tmp(ename);

    tmp[0] = toupper(tmp[0]);
    return poly ? '`' + tmp : tmp;
}

static std::string to_struct_name(bool stype, std::string const& sname)
{
    std::string tmp(sname);

    tmp[0] = tolower(tmp[0]);
    return stype ? tmp + "_s" : tmp;
}

static std::string xlatType(Struct const& s, bool const isArray,
			    bool const isOptional)
{
    std::string tmp;

    if (s.name == "bool")
	tmp = "bool";
    else if (s.name == "int16")
	tmp = "int";
    else if (s.name == "int32")
	tmp = "int32";
    else if (s.name == "int64")
	tmp = "int64";
    else if (s.name == "double")
	tmp = "float";
    else if (s.name == "string")
	tmp = "string";
    else if (s.name == "binary")
	tmp = "Bytes.t";
    else if (s.use == Struct::asEnum)
	tmp = to_enum_name(s.name);
    else if (s.use == Struct::asStruct)
	tmp = to_struct_name(true, s.name);

    if (isArray)
	tmp += " array";

    if (isOptional)
	tmp += " option";

    return tmp;
}

static std::string mName(Struct const& s)
{
    std::string tmp;

    if (s.name == "bool")
	tmp = "bool";
    else if (s.name == "int16")
	tmp = "int16";
    else if (s.name == "int32")
	tmp = "int32";
    else if (s.name == "int64")
	tmp = "int64";
    else if (s.name == "double")
	tmp = "float";
    else if (s.name == "string")
	tmp = "string";
    else if (s.name == "binary")
	tmp = "binary";
    else if (s.use == Struct::asEnum)
	tmp = to_enum_name(s.name);
    else if (s.use == Struct::asStruct)
	tmp = to_struct_name(true, s.name);
    else
	tmp = to_struct_name(false, s.name);
    return tmp;
}

static std::string getDefault(Struct const& s)
{
    if (s.name == "bool")
	return "false";
    else if (s.name == "int16")
	return "0";
    else if (s.name == "int32")
	return "0l";
    else if (s.name == "int64")
	return "0L";
    else if (s.name == "double")
	return "0.0";
    else if (s.name == "string")
	return "\"\"";
    else if (s.name == "binary")
	return "Bytes.empty";
    else if (s.use == Struct::asEnum)
	return "i_" + to_enum_name(s.name);
    else {
	assert(s.use == Struct::asStruct);
	return "i_" + to_struct_name(true, s.name);
    }
}

static std::string to_field_name(std::string const& fname)
{
    std::string tmp(fname);

    tmp[0] = tolower(tmp[0]);
    return '_' + tmp;
}

static void emitEnumCtor(Protocol const&, Field const& f, std::ostream& os)
{
    os << " | " << to_ctor(false, f.name) << '\n';
}

static void emitEnumMLine(Protocol const&, Field const& f, std::ostream& os)
{
    os << "  | " << to_ctor(false, f.name) << " -> ";
    emit_ConstInt(os, 0x80, f.hash);
    os << '\n';
}

static void emitEnumULine(Protocol const&, Field const& f, std::ostream& os)
{
    os <<
	"    | " << std::dec << f.hash << "L, s -> " <<
	to_ctor(false, f.name) << ", s\n";
}

struct field_state {
    std::string leader;
    bool first;
    bool stype;
    std::string name;
};

static void emitSField(Protocol const& p, Field const& f, std::ostream& os,
		       field_state& state)
{
    std::string const next = ";\n" + indent(state.leader.size());

    os <<
	(state.first ? state.first = false, '\n' + state.leader : next) <<
	to_field_name(f.name) <<
	" : " << xlatType(findStruct(p, f.type), f.array, f.optional);
}

// Emits the definition, marshaller and unmarshaller for an
// enumeration. The last parameter is an array of two bools. Before
// this function is called, the first bool needs to be set to
// 'false'. The second bool determines whether the marshalling
// functions should be emitted (i.e. in the header, the second bool is
// 'false'.

static void emitEnum(Protocol const& p, Struct const& s, std::ostream& os,
		     enum_state const& es)
{
    if (s.use == Struct::asEnum && s.isUsed(es.uTyp | es.mTyp)) {
	std::string const ename = to_enum_name(s.name);

	os << "type " << ename << " =\n";

	apply(p, os, s.fields.begin(), s.fields.end(), emitEnumCtor);

	os << '\n';

	if (es.flags[0]) {
	    if (s.isUsed(es.mTyp)) {
		os <<
		    "let m_" << ename << " f v =\n"
		    "  match v with\n";
		apply(p, os, s.fields.begin(), s.fields.end(), emitEnumMLine);
		os << "\n";
	    }

	    if (s.isUsed(es.uTyp)) {
		os <<
		    "let i_" << ename << " = " <<
		    to_ctor(false, s.fields.begin()->name) << "\n\n"
		    "let u_" << ename << " f s = \n"
		    "  match u_int 0x80 f s with\n";
		apply(p, os, s.fields.begin(), s.fields.end(), emitEnumULine);
		os <<
		    "    | _ -> raise (Protocol \"bad value for " <<
		    to_enum_name(s.name) << " enum\")\n\n";
	    }
	}
    }
}

static void dumpEnumerations(UseType const mTyp, UseType const uTyp,
			     Protocol const& p, std::ostream& os, bool iface)
{
    bool flag[] = { iface };
    enum_state es;

    es.flags = flag;
    es.mTyp = mTyp;
    es.uTyp = uTyp;

    apply(p, os, p.types.begin(), p.types.end(), emitEnum, es);
}

static size_t countOptionals(Struct const& s)
{
    size_t total = 0;
    FieldList::const_iterator jj;

    for (jj = s.fields.begin(); jj != s.fields.end(); ++jj)
	if (jj->optional)
	    ++total;
    return total;
}

static void emitNoneTest(Protocol const&, Field const& f, std::ostream& os,
			 std::pair<Struct const, std::string const> e)
{
    if (f.optional)
	os <<
	    e.second << "- (if v." <<
	    to_field_name(f.name) << " = None then 1 else 0)\n";
}

struct UFieldState {
    Struct const& s;
    std::string ii;
    size_t* count;
    size_t total;
};

static void emitFieldUnmarshaller(Protocol const& p, Field const& f,
				  std::ostream& os,
				  UFieldState const& e)
{
    std::string const fType = mName(findStruct(p, f.type));

    os << e.ii << "| " <<
	std::dec << std::setw(6) << std::setfill(' ') << f.hash <<
	", s ->\n"
	"let vfld, s = ";

    if (f.array)
	os << "u_array u_" << fType << " f s in\n";
    else
	os << "u_" << fType << " f s in\n";

    os <<
	"loop ";

    if (f.optional) {
	os << "m {" << (e.total > 1 ? "v with ": "") <<
	    to_field_name(f.name) <<
	    " = Some vfld}";
    } else {
	os << "(m lor " << (1 << (*e.count)++) << ") {" <<
	    (e.total > 1 ? "v with " : "") <<
	    to_field_name(f.name) <<
	    " = vfld}";
    }
    os << " s (n - 1)\n";
}

static void dumpStructMarshaller(UseType const mTyp, UseType const uTyp,
				 Protocol const& p, size_t const ii,
				 std::string const& name, Struct const& s,
				 std::ostream& os)
{
    if (s.fields.size() > 0) {
	size_t const nOpts = countOptionals(s);
	assert(s.fields.size() >= nOpts);
	size_t const nReq = s.fields.size() - nOpts;

	if (s.isUsed(mTyp)) {
	    os << indent(ii) << "let m_" << name << " f (v : " << name <<
		") s =\n";

	    if (nOpts > 0) {
		os <<
		    indent(ii + 2) << "let n = " << s.fields.size() << '\n';

		apply(p, os, s.fields.begin(), s.fields.end(), emitNoneTest,
		      std::make_pair(s, indent(ii + 4)));

		os <<
		    indent(ii + 2) << "in\n" <<
		    indent(ii + 2) <<
		    "let s = m_int 0x50 f (Int64.of_int (n * 2)) s in\n" <<
		    indent(ii + 2) << "s";
	    } else {
		os << indent(ii + 2);
		emit_ConstInt(os, 0x50, s.fields.size() * 2);
		os << " s";
	    }

	    for (FieldList::const_iterator ff = s.fields.begin();
		 ff != s.fields.end(); ++ff) {
		std::string const& fType = mName(findStruct(p, ff->type));

		os << " |>\n" << indent(ii + 2);

		if (ff->optional) {
		    os << "m_optional " << mk_constInt(0x10, ff->hash);
		    if (ff->array)
			os << " (m_array m_" << fType << ") f v." <<
			    to_field_name(ff->name);
		    else
			os << " m_" << fType << " f v." <<
			    to_field_name(ff->name);
		} else {
		    emit_ConstInt(os, 0x10, ff->hash);
		    os << " |>\n" << indent(ii + 2);
		    if (ff->array)
			os << "m_array m_" << fType << " f v." <<
			    to_field_name(ff->name);
		    else
			os << "m_" << fType << " f v." <<
			    to_field_name(ff->name);
		}
	    }

	    os << "\n\n";
	}

	if (s.isUsed(uTyp)) {
	    std::string const leader =
		indent(ii) + "let i_" + name + " = { ";
	    std::string const nonLeader = ";\n" + indent(leader.size());

	    for (FieldList::const_iterator jj = s.fields.begin();
		 jj != s.fields.end(); ++jj) {
		os << (jj == s.fields.begin() ? leader : nonLeader) <<
		    to_field_name(jj->name) << " = ";
		if (jj->optional)
		    os << "None";
		else if (jj->array)
		    os << "[||]";
		else
		    os << getDefault(findStruct(p, jj->type));
	    }
	    os <<
		" }\n\n" <<
		indent(ii) << "let u_" << name << " f s =\n" <<
		indent(ii + 2) << "let rec loop m (v : " << name <<
		") s = function\n" <<
		indent(ii + 4) << "| 0 ->\n" <<
		indent(ii + 6) << "if m = 0x" << std::hex <<
		((1 << nReq) - 1) << " then v, s\n" <<
		indent(ii + 6) << "else raise (Protocol \"missing required "
		"field in " <<
		name << "\")\n" <<
		indent(ii + 4) << "| n ->\n" <<
		indent(ii + 6) << "match u_int16 f s with\n";

	    size_t count = 0;
	    UFieldState ufs = { s, indent(ii + 7), &count, s.fields.size() };

	    apply(p, os, s.fields.begin(), s.fields.end(),
		  emitFieldUnmarshaller, ufs);

	    os <<
		indent(ii + 7) << "| _ -> raise (Protocol \"bad field tag "
		"in " << name << "\") in\n" <<
		indent(ii + 2) << "let nFlds, s = u_int 0x50 f s in\n" <<
		indent(ii + 2) << "let nFlds = Int64.to_int nFlds in\n" <<
		indent(ii + 2) << "if nFlds mod 2 = 0 then\n" <<
		indent(ii + 4) << "if nFlds >= " << (nReq * 2) << " then\n" <<
		indent(ii + 6) << "loop 0 i_" << name << " s (nFlds / 2)\n" <<
		indent(ii + 4) << "else\n" <<
		indent(ii + 6) << "raise (Protocol \"not enough fields "
		"defined in " <<
		name << "\")\n" <<
		indent(ii + 2) << "else\n" <<
		indent(ii + 4) << "raise (Protocol \"nonsensical field "
		"count for " <<
		name << "\")\n";
	}
    }
}

static void dumpStructures(UseType const mTyp, UseType const uTyp,
			   Protocol const& p, StructList const& sl,
			   std::ostream& os, bool const iface,
			   size_t const ind)
{
    StructList::const_iterator ss;

    for (ss = sl.begin(); ss != sl.end(); ++ss) {
	if (ss->fields.size() > 0 &&
	    (ss->use == Struct::asStruct || ss->use == Struct::asRequest ||
	     ss->use == Struct::asReply) && ss->isUsed(uTyp | mTyp)) {
	    std::string const sname =
		to_struct_name(ss->use == Struct::asStruct, ss->name);
	    field_state fs;

	    os <<
		indent(ind) << "(* Define '" << ss->name << "'. *)\n";

	    fs.leader = indent(ind) + "type " + sname + " = { ";
	    fs.first = true;
	    fs.stype = ss->use == Struct::asStruct;
	    fs.name = ss->name;

	    apply(p, os, ss->fields.begin(), ss->fields.end(), emitSField, fs);
	    os << " }\n\n";

	    if (iface) {
		os <<
		    indent(ind) <<
		    "(* Define the marshal/unmarshal functions for '" <<
		    ss->name << "'. *)\n"
		    "\n";
		dumpStructMarshaller(mTyp, uTyp, p, ind, sname, *ss, os);
		os << '\n';
	    }
	}
    }
}

static void dumpCTors(StructList const& sl, std::ostream& os, size_t const ind)
{
    bool first = true;
    std::string const lead = "\n" + indent(ind) + "| ";

    for (StructList::const_iterator ii = sl.begin(); ii != sl.end(); ++ii)
	if (ii->use == Struct::asRequest || ii->use == Struct::asReply) {
	    if (!first)
		os << lead;
	    os << to_ctor(ocamlPolyVT, ii->name);
	    if (ii->fields.size() > 0)
		os << " of " <<
		    to_struct_name(ii->use == Struct::asStruct, ii->name);
	    first = false;
	}
}

static void dumpMarshal(Protocol const& p, StructList const& sl,
			std::ostream& os, size_t const ind)
{
    os << indent(ind) << "let marshal f s =\n" <<
	indent(ind + 2) << "let hdr s =\n" << indent(ind + 4) <<
	"emit_str f \"SDD\\x02\" s |>\n" << indent(ind + 4);
    emit_ConstInt(os, 0x50, 3);
    os << " |>\n" << indent(ind + 4);
    emit_ConstInt(os, 0x10, p.longHash(p.name));
    os << '\n' << indent(ind + 2) << "in\n" <<
	indent(ind + 4) << "function\n";

    std::string const lead = indent(ind + 4) + "| ";

    for (StructList::const_iterator ii = sl.begin(); ii != sl.end(); ++ii)
	if (ii->use == Struct::asRequest || ii->use == Struct::asReply) {
	    os << lead << to_ctor(ocamlPolyVT, ii->name);
	    if (ii->fields.size() > 0) {
		os << " v ->\n" << indent(ind + 8) <<
		    "hdr s |>\n" << indent(ind + 8);
		emit_ConstInt(os, 0x10, ii->hash);
		os << " |>\n" << indent(ind + 8) <<
		    "m_" <<
		    to_struct_name(ii->use == Struct::asStruct, ii->name) <<
		    " f v\n";
	    } else {
		os << " ->\n" << indent(ind + 8) <<
		    "hdr s |>\n" << indent(ind + 8);
		emit_ConstInt(os, 0x10, ii->hash);
		os << " |>\n" << indent(ind + 8);
		emit_ConstInt(os, 0x50, 0);
		os << '\n';
	    }
	}
    os << "\n";
}

static void dumpUnmarshal(Protocol const& p, StructList const& sl,
			  std::ostream& os, size_t const ind)
{
    os << indent(ind) << "let unmarshal f s =\n" <<
	indent(ind + 2) << "let u_val s =\n" <<
	indent(ind + 4) << "match u_int16 f s with\n";

    std::string const lead = indent(ind + 5) + "| ";

    for (StructList::const_iterator ii = sl.begin(); ii != sl.end(); ++ii)
	if (ii->use == Struct::asRequest || ii->use == Struct::asReply) {
	    os << lead << std::dec << std::setfill(' ') << ii->hash <<
		", s ->\n";

	    if (ii->fields.size() == 0)
		os << indent(ind + 9) <<
		    "begin match u_nint 0x50 f s with\n" <<
		    indent(ind + 9) << "| 0, s -> " <<
		    to_ctor(ocamlPolyVT, ii->name) << ", s\n" <<
		    indent(ind + 9) << "| _ -> " <<
		    "raise (Protocol \"message '" << ii->name <<
		    "' has no fields\")\n" <<
		    indent(ind + 9) << "end";
	    else
		os << indent(ind + 9) << "let v, s = u_" <<
		    to_struct_name(ii->use == Struct::asStruct, ii->name) <<
		    " f s in\n" << indent(ind + 9) <<
		    to_ctor(ocamlPolyVT, ii->name) << " v, s";

	    os << "\n";
	}
    os <<
	lead << "_ ->\n" <<
	indent(ind + 9) << "raise (Protocol \"unknown message\")\n" <<
	indent(ind + 2) <<
	"in\n" <<
	indent(ind + 2) <<
	"verify \"SDD\\x02\" \"bad header\" f s |>\n" <<
	indent(ind + 2) <<
	"verify " << mk_constInt(0x50, 3) << " \"bad header\" f |>\n" <<
	indent(ind + 2) <<
	"verify " << mk_constInt(0x10, p.longHash(p.name)) <<
	" \"bad protocol\" f |>\n" <<
	indent(ind + 2) <<
	"u_val\n\n";
}

static void dumpHeaderFile(Protocol const& p, std::ostream& os)
{
    UseType const mTyp =
	((target & Client) ? InRequest : Nowhere) |
	((target & Server) ? InReply : Nowhere);
    UseType const uTyp =
	((target & Server) ? InRequest : Nowhere) |
	((target & Client) ? InReply : Nowhere);

    os <<
	"exception Protocol of string\n\n";

    dumpEnumerations(mTyp, uTyp, p, os, false);
    dumpStructures(mTyp, uTyp, p, p.types, os, false, 0);

    if (!p.requests.empty()) {
	os <<
	    "module Request :\n"
	    "sig\n";

	dumpStructures(mTyp, uTyp, p, p.requests, os, false, 2);

	if (ocamlPolyVT) {
	    os << "  type t = [ ";
	    dumpCTors(p.requests, os, 11);
	    os << " ]\n\n";
	} else {
	    os << "  type t = ";
	    dumpCTors(p.requests, os, 9);
	    os << "\n\n";
	}

	if (target & Client)
	    os << "  val marshal : (char -> 'a -> 'a) -> 'a -> " <<
		(ocamlPolyVT ? "[< t]" : "t") << " -> 'a\n";

	if (target & Server)
	    os << "  val unmarshal : ('a -> (char * 'a)) -> 'a -> (" <<
		(ocamlPolyVT ? "[> t]" : "t") << " * 'a)\n";

	os <<
	    "end\n\n";
    }

    if (!p.replies.empty()) {
	os <<
	    "module Reply :\n"
	    "sig\n";

	dumpStructures(mTyp, uTyp, p, p.replies, os, false, 2);

	if (ocamlPolyVT) {
	    os << "  type t = [ ";
	    dumpCTors(p.replies, os, 11);
	    os << " ]\n\n";
	} else {
	    os << "  type t = ";
	    dumpCTors(p.replies, os, 9);
	    os << "\n\n";
	}

	if (target & Server)
	    os << "  val marshal : (char -> 'a -> 'a) -> 'a -> " <<
		(ocamlPolyVT ? "[< t]" : "t") << " -> 'a\n";

	if (target & Client)
	    os << "  val unmarshal : ('a -> (char * 'a)) -> 'a -> (" <<
		(ocamlPolyVT ? "[> t]" : "t") << " * 'a)\n";

	os <<
	    "end\n";
    }
}

static void dumpSourceFile(Protocol const& p, std::ostream& os)
{
    UseType const mTyp =
	((target & Client) ? InRequest : Nowhere) |
	((target & Server) ? InReply : Nowhere);
    UseType const uTyp =
	((target & Server) ? InRequest : Nowhere) |
	((target & Client) ? InReply : Nowhere);

    os <<
	"exception Protocol of string\n\n"
	"(* Define a set of constants for testing the range of "
	"integers. *)\n\n"
	"let max_nint = Int64.of_int max_int\n"
	"and min_nint = Int64.of_int min_int\n"
	"and min_int32 = -0x80000000L\n"
	"and max_int32 = 0x7fffffffL\n"
	"and min_int16 = -0x8000L\n"
	"and max_int16 = 0x7fffL\n"
	"and min_int8 = -0x80L\n"
	"and max_int8 = 0x7fL\n"
	"\n";

    // All marshal functions should have the following signature:
    //
    // type 'a marshal_func = (char -> 'b -> 'b) -> 'a -> 'b -> 'b

    if (p.needed(mTyp, "bool"))
	os <<
	    "let m_bool f v =\n"
	    "  f (if v then '\\x71' else '\\x70')\n\n";

    os <<
	"let m_int t f v s =\n"
	"  let rec emit n s =\n"
	"    if n > 0 then\n"
	"      let v' = Int64.logand (Int64.shift_right v (n * 8 - 8)) 255L in\n"
	"      (f (Char.chr (Int64.to_int v')) s |> emit (n - 1))\n"
	"    else s in\n"
	"  let emit_tag n =\n"
	"    f (Char.chr (t lor n)) s |> emit n\n"
	"  in\n"
	"  if v >= min_int8 && v <= max_int8 then\n"
	"    emit_tag 1\n"
	"  else if v >= min_int16 && v <= max_int16 then\n"
	"    emit_tag 2\n"
	"  else if v >= min_int32 && v <= max_int32 then\n"
	"    emit_tag 4\n"
	"  else\n"
	"    emit_tag 8\n\n";

    if (p.needed(mTyp, "int16"))
	os <<
	    "let m_int16 f v =\n"
	    "  let v' = Int64.of_int v in\n"
	    "  if v' >= min_int16 && v' <= max_int16 then\n"
	    "    m_int 0x10 f v'\n"
	    "  else\n"
	    "    raise (Protocol \"int16 value out of range\")\n\n";

    if (p.needed(mTyp, "int32"))
	os <<
	    "let m_int32 f v =\n"
	    "  let v' = Int64.of_int32 v in\n"
	    "  if v' >= min_int32 && v' <= max_int32 then\n"
	    "    m_int 0x10 f v'\n"
	    "  else\n"
	    "    raise (Protocol \"int32 value out of range\")\n\n";

    if (p.needed(mTyp, "int64"))
	os <<
	    "let m_int64 f = m_int 0x10 f\n\n";

    if (p.needed(mTyp, "double"))
	os <<
	    "let m_float f v s =\n"
	    "  let v = Int64.bits_of_float v in\n"
	    "  let rec emit n s =\n"
	    "    if n > 0 then\n"
	    "      let v' = Int64.logand (Int64.shift_right v (n * 8 - 8)) 255L in\n"
	    "      (f (Char.chr (Int64.to_int v')) s |> emit (n - 1))\n"
	    "    else s in\n"
	    "  f '\\x28' s |> emit 8\n\n";

    os <<
	"let emit_str f v =\n"
	"  let len = String.length v in\n"
	"  let rec loop n s =\n"
	"    if n < len then\n"
	"      (f v.[n] s |> loop (n + 1))\n"
	"    else\n"
	"      s in\n"
	"  loop 0\n\n";

    if (p.needed(mTyp, "string"))
	os <<
	    "let m_string f v s =\n"
	    "  let n = String.length v in\n"
	    "  m_int 0x40 f (Int64.of_int n) s |> emit_str f v\n\n";

    if (p.needed(mTyp, "binary"))
	os <<
	    "let m_binary f v s =\n"
	    "  let n = Bytes.length v in\n"
	    "  m_int 0x30 f (Int64.of_int n) s |>\n"
	    "  emit_str f (Bytes.unsafe_to_string v)\n\n";

    if (p.anyArrays(mTyp))
	os <<
	    "let m_array mf f v s =\n"
	    "  let n = Array.length v in\n"
	    "  let rec contents i s =\n"
	    "    if i < n then\n"
	    "      mf f v.(i) s |> contents (i + 1)\n"
	    "    else s in\n"
	    "  m_int 0x50 f (Int64.of_int n) s |>\n"
	    "  contents 0\n\n";

    if (p.anyOptionals())
	os <<
	    "let m_optional nm mf f v s =\n"
	    "  match v with\n"
	    "  | None -> s\n"
	    "  | Some v ->\n"
	    "    emit_str f nm s |> mf f v\n\n";

    if (p.needed(uTyp, "bool"))
	os <<
	    "let u_bool f s =\n"
	    "  match f s with\n"
	    "  | '\\x70', s' -> false, s'\n"
	    "  | '\\x71', s' -> true, s'\n"
	    "  | _ -> raise (Protocol \"expected boolean tag\")\n"
	    "\n";

    os <<
	"(* Pulls an integer from a stream. The integer must be marked with the\n"
	"   tag 't'. *)\n"
	"\n"
	"let u_int t f s =\n"
	"  let nint s = let v, s' = f s in\n"
	"               Char.code v, s' in\n"
	"  let rec loop v s = function\n"
	"    | 0 -> v, s\n"
	"    | n ->\n"
	"      let v' = Int64.shift_left v 8\n"
	"      and v'', s = nint s in\n"
	"      loop (Int64.logor v' (Int64.of_int v'')) s (n - 1) in\n"
	"  let t', s = nint s in\n"
	"  let n = t' land 0xf in\n"
	"  if ((t' land 0xf0) = t) && (n > 0) && (n <= 8) then\n"
	"    let fv, s = nint s in\n"
	"    let fv = Int64.of_int fv in\n"
	"    loop (if fv > max_int8 then Int64.sub fv 256L else fv) s (n - 1)\n"
	"  else\n"
	"    raise (Protocol \"unexpected tag\")\n\n";

    if (p.needed(uTyp, "int64"))
	os <<
	    "let u_int64 f = u_int 0x10 f\n\n";

    if (p.needed(uTyp, "int32"))
	os <<
	    "let u_int32 f s =\n"
	    "  let v, s = u_int 0x10 f s in\n"
	    "  if v >= min_int32 && v <= max_int32 then\n"
	    "    Int64.to_int32 v, s\n"
	    "  else\n"
	    "    raise (Protocol \"int32 field is out of range\")\n\n";

    os <<
	"let u_nint t f s =\n"
	"  let v, s = u_int t f s in\n"
	"  if v >= min_nint && v <= max_nint then\n"
	"    Int64.to_int v, s\n"
	"  else\n"
	"    raise (Protocol \"length field is out of range\")\n\n";

    os <<
	"let u_int16 f s =\n"
	"  let v, s = u_int 0x10 f s in\n"
	"  if v >= min_int16 && v <= max_int16 then\n"
	"    Int64.to_int v, s\n"
	"  else\n"
	"    raise (Protocol \"int16 field is out of range\")\n\n";

    if (p.needed(uTyp, "double"))
	os <<
	    "let u_float f s =\n"
	    "  let v, s = u_int 0x20 f s in\n"
	    "  Int64.float_of_bits v, s\n\n";

    os <<
	"let get_bytes len f s =\n"
	"  let buf = Bytes.create len in\n"
	    "  let rec loop s = function\n"
	    "    | n when n < len ->\n"
	    "        let c, s = f s in\n"
	    "        begin\n"
	    "          Bytes.set buf n c;\n"
	    "          loop s (n + 1)\n"
	    "        end\n"
	    "    | _ -> buf, s\n"
	    "  in\n"
	    "  loop s 0\n\n";

    os <<
	"let verify str err_msg f s =\n"
	"  let buf, s = get_bytes (String.length str) f s in\n"
	"  if Bytes.unsafe_to_string buf = str then\n"
	"    s\n"
	"  else\n"
	"    raise (Protocol err_msg)\n\n";

    if (p.needed(uTyp, "binary"))
	os <<
	    "let u_binary f s =\n"
	    "  let len, s = u_nint 0x30 f s in\n"
	    "  get_bytes len f s\n\n";

    if (p.needed(uTyp, "string"))
	os <<
	    "let u_string f s =\n"
	    "  let len, s = u_nint 0x40 f s in\n"
	    "  let buf, s = get_bytes len f s in\n"
	    "  Bytes.unsafe_to_string buf, s\n\n";

    if (p.anyArrays(uTyp))
	os <<
	    "let u_array uf f s =\n"
	    "  let len, s = u_nint 0x50 f s in\n"
	    "  let tmp = ref s in\n"
	    "  let init _ = let v, s' = uf f !tmp in\n"
	    "               begin\n"
	    "                 tmp := s';\n"
	    "                 v\n"
	    "               end\n"
	    "  in\n"
	    "  Array.init len init, !tmp\n\n";

    dumpEnumerations(mTyp, uTyp, p, os, true);
    dumpStructures(mTyp, uTyp, p, p.types, os, true, 0);

    if (!p.requests.empty()) {
	os <<
	    "module Request =\n"
	    "struct\n";

	dumpStructures(mTyp, uTyp, p, p.requests, os, true, 2);

	if (ocamlPolyVT) {
	    os << "  type t = [ ";
	    dumpCTors(p.requests, os, 11);
	    os << " ]\n\n";
	} else {
	    os << "  type t = ";
	    dumpCTors(p.requests, os, 9);
	    os << "\n\n";
	}

	if (target & Client)
	    dumpMarshal(p, p.requests, os, 2);
	if (target & Server)
	    dumpUnmarshal(p, p.requests, os, 2);

	os <<
	    "end\n\n";
    }

    if (!p.replies.empty()) {
	os <<
	    "module Reply =\n"
	    "struct\n";

	dumpStructures(mTyp, uTyp, p, p.replies, os, true, 2);

	if (ocamlPolyVT) {
	    os << "  type t = [ ";
	    dumpCTors(p.replies, os, 11);
	    os << " ]\n\n";
	} else {
	    os << "  type t = ";
	    dumpCTors(p.replies, os, 9);
	    os << "\n\n";
	}

	if (target & Server)
	    dumpMarshal(p, p.replies, os, 2);
	if (target & Client)
	    dumpUnmarshal(p, p.replies, os, 2);

	os <<
	    "end\n";
    }
}

// This function generates the .ml and .mli files for a single
// protocol. It does this by opening the two output files and then
// calling the two functions that actually generate the content for
// each file type.

static void dumpProtocol(Protocol const& p)
{
    std::string hdrName = targetPath + "/" + toLower(p.name) + "_protocol.mli";

    // Open the two files.

    std::ofstream hdr(hdrName.c_str());
    std::ofstream src((targetPath + "/" + toLower(p.name) + "_protocol.ml").c_str());

    hdr <<
	"(* Generated by the protocol compiler version " << pcVersion << ".\n"
	"   DO NOT EDIT THIS FILE DIRECTLY! *)\n\n";
    dumpHeaderFile(p, hdr);

    src <<
	"(* Generated by the protocol compiler version " << pcVersion << ".\n"
	"   DO NOT EDIT THIS FILE DIRECTLY! *)\n\n";
    dumpSourceFile(p, src);
}

// The only externally visible function from this module. This is the
// entry point to generate OCaml files that handle the protocol.

void generateOCaml(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("OCaml generator only supports SDDv2");

    dumpProtocol(p);
}
