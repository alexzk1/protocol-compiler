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

// This module generates a JavaScript file that can communicate using
// a specified protocol. The .proto grammar gets mapped to JavaScript
// in the following way:
//
//    bool       -> boolean
//    int16      -> Number
//    int32      -> Number
//    int64      -> Number
//    double     -> Number
//    string     -> String
//    binary     -> ArrayBuffer
//    array of T -> Array
//    enum T     -> nested Object
//    T          -> nested Object
//    optional T -> undefined or data

#include <iostream>
#include <fstream>
#include <iomanip>
#include <assert.h>
#include <sstream>
#include "pc.h"

static std::string xlat_kw(std::string const& name)
{
    if (name == "arguments")
	return "fldArguments";
    else
	return name;
}

static void emitJsLibrary()
{
    std::ofstream os("proto_lib.js");

    os.write(reinterpret_cast<char const*>(js_lib), js_lib_size);
}

static std::string toJsArray(std::string const& s)
{
    std::ostringstream os;

    os << '[';
    for (std::string::const_iterator ii = s.begin(); ii != s.end(); ++ii) {
	if (ii != s.begin())
	    os << ", ";
	os << std::dec << (int) (uint8_t) *ii;
    }
    os << ']';
    return os.str();
}

static bool dumpEnum(Protocol const&, Struct const& s, std::ostream& os,
		     std::string const& sep)
{
    if (s.isUserDefined() /* && s.isUsed() */)
	if (s.use == Struct::asEnum) {
	    os << sep << "    \"" << s.name << "\": Object.freeze({";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii)
		os <<
		    (ii == s.fields.begin() ? "\n\t\"" : ",\n\t\"") <<
		    xlat_kw(ii->name) << "\": " << std::dec << ii->hash;

	    os << " })";
	    return true;
	}
    return false;
}

static void dumpEnumM(Protocol const& p, Struct const& s, std::ostream& os)
{
    if (s.isUserDefined() /* && s.isUsed() */)
	if (s.use == Struct::asEnum) {
	    os <<
		"\n" << toUpper(p.name) << "_PROTO.m_enum_" << s.name <<
		" = function* (v) {\n"
		"    switch (v) {\n";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii)
		os << "     case " << std::dec << ii->hash << ":\n";

	    os <<
		"\tyield* PROTOCOL.m_enum(v);\n"
		"\tbreak;\n"
		"     default:\n"
		"\tthrow new RangeError(\"Bad value for " <<
		xlat_kw(s.name) <<
		" enum\");\n"
		"    }\n"
		"};\n";
	}
}

static void dumpEnumU(Protocol const& p, Struct const& s, std::ostream& os)
{
    if (s.isUserDefined() /* && s.isUsed() */)
	if (s.use == Struct::asEnum) {
	    os <<
		"\n" << toUpper(p.name) << "_PROTO.u_enum_" << s.name <<
		" = function (iter) {\n"
		"    const v = PROTOCOL.u_tagged_int(0x80, iter);\n\n"
		"    switch (v) {\n";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii)
		os << "     case " << std::dec << ii->hash << ":\n";

	    os <<
		"\treturn v;\n"
		"     default:\n"
		"\tthrow new TypeError(\"expecting " <<
		xlat_kw(s.name) <<
		" enum\");\n"
		"    }\n"
		"};\n";
	}
}

static std::string structName(Protocol const& p, Struct const& s)
{
    std::string t;

    switch (s.use) {
     case Struct::asRequest:	return toUpper(p.name) + "_request_" + s.name;
     case Struct::asReply:	return toUpper(p.name) + "_reply_" + s.name;
     case Struct::asStruct:	return toUpper(p.name) + "_struct_" + s.name;
     default:
	throw std::logic_error("structName called with non-structure type");
    }

    return toUpper(p.name) + "_struct_" + s.name;
}

static std::string getMFunc(Protocol const& p, std::string const& t)
{
    std::string const objName = toUpper(p.name) + "_PROTO";
    Struct const& s = p.findType(t);

    if (s.name == "int16" || s.name == "int32" || s.name == "int64")
	return "PROTOCOL.m_int";
    else if (s.name == "double")
	return "PROTOCOL.m_float";
    else if (s.name == "bool")
	return "PROTOCOL.m_bool";
    else if (s.name == "string")
	return "PROTOCOL.m_string";
    else if (s.name == "binary")
	return "PROTOCOL.m_binary";
    else if (s.use == Struct::asEnum)
	return objName + ".m_enum_" + s.name;
    else
	return "PROTOCOL.m_struct";
}

static std::string getUFunc(Protocol const& p, std::string const& t)
{
    std::string const objName = toUpper(p.name) + "_PROTO";
    Struct const& s = p.findType(t);

    if (s.name == "int16" || s.name == "int32" || s.name == "int64")
	return "PROTOCOL.u_int";
    else if (s.name == "double")
	return "PROTOCOL.u_float";
    else if (s.name == "bool")
	return "PROTOCOL.u_bool";
    else if (s.name == "string")
	return "PROTOCOL.u_string";
    else if (s.name == "binary")
	return "PROTOCOL.u_binary";
    else if (s.use == Struct::asEnum)
	return objName + ".u_enum_" + s.name;
    else if (s.use == Struct::asStruct)
	return objName + ".u_struct_" + s.name;
    else
	throw std::logic_error("getUFunc() given an unmarshallable type");
}

static void dumpStructCtor(Protocol const& p, Struct const& s,
			   std::ostream& os)
{
    std::string const sname = structName(p, s);

    os << "\nexport class " << sname << " {\n"
	"    constructor() {\n";

    for (FieldList::const_iterator ii = s.fields.begin();
	 ii != s.fields.end(); ++ii)
	if (!ii->optional) {
	    os << "\tthis." << xlat_kw(ii->name) << " = ";

	    if (ii->array)
		os << "[]";
	    else if (ii->type == "string")
		os << "\"\"";
	    else if (ii->type == "int16" || ii->type == "int32" ||
		     ii->type == "int64" || ii->type == "double")
		os << "0";
	    else if (ii->type == "binary")
		os << "new ArrayBuffer()";
	    else if (ii->type == "bool")
		os << "false";
	    else if (isEnum(p, ii->type))
		os << "undefined";
	    else
		os << "new " << structName(p, p.findType(ii->type)) << "()";
	    os << ";\n";
	}

    os <<
	"    };\n";
}

static void dumpStructMarshaller(Protocol const& p, Struct const& s,
				 std::ostream& os)
{
    std::string const sname = structName(p, s);
    std::ostringstream hs;

    if (((target & Server) != Server && s.use == Struct::asReply) ||
	((target & Client) != Client && s.use == Struct::asRequest)) {
	os <<
	    "};\n";
	return;
    }

    if (s.use == Struct::asRequest || s.use == Struct::asReply) {
	hs << "SDD\x02\x51\x03";
	emitRawInt(hs, 0x10, p.longHash(p.name));
	emitRawInt(hs, 0x10, s.hash);
    }

    os << "\n    *marshal() {\n";

    size_t const required = countRequired(s);

    if (required == s.fields.size()) {
	emitRawInt(hs, 0x50, (required * 2));
	os << "\tyield* " << toJsArray(hs.str()) << ";\n";
    } else {
	os <<
	    "\tconst nullFields =\n"
	    "\t\t";

	bool first = true;

	for (FieldList::const_iterator ii = s.fields.begin();
	     ii != s.fields.end(); ++ii)
	    if (ii->optional) {
		if (!first)
		    os << "\n\t    + ";
		os << "(this." << xlat_kw(ii->name) <<
		    " === undefined ? 2 : 0)";
		first = false;
	    }

	os <<
	    ";\n\n";

	if (hs.str() != "")
	    os <<
		"\tyield* " << toJsArray(hs.str()) << ";\n";

	os <<
	    "\tyield* PROTOCOL.m_tagged_int(0x50, " <<
	    (s.fields.size() * 2) << " - nullFields);\n";
    }

    for (FieldList::const_iterator ii = s.fields.begin();
	 ii != s.fields.end(); ++ii) {
	if (ii->optional)
	    os << "\tif (this." << xlat_kw(ii->name) <<
		" !== undefined) {\n";

	os << (ii->optional ? "\t    " : "\t");

	std::ostringstream fs;

	emitRawInt(fs, 0x10, ii->hash);
	os << "yield* " << toJsArray(fs.str()) << ";\n";

	os << (ii->optional ? "\t    " : "\t");

	if (ii->array)
	    os << "yield* PROTOCOL.m_array(" << getMFunc(p, ii->type) <<
		", this." << xlat_kw(ii->name) << ");\n";
	else
	    os << "yield* " << getMFunc(p, ii->type) << "(this." <<
		xlat_kw(ii->name) << ");\n";

	if (ii->optional)
	    os << "\t}\n";
    }

    os <<
	"    };\n"
	"};\n";
}

static void dumpStructObject(Protocol const& p, Struct const& s,
			     std::ostream& os)
{
    if (s.isUserDefined() /* && s.isUsed() */)
	switch (s.use) {
	 case Struct::asRequest:
	 case Struct::asReply:
	 case Struct::asStruct:
	    dumpStructCtor(p, s, os);
	    dumpStructMarshaller(p, s, os);
	 default:
	    break;
	}
}

static void dumpStructUnmarshaller(Protocol const& p, Struct const& s,
				   std::ostream& os)
{
    if (s.isUserDefined() /* && s.isUsed() */) {
	if (((target & Client) != Client && s.use == Struct::asReply) ||
	    ((target & Server) != Server && s.use == Struct::asRequest))
	    return;

	std::string iname = "v";

	switch (s.use) {
	 case Struct::asRequest:
	 case Struct::asReply:
	    if (jsSingletons) {
		std::string const sname = structName(p, s);

		iname = "glb" + sname;
		os << "\nconst " << iname << " = new " << sname << "();\n";
	    }
        // fall through    
	 case Struct::asStruct:
	    {
		std::string const sname = structName(p, s);

		os << "\n" << toUpper(p.name) << "_PROTO.u_";

		if (s.use == Struct::asRequest)
		    os << "request";
		else if (s.use == Struct::asReply)
		    os << "reply";
		else
		    os << "struct";

		os << '_' <<
		    s.name << " = function (iter) {\n";

		if (s.fields.size() == 0) {
		    os <<
			"    if (PROTOCOL.u_tagged_int(0x50, iter) !== 0)\n"
			"\tthrow new Error(\"unknown field when building " <<
			structName(p, s) << "\");\n";

		    if (!jsSingletons || s.use == Struct::asStruct)
			os << "    return new " << sname << "();\n";
		    else
			os << "    return " << iname << ";\n";

		    os <<
			"};\n";
		    return;
		}

		size_t const required = countRequired(s);

		if (required == 1 && s.fields.size() == 1) {
		    FieldList::const_iterator ii = s.fields.begin();

		    if (!jsSingletons || s.use == Struct::asStruct)
			os << "    const v = new " << sname << "();\n\n";

		    os <<
			"    if (PROTOCOL.u_tagged_int(0x50, iter) !== 2)\n"
			"\tthrow new Error(\"required fields missing when "
			"building " << structName(p, s) << "\");\n"
			"    if (PROTOCOL.u_tagged_int(0x10, iter) !== " <<
			ii->hash << ")\n"
			"\tthrow new Error(\"unknown field when building " <<
			structName(p, s) << "\");\n"
			"    " << iname << "." << xlat_kw(ii->name) << " = ";

		    if (ii->array)
			os << "PROTOCOL.u_array(" << getUFunc(p, ii->type) <<
			    ", iter";
		    else
			os << getUFunc(p, ii->type) << "(iter";

		    os <<
			");\n"
			"    return " << iname << ";\n"
			"}\n";
		    return;
		}

		size_t currentBit = 0;

		if (!jsSingletons || s.use == Struct::asStruct)
		    os << "    const v = new " << sname << "();\n";
		else {
		    for (FieldList::const_iterator ii = s.fields.begin();
			 ii != s.fields.end(); ++ii)
			if (ii->optional)
			    os << "    delete " << iname << "." <<
				xlat_kw(ii->name) << ";\n\n";
		}

		os <<
		    "    const fflg = new Uint8Array(" << ((required + 7) / 8) <<
		    ");\n"
		    "    var nFlds = PROTOCOL.u_tagged_int(0x50, iter);\n\n"
		    "    while (nFlds > 0) {\n"
		    "\tswitch (PROTOCOL.u_tagged_int(0x10, iter)) {\n";

		for (FieldList::const_iterator ii = s.fields.begin();
		     ii != s.fields.end(); ++ii) {
		    os <<
			"\t case " << ii->hash << ":\n"
			"\t    " << iname << "." << xlat_kw(ii->name) << " = ";

		    if (ii->array)
			os << "PROTOCOL.u_array(" << getUFunc(p, ii->type) <<
			    ", iter";
		    else
			os << getUFunc(p, ii->type) << "(iter";

		    os << ");\n";

		    if (!ii->optional) {
			os << "\t    fflg[" << (currentBit / 8) << "] |= " <<
			    (1 << (currentBit % 8)) << ";\n";
			++currentBit;
		    }

		    os << "\t    break;\n";
		}

		os <<
		    "\t default:\n"
		    "\t    throw new Error(\"unknown field when building " <<
		    structName(p, s) << "\");\n"
		    "\t}\n"
		    "\tnFlds -= 2;\n"
		    "    }\n";

		if (required > 0) {
		    os << "    if (";

		    for (size_t ii = 0; ii < (required + 7) / 8; ++ii) {
			if (ii > 0)
			    os << " || ";
			os << "fflg[" << ii << "] !== " <<
			    ((ii + 1) * 8 < required ? 255 :
			     255 >> ((ii + 1) * 8 - required));
		    }

		    os <<
			")\n"
			"\tthrow new Error(\"required fields missing when "
			"building " << structName(p, s) << "\");\n";
		}
		os <<
		    "    return " << iname << ";\n"
		    "};\n";
	    }
	 default:
	    break;
	}
    }
}

static void dumpSourceFile(std::ofstream& os, Protocol const& p)
{
    std::string const objName = toUpper(p.name) + "_PROTO";

    os <<
	"import { PROTOCOL } from '@fnal/proto_lib';\n\n"
	"export const " << objName << " = {};\n\n"
	"// Define enumerations of the protocol.\n\n" <<
	objName << ".enum = {\n";
    apply_sep(p, os, p.types.begin(), p.types.end(), dumpEnum, ",\n\n");
    os << "\n};\n";
    apply(p, os, p.types.begin(), p.types.end(), dumpEnumM);
    apply(p, os, p.types.begin(), p.types.end(), dumpEnumU);

    os << "\n// Define user-defined structures of the protocol.\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpStructObject);
    apply(p, os, p.types.begin(), p.types.end(), dumpStructUnmarshaller);

    os << "\n// Define requests of the protocol.\n";

    apply(p, os, p.requests.begin(), p.requests.end(), dumpStructObject);
    apply(p, os, p.requests.begin(), p.requests.end(),
	  dumpStructUnmarshaller);

    os << "\n// Define replies of the protocol.\n";

    apply(p, os, p.replies.begin(), p.replies.end(), dumpStructObject);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpStructUnmarshaller);

    if ((target & Server) == Server) {
	os <<
	    "\n" <<
	    objName << ".unmarshal_request = function (iter) {\n"
	    "    PROTOCOL.validate_header(2, iter);\n"
	    "    if (PROTOCOL.u_tagged_int(0x50, iter) !== 3)\n"
	    "\tthrow new Error(\"badly formed message header\");\n"
	    "    if (PROTOCOL.u_tagged_int(0x10, iter) !== " <<
	    p.longHash(p.name) <<
	    ")\n"
	    "\tthrow new Error(\"unknown protocol type\");\n"
	    "    switch (PROTOCOL.u_tagged_int(0x10, iter)) {\n";

	for (StructList::const_iterator ii = p.requests.begin();
	     ii != p.requests.end(); ++ii) {
	    os <<
		"     case " << ii->hash << ":\n"
		"\treturn " << objName << ".u_request_" << ii->name <<
		"(iter);\n";
	}

	os <<
	    "     default:\n"
	    "\tthrow new Error(\"unknown request in protocol\");\n"
	    "    }\n"
	    "}\n";
    }

    if ((target & Client) == Client) {
	os <<
	    "\n" <<
	    objName << ".unmarshal_reply = function (iter) {\n"
	    "    PROTOCOL.validate_header(2, iter);\n"
	    "    if (PROTOCOL.u_tagged_int(0x50, iter) !== 3)\n"
	    "\tthrow new Error(\"badly formed message header\");\n"
	    "    if (PROTOCOL.u_tagged_int(0x10, iter) !== " <<
	    p.longHash(p.name) <<
	    ")\n"
	    "\tthrow new Error(\"unknown protocol type\");\n"
	    "    switch (PROTOCOL.u_tagged_int(0x10, iter)) {\n";

	for (StructList::const_iterator ii = p.replies.begin();
	     ii != p.replies.end(); ++ii) {
	    os <<
		"     case " << ii->hash << ":\n"
		"\treturn " << objName << ".u_reply_" << ii->name <<
		"(iter);\n";
	}

	os <<
	    "     default:\n"
	    "\tthrow new Error(\"unknown reply in protocol\");\n"
	    "    }\n"
	    "}\n";
    }
}

static std::string toJsType(Protocol const& p, Field const& f)
{
    std::string result;

    if (f.array)
	result = "Array<";

    Struct const& s = p.findType(f.type);

    if (s.name == "int16" || s.name == "int32" ||
	s.name == "int64" || s.name == "double")
	result += "number";
    else if (s.name == "binary")
	result += "ArrayBuffer";
    else if (s.name == "bool")
	result += "boolean";
    else if (s.use == Struct::asStruct)
	result += structName(p, s);
    else if (s.use == Struct::asEnum)
	result += toUpper(p.name) + "_enum_" + s.name;
    else
	result += s.name;

    if (f.array)
	result += ">";

    return result;
}

static void dumpDtsClass(std::ofstream& os, Protocol const& p,
			 Struct::Usage const use, StructList const& sl,
			 TargetType const tgt)
{
    for (StructList::const_iterator ii = sl.begin(); ii != sl.end();
	 ++ii)
	if (ii->use == use) {
	    os << "export declare class " << structName(p, *ii) << " {\n";

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj) {
		os << "    " << jj->name << (jj->optional ? "?: " : ": ") <<
		    toJsType(p, *jj) << ";\n";
	    }

	    os << "    constructor();\n";

	    if ((target & tgt) != None)
		os << "    *marshal(): IterableIterator<number>;\n";

	    os << "}\n\n";
	}
}

static void dumpDtsFile(std::ofstream& os, Protocol const& p)
{
    std::string const protoName = toUpper(p.name);
    std::string const objName = protoName + "_PROTO";

    for (StructList::const_iterator ii = p.types.begin();
	 ii != p.types.end(); ++ii)
	if (ii->use == Struct::asEnum) {
	    os <<
		"export declare enum " << protoName << "_enum_" <<
		ii->name << " {\n";

	    bool first = true;

	    for (FieldList::const_iterator jj = ii->fields.begin();
		 jj != ii->fields.end(); ++jj)
		os <<
		    (first ? first = false, "    " : ",\n    ") <<
		    xlat_kw(jj->name) << " = " << std::dec << jj->hash;

	    os <<
		"\n}\n\n";
	}

    dumpDtsClass(os, p, Struct::asStruct, p.types, Both);
    dumpDtsClass(os, p, Struct::asRequest, p.requests, Client);
    dumpDtsClass(os, p, Struct::asReply, p.replies, Server);

    os <<
	"type " << protoName << "_Requests ";

    std::string indent = std::string(15 + protoName.size(), ' ');

    for (StructList::const_iterator ii = p.requests.begin();
	 ii != p.requests.end(); ++ii)
	os << (ii == p.requests.begin() ? "= " : "\n" + indent + "| ") <<
	    structName(p, *ii);

    os <<
	";\n\n"
	"type " << protoName << "_Replies ";

    indent = std::string(14 + protoName.size(), ' ');

    for (StructList::const_iterator ii = p.replies.begin();
	 ii != p.replies.end(); ++ii)
	os << (ii == p.replies.begin() ? "= " : "\n" + indent + "| ") <<
	    structName(p, *ii);

    os <<
	";\n\n"
	"interface " << protoName << "_IF {\n";

    if ((target & Server) == Server)
	os <<
	    "    unmarshal_request: (iter: IterableIterator<number>) => " <<
	    protoName << "_Requests;\n";

    if ((target & Client) == Client)
	os <<
	    "    unmarshal_reply: (iter: IterableIterator<number>) => " <<
	    protoName << "_Replies;\n";

    os <<
	"}\n\n"
	"export const " << objName << ": " << protoName << "_IF;\n";
}

void generateJS(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("Javascript generator only supports SDDv2");

    if (jsEmitLib)
	emitJsLibrary();
    else {
	{
	    std::string const srcName =
		targetPath + "/" + toLower(p.name) + "_protocol.js";
	    std::ofstream src(srcName.c_str());

	    dumpSourceFile(src, p);
	}

	if (jsEmitDts) {
	    std::string const srcName =
		targetPath + "/" + toLower(p.name) + "_protocol.d.ts";
	    std::ofstream src(srcName.c_str());

	    dumpDtsFile(src, p);
	}
    }
}
