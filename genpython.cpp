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

// This module generates Python files that can communicate using a
// specified protocol. The .proto grammar gets mapped to Python in the
// following way:
//
// The marshalling and unmarshalling routines are placed in a source
// file named <<proto>>_protocol.py, where <<proto>> is the name of
// the protocol in all lower case. The only visible functions are
// marshal_request, marshal_reply, unmarshal_request, and
// unmarshal_reply.
//
// Request, Reply, and Structs are implemented using python classes.
//
// The fields of messages and structures get mapped to the following
// types:
//
//    enum       -> predefined symbols containing calculated values
//    bool       -> False or True
//    int16      -> int
//    int32      -> int
//    int64      -> long
//    double     -> float
//    string     -> str
//    binary     -> bytearray
//    array of T -> list
//    T          -> class T
//    optional T -> T or not present
//
// To make the names of structures and messages and fields common to
// all target languages, the first character of the protocol and
// message names should be uppercase and the field names should begin
// with a lowercase letter.

#include <sstream>
#include <iosfwd>
#include <iomanip>
#include <fstream>
#include <numeric>
#include <cctype>
#include <cassert>
#include "pc.h"

static std::string const& range()
{
    static std::string name = pyV3 ? "range" : "xrange";

    return name;
}

static std::string const& next()
{
    static std::string name = pyV3 ? "__next__" : "next";

    return name;
}

static std::string iter_value(int32_t v)
{
    std::ostringstream os;

    if (pyV3)
	os << v;
    else
	os << "'\\x" << std::setfill('0') << std::setw(2) <<
	    std::hex << v << std::dec << "'";
    return os.str();
}

static std::string const& iter_to_int()
{
    static std::string name = pyV3 ? "" : "ord";

    return name;
}

static std::string buf_append(std::string const& buf, std::string const& expr)
{
    if (pyV3)
	return buf + ".append(" + expr + ")";
    else
	return buf + " += chr(" + expr + ")";
}

static std::string int_const(int32_t const& v)
{
    std::ostringstream os;

    os << "int(" << v << ")";
    return os.str();
}

static std::string int_const(int64_t const& v)
{
    std::ostringstream os;

    if (pyV3)
	os << "int(" << v << ")";
    else
	os << "long(" << v << "L)";
    return os.str();
}

static Struct const& getType(Protocol const& p, std::string const& type)
{
    return p.findType(type);
}

static void emitConstantInt(std::ostream& os, uint8_t tag, int64_t const val)
{
    std::ostringstream tmp;

    emitRawInt(tmp, tag, val);
    os << std::hex << "b'";

    std::string const s = tmp.str();

    for (std::string::const_iterator ii = s.begin(); ii != s.end(); ++ii)
	os << "\\x" << std::setfill('0') << std::setw(2) <<
	    (unsigned)(uint8_t) *ii;

    os << std::dec << '\'';
}

static std::string xlatType(Protocol const& p, std::string const& type)
{
    if (type == "int16" || type == "int32" || type == "int64" ||
	type == "double" || type == "string" || type == "binary" ||
	type == "bool")
	return type;
    else
	return type + (isEnum(p, type) ? "_enum" : "_struct");
}

static void dumpClassDefinition(std::ostream& os, Protocol const& p,
				Struct const& s, std::string const& name)
{
    os << "class " << name << ":\n";

    FieldList::const_iterator ii = s.fields.begin();

    if (ii != s.fields.end()) {
	if (countRequired(s) > 0) {
	    os <<
		"    def __init__(self):\n";

	    for (; ii != s.fields.end(); ++ii)
		if (!ii->optional) {
		    os << "        self." << ii->name << " = ";
		    if (ii->array)
			os << "[]\n";
		    else {
			if (ii->type == "bool")
			    os << "bool(False)\n";
			else if (ii->type == "int16")
			    os << int_const(int16_t(0)) << '\n';
			else if (ii->type == "int32")
			    os << int_const(int32_t(0)) << '\n';
			else if (ii->type == "int64")
			    os << int_const(int64_t(0)) << '\n';
			else if (ii->type == "double")
			    os << "float(0.0)\n";
			else if (ii->type == "string")
			    os << "''\n";
			else if (ii->type == "binary")
			    os << "bytearray(b'')\n";
			else {
			    Struct const& tmp = getType(p, ii->type);

			    if (tmp.use == Struct::asEnum) {
				Field const& fld = *(tmp.fields.begin());

				os << tmp.name << '_' << fld.name << "\n";
			    } else
				os << tmp.name << "_struct()\n";
			}
		    }
		}
	}
	os <<
	    "\n"
	    "    def __eq__(self, other):\n"
	    "        return ";

	for (ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
	    os <<
		(ii == s.fields.begin() ? "" : " and \\\n            ");
	    if (!ii->optional)
		os << "self." << ii->name << " == other." << ii->name;
	    else
		os <<
		    "((not hasattr(self, '" << ii->name << "') and "
		    "not hasattr(other, '" << ii->name << "')) or \\\n"
		    "            (hasattr(self, '" << ii->name << "') and "
		    "hasattr(other, '" << ii->name << "') and \\\n"
		    "             self." << ii->name << " == other." <<
		    ii->name << "))";
	}

	os <<
	    "\n\n"
	    "    def __ne__(self, other):\n"
	    "        return not self.__eq__(other)\n\n";
    } else
	os <<
	    "    def __eq__(self, other):\n"
	    "        return True\n\n"
	    "    def __ne__(self, other):\n"
	    "        return False\n\n";
}

static void dumpUserTypeMarshaller(Protocol const& p, Struct const& s,
				   std::ostream& os)
{
    if (s.isUserDefined() && s.isUsed()) {
	if (s.use == Struct::asEnum) {
	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii)
		os <<
		    "__all__.append('" << s.name << '_' << ii->name <<
		    "')\n" <<
		    s.name << '_' << (*ii).name << " = " << ii->hash << "\n";

	    os << "\ndef marshal_" << xlatType(p, s.name) << "(val):\n";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii) {
		os <<
		    (ii == s.fields.begin() ? "    if" : "    elif") <<
		    " int(val) == " << ii->hash << ":\n"
		    "        return ";
		emitConstantInt(os, 0x80, ii->hash);
		os <<
		    "\n";
	    }
	    os <<
		"    else:\n"
		"        raise ProtocolError(\"invalid value for enum '" <<
	       s.name << "'\")\n\n";
	} else {
	    dumpClassDefinition(os, p, s, xlatType(p, s.name));
	    os << "def marshal_" << xlatType(p, s.name) << "(val):\n";

	    if (s.fields.size() > 0) {
		size_t const required = countRequired(s);

		os << "    return chain(";

		if (required == s.fields.size()) {
		    emitConstantInt(os, 0x50, required * 2);
		} else {
		    os << "emitRawInt(0x50, " << (required * 2);
		    for (FieldList::const_iterator ii = s.fields.begin();
			 ii != s.fields.end(); ++ii)
			if (ii->optional)
			    os <<
				" \\\n" << std::string(20, ' ') <<
				"+ (2 if hasattr(val, '" << ii->name <<
				"') else 0)";
		    os << ")";
		}

		for (FieldList::const_iterator ii = s.fields.begin();
		     ii != s.fields.end(); ++ii)
		    if (!ii->optional) {
			os <<
			    ",\n" << std::string(17, ' ');
			emitConstantInt(os, 0x10, ii->hash);
			os <<
			    ",\n" << std::string(17, ' ');
			if (ii->array)
			    os <<
				"marshal_array(marshal_" <<
				xlatType(p, ii->type) << ", val." <<
				ii->name << ")";
			else
			    os <<
				"marshal_" << xlatType(p, ii->type) <<
				"(val." << ii->name << ")";
		    } else {
			os <<
			    ",\n" <<
			    std::string(17, ' ') << "chain(";
			emitConstantInt(os, 0x10, ii->hash);
			os <<
			    ",\n" << std::string(23, ' ');
			if (ii->array)
			    os <<
				"marshal_array(marshal_" <<
				xlatType(p, ii->type) <<
				", val." << ii->name << ")";
			else
			    os <<
				"marshal_" << xlatType(p, ii->type) <<
				"(val." << ii->name << ")";
			os <<
			    ") \\\n" << std::string(23, ' ') <<
			    "if hasattr(val, '" << ii->name << "') else bytearray()";
		    }
		os << ")\n\n";
	    } else {
		os << "    return ";
		emitConstantInt(os, 0x50, 0);
		os << ".__iter__()\n\n";
	    }
	}
    }
}

static void dumpMessageMarshaller(Protocol const& p, Struct const& s,
				  std::ostream& os)
{
    assert(s.use == Struct::asRequest || s.use == Struct::asReply);
    char const* const suffix =
	s.use == Struct::asRequest ? "_request" : "_reply";

    dumpClassDefinition(os, p, s, s.name + suffix);

    os <<
	"    def marshal(self):\n"
	"        '''Returns a generator that emits a character stream "
	"representing\n"
	"           the marshaled contents of " <<
	(s.name + suffix) << ".'''\n";

    os <<
	"        return chain(marshal_header(),\n"
	"                     ";
    emitConstantInt(os, 0x10, s.hash);
    os <<
	",\n"
	"                     ";

    if (s.fields.size() > 0) {
	size_t const required = countRequired(s);

	if (required == s.fields.size())
	    emitConstantInt(os, 0x50, required * 2);
	else {
	    os << "emitRawInt(0x50, " << (required * 2);
	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii)
		if (ii->optional)
		    os <<
			" \\\n" << std::string(24, ' ') <<
			"+ (2 if hasattr(self, '" << ii->name << "') else 0)";
	    os << ")";
	}

	for (FieldList::const_iterator ii = s.fields.begin();
	     ii != s.fields.end(); ++ii)
	    if (!ii->optional) {
		os <<
		    ",\n" << std::string(21, ' ');
		emitConstantInt(os, 0x10, ii->hash);
		os <<
		    ",\n" << std::string(21, ' ');
		if (ii->array)
		    os <<
			"marshal_array(marshal_" << xlatType(p, ii->type) <<
			", self." << ii->name << ")";
		else
		    os <<
			"marshal_" << xlatType(p, ii->type) << "(self." <<
			ii->name  << ")";
	    } else {
		os <<
		    ",\n" <<
		    std::string(21, ' ') << "chain(";
		emitConstantInt(os, 0x10, ii->hash);
		os <<
		    ",\n" << std::string(27, ' ');
		if (ii->array)
		    os <<
			"marshal_array(marshal_" << xlatType(p, ii->type) <<
			", self." << ii->name << ")";
		else
		    os <<
			"marshal_" << xlatType(p, ii->type) << "(self." <<
			ii->name << ")";
		os <<
		    ") \\\n" << std::string(27, ' ') <<
		    "if hasattr(self, '" << ii->name << "') else bytearray()";
	    }
    } else
	emitConstantInt(os, 0x50, 0);
    os << ")\n\n";
}

static void dumpStructUnmarshaller(std::ostream& os, Protocol const& p,
				   Struct const& s, std::string const& typ)
{
    os <<
	"def unmarshal_" << typ << "(ii):\n";

    if (s.fields.size() > 0) {
	size_t const required = countRequired(s);

	os << "    nFlds = consumeRawInt(ii, 0x50)\n";

	if (s.fields.size() == required)
	    os <<
		"    if nFlds != " << (2 * required) << ":\n"
		"        raise ProtocolError(\"incorrect number of fields\")\n";
	else
	    os <<
		"    if (nFlds % 2) != 0 or nFlds < " << (2 * required) <<
		" or nFlds > " << (2 * s.fields.size()) << ":\n"
		"        raise ProtocolError(\"incorrect number of fields\")\n";
	os <<
	    "    else:\n"
	    "        tmp = " << typ << "()\n"
	    "        for xx in " << range() << "(nFlds // 2):\n"
	    "            fld = consumeRawInt(ii, 0x10)\n";

	for (FieldList::const_iterator ii = s.fields.begin();
	     ii != s.fields.end(); ++ii) {
	    os <<
		(ii == s.fields.begin() ? "            " : "            el") <<
		"if fld == " << ii->hash << ":\n"
		"                tmp." << ii->name << " = unmarshal_";
	    if (ii->array)
		os <<
		    "array(ii, unmarshal_" << xlatType(p, ii->type) << ")\n";
	    else
		os <<
		    xlatType(p, ii->type) << "(ii)\n";
	}

	os <<
	    "            else:\n"
	    "                raise ProtocolError(\"unknown field found\")\n"
	    "        return tmp\n\n";
    } else
	os <<
	    "    if consumeRawInt(ii, 0x50) != 0:\n"
	    "        raise ProtocolError(\"incorrect number of fields\")\n"
	    "    else:\n"
	    "        return " << typ << "()\n\n";
}

static void dumpUserTypeUnmarshaller(Protocol const& p, Struct const& s,
				     std::ostream& os)
{
    if (s.isUserDefined() && s.isUsed()) {
	if (s.use == Struct::asEnum) {
	    os <<
		"def unmarshal_" << xlatType(p, s.name) << "(ii):\n"
		"    val = consumeRawInt(ii, 0x80)\n";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii)
		os <<
		    (ii == s.fields.begin() ? "    if" : "    elif") <<
		    " val == " << ii->hash << ":\n"
		    "        return " << s.name << '_' << (*ii).name << '\n';

	    os <<
		"    else:\n"
		"        raise ProtocolError(\"invalid value for enum '" <<
	       s.name << "'\")\n\n";
	} else
	    dumpStructUnmarshaller(os, p, s, s.name + "_struct");
    }
}

static void dumpMessageUnmarshaller(Protocol const& p, Struct const& s,
				    std::ostream& os)
{
    std::string const tName =
	s.name + (s.use == Struct::asRequest ? "_request" : "_reply");

    dumpStructUnmarshaller(os, p, s, tName);
}

static void dumpGeneralMessageUnmarshaller(Protocol const&, std::ostream& os,
					   StructList const& ss,
					   std::string const& msgType)
{
    StructList::const_iterator ii = ss.begin();

    os << "def unmarshal_" << msgType << "(ii):\n" <<
	"    '''Attempts to unmarshal a " << msgType <<  " message from"
	" the specified\n"
	"       generator, ii. If an error occurs, the ProtocolError"
	" exception\n"
	"       will be raised.'''\n";

    if (ii != ss.end()) {
	os <<
	    "    try:\n"
	    "        unmarshal_header(ii)\n";

	if (ss.size() == 1)
	    os <<
		"        if consumeRawInt(ii, 0x10) == " << ii->hash << ":\n"
		"            return unmarshal_" << ii->name << "_" <<
		msgType << "(ii)\n";
	else {
	    os << "        msg = consumeRawInt(ii, 0x10)\n";
	    do {
		os <<
		    (ii == ss.begin() ? "        " : "        el") <<
		    "if msg == " << ii->hash << ":\n"
		    "            return unmarshal_" << ii->name << "_" <<
		    msgType << "(ii)\n";
	    } while (++ii != ss.end());
	}

	os <<
	    "        else:\n"
	    "            raise ProtocolError(\"unknown " << msgType <<
	    " type\")\n"
	    "    except StopIteration:\n"
	    "        raise ProtocolError(\"unexpected end of input\")\n\n";
    } else
	os <<
	    "    raise ProtocolError(\"protocol doesn't define any " <<
	    msgType << " messages\")\n\n";
}

static void dumpPublicNames(Protocol const&, Struct const& s,
			    std::ostream& os)
{
    switch (s.use) {
     case Struct::asRequest:
	os << ",\n           '" << s.name << "_request'";
	break;
     case Struct::asReply:
	os << ",\n           '" << s.name << "_reply'";
	break;
     case Struct::asStruct:
	os << ",\n           '" << s.name << "_struct'";
	break;
     default:
	break;
    }
}

static void dumpSourceFile(std::ostream& os, Protocol const& p)
{
    os <<
	"# Generated by the protocol compiler version " << pcVersion << "\n"
	"# DO NOT EDIT THIS FILE DIRECTLY!\n\n"
	"__doc__ = 'Message serializer for the " << p.name << " protocol.'\n\n"
	"from itertools import chain, islice\n\n";

    os << "__all__ = ['ProtocolError',\n"
	"           'unmarshal_request',\n"
	"           'unmarshal_reply'";

    apply(p, os, p.types.begin(), p.types.end(), dumpPublicNames);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpPublicNames);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpPublicNames);

    os << "]\n\n";

    if (p.needed("double"))
	os <<
	    "import struct\n\n";

    os <<
	"class ProtocolError(Exception):\n"
	"    '''Exception class that gets raised when there's a problem"
	" marshaling\n"
	"       or unmarshaling a message from the " << p.name <<
	" protocol.'''\n\n"
	"    def __init__(self, reason):\n"
	"        self.reason = reason\n\n"
	"    def __str__(self):\n"
	"        return repr(self.reason)\n\n"
	"# -- Internal marshalling routines --\n\n"
	"def emitRawInt(tag, val):\n"
	"    def emitEach(buf, n):\n"
        "        curr = (val >> (n * 8)) & 0xff\n"
	"        next = val >> ((n + 1) * 8)\n"
        "        if (next == 0 and (curr & 0x80) != 0x80) or \\\n"
	"           (next == -1 and (curr & 0x80) == 0x80):\n"
	"            " << buf_append("buf", "tag + n + 1") << "\n"
	"        else:\n"
	"            emitEach(buf, n + 1)\n"
	"        " << buf_append("buf", "curr") << "\n"
	"    tmp = bytearray()\n"
	"    emitEach(tmp, 0)\n"
	"    return bytes(tmp)\n\n";

    if (p.needed("bool"))
	os <<
	    "def marshal_bool(val):\n"
	    "    yield " << (pyV3 ? "" : "chr") <<"(0x71 if val else 0x70)\n\n";

    os <<
	"def marshal_int16(val):\n"
	"    if isinstance(val, int)" << (pyV3 ? "" : " or isinstance(val, long)") << ":\n"
	"        if val < 32768 and val > -32769:\n"
	"            return emitRawInt(0x10, val)\n"
	"        else:\n"
	"            raise ProtocolError(\"value out of range for int16\")\n"
	"    else:\n"
	"        raise ProtocolError(\"expected integer type\")\n\n"
	"def marshal_int32(val):\n"
	"    if isinstance(val, int)" << (pyV3 ? "" : " or isinstance(val, long)") << ":\n"
	"        if " << int_const(int32_t(-0x80000000L)) <<
	" <= val <= " << int_const(int32_t(0x7fffffffL)) << ":\n"
	"            return emitRawInt(0x10, val)\n"
	"        else:\n"
	"            raise ProtocolError(\"value out of range for int32\")\n"
	"    else:\n"
	"        raise ProtocolError(\"expected integer type\")\n\n";

    if (p.needed("int64"))
	os <<
	    "def marshal_int64(val):\n"
	    "    if isinstance(val, int)" << (pyV3 ? "" : " or isinstance(val, long)") << ":\n"
	    "        if " << int_const(int64_t(-0x8000000000000000LL)) <<
	    " <= val <= " << int_const(int64_t(0x7fffffffffffffffLL)) <<
	    ":\n"
	    "            return emitRawInt(0x10, val)\n"
	    "        else:\n"
	    "            raise ProtocolError(\"value out of range for int64\")\n"
	    "    else:\n"
	    "        raise ProtocolError(\"expected integer type\")\n\n";

    if (p.needed("double"))
	os <<
	    "def marshal_double(val):\n"
	    "    return chain(b'\\x28', (ii for ii in struct.pack(\">d\", float(val))))\n\n";

    if (p.needed("string"))
	os <<
	    "def marshal_string(val):\n"
	    "    if isinstance(val, str):\n"
	    "        return chain(emitRawInt(0x40, len(val)),\\\n"
	    "                     (" << (pyV3 ? "ord(ii)" : "ii") <<
	    " for ii in val))\n"
	    "    else:\n"
	    "        raise ProtocolError(\"expected string type\")\n\n";

    if (p.needed("binary")) {
	os <<
	    "def marshal_binary(val):\n"
	    "    if isinstance(val, bytearray):\n";
	if (pyV3)
	    os <<
		"        return chain(emitRawInt(0x30, len(val)), val)\n";
	else
	    os <<
		"        return chain(emitRawInt(0x30, len(val)),\\\n"
		"                     (chr(ii) for ii in val))\n";
	os <<
	    "    else:\n"
	    "        raise ProtocolError(\"expected bytearray type\")\n\n";
    }

    os <<
	"def marshal_array(fn, val):\n"
	"    if isinstance(val, list):\n"
	"        return chain(emitRawInt(0x50, len(val)),\\\n"
	"                     chain.from_iterable((fn(v) for v in val)))\n"
	"    else:\n"
	"        raise ProtocolError(\"expected list type\")\n\n"
	"def marshal_header():\n"
	"    return (ii for ii in b'SDD\\x02\\x51\\x03' ";
    emitConstantInt(os, 0x10, p.longHash(p.name));
    os <<
	")\n\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpUserTypeMarshaller);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpMessageMarshaller);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpMessageMarshaller);

    os <<
	"def marshal_request(val):\n"
	"    return val.marshal()\n\n"
	"def marshal_reply(val):\n"
	"    return val.marshal()\n\n"
	"# -- Internal unmarshalling routines --\n\n";

    os <<
	"def consumeRawInt(ii, tag):\n"
	"    iiTag = " << iter_to_int() << "(ii." << next() << "())\n"
	"    iiLen = iiTag & 0xf\n"
	"    if (iiTag & 0xf0) == (tag & 0xf0) and iiLen > 0 and iiLen <= 8:\n"
	"        firstByte = " << iter_to_int() << "(ii." << next() << "())\n"
	"        retVal = (0 if (0x80 & firstByte) == 0 else -256) | firstByte\n"
	"        while iiLen > 1:\n"
	"            retVal = (retVal << 8) | " << iter_to_int() << "(ii." <<
	next() << "())\n"
	"            iiLen = iiLen - 1\n"
	"        return " << (pyV3 ? "int" : "long") << "(retVal)\n"
	"    else:\n"
	"        raise ProtocolError(\"bad tag or length\")\n\n";

    if (p.needed("bool"))
	os <<
	    "def unmarshal_bool(ii):\n"
	    "    val = ii." << next() << "()\n"
	    "    if val == " << iter_value(0x70) << ":\n"
	    "        return False\n"
	    "    elif val == " << iter_value(0x71) << ":\n"
	    "        return True\n"
	    "    else:\n"
	    "        raise ProtocolError(\"expected boolean value\")\n\n";

    os <<
	"def unmarshal_int16(ii):\n"
	"    val = consumeRawInt(ii, 0x10)\n"
	"    if val >= -0x8000 and val < 0x8000:\n"
	"        return int(val)\n"
	"    else:\n"
	"        raise ProtocolError(\"value out of range for int16\")\n\n"
	"def unmarshal_int32(ii):\n"
	"    val = consumeRawInt(ii, 0x10)\n"
	"    if " << int_const(int32_t(-0x80000000L)) <<
	" <= val <= " << int_const(int32_t(0x7fffffffL)) << ":\n"
	"        return int(val)\n"
	"    else:\n"
	"        raise ProtocolError(\"value out of range for int32\")\n\n";

    if (p.needed("int64"))
	os <<
	    "def unmarshal_int64(ii):\n"
	    "    val = consumeRawInt(ii, 0x10)\n"
	    "    if " << int_const(int64_t(-0x8000000000000000LL)) <<
	    " <= val <= " << int_const(int64_t(0x7fffffffffffffffLL)) << ":\n"
	    "        return val\n"
	    "    else:\n"
	    "        raise ProtocolError(\"value out of range for int64\")\n\n";

    if (p.needed("double")) {
	os <<
	    "def unmarshal_double(ii):\n"
	    "    if ii." << next() << "() == " << iter_value(0x28) << ":\n";

	if (pyV3)
	    os <<
		"        raw = bytearray(islice(ii, 8))\n";
	else
	    os <<
		"        raw = ''\n"
		"        for x in " << range() << "(8):\n"
		"            raw = raw + ii." << next() << "()\n";

	os <<
	    "        v, = struct.unpack(\">d\", raw)\n"
	    "        return v\n"
	    "    else:\n"
	    "        raise ProtocolError(\"expected tag for double\")\n\n";
    }

    if (p.needed("string"))
	os <<
	    "def unmarshal_string(ii):\n" <<
	    "    return " <<
	    (pyV3 ?
	     "bytearray(islice(ii, consumeRawInt(ii, 0x40))).decode('utf-8')" :
	     "str().join(islice(ii, consumeRawInt(ii, 0x40)))") << "\n\n";

    if (p.needed("binary"))
	os <<
	    "def unmarshal_binary(ii):\n"
	    "    return bytearray(islice(ii, consumeRawInt(ii, 0x30)))\n\n";

    os <<
	"def unmarshal_array(ii, fn):\n"
	"    return [fn(ii) for x in " << range() <<
	"(consumeRawInt(ii, 0x50))]\n\n"
	"def unmarshal_header(ii):\n"
	"    if ii." << next() << "() != " << iter_value('S') <<
	" or ii." << next() << "() != " << iter_value('D') << " or \\\n"
	"       ii." << next() << "() != " << iter_value('D') <<
	" or ii." << next() << "() != " << iter_value(2) << " or \\\n"
	"       consumeRawInt(ii, 0x50) != 3:\n"
	"        raise ProtocolError(\"invalid header\")\n"
	"    elif consumeRawInt(ii, 0x10) != " << p.longHash(p.name) <<
	":\n"
	"        raise ProtocolError(\"incorrect protocol specified\")\n\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpUserTypeUnmarshaller);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpMessageUnmarshaller);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpMessageUnmarshaller);

    dumpGeneralMessageUnmarshaller(p, os, p.requests, "request");
    dumpGeneralMessageUnmarshaller(p, os, p.replies, "reply");
}

// The only externally visible function from this module. This is the
// entry point to generate the Python file that handles the protocol.

void generatePython(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("Python generator only supports SDDv2");

    std::string const srcName = targetPath + "/" + toLower(p.name) + "_protocol.py";
    std::ofstream src(srcName.c_str());

    dumpSourceFile(src, p);
}
