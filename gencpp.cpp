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

// This module generates C++ files that can communicate using a
// specified protocol. The .proto grammar gets mapped to C++ in the
// following way:
//
//    "protocol Name" creates an abstract base class called Base. An
//    interface is also created, called Receiver. Receiver defines
//    pure virtual functions that handle each message type.
//
//    "message Name" creates a class derived from the protocol base
//    class.
//
//    "struct Name" creates a structure (not derived from anything.)
//
// The fields of messages and structures get mapped to the following
// types:
//
//    bool       -> bool
//    int16      -> int16_t
//    int32      -> int32_t
//    int64      -> int64_t
//    double     -> double
//    string     -> std::string
//    binary     -> std::vector<uint8_t>
//    array of T -> std::vector<T>
//    T          -> struct T   (where T is a type defined earlier)
//    enum       -> enum with fixed, calculated values
//
// If a field is marked "optional", it'll be implemented as an
// std::auto_ptr<>, a std::unique_ptr<>, or an std::optional<>
// (depending on the version of C++) to one of the above types.
//
// To make the names of structures and messages and fields common to
// all target languages, the first character of the protocol and
// message names should be uppercase and the field names should begin
// with a lowercase letter.

#include <fstream>
#include <iomanip>
#include <assert.h>
#include <sstream>
#include "pc.h"

// Prototypes.

static void constructors(Protocol const&, Struct const&, std::ostream&);
static void dumpBase(std::ostream&, StructList const&);
static void dumpBaseUnmarshaller(Protocol const&, std::ostream&, StructList const&);
static void dumpConstructor(Struct const&, std::ostream&, int);
static void dumpEqualityOperators(Protocol const&, Struct const&, std::ostream&, int);
static void dumpField(Protocol const&, Field const&, std::ostream&, size_t);
static void dumpForwardDecl(Protocol const&, Struct const&, std::ostream&, int);
static void dumpHeaderFile(Protocol const&, std::ostream&, int);
static void dumpMessage(Protocol const&, Struct const&, std::ostream&, int);
static void dumpMessageHandler(Protocol const&, Struct const&, std::ostream&, int);
static void dumpMessageNamespace(Protocol const&, char const*, StructList const&, std::ostream&, int);
static void dumpMessageSource(Protocol const&, Struct const&, std::ostream&);
static void dumpMsgUnmarshaller(Protocol const&, Struct const&, std::ostream&);
static void dumpProtocol(Protocol const&);
static void dumpSourceFile(Protocol const&, std::ostream&);
static void dumpStructFields(Protocol const&, Struct const&, std::ostream&, size_t);
static void dumpStructMarshaller(Protocol const&, Struct const&, std::ostream&);
static void dumpStructSwap(Protocol const&, Struct const&, std::ostream&);
static void dumpSwapField(Protocol const&, Field const&, std::ostream&);
static void dumpType(Protocol const&, Struct const&, std::ostream&, int);
static std::string fqName(Struct const&);
static std::string getNamespace(Struct const&);
static void memberAssignment(Protocol const&, Field const&, std::ostream&);
static void memberCopy(Protocol const&, Field const&, std::ostream&, bool*);
static void static_prototypes(Protocol const&, Struct const&, std::ostream&);
static void struct_unmarshaller(Protocol const&, Struct const&, std::ostream&);
static std::string type_name(Field const&);
static void unmarshalField(Protocol const&, Field const&, std::ostream&, size_t*);
static void unmarshalFieldLarge(Protocol const&, Field const&, std::ostream&, size_t*);
static std::string xlatType(std::string const&);

// Small class hierarchy to support the various ways we map optional
// values to C++.

class OptType {
 public:
    virtual ~OptType() {}
    virtual std::string emit_type(std::string const&) const = 0;
    virtual std::string emit_has_value(std::string const&) const = 0;
    virtual std::string emit_equality_test(std::string const&,
					   std::string const&) const = 0;
    virtual std::string emit_swap(std::string const&,
				  std::string const&) const = 0;
    virtual std::string emit_assignment(std::string const&,
					std::string const&,
					std::string const&) const = 0;
    virtual std::string emit_copy(std::string const&,
				  std::string const&,
				  std::string const&) const = 0;
    virtual std::string emit_unmarshal(Field const&,
				       std::string const&,
				       std::string const&,
				       std::string const&) const = 0;
};

class OptTypeAutoPtr : public OptType {
 public:
    std::string emit_type(std::string const& type) const
    {
	return "std::auto_ptr< " + type + " >";
    }

    std::string emit_has_value(std::string const& a) const
    {
	return a + ".get()";
    }

    std::string emit_swap(std::string const& a,
			  std::string const& b) const
    {
	return "std::swap(" + a + ", " + b + ");";
    }

    std::string emit_equality_test(std::string const& a,
				   std::string const& b) const
    {
	return "((" + a + ".get() == NULL && " + b + ".get() == NULL) || "
	    "(" + a + ".get() != NULL && " + b + ".get() != NULL && "
	    "*" + a + " == *" + b + "))";
    }

    std::string emit_assignment(std::string const& a,
				std::string const& b,
				std::string const& type) const
    {
	return a + ".reset(" + b + ".get() ? new " + type + "(*" + b + ") : 0)";
    }

    std::string emit_copy(std::string const& a, std::string const& b,
			  std::string const& type) const
    {
	return a + "(" + b + ".get() ? new " + type + "(*" + b + ") : 0)";
    }

    std::string emit_unmarshal(Field const& f,
			       std::string const& name,
			       std::string const& p_indent,
			       std::string const& s_indent) const
    {
	return p_indent + "if (!" + name + ".get())\n" +
	    s_indent + name + ".reset(new " + type_name(f) + "());\n" +
	    p_indent + "unmarshal(is, *" + name + ");";
    }
};

class OptTypeUniquePtr : public OptType {
 public:
    std::string emit_type(std::string const& type) const
    {
	return "std::unique_ptr< " + type + " >";
    }

    std::string emit_has_value(std::string const& a) const
    {
	return a;
    }

    std::string emit_swap(std::string const& a,
			  std::string const& b) const
    {
	return "std::swap(" + a + ", " + b + ");";
    }

    std::string emit_equality_test(std::string const& a,
				   std::string const& b) const
    {
	return "((!" + a + " && !" + b + ") || "
	    "(" + a + " && " + b + " && " "*" + a + " == *" + b + "))";
    }

    std::string emit_assignment(std::string const& a,
				std::string const& b,
				std::string const& type) const
    {
	return a + ".reset(" + b + " ? new " + type + "(*" + b +
	    ") : nullptr)";
    }

    std::string emit_copy(std::string const& a, std::string const& b,
			  std::string const& type) const
    {
	return a + "(" + b + " ? new " + type + "(*" + b + ") : nullptr)";
    }

    std::string emit_unmarshal(Field const& f,
			       std::string const& name,
			       std::string const& p_indent,
			       std::string const& s_indent) const
    {
	return p_indent + "if (!" + name + ")\n" +
	    s_indent + name + ".reset(new " + type_name(f) + "());\n" +
	    p_indent + "unmarshal(is, *" + name + ");";
    }
};

class OptTypeOptional : public OptType {
 public:
    std::string emit_type(std::string const& type) const
    {
	return "std::experimental::optional< " + type + " >";
    }

    std::string emit_has_value(std::string const& a) const
    {
	return a + " != std::experimental::nullopt";
    }

    std::string emit_swap(std::string const& a,
			  std::string const& b) const
    {
	return a + ".swap(" + b + ");";
    }

    std::string emit_equality_test(std::string const& a,
				   std::string const& b) const
    {
	return "(" + a + " == " + b + ")";
    }

    std::string emit_assignment(std::string const& a,
				std::string const& b,
				std::string const&) const
    {
	return a + " = " + b;
    }

    std::string emit_copy(std::string const& a, std::string const& b,
			  std::string const&) const
    {
	return a + "(" + b + ")";
    }

    std::string emit_unmarshal(Field const& f,
			       std::string const& name,
			       std::string const& p_indent,
			       std::string const& s_indent) const
    {
	return p_indent + "{\n" +
	    s_indent + "auto tmp = std::experimental::make_optional(" + type_name(f) + "());\n\n" +
	    s_indent + "unmarshal(is, *tmp);\n" +
	    s_indent + name + ".swap(tmp);\n" +
	    p_indent + '}';
    }
};

static OptType const* optType = 0;

static char const* no_except()
{
    switch (cppLevel) {
     case cpp11:
	return " noexcept";

     case cpp14:
     case cpp17:
     case cpp_exp:
	return " noexcept(true)";

     case cppLegacy:
     default:
	return "";
    }
}

static std::string mk_ptr_to(std::string const type)
{
    return (cppLevel == cppLegacy ?
	    "std::auto_ptr< " : "std::unique_ptr< ") + type + " >";
}

static std::string const indent(size_t const indent)
{
    return std::string((indent * 4) / 8, '\t') +
	std::string((indent * 4) % 8, ' ');
}

static std::string type_name(Field const& f)
{
    return f.array ? "std::vector<" + xlatType(f.type) + " >" :
	xlatType(f.type);
}

std::string toUpper(std::string const& s)
{
    std::string d;

    for (size_t ii = 0; ii < s.size(); ii++) {
	char c = toupper(s[ii]);
	d.append(&c, 1);
    }
    return d;
}

size_t containerHeaderLength(size_t const n)
{
    if (n < 0x100)
	return MIN_CONTAINER_OVERHEAD;
    else if (n < 0x10000)
	return 3;
    else if (n < 0x1000000)
	return 4;
    else
	return MAX_CONTAINER_OVERHEAD;
}

// Dumps a single field to the stream.

static void dumpField(Protocol const&, Field const& f, std::ostream& os,
		      size_t const indentLevel)
{
    std::string typeName = type_name(f);

    if (f.optional)
	typeName = optType->emit_type(typeName);

    os << indent(indentLevel) << typeName << " " << f.name << ";\n";
}

static bool pred(Field const& f)
{
    return f.optional;
}

bool Struct::hasOptionals() const
{
    return std::find_if(fields.begin(), fields.end(), pred) != fields.end();
}

static bool isPrimitive(std::string const& s)
{
    return s == "bool" || s == "int16" || s == "int32" ||
	s == "int64" || s == "double";
}

void emitRawInt(std::ostream& os, byte const tag, int64_t const val)
{
    byte buf[1 + sizeof(int64_t)];

    if (val >= -0x80 && val <= 0x7f) {
	buf[0] = byte((tag & 0xf0) + 1);
	buf[1] = byte(val);
	os.write(buf, 2);
    } else if (val >= -0x8000 && val <= 0x7fff) {
	buf[0] = byte((tag & 0xf0) + 2);
	buf[1] = byte(val >> 8);
	buf[2] = byte(val);
	os.write(buf, 3);
    } else if (val >= -0x800000 && val <= 0x7fffff) {
	buf[0] = byte((tag & 0xf0) + 3);
	buf[1] = byte(val >> 16);
	buf[2] = byte(val >> 8);
	buf[3] = byte(val);
	os.write(buf, 4);
    } else if (val >= -0x80000000ll && val <= 0x7fffffffll) {
	buf[0] = byte((tag & 0xf0) + 4);
	buf[1] = byte(val >> 24);
	buf[2] = byte(val >> 16);
	buf[3] = byte(val >> 8);
	buf[4] = byte(val);
	os.write(buf, 5);
    } else if (val >= -0x8000000000ll && val <= 0x7fffffffffll) {
	buf[0] = byte((tag & 0xf0) + 5);
	buf[1] = byte(val >> 32);
	buf[2] = byte(val >> 24);
	buf[3] = byte(val >> 16);
	buf[4] = byte(val >> 8);
	buf[5] = byte(val);
	os.write(buf, 6);
    } else if (val >= -0x800000000000ll && val <= 0x7fffffffffffll) {
	buf[0] = byte((tag & 0xf0) + 6);
	buf[1] = byte(val >> 40);
	buf[2] = byte(val >> 32);
	buf[3] = byte(val >> 24);
	buf[4] = byte(val >> 16);
	buf[5] = byte(val >> 8);
	buf[6] = byte(val);
	os.write(buf, 7);
    } else if (val >= -0x80000000000000ll && val <= 0x7fffffffffffffll) {
	buf[0] = byte((tag & 0xf0) + 7);
	buf[1] = byte(val >> 48);
	buf[2] = byte(val >> 40);
	buf[3] = byte(val >> 32);
	buf[4] = byte(val >> 24);
	buf[5] = byte(val >> 16);
	buf[6] = byte(val >> 8);
	buf[7] = byte(val);
	os.write(buf, 8);
    } else {
	buf[0] = byte((tag & 0xf0) + 8);
	buf[1] = byte(val >> 56);
	buf[2] = byte(val >> 48);
	buf[3] = byte(val >> 40);
	buf[4] = byte(val >> 32);
	buf[5] = byte(val >> 24);
	buf[6] = byte(val >> 16);
	buf[7] = byte(val >> 8);
	buf[8] = byte(val);
	os.write(buf, 9);
    }
}

static void emitConstantInt(std::ostream& os, int indentLevel, byte tag,
			    int64_t const val)
{
    std::string const level0 = indent(indentLevel);
    std::string const level1 = indent(indentLevel + 1);
    std::string const level2 = indent(indentLevel + 2);
    std::ostringstream tmp;

    emitRawInt(tmp, tag, val);
    os <<
	level0 << "{\n" <<
	level1 << "static protocol::byte const data[] = {\n" <<
	level2;

    std::string const s = tmp.str();

    for (std::string::const_iterator ii = s.begin(); ii != s.end(); ++ii) {
	if (ii != s.begin())
	    os << ",\n" << level2;
	os <<
	    "static_cast<protocol::byte>(" <<
	    std::dec << int(static_cast<char>(*ii)) << ')';
    }

    os << "\n" <<
	level1 << "};\n\n" <<
	level1 << "os.write(data, sizeof(data));\n" <<
	level0 << "}\n";
}

static void dumpEqualityOperators(Protocol const&, Struct const& s,
				  std::ostream& os, int indentLevel)
{
    if (s.isUserDefined() && s.use != Struct::asEnum) {
	if (s.fields.size()) {
	    os << '\n' <<
		indent(indentLevel) << "inline int operator==(" << s.name << " const& o) const" << no_except() << "\n" <<
		indent(indentLevel) << "{\n" <<
		indent(indentLevel) << "    return ";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii) {
		Field const& f = *ii;

		if (ii != s.fields.begin())
		    os << " &&\n" << indent(indentLevel + 2);

		if (f.optional)
		    os << optType->emit_equality_test(f.name, "o." + f.name);
		else
		    os << "(" << f.name << " == o." << f.name << ")";
	    }
	    os << ";\n";
	} else
	    os << '\n' <<
		indent(indentLevel) << "inline int operator==(" << s.name << " const&) const" << no_except() << "\n" <<
		indent(indentLevel) << "{\n" <<
		indent(indentLevel) << "    return true;\n";

	os << indent(indentLevel) << "}\n";
    }
}

static void dumpForwardDecl(Protocol const&, Struct const& s, std::ostream& os, int indentLevel)
{
    os << indent(indentLevel) << "struct " << s.name << ";\n";
}

static void dumpMessageNamespace(Protocol const& p, char const* name, StructList const& list,
				 std::ostream& os, int indentLevel)
{
    if (list.size()) {
	os << indent(indentLevel) << "namespace " << name << " {\n\n";

	os << indent(indentLevel) << "    // Forward declaration of messages.\n\n";

	apply(p, os, list.begin(), list.end(), dumpForwardDecl, indentLevel + 1);

	os <<
	    "\n" <<
	    indent(indentLevel) << "    class Receiver {\n" <<
	    indent(indentLevel) << "     public:\n" <<
	    indent(indentLevel) << "        virtual ~Receiver();\n";

	apply(p, os, list.begin(), list.end(), dumpMessageHandler, indentLevel + 2);

	os <<
	    indent(indentLevel) << "    };\n\n" <<
	    indent(indentLevel) << "    // Start of the message object hierarchy.\n\n" <<
	    indent(indentLevel) << "    struct Base {\n" <<
	    indent(indentLevel) << "\ttypedef " << mk_ptr_to("Base") << " Ptr;\n\n" <<
	    indent(indentLevel) << "\tvirtual ~Base();\n" <<
	    indent(indentLevel) << "     public:\n" <<
	    indent(indentLevel) << "\tvirtual void deliverTo(Receiver&) = 0;\n" <<
	    indent(indentLevel) << "\tvirtual void marshal(protocol::ostream&) const = 0;\n" <<
	    indent(indentLevel) << "\tvirtual bool needsReply() const = 0;\n" <<
	    indent(indentLevel) << "\tstatic Ptr unmarshal(protocol::istream&);\n" <<
	    indent(indentLevel) << "    };\n\n";

	apply(p, os, list.begin(), list.end(), dumpMessage, indentLevel + 1);
	os << indent(indentLevel) << "}\n";
    }
}

// This function generates the .h file.

static void dumpHeaderFile(Protocol const& p, std::ostream& os, int indentLevel)
{
    os <<
	indent(indentLevel) << "// Data types used in messages, but which aren't messages themselves.\n\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpType, indentLevel);

    dumpMessageNamespace(p, "request", p.requests, os, indentLevel);
    dumpMessageNamespace(p, "reply", p.replies, os, indentLevel);
}

static void dumpMessage(Protocol const& p, Struct const& s, std::ostream& os,
			int indentLevel)
{
    os <<
	indent(indentLevel) << "struct " << s.name << " : public Base {\n";

    dumpStructFields(p, s, os, indentLevel + 1);

    os <<
	"\n" <<
	indent(indentLevel + 1) << "void swap(" << s.name << "&)" <<
	no_except() << ";\n" <<
	indent(indentLevel + 1) << "virtual void deliverTo(Receiver&);\n" <<
	indent(indentLevel) << " public:\n";

    dumpConstructor(s, os, indentLevel + 1);

    os <<
	indent(indentLevel + 1) << "virtual void marshal(protocol::ostream&) const;\n" <<
	indent(indentLevel + 1) << "virtual bool needsReply() const { return " <<
	(s.use == Struct::asRequest ? "true" : "false") << "; };\n";

	dumpEqualityOperators(p, s, os, indentLevel + 1);
    os <<
	indent(indentLevel) << "};\n\n";
}

static void dumpMessageHandler(Protocol const&, Struct const& s, std::ostream& os, int indentLevel)
{
    os << indent(indentLevel) << "virtual void handle(" << s.name << "&) = 0;\n";
}

static void memberAssignment(Protocol const&, Field const& f, std::ostream& os)
{
    os << "    ";

    if (f.optional)
	os <<
	    optType->emit_assignment(f.name, "o." + f.name, type_name(f)) <<
	    ";\n";
    else
	os <<
	    f.name << " = o." << f.name << ";\n";
}

static void memberCopy(Protocol const&, Field const& f, std::ostream& os,
		       bool* const first)
{
    os <<
	(*first ? " :" : ",") << "\n    ";

    if (f.optional)
	os <<
	    optType->emit_copy(f.name, "o." + f.name, type_name(f));
    else
	os <<
	    f.name << "(o." << f.name << ')';

    *first = false;
}

static void constructors(Protocol const& p, Struct const& s, std::ostream& os)
{
    if (s.isUserDefined() && s.hasOptionals()) {
	bool first = true;

	os <<
	    s.name << "::" << s.name << "(" << s.name << " const& o)";

	apply(p, os, s.fields.begin(), s.fields.end(), memberCopy, &first);

	os <<
	    "\n"
	    "{\n";

	if (cppLevel != cpp_exp) {
	    os <<
		"}\n\n" <<
		s.name << "& " << s.name << "::operator=(" << s.name
			<< " const& o)" << no_except() << "\n"
		"{\n";

	    apply(p, os, s.fields.begin(), s.fields.end(), memberAssignment);

	    os <<
		"    return *this;\n";
	}

	os <<
	    "}\n\n";
    }
}

static void struct_unmarshaller(Protocol const& p, Struct const& s, std::ostream& os)
{
    if (s.isUserDefined() && s.isUsed()) {
	if (s.use == Struct::asEnum) {
	    os <<
		(s.used ? "static " : "") <<
		"void unmarshal(protocol::istream& is, " << fqName(s) <<
		"& v)\n"
		"{\n"
		"    switch (readFieldLabel(is, 0x80)) {\n";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii) {
		std::string const eName = cppLevel == cppLegacy ?
		    ii->name : fqName(s) + "::" + ii->name;

		os <<
		    "     case " << std::dec << ii->hash << ":\n"
		    "\tv = " << eName << ";\n"
		    "\tbreak;\n\n";
	    }

	    os <<
		"     default:\n"
		"\tthrow std::runtime_error(\"bad value for enum " <<
		s.name << "\");\n"
		"    }\n"
		"}\n\n";
	} else if (s.fields.size()) {
	    size_t const total = countRequired(s);
	    size_t current = 0;

	    os <<
		(s.used ? "static " : "") <<
		"void unmarshal(protocol::istream& is, " << fqName(s) << "& v)\n"
		"{\n";

	    if (total > 32)
		os << "    std::bitset<" << std::dec << total << "> flg;\n";
	    else if (total > 0)
		os << "    uint32_t flg = 0;\n";

	    os <<
		"    size_t const total = readLength(is, 0x50);\n\n"
		"    for (size_t ii = 0; ii < total; ii += 2) {\n"
		"\tswitch (readFieldLabel(is, 0x10)) {\n";

	    apply(p, os, s.fields.begin(), s.fields.end(), total > 32 ? unmarshalFieldLarge : unmarshalField, &current);

	    os <<
		"\t default:\n"
		"\t    throw std::runtime_error(\"found unknown field\");\n"
		"\t}\n"
		"    }\n\n";

	    if (total > 32)
		os <<
		    "    if (flg.count() != " << std::dec << total << ")\n"
		    "\tthrow std::runtime_error(\"missing required field(s) "
		    "while unmarshalling '" << fqName(s) << "' type\");\n";
	    else if (total > 0)
		os <<
		    "    if (flg != 0x" << std::hex << (total == 32 ? 0xffffffff : ((1 << total) - 1)) << ")\n"
		    "\tthrow std::runtime_error(\"missing required field(s) "
		    "while unmarshalling '" << fqName(s) << "' type\");\n";

	    os << "}\n\n";
	} else
	    os <<
		(s.used ? "static " : "") <<
		"void unmarshal(protocol::istream& is, " << fqName(s) << "&)\n"
		"{\n"
		"    if (readLength(is, 0x50) != 0)\n"
		"\tthrow std::runtime_error(\"error unmarshalling '" << fqName(s) << "' type\");\n"
		"}\n\n";
    }
}

static void dumpSwapField(Protocol const& p, Field const& f, std::ostream& os)
{
    if (f.optional)
	os << "    " << optType->emit_swap(f.name, "o." + f.name) << '\n';
    else if (isPrimitive(f.type) || isEnum(p, f.type))
	os << "    std::swap(" << f.name << ", o." << f.name << ");\n";
    else
	os << "    " << f.name << ".swap(o." << f.name << ");\n";
}

static void dumpStructSwap(Protocol const& p, Struct const& s, std::ostream& os)
{
    if (s.isUserDefined() && s.use != Struct::asEnum && s.used) {
	os <<
	    "void " << fqName(s) << "::swap(" << fqName(s) << "&" << (s.fields.size() ? " o" : "") << ")" << no_except() << "\n"
	    "{\n";
	apply(p, os, s.fields.begin(), s.fields.end(), dumpSwapField);
	os <<
	    "}\n\n";
    }
}

static void dumpMessageSource(Protocol const& p, Struct const& s, std::ostream& os)
{
    os <<
	"void " << fqName(s) << "::deliverTo(Receiver& r)\n"
	"{\n"
	"    r.handle(*this);\n"
	"}\n\n";

    os <<
	"static " << getNamespace(s) << "::Base::Ptr " << getNamespace(s) << '_' << s.name << "_unmarshaller(protocol::istream& is)\n"
	"{\n"
	"    " << mk_ptr_to(fqName(s)) << " ptr(new " << fqName(s) << ");\n\n"
	"    unmarshal(is, *ptr);\n"
	"    return " << getNamespace(s) << "::Base::Ptr(ptr.release());\n"
	"}\n\n";

    dumpStructSwap(p, s, os);
}

static void dumpMsgUnmarshaller(Protocol const&, Struct const& s, std::ostream& os)
{
    os <<
	"     case " << std::dec << s.hash << ":\n"
	"\treturn " << getNamespace(s) << '_' << s.name << "_unmarshaller(is);\n\n";
}

// This function generates the .h and .cpp files for a single
// protocol. It does this by opening the two output files and then
// calling the two functions that actually generate the content for
// each file type.

static void dumpProtocol(Protocol const& p)
{
    if (cppGenFiles == cppBoth || cppGenFiles == cppHeader) {
	std::string hdrName = targetPath + "/" + p.name + ".h";
	std::ofstream hdr(hdrName.c_str());

	hdr <<
	    "// Generated by the protocol compiler version " << pcVersion <<
	    "\n"
	    "// DO NOT EDIT THIS FILE DIRECTLY!\n\n"
	    "#ifndef _" << toUpper(p.name) << "_PROTOCOL_H_\n"
	    "#define _" << toUpper(p.name) << "_PROTOCOL_H_\n\n"
	    "#include <iostream>\n"
	    "#include <iterator>\n"
	    "#include <vector>\n"
	    "#include <string>\n"
	    "#include <memory>\n";

	if (cppLevel == cpp_exp)
	    hdr << "#include <experimental/optional>";

	hdr <<
	    "\n"
	    "namespace protocol {\n"
	    "    typedef std::ostream::char_type byte;\n"
	    "    typedef std::basic_ostream<protocol::byte> ostream;\n"
	    "    typedef std::basic_istream<protocol::byte> istream;\n\n"
	    "    namespace " << p.name << " {\n\n";

	dumpHeaderFile(p, hdr, 2);

	hdr << "    }\n"
	    "}\n\n"
	    "#endif\n";
    }

    if (cppGenFiles == cppBoth || cppGenFiles == cppSource) {
	std::ofstream src((targetPath + "/" + p.name + ".cpp").c_str());

	src <<
	    "// Generated by the protocol compiler version " << pcVersion <<
	    "\n"
	    "// DO NOT EDIT THIS FILE DIRECTLY!\n\n"
	    "#include <arpa/inet.h>\n"
	    "#include <stdexcept>\n";
	if (p.maxNumberOfFields() > 32)
	    src << "#include <bitset>\n";
	src <<
	    "#include \"" << p.name << ".h\"\n\n"
	    "namespace protocol {\n"
	    "namespace "<< p.name << " {\n\n";

	dumpSourceFile(p, src);

	src <<
	    "}\n"
	    "}\n";
    }
}

static void static_prototypes(Protocol const&, Struct const& s,
			      std::ostream& os)
{
    if (s.isUserDefined() && s.used) {
	os << "static void marshal(protocol::ostream&, " << fqName(s) <<
	    " const&);\n";
	os << "static void unmarshal(protocol::istream&, " << fqName(s) << "&);\n";
    }
}

static void dumpBase(std::ostream& os, StructList const& list)
{
    if (list.size()) {
	std::string ns = getNamespace(list.front());

	// Never pass types array to this function

	assert(!ns.empty());

	os <<
	    "\n" <<
	    ns << "::Base::~Base()\n"
	    "{\n"
	    "}\n\n";
	os <<
	    ns << "::Receiver::~Receiver()\n"
	    "{\n"
	    "}\n\n";
    }
}

static void dumpBaseUnmarshaller(Protocol const& p, std::ostream& os, StructList const& list)
{
    if (list.size()) {
	std::string ns = getNamespace(list.front());

	// Never pass types array to this function

	assert(!ns.empty());

	os <<
	    ns << "::Base::Ptr " << ns << "::Base::unmarshal(protocol::istream& is)\n"
	    "{\n"
	    "    class exMan {\n"
	    "      std::ios::iostate const orig;\n"
	    "      protocol::istream& is;\n"
	    "     public:\n"
	    "      explicit exMan(protocol::istream& s) : orig(s.exceptions()), is(s)\n"
	    "      {\n"
	    "        is.exceptions(std::ios::failbit | std::ios::badbit);\n"
	    "        std::noskipws(is);\n"
	    "      }\n"
	    "	   ~exMan() { is.exceptions(orig); }\n"
	    "    } em(is);\n"
	    "    protocol::byte buf[4];\n\n"
	    "    if (!is.read(buf, sizeof(buf)))\n"
	    "\tthrow std::runtime_error(\"couldn't read header\");\n"
	    "    if (buf[0] != 'S' || buf[1] != 'D' || buf[2] != 'D' || buf[3] != 2)\n"
	    "\tthrow std::runtime_error(\"packet has invalid header\");\n"
	    "    ssize_t const total = readLength(is, 0x50);\n"
	    "    if (total != 3)\n"
	    "\tthrow std::runtime_error(\"'" << p.name << "' protocol packet has invalid length\");\n\n"
	    "    int32_t proto;\n\n"
	    "    protocol::" << p.name << "::unmarshal(is, proto);\n"
	    "    if (proto != (int32_t) 0x" << std::hex << p.longHash(p.name) << ")\n"
	    "\tthrow std::runtime_error(\"unknown protocol\");\n\n"
	    "    int16_t message;\n\n"
	    "    protocol::" << p.name << "::unmarshal(is, message);\n"
	    "    switch (message) {\n";

	apply(p, os, list.begin(), list.end(), dumpMsgUnmarshaller);

	os <<
	    "     default:\n"
	    "\tthrow std::runtime_error(\"invalid " << ns << " for '" << p.name << "' protocol\");\n"
	    "    }\n"
	    "}\n\n";
    }
}

// This function generates the .cpp contents for a protocol.

static void dumpSourceFile(Protocol const& p, std::ostream& os)
{
    apply(p, os, p.types.begin(), p.types.end(), static_prototypes);
    apply(p, os, p.requests.begin(), p.requests.end(), static_prototypes);
    apply(p, os, p.replies.begin(), p.replies.end(), static_prototypes);

    dumpBase(os, p.requests);
    dumpBase(os, p.replies);

    os <<
	"static void emitRawInt(protocol::ostream& os, protocol::byte const tag, int64_t const val)\n"
	"{\n"
	"    protocol::byte buf[1 + sizeof(int64_t)];\n\n"
	"    if (val >= -0x80 && val <= 0x7f) {\n"
	"\tbuf[0] = protocol::byte((tag & 0xf0) + 1);\n"
	"\tbuf[1] = protocol::byte(val);\n"
	"\tos.write(buf, 2);\n"
	"    } else if (val >= -0x8000 && val <= 0x7fff) {\n"
	"\tbuf[0] = protocol::byte((tag & 0xf0) + 2);\n"
	"\tbuf[1] = protocol::byte(val >> 8);\n"
	"\tbuf[2] = protocol::byte(val);\n"
	"\tos.write(buf, 3);\n"
	"    } else if (val >= -0x800000 && val <= 0x7fffff) {\n"
	"\tbuf[0] = protocol::byte((tag & 0xf0) + 3);\n"
	"\tbuf[1] = protocol::byte(val >> 16);\n"
	"\tbuf[2] = protocol::byte(val >> 8);\n"
	"\tbuf[3] = protocol::byte(val);\n"
	"\tos.write(buf, 4);\n"
	"    } else if (val >= -0x80000000ll && val <= 0x7fffffffll) {\n"
	"\tbuf[0] = protocol::byte((tag & 0xf0) + 4);\n"
	"\tbuf[1] = protocol::byte(val >> 24);\n"
	"\tbuf[2] = protocol::byte(val >> 16);\n"
	"\tbuf[3] = protocol::byte(val >> 8);\n"
	"\tbuf[4] = protocol::byte(val);\n"
	"\tos.write(buf, 5);\n"
	"    } else if (val >= -0x8000000000ll && val <= 0x7fffffffffll) {\n"
	"\tbuf[0] = protocol::byte((tag & 0xf0) + 5);\n"
	"\tbuf[1] = protocol::byte(val >> 32);\n"
	"\tbuf[2] = protocol::byte(val >> 24);\n"
	"\tbuf[3] = protocol::byte(val >> 16);\n"
	"\tbuf[4] = protocol::byte(val >> 8);\n"
	"\tbuf[5] = protocol::byte(val);\n"
	"\tos.write(buf, 6);\n"
	"    } else if (val >= -0x800000000000ll && val <= 0x7fffffffffffll) {\n"
	"\tbuf[0] = protocol::byte((tag & 0xf0) + 6);\n"
	"\tbuf[1] = protocol::byte(val >> 40);\n"
	"\tbuf[2] = protocol::byte(val >> 32);\n"
	"\tbuf[3] = protocol::byte(val >> 24);\n"
	"\tbuf[4] = protocol::byte(val >> 16);\n"
	"\tbuf[5] = protocol::byte(val >> 8);\n"
	"\tbuf[6] = protocol::byte(val);\n"
	"\tos.write(buf, 7);\n"
	"    } else if (val >= -0x80000000000000ll && val <= 0x7fffffffffffffll) {\n"
	"\tbuf[0] = protocol::byte((tag & 0xf0) + 7);\n"
	"\tbuf[1] = protocol::byte(val >> 48);\n"
	"\tbuf[2] = protocol::byte(val >> 40);\n"
	"\tbuf[3] = protocol::byte(val >> 32);\n"
	"\tbuf[4] = protocol::byte(val >> 24);\n"
	"\tbuf[5] = protocol::byte(val >> 16);\n"
	"\tbuf[6] = protocol::byte(val >> 8);\n"
	"\tbuf[7] = protocol::byte(val);\n"
	"\tos.write(buf, 8);\n"
	"    } else {\n"
	"\tbuf[0] = protocol::byte((tag & 0xf0) + 8);\n"
	"\tbuf[1] = protocol::byte(val >> 56);\n"
	"\tbuf[2] = protocol::byte(val >> 48);\n"
	"\tbuf[3] = protocol::byte(val >> 40);\n"
	"\tbuf[4] = protocol::byte(val >> 32);\n"
	"\tbuf[5] = protocol::byte(val >> 24);\n"
	"\tbuf[6] = protocol::byte(val >> 16);\n"
	"\tbuf[7] = protocol::byte(val >> 8);\n"
	"\tbuf[8] = protocol::byte(val);\n"
	"\tos.write(buf, 9);\n"
	"    }\n"
	"}\n\n"
	"static int64_t consumeRawInt(protocol::istream& is, protocol::byte const expTag)\n"
	"{\n"
	"    protocol::byte tag;\n\n"
	"    if (is >> tag) {\n"
	"\tint const len = tag & 0xf;\n\n"
	"\tif ((tag & 0xf0) == (expTag & 0xf0) && len > 0 && len <= 8) {\n"
	"\t    int32_t val[2];\n\n"
	"\t    if (is.read(reinterpret_cast<protocol::byte*>(&val), len))\n"
	"\t\treturn ((int64_t(ntohl(val[0])) << 32) + int64_t(ntohl(val[1]))) >>\n"
	"\t\t    ((sizeof(int64_t) - len) * 8);\n"
	"\t} else\n"
	"\t    throw std::runtime_error(\"bad tag value\");\n"
	"    }\n"
	"    throw std::runtime_error(\"unexpected end of file\");\n"
	"}\n\n"
	"static size_t readLength(protocol::istream& is, protocol::byte const expTag)\n"
	"{\n"
	"    int64_t const val = consumeRawInt(is, expTag);\n\n"
	"    if (val >= 0 && val <= 2147483647)\n"
	"\treturn size_t(val);\n"
	"    else\n"
	"\tthrow std::runtime_error(\"length field out of range\");\n"
	"}\n\n"
	"static int16_t readFieldLabel(protocol::istream& is, protocol::byte const expTag)\n"
	"{\n"
	"    int64_t const val = consumeRawInt(is, expTag);\n\n"
	"    if (val >= -32768 && val <= 32767)\n"
	"\treturn int16_t(val);\n"
	"    else\n"
	"\tthrow std::runtime_error(\"field out of range\");\n"
	"}\n\n";

    // Marshalling/Unmarshalling primitives

    if (p.needed("bool"))
	os <<
	    "static void marshal(protocol::ostream& os, bool const& v)\n"
	    "{\n"
	    "    os.put(v ? 0x71 : 0x70);\n"
	    "}\n\n"
	    "static void unmarshal(protocol::istream& is, bool& v)\n"
	    "{\n"
	    "    protocol::byte tmp;\n\n"
	    "    if (is >> tmp) {\n"
	    "\tswitch (tmp) {\n"
	    "\t case 0x70: v = false; break;\n"
	    "\t case 0x71: v = true; break;\n"
	    "\t default: throw std::runtime_error(\"Bad bool tag\");\n"
	    "\t}\n"
	    "    } else\n"
	    "\tthrow std::runtime_error(\"Unexpected end of file\");\n"
	    "}\n\n";

    if (p.needed("int16"))
	os <<
	    "static void marshal(protocol::ostream& os, int16_t const& v)\n"
	    "{\n"
	    "    emitRawInt(os, 0x10, v);\n"
	    "}\n\n";

    // We always need the int16 unmarshaller for field tags

    os <<
	"static void unmarshal(protocol::istream& is, int16_t& v)\n"
	"{\n"
	"    int64_t const vv = consumeRawInt(is, 0x10);\n\n"
	"    if (vv >= -0x8000 && vv <= 0x7fff)\n"
	"\tv = int16_t(vv);\n"
	"    else\n"
	"\tthrow std::runtime_error(\"int16 out of range\");\n"
	"}\n\n";

    if (p.needed("int32"))
	os <<
	    "static void marshal(protocol::ostream& os, int32_t const& v)\n"
	    "{\n"
	    "    emitRawInt(os, 0x10, v);\n"
	    "}\n\n";

    // We always need the int32 unmarshaller for the protocol tag

    os <<
	"static void unmarshal(protocol::istream& is, int32_t& v)\n"
	"{\n"
	"    int64_t const vv = consumeRawInt(is, 0x10);\n\n"
	"    if (vv >= -0x80000000ll && vv <= 0x7fffffffll)\n"
	"\tv = int32_t(vv);\n"
	"    else\n"
	"\tthrow std::runtime_error(\"int32 out of range\");\n"
	"}\n\n";

    if (p.needed("int64"))
	os <<
	    "static void marshal(protocol::ostream& os, int64_t const& v)\n"
	    "{\n"
	    "    emitRawInt(os, 0x10, v);\n"
	    "}\n\n"
	    "static void unmarshal(protocol::istream& is, int64_t& v)\n"
	    "{\n"
	    "    v = consumeRawInt(is, 0x10);\n"
	    "}\n\n";

    if (p.needed("double"))
	os <<
	    "static void marshal(protocol::ostream& os, double const& v)\n"
	    "{\n"
	    "    protocol::byte buf[9];\n"
	    "    int64_t vv = *reinterpret_cast<int64_t const*>(&v);\n\n"
	    "    buf[0] = 0x28;\n"
	    "    for (size_t ii = 8; ii > 0; --ii, vv >>= 8)\n"
	    "        buf[ii] = protocol::byte(vv);\n"
	    "    os.write(buf, sizeof(buf));\n"
	    "}\n\n"
	    "static void unmarshal(protocol::istream& is, double& v)\n"
	    "{\n"
	    "    protocol::byte tag;\n\n"
	    "    if (is >> tag) {\n"
	    "        if (tag == 0x28) {\n"
	    "            int64_t val = 0l;\n"
	    "            union {\n"
	    "                double vDbl;\n"
	    "                int64_t vInt;\n"
	    "            } tmp __attribute__((aligned(8)));\n\n"
	    "            for (size_t ii = 0; ii < 8; ++ii) {\n"
	    "                protocol::byte ch;\n\n"
	    "                if (!(is >> ch))\n"
	    "                    throw std::runtime_error(\"end of file\");\n"
	    "                val = (val * 256) + uint8_t(ch);\n"
	    "            }\n"
	    "            tmp.vInt = val;\n"
	    "            v = tmp.vDbl;\n"
	    "        } else\n"
	    "            throw std::runtime_error(\"Bad double tag\");\n"
	    "    } else\n"
	    "        throw std::runtime_error(\"end of file\");\n"
	    "}\n\n";

    if (p.needed("string"))
	os <<
	    "static void marshal(protocol::ostream& os, std::string const& v)\n"
	    "{\n"
	    "    emitRawInt(os, 0x40, v.size());\n"
	    "    os.write(v.data(), v.size());\n"
	    "}\n\n"
	    "static void unmarshal(protocol::istream& is, std::string& v)\n"
	    "{\n"
	    "    size_t const len = readLength(is, 0x40);\n"
	    "    std::string tmp;\n\n"
	    "    tmp.resize(len);\n"
	    "    is.read(&tmp[0], len);\n"
	    "    v.swap(tmp);\n"
	    "}\n\n";

    os <<
	"template<typename T>\n"
	"void marshal(protocol::ostream& os, std::vector<T> const& v)\n"
	"{\n"
	"    emitRawInt(os, 0x50, v.size());\n";

    if (cppLevel == cppLegacy)
	os <<
	    "    typename std::vector<T>::const_iterator ii = v.begin();\n\n"
	    "    for (; ii != v.end(); ++ii)\n"
	    "	marshal(os, *ii);\n";
    else
	os <<
	    "    for (auto const& ii : v)\n"
	    "	marshal(os, ii);\n";

    os <<
	"}\n\n"
	"template <typename T>\n"
	"void unmarshal(protocol::istream& is, std::vector<T>& v)\n"
	"{\n"
	"    std::vector<T> vec;\n"
	"    size_t len = readLength(is, 0x50);\n\n"
	"    vec.reserve(len);\n"
	"    while (len--) {\n"
	"\tT tmp;\n\n"
	"\tunmarshal(is, tmp);\n"
	"\tvec.push_back(" <<
	(cppLevel == cppLegacy ? "tmp" : "std::move(tmp)") << ");\n"
	"    }\n"
	"    v.swap(vec);\n"
	"}\n\n";

    if (p.needed("binary"))
	os <<
	    "template <>\n"
	    "void marshal(protocol::ostream& os, std::vector<uint8_t> const& v)\n"
	    "{\n"
	    "    emitRawInt(os, 0x30, v.size());\n"
	    "    os.write(reinterpret_cast<protocol::byte const*>(&v[0]), v.size());\n"
	    "}\n\n"
	    "template <>\n"
	    "void unmarshal(protocol::istream& is, std::vector<uint8_t>& v)\n"
	    "{\n"
	    "    size_t const len = readLength(is, 0x30);\n"
	    "    std::vector<uint8_t> tmp;\n\n"
	    "    tmp.resize(len);\n"
	    "    is.read(reinterpret_cast<protocol::byte*>(&tmp[0]), len);\n"
	    "    v.swap(tmp);\n"
	    "}\n\n";

    apply(p, os, p.types.begin(), p.types.end(), constructors);

    apply(p, os, p.types.begin(), p.types.end(), struct_unmarshaller);
    apply(p, os, p.requests.begin(), p.requests.end(), struct_unmarshaller);
    apply(p, os, p.replies.begin(), p.replies.end(), struct_unmarshaller);

    apply(p, os, p.types.begin(), p.types.end(), dumpStructMarshaller);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpStructMarshaller);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpStructMarshaller);

    apply(p, os, p.types.begin(), p.types.end(), dumpStructSwap);

    apply(p, os, p.requests.begin(), p.requests.end(), dumpMessageSource);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpMessageSource);

    dumpBaseUnmarshaller(p, os, p.requests);
    dumpBaseUnmarshaller(p, os, p.replies);
}

// Dumps all the fields of a structure. It assumes the stream is
// "ready" for the fields (i.e. the C++ prologue codes has already
// been written and the epilogue will be written at a later time.

static void dumpStructFields(Protocol const& p, Struct const& s, std::ostream& os, size_t indent)
{
    apply(p, os, s.fields.begin(), s.fields.end(), dumpField, indent);
}

#define IPL	6

static void dumpConstructor(Struct const& s, std::ostream& os, int indentLevel)
{
    os << indent(indentLevel) << s.name << "() ";

    FieldList::const_iterator ii = s.fields.begin();
    bool zero = true;
    size_t total = 0;

    for (; ii != s.fields.end(); ++ii)
	if (!ii->optional && !ii->array && isPrimitive(ii->type)) {
	    os << (zero ? ":\n" + indent(indentLevel + 1) :
		   (++total == IPL ? ",\n" + indent(indentLevel + 1) : ", "));
	    zero = false;
	    total %= IPL;
	    os << ii->name << "(" <<
		(ii->type == "bool" ? "false" : (ii->type == "double" ? "0." : "0")) <<
		")";
	}

    os << '\n' << indent(indentLevel + 1) << "{}\n";
}

// Dumps a "type" to the stream. A type is a structure that isn't part
// of the protocol object hierarchy.

static void dumpType(Protocol const& p, Struct const& s, std::ostream& os,
		     int indentLevel)
{
    if (s.isUserDefined()) {
	if (s.use == Struct::asEnum) {
	    os << indent(indentLevel) << (cppLevel == cppLegacy ? "enum " : "enum class ") << s.name
	       << " {\n";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii) {
		if (ii != s.fields.begin())
		    os << ",\n";
		os <<
		    indent(indentLevel + 1) << ii->name;
	    }

	    os << '\n' << indent(indentLevel) << "};\n\n";
	} else {
	    os << indent(indentLevel) << "struct " << s.name << " {\n";

	    dumpStructFields(p, s, os, indentLevel + 1);

	    os << "\n";

	    dumpConstructor(s, os, indentLevel + 1);
	    os << indent(indentLevel + 1) <<
		"void marshal(protocol::ostream& os) const;\n";

	    if (s.hasOptionals()) {
		os <<
		    indent(indentLevel + 1) << s.name << "(" << s.name
					    << " const&);\n";
		if (cppLevel != cpp_exp)
		    os <<
			indent(indentLevel + 1) << s.name << "& operator=("
						<< s.name << " const&)"
						<< no_except() << ";\n";
	    }

	    os <<
		indent(indentLevel + 1) << "void swap(" << s.name << "&)" <<
		no_except() << ";\n";

	    dumpEqualityOperators(p, s, os, indentLevel + 1);

	    os << indent(indentLevel) << "};\n\n";
	}
    }
}

static void dumpStructMarshaller(Protocol const& p, Struct const& s,
				 std::ostream& os)
{
    if (s.isUserDefined() && s.isUsed()) {
	if (s.use == Struct::asEnum) {
	    os <<
		"static void marshal(protocol::ostream& os, "
	       << fqName(s)
	       << " const& v)\n"
		"{\n"
		"    switch (v) {\n";

	    for (FieldList::const_iterator ii = s.fields.begin();
		 ii != s.fields.end(); ++ii) {
		std::string const eName = cppLevel == cppLegacy ?
		    ii->name : fqName(s) + "::" + ii->name;

		os <<
		    "     case " << eName << ":\n";
		emitConstantInt(os, 2, 0x80, ii->hash);
		os <<
		    "\tbreak;\n\n";
	    }

	    if (cppLevel == cppLegacy)
		os <<
		    "     default:\n"
		    "\tthrow std::runtime_error(\"illegal enum value\");\n";

	    os <<
		"    }\n"
		"}\n\n";
	} else {
	    FieldList::const_iterator ii = s.fields.begin();
	    size_t const required = countRequired(s);

	    os <<
		"static void marshal(protocol::ostream& os, " << fqName(s) << " const&" <<
		(s.fields.size() ? " v" : "") << ")\n"
		"{\n";

	    if (required == s.fields.size())
		emitConstantInt(os, 1, 0x50, 2 * required);
	    else {
		os <<
		    "    {\n"
		    "\tsize_t fields = " << std::dec << (2 * required) << ";\n\n";

		for (; ii != s.fields.end(); ++ii) {
		    Field const& f = *ii;

		    if (f.optional)
			os << "\tif (" <<
			    optType->emit_has_value("v." + f.name) << ")\n" <<
			    "\t    fields += 2;\n";
		}
		os <<
		    "\temitRawInt(os, 0x50, fields);\n"
		    "    }\n\n";
	    }

	    for (ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		Field const& f = *ii;

		if (f.optional) {
		    os << "    if (" <<
			optType->emit_has_value("v." + f.name) << ") {\n";
		    emitConstantInt(os, 2, 0x10, f.hash);
		    os <<
			"\tmarshal(os, *v." << f.name << ");\n"
			"    }\n";
		} else {
		    emitConstantInt(os, 1, 0x10, f.hash);
		    os <<
			"    marshal(os, v." << f.name << ");\n";
		}
	    }
	    os <<
		"}\n\n"
		"void " << fqName(s) << "::marshal(protocol::ostream& os) const\n"
		"{\n";

	    if (s.use == Struct::asRequest || s.use == Struct::asReply) {
		os <<
		    "    class exMan {\n"
		    "      std::ios::iostate const orig;\n"
		    "      protocol::ostream& os;\n"
		    "     public:\n"
		    "      explicit exMan(protocol::ostream& s) : orig(s.exceptions()), os(s)\n"
		    "      {\n"
		    "        os.exceptions(std::ios::failbit | std::ios::badbit);\n"
		    "        std::noskipws(os);\n"
		    "      }\n"
		    "	   ~exMan() { os.exceptions(orig); }\n"
		    "    } em(os);\n\n"
		    "    os << \"SDD\\x02\\x51\\x03\";\n";
		emitConstantInt(os, 1, 0x10, p.longHash(p.name));
		emitConstantInt(os, 1, 0x10, s.hash);
	    }

	    os <<
		"    protocol::" << p.name << "::marshal(os, *this);\n"
		"}\n\n";
	}
    }
}

static void unmarshalField(Protocol const&, Field const& f, std::ostream& os,
			   size_t* current)
{
    os << "\t case " << std::dec << f.hash << ":\n";

    if (f.optional)
	os <<
	    optType->emit_unmarshal(f, "v." + f.name, "\t    ", "\t\t") <<
	    '\n';
    else
	os <<
	    "\t    unmarshal(is, v." << f.name << ");\n"
	    "\t    flg |= 0x" << std::hex << (1 << (*current)++) << ";\n";

    os << "\t    break;\n\n";
}

static void unmarshalFieldLarge(Protocol const&, Field const& f,
				std::ostream& os, size_t* current)
{
    os << "\t case " << std::dec << f.hash << ":\n";

    if (f.optional)
	os <<
	    optType->emit_unmarshal(f, "v." + f.name, "\t    ", "\t\t") <<
	    '\n';
    else
	os <<
	    "\t    unmarshal(is, v." << f.name << ");\n"
	    "\t    flg.set(" << std::dec << (*current)++ << ");\n";

    os << "\t    break;\n\n";
}

// Takes a Field and returns the SDD mapping of the Field's type.

static std::string xlatType(std::string const& type)
{
    if (type == "int16")
	return "int16_t";
    else if (type == "int32")
	return "int32_t";
    else if (type == "int64")
	return "int64_t";
    else if (type == "double")
	return "double";
    else if (type == "string")
	return "std::string";
    else if (type == "binary")
	return "std::vector<uint8_t>";
    else
	return type;
}

static std::string getNamespace(Struct const& s)
{
    switch (s.use) {
     case Struct::asRequest:
	return "request";

     case Struct::asReply:
	return "reply";

     default:
	return "";
    }
}

static std::string fqName(Struct const& s)
{
    std::string ns = getNamespace(s);

    return ns.empty() ? s.name : ns + "::" + s.name;
}

// The only externally visible function from this module. This is the
// entry point to generate C++ files that handle the protocol.

void generateCPlusPlus(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("C++ generator only supports SDDv2");

    switch (cppLevel) {
     case cppLegacy:
	optType = new OptTypeAutoPtr();
	break;

     case cpp11:
     case cpp14:
     case cpp17:
	optType = new OptTypeUniquePtr();
	break;

     case cpp_exp:
	optType = new OptTypeOptional();
	break;
    }

    dumpProtocol(p);
    delete optType;
}
