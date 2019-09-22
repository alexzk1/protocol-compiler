// Fermilab Software Legal Information (BSD License)
// Copyright (c) 2008-2017, 2019, Fermi Research Alliance, LLC
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

// This module generates Objective-C files that can communicate using a
// specified protocol. The .proto grammar gets mapped to Objective-C in the
// following way:
//
//    enum	 -> enum
//    bool       -> BOOL
//    int16      -> SInt16
//    int32      -> SInt32
//    int64      -> SInt64
//    double     -> double
//    string     -> NSString *
//    binary     -> NSData *
//    array of T -> messages to access array of T
//    T          -> @interface T   (where T is a type defined earlier)
//
// To make the names of structures and messages and fields common to
// all target languages, the first character of the protocol and
// message names should be uppercase and the field names should begin
// with a lowercase letter.

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include "pc.h"

// Prototypes.

static std::string xlatType(std::string const&);

static inline std::string const indent(int indentLevel)
{
    return std::string(indentLevel * 4, ' ');
}

static bool isBuiltinType(std::string const& type)
{
    return (type == "bool" || type == "int16" || type == "int32" ||
	type == "int64" || type == "double" || type == "string" ||
	type == "binary");
}

static bool userDefinedType(std::string const& type)
{
    return !isBuiltinType(type);
}

static bool isNumeric(std::string const& type)
{
    return type == "bool" ||
	    type == "int16" ||
	    type == "int32" ||
	    type == "int64" ||
	    type == "double";
}

static bool isNumeric(Field const& f)
{
    return isNumeric(f.type);
}

static std::string xlatType(std::string const& type)
{
    if (type == "bool")
	return "BOOL";
    else if (type == "int16")
	return "SInt16";
    else if (type == "int32")
	return "SInt32";
    else if (type == "int64")
	return "SInt64";
    else if (type == "double")
	return "double";
    else if (type == "string")
	return "NSString *";
    else if (type == "binary")
	return "NSData *";
    else
	return type;
}

static void emitConstantInt(std::ostream& os, int indentLevel, uint8_t tag,
			    int64_t const val)
{
    std::ostringstream tmp;

    emitRawInt(tmp, tag, val);
    os <<
	indent(indentLevel) << "{\n" <<
	indent(indentLevel + 1) << "static uint8_t const _d_a_t_a_[] = {\n" <<
	indent(indentLevel + 2);

    std::string const s = tmp.str();

    for (std::string::const_iterator ii = s.begin(); ii != s.end(); ++ii) {
	if (ii != s.begin())
	    os << ", ";
	os << std::dec << (unsigned)(uint8_t) *ii;
    }

    os << "\n" <<
	indent(indentLevel + 1) << "};\n\n" <<
	indent(indentLevel + 1) << "putbuf(os, _d_a_t_a_, sizeof(_d_a_t_a_));\n" <<
	indent(indentLevel) << "}\n";
}

// Takes a Field and returns the SDD mapping of the Field's type.

static std::string xlatType(Field const& f)
{
    return xlatType(f.type);
}

static std::string backingVarName(Field const& f)
{
    return "__" + f.name;
}

static std::string messageType(Struct const& s)
{
    switch (s.use) {
     case Struct::asRequest:
	return "Request";

     case Struct::asReply:
	return "Reply";

     default:
	return "Struct";
    }
}

static std::string fqName(Protocol const& p, Struct const& s)
{
    if (s.anonymous)
	return p.name + "_" + s.name + "_struct";
    else {
	std::string tmp = p.name + "_" + s.name;

	switch (s.use) {
	 case Struct::asRequest:
	    return tmp + "request";

	 case Struct::asReply:
	    return tmp + "reply";

	 case Struct::asStruct:
	    return tmp + "struct";

	 default:
	    return tmp;
	}
    }
}

static void dumpMessageInterface(Protocol const& p, Struct const& s,
				 std::ostream& os, int il)
{
    if (s.isUserDefined()) {
	if (s.use == Struct::asEnum) {
	    os <<
		indent(il) << "enum " << fqName(p, s) << " {\n";

		for (FieldList::const_iterator ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		    Field const& f = *ii;

		    if (ii != s.fields.begin())
			os << ",\n";

		    os <<
			indent(il + 1) << p.name << "_" << s.name << "_" << f.name << " = " << f.hash;
		}

	    os <<
		indent(il) << "\n};\n\n";
	} else {
	    os <<
		indent(il) << "\n// " << s.name << " " << messageType(s) << "\n\n" <<
		indent(il) << "@interface " << fqName(p, s) << " : " << p.name << " {\n" <<
		indent(il) << "}\n";

	    if (s.fields.size()) {
		os << "\n";

		FieldList::const_iterator ii;

		// Iterate through non-array fields first

		for (ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		    Field const& f = *ii;

		    if (!f.array) {
			if (userDefinedType(f.type)) {
			    if (isEnum(p, f.type))
				os <<
				    indent(il) << "@property (readwrite) enum " << p.name << "_" << f.type << ' ' << f.name << ";\n";
			    else if (objcUseARC)
				os <<
				    indent(il) << "@property (readwrite,strong) " << p.name << "_" << f.type << "_struct *" << f.name << ";\n";
			    else
				os <<
				    indent(il) << "@property (readwrite,retain) " << p.name << "_" << f.type << "_struct *" << f.name << ";\n";
			} else {
			    if (isNumeric(f.type))
				os <<
				    indent(il) << "@property (readwrite) " << xlatType(f) << ' ' << f.name << ";\n";
			    else if (objcUseARC)
				os <<
				    indent(il) << "@property (readwrite,strong) " << xlatType(f) << f.name << ";\n";
			    else
				os <<
				    indent(il) << "@property (readwrite,retain) " << xlatType(f) << f.name << ";\n";
			}
		    }
		}

		// Now add interfaces for arrays

		for (ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		    Field const& f = *ii;

		    if (f.array) {
			std::string typeName;

			if (userDefinedType(f.type)) {
			    if (isEnum(p, f.type))
				typeName = "enum " + p.name + "_" + f.type;
			    else
				typeName = p.name + "_" + f.type + "_struct *";
			} else
			    typeName = xlatType(f);

			os <<
			    "\n" <<
			    indent(il) << "- (void)" << f.name << "ArrayWithCapacity:(NSUInteger)numItems;\n" <<
			    indent(il) << "- (void)" << f.name << "Add:(" << typeName << ")v;\n" <<
			    indent(il) << "- (" << typeName << ")" << f.name << "AtIndex:(NSUInteger)index;\n" <<
			    indent(il) << "- (NSUInteger)" << f.name << "Count;\n";
		    }
		}

		os <<
		    "\n";

		// Now add message for optional fields

		for (ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		    Field const& f = *ii;

		    if (f.optional)
			os <<
			    indent(il) << "- (BOOL)" << f.name << "IsSet;\n";
		}
	    }

	    os <<
		"\n" <<
		indent(il) << "+ (id)instance;\n" <<
		indent(il) << "- (NSData *)marshal;\n" <<
		indent(il) << "+ (" << fqName(p, s) << " *)unmarshal:(NSData *)d;\n\n";

	    os <<
		indent(il) << "@end\n\n";
	}
    }
}

static std::string fieldAccess(Protocol const&p, Struct const& s, Field const& f)
{
    return "((" + fqName(p, s) + "*) v)->" + backingVarName(f);
}

static std::string marshalFunc(Protocol const&p, Field const& f)
{
    if (userDefinedType(f.type) && !isEnum(p, f.type))
	return "marshal_" + f.type + "_struct";
    else
	return "marshal_" + f.type;
}

static std::string unmarshalFunc(Protocol const& p, Field const& f)
{
    if (userDefinedType(f.type) && !isEnum(p, f.type))
	return "unmarshal_" + f.type + "_struct";
    else
	return "unmarshal_" + f.type;
}

static std::string marshalFunc(Struct const& s)
{
    std::string t;

    if (s.use == Struct::asRequest)
	t = "_request";
    else if (s.use == Struct::asReply)
	t = "_reply";
    else if (s.use == Struct::asStruct)
	t = "_struct";
    else
	t = "";

    return "marshal_" + s.name + t;
}

static std::string unmarshalFunc(Struct const& s)
{
    std::string t;

    if (s.use == Struct::asRequest)
	t = "_request";
    else if (s.use == Struct::asReply)
	t = "_reply";
    else if (s.use == Struct::asStruct)
	t = "_struct";
    else
	t = "";

    return "unmarshal_" + s.name + t;
}

static void dumpMessageMarshalFunc(Protocol const& p, Struct const& s,
				   std::ostream& os, int)
{
    if (s.isUserDefined()) {
	if (s.use == Struct::asEnum) {
	    os <<
		"static void " << marshalFunc(s) << "(NSOutputStream* os, id v)\n"
		"{\n"
		"    switch ([v unsignedShortValue]) {\n";

		for (FieldList::const_iterator ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		    Field const& f = *ii;

		    os << "     case 0x" << std::hex<< f.hash << ":\n";
		    emitConstantInt(os, 2, 0x80, f.hash);
		    os << "        break;\n\n";
		}

	    os <<
		"     default:\n"
		"\t@throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"bad enum value\" userInfo:nil];\n"
		"    }\n"
		"}\n\n";
	} else {

	    FieldList::const_iterator ii = s.fields.begin();
	    size_t const required = countRequired(s);

	    os <<
		"static void " << marshalFunc(s) << "(NSOutputStream* const os, id v" << (s.fields.size() == 0 ? " __attribute((unused))" : "") << ")\n"
	    "{\n";

	    if (required == s.fields.size())
		emitConstantInt(os, 1, 0x50, 2 * required);
	    else {
		os <<
		    "    int32_t fields = " << std::dec << (2 * required) << ";\n\n";

		for (; ii != s.fields.end(); ++ii) {
		    Field const& f = *ii;

		    if (f.optional)
			os <<
			    "    if (" << fieldAccess(p, s, f) << " != nil)\n"
			    "        fields += 2;\n";
		}
		os <<
		    "\n"
		    "    emitRawInt(os, 0x50, fields);\n";
	    }

	    for (ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		Field const& f = *ii;

		if (f.optional) {
		    os <<
			"    if (" << fieldAccess(p, s, f) << " != nil) {\n";
		    emitConstantInt(os, 2, 0x10, f.hash);

		    if (f.array)
			os <<
			    "        marshal_array(os, " << fieldAccess(p, s, f) << ", " << marshalFunc(p, f) << ");\n";
		    else
			os <<
			    "        " << marshalFunc(p, f) << "(os, " << fieldAccess(p, s, f) << ");\n";

		    os <<
			"    }\n";
		} else {
		    os <<
			"    if (" << fieldAccess(p, s, f) << " != nil) {\n";
		    emitConstantInt(os, 2, 0x10, f.hash);
		    if (f.array)
			os <<
			    "        marshal_array(os, " << fieldAccess(p, s, f) << ", " << marshalFunc(p,f) << ");\n";
		    else
			os <<
			    "        " << marshalFunc(p, f) << "(os, " << fieldAccess(p, s, f) << ");\n";
		    os <<
			"    } else\n"
			"\t@throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"missing required field\" userInfo:nil];\n";
		}

	    }
	    os <<
		"}\n\n";
	}
    }
}

static std::string nsNumberAccessor(Field const& f)
{
    if (f.type == "bool")
	return "boolValue";
    else if (f.type == "double")
	return "doubleValue";

    return "longLongValue";
}

static std::string nsNumberCreator(Field const& f)
{
    if (f.type == "bool")
	return "numberWithBool";
    else if (f.type == "double")
	return "numberWithDouble";

    return "numberWithLongLong";
}

static void dumpFieldAccessors(Protocol const& p, Field const& f, std::ostream& os)
{
    std::string typeName;

    if (userDefinedType(f.type)) {
	if (isEnum(p, f.type))
	    typeName = "enum " + p.name + "_" + f.type;
	else
	    typeName = p.name + "_" + f.type + "_struct *";
    } else
	typeName = xlatType(f);

    if (f.array) {

	// Create array

	os <<
	    "- (void)" << f.name << "ArrayWithCapacity:(NSUInteger)numItems\n" <<
	    "{\n";

	if (!objcUseARC)
	    os << "    [" << backingVarName(f) << " release];\n";

	if (objcUseARC)
	    os << "    " << backingVarName(f) << " = [NSMutableArray arrayWithCapacity:numItems];\n";
	else
	    os << "    " << backingVarName(f) << " = [[NSMutableArray arrayWithCapacity:numItems] retain];\n";

	os <<
	    "}\n\n";

	// Add to array

	os <<
	    "- (void)" << f.name << "Add:(" << typeName << ")v\n" <<
	    "{\n";

	if (isNumeric(f) || isEnum(p, f.type))
	    os <<
		"    [" << backingVarName(f) << " addObject:[NSNumber " << nsNumberCreator(f) << ":v]];\n";
	else
	    os <<
		"    [" << backingVarName(f) << " addObject:v];\n";

	os <<
	    "}\n\n";

	// Get from array

	os <<
	    "- (" << typeName << ")" << f.name << "AtIndex:(NSUInteger)index\n" <<
	    "{\n";

	if (isNumeric(f) || isEnum(p, f.type))
	    os <<
		"    return (" << typeName << ") [[" << backingVarName(f) << " objectAtIndex:index] " << nsNumberAccessor(f) << "];\n";
	else
	    os <<
		"    return [" << backingVarName(f) << " objectAtIndex:index];\n";
	os <<
	    "}\n\n";

	// Array size

	os <<
	    "- (NSUInteger)" << f.name << "Count\n" <<
	    "{\n" <<
	    "    return [" << backingVarName(f) << " count];\n" <<
	    "}\n\n";

    } else if (isNumeric(f) || isEnum(p, f.type)) {
	os <<
	    "- (" << typeName << ")" << f.name << "\n" <<
	    "{\n" <<
	    "    return (" << typeName << ")[" << backingVarName(f) << " " << nsNumberAccessor(f) << "];\n" <<
	    "}\n\n";

	std::string s = f.name;
	s[0] = toupper(s[0]);

	os <<
	    "- (void)set" << s << ":(" << typeName << ")v\n" <<
	    "{\n";

	if (!objcUseARC)
	    os << "    [" << backingVarName(f) << " release];\n";

	if (objcUseARC)
	    os << "    " << backingVarName(f) << " = [NSNumber " << nsNumberCreator(f) << ":v];\n";
	else
	    os << "    " << backingVarName(f) << " = [[NSNumber " << nsNumberCreator(f) << ":v] retain];\n";

	os <<
	    "}\n\n";
    } else {
	os <<
	    "- (" << typeName << ")" << f.name << "\n" <<
	    "{\n" <<
	    "    return " << backingVarName(f) << ";\n"
	    "}\n\n";

	std::string s = f.name;
	s[0] = toupper(s[0]);

	os <<
	    "- (void)set" << s << ":(" << typeName << ")v\n" <<
	    "{\n";

	if (!objcUseARC)
	    os << "    [" << backingVarName(f) << " release];\n";

	if (objcUseARC)
	    os << "    " << backingVarName(f) << " = v;\n";
	else
	    os << "    " << backingVarName(f) << " = [v retain];\n";

	os <<
	    "}\n\n";
    }

    if (f.optional)
	os <<
	    "- (BOOL)" << f.name << "IsSet\n" <<
	    "{\n" <<
	    "    return " << backingVarName(f) << " == nil ? NO : YES;\n" <<
	    "}\n\n";
}

static void dumpMessageImplementation(Protocol const& p, Struct const& s, std::ostream& os, int)
{
    if (s.isUserDefined() && s.use != Struct::asEnum) {
	FieldList::const_iterator ii = s.fields.begin();

	os <<
	    "\n"
	    "// " << s.name << " " << messageType(s) << "\n\n" <<
	    "@implementation " << fqName(p, s) << "\n\n";

	if (s.fields.size()) {
	    while (ii != s.fields.end())
		dumpFieldAccessors(p, *ii++, os);
	}

	os <<
	    "+ (id)instance\n"
	    "{\n";

	if (objcUseARC)
	    os << "    return [[" << fqName(p, s) << " alloc] init];\n";
	else
	    os << "    return [[[" << fqName(p, s) << " alloc] init] autorelease];\n";

	os <<
	    "}\n\n";

	// marshal message

	os <<
	    "- (NSData *)marshal\n"
	    "{\n"
	    "    NSOutputStream* const os = [NSOutputStream outputStreamToMemory];\n\n"
	    "    [os open];\n";

	if (s.use == Struct::asRequest || s.use == Struct::asReply) {
	    os <<
		"    static uint8_t const hdr[] = {\n"
		"        'S', 'D', 'D', 2, 0x51, 3\n"
		"    };\n\n"
		"    putbuf(os, hdr, sizeof(hdr));\n";

	    emitConstantInt(os, 1, 0x10, p.longHash(p.name));
	    emitConstantInt(os, 1, 0x10, s.hash);
	}

	std::string t = s.use == Struct::asRequest ? "request" : (s.use == Struct::asReply ? "reply" : "struct");

	os <<
	    "    marshal_" << s.name << "_" << t << "(os, self);\n"
	    "    return [os propertyForKey:NSStreamDataWrittenToMemoryStreamKey];\n"
	    "}\n\n";

	// unmarshal message

	os <<
	    "+ (" << fqName(p, s) << " *)unmarshal:(NSData *)d\n"
	    "{\n";

	os <<
	    "    NSInputStream* const is = [NSInputStream inputStreamWithData:d];\n\n"
	    "    [is open];\n"
	    "    return " << unmarshalFunc(s) << "(is);\n";

	os <<
	    "}\n\n";

	// dealloc message

	if (s.fields.size() && !objcUseARC) {
	    os <<
		"- (void)dealloc\n"
		"{\n";

	    for (ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		Field const& f = *ii;
		os <<
		    "    [" << backingVarName(f) << " release];\n";
	    }

	    os <<
		"    [super dealloc];\n"
		"}\n";
	}

	os <<
	    "@end\n\n";
    }
}

static void dumpMsgUnmarshallerCall(Protocol const&, Struct const& s,
				    std::ostream& os, int il)
{
    os <<
	indent(il) << " case " << std::dec << s.hash << ":\n" <<
	indent(il) << "    return " << unmarshalFunc(s) << "(is);\n\n";
}

static void unmarshalField(Protocol const& p, Field const& f, std::ostream& os, size_t* current)
{
    os << "         case " << std::dec << f.hash << ":\n";

    if (f.array) {
	if (objcUseARC)
	    os << "            v->" << backingVarName(f) << " = unmarshal_array(is, " << unmarshalFunc(p, f) << ");\n";
	else
	    os << "            v->" << backingVarName(f) << " = [unmarshal_array(is, " << unmarshalFunc(p, f) << ") retain];\n";
    } else {
	if (objcUseARC)
	    os << "            v->" << backingVarName(f) << " = " << unmarshalFunc(p, f) << "(is);\n";
	else
	    os << "            v->" << backingVarName(f) << " = [" << unmarshalFunc(p, f) << "(is) retain];\n";
    }

    if (!f.optional)
	os <<
	    "            flg |= 0x" << std::hex << (1 << (*current)++) << ";\n";

    os << "            break;\n\n";
}

static void dumpMsgUnmarshalFunc(Protocol const& p, Struct const& s, std::ostream& os)
{
    if (s.isUserDefined()) {
	if (s.use == Struct::asEnum) {
	    os <<
		"static id " << unmarshalFunc(s) << "(NSInputStream* is)\n"
		"{\n"
		"    switch (consumeRawInt(is, 0x80)) {\n";

		for (FieldList::const_iterator ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		    Field const& f = *ii;

		    os <<
			"     case " << std::dec << f.hash << ":\n"
			"        return [NSNumber numberWithShort:" << f.hash <<
			"];\n\n";
		}

	    os <<
		"     default:\n"
		"        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"bad enum value\" userInfo:nil];\n"
		"    }\n";

	    os <<
		"}\n\n";
	} else {
	    if (s.fields.size()) {
		size_t const total = countRequired(s);
		size_t current = 0;

		os <<
		    "static id " << unmarshalFunc(s) << "(NSInputStream* const is)\n"
		    "{\n";

		if (total > 0)
		    os << "    uint64_t flg = 0;\n";

		if (objcUseARC)
		    os << "    " << fqName(p, s) << "* const v = " << "[[" << fqName(p, s) << " alloc] init];\n";
		else
		    os << "    " << fqName(p, s) << "* const v = " << "[[[" << fqName(p, s) << " alloc] init] autorelease];\n";

		os <<
		    "    NSInteger const total = (NSInteger) consumeRawInt(is, 0x50);\n\n"
		    "    for (ssize_t ii = 0; ii < total; ii += 2) {\n"
		    "        switch (consumeRawInt(is, 0x10)) {\n";

		apply(p, os, s.fields.begin(), s.fields.end(), unmarshalField, &current);

		os <<
		    "         default:\n"
		    "\t    @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"unknown field\" userInfo:nil];\n"
		    "        }\n"
		    "    }\n";

		if (total > 0)
		    os <<
			"    return flg != 0x" << std::hex <<
			(total == 64 ? 0xffffffffffffffffLL : ((1 << total) - 1)) <<
			" ? nil : v;\n";
		else
		    os <<
			"    return v;\n";

		os << "}\n\n";
	    } else {
		os <<
		    "static id " << unmarshalFunc(s) << "(NSInputStream* is)\n"
		    "{\n";

		if (objcUseARC)
		    os << "    return consumeRawInt(is, 0x50) == 0 ? [[" << fqName(p, s) << " alloc] init] : nil;\n";
		else
		    os << "    return consumeRawInt(is, 0x50) == 0 ? [[[" << fqName(p, s) << " alloc] init] autorelease] : nil;\n";

		os <<
		    "}\n\n";
	    }
	}
    }
}

static void dumpHandler(Protocol const& p, Struct const& s,
				 std::ostream& os, int)
{
    os <<
	"- (void)handle_" << fqName(p, s) << ":(" <<
	fqName(p, s) << " *)r;\n";
}

static void dumpClassDeclarations(Protocol const& p, StructList const& l, std::ostream& os)
{
    if (l.size()) {
	int len = 0;

	os << "@class ";

	for (StructList::const_iterator ii = l.begin(); ii != l.end(); ++ii) {
	    Struct const& s = *ii;
	    std::string name = fqName(p, s);

	    if (len > 60) {
		os << ",\n        ";
		len = 0;
	    }

	    os << (len == 0 ? "" : ", ") << name;
	    len += (name.length() + 2);
	}
	os << ";\n\n";
    }
}

static void dumpProtocolInterface(Protocol const& p, StructList const& l, std::string type, std::ostream& os)
{
    os <<
	"@protocol " << p.name << "_" << type << "_handler <NSObject>\n\n"
	"@optional\n";
    apply(p, os, l.begin(), l.end(), dumpHandler, 0);
    os <<
	"\n"
	"@end\n\n";
}

static void dumpHandlerCall(Protocol const& p, Struct const& s,
				 std::ostream& os, int il)
{
    std::string mName = fqName(p, s);
    std::string hName = "handle_" + fqName(p, s);

    os <<
	indent(il) << "if (c == [" << mName << " class] && " <<
			"[h respondsToSelector:@selector(" << hName << ":)]) {\n" <<
	indent(il + 1) << "[h " << hName << ":(" << mName << " *) r];\n" <<
	indent(il + 1) << "return YES;\n" <<
	indent(il) << "}\n";
}

static void dumpHandler(Protocol const& p, StructList const& l, std::string type, std::ostream& os)
{
    os <<
	"+ (BOOL)handle_" << type << ":(NSData *)d h:(id<" << p.name << "_" << type << "_handler>)h\n"
	"{\n"
	"    " << p.name << " *r = [" << p.name << " unmarshal:d];\n\n"
	"    if (r != nil) {\n"
	"        Class c = [r class];\n\n";
    apply(p, os, l.begin(), l.end(), dumpHandlerCall, 2);
    os <<
	"    }\n\n"
	"    return NO;\n"
	"}\n\n";
}

static void dumpHeaderFile(Protocol const& p, std::ostream& os)
{
    os <<
	"// Generated by the protocol compiler version " << pcVersion << "\n"
	"// DO NOT EDIT THIS FILE DIRECTLY!\n\n" <<
	"#import <Foundation/Foundation.h>\n\n";

    dumpClassDeclarations(p, p.requests, os);
    dumpClassDeclarations(p, p.replies, os);
    dumpProtocolInterface(p, p.requests, "request", os);
    dumpProtocolInterface(p, p.replies, "reply", os);

    os <<
	"@interface " << p.name << " : NSObject {\n"
	"}\n\n"
	"- (NSData *)marshal;\n"
	"+ (" << p.name << " *)unmarshal:(NSData *)d;\n"
	"+ (BOOL)handle_request:(NSData *)d h:(id<" << p.name << "_request_handler>)h;\n"
	"+ (BOOL)handle_reply:(NSData *)d h:(id<" << p.name << "_reply_handler>)h;\n\n"
	"@end\n\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpMessageInterface, 0);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpMessageInterface, 0);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpMessageInterface, 0);
}

static void dumpClassExtension(Protocol const& p, Struct const& s,
				 std::ostream& os, int il)
{
    if (s.isUserDefined() && s.use != Struct::asEnum) {
	if (s.fields.size()) {
	    os <<
		indent(il) << "@interface " << fqName(p, s) << " () {\n";

	    FieldList::const_iterator ii;

	    for (ii = s.fields.begin(); ii != s.fields.end(); ++ii) {
		Field const& f = *ii;

		if (f.array)
		    os <<
			indent(il + 1) << "@public NSMutableArray *" << backingVarName(f) << ";\n";
		else if (userDefinedType(f.type)) {
		    if (isEnum(p, f.type)) {
			os <<
			    indent(il + 1) << "@public NSNumber *" << backingVarName(f) << ";\n";
		    } else
			os <<
			    indent(il + 1) << "@public " << p.name << "_" << f.type << "_struct *" << backingVarName(f) << ";\n";
		} else {
		    if (isNumeric(f.type))
			os <<
			    indent(il + 1) << "@public NSNumber *" << backingVarName(f) << ";\n";
		    else
			os <<
			    indent(il + 1) << "@public " << xlatType(f) << backingVarName(f) << ";\n";
		}
	    }

	    os <<
		indent(il) << "}\n" <<
		indent(il) << "@end\n\n";
	}
    }
}

static void dumpSourceFile(Protocol const& p, std::ostream& os)
{
    os <<
	"// Generated by the protocol compiler version " << pcVersion << "\n"
	"// DO NOT EDIT THIS FILE DIRECTLY!\n\n"
	"#import \"" << p.name + ".h" << "\"\n\n";

    if (p.needed("bool"))
	os <<
	    "static void put(NSOutputStream* const os, uint8_t const tmp)\n"
	    "{\n"
	    "    if ([os write:&tmp maxLength:sizeof(tmp)] != sizeof(tmp))\n"
	    "        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error writing output stream\" userInfo:nil];\n"
	    "}\n\n";
    os <<
	"static void putbuf(NSOutputStream* const os, uint8_t const tmp[], NSInteger const n)\n"
	"{\n"
	"    if (n && [os write:tmp maxLength:n] != n)\n"
	"        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error writing output stream\" userInfo:nil];\n"
	"}\n\n"
	"static void get(NSInputStream* const is, uint8_t* const tmp)\n"
	"{\n"
	"    if ([is read:tmp maxLength:sizeof(*tmp)] != sizeof(*tmp))\n"
	"        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error reading input stream\" userInfo:nil];\n"
	"}\n\n"
	"static void emitRawInt(NSOutputStream* const os, uint8_t const tag, int64_t const val)\n"
	"{\n"
	"    uint8_t buf[1 + sizeof(int64_t)];\n\n"
	"    if (val >= -0x80 && val <= 0x7f) {\n"
	"\tbuf[0] = (tag & 0xf0) + 1;\n"
	"\tbuf[1] = (uint8_t)(val);\n"
	"\tputbuf(os, buf, 2);\n"
	"    } else if (val >= -0x8000 && val <= 0x7fff) {\n"
	"\tbuf[0] = (tag & 0xf0) + 2;\n"
	"\tbuf[1] = (uint8_t)(val >> 8);\n"
	"\tbuf[2] = (uint8_t)(val);\n"
	"\tputbuf(os, buf, 3);\n"
	"    } else if (val >= -0x800000 && val <= 0x7fffff) {\n"
	"\tbuf[0] = (tag & 0xf0) + 3;\n"
	"\tbuf[1] = (uint8_t)(val >> 16);\n"
	"\tbuf[2] = (uint8_t)(val >> 8);\n"
	"\tbuf[3] = (uint8_t)(val);\n"
	"\tputbuf(os, buf, 4);\n"
	"    } else if (val >= -0x80000000ll && val <= 0x7fffffffll) {\n"
	"\tbuf[0] = (tag & 0xf0) + 4;\n"
	"\tbuf[1] = (uint8_t)(val >> 24);\n"
	"\tbuf[2] = (uint8_t)(val >> 16);\n"
	"\tbuf[3] = (uint8_t)(val >> 8);\n"
	"\tbuf[4] = (uint8_t)(val);\n"
	"\tputbuf(os, buf, 5);\n"
	"    } else if (val >= -0x8000000000ll && val <= 0x7fffffffffll) {\n"
	"\tbuf[0] = (tag & 0xf0) + 5;\n"
	"\tbuf[1] = (uint8_t)(val >> 32);\n"
	"\tbuf[2] = (uint8_t)(val >> 24);\n"
	"\tbuf[3] = (uint8_t)(val >> 16);\n"
	"\tbuf[4] = (uint8_t)(val >> 8);\n"
	"\tbuf[5] = (uint8_t)(val);\n"
	"\tputbuf(os, buf, 6);\n"
	"    } else if (val >= -0x800000000000ll && val <= 0x7fffffffffffll) {\n"
	"\tbuf[0] = (tag & 0xf0) + 6;\n"
	"\tbuf[1] = (uint8_t)(val >> 40);\n"
	"\tbuf[2] = (uint8_t)(val >> 32);\n"
	"\tbuf[3] = (uint8_t)(val >> 24);\n"
	"\tbuf[4] = (uint8_t)(val >> 16);\n"
	"\tbuf[5] = (uint8_t)(val >> 8);\n"
	"\tbuf[6] = (uint8_t)(val);\n"
	"\tputbuf(os, buf, 7);\n"
	"    } else if (val >= -0x80000000000000ll && val <= 0x7fffffffffffffll) {\n"
	"\tbuf[0] = (tag & 0xf0) + 7;\n"
	"\tbuf[1] = (uint8_t)(val >> 48);\n"
	"\tbuf[2] = (uint8_t)(val >> 40);\n"
	"\tbuf[3] = (uint8_t)(val >> 32);\n"
	"\tbuf[4] = (uint8_t)(val >> 24);\n"
	"\tbuf[5] = (uint8_t)(val >> 16);\n"
	"\tbuf[6] = (uint8_t)(val >> 8);\n"
	"\tbuf[7] = (uint8_t)(val);\n"
	"\tputbuf(os, buf, 8);\n"
	"    } else {\n"
	"\tbuf[0] = (tag & 0xf0) + 8;\n"
	"\tbuf[1] = (uint8_t)(val >> 56);\n"
	"\tbuf[2] = (uint8_t)(val >> 48);\n"
	"\tbuf[3] = (uint8_t)(val >> 40);\n"
	"\tbuf[4] = (uint8_t)(val >> 32);\n"
	"\tbuf[5] = (uint8_t)(val >> 24);\n"
	"\tbuf[6] = (uint8_t)(val >> 16);\n"
	"\tbuf[7] = (uint8_t)(val >> 8);\n"
	"\tbuf[8] = (uint8_t)(val);\n"
	"\tputbuf(os, buf, sizeof(buf));\n"
	"    }\n"
	"}\n\n"
	"static int64_t consumeRawInt(NSInputStream* const is, uint8_t const expTag)\n"
	"{\n"
	"    uint8_t tag;\n\n"
	"    get(is, &tag);\n\n"

	"    int const len = tag & 0xf;\n\n"
	"    if ((tag & 0xf0) == (expTag & 0xf0) && len > 0 && len <= 8) {\n"
	"        int32_t val[2];\n\n"
	"        if ([is read:(uint8_t*)val maxLength:len] == len)\n"
	"            return (((int64_t) ntohl(val[0]) << 32) + (int64_t) ntohl(val[1])) >>\n"
	"                    ((sizeof(int64_t) - len) * 8);\n"
	"        else\n"
	"            @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error reading from input stream\" userInfo:nil];\n"
	"    } else\n"
	"        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error bad tag value\" userInfo:nil];\n"
	"}\n\n";

    if (p.needed("bool"))
	os <<
	    "static void marshal_bool(NSOutputStream* const os, NSNumber* const v)\n"
	    "{\n"
	    "    put(os, [v boolValue] == YES ? 0x71 : 0x70);\n"
	    "}\n\n"
	    "static NSNumber* unmarshal_bool(NSInputStream* const is)\n"
	    "{\n"
	    "    uint8_t tmp;\n\n"
	    "    if ([is read:&tmp maxLength:sizeof(tmp)]) {\n"
	    "        switch (tmp) {\n"
	    "         case 0x70: return [NSNumber numberWithBool:NO];\n"
	    "         case 0x71: return [NSNumber numberWithBool:YES];\n"
	    "        }\n"
	    "    }\n"
	    "    @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error unmarshalling bool\" userInfo:nil];\n"
	    "}\n\n";

    if (p.needed("int16"))
	os <<
	    "static void marshal_int16(NSOutputStream* const os, NSNumber* const v)\n"
	    "{\n"
	    "    int64_t const tmp = [v longLongValue];\n\n"
	    "    if (tmp <= 0x7fffll && tmp >= -0x8000ll)\n"
	    "\temitRawInt(os, 0x10, tmp);\n"
	    "    else \n"
	    "\t@throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"int16 out of range\" userInfo:nil];\n"
	    "}\n\n"
	    "static NSNumber* unmarshal_int16(NSInputStream* const is)\n"
	    "{\n"
	    "    int64_t const tmp = consumeRawInt(is, 0x10);\n\n"
	    "    if (tmp >= -0x8000ll && tmp <= 0x7fffll)\n"
	    "\treturn [NSNumber numberWithInt:(int16_t) tmp];\n"
	    "    else\n"
	    "\t@throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"int16 out of range\" userInfo:nil];\n"
	    "}\n\n";

    if (p.needed("int32"))
	os <<
	    "static void marshal_int32(NSOutputStream* const os, NSNumber* const v)\n"
	    "{\n"
	    "    int64_t const tmp = [v longLongValue];\n\n"
	    "    if (tmp <= 0x7fffffffll && tmp >= -0x80000000ll)\n"
	    "\temitRawInt(os, 0x10, tmp);\n"
	    "    else\n"
	    "\t@throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"int32 out of range\" userInfo:nil];\n"
	    "}\n\n"
	    "static NSNumber* unmarshal_int32(NSInputStream* const is)\n"
	    "{\n"
	    "    int64_t const tmp = consumeRawInt(is, 0x10);\n\n"
	    "    if (tmp >= -0x80000000ll && tmp <= 0x7fffffffll)\n"
	    "\treturn [NSNumber numberWithInt:(int32_t) tmp];\n"
	    "    else\n"
	    "\t@throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"int32 out of range\" userInfo:nil];\n"
	    "}\n\n";

    if (p.needed("int64"))
	os <<
	    "static void marshal_int64(NSOutputStream* const os, NSNumber* const v)\n"
	    "{\n"
	    "    emitRawInt(os, 0x10, (int64_t)[v longLongValue]);\n"
	    "}\n\n"
	    "static NSNumber* unmarshal_int64(NSInputStream* const is)\n"
	    "{\n"
	    "    return [NSNumber numberWithLongLong:consumeRawInt(is, 0x10)];\n"
	    "}\n\n";

    if (p.needed("double"))
	os <<
	    "static void marshal_double(NSOutputStream* const os, NSNumber* const v)\n"
	    "{\n"
	    "    uint8_t buf[1 + sizeof(int64_t)];\n"
	    "    union {\n"
	    "        double vDbl;\n"
	    "        int64_t v64;\n"
	    "    } tmp __attribute__((aligned(8)));\n\n"
	    "    tmp.vDbl = [v doubleValue];\n"
	    "    buf[0] = 0x28;\n"
	    "    buf[1] = (uint8_t)(tmp.v64 >> 56);\n"
	    "    buf[2] = (uint8_t)(tmp.v64 >> 48);\n"
	    "    buf[3] = (uint8_t)(tmp.v64 >> 40);\n"
	    "    buf[4] = (uint8_t)(tmp.v64 >> 32);\n"
	    "    buf[5] = (uint8_t)(tmp.v64 >> 24);\n"
	    "    buf[6] = (uint8_t)(tmp.v64 >> 16);\n"
	    "    buf[7] = (uint8_t)(tmp.v64 >> 8);\n"
	    "    buf[8] = (uint8_t)(tmp.v64);\n"
	    "    putbuf(os, buf, sizeof(buf));\n"
	    "}\n\n"
	    "static NSNumber* unmarshal_double(NSInputStream* const is)\n"
	    "{\n"
	    "    union {\n"
	    "        double vDbl;\n"
	    "        int64_t v64;\n"
	    "    } tmp __attribute__((aligned(8)));\n\n"
	    "    tmp.v64 = consumeRawInt(is, 0x20);\n"
	    "    return [NSNumber numberWithDouble:tmp.vDbl];\n"
	    "}\n\n";

    if (p.needed("string")) {
	os <<
	    "static void marshal_string(NSOutputStream* const os, NSString* const v)\n"
	    "{\n"
	    "    NSInteger const len = [v lengthOfBytesUsingEncoding:NSUTF8StringEncoding];\n\n"
	    "    emitRawInt(os, 0x40, (int64_t) len);\n"
	    "    putbuf(os, (uint8_t const*) [v UTF8String], len);\n"
	    "}\n\n";

	if (objcUseARC)
	    os <<
		"static NSString* unmarshal_string(NSInputStream* const is)\n"
		"{\n"
		"    NSInteger const len = (NSInteger) consumeRawInt(is, 0x40);\n\n"
		"    if (len >= 0) {\n"
		"        NSMutableData* const buf = [NSMutableData dataWithLength:len];\n\n"
		"        if ([is read:[buf mutableBytes] maxLength:len] == len)\n"
		"            return [[NSString alloc] initWithData:buf encoding:NSUTF8StringEncoding];\n\n"
		"        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error reading input stream\" userInfo:nil];\n"
		"    }\n\n"
		"    @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"bad string length\" userInfo:nil];\n"
		"}\n\n";
	else
	    os <<
		"static NSString* unmarshal_string(NSInputStream* const is)\n"
		"{\n"
		"    NSInteger const len = (NSInteger) consumeRawInt(is, 0x40);\n\n"
		"    if (len >= 0) {\n"
		"        NSMutableData* const buf = [NSMutableData dataWithLength:len];\n\n"
		"        if ([is read:[buf mutableBytes] maxLength:len] == len)\n"
		"            return [[[NSString alloc] initWithData:buf encoding:NSUTF8StringEncoding] autorelease];\n\n"
		"        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error reading input stream\" userInfo:nil];\n"
		"    }\n\n"
		"    @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"bad string length\" userInfo:nil];\n"
		"}\n\n";
    }

    if (p.needed("binary"))
	os <<
	    "static void marshal_binary(NSOutputStream* const os, NSData *const v)\n"
	    "{\n"
	    "    emitRawInt(os, 0x30, (int64_t) [v length]);\n"
	    "    if ([os write:[v bytes] maxLength:[v length]] != (NSInteger)[v length])\n"
	    "        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error writing output stream\" userInfo:nil];\n"
	    "}\n\n"
	    "static NSData *unmarshal_binary(NSInputStream* const is)\n"
	    "{\n"
	    "    NSInteger const len = (NSInteger) consumeRawInt(is, 0x30);\n\n"
	    "    if (len >= 0) {\n"
	    "        NSMutableData* const buf = [NSMutableData dataWithLength:len];\n\n"
	    "        if ([is read:[buf mutableBytes] maxLength:len] == len)\n"
	    "            return buf;\n\n"
	    "        @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"error reading input stream\" userInfo:nil];\n"
	    "    }\n\n"
	    "    @throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"bad binary length\" userInfo:nil];\n"
	    "}\n\n";

    os <<
	"static void marshal_array(NSOutputStream* const os, NSArray* const v, void (*marshal_func)(NSOutputStream*, id))\n"
	"{\n"
	"    NSUInteger const sz = [v count];\n\n"
	"    if (sz < 0x10000) {\n"
	"\temitRawInt(os, 0x50, sz);\n"
	"\tfor (NSUInteger ii = 0 ; ii < sz; ++ii)\n"
	"\t    (*marshal_func)(os, [v objectAtIndex:ii]);\n"
	"    } else\n"
	"\t@throw [NSException exceptionWithName:@\"Protocol Error\" reason:@\"bad array size\" userInfo:nil];\n"
	"}\n\n"
	"static NSMutableArray* unmarshal_array(NSInputStream* const is, id (*unmarshal_func)(NSInputStream*))\n"
	"{\n"
	"    NSInteger len = (NSInteger) consumeRawInt(is, 0x50);\n"
	"    NSMutableArray* const a = [NSMutableArray arrayWithCapacity:len];\n\n"
	"    while (len--)\n"
	"        [a addObject:(*unmarshal_func)(is)];\n"
	"    return a;\n"
	"}\n\n";


    apply(p, os, p.types.begin(), p.types.end(), dumpClassExtension, 0);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpClassExtension, 0);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpClassExtension, 0);

    apply(p, os, p.types.begin(), p.types.end(), dumpMessageMarshalFunc, 0);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpMessageMarshalFunc, 0);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpMessageMarshalFunc, 0);

    apply(p, os, p.types.begin(), p.types.end(), dumpMsgUnmarshalFunc);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpMsgUnmarshalFunc);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpMsgUnmarshalFunc);

    apply(p, os, p.types.begin(), p.types.end(), dumpMessageImplementation, 0);
    apply(p, os, p.requests.begin(), p.requests.end(), dumpMessageImplementation, 0);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpMessageImplementation, 0);

    os <<
	"@implementation " << p.name << "\n\n"
	"- (NSData *)marshal;\n"
	"{\n"
	"    @throw @\"Invalid message\";\n"
	"}\n\n"
	"+ (" << p.name << "*)unmarshal:(NSData *)d\n"
	"{\n"
	"    NSInputStream* const is = [NSInputStream inputStreamWithData:d];\n"
	"    uint8_t buf[4];\n\n"
	"    [is open];\n\n"
	"    if ([is read:buf maxLength:sizeof(buf)] != sizeof(buf))\n"
	"        return nil;\n\n"
	"    if (buf[0] != 'S' || buf[1] != 'D' || buf[2] != 'D' || buf[3] != 2)\n"
	"        return nil;\n\n"
	"    if (consumeRawInt(is, 0x50) != 3)\n"
	"        return nil;\n"
	"    if (consumeRawInt(is, 0x10) != " << std::dec << p.longHash(p.name) << ")\n"
	"        return nil;\n\n"
	"    switch (consumeRawInt(is, 0x10)) {\n";

    apply(p, os, p.requests.begin(), p.requests.end(), dumpMsgUnmarshallerCall, 1);
    apply(p, os, p.replies.begin(), p.replies.end(), dumpMsgUnmarshallerCall, 1);

    os <<
	"     default:\n"
	"        return nil;\n"
	"    }\n"
	"}\n\n";

    dumpHandler(p, p.requests, "request", os);
    dumpHandler(p, p.replies, "reply", os);

    os <<
	"@end\n";
}

// The only externally visible function from this module. This is the
// entry point to generate C++ files that handle the protocol.

void generateObjC(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("Objective-C generator only supports SDDv2");

    std::ofstream hdr((targetPath + "/" + p.name + ".h").c_str());
    std::ofstream src((targetPath + "/" + p.name + ".m").c_str());

    dumpHeaderFile(p, hdr);
    dumpSourceFile(p, src);
}
