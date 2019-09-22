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

// This module generates Java files that overlay a corresponding
// protocol element in javascript. The .proto grammar gets mapped to Java in the
// following way:
//
//    "protocol Name" creates a class called Name as the container for all other
//    classes.
//
//    "message Name" creates a class overlaying the corresponding javascript object
//    creating getter and setter methods to access all fields of the message.
//
//    "struct Name" creates a class overlaying the corresponding javacript object
//    creating getter and setter methods to access all fields of the struct.
//
// The fields of messages and structures get mapped to the following
// types:
//
//    bool       -> boolean
//    int16      -> int
//    int32      -> int
//    int64      -> double
//    double     -> double
//    string     -> String
//    binary     -> ArrayBufferNative
//    array of T -> JsArray<T>
//    T          -> class T   (where T is a type defined earlier)
//
// To make the names of structures and messages and fields common to
// all target languages, the first character of the protocol and
// message names should be uppercase and the field names should begin
// with a lowercase letter.

#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include "pc.h"

// Prototypes.

static void dumpField(Protocol const&, Field const&, std::ostream&, int);
static void dumpMessage(Protocol const&, Struct const&, std::ostream&, int);
static void dumpProtocol(Protocol const&);
static void dumpSourceFile(Protocol const&, std::ostream&);
static void dumpStructFields(Protocol const&, Struct const&, std::ostream&, int);
static void dumpType(Protocol const&, Struct const&, std::ostream&, int);
static std::string xlatType(Field const&);
static std::string xlatType(std::string const&);
static std::string messageType(Struct const&);
static std::string jsMessageType(Struct const&);

static inline std::string const indent(int indentLevel)
{
    return std::string(indentLevel * 4, ' ');
}

// Dumps a single field to the stream.

static void dumpField(Protocol const&, Field const& f, std::ostream& os, int il)
{
    os << indent(il) << "public native " << xlatType(f) << " " << f.name <<
			    "() /*-{ return this." + f.name + "; }-*/;\n";
    os << indent(il) << "public native void " << f.name <<
			    "(" + xlatType(f) + " v) /*-{ this." + f.name  + " = v; }-*/;\n";
    if (f.optional)
	os << indent(il) << "public native boolean " << f.name << "IsValid() /*-{" <<
				" return this." << f.name << " !== null; }-*/;\n";
}

static void dumpConstructor(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    std::string jsName = "$wnd." + p.name + "_" + jsMessageType(s) + "_" + s.name;

    os << indent(il) << "protected " << s.name << "() { }\n";

    os << indent(il) << "public static native " << s.name <<
			    " create() /*-{ return new " << jsName << "; }-*/;\n";

    os << indent(il) << "public static native " << s.name << " castTo(" << p.name <<
			    " o) /*-{ return (o instanceof " << jsName << ") ? o : null; }-*/;\n";
}

static void dumpMessage(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    os << indent(il) << "public static final class " << s.name << " extends JavaScriptObject {\n";

    dumpConstructor(p, s, os, il + 1);
    dumpStructFields(p, s, os, il + 1);

    os << indent(il) << "}\n\n";
}

static void dumpProtocol(Protocol const& p)
{
    std::ofstream src((targetPath + "/" + p.name + ".java").c_str());

    src <<
	"// Generated by the protocol compiler version " << pcVersion << "\n"
	"// DO NOT EDIT THIS FILE DIRECTLY!\n\n";

    if (javaPkg.size())
	src << "package " << javaPkg << ";\n\n";

    src << "import com.google.gwt.core.client.JsArray;\n";
    src << "import com.google.gwt.core.client.JsArrayString;\n";
    src << "import com.google.gwt.core.client.JsArrayNumber;\n";
    src << "import com.google.gwt.core.client.JsArrayInteger;\n";
    src << "import com.google.gwt.core.client.JsArrayBoolean;\n";
    src << "import com.google.gwt.core.client.JavaScriptObject;\n";
    src << "import com.google.gwt.typedarrays.client.DataViewNative;\n";
    src << "import com.google.gwt.typedarrays.client.ArrayBufferNative;\n\n";

    dumpSourceFile(p, src);
}

static void dumpMessageType(Protocol const& p, std::ostream& os, StructList const& list, int il)
{
    if (list.size()) {
	std::string msgType = messageType(list.front());

	os << "\n" <<
		indent(il) << "// " << msgType << " types\n" <<
		indent(il) << "\n" <<
		indent(il) << "public static final class " << msgType << " {\n";

	apply(p, os, list.begin(), list.end(), dumpMessage, il + 1);

	os << indent(il) << "}\n";
    }
}

static void dumpUnmarshallers(Protocol const& p, std::ostream& os, int il)
{
    os << indent(il) << "public static native " << p.name << " unmarshalRequest(DataViewNative obj) /*-{ return obj === null ? null : $wnd."
			    << toUpper(p.name) << "_PROTO.unmarshal_request({ v:obj, o:0 }); }-*/;\n";

    os << indent(il) << "public static native " << p.name << " unmarshalReply(DataViewNative obj) /*-{ return obj === null ? null : $wnd."
			    << toUpper(p.name) << "_PROTO.unmarshal_reply({ v:obj, o:0 }); }-*/;\n";
}

// This function generates the .java contents for a protocol.

static void dumpSourceFile(Protocol const& p, std::ostream& os)
{
    os << "// Start of the " << p.name << " object hierarchy.\n\n"
	  "public final class " << p.name << " extends JavaScriptObject {\n";

    os << indent(1) << "protected " << p.name << "() { }\n\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpType, 1);
    dumpMessageType(p, os, p.requests, 1);
    dumpMessageType(p, os, p.replies, 1);

    os << "\n";

    dumpUnmarshallers(p, os, 1);

    os << "}\n";
}

// Dumps all the fields of a structure. It assumes the stream is
// "ready" for the fields (i.e. the C++ prologue codes has already
// been written and the epilogue will be written at a later time.

static void dumpStructFields(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    apply(p, os, s.fields.begin(), s.fields.end(), dumpField, il);
}

struct EnumExtra {
    int il;
    std::string sname;
};

static void dumpEnumField(Protocol const&, Field const& f, std::ostream& os, EnumExtra e)
{
    os << indent(e.il) << "public static " << e.sname << " " << f.name << " = create(" << f.hash << ");\n";
}

// Dumps a "type" to the stream. A type is a class that isn't part
// of the protocol object hierarchy.

static void dumpType(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    if (!s.fields.empty() && s.isUsed()) {
	if (s.use == Struct::asEnum) {
	    os << indent(il) << "public static final class " << s.name << " extends JavaScriptObject {\n";
	    EnumExtra extra = { il + 1, s.name };
	    apply(p, os, s.fields.begin(), s.fields.end(), dumpEnumField, extra);

	    os << indent(il + 1) << "\n";
	    os << indent(il + 1) << "protected " << s.name << "() { };\n";
	    os << indent(il + 1) << "private static native " << s.name << " create(int v) /*-{ return v; }-*/;\n";

	    os << indent(il) << "}\n\n";
	} else {
	    os << indent(il) << "public static final class " << s.name << " extends JavaScriptObject {\n";

	    dumpConstructor(p, s, os, il + 1);
	    dumpStructFields(p, s, os, il + 1);

	    os << indent(il) << "}\n\n";
	}
    }
}

static std::string xlatType(std::string const& type)
{
    if (type == "bool")
	return "boolean";
    else if (type == "int16" || type == "int32")
	return "int";
    else if (type == "int64" || type == "double")
	return "double";
    else if (type == "string")
	return "java.lang.String";
    else if (type == "binary")
	return "ArrayBufferNative";
    else
	return type;
}

// Takes a Field and returns the SDD mapping of the Field's type.

static std::string xlatType(Field const& f)
{
    if (f.array) {
	if (f.type == "bool")
	    return "JsArrayBoolean";
	else if (f.type == "int16" || f.type == "int32")
	    return "JsArrayInteger";
	else if (f.type == "int64" || f.type == "double")
	    return "JsArrayNumber";
	else if (f.type == "string")
	    return "JsArrayString";
	else if (f.type == "binary")
	    return "JsArray<ArrayBufferNative>";
    }

    return xlatType(f.type);;
}

static std::string messageType(Struct const& s)
{
    switch (s.use) {
     case Struct::asRequest: return "Request";
     case Struct::asReply: return "Reply";
     case Struct::asStruct: return "Struct";
     default:
	throw std::runtime_error("messageType() -- bad token type");
    }
}

static std::string jsMessageType(Struct const& s)
{
    switch (s.use) {
     case Struct::asRequest: return "request";
     case Struct::asReply: return "reply";
     case Struct::asStruct: return "struct";
     default:
	throw std::logic_error("jsMessageType() -- bad token type");
    }
}

void generateGwtJava(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("GWT generator only supports SDDv2");

    dumpProtocol(p);
}
