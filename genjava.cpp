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

// This module generates Java files that can communicate using a
// specified protocol. The .proto grammar gets mapped to Java in the
// following way:
//
//    "protocol Name" creates an abstract base class called Name. An
//    interface is also created, called Receiver. Receiver
//    defines pure virtual functions that handle each message type.
//
//    "message Name" creates a class derived from the protocol base
//    class.
//
//    "struct Name" creates a structure (not derived from anything.)
//
// The fields of messages and structures get mapped to the following
// types:
//
//    bool       -> boolean
//    int16      -> short
//    int32      -> int
//    int64      -> long
//    double     -> double
//    string     -> String
//    binary     -> byte[]
//    array of T -> T[]
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

enum IOType { IOTypeStream, IOTypeBuffer };

struct CodeGenParams {
    int indentLevel;
    IOType ioType;

    CodeGenParams(int indentLevel, IOType ioType)
    {
	this->indentLevel = indentLevel;
	this->ioType = ioType;
    }

    CodeGenParams operator+(int indentLevel)
    {
	return CodeGenParams(this->indentLevel + indentLevel, this->ioType);
    }
};

// Prototypes.

static void dumpField(Protocol const&, Field const&, std::ostream&, int);
static void dumpMessage(Protocol const&, Struct const&, std::ostream&, int);
static void dumpMsgUnmarshaller(Protocol const&, Struct const&, std::ostream&, int);
static void dumpProtocol(Protocol const&);
static void dumpSourceFile(Protocol const&, std::ostream&);
static void dumpStructFields(Protocol const&, Struct const&, std::ostream&, int);
static void dumpType(Protocol const&, Struct const&, std::ostream&, int);
static void unmarshalField(Protocol const& p, Field const&, std::ostream&, size_t*);
static void dumpStructMarshaller(Protocol const&, Struct const&, std::ostream&, CodeGenParams);
static std::string xlatType(Field const&);
static std::string xlatType(std::string const&, bool);
static bool isNumeric(std::string const&);
static bool userDefinedType(std::string const&);
static std::string messageType(Struct const&);
static long serialVersionUID(Protocol const&, Struct const&);

static const char *streamPrimitives =
    "    final static void writeInt(java.io.DataOutputStream out, int tag, long v) throws java.io.IOException\n"
    "    {\n"
    "        if (v >= -0x80 && v <= 0x7f) {\n"
    "            out.writeByte((tag & 0xf0) + 1);\n"
    "            out.writeByte((int) v);\n"
    "        } else if (v >= -0x8000 && v <= 0x7fff) {\n"
    "            out.writeByte((tag & 0xf0) + 2);\n"
    "            out.writeShort((short) v);\n"
    "        } else if (v >= -0x800000 && v <= 0x7fffff) {\n"
    "            out.writeByte((tag & 0xf0) + 3);\n"
    "            out.writeByte((int) (v >> 16));\n"
    "            out.writeShort((short) v);\n"
    "        } else if (v >= -0x80000000L && v <= 0x7fffffffL) {\n"
    "            out.writeByte((tag & 0xf0) + 4);\n"
    "            out.writeInt((int) v);\n"
    "        } else if (v >= -0x8000000000L && v <= 0x7fffffffffL) {\n"
    "            out.writeByte((tag & 0xf0) + 5);\n"
    "            out.writeByte((int) (v >> 32));\n"
    "            out.writeInt((int) v);\n"
    "        } else if (v >= -0x800000000000L && v <= 0x7fffffffffffL) {\n"
    "            out.writeByte((tag & 0xf0) + 6);\n"
    "            out.writeByte((int) (v >> 40));\n"
    "            out.writeByte((int) (v >> 32));\n"
    "            out.writeInt((int) v);\n"
    "        } else if (v >= -0x80000000000000L && v <= 0x7fffffffffffffL) {\n"
    "            out.writeByte((tag & 0xf0) + 7);\n"
    "            out.writeByte((int) (v >> 48));\n"
    "            out.writeByte((int) (v >> 40));\n"
    "            out.writeByte((int) (v >> 32));\n"
    "            out.writeInt((int) v);\n"
    "        } else {\n"
    "            out.writeByte((tag & 0xf0) + 8);\n"
    "            out.writeLong(v);\n"
    "        }\n"
    "    }\n\n"
    "    final static long readInt(java.io.DataInputStream in, int expTag) throws java.io.IOException\n"
    "    {\n"
    "       byte tag = in.readByte();\n"
    "       int len = tag & 0x0f;\n\n"
    "       if ((tag & 0xf0) == (expTag & 0xf0) && len > 0 && len <= 8) {\n"
    "           long v = in.readByte();\n\n"
    "           for (int ii = 1; ii < len; ii++)\n"
    "               v = (v << 8) + (in.readByte() & 0xff);\n\n"
    "           return v;\n"
    "       }\n\n"
    "       throw new java.io.IOException(\"invalid integer for protocol\");\n"
    "    }\n\n"
    "    final static int readLength(java.io.DataInputStream in, int tag) throws java.io.IOException\n"
    "    {\n"
    "        final long len = readInt(in, tag);\n\n"
    "        if (len >= 0 && len <= Integer.MAX_VALUE)\n"
    "            return (int) len;\n\n"
    "        throw new java.io.IOException(\"invalid length value for protocol\");\n"
    "    }\n\n"
    "    static void writeLength(java.io.DataOutputStream out, int tag, int len) throws java.io.IOException\n"
    "    {\n"
    "        if (len < 0)\n"
    "            throw new java.io.IOException(\"invalid length value for protocol\");\n\n"
    "        writeInt(out, tag, len);\n"
    "    }\n\n"
    "    static void marshal(java.io.DataOutputStream out, boolean v) throws java.io.IOException\n"
    "    {\n"
    "        out.writeByte(v ? 0x71 : 0x70);\n"
    "    }\n\n"
    "    static void marshal(java.io.DataOutputStream out, short v) throws java.io.IOException\n"
    "    {\n"
    "        writeInt(out, 0x10, v);\n"
    "    }\n\n"
    "    static void marshal(java.io.DataOutputStream out, int v) throws java.io.IOException\n"
    "    {\n"
    "        writeInt(out, 0x10, v);\n"
    "    }\n\n"
    "    static void marshal(java.io.DataOutputStream out, long v) throws java.io.IOException\n"
    "    {\n"
    "        writeInt(out, 0x10, v);\n"
    "    }\n\n"
    "    static void marshal(java.io.DataOutputStream out, double v) throws java.io.IOException\n"
    "    {\n"
    "        out.writeByte(0x28);\n"
    "        out.writeLong(Double.doubleToLongBits(v));\n"
    "    }\n\n"
    "    static void marshal(java.io.DataOutputStream out, byte[] v) throws java.io.IOException\n"
    "    {\n"
    "        writeLength(out, 0x30, v.length);\n"
    "        out.write(v);\n"
    "    }\n\n"
    "    static void marshal(java.io.DataOutputStream out, java.lang.String v) throws java.io.IOException\n"
    "    {\n"
    "        java.nio.ByteBuffer buf = charSet.encode(v);\n"
    "        int len = buf.remaining();\n\n"
    "        writeLength(out, 0x40, len);\n"
    "        if (buf.hasArray())\n"
    "            out.write(buf.array(), buf.arrayOffset() + buf.position(), len);\n"
    "        else {\n"
    "            byte[] data = new byte[len];\n"
    "            buf.get(data);\n"
    "            out.write(data);\n"
    "        }\n"
    "    }\n\n"
    "    static boolean unmarshal_bool(java.io.DataInputStream in) throws java.io.IOException\n"
    "    {\n"
    "        byte b = in.readByte();\n\n"
    "        if (b == 0x70)\n"
    "            return false;\n"
    "        else if (b == 0x71)\n"
    "            return true;\n"
    "        else\n"
    "            throw new java.io.IOException(\"invalid bool value in message\");\n"
    "    }\n\n"
    "    static short unmarshal_int16(java.io.DataInputStream in) throws java.io.IOException\n"
    "    {\n"
    "        long v = readInt(in, 0x10);\n\n"
    "        if (v >= Short.MIN_VALUE && v <= Short.MAX_VALUE)\n"
    "            return (short) v;\n\n"
    "        throw new java.io.IOException(\"int16 value out of range\");\n"
    "    }\n\n"
    "    static int unmarshal_int32(java.io.DataInputStream in) throws java.io.IOException\n"
    "    {\n"
    "        long v = readInt(in, 0x10);\n\n"
    "        if (v >= Integer.MIN_VALUE && v <= Integer.MAX_VALUE)\n"
    "            return (int) v;\n\n"
    "        throw new java.io.IOException(\"int32 value out of range\");\n"
    "    }\n\n"
    "    static long unmarshal_int64(java.io.DataInputStream in) throws java.io.IOException\n"
    "    {\n"
    "        return readInt(in, 0x10);\n"
    "    }\n\n"
    "    static double unmarshal_double(java.io.DataInputStream in) throws java.io.IOException\n"
    "    {\n"
    "        if (in.readByte() == 0x28)\n"
    "            return Double.longBitsToDouble(in.readLong());\n"
    "        throw new java.io.IOException(\"expecting tag for double\");\n"
    "    }\n\n"
    "    static byte[] unmarshal_binary(java.io.DataInputStream in) throws java.io.IOException\n"
    "    {\n"
    "        byte[] b = new byte[readLength(in, 0x30)];\n\n"
    "        if (b.length > 0 && (in.read(b) != b.length))\n"
    "            throw new java.io.IOException(\"invalid binary length in message\");\n\n"
    "        return b;\n"
    "    }\n\n"
    "    static java.lang.String unmarshal_string(java.io.DataInputStream in) throws java.io.IOException\n"
    "    {\n"
    "        int len = readLength(in, 0x40);\n"
    "        byte[] buf = new byte[len];\n"
    "        int total = 0;\n\n"
    "        while (total < len) {\n"
    "            int n = in.read(buf, total, len - total);\n"
    "            if (n == -1)\n"
    "                throw new java.io.EOFException();\n\n"
    "            total += n;\n"
    "        }\n"
    "        return charSet.decode(java.nio.ByteBuffer.wrap(buf)).toString();\n"
    "    }\n\n"
    "    public void marshal(java.io.OutputStream os) throws java.io.IOException\n"
    "    {\n"
    "        java.io.DataOutputStream out = new java.io.DataOutputStream(os);\n\n"
    "        out.writeBytes(\"SDD\");\n"
    "        out.writeByte(2);\n\n"
    "        marshal(out);\n"
    "    }\n\n";

static const char *bufferPrimitives =
    "    final static void writeInt(java.nio.ByteBuffer out, int tag, long v) throws java.io.IOException\n"
    "    {\n"
    "        if (v >= -0x80 && v <= 0x7f) {\n"
    "            out.put((byte)((tag & 0xf0) + 1));\n"
    "            out.put((byte)(int) v);\n"
    "        } else if (v >= -0x8000 && v <= 0x7fff) {\n"
    "            out.put((byte)((tag & 0xf0) + 2));\n"
    "            out.putShort((short) v);\n"
    "        } else if (v >= -0x800000 && v <= 0x7fffff) {\n"
    "            out.put((byte)((tag & 0xf0) + 3));\n"
    "            out.put((byte)(int) (v >> 16));\n"
    "            out.putShort((short) v);\n"
    "        } else if (v >= -0x80000000L && v <= 0x7fffffffL) {\n"
    "            out.put((byte)((tag & 0xf0) + 4));\n"
    "            out.putInt((int) v);\n"
    "        } else if (v >= -0x8000000000L && v <= 0x7fffffffffL) {\n"
    "            out.put((byte)((tag & 0xf0) + 5));\n"
    "            out.put((byte)(int) (v >> 32));\n"
    "            out.putInt((int) v);\n"
    "        } else if (v >= -0x800000000000L && v <= 0x7fffffffffffL) {\n"
    "            out.put((byte)((tag & 0xf0) + 6));\n"
    "            out.put((byte)(int) (v >> 40));\n"
    "            out.put((byte)(int) (v >> 32));\n"
    "            out.putInt((int) v);\n"
    "        } else if (v >= -0x80000000000000L && v <= 0x7fffffffffffffL) {\n"
    "            out.put((byte)((tag & 0xf0) + 7));\n"
    "            out.put((byte)(int) (v >> 48));\n"
    "            out.put((byte)(int) (v >> 40));\n"
    "            out.put((byte)(int) (v >> 32));\n"
    "            out.putInt((int) v);\n"
    "        } else {\n"
    "            out.put((byte)((tag & 0xf0) + 8));\n"
    "            out.putLong(v);\n"
    "        }\n"
    "    }\n\n"
    "    final static long readInt(java.nio.ByteBuffer in, int expTag) throws java.io.IOException\n"
    "    {\n"
    "       byte tag = in.get();\n"
    "       int len = tag & 0x0f;\n\n"
    "       if ((tag & 0xf0) == (expTag & 0xf0) && len > 0 && len <= 8) {\n"
    "           long v = in.get();\n\n"
    "           for (int ii = 1; ii < len; ii++)\n"
    "               v = (v << 8) + (in.get() & 0xff);\n\n"
    "           return v;\n"
    "       }\n\n"
    "       throw new java.io.IOException(\"invalid integer tag for protocol\");\n"
    "    }\n\n"
    "    final static int readLength(java.nio.ByteBuffer in, int tag) throws java.io.IOException\n"
    "    {\n"
    "        final long len = readInt(in, tag);\n\n"
    "        if (len >= 0 && len <= Integer.MAX_VALUE)\n"
    "            return (int) len;\n\n"
    "        throw new java.io.IOException(\"invalid length value for protocol\");\n"
    "    }\n\n"
    "    static void writeLength(java.nio.ByteBuffer out, int tag, int len) throws java.io.IOException\n"
    "    {\n"
    "        if (len < 0)\n"
    "            throw new java.io.IOException(\"invalid length value for protocol\");\n\n"
    "        writeInt(out, tag, len);\n"
    "    }\n\n"
    "    public java.nio.ByteBuffer marshal(java.nio.ByteBuffer out) throws java.io.IOException\n"
    "    {\n"
    "        out.put((byte)83);\n"
    "        out.put((byte)68);\n"
    "        out.put((byte)68);\n"
    "        out.put((byte)2);\n"
    "        _marshal(out);\n\n"
    "        return out;\n"
    "    }\n\n"
    "    static void marshal(java.nio.ByteBuffer out, boolean v) throws java.io.IOException\n"
    "    {\n"
    "        out.put(v ? (byte)0x71 : (byte)0x70);\n"
    "    }\n\n"
    "    static void marshal(java.nio.ByteBuffer out, short v) throws java.io.IOException\n"
    "    {\n"
    "        writeInt(out, 0x10, v);\n"
    "    }\n\n"
    "    static void marshal(java.nio.ByteBuffer out, int v) throws java.io.IOException\n"
    "    {\n"
    "        writeInt(out, 0x10, v);\n"
    "    }\n\n"
    "    static void marshal(java.nio.ByteBuffer out, long v) throws java.io.IOException\n"
    "    {\n"
    "        writeInt(out, 0x10, v);\n"
    "    }\n\n"
    "    static void marshal(java.nio.ByteBuffer out, double v) throws java.io.IOException\n"
    "    {\n"
    "        out.put((byte) 0x28);\n"
    "        out.putLong(Double.doubleToLongBits(v));\n"
    "    }\n\n"
    "    static void marshal(java.nio.ByteBuffer out, byte[] v) throws java.io.IOException\n"
    "    {\n"
    "        writeLength(out, 0x30, v.length);\n"
    "        out.put(v);\n"
    "    }\n\n"
    "    static void marshal(java.nio.ByteBuffer out, java.lang.String v) throws java.io.IOException\n"
    "    {\n"
    "        java.nio.ByteBuffer buf = charSet.encode(v);\n"
    "        writeLength(out, 0x40, buf.remaining());\n"
    "        out.put(buf);\n"
    "    }\n\n"
    "    static boolean unmarshal_bool(java.nio.ByteBuffer in) throws java.io.IOException\n"
    "    {\n"
    "        byte b = in.get();\n\n"
    "        if (b == 0x70)\n"
    "            return false;\n"
    "        else if (b == 0x71)\n"
    "            return true;\n"
    "        else\n"
    "            throw new java.io.IOException(\"invalid bool value in message\");\n"
    "    }\n\n"
    "    static short unmarshal_int16(java.nio.ByteBuffer in) throws java.io.IOException\n"
    "    {\n"
    "        long v = readInt(in, 0x10);\n\n"
    "        if (v >= Short.MIN_VALUE && v <= Short.MAX_VALUE)\n"
    "            return (short) v;\n\n"
    "        throw new java.io.IOException(\"int16 value out of range\");\n"
    "    }\n\n"
    "    static int unmarshal_int32(java.nio.ByteBuffer in) throws java.io.IOException\n"
    "    {\n"
    "        long v = readInt(in, 0x10);\n\n"
    "        if (v >= Integer.MIN_VALUE && v <= Integer.MAX_VALUE)\n"
    "            return (int) v;\n\n"
    "        throw new java.io.IOException(\"int32 value out of range\");\n"
    "    }\n\n"
    "    static long unmarshal_int64(java.nio.ByteBuffer in) throws java.io.IOException\n"
    "    {\n"
    "        return readInt(in, 0x10);\n"
    "    }\n\n"
    "    static double unmarshal_double(java.nio.ByteBuffer in) throws java.io.IOException\n"
    "    {\n"
    "        if (in.get() == 0x28)\n"
    "            return Double.longBitsToDouble(in.getLong());\n"
    "        throw new java.io.IOException(\"expecting tag for double\");\n"
    "    }\n\n"
    "    static byte[] unmarshal_binary(java.nio.ByteBuffer in) throws java.io.IOException\n"
    "    {\n"
    "        byte[] b = new byte[readLength(in, 0x30)];\n\n"
    "        in.get(b);\n"
    "        return b;\n"
    "    }\n\n"
    "    static java.lang.String unmarshal_string(java.nio.ByteBuffer in) throws java.io.IOException\n"
    "    {\n"
    "        byte[] buf = new byte[readLength(in, 0x40)];\n"
    "        in.get(buf);\n"
    "        return charSet.decode(java.nio.ByteBuffer.wrap(buf)).toString();\n"
    "    }\n\n";

static const char *commonPrimitives =
    "    static int encodedVariableLength(int len)\n"
    "    {\n"
    "        if (len > 0xff) {\n"
    "            if (len > 0xffff) {\n"
    "                if (len > 0xffffff)\n"
    "                    return 5;\n"
    "                return 4;\n"
    "            }\n"
    "            return 3;\n"
    "        }\n"
    "        return 2;\n"
    "    }\n\n"
    "    static boolean equals(Object o1, Object o2)\n"
    "    {\n"
    "        if (o1 == null && o2 == null)\n"
    "            return true;\n\n"
    "        if (o1 != null)\n"
    "            return o1.equals(o2);\n\n"
    "        return false;\n"
    "    }\n\n"
    "    static boolean equals(byte[] o1, byte[] o2)\n"
    "    {\n"
    "        return java.util.Arrays.equals(o1, o2);\n"
    "    }\n\n"
    "    static boolean equals(Object[] o1, Object[] o2)\n"
    "    {"
    "        return java.util.Arrays.equals(o1, o2);\n"
    "    }\n\n"
    "    static boolean equals(byte[][] o1, byte[][] o2)\n"
    "    {\n"
    "        if (o1 == null && o2 == null)\n"
    "            return true;\n\n"
    "        if (o1 == null || o2 == null)\n"
    "            return false;\n\n"
    "        if (o1.length == o2.length) {\n"
    "            for (int ii = 0; ii < o1.length; ii++)\n"
    "                if (!java.util.Arrays.equals(o1[ii], o2[ii]))\n"
    "                    return false;\n\n"
    "            return true;\n"
    "        }\n\n"
    "        return false;\n"
    "    }\n\n";

static inline std::string const indent(int indentLevel)
{
    return std::string(indentLevel * 4, ' ');
}

static inline std::string const indent(CodeGenParams cgp)
{
    return indent(cgp.indentLevel);
}

// Dumps a single field to the stream.

static void dumpField(Protocol const&, Field const& f, std::ostream& os, int il)
{
    os << indent(il) << "public " << xlatType(f) << " " << f.name << ";\n";
}

// Dumps a single enum field to the stream.

static void dumpEnumField(Protocol const&, Field const& f, std::ostream& os, int il)
{
    os << indent(il) << f.name << ",\n";
}

static void emitConstantInt(std::ostream& os, CodeGenParams cgp, uint8_t tag,
			    int64_t const val)
{
    std::ostringstream tmp;

    emitRawInt(tmp, tag, val);
    std::string const s = tmp.str();

    for (std::string::const_iterator ii = s.begin(); ii != s.end(); ++ii) {
	if (cgp.ioType == IOTypeStream)
	    os << indent(cgp) << "out.writeByte(" << (int) *ii << ");\n";
	else
	    os << indent(cgp) << "out.put((byte)" << (int) *ii << ");\n";
    }
}

static void dumpTypeEquality(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    if (s.isUserDefined()) {
	if (s.fields.size()) {
	    os <<
		"\n" <<
		indent(il) << "public boolean equals(Object _o)\n" <<
		indent(il) << "{\n" <<
		indent(il) << "    if (_o instanceof " << s.name << ") {\n" <<
		indent(il) << "        " << s.name << " o = (" << s.name << ") _o;\n\n" <<
		indent(il) << "        return ";

	    FieldList::const_iterator ii = s.fields.begin();
	    while (true) {
		Field f = *ii;

		if (!(ii == s.fields.begin()))
		    os << indent(il) << "            ";

		if (isNumeric(f.type) && !f.optional && !f.array)
		    os << f.name << " == o." << f.name;
		else if (f.array && f.type != "binary")
		    os << "java.util.Arrays.equals(" << f.name << ", o." << f.name << ")";
		else
		    os << p.name << ".equals(" << f.name << ", o." << f.name << ")";

		if (++ii != s.fields.end())
		    os << " && \n";
		else {
		    os << ";\n";
		    break;
		}
	    }

	    os <<
		indent(il) << "    }\n\n" <<
		indent(il) << "    return false;\n" <<
		indent(il) << "}\n";
	} else
	    os <<
		indent(il) << "\n" <<
		indent(il) << "public boolean equals(Object _o)\n" <<
		indent(il) << "{\n" <<
		indent(il) << "    return _o instanceof " << s.name << ";\n" <<
		indent(il) << "}\n";
    }
}

static void dumpHashMethod(Protocol const&, Struct const& s, std::ostream& os, int il)
{
    if (s.isUserDefined()) {
	if (s.fields.size()) {
	    os <<
		"\n" <<
		indent(il) << "public int hashCode()\n" <<
		indent(il) << "{\n" <<
		indent(il) << "    return ";

	    FieldList::const_iterator ii = s.fields.begin();
	    while (true) {
		Field f = *ii;

		if (!(ii == s.fields.begin()))
		    os << indent(il) << "            ";

		if (isNumeric(f.type) && !f.optional && !f.array) {
		    if (f.type == "int16" || f.type == "int32")
			os << f.name;
		    else if (f.type == "bool")
			os << "(new Boolean(" << f.name << ").hashCode())";
		    else if (f.type == "int64")
			os << "(new Long(" << f.name << ").hashCode())";
		    else if (f.type == "double")
			os << "(new Double(" << f.name << ").hashCode())";
		} else if (f.array || f.type == "binary")
		    os << "java.util.Arrays.hashCode(" << f.name << ")";
		else if (f.optional)
		    os << "(" << f.name << " != null ? " << f.name << ".hashCode() : 0)";
		else
		    os << f.name << ".hashCode()";

		if (++ii != s.fields.end())
		    os << " + \n";
		else {
		    os << ";\n";
		    break;
		}
	    }

	    os <<
		indent(il) << "}\n";

	} else
	    os <<
		"\n" <<
		indent(il) << "public int hashCode()\n" <<
		indent(il) << "{\n" <<
		indent(il) << "    return super.hashCode();\n" <<
		indent(il) << "}\n";
    }
}

static void dumpStringMethod(Protocol const&, Struct const& s, std::ostream& os, int il)
{
    if (s.isUserDefined()) {
	os <<
	    "\n" <<
	    indent(il) << "public java.lang.String toString()\n" <<
	    indent(il) << "{\n" <<
	    indent(il) << "    StringBuilder buf = new StringBuilder(\"" << messageType(s) << " " << s.name << " {\\n\");\n\n";

	FieldList::const_iterator ii = s.fields.begin();
	while (ii != s.fields.end()) {
	    Field f = *ii++;

	    os << indent(il) << "    buf.append(\"    " << (f.optional ? "optional " : "") << f.type << " " << f.name << (f.array ? "[]" : "") << " = \");\n";

	    if (f.array || f.type == "binary") {
		os
		    << indent(il) << "    if (" << f.name << " != null) {\n"
		    << indent(il) << "        buf.append(\"" << f.type << "[\");\n"
		    << indent(il) << "        buf.append(" << f.name << ".length);\n"
		    << indent(il) << "        buf.append(\"];\\n\");\n"
		    << indent(il) << "    } else\n"
		    << indent(il) << "        buf.append(\"null;\\n\");\n";
	    } else if (f.type == "string") {
		os
		    << indent(il) << "    if (" << f.name << " != null" << ")\n"
		    << indent(il) << "        buf.append('\"');\n"
		    << indent(il) << "    buf.append(" << f.name << ");\n"
		    << indent(il) << "    if (" << f.name << " != null" << ")\n"
		    << indent(il) << "        buf.append('\"');\n"
		    << indent(il) << "    buf.append(\";\\n\");\n";
	    } else {
		os
		    << indent(il) << "    buf.append(" << f.name << ");\n"
		    << indent(il) << "    buf.append(\";\\n\");\n";
	    }

	    os << "\n";
	}

	os <<
	    indent(il) << "    buf.append(\"}\\n\");\n";

	os <<
	    indent(il) << "    return buf.toString();\n";

	os <<
	    indent(il) << "}\n";
    }
}

static void dumpConstructor(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    os <<
	"\n" <<
	indent(il) << "public " << s.name << "()\n" <<
	indent(il) << "{\n";

    FieldList::const_iterator ii = s.fields.begin();
    while (ii != s.fields.end()) {
	Field f = *ii++;

	if (!f.optional) {
	    if (userDefinedType(f.type)) {
		if (f.array)
		    os << indent(il) << "    " << f.name << " = new " << f.type << "[0];\n";
		else {
		    if (!isEnum(p, f.type))
			os << indent(il) << "    " << f.name << " = new " << f.type << "();\n";
		}
	    } else {
		std::string type = xlatType(f.type, false);

		if (f.array) {
		    if (f.type == "binary")
			os << indent(il) << "    " << f.name << " = new byte[0][0];\n";
		    else
			os << indent(il) << "    " << f.name << " = new " << type << "[0];\n";
		} else {
		    if (f.type == "binary")
			os << indent(il) << "    " << f.name << " = new byte[0];\n";
		    else if (f.type == "string")
			os << indent(il) << "    " << f.name << " = \"\";\n";
		    else if (f.type == "bool")
			os << indent(il) << "    " << f.name << " = false;\n";
		    else
			os << indent(il) << "    " << f.name << " = 0;\n";
		}
	    }
	}
    }

    os <<
	indent(il) << "}\n";

}

static void dumpMessage(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    os <<
	indent(il) << "public static class " << s.name << " extends " << messageType(s) << " {\n" <<
	indent(il) << "    static final long serialVersionUID = " << serialVersionUID(p, s) << "L;\n";

    dumpStructFields(p, s, os, il + 1);
    dumpConstructor(p, s, os, il + 1);

    os <<
	"\n" <<
	indent(il) << "    public void deliverTo(Receiver r)\n" <<
	indent(il) << "    {\n" <<
	indent(il) << "        r.handle(this);\n" <<
	indent(il) << "    }\n\n";

    if (javaStreams) {
	os <<
	    indent(il) << "    void marshal(java.io.DataOutputStream out) throws java.io.IOException\n" <<
	    indent(il) << "    {\n" <<
	    indent(il) << "        out.writeByte(0x51);\n" <<
	    indent(il) << "        out.writeByte(3);\n\n" <<
	    indent(il) << "        // Protocol name\n\n";

	CodeGenParams cgp(il + 2, IOTypeStream);

	emitConstantInt(os, cgp, 0x10, p.longHash(p.name));

	os <<
	    "\n" <<
	    indent(il) << "        // Message name\n\n";

	emitConstantInt(os, cgp, 0x10, s.hash);

	os <<
	    "\n" <<
	    indent(il) << "        // Message\n\n" <<
	    indent(il) << "        marshal(out, this);\n" <<
	    indent(il) << "    }\n\n";
    }

    if (javaBuffers) {
	os <<
	    indent(il) << "    void _marshal(java.nio.ByteBuffer out) throws java.io.IOException\n" <<
	    indent(il) << "    {\n" <<
	    indent(il) << "        out.put((byte)0x51);\n" <<
	    indent(il) << "        out.put((byte)3);\n\n" <<
	    indent(il) << "        // Protocol name\n\n";

	CodeGenParams cgp(il + 2, IOTypeBuffer);

	emitConstantInt(os, cgp, 0x10, p.longHash(p.name));

	os <<
	    "\n" <<
	    indent(il) << "        // Message name\n\n";

	emitConstantInt(os, cgp, 0x10, s.hash);

	os <<
	    "\n" <<
	    indent(il) << "        // Message\n\n" <<
	    indent(il) << "        marshal(out, this);\n" <<
	    indent(il) << "    }\n";
    }

    dumpTypeEquality(p, s, os, il + 1);
    dumpHashMethod(p, s, os, il + 1);
    dumpStringMethod(p, s, os, il + 1);

    os << indent(il) << "}\n\n";
}

static void dumpMessageHandler(Protocol const&, Struct const& s, std::ostream& os, int il)
{
    os <<
	indent(il) << "void handle(" << s.name << " m);\n";
}

static long serialVersionUID(Protocol const& p, Struct const& s)
{
    long id = 1;

    for (FieldList::const_iterator ii = s.fields.begin(); ii != s.fields.end(); ++ii)
	if (!ii->optional)
	    id += p.longHash(ii->name + ":" + ii->type);
    return id;
}

static void dumpStructUnmarshaller(Protocol const& p, Struct const& s, std::ostream& os, CodeGenParams cgp)
{
    if (s.isUserDefined() && s.isUsed()) {
	if (s.fields.size()) {
	    if (s.use == Struct::asEnum) {
		if (cgp.ioType == IOTypeStream)
		    os << indent(cgp) << "static " << s.name << " unmarshal_" << s.name << "(java.io.DataInputStream in) throws java.io.IOException\n";
		else
		    os << indent(cgp) << "static " << s.name << " unmarshal_" << s.name << "(java.nio.ByteBuffer in) throws java.io.IOException\n";

		os <<
		    indent(cgp) << "{\n" <<
		    indent(cgp) << "    long v = readInt(in, 0x80);\n\n" <<
		    indent(cgp) << "    if (v >= Integer.MIN_VALUE && v <= Integer.MAX_VALUE) {\n" <<
		    indent(cgp) << "        switch ((int) v) {\n";

		FieldList::const_iterator ii = s.fields.begin();
		while (ii != s.fields.end()) {
		    Field f = *ii;
		    os <<
			indent(cgp) << "         case " << f.hash << ":\n" <<
			indent(cgp) << "            return " << s.name << "." << f.name << ";\n";
		    ii++;

		    if (ii != s.fields.end())
			os << "\n";
		}

		os <<
		    indent(cgp) << "        }\n" <<
		    indent(cgp) << "    }\n\n" <<
		    indent(cgp) << "    throw new java.io.IOException(\"invalid enum value\");\n" <<
		    indent(cgp) << "}\n\n";
	    } else {
		size_t const total = countRequired(s);
		size_t current = 0;

		if (cgp.ioType == IOTypeStream)
		    os << indent(cgp) << "static " << s.name << " unmarshal_" << s.name << "(java.io.DataInputStream in) throws java.io.IOException\n";
		else
		    os << indent(cgp) << "static " << s.name << " unmarshal_" << s.name << "(java.nio.ByteBuffer in) throws java.io.IOException\n";

		os <<
		    indent(cgp) << "{\n" <<
		    indent(cgp) << "    final int flds = readLength(in, 0x50);\n\n" <<
		    indent(cgp) << "    if (flds < 0)\n" <<
		    indent(cgp) << "        throw new java.io.IOException(\"invalid array size\");\n\n";

		if (total)
		    os <<
			indent(cgp) << "    java.util.BitSet flg = new java.util.BitSet(" << total << ");\n";

		os <<
		    indent(cgp) << "    " << s.name << " d = new " << s.name << "();\n\n" <<
		    indent(cgp) << "    for (int ii = 0; ii < flds; ii += 2) {\n" <<
		    indent(cgp) << "        switch (unmarshal_int16(in)) {\n";

		apply(p, os, s.fields.begin(), s.fields.end(), unmarshalField, &current);

		os <<
		    indent(cgp) << "         default:\n" <<
		    indent(cgp) << "            throw new java.io.IOException(\"unknown field in message\");\n" <<
		    indent(cgp) << "        }\n" <<
		    indent(cgp) << "    }\n\n";

		if (total)
		    os <<
			indent(cgp) << "    if (flg.cardinality() != " << total << ")\n" <<
			indent(cgp) << "        throw new java.io.IOException(\"missing required field(s)\");\n\n";

		os <<
		    indent(cgp) << "    return d;\n" <<
		    indent(cgp) << "}\n\n";
	    }
	} else {
	    if (cgp.ioType == IOTypeStream)
		os << indent(cgp) << "static " << s.name << " unmarshal_" << s.name << "(java.io.DataInputStream in) throws java.io.IOException\n";
	    else
		os << indent(cgp) << "static " << s.name << " unmarshal_" << s.name << "(java.nio.ByteBuffer in) throws java.io.IOException\n";

	    os <<
		indent(cgp) << "{\n" <<
		indent(cgp) << "    if (readLength(in, 0x50) != 0)\n" <<
		indent(cgp) << "        throw new java.io.IOException(\"invalid message length\");\n\n" <<
		indent(cgp) << "    return new " << s.name << "();\n" <<
		indent(cgp) << "}\n\n";
	}
    }
}

static void dumpMsgUnmarshaller(Protocol const&, Struct const& s,
				std::ostream& os, int il)
{
    os <<
	indent(il) << " case " << s.hash << ":\n" <<
	indent(il) << "    return unmarshal_" << s.name << "(in);\n\n";
}

// This function generates the .h and .cpp files for a single
// protocol. It does this by opening the two output files and then
// calling the two functions that actually generate the content for
// each file type.

static void dumpProtocol(Protocol const& p)
{
    std::ofstream src((targetPath + "/" + p.name + ".java").c_str());

    src <<
	"// Generated by the protocol compiler version " << pcVersion << "\n"
	"// DO NOT EDIT THIS FILE DIRECTLY!\n\n";

    if (javaPkg.size())
	src << "package " << javaPkg << ";\n\n";

    dumpSourceFile(p, src);
}

static void dumpArrayMarshaller(Protocol const&, Struct const& s, std::ostream& os, CodeGenParams cgp)
{
    if (s.usedInArray) {
	    if (cgp.ioType == IOTypeStream)
		os << indent(cgp) << "static void marshal(java.io.DataOutputStream out, " << xlatType(s.name, false) << "[] v) throws java.io.IOException\n";
	    else
		os << indent(cgp) << "static void marshal(java.nio.ByteBuffer out, " << xlatType(s.name, false) << "[] v) throws java.io.IOException\n";

	    os <<
		indent(cgp) << "{\n" <<
		indent(cgp) << "    writeLength(out, 0x50, v.length);\n" <<
		indent(cgp) << "    for (" << xlatType(s.name, false) << "  o : v)\n" <<
		indent(cgp) << "        marshal(out, o);\n" <<
		indent(cgp) << "}\n\n";
    }
}

static void dumpMessageType(Protocol const& p, std::ostream& os, StructList const& list, int il)
{
    if (list.size()) {
	std::string msgType = messageType(list.front());

	os <<
	    indent(il) << "//\n" <<
	    indent(il) << "// " << msgType << " types\n" <<
	    indent(il) << "//\n" <<
	    indent(il) << "public abstract static class " << msgType << " extends " << p.name << " {\n\n";


	if (javaStreams) {
	    os <<
		indent(il) << "    public static " <<  p.name << '.' << msgType << " unmarshal(java.io.InputStream is) throws java.io.IOException\n" <<
		indent(il) << "    {\n" <<
		indent(il) << "        java.io.DataInputStream in = new java.io.DataInputStream(is);\n\n" <<
		indent(il) << "        byte[] hdr = new byte[3];\n" <<
		indent(il) << "        if (in.read(hdr) != hdr.length)\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid SDD message format\");\n\n" <<
		indent(il) << "        if (!(new java.lang.String(hdr)).equals(\"SDD\"))\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid SDD message format\");\n\n" <<
		indent(il) << "        if (in.readByte() != 2)\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid SDD version number\");\n\n" <<
		indent(il) << "        if (readLength(in, 0x50) != 3)\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid start of message for the '" << p.name << "' protocol\");\n\n" <<
		indent(il) << "        if (unmarshal_int32(in) != " << p.longHash(p.name) << ")\n" <<
		indent(il) << "            throw new java.io.IOException(\"unknown protocol\");\n\n" <<
		indent(il) << "        switch (unmarshal_int16(in)) {\n";

	    apply(p, os, list.begin(), list.end(), dumpMsgUnmarshaller, il + 2);

	    os <<
		indent(il) << "         default:\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid message for '" << p.name << "' protocol\");\n" <<
		indent(il) << "        }\n" <<
		indent(il) << "    }\n\n";

	    CodeGenParams cgp(il + 1, IOTypeStream);

	    apply(p, os, list.begin(), list.end(), dumpStructMarshaller, cgp);
	    apply(p, os, list.begin(), list.end(), dumpStructUnmarshaller, cgp);
	}

	if (javaBuffers) {
	    os <<
		indent(il) << "    public static " <<  p.name << '.' << msgType << " unmarshal(java.nio.ByteBuffer in) throws java.io.IOException\n" <<
		indent(il) << "    {\n" <<
		indent(il) << "        byte[] hdr = new byte[3];\n\n" <<
		indent(il) << "        in.get(hdr);\n\n" <<
		indent(il) << "        if (!(new java.lang.String(hdr)).equals(\"SDD\"))\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid SDD message format\");\n\n" <<
		indent(il) << "        if (in.get() != 2)\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid SDD version number\");\n\n" <<
		indent(il) << "        if (readLength(in, 0x50) != 3)\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid start of message for the '" << p.name << "' protocol\");\n\n" <<
		indent(il) << "        if (unmarshal_int32(in) != " << p.longHash(p.name) << ")\n" <<
		indent(il) << "            throw new java.io.IOException(\"unknown protocol\");\n\n" <<
		indent(il) << "        switch (unmarshal_int16(in)) {\n";

	    apply(p, os, list.begin(), list.end(), dumpMsgUnmarshaller, il + 2);

	    os <<
		indent(il) << "         default:\n" <<
		indent(il) << "            throw new java.io.IOException(\"invalid message for '" << p.name << "' protocol\");\n" <<
		indent(il) << "        }\n" <<
		indent(il) << "    }\n\n";

	    CodeGenParams cgp(il + 1, IOTypeBuffer);

	    apply(p, os, list.begin(), list.end(), dumpStructMarshaller, CodeGenParams(il + 1, IOTypeBuffer));
	    apply(p, os, list.begin(), list.end(), dumpStructUnmarshaller, CodeGenParams(il + 1, IOTypeBuffer));
	}

	os <<
	    indent(il) << "    // Interface for receiving all messages.\n\n" <<
	    indent(il) << "    public interface Receiver {\n";

	apply(p, os, list.begin(), list.end(), dumpMessageHandler, il + 2);

	os <<
	    indent(il) << "    }\n\n";

	os <<
	    indent(il) << "    public abstract void deliverTo(Receiver r);\n\n";

	apply(p, os, list.begin(), list.end(), dumpMessage, il + 1);

	os << indent(il) << "}\n\n";
    }
}

// This function generates the .java contents for a protocol.

static void dumpSourceFile(Protocol const& p, std::ostream& os)
{
    os <<
	"// Start of the " << p.name << " object hierarchy.\n\n"
	"public abstract class " << p.name << " implements java.io.Serializable {\n"
	"    private static final java.nio.charset.Charset charSet = java.nio.charset.Charset.forName(\"ISO-8859-1\");\n\n"
	"    // Data types used in messages, but which aren't messages themselves.\n\n";

    apply(p, os, p.types.begin(), p.types.end(), dumpType, 1);

    if (javaStreams) {
	CodeGenParams cgp(1, IOTypeStream);

	apply(p, os, p.types.begin(), p.types.end(), dumpStructMarshaller, cgp);
	apply(p, os, p.types.begin(), p.types.end(), dumpStructUnmarshaller, cgp);
	apply(p, os, p.types.begin(), p.types.end(), dumpArrayMarshaller, cgp);
	os << "    abstract void marshal(java.io.DataOutputStream out) throws java.io.IOException;\n\n" << streamPrimitives;
    }

    if (javaBuffers) {
	CodeGenParams cgp(1, IOTypeBuffer);

	apply(p, os, p.types.begin(), p.types.end(), dumpStructMarshaller, cgp);
	apply(p, os, p.types.begin(), p.types.end(), dumpStructUnmarshaller, cgp);
	apply(p, os, p.types.begin(), p.types.end(), dumpArrayMarshaller, cgp);
	os << "    abstract void _marshal(java.nio.ByteBuffer out) throws java.io.IOException;\n\n" << bufferPrimitives;
    }

    os << commonPrimitives;


    dumpMessageType(p, os, p.requests, 1);
    dumpMessageType(p, os, p.replies, 1);

    os << "}\n\n";
}

// Dumps all the fields of a structure. It assumes the stream is
// "ready" for the fields (i.e. the C++ prologue codes has already
// been written and the epilogue will be written at a later time.

static void dumpStructFields(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    apply(p, os, s.fields.begin(), s.fields.end(), dumpField, il);
}

// Dumps a "type" to the stream. A type is a class that isn't part
// of the protocol object hierarchy.

static void dumpType(Protocol const& p, Struct const& s, std::ostream& os, int il)
{
    if (!s.fields.empty() && s.isUsed()) {
	if (s.use == Struct::asEnum) {
	    os <<
		indent(il) << "public static enum " << s.name << " {\n";

	    apply(p, os, s.fields.begin(), s.fields.end(), dumpEnumField, il + 1);

	    os <<
		indent(il) << "}\n\n";

	} else {
	    os <<
		indent(il) << "public static class " << s.name << " implements java.io.Serializable {\n" <<
		indent(il) << "    static final long serialVersionUID = " << serialVersionUID(p, s) << "L;\n";

	    dumpStructFields(p, s, os, il + 1);
	    dumpConstructor(p, s, os, il + 1);
	    dumpTypeEquality(p, s, os, il + 1);
	    dumpHashMethod(p, s, os, il + 1);
	    dumpStringMethod(p, s, os, il + 1);

	    if (javaStreams)
		os <<
		    indent(il + 1) << "\n" <<
		    indent(il + 1) << "public void marshal(java.io.OutputStream out) throws java.io.IOException\n" <<
		    indent(il + 1) << "{\n" <<
		    indent(il + 2) << p.name << ".marshal(new java.io.DataOutputStream(out), this);\n" <<
		    indent(il + 1) << "}\n";

	    if (javaBuffers)
		os <<
		    indent(il + 1) << "\n" <<
		    indent(il + 1) << "public void marshal(java.nio.ByteBuffer out) throws java.io.IOException\n" <<
		    indent(il + 1) << "{\n" <<
		    indent(il + 2) << p.name << ".marshal(out, this);\n" <<
		    indent(il + 1) << "}\n";

	    os << indent(il) << "}\n\n";
	}
    }
}

static void dumpStructMarshaller(Protocol const&, Struct const& s,
				 std::ostream& os, CodeGenParams cgp)
{
    if (s.isUserDefined() && s.isUsed()) {

	if (cgp.ioType == IOTypeStream)
	    os << indent(cgp) << "static void marshal(java.io.DataOutputStream out, " << s.name << " v) throws java.io.IOException\n";
	else if (cgp.ioType == IOTypeBuffer)
	    os << indent(cgp) << "static void marshal(java.nio.ByteBuffer out, " << s.name << " v) throws java.io.IOException\n";

	os << indent(cgp) << "{\n";

	if (s.use == Struct::asEnum) {
	    os <<
		indent(cgp) << "    switch(v) {\n";

		FieldList::const_iterator ii = s.fields.begin();
		while (ii != s.fields.end()) {
		    Field f = *ii;
		    os <<
			indent(cgp) << "     case " << f.name << ":\n";// <<

		    emitConstantInt(os, cgp + 2, 0x80, f.hash);

		    os <<
			indent(cgp) << "        break;\n";
		    ii++;

		    if (ii != s.fields.end())
			os << "\n";
		}

	    os <<
		indent(cgp) << "    }\n";
	} else {
	    // Figure out number of fields that will be written

	    if (countRequired(s) == s.fields.size())
		emitConstantInt(os, cgp + 1, 0x50, countRequired(s) * 2);
	    else {
		os <<
		    indent(cgp) << "    int flds = " << (countRequired(s) * 2) << ";\n\n";

		FieldList::const_iterator ii = s.fields.begin();
		while (ii != s.fields.end()) {
		    Field f = *ii;

		    if (f.optional)
			os <<
			    indent(cgp) << "    if (v." << f.name << " != null)\n" <<
			    indent(cgp) << "        flds += 2;\n";
		    ii++;
		}

		os <<
		    indent(cgp) << "    writeLength(out, 0x50, flds);\n";
	    }

	    // Write out marshalling of the fields

	    FieldList::const_iterator ii = s.fields.begin();
	    while (ii != s.fields.end()) {
		Field f = *ii;

		if (f.optional) {
		    os <<
			"\n" <<
			indent(cgp) << "    if (v." << f.name << " != null) {\n";

		    emitConstantInt(os, cgp + 2, 0x10, f.hash);

		    os <<
			indent(cgp) << "        marshal(out, v." << f.name << ");\n" <<
			indent(cgp) << "    }\n";
		} else {
		    os << "\n";

		    emitConstantInt(os, cgp + 1, 0x10, f.hash);

		    os <<
			indent(cgp) << "    marshal(out, v." << f.name << ");\n";
		}

		ii++;
	    }
	}

	os <<
	    indent(cgp) << "}\n\n";
    }
}

static bool isNumeric(std::string const& type)
{
    return type == "bool" || type == "int16" || type == "int32" || type == "int64" || type == "double";
}

static void unmarshalField(Protocol const&, Field const& f, std::ostream& os,
			   size_t* current)
{
    os <<
	"             case " << f.hash << ": //" << f.type << " " << f.name << "\n"
	"                {\n";

    if (!f.optional)
	os <<
	    "                    flg.set(" << (*current)++ << ");\n";

    if (f.array) {
	os <<
	    "                    int arrSize = readLength(in, 0x50);\n\n"
	    "                    if (arrSize < 0)\n"
	    "                        throw new java.io.IOException(\"invalid array size\");\n\n";

        if (f.type == "binary")
	    os << "                    d." << f.name << " = new byte[arrSize][];\n\n";
        else
	    os << "                    d." << f.name << " = new " << xlatType(f.type, false) << "[arrSize];\n\n";

    os <<
        "                    for (int jj = 0; jj < arrSize; ++jj)\n"
        "                        d." << f.name << "[jj] = " << "unmarshal_" << f.type << "(in);\n"
        "                    \n";
    } else {
	if (f.optional && isNumeric(f.type))
	    os <<
		"                    d." << f.name << " = new " << xlatType(f.type, true) << "(unmarshal_" << f.type << "(in));\n";
	else
	    os <<
		"                    d." << f.name << " = unmarshal_" << f.type << "(in);\n";
    }

    os <<
	"                }\n"
	"                break;\n\n";
}

bool isEnum(Protocol const& p, std::string const& type)
{
    return p.findType(type).use == Struct::asEnum;
}

static bool userDefinedType(std::string const& type)
{
    return xlatType(type, true) == type;
}

static std::string xlatType(std::string const& type, bool useObj)
{
    if (type == "bool")
	return useObj ? "Boolean" : "boolean";
    else if (type == "int16")
	return useObj ? "Short" : "short";
    else if (type == "int32")
	return useObj ? "Integer" : "int";
    else if (type == "int64")
	return useObj ? "Long" : "long";
    else if (type == "double")
	return useObj ? "Double" : "double";
    else if (type == "string")
	return "java.lang.String";
    else if (type == "binary")
	return "byte[]";
    else
	return type;
}

// Takes a Field and returns the SDD mapping of the Field's type.

static std::string xlatType(Field const& f)
{
    std::string jType = xlatType(f.type, !f.array && f.optional);

    if (f.array)
	jType.append("[]");

    return jType;
}

static std::string messageType(Struct const& s)
{
    switch (s.use) {
     case Struct::asRequest:
	return "Request";

     case Struct::asReply:
	return "Reply";

     default:
	return "";
    }
}

// The only externally visible function from this module. This is the
// entry point to generate C++ files that handle the protocol.

void generateJava(Protocol const& p)
{
    if (sddVersion != 2)
	throw std::runtime_error("Java generator only supports SDDv2");

    if (!javaStreams && !javaBuffers)
	javaStreams = javaBuffers = true;

    dumpProtocol(p);
}
