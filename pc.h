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

#ifndef __PC_H
#define __PC_H

#include <stdint.h>
#include <cstdio>
#include <algorithm>
#include <string>
#include <list>
#include <stdexcept>
#include <iostream>
#include <map>
#include <set>

typedef std::ostream::char_type byte;
typedef std::set<int16_t> HashSet;

// The Lexer or Parser will throw parse_error exceptions if errors are
// found in the proto file.

class parse_error : public std::runtime_error {
 public:
    parse_error(char const*, size_t, size_t);
};

enum TargetType { None = 0, Client = 1, Server = 2, Both = 3 };
enum UseType { Nowhere = 0, InRequest = 1, InReply = 2, InBoth = 3,
	       InEither = 3 };

inline UseType operator|(UseType const a, UseType const b)
{
    return UseType(int(a) | int(b));
}

inline UseType operator&(UseType const a, UseType const b)
{
    return UseType(int(a) & int(b));
}

enum CppLevel {
    cppLegacy, cpp11, cpp14, cpp17, cpp_exp
};

enum CppGenFiles {
    cppBoth, cppHeader, cppSource
};

extern char const pcVersion[];
extern bool warnings;
extern bool verbose;
extern bool allStructs;
extern bool checkReturns;
extern CppLevel cppLevel;
extern CppGenFiles cppGenFiles;
extern bool erlangStringsAsBinaries;
extern bool erlangNative;
extern bool ocamlPolyVT;
extern TargetType target;
extern std::string javaPkg;
extern bool javaStreams;
extern bool javaBuffers;
extern std::string targetPath;
extern bool objcUseARC;
extern bool pyV3;
extern bool jsEmitLib;
extern bool jsEmitDts;
extern bool jsSingletons;
extern byte const js_lib[];
extern size_t const js_lib_size;
extern std::string rustAllPublic;
extern unsigned sddVersion;

struct Protocol;

// A FIELD holds information for one field of a structure or message.

struct Field {
    std::string const name;
    int16_t const hash;
    std::string const type;
    bool optional;
    bool array;
    int arraySize;

    Field(std::string const& n, int16_t const h, std::string const& t,
	  bool const isOptional) :
	name(n), hash(h), type(t), optional(isOptional), array(false),
	arraySize(0) {}
};

typedef std::list<Field> FieldList;

// A STRUCT holds information for a local type, request, reply, or
// enumeration. All of these items are simply a name and a list of
// fields.

struct Struct;
typedef std::list<Struct> StructList;

struct Struct {
    enum Usage { asBuiltIn, asStruct, asRequest, asReply, asEnum };

    std::string name;
    int16_t const hash;
    bool anonymous;
    UseType used;
    bool usedInArray;
    bool inReturns;
    Usage use;
    FieldList fields;

    Struct(std::string const&, Usage = asBuiltIn);

    bool hasOptionals() const;
    bool isUserDefined() const { return use != asBuiltIn; }

    bool isUsed(UseType const ut) const
    { return allStructs || (used & ut) != Nowhere; }

    bool isUsed() const { return isUsed(InEither); }

    void markUsedTypes(Protocol& p, UseType);
    int16_t genFldHash(std::string const&);

 private:
    HashSet fieldHashes;

    Struct();
};

// A PROTOCOL is made up of a series of messages. The PROTOCOL may
// also have local types defined.

struct Protocol {
    std::string name;
    StructList types;
    StructList requests;
    StructList replies;

    Protocol();

    Struct const& findType(std::string const&) const;
    Struct& findType(std::string const&);

    bool anyArrays(UseType) const;
    bool anyOptionals() const;
    bool anyOptionalArrays() const;
    bool needed(UseType, std::string const&) const;

    bool needed(std::string const& n) const { return needed(InEither, n); }

    size_t maxNumberOfFields() const;

    int32_t longHash(std::string const&) const;

    static int16_t genGblHash(std::string const&, Struct::Usage);

 private:
    static HashSet globalHashes;
};

// These templates allow us to apply a function to a range of items in
// a container. There was a problem using std::for_each() (it wouldn't
// let me pass arguments to my function by reference), so I copied
// for_each() and cut out portions I didn't need.

template<typename _InputIterator, typename _Function>
void apply(Protocol const& p, std::ostream& os, _InputIterator __first,
	   _InputIterator __last, _Function __f)
{
#ifndef DARWIN
    __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
    __glibcxx_requires_valid_range(__first, __last);
#endif

    for ( ; __first != __last; ++__first)
	__f(p, *__first, os);
}

template<typename _InputIterator, typename _Function>
void apply_sep(Protocol const& p, std::ostream& os, _InputIterator __first,
	       _InputIterator __last, _Function __f, std::string sep)
{
    bool emitSep = false;
#ifndef DARWIN
    __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>);
    __glibcxx_requires_valid_range(__first, __last);
#endif
    for (; __first != __last; ++__first)
	emitSep |= __f(p, *__first, os, emitSep ? sep : "");
}

template<typename _InputIterator, typename _Function, typename T>
     void apply(Protocol const& p, std::ostream& os, _InputIterator __first, _InputIterator __last, _Function __f, T __extra)
{
#ifndef DARWIN
    __glibcxx_function_requires(_InputIteratorConcept<_InputIterator>)
    __glibcxx_requires_valid_range(__first, __last);
#endif

    for ( ; __first != __last; ++__first)
	__f(p, *__first, os, __extra);
}

bool isEnum(Protocol const&, std::string const&);
size_t countRequired(Struct const&);
void generateCPlusPlus(Protocol const&);
void generateErlang(Protocol const&);
void generateHaskell(Protocol const&);
void generateJava(Protocol const&);
void generateObjC(Protocol const&);
void generateOCaml(Protocol const&);
void generateJS(Protocol const&);
void generateGwtJava(Protocol const&);
void generatePython(Protocol const&);
void generateRust(Protocol const&);
Protocol parse(std::string const&);
void emitRawInt(std::ostream&, byte, int64_t);
std::string toLower(std::string const&);
std::string toUpper(std::string const&);
std::string usage(Struct const&);

extern "C" int32_t md5hash(void const*, size_t, void const*, size_t);

#define MAX_CONTAINER_OVERHEAD	5
#define MIN_CONTAINER_OVERHEAD	2
#define ENCSIZE_BOOL		1
#define ENCSIZE_INT16		3
#define ENCSIZE_INT32		5
#define ENCSIZE_INT64		9
#define ENCSIZE_DOUBLE		9
#define ARRAY_OVERHEAD		3
#define SDD_OVERHEAD		(4 + MIN_CONTAINER_OVERHEAD + ENCSIZE_INT32 + ENCSIZE_INT16)

size_t containerHeaderLength(size_t);

#ifndef NDEBUG
#include <ostream>

inline std::ostream& operator<<(std::ostream& os, UseType const a)
{
    switch (a) {
     case Nowhere:
	return os << "Nowhere";

     case InRequest:
	return os << "InRequest";

     case InReply:
	return os << "InReply";

     case InEither:
	return os << "InEither";

     default:
	return os << "*** UNKNOWN VALUE ***";
    }
}
#endif

#endif

// Local Variables:
// mode:c++
// End:
