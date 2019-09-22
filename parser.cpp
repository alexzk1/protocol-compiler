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

#include <iostream>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <fstream>
#include "lexer.h"
#include "pc.h"

class TypeMatch {
    std::string const name;

 public:
    TypeMatch(std::string const& _name) : name(_name) {}
    bool operator()(Struct const& s) const { return name == s.name; }
};

static Field parseField(Protocol&, Struct&, Lexer&,
			std::string const& = std::string());

static std::string const s[] = {
    "bool", "int16", "int32", "int64", "double", "string", "binary"
};

HashSet Protocol::globalHashes;

Struct::Struct(std::string const& _n, Usage const _u) :
    name(_n), hash(Protocol::genGblHash(_n, _u)), anonymous(false),
    used(Nowhere), usedInArray(false), inReturns(false), use(_u)
{
}

Protocol::Protocol() :
    types(s, s + sizeof(s) / sizeof(*s))
{
}

Struct& Protocol::findType(std::string const& name)
{
    StructList::iterator ii =
	std::find_if(types.begin(), types.end(), TypeMatch(name));

    if (ii != types.end())
	return *ii;
    else
	throw std::logic_error("error looking for type '" + name + "'");
}

Struct const& Protocol::findType(std::string const& name) const
{
    return const_cast<Protocol*>(this)->findType(name);
}

bool Protocol::needed(UseType const ut, std::string const& name) const
{
    return findType(name).isUsed(ut);
}

bool Protocol::anyOptionals() const
{
    StructList::const_iterator ii;

    for (ii = types.begin(); ii != types.end(); ++ii) {
	FieldList::const_iterator jj;

	for (jj = ii->fields.begin(); jj != ii->fields.end(); ++jj)
	    if (jj->optional && !jj->array)
		return true;
    }
    for (ii = requests.begin(); ii != requests.end(); ++ii) {
	FieldList::const_iterator jj;

	for (jj = ii->fields.begin(); jj != ii->fields.end(); ++jj)
	    if (jj->optional && !jj->array)
		return true;
    }
    for (ii = replies.begin(); ii != replies.end(); ++ii) {
	FieldList::const_iterator jj;

	for (jj = ii->fields.begin(); jj != ii->fields.end(); ++jj)
	    if (jj->optional && !jj->array)
		return true;
    }
    return false;
}

static bool findArrayInStruct(Protocol const& p, Struct const& s)
{
    for (FieldList::const_iterator ii = s.fields.begin();
	 ii != s.fields.end(); ++ii) {
#ifdef DEBUG
	std::cerr << "Checking if " << ii->name << " in struct " << s.name <<
	    " is an array, (" << ii->array << ")." << std::endl;
#endif
	if (ii->array)
	    return true;

	Struct const& sub = p.findType(ii->type);

	if (sub.use == Struct::asStruct && sub.name != s.name &&
	    findArrayInStruct(p, sub))
	    return true;
    }
    return false;
}

static bool findArray(Protocol const& p, StructList const& sl)
{
    for (StructList::const_iterator ii = sl.begin(); ii != sl.end(); ++ii)
	if (findArrayInStruct(p, *ii))
	    return true;
    return false;
}

bool Protocol::anyArrays(UseType const ut) const
{
    return ((ut & InRequest) == InRequest && findArray(*this, requests)) ||
	((ut & InReply) == InReply && findArray(*this, replies));
}

bool Protocol::anyOptionalArrays() const
{
    StructList::const_iterator ii;

    for (ii = types.begin(); ii != types.end(); ++ii) {
	FieldList::const_iterator jj;

	for (jj = ii->fields.begin(); jj != ii->fields.end(); ++jj)
	    if (jj->optional && jj->array)
		return true;
    }
    for (ii = requests.begin(); ii != requests.end(); ++ii) {
	FieldList::const_iterator jj;

	for (jj = ii->fields.begin(); jj != ii->fields.end(); ++jj)
	    if (jj->optional && jj->array)
		return true;
    }
    for (ii = replies.begin(); ii != replies.end(); ++ii) {
	FieldList::const_iterator jj;

	for (jj = ii->fields.begin(); jj != ii->fields.end(); ++jj)
	    if (jj->optional && jj->array)
		return true;
    }
    return false;
}

size_t Protocol::maxNumberOfFields() const
{
    size_t mx = 0;
    StructList::const_iterator ii;

    for (ii = types.begin(); ii != types.end(); ++ii)
	if (ii->fields.size() > mx)
	    mx = ii->fields.size();

    for (ii = requests.begin(); ii != requests.end(); ++ii)
	if (ii->fields.size() > mx)
	    mx = ii->fields.size();

    for (ii = replies.begin(); ii != replies.end(); ++ii)
	if (ii->fields.size() > mx)
	    mx = ii->fields.size();
    return mx;
}

int16_t Protocol::genGblHash(std::string const& n, Struct::Usage const u)
{
    if (u == Struct::asRequest || u == Struct::asReply) {
	std::string const name = (u == Struct::asRequest ? "req" : "rpy") + n;

	size_t total = 0;
	int16_t tmp;

	do {
	    std::string const suffix = std::string(total++, '_');

	    tmp = md5hash(name.c_str(), name.size(), suffix.c_str(),
			  suffix.size());
	} while(globalHashes.find(tmp) != globalHashes.end());
	globalHashes.insert(tmp);
	return tmp;
    } else
	return 0;
}

int16_t Struct::genFldHash(std::string const& fname)
{
    size_t total = 0;
    int16_t tmp;

    do {
	std::string const suffix = std::string(total++, '_');

	tmp = md5hash(fname.c_str(), fname.size(), suffix.c_str(),
		      suffix.size());
    } while(fieldHashes.find(tmp) != fieldHashes.end());
    fieldHashes.insert(tmp);
    return tmp;
}

// A helper function for the parse_error constructor.

static std::string form(char const* const text, size_t const line,
			size_t const col)
{
    std::ostringstream os;

    os << ":" << line << ":" << col << ": error: " << text;
    return os.str();
}

// Builds a runtime_error exception that incorporates the line and
// column numbers where the error took place.

parse_error::parse_error(char const* const text, size_t const line,
			 size_t const col) :
    std::runtime_error(form(text, line, col).c_str())
{
}

static std::string nextAnonymousName()
{
    static int anonymousCount = 0;
    std::ostringstream os;

    os << "Anon_" << std::setfill('0') << std::setw(4) << ++anonymousCount;

    return os.str();
}

static Struct parseAnonymousStruct(Protocol& p, Lexer& lex)
{
    Token token = lex.next();

    if (token.type == OPEN_CURLY) {
	Struct s(nextAnonymousName(), Struct::asStruct);

	s.anonymous = true;

	while ((token = lex.next()).type != CLOSE_CURLY) {
	    lex.pushToken(token);
	    s.fields.push_back(parseField(p, s, lex));
	}

	return s;
    } else
	throw parse_error("Missing open brace", token.line, token.column);
}

static Field parseField(Protocol& p, Struct& str, Lexer& lex,
			std::string const& sname)
{
    Token token = lex.next();
    bool fieldHasAnonymousType = false;
    bool isOptional = false;

    if (token.type == OPTIONAL) {
	isOptional = true;
	token = lex.next();
    }

    std::string fldType;

    if (token.type == IDENTIFIER) {
	StructList::const_iterator ii =
	    std::find_if(p.types.begin(), p.types.end(),
			 TypeMatch(token.value));

	if (ii != p.types.end() || (sname != "" && isOptional))
	    fldType = token.value;
	else
	    throw parse_error("Unknown type", token.line, token.column);
    } else if (token.type == STRUCT) {
	if (!isOptional) {
	    Struct s = parseAnonymousStruct(p, lex);

	    if (s.fields.empty())
		throw parse_error("Must provide at least one field in a struct",
				  token.line, token.column);
	    else
		s.use = Struct::asStruct;

	    p.types.push_back(s);

	    fldType = s.name;
	    fieldHasAnonymousType = true;
	} else
	    throw parse_error("Optional fields can't be anonymous",
			      token.line, token.column);
    } else
	throw parse_error("Need to specify a type name",
			  token.line, token.column);

    token = lex.next();

    if (token.type != IDENTIFIER)
	throw parse_error("Need to specify a field name",
			  token.line, token.column);

    Field f(token.value, str.genFldHash(token.value), fldType, isOptional);

    if ((token = lex.next()).type == OPEN_ARRAY) {
	if ((token = lex.next()).type == NUMBER) {
	    f.arraySize = atoi(token.value.c_str());
	    if (f.arraySize < 1 || f.arraySize > 0xffff)
		throw parse_error("Array size must be in the range 1 - 65535 ", token.line, token.column);
	    token = lex.next();
	}

	if (token.type == CLOSE_ARRAY)
	    if (!fieldHasAnonymousType) {
		f.array = true;
		token = lex.next();
	    } else
		throw parse_error("Arrays can't contain anonymous types",
				  token.line, token.column);
	else
	    throw parse_error("Expected ']'", token.line, token.column);
    }

    if (token.type != SEMICOLON)
	throw parse_error("Expected a semicolon",
			  token.line, token.column);

    return f;
}

static Struct parseStruct(Protocol& p, Lexer& lex, Struct::Usage const u)
{
    Token token = lex.next();

    if (token.type == IDENTIFIER) {
	Struct s(token.value, u);

	if ((token = lex.next()).type == OPEN_CURLY) {
	    while ((token = lex.next()).type != CLOSE_CURLY) {
		lex.pushToken(token);
		s.fields.push_back(parseField(p, s, lex, s.name));
	    }

	    return s;
	} else
	    throw parse_error("Missing open brace", token.line, token.column);
    } else
	throw parse_error("Messages and structs need to be followed by an "
			  "identifier", token.line, token.column);
}

static Struct parseEnum(Lexer& lex)
{
    Token token = lex.next();

    if (token.type == IDENTIFIER) {
	Struct s(token.value, Struct::asEnum);

	if ((token = lex.next()).type == OPEN_CURLY) {
	    while ((token = lex.next()).type != CLOSE_CURLY) {
		if (token.type == IDENTIFIER) {
		    Field f(token.value, s.genFldHash(token.value), "int16",
			    false);

		    s.fields.push_back(f);
		} else
		    throw parse_error("Bad enum value", token.line, token.column);

		token = lex.next();
		if (token.type == CLOSE_CURLY)
		    lex.pushToken(token);
		else if (token.type != COMMA)
		    throw parse_error("Invalid enum definition", token.line, token.column);
	    }

	    return s;
	} else
	    throw parse_error("Missing open brace", token.line, token.column);
    } else
	throw parse_error("enums need to be followed by an identifier",
			  token.line, token.column);
}

void Struct::markUsedTypes(Protocol& p, UseType const ut)
{
    used = used | ut;
#ifdef DEBUG
    std::cerr << "Type '" << name << "' use is " << used << " and is" <<
	(usedAsArray ? "" : " not") << " used in an array." <<  std::endl;
#endif

    if (use != Struct::asEnum) {
	FieldList::const_iterator ii;

	for (ii = fields.begin(); ii != fields.end(); ++ii)
	    if (ii->type != name) {
		Struct& jj = p.findType(ii->type);

		if (ii->array)
		    jj.usedInArray = true;
		jj.markUsedTypes(p, ut);
	    }
    }
}

static void parseReturnsStatement(Lexer& lex, Protocol& p, Struct& s)
{
    Token token = lex.next();

    if (token.type == RETURNS) {
	bool replies = false;

	// Parse the replies in a returns statement

	while ((token = lex.next()).type != ENDOFFILE) {
	    if (token.type == MULTIPLE || token.type == SINGLE) {
		if ((token = lex.next()).type == IDENTIFIER) {
		    StructList::iterator jj =
			std::find_if(p.replies.begin(), p.replies.end(),
				     TypeMatch(token.value));

		    if (jj != p.replies.end()) {

			// At this point we have identified a reply

			jj->inReturns = true;
			replies = true;

			if ((token = lex.next()).type == PIPE)
			    continue;
			else if (token.type == SEMICOLON)
			    return;
			else
			    throw parse_error("Expected '|' or ';'",
					      token.line, token.column);
		    } else
			throw parse_error("Expected reply type", token.line,
					  token.column);
		} else
		    throw parse_error("Expected reply type", token.line,
				      token.column);
	    } else if (token.type == NOTHING) {
		if (!replies) {
		    if ((token = lex.next()).type == SEMICOLON) {
			s.use = Struct::asRequest;
			return;
		    }
		    throw parse_error("Expected semicolon", token.line,
				      token.column);
		} else
		    throw parse_error("'nothing' keyword found in list of "
				      "replies", token.line, token.column);
	    } else
		throw parse_error("Expected 'multiple', 'single' or 'nothing'",
				  token.line, token.column);
	}
	throw parse_error("Unexpected end of file", token.line, token.column);
    } else
	throw parse_error("Expected '->'", token.line, token.column);
}

static void setSddVersion(Lexer& lex, unsigned v)
{
    Token const t = lex.next();

    if (t.type == SEMICOLON)
	sddVersion = v;
    else
	throw parse_error("Expected ';'", t.line, t.column);
}

static Protocol parseProtocol(Protocol& p, Lexer& lex)
{
    Token token;

    while ((token = lex.next()).type != ENDOFFILE) {
	switch (token.type) {
	 case SDD2:
	    setSddVersion(lex, 2);
	    break;

	 case SDD3:
	    setSddVersion(lex, 3);
	    break;

	 case REQUEST:
	    {
		Struct s = parseStruct(p, lex, Struct::asRequest);

		s.used = InRequest;
		s.markUsedTypes(p, InRequest);
		p.requests.push_back(s);
	    }
	    break;

	 case REPLY:
	    {
		Struct s = parseStruct(p, lex, Struct::asReply);

		s.used = InReply;
		s.markUsedTypes(p, InReply);
		p.replies.push_back(s);
	    }
	    break;

	 case STRUCT:
	    {
		Struct s = parseStruct(p, lex, Struct::asStruct);

		if (s.fields.empty())
		    throw parse_error("Must provide at least one field in "
				      "a struct", token.line, token.column);

		if (allStructs)
		    s.markUsedTypes(p, InEither);

		p.types.push_back(s);
	    }
	    break;

	 case ENUM:
	    {
		Struct s = parseEnum(lex);

		if (s.fields.empty())
		    throw parse_error("Must provide at least one value in "
				      "an enum", token.line, token.column);
		else
		    s.use = Struct::asEnum;

		p.types.push_back(s);
	    }
	    break;

	 case ENDOFFILE:
	    throw parse_error("Missing closing brace", token.line,
			      token.column);

	 case IDENTIFIER:
	    {
		StructList::iterator jj =
		    std::find_if(p.requests.begin(), p.requests.end(),
				 TypeMatch(token.value));

		if (jj != p.requests.end()) {
		    jj->inReturns = true;
		    parseReturnsStatement(lex, p, *jj);
		} else
		    throw parse_error("Expected 'request', 'reply', 'struct' "
				      "or name of request type", token.line,
				      token.column);
	    }
	    break;

	 default:
	    throw parse_error("Expected 'request', 'reply' or 'struct'",
			      token.line, token.column);
	}
    }


    // Check requests and replies usage in a returns statement if desired

    if (checkReturns) {
	for (StructList::iterator jj = p.requests.begin(); jj != p.requests.end(); jj++) {
	    std::ostringstream os;

	    if (!jj->inReturns) {
		os << "request '" << jj->name << "' not found in a returns statement";
		throw parse_error(os.str().c_str(), token.line, token.column);
	    }
	}

	for (StructList::iterator jj = p.replies.begin(); jj != p.replies.end(); jj++) {
	    std::ostringstream os;

	    if (!jj->inReturns) {
		os << "reply '" << jj->name << "' not found in a returns statement";
		throw parse_error(os.str().c_str(), token.line, token.column);
	    }
	}
    }

    return p;
}

static std::string protocolName(std::string const& file)
{
    static const char allowed[] = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    size_t const idx = file.rfind('/');
    std::string const tmp = file.substr(idx != std::string::npos ? idx + 1 : 0);
    size_t const ii = tmp.find_first_not_of(allowed);

    if (ii != std::string::npos) {
	if (tmp[ii] == '.')
	    return tmp.substr(0, ii);
	else
	    throw std::runtime_error("Invalid file name for protocol");
    }

    return tmp;
}

// Main entry point for the parser. Returns a data structure that
// represents the contents of the file.

Protocol parse(std::string const& name)
{
    Protocol p;
    std::ifstream is(name.c_str());

    p.name = protocolName(name);

    if (is) {
	Lexer lex(is);
	Token token;

	parseProtocol(p, lex);

	if (warnings)
	    for (StructList::iterator ii = p.types.begin(); ii != p.types.end(); ++ii)
		if ((ii->use == Struct::asEnum || ii->use == Struct::asStruct)
		    && !ii->isUsed(InBoth))
		    std::cerr << "warning: '" << ii->name << "' defined but not used\n";

	return p;
    } else
	throw std::logic_error("cannot open source file");
}
