// Fermilab Software Legal Information (BSD License)
// Copyright (c) 2008-2015, 2019, Fermi Research Alliance, LLC
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

#include <cstdio>
#include <istream>
#include "lexer.h"
#include "pc.h"

Lexer::Lexer(std::istream& is) :
    is(is), line(1), column(0), prevColumn(0), savedToken(false),
    sddFixed(false)
{
    keywords.insert(std::make_pair("request", REQUEST));
    keywords.insert(std::make_pair("reply", REPLY));
    keywords.insert(std::make_pair("optional", OPTIONAL));
    keywords.insert(std::make_pair("struct", STRUCT));
    keywords.insert(std::make_pair("enum", ENUM));
    keywords.insert(std::make_pair("multiple", MULTIPLE));
    keywords.insert(std::make_pair("single", SINGLE));
    keywords.insert(std::make_pair("nothing", NOTHING));
    keywords.insert(std::make_pair("sdd2", SDD2));
    keywords.insert(std::make_pair("sdd3", SDD3));
}

int Lexer::get()
{
    char c = is.get();

    if (c == '\n') {
        if (is.peek() != EOF) {
            line++;
            prevColumn = column;
            column = 0;
        }
    } else if (c == '\t')
        column += 8;
    else
        column++;

    return c;
}

bool Lexer::get(char& c)
{
    if (is.peek() == EOF)
	return false;
    else
	c = get();

    return true;
}

void Lexer::putback(char c)
{
    if (c == '\n') {
        if (is.peek() != EOF) {
            line--;
            column = prevColumn;
            prevColumn = 0;
        }
    } if (c == '\t')
        column -= 8;
    else
        column--;

    is.putback(c);
}

void Lexer::skip()
{
    char c;
    bool comment = false;

    while (get(c)) {
        if (c == '\n')
            comment = false;
        else if (c == '#')
            comment = true;
	else if (!comment && c == '/') {
	    if (!get(c) || c != '/')
		throw parse_error("expected '/'", line, column);
	    else
		comment = true;
	}

        if (!comment && !isspace(c)) {
            putback(c);
            return;
        }
    }
}

bool Lexer::identifierChar(int c)
{
    return (c == '_' || isalpha(c));
}

void Lexer::getIdentifier(Token& t)
{
    char c;

    while (get(c)) {
	if (c == '_' || isalnum(c))
	    t.value.append(1, c);
	else {
	    putback(c);
	    KeywordMap::iterator iter = keywords.find(t.value);
	    if (iter != keywords.end()) {
		if (sddFixed && (iter->second == SDD2 || iter->second == SDD3))
		    throw parse_error("SDD version cannot be set here", line,
				      column);
		t.type = iter->second;
		sddFixed = true;
	    }
	    break;
	}
    }
}

void Lexer::getNumber(Token& t)
{
    char c;

    while (get(c)) {
	if (isdigit(c))
	    t.value.append(1, c);
	else {
	    putback(c);
	    break;
	}
    }
}

void Lexer::pushToken(Token const& tok)
{
    if (!savedToken) {
	savedToken = true;
	saved = tok;
    } else
	throw std::logic_error("tried to push two tokens back");
}

Token Lexer::next()
{
    if (savedToken) {
	savedToken = false;
	return saved;
    }

    skip();

    int c = get();

    switch (c) {
     case '|':
	return Token(PIPE, line, column);

     case '{':
	return Token(OPEN_CURLY, line, column);

     case '}':
	return Token(CLOSE_CURLY, line, column);

     case ',':
	return Token(COMMA, line, column);

     case '-':
	if (get() == '>')
	    return Token(RETURNS, line, column);
	throw parse_error("expected >", line, column);

     case '[':
	return Token(OPEN_ARRAY, line, column);

     case ']':
	return Token(CLOSE_ARRAY, line, column);

     case ';':
	return Token(SEMICOLON, line, column);

     case EOF:
	return Token(ENDOFFILE, line, column);
    }

    if (c == '_' || isalpha(c)) {
	Token t(IDENTIFIER, line, column);
	t.value.append(1, static_cast<char>(c));
	getIdentifier(t);
	return t;
    }

    if (isdigit(c)) {
	Token t(NUMBER, line, column);
	t.value.append(1, static_cast<char>(c));
	getNumber(t);
	return t;
    }

    throw parse_error("Unexpected input in file", line, column);
}
