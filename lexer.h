#ifndef __LEXER_H
#define __LEXER_H

#include <map>

enum TokenType {
  REQUEST, REPLY, OPTIONAL, ENUM, STRUCT, IDENTIFIER, NUMBER,
  OPEN_CURLY, CLOSE_CURLY, OPEN_ARRAY, CLOSE_ARRAY,
  SINGLE, MULTIPLE, NOTHING, SEMICOLON, PIPE, COMMA,
  RETURNS, SDD2, SDD3, ENDOFFILE
};

struct Token {
    TokenType type;
    std::string value;
    size_t line;
    size_t column;

    Token() :
	type(ENDOFFILE), line(0), column(0) {}
    Token(TokenType type, size_t line, size_t column) :
	type(type), line(line), column(column) {}
};

class Lexer {
    typedef std::map<std::string, TokenType> KeywordMap;
    std::istream& is;
    size_t line, column, prevColumn;
    KeywordMap keywords;

    bool savedToken;
    Token saved;
    bool sddFixed;

    int get();
    bool get(char&);
    void putback(char);
    void skip();
    bool identifierChar(int);
    void getIdentifier(Token&);
    void getNumber(Token&);

 public:
    Lexer(std::istream&);
    Token next();
    void pushToken(Token const&);
};

#endif
