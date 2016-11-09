import java.util.Map;
import java.util.TreeMap;

%%

%class LexicalAnalyzer
%line
%column
%unicode
%ignorecase
%type Symbol

%s PROGRAM, COMMENT, PRECOMMENT

%{
private TreeMap<String, Integer> identifiers = new TreeMap<String, Integer>();

/**
* Add an identifier and it's declaration location to the list of identifiers
*/
private void addIdentifier(){
  if (!identifiers.containsKey(yytext())) {
    identifiers.put(yytext(), yyline+1);
  }
}

/**
* Helper function too create new symbol with position
* @param type of LexicalUnit inferred
* @return created symbol
*/
private Symbol symbolBuilder(LexicalUnit unit){
    return new Symbol(unit, yyline, yycolumn, yytext());
}

/**
* Helper function too create new symbol with position
* @param type of LexicalUnit inferred
* @param value associated
* @return created symbol
*/
private Symbol symbolBuilder(LexicalUnit unit, Object value){
    return new Symbol(unit, yyline, yycolumn, value);
}

%}

%eof{
  System.out.println("Identifiers");
  for (Map.Entry<String, Integer> i: identifiers.entrySet()){
    System.out.println(i.getKey() + " " + i.getValue());
  }
%eof}

/**
* Patterns
*/
letter = [a-zA-Z]
digit = [0-9]
identifier = {letter}({letter}|{digit})*
number = {digit}{digit}*
end_of_line = \r?\n
whitespace = [ \t]
%%

<YYINITIAL> {
  ^[cCdD\*!] {yybegin(PRECOMMENT);}
  program {return symbolBuilder(LexicalUnit.PROGRAM);}
  {identifier} {return symbolBuilder(LexicalUnit.VARNAME); yybegin(PROGRAM);}
  {end_of_line} {}
  . {}
}

<PROGRAM> {
  ^[cCdD\*!] {yybegin(COMMENT);}
  "!" {return symbolBuilder(LexicalUnit.ENDLINE, " "); yybegin(COMMENT);}
  ^{whitespace}*{end_of_line} {}
  {end_of_line} {return symbolBuilder(LexicalUnit.ENDLINE, " ");}
  end {return symbolBuilder(LexicalUnit.END); yybegin(YYINITIAL);}
  integer {return symbolBuilder(LexicalUnit.INTEGER);}
  if {return symbolBuilder(LexicalUnit.IF);}
  then {return symbolBuilder(LexicalUnit.THEN);}
  endif {return symbolBuilder(LexicalUnit.ENDIF);}
  else {return symbolBuilder(LexicalUnit.ELSE);}
  do {return symbolBuilder(LexicalUnit.DO);}
  enddo {return symbolBuilder(LexicalUnit.ENDDO);}
  read\* {return symbolBuilder(LexicalUnit.READ);}
  print\* {return symbolBuilder(LexicalUnit.PRINT);}
  \.not\. {return symbolBuilder(LexicalUnit.NOT);}
  \.an\. {return symbolBuilder(LexicalUnit.AND);}
  \.or\. {return symbolBuilder(LexicalUnit.OR);}
  \.eq\. {return symbolBuilder(LexicalUnit.EQUAL_COMPARE);}
  \.ge\. {return symbolBuilder(LexicalUnit.GREATER_EQUAL);}
  \.gt\. {return symbolBuilder(LexicalUnit.GREATER);}
  \.le\. {return symbolBuilder(LexicalUnit.SMALLER_EQUAL);}
  \.lt\. {return symbolBuilder(LexicalUnit.SMALLER);}
  \.ne\. {return symbolBuilder(LexicalUnit.DIFFERENT);}
  "," {return symbolBuilder(LexicalUnit.COMMA);}
  "=" {return symbolBuilder(LexicalUnit.EQUAL);}
  "(" {return symbolBuilder(LexicalUnit.LEFT_PARENTHESIS);}
  ")" {return symbolBuilder(LexicalUnit.RIGHT_PARENTHESIS);}
  "-" {return symbolBuilder(LexicalUnit.MINUS);}
  "+" {return symbolBuilder(LexicalUnit.PLUS);}
  "*" {return symbolBuilder(LexicalUnit.TIMES);}
  "/" {return symbolBuilder(LexicalUnit.DIVIDE);}
  {number} {return symbolBuilder(LexicalUnit.NUMBER, new Integer(yytext()));}
  {identifier} {addIdentifier(); return symbolBuilder(LexicalUnit.VARNAME);}
  {whitespace} {}
  . {System.out.println("TOKEN NOT RECOGNIZED: " + yytext());}
}

<PRECOMMENT> {
  {end_of_line} {yybegin(YYINITIAL);}
  . {}
}

<COMMENT> {
  {end_of_line} {yybegin(PROGRAM);}
  . {}
}
