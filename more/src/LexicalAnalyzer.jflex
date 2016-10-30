import java.util.Map;
import java.util.TreeMap;

%%

%class LexicalAnalyzer
%line
%column
%unicode
%ignorecase
%standalone

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

private void log(Symbol s){
  System.out.println(s);
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
  program {log(symbolBuilder(LexicalUnit.PROGRAM));}
  {identifier} {log(symbolBuilder(LexicalUnit.VARNAME)); yybegin(PROGRAM);}
  {end_of_line} {}
  . {}
}

<PROGRAM> {
  ^[cCdD\*!] {yybegin(COMMENT);}
  "!" {log(symbolBuilder(LexicalUnit.ENDLINE, " ")); yybegin(COMMENT);}
  ^{whitespace}*{end_of_line} {}
  {end_of_line} {log(symbolBuilder(LexicalUnit.ENDLINE, " "));}
  end {log(symbolBuilder(LexicalUnit.END)); yybegin(YYINITIAL);}
  integer {log(symbolBuilder(LexicalUnit.INTEGER));}
  if {log(symbolBuilder(LexicalUnit.IF));}
  then {log(symbolBuilder(LexicalUnit.THEN));}
  endif {log(symbolBuilder(LexicalUnit.ENDIF));}
  else {log(symbolBuilder(LexicalUnit.ELSE));}
  do {log(symbolBuilder(LexicalUnit.DO));}
  enddo {log(symbolBuilder(LexicalUnit.ENDDO));}
  read\* {log(symbolBuilder(LexicalUnit.READ));}
  print\* {log(symbolBuilder(LexicalUnit.PRINT));}
  \.not\. {log(symbolBuilder(LexicalUnit.NOT));}
  \.an\. {log(symbolBuilder(LexicalUnit.AND));}
  \.or\. {log(symbolBuilder(LexicalUnit.OR));}
  \.eq\. {log(symbolBuilder(LexicalUnit.EQUAL_COMPARE));}
  \.ge\. {log(symbolBuilder(LexicalUnit.GREATER_EQUAL));}
  \.gt\. {log(symbolBuilder(LexicalUnit.GREATER));}
  \.le\. {log(symbolBuilder(LexicalUnit.SMALLER_EQUAL));}
  \.lt\. {log(symbolBuilder(LexicalUnit.SMALLER));}
  \.ne\. {log(symbolBuilder(LexicalUnit.DIFFERENT));}
  "," {log(symbolBuilder(LexicalUnit.COMMA));}
  "=" {log(symbolBuilder(LexicalUnit.EQUAL));}
  "(" {log(symbolBuilder(LexicalUnit.LEFT_PARENTHESIS));}
  ")" {log(symbolBuilder(LexicalUnit.RIGHT_PARENTHESIS));}
  "-" {log(symbolBuilder(LexicalUnit.MINUS));}
  "+" {log(symbolBuilder(LexicalUnit.PLUS));}
  "*" {log(symbolBuilder(LexicalUnit.TIMES));}
  "/" {log(symbolBuilder(LexicalUnit.DIVIDE));}
  {number} {log(symbolBuilder(LexicalUnit.NUMBER, new Integer(yytext())));}
  {identifier} {addIdentifier(); log(symbolBuilder(LexicalUnit.VARNAME));}
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
