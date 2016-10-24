import java.util.ArrayList;

%%

%class LexicalAnalyzer
%line
%column
%unicode
%ignorecase
%standalone


%{
private ArrayList<String> identifiers = new ArrayList<String>();
private boolean inComment = false;
private boolean inEndStatement = false;

/**
* Add an identifier and it's declaration location to the list of identifiers
*/
private void addIdentifier(){
  identifiers.add(yytext() + " " + (yyline+1));
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

/**
* Informs a comment line is encountered
* @return void
*/
private void comment(){
  inComment = true;
}

/**
* Handles what to do in case of a
* end_of_line match.
* @return void
*/
private void endOfLine(){
  if(!inComment && !inEndStatement){
    System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.ENDLINE, " "));
  }
  inComment = false;
}

/**
* Informs a END statement is encountered
* @return void
*/
private void endStatement(){
  System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.END));
  inEndStatement = true;
}

private void log(Symbol s){
  System.out.println("line: "+ (yyline+1) + " " + s);
}

%}

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

^[cCdD\*].*$ {comment();}
"!".*$ {comment();}
^{whitespace}*\n$ {}
^{whitespace}*{end_of_line} {}
{end_of_line} {endOfLine();}
integer {log(symbolBuilder(LexicalUnit.INTEGER));}
program {log(symbolBuilder(LexicalUnit.PROGRAM));}
end {endStatement();}
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
{identifier} {log(symbolBuilder(LexicalUnit.VARNAME));}
{whitespace} {}
. {System.out.println("TOKEN NOT RECOGNIZED: " + yytext());}
