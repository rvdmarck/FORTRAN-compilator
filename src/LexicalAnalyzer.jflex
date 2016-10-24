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
*Handling of comments block
*/
private void comment(){
  inComment = true;
}

private void endOfLine(){
  if(!inComment){
    System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.ENDLINE, " "));
  }
  inComment = false;
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
integer {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.INTEGER));}
program {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.PROGRAM));}
end {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.END));System.exit(0);}
if {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.IF));}
then {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.THEN));}
endif {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.ENDIF));}
else {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.ELSE));}
do {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.DO));}
enddo {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.ENDDO));}
read\* {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.READ));}
print\* {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.PRINT));}
\.not\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.NOT));}
\.an\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.AND));}
\.or\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.OR));}
\.eq\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.EQUAL_COMPARE));}
\.ge\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.GREATER_EQUAL));}
\.gt\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.GREATER));}
\.le\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.SMALLER_EQUAL));}
\.lt\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.SMALLER));}
\.ne\. {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.DIFFERENT));}
"," {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.COMMA));}
"=" {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.EQUAL));}
"(" {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.LEFT_PARENTHESIS));}
")" {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.RIGHT_PARENTHESIS));}
"-" {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.MINUS));}
"+" {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.PLUS));}
"*" {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.TIMES));}
"/" {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.DIVIDE));}
{number} {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.NUMBER, new Integer(yytext())));}
{identifier} {System.out.println("line: " + (yyline+1) + " " + symbolBuilder(LexicalUnit.VARNAME));}
{whitespace} {}
. {System.out.println("TOKEN NOT RECOGNIZED: " + yytext());}
