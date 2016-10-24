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

^[cCdD\*].*\n {}
"!".*\n {}
^{whitespace}*\n$ {}
{end_of_line} {System.out.println(symbolBuilder(LexicalUnit.ENDLINE));}
integer {System.out.println(symbolBuilder(LexicalUnit.INTEGER));}
program {System.out.println(symbolBuilder(LexicalUnit.PROGRAM));}
end {System.out.println(symbolBuilder(LexicalUnit.END));}
if {System.out.println(symbolBuilder(LexicalUnit.IF));}
then {System.out.println(symbolBuilder(LexicalUnit.THEN));}
endif {System.out.println(symbolBuilder(LexicalUnit.ENDIF));}
else {System.out.println(symbolBuilder(LexicalUnit.ELSE));}
do {System.out.println(symbolBuilder(LexicalUnit.DO));}
enddo {System.out.println(symbolBuilder(LexicalUnit.ENDDO));}
read\* {System.out.println(symbolBuilder(LexicalUnit.READ));}
print\* {System.out.println(symbolBuilder(LexicalUnit.PRINT));}
\.not\. {System.out.println(symbolBuilder(LexicalUnit.NOT));}
\.an\. {System.out.println(symbolBuilder(LexicalUnit.AND));}
\.or\. {System.out.println(symbolBuilder(LexicalUnit.OR));}
\.eq\. {System.out.println(symbolBuilder(LexicalUnit.EQUAL_COMPARE));}
\.ge\. {System.out.println(symbolBuilder(LexicalUnit.GREATER_EQUAL));}
\.gt\. {System.out.println(symbolBuilder(LexicalUnit.GREATER));}
\.le\. {System.out.println(symbolBuilder(LexicalUnit.SMALLER_EQUAL));}
\.lt\. {System.out.println(symbolBuilder(LexicalUnit.SMALLER));}
\.ne\. {System.out.println(symbolBuilder(LexicalUnit.DIFFERENT));}
"," {System.out.println(symbolBuilder(LexicalUnit.COMMA));}
"=" {System.out.println(symbolBuilder(LexicalUnit.EQUAL));}
"(" {System.out.println(symbolBuilder(LexicalUnit.LEFT_PARENTHESIS));}
")" {System.out.println(symbolBuilder(LexicalUnit.RIGHT_PARENTHESIS));}
"-" {System.out.println(symbolBuilder(LexicalUnit.MINUS));}
"+" {System.out.println(symbolBuilder(LexicalUnit.PLUS));}
"*" {System.out.println(symbolBuilder(LexicalUnit.TIMES));}
"/" {System.out.println(symbolBuilder(LexicalUnit.DIVIDE));}
{number} {System.out.println(symbolBuilder(LexicalUnit.NUMBER, new Integer(yytext())));}
{identifier} {System.out.println(symbolBuilder(LexicalUnit.VARNAME));}
{whitespace} {}
. {System.out.println("TOKEN NOT RECOGNIZED: " + yytext());}
