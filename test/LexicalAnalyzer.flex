%%// Options of the scanner

%class LexicalAnalyzer	//Name
%unicode			//Use unicode
%standalone		//Tell that Jflex don't use a parser

%{
  private int countIF = 0;
  private int countENDIF = 0;
  private int countCOMMENT = 0;
  private boolean isComment = false;
  private boolean previousCharOK = false;

  private void comment(){
    countCOMMENT += 1;
    isComment = true;
  }

  private void otherThanComment(){
    previousCharOK = false;
    if(!isComment){
      System.out.println("ok: "+yytext());
    }
  }

  private void endOfLine(){
    isComment = false;
    previousCharOK = true;
  }

  private void spaces(){
    previousCharOK = true;
  }

  private void ifCase(){
    if(previousCharOK){
      countIF += 1;
    }else{
      System.out.println("Rejected by IFCASE: "+yytext());
    }
  }

  private void endifCase(){
    if(previousCharOK){
      countENDIF += 1;
    }else{
      System.out.println("Rejected by ENDIFCASE: "+yytext());
    }
  }

%}

%eof{
  System.out.println("nr of if's: "+countIF);
  System.out.println("nr of endif's: "+countENDIF);
  System.out.println("nr of Comment's: "+countCOMMENT);

%eof}

//Extended Regular Expressions
EndOfLine		= "\r"?"\n"
Space = "\t" | " "

If = IF
Endif = ENDIF
Comment = c|C|\*|d|D|\!
%% //Identification of tokens

{If} {ifCase();}
{Endif} {endifCase();}
^{Comment}  {comment();}
{EndOfLine} {endOfLine();}
{Space}+ {spaces();}
. {otherThanComment();}
