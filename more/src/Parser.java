import java.io.IOException;
import java.text.ParseException;

/**
 * Created by acaccia on 02/12/16.
 */
public class Parser {
    private LexicalAnalyzer la;
    private Symbol peeked = null;

    public Parser(LexicalAnalyzer la){
        this.la = la;
    }

    public void run() throws ParserException {
        peek();
        program();
    }

    private void peek() {
        try {
            peeked = la.yylex();
        } catch (IOException e) {
        }
    }

    private boolean match(LexicalUnit unit){
        LexicalUnit lu = peeked.getType();
        if (unit == lu){
            peek();
            return true;
        }
        return false;
    }

    private boolean matchAny(LexicalUnit... units){
        LexicalUnit lu = peeked.getType();
        for (LexicalUnit u : units){
            if (u == lu)
                return true;
        }
        return false;
    }

    private boolean matchOrThrow(LexicalUnit lu) throws ParserException{
        if (!match(lu))
            throw new ParserException(peeked);
        else
            return true;
    }

    private void program() throws ParserException{
        System.out.println("program");
        matchOrThrow(LexicalUnit.PROGRAM);
        matchOrThrow(LexicalUnit.VARNAME);
        matchOrThrow(LexicalUnit.ENDLINE);
        vars();
        code();
        matchOrThrow(LexicalUnit.END);
    }

    private void vars() throws ParserException{
        if (match(LexicalUnit.INTEGER)){
            System.out.println("vars1");
            varlist();
            matchOrThrow(LexicalUnit.ENDLINE);
        } else if(matchAny(LexicalUnit.ENDLINE, LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)){
            System.out.println("vars2");
        }else{
            throw new ParserException(peeked);
        }
    }

    private void varlist() throws ParserException{
        System.out.println("varlist");
        matchOrThrow(LexicalUnit.VARNAME);
        followVarlist();
    }

    private void followVarlist() throws ParserException{
        if (match(LexicalUnit.COMMA)){
            System.out.println("followvarlist1");
            varlist();
        } else if(matchAny(LexicalUnit.ENDLINE)){
            System.out.println("followvarlist2");
        } else{
            throw new ParserException(peeked);
        }
    }

    private void code() throws ParserException{
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)){
            System.out.println("code1");
            instruction();
            matchOrThrow(LexicalUnit.ENDLINE);
            code();
        } else if(matchAny(LexicalUnit.ENDDO, LexicalUnit.NOT, LexicalUnit.VARNAME, LexicalUnit.NUMBER, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END, LexicalUnit.ENDIF)){
            System.out.println("code2");
        } else{
            throw new ParserException(peeked);
        }
    }

    private void instruction() throws ParserException{
        switch (peeked.getType()){
            case VARNAME:
                System.out.println("instruction1");
                assign();
                break;
            case IF:
                System.out.println("instruction2");
                ifrule();
                break;
            case DO:
                System.out.println("instruction3");
                doRule();
                break;
            case PRINT:
                System.out.println("instruction4");
                print();
                break;
            case READ:
                System.out.println("instruction5");
                read();
                break;
            default:
                throw new ParserException(peeked);
        }
    }

    private void assign() throws ParserException{
        System.out.println("assign");
        matchOrThrow(LexicalUnit.VARNAME);
        matchOrThrow(LexicalUnit.EQUAL);
        exprArithA();
    }

    private void exprArithA() throws ParserException{
        System.out.println("exprArithA");
        u();
        v();
    }

    private void u() throws ParserException{
        System.out.println("u");
        exprArithB();
    }

    private void v() throws ParserException{
        if (matchAny(LexicalUnit.PLUS, LexicalUnit.MINUS)) {
            System.out.println("v1");
            addOp();
            exprArithB();
            v();
        } else if(matchAny(LexicalUnit.COMMA, LexicalUnit.EQUAL, LexicalUnit.GREATER_EQUAL, LexicalUnit.GREATER, LexicalUnit.SMALLER_EQUAL, LexicalUnit.DIFFERENT, LexicalUnit.RIGHT_PARENTHESIS, LexicalUnit.TIMES, LexicalUnit.DIVIDE, LexicalUnit.ENDLINE, LexicalUnit.AND, LexicalUnit.OR, LexicalUnit.PLUS, LexicalUnit.MINUS)){
            System.out.println("v2");
        } else{
            throw new ParserException(peeked);
        }
    }

    private void exprArithB() throws ParserException{
        System.out.println("exprArithB");
        w();
        x();
    }

    private void w() throws ParserException{
        System.out.println("w");
        exprArithC();
    }

    private void x() throws ParserException{
        if (matchAny(LexicalUnit.TIMES, LexicalUnit.DIVIDE)){
            System.out.println("x1");
            mulOp();
            exprArithC();
            x();
        } else if(matchAny(LexicalUnit.COMMA, LexicalUnit.EQUAL, LexicalUnit.GREATER_EQUAL, LexicalUnit.GREATER, LexicalUnit.SMALLER_EQUAL, LexicalUnit.DIFFERENT, LexicalUnit.RIGHT_PARENTHESIS, LexicalUnit.TIMES, LexicalUnit.DIVIDE, LexicalUnit.ENDLINE, LexicalUnit.AND, LexicalUnit.OR, LexicalUnit.PLUS, LexicalUnit.MINUS)){
            System.out.println("x2");
        } else{
            throw new ParserException(peeked);
        }
    }

    private void exprArithC() throws ParserException{
        if (match(LexicalUnit.VARNAME)){
            System.out.println("exprAritC1");
        } else if (match(LexicalUnit.NUMBER)){
            System.out.println("exprAritC2");
        } else if (match(LexicalUnit.LEFT_PARENTHESIS)){
            System.out.println("exprArithC3");
            exprArithA();
            matchOrThrow(LexicalUnit.RIGHT_PARENTHESIS);
        } else if (match(LexicalUnit.MINUS)){
            System.out.println("exprArithC4");
            exprArithA();
        } else {
            throw new ParserException(peeked);
        }
    }

    private void addOp() throws ParserException{
        if (match(LexicalUnit.PLUS)){
            System.out.println("addop1");
        } else if (match(LexicalUnit.MINUS)){
            System.out.println("addop2");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void mulOp() throws ParserException{
        if (match(LexicalUnit.TIMES)){
            System.out.println("mulop1");
        } else if (match(LexicalUnit.DIVIDE)){
            System.out.println("mulop2");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void ifrule() throws ParserException{
        System.out.println("if");
        matchOrThrow(LexicalUnit.IF);
        condA();
        matchOrThrow(LexicalUnit.THEN);
        matchOrThrow(LexicalUnit.ENDLINE);
        code();
        elserule();
        matchOrThrow(LexicalUnit.ENDIF);
    }

    private void elserule() throws ParserException{
        if (match(LexicalUnit.ELSE)){
            System.out.println("elserule1");
            matchOrThrow(LexicalUnit.ENDLINE);
            code();
        } else if(matchAny(LexicalUnit.ENDIF)){
            System.out.println("elserule2");
        } else{
            throw new ParserException(peeked);
        }
    }

    private void condA() throws ParserException{
        System.out.println("condA");
        a();
        b();
    }

    private void a() throws ParserException{
        System.out.println("a");
        condB();
    }

    private void b() throws ParserException{
        if (match(LexicalUnit.OR)){
            System.out.println("b1");
            condB();
            b();
        } else if(matchAny(LexicalUnit.RIGHT_PARENTHESIS)){
            System.out.println("b2");
        } else{
            throw new ParserException(peeked);
        }
    }

    private void condB() throws ParserException{
        System.out.println("condB");
        c();
        d();
    }

    private void c() throws ParserException{
        System.out.println("c");
        condC();
    }

    private void d() throws ParserException{
        if (match(LexicalUnit.AND)){
            System.out.println("d1");
            condC();
        } else if(matchAny(LexicalUnit.OR, LexicalUnit.RIGHT_PARENTHESIS)){
            System.out.println("d2");
        } else{
            throw new ParserException(peeked);
        }
    }

    private void condC() throws ParserException{
        System.out.println(match(LexicalUnit.NOT) ? "condC1" : "condC2");
        simpleCond();
    }

    private void simpleCond() throws ParserException{
        System.out.println("simpleCond");
        exprArithA();
        comp();
        exprArithA();
    }

    private void comp() throws ParserException{
        switch (peeked.getType()){
            case EQUAL_COMPARE:
                System.out.println("comp1");
                break;
            case GREATER_EQUAL:
                System.out.println("comp2");
                break;
            case GREATER:
                System.out.println("comp3");
                break;
            case SMALLER_EQUAL:
                System.out.println("comp4");
                break;
            case SMALLER:
                System.out.println("comp5");
                break;
            case DIFFERENT:
                System.out.println("comp6");
                break;
            default:
                throw new ParserException(peeked);
        }
    }

    private void doRule() throws ParserException{
        System.out.println("dorule");
        matchOrThrow(LexicalUnit.DO);
        matchOrThrow(LexicalUnit.VARNAME);
        matchOrThrow(LexicalUnit.EQUAL);
        matchOrThrow(LexicalUnit.NUMBER);
        matchOrThrow(LexicalUnit.COMMA);
        matchOrThrow(LexicalUnit.NUMBER);
        matchOrThrow(LexicalUnit.ENDLINE);
        code();
        matchOrThrow(LexicalUnit.ENDDO);
    }

    private void print() throws ParserException{
        System.out.println("print");
        matchOrThrow(LexicalUnit.PRINT);
        matchOrThrow(LexicalUnit.COMMA);
        expList();
    }

    private void read() throws ParserException{
        System.out.println("read");
        matchOrThrow(LexicalUnit.READ);
        matchOrThrow(LexicalUnit.COMMA);
        varlist();
    }

    private void expList() throws ParserException{
        System.out.println("explist");
        exprArithA();
        followExplist();
    }

    private void followExplist() throws ParserException{
        if (match(LexicalUnit.COMMA)){
            System.out.println("followExplist1");
            expList();
        } else if(matchAny(LexicalUnit.ENDLINE)){
            System.out.println("followExpList2");
        } else{
            throw new ParserException(peeked);
        }
    }
}
