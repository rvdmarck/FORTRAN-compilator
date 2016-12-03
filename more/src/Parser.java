import java.io.IOException;

class Parser {
    private LexicalAnalyzer la;
    private Symbol peeked = null;

    Parser(LexicalAnalyzer la) {
        this.la = la;
    }

    void run() throws ParserException {
        peek();
        program();
    }

    private void peek() {
        try {
            peeked = la.yylex();
            System.out.println(peeked);
        } catch (IOException e) {
        }
    }

    private boolean match(LexicalUnit unit) {
        LexicalUnit lu = peeked.getType();
        if (unit == lu) {
            peek();
            return true;
        }
        return false;
    }

    private boolean matchAny(LexicalUnit... units) {
        LexicalUnit lu = peeked.getType();
        for (LexicalUnit u : units) {
            if (u == lu)
                return true;
        }
        return false;
    }

    private boolean matchOrThrow(LexicalUnit lu) throws ParserException {
        if (!match(lu))
            throw new ParserException(peeked);
        else
            return true;
    }

    private void printRule(int n, String left, String right) {
        String nStr = "[" + n + "]";
        left = "<" + left + ">";
        System.out.println(String.format("%-5s %-15s \u2192 %s", nStr, left, right));
    }

    private void program() throws ParserException {
        printRule(1, "Program", "PROGRAM [ProgName] [EndLine] <Vars> <Code> END");
        matchOrThrow(LexicalUnit.PROGRAM);
        matchOrThrow(LexicalUnit.VARNAME);
        matchOrThrow(LexicalUnit.ENDLINE);
        vars();
        code();
        matchOrThrow(LexicalUnit.END);
    }

    private void vars() throws ParserException {
        if (match(LexicalUnit.INTEGER)) {
            printRule(2, "Vars", "INTEGER <VarList> [EndLine]");
            varlist();
            matchOrThrow(LexicalUnit.ENDLINE);
        } else if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)) {
            printRule(3, "Vars", "\u0395");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void varlist() throws ParserException {
        printRule(4, "VarList", "[VarName] <FollowVarList>");
        matchOrThrow(LexicalUnit.VARNAME);
        followVarlist();
    }

    private void followVarlist() throws ParserException {
        if (match(LexicalUnit.COMMA)) {
            printRule(5, "FollowVarList", ", <VarList>");
            varlist();
        } else if (matchAny(LexicalUnit.ENDLINE)) {
            printRule(5, "FollowVarList", "\u0395");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void code() throws ParserException {
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)) {
            printRule(7, "Code", "<Instruction> [EndLine] <Code>");
            instruction();
            matchOrThrow(LexicalUnit.ENDLINE);
            code();
        } else if (matchAny(LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)) {
            printRule(8, "Code", "\u0395");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void instruction() throws ParserException {
        switch (peeked.getType()) {
        case VARNAME:
            printRule(9, "Instruction", "<Assign>");
            ;
            assign();
            break;
        case IF:
            printRule(10, "Instruction", "<If>");
            ifrule();
            break;
        case DO:
            printRule(11, "Instruction", "<Do>");
            doRule();
            break;
        case PRINT:
            printRule(12, "Instruction", "<Print>");
            print();
            break;
        case READ:
            printRule(13, "Instruction", "<Read>");
            read();
            break;
        default:
            throw new ParserException(peeked);
        }
    }

    private void assign() throws ParserException {
        printRule(14, "Assign", "[VarName] = <ExprArithA>");
        matchOrThrow(LexicalUnit.VARNAME);
        matchOrThrow(LexicalUnit.EQUAL);
        exprArithA();
    }

    private void exprArithA() throws ParserException {
        printRule(15, "ExprArithA", "<ExprArithB> <V>");
        exprArithB();
        v();
    }

    private void v() throws ParserException {
        if (matchAny(LexicalUnit.PLUS, LexicalUnit.MINUS)) {
            printRule(16, "V", "<AddOp> <ExprArithB> <V>");
            addOp();
            exprArithB();
            v();
        } else if (matchAny(LexicalUnit.COMMA, LexicalUnit.EQUAL, LexicalUnit.GREATER_EQUAL, LexicalUnit.GREATER, LexicalUnit.SMALLER_EQUAL,
                LexicalUnit.DIFFERENT, LexicalUnit.RIGHT_PARENTHESIS, LexicalUnit.TIMES, LexicalUnit.DIVIDE, LexicalUnit.ENDLINE,
                LexicalUnit.AND, LexicalUnit.OR, LexicalUnit.PLUS, LexicalUnit.MINUS)) {
            printRule(17, "V", "\u0395");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void exprArithB() throws ParserException {
        printRule(18, "ExprArithB", "<ExprArithC> <X>");
        exprArithC();
        x();
    }

    private void x() throws ParserException {
        if (matchAny(LexicalUnit.TIMES, LexicalUnit.DIVIDE)) {
            printRule(19, "X", "<MulOp> <ExprArithC> <X>");
            mulOp();
            exprArithC();
            x();
        } else if (matchAny(LexicalUnit.COMMA, LexicalUnit.EQUAL, LexicalUnit.GREATER_EQUAL, LexicalUnit.GREATER, LexicalUnit.SMALLER_EQUAL,
                LexicalUnit.DIFFERENT, LexicalUnit.RIGHT_PARENTHESIS, LexicalUnit.TIMES, LexicalUnit.DIVIDE, LexicalUnit.ENDLINE,
                LexicalUnit.AND, LexicalUnit.OR, LexicalUnit.PLUS, LexicalUnit.MINUS)) {
            printRule(20, "X", "\u0395");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void exprArithC() throws ParserException {
        if (match(LexicalUnit.VARNAME)) {
            printRule(21, "ExprArithC", "[VarName]");
        } else if (match(LexicalUnit.NUMBER)) {
            printRule(22, "ExprArithC", "[Number]");
        } else if (match(LexicalUnit.LEFT_PARENTHESIS)) {
            printRule(23, "ExprArithC", "( <ExprArithA> )");
            exprArithA();
            matchOrThrow(LexicalUnit.RIGHT_PARENTHESIS);
        } else if (match(LexicalUnit.MINUS)) {
            printRule(24, "ExprArithC", "- <ExprArithA>");
            exprArithA();
        } else {
            throw new ParserException(peeked);
        }
    }

    private void addOp() throws ParserException {
        if (match(LexicalUnit.PLUS)) {
            printRule(25, "AddOp", "+");
        } else if (match(LexicalUnit.MINUS)) {
            printRule(26, "AddOp", "-");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void mulOp() throws ParserException {
        if (match(LexicalUnit.TIMES)) {
            printRule(27, "MulOp", "*");
        } else if (match(LexicalUnit.DIVIDE)) {
            printRule(28, "MulOp", "/");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void ifrule() throws ParserException {
        printRule(29, "If", "IF ( <CondA> ) THEN [EndLine] <Code> <IfSeq>");
        matchOrThrow(LexicalUnit.IF);
        matchOrThrow(LexicalUnit.LEFT_PARENTHESIS);
        condA();
        matchOrThrow(LexicalUnit.RIGHT_PARENTHESIS);
        matchOrThrow(LexicalUnit.THEN);
        matchOrThrow(LexicalUnit.ENDLINE);
        code();
        ifSeq();
    }

    private void ifSeq() throws ParserException {
        if (match(LexicalUnit.ENDIF)) {
            printRule(30, "Else", "ENDIF");
        } else if (match(LexicalUnit.ELSE)) {
            printRule(31, "Else", "ELSE [EndLine] <Code> ENDIF");
            matchOrThrow(LexicalUnit.ENDLINE);
            code();
            matchOrThrow(LexicalUnit.ENDIF);
        } else {
            throw new ParserException(peeked);
        }
    }

    private void condA() throws ParserException {
        printRule(32, "CondA", "<CondB> <B>");
        condB();
        b();
    }

    private void b() throws ParserException {
        if (match(LexicalUnit.OR)) {
            printRule(33, "B", ".OR. <CondB> <B>");
            condB();
            b();
        } else if (matchAny(LexicalUnit.RIGHT_PARENTHESIS)) {
            printRule(34, "B", "\u0395");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void condB() throws ParserException {
        printRule(35, "CondB", "<CondC> <D>");
        condC();
        d();
    }

    private void d() throws ParserException {
        if (match(LexicalUnit.AND)) {
            printRule(36, "D", ".AND. <CondC>");
            condC();
        } else if (matchAny(LexicalUnit.OR, LexicalUnit.RIGHT_PARENTHESIS)) {
            printRule(37, "D", "\u0395");
        } else {
            throw new ParserException(peeked);
        }
    }

    private void condC() throws ParserException {
        if (match(LexicalUnit.NOT)) {
            printRule(38, "CondC", ".NOT. <SimpleCond>");
        } else {
            printRule(39, "CondC", "<SimpleCond>");
        }
        simpleCond();
    }

    private void simpleCond() throws ParserException {
        printRule(40, "SimpleCond", "<ExprArithA> <Comp> <ExprArithA>");
        exprArithA();
        comp();
        exprArithA();
    }

    private void comp() throws ParserException {
        switch (peeked.getType()) {
        case EQUAL_COMPARE:
            printRule(41, "Comp", ".EQ.");
            break;
        case GREATER_EQUAL:
            printRule(42, "Comp", ".GE.");
            break;
        case GREATER:
            printRule(43, "Comp", ".GT.");
            break;
        case SMALLER_EQUAL:
            printRule(44, "Comp", ".LE.");
            break;
        case SMALLER:
            printRule(45, "Comp", ".LT.");
            break;
        case DIFFERENT:
            printRule(46, "Comp", ".NE.");
            break;
        default:
            throw new ParserException(peeked);
        }
        peek();
    }

    private void doRule() throws ParserException {
        printRule(47, "Do", "DO [VarName] = [Number] , [Number] [EndLine] <Code> ENDDO");
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

    private void print() throws ParserException {
        printRule(48, "Print", "PRINT* , <ExpList>");
        matchOrThrow(LexicalUnit.PRINT);
        matchOrThrow(LexicalUnit.COMMA);
        expList();
    }

    private void read() throws ParserException {
        printRule(49, "Read", "READ* , <VarList>");
        matchOrThrow(LexicalUnit.READ);
        matchOrThrow(LexicalUnit.COMMA);
        varlist();
    }

    private void expList() throws ParserException {
        printRule(50, "ExpList", "<ExprArithA> <FollowExpList>");
        exprArithA();
        followExplist();
    }

    private void followExplist() throws ParserException {
        if (match(LexicalUnit.COMMA)) {
            printRule(51, "FollowExpList", ", <ExpList>");
            expList();
        } else if (matchAny(LexicalUnit.ENDLINE)) {
            printRule(52, "FollowExpList", "\u0395");
        } else {
            throw new ParserException(peeked);
        }
    }
}