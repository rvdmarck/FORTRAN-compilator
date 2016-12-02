/**
 * Created by acaccia on 02/12/16.
 */
public class ParserException extends Exception {
    public ParserException(Symbol s, Throwable cause){
        super("Unexpected token: " + s.getType() + " (line " + s.getLine() + ", col " + s.getColumn() + ")", cause);
    }

    public ParserException(Symbol s){
        super("Unexpected token: " + s.getType() + " (line " + s.getLine() + ", col " + s.getColumn() + ")");
    }
}
