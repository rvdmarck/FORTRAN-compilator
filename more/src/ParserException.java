/**
 * Created by acaccia on 02/12/16.
 */
class ParserException extends Exception {
    ParserException(Symbol s){
        super("Unexpected token: " + s.getValue() + " (line " + s.getLine() + ", col " + s.getColumn() + ")");
    }
}
