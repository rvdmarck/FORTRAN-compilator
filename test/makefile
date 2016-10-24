JFC=jflex
JC=javac
FLEX = LexicalAnalyzer.flex



testmake: $(FLEX)
	$(JFC) $(FLEX) & $(JC) LexicalAnalyzer.java

clean:
	$(RM) *.class
