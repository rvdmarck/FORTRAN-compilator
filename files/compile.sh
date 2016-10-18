#!/bin/sh
java -cp .:JFLex.jar JFlex.Main LexicalAnalyzer.flex
javac *.java
