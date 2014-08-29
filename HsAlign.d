#!/opt/bin/rdmd
//Convenience script for formatting Haskell data structures written to stdin

import std.algorithm;
import std.conv : to;
import std.range;
import std.stdio;
import std.string;
import std.typecons;
import std.uni : isWhite;


void main(){
    foreach(line; stdin.byLine())
        writeln(line.idup.process().detab(4).chomp());
}

string process(string input){
    int indentation = 0;
    string result = "";

    void linebreak(){
        if(result[$ - 1] != '\t')
            result ~= "\n" ~ '\t'.repeat().take(indentation).to!string;
    };

    foreach(c; input){
        switch(c){
            case ',':
                result ~= c;
                linebreak();
                break;

            case '(', '[', '{':
                result ~= c;
                indentation++;
                linebreak();
                break;

            case ')', ']', '}':
                indentation--;
                linebreak();
                result ~= c;
                break;

            default:
                if(result == "" || !result[$ - 1].isWhite || !c.isWhite)
                    result ~= c;
        } //end switch
    } //end foreach

    assert(indentation == 0);
    return result;
}
