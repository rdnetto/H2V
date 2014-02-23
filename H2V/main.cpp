#include <stdio.h>
#include <QString>
#include <QStringList>
#include <QFile>
#include <QList>
#include <QTextStream>

#include <QChar>

//basic data type for graphs
template <typename T>
class Node{
    public:
        T Value;
        QList< Node<T>* > Edges;
};

//function definitions
Node<QString>* tokenize(QString line);



int Parse(QTextStream *input){
    //Creates a new SyntaxTree object, parsing the input from a QTextStream

    while(! input->atEnd()){
        QString line = input->readLine();

        if(line.contains('=')){
            auto head = tokenize(line.section('=', 0));
            auto tail = tokenize(line.section('=', 1));

            if(line.section('=', 0).contains('('))
                throw "Pattern matching is not supported";

            //convert AST to data flow diagram -- need to make sure all leaves are constants or arguments
            //TODO: figure out how to do this (it's really hard...)
            QString funcName = head->Edges[0]->Value;
            QStringList args;
            for(int i = 1; i < head->Edges.length(); i++)
                args.append(head->Edges[i]->Value);


        } else {
            throw "Invalid line: " + line;
        } //end if
    } //end while
}

Node<QString>* tokenize(QString line){
    //TODO: replace this with bison/flex
    //divides a string into a tree of tokens
    //printf("Calling Tokenize(%s)\n", line.toLatin1().data());

    Node<QString> *res = new Node<QString>();
    int tokenStart = 0;

    for(int i = 0; i < line.length(); i++){
        if(line[i] == ' '){
            if(i == tokenStart){
                tokenStart++;
            }else{
                auto token = new Node<QString>();
                token->Value = line.mid(tokenStart, i - tokenStart);
                res->Edges.append(token);
                //printf("Mid token '%s' is (%i, %i)\n", line.mid(tokenStart, i-tokenStart).toAscii().data(),
                        //tokenStart, i-tokenStart);

                tokenStart = i + 1;
            } //end if
        } //end if

        if(line[i] == '('){
            int tokenEnd = line.lastIndexOf(')');

            if(tokenEnd == -1)
                throw "Mismatched brackets: " + line;

            res->Edges.append(tokenize(line.mid(i + 1, tokenEnd - i - 1)));
            i = tokenEnd;
            tokenStart = i + 1;
        } //end if
    } //end for

    if(res->Edges.length() == 0)
        res->Value = line;
    else if(tokenStart < line.length()){
        auto token = new Node<QString>();
        token->Value = line.mid(tokenStart);
        res->Edges.append(token);
        //printf("Final token '%s' is (%i, %i)\n", line.mid(tokenStart).toAscii().data(), tokenStart, -1);
    } //end if

    return res;
}

void displayToken(Node<QString> *token, int indent=0){
    QString line = QString("%1Token: '%2'\n").arg(QString("    ").repeated(indent), token->Value);
    printf(line.toAscii());

    for(auto node : token->Edges)
        displayToken(node, indent + 1);
}

int main_debugTokenize(){
    QString line = "foo a b c (bar e f) g";
    printf(line.toAscii());
    printf("\n");
    auto res = tokenize(line);

    displayToken(res);
    return 0;
}

int main(int argc, char *argv[]){
    //load file
    if(argc != 2){
        printf("USAGE: H2V FILE\n");
        return 1;
    } //end if

    //read input
    printf("Reading from: %s\n", argv[1]);
    QString srcPath = argv[1];
    QFile srcCode(srcPath);

    if(!srcCode.open(QIODevice::ReadOnly | QIODevice::Text)){
        printf("Unable to open file.");
        return 1;
    } //end if

    QTextStream ts(&srcCode);
    auto ast = Parse(&ts);
    srcCode.close();

    //write results to output file
    QString dstPath = (srcPath.contains('.') ? srcPath.remove(srcPath.lastIndexOf('.')) : srcPath) + ".v";

    return 0;
}
