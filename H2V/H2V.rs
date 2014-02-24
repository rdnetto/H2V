use std::io::fs::File;
use std::path::Path;
use std::io::buffered::BufferedReader;


enum AbstractSyntaxTree{
    Leaf(~str),
    AST(~[AbstractSyntaxTree]),
}

fn Parse(reader: &mut BufferedReader<File>) -> ~[(AbstractSyntaxTree, AbstractSyntaxTree)] {
    //! Creates a new AST

    //vector of (header, body) tuples. May need to add types in the future
    let mut functions = ~[];

    for line in reader.lines(){
        if(line.contains_char('=')){
            let words: ~[&str] = line.splitn('=', 1).collect();
            assert_eq!(words.len(), 2);

            let funcHeader = tokenize(words[0]);        //this provides a list of arguments with pattern matching
            let funcBody = tokenize(words[1]);          //this contains the structure of the function definition
            functions.push((funcHeader, funcBody));

        }else{
            fail!("Invalid line: " + line);
        } //end if
    } //end for

    //TODO: convert AST into DFD. All leaves of DFD should be constants or arguments

    return functions;
}

fn tokenize(line: &str) -> AbstractSyntaxTree{
    //! Converts expressions into a syntax tree.
    //! TODO: replace this with bison/flex, or something equally powerful

    let mut tokens = ~[];
    let mut tokenStart = 0;

    let mut i = 0;
    while(i < line.len()){
        if(line.char_at(i) == ' '){
            if(i == tokenStart){
                tokenStart += 1;
            }else{
                tokens.push(Leaf(line.slice(tokenStart, i).to_owned()));
                tokenStart = i + 1;
            } //end if

        }else if(line.char_at(i) == '('){
            match(line.rfind(')')){
                Some(tokenEnd) => {
                    tokens.push(tokenize(line.slice(i + 1, tokenEnd)));
                    i = tokenEnd;
                    tokenStart = i + 1;
                },
                None           => fail!("Unmatched bracket:" + line)
            } //end match
        } //end if

        i += 1;
    } //end while

    if(tokens.len() > 0 && tokenStart < line.len()){
        tokens.push(Leaf(line.slice(tokenStart, i).to_owned()));
    } //end if

    return
        if(tokens.len() == 0){
            Leaf(line.to_owned())
        }else{
            AST(tokens)
        };
}

fn main(){
    let reader = File::open(&Path::new("../bison-flex_experimental/test.hs")).unwrap();
    let mut bufRead = BufferedReader::new(reader);
    let ast = Parse(&mut bufRead);
}
