use std::io::fs::File;
use std::path::Path;
use std::io::buffered::BufferedReader;
use std::hashmap::HashMap;


#[deriving(Eq, Clone, IterBytes)]
enum AbstractSyntaxTree{
    Leaf(~str),
    AST(~[AbstractSyntaxTree]),
}

struct Function{
    Header: AbstractSyntaxTree,
    Body: AbstractSyntaxTree
}

impl AbstractSyntaxTree{
    pub fn to_str(&self) -> ~str{
        self._to_str(0)
    }

    fn _to_str(&self, indent: uint) -> ~str{
        return " ".repeat(4*indent)
            + match(self){
                &Leaf(ref value) => format!("'{:s}'", *value),
                &AST(ref nodes) =>  "-\n" + nodes.map(|ast| ast._to_str(indent + 1)).connect("\n"),
            }; //end match
    }

    fn traverse(&self, process: |&AbstractSyntaxTree|){
        process(self);

        match(self){
            &Leaf(_)            => (),
            &AST(ref nodes)     =>
                for node in nodes.iter(){
                    node.traverse(|x| process(x));
                } //end for
        } //end match
    }

    fn traverse_sep(&self, processInner: |&[AbstractSyntaxTree]|, processLeaf: |&str|){
        match(self){
            &Leaf(ref value)    =>   processLeaf(*value),
            &AST(ref nodes)     => {
                processInner(*nodes);

                for node in nodes.iter(){
                    node.traverse_sep(|x| processInner(x), |x| processLeaf(x));
                } //end for
            }
        } //end match
    }

    fn leaves(&self) -> ~[~str]{
        let mut res = ~[];
        self.traverse_sep(|_| (), |s| res.push(s.to_owned()));
        return res;
    }
}


fn Parse(reader: &mut BufferedReader<File>) -> ~[Function] {
    //! Creates a new AST

    //parse the AST
    let mut functions = ~[];

    for line in reader.lines(){
        if(line.contains_char('=')){
            let words: ~[&str] = line.trim().splitn('=', 1).collect();
            assert_eq!(words.len(), 2);

            functions.push(Function{
                Header: tokenize(words[0]),        //this provides a list of arguments with pattern matching
                Body:   tokenize(words[1]),        //this contains the structure of the function definition
            })

        }else{
            fail!("Invalid line: " + line);
        } //end if
    } //end for

    //TODO: convert AST into DFD. We can skip that for now because our
    //TODO: Validate - all leaves of DFD should be constants or arguments. Types needs to be integers, tuples, or (in the future) lists

    return functions;
}

fn tokenize(line: &str) -> AbstractSyntaxTree{
    //! Converts expressions into a syntax tree.
    //! TODO: replace this with bison/flex, or something equally powerful

    let mut tokens = ~[];
    let mut tokenStart = 0;

    //divide into tokens based on words and brackets
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

    //handle operators
    loop{
        let mut i = tokens.len();

        //first tier of operators
        match(tokens.position_elem(&Leaf(~"*"))){
            Some(index) if index < i => i = index,
            _ => ()
        } //end match

        match(tokens.position_elem(&Leaf(~"/"))){
            Some(index) if index < i => i = index,
            _ => ()
        } //end match

        if(i != tokens.len()){
            let node = AST(~[tokens[i - 1].clone(), tokens[i].clone(), tokens[i + 1].clone()]);
            tokens.remove(i);
            tokens.remove(i);
            tokens[i - 1] = node;
            continue;
        } //end if

        //second tier of operators
        match(tokens.position_elem(&Leaf(~"+"))){
            Some(index) if index < i => i = index,
            _ => ()
        } //end match

        match(tokens.position_elem(&Leaf(~"-"))){
            Some(index) if index < i => i = index,
            _ => ()
        } //end match

        if(i != tokens.len()){
            let node = AST(~[tokens[i - 1].clone(), tokens[i].clone(), tokens[i + 1].clone()]);
            tokens.remove(i);
            tokens.remove(i);
            tokens[i - 1] = node;
            continue;
        } //end if

        break;
    } //end loop

    return
        if(tokens.len() == 0){
            Leaf(line.to_owned())
        }else{
            AST(tokens)
        };
}

fn OutputVerilog(functions: ~[Function]) -> ~str{
    //! Output DFD as Verilog. Each function becomes a combinatorial module
    //! we represent intermediate values as wires, and operations as assign statements or module imports.

    //! NOTE: because we using an AST instead of a proper DFD in the prototype, there will be lots of hacks in here that will need
    //! to be refactored when we implement the DFD code

    let mut res = ~"";

    for function in functions.iter() {
        //the first leaf in the header will be the function name
        //all others will be arguments names
        let args = function.Header.leaves();
        res.push_str(format!("module {} (", args[0]));

        for arg in args.iter().skip(1){
            res.push_str(*arg + ", ");
        } //end for

        res.pop_char();
        res.pop_char();
        res.push_str(");\n");

        //input declarations
        //TODO: we should really be checking what type the argument actually is, but that requires type inference
        for arg in args.iter().skip(1){
            res.push_str(format!("\tinput [31:0] {};\n", *arg));
        } //end for

        //output declaration
        //TODO: this should really be based on the function's type, but I haven't implemented code to perform type inference yet
        res.push_str("\toutput [31:0] res;\n\n");

        //process operators and function calls

        //maps nodes to intermediate variables. Note that all leaves are implicitly mapped to their constants/args.
        let mut intCount = 0;
        let mut intermediates = HashMap::<AbstractSyntaxTree, uint>::new();

        let getNodeID = |node: &AbstractSyntaxTree| -> ~str{
            match(node){
                &Leaf(ref value) => value.to_owned(),
                &AST(_)          => {
                    let id = *intermediates.find_or_insert(node.clone(), intCount);

                    if(id == intCount){
                        res.push_str(format!("\twire [31:0] w{:u};\n", id));
                        intCount += 1;
                    } //end if

                    return "w" + id.to_str();
                }
            } //end match
        }; //end lambda

        //output function calls. We assume these are simple operators, as we do not have support for function calls yet.
        let mut assigns = ~"";
        let mut rootNode = function.Body.clone();
        function.Body.traverse(|node: &AbstractSyntaxTree|
            match(node){
                &AST(ref children) if children.len() == 1 => rootNode = children[0].clone(),
                &AST(ref children) if children.len() != 3 => fail!("Invariant violated - an operator is non-binary:\n" + node.to_str()),
                &AST(ref children) => assigns.push_str(format!("\tassign {} = {} {} {};\n",
                                                        getNodeID(node),
                                                        getNodeID(&children[0]),
                                                        getNodeID(&children[1]),            //HACK: assuming this is the operator
                                                        getNodeID(&children[2]))),
                &Leaf(_)       => ()
            }
        );

        res.push_str("\n");
        res.push_str(assigns);
        res.push_str(format!("\tassign res = {};\n", getNodeID(&rootNode)));
        res.push_str("endmodule;\n\n");
    } //end for

    return res;
}

fn main(){
    let reader = File::open(&Path::new("test.hs")).unwrap();
    let mut bufRead = BufferedReader::new(reader);
    let functions = Parse(&mut bufRead);
    let v = OutputVerilog(functions);

    println(v);
}
