// Grammar for a part of LISP
// ==========================
//
//

S = Expression  //Identifier

_ "whitespace"
  = [ \t\n\r]*
__  = [ \t\n\r]+

Alph = [a-z,A-Z]

Keyword = "define" / "if" / "else" / "cond"
BuiltinFunctionStr = ("car" / "cdr" / "cons" / "list" / "append" / "eq" / "equal" / "length"
                       / "map" / "apply" / "display"
                       / "+" / "-" / "*" / "/" / ">" / "<" / ">=" / "<=" / "=")
BuiltinStr = Keyword / BuiltinFunctionStr / Constant

True = "#t" {return true;}
False = "#f" {return false;}
Boolean = True / False
Null = "'()" / "null"
Constant = Boolean / Null

Identifier = !(BuiltinStr ((!.)/__))[_]*Alph(Alph/[_,-]/[0-9])* {return text()}

Integer "integer"
  = _ [0-9]+ { return parseInt(text(), 10); }
SignedInteger
  = _ sign:("+" / "-") _ int:Integer { return sign === "-" ? -int : int; }
Int = Integer / SignedInteger

Float = Int"." [0-9]+ {return parseFloat(text(), 10);}
Numeric = Float / Int
String = "\"" symbols:[^"]* "\"" {return symbols.reduce((res, char) => res+char, "")}

ArithmeticFunction
    = "("_ operation:("+" / "-" / "*" / "/") _ init:Expression _ rest:(expr:Expression _{ return expr; })* _  _ ")"{
        //console.log("rest: ", rest)
        //filtered_rest = map(item => parseFloat(item.text(), 10))
        //filtered_rest = rest.map(item => parseFloat(item.text(), 10))
        //console.log("filtered_rest: ", filtered_rest)
        return  rest.reduce( (acc, x) => {
            switch (operation){
                case "+" : return acc + x;
                case "-" : return acc - x;
                case "*" : return acc * x;
                case "/" : return acc / x;
             }
            }, init);
    } // object = {name: value}

ArithmeticComparison
= "("_ operator:( ">=" / "<=" / ">" / "<" / "=" / "/=")  _ init:Expression
     _ rest:(expr:Expression _ {return expr;})+ _ ")" {
      return rest.reduce( (acc, x) => {switch (operator){
        case "<" : return acc && (init < x);
        case ">" : return acc && (init > x);
        case "=" : return acc && (init == x);
        case "<=" : return acc && (init <= x);
        case ">=" : return acc && (x <= init);
        case "/=" : return acc && (init != x);
          }}, true);
    }

If = "("_ "if" _ condition:Expression _ then_:Expression _ else_:Expression _ ")" {
    if (condition) {
        return then_;
    } else {
        return else_;
    }
}

BuiltinFunctionCall = ArithmeticFunction / ArithmeticComparison / If // ArithmeticFunction / If / Display
Expression = Float / Int /  String / Boolean / Null / Identifier / BuiltinFunctionCall  // / FunctionCall