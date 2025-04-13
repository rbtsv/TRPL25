// Grammar for a part of LISP
// ==========================
//
//
{
    var $scope = {} //global  scope
    var $$scope = [] // local scope stack
    $$scope.push($scope);
    var $get_current_scope = () => $$scope[$$scope.length-1]
    //$scope["x"] = 100;
    //let scope = $get_current_scope()
    class Function{
        constructor(name, args, definitions, expressions){
            this.name = name;
            this.args = args;
            this.definitions = definitions;
            this.expressions = expressions;
            this.scope = {...$get_current_scope()};
            $$scope.push(this.scope);
            this.definitions.forEach(definition => definition.define());
            $$scope.pop();
            //this.scope =  get_current_scope();
        }
        call(args){
            console.log("Inside the Function call:")
            //let scope = {...this.scope}
            let scope = $$scope.reduce((acc, x) => ({...acc, ...x}), {});
            scope = {...scope, ...this.scope};
            this.args.forEach((arg, i) => {
                scope[arg] = args[i];
            });
           $$scope.push(scope);
           console.log("args:", args);
           console.log("scope:", scope);
           if("n" in scope){
               console.log("Value of n:", scope["n"].call());
           }
           //let expr_values = this.expressions.map((x) => x.call());
           console.log("expressions:", this.expressions);
           let result = this.expressions.reduce((acc, x) => x.call(), null);
           $$scope.pop();
           return result;
        }
    }
    //(define (plus a b) (+ a b))
    $scope["_plus"] = new Function("_plus", ["a", "b"], [], [
        {call: () => {
            let scope = $get_current_scope();
            console.log("plus scope:", scope);
            console.log("Plus current scope:", scope);
            return scope["a"].call() + scope["b"].call();
        }}
    ]);
}

S =(_ evaluated:(expr:Expression{return expr.call()}
    / def:Definition {def.define(); return def}) _ {return evaluated}) *

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
    = "("_ operation:("+" / "-" / "*" / "/") _ init:Expression _ rest:MayBeExpressions  _ ")"{
       const opMap = {
                 "+": (a, b) => a + b,
                 "-": (a, b) => a - b,
                 "*": (a, b) => a * b,
                 "/": (a, b) => a / b
               };
               //console.log("init:", init);
               console.log("rest:", rest);
               return {call: () => rest.map((expr) => expr.call()).reduce(opMap[operation], init.call())}
    } // object = {name: value}

ArithmeticComparison
= "("_ operator:( ">=" / "<=" / ">" / "<" / "=" / "/=")  _ init_expr:Expression
     _ rest:Expressions _ ")" {
         return {call: function (){
                const init = init_expr.call();
                return rest.map((expr) => expr.call())
                            .reduce( (acc, x) =>
                                {switch (operator){
                                    case "<" : return acc && (init < x);
                                    case ">" : return acc && (init > x);
                                    case "=" : return acc && (init == x);
                                    case "<=" : return acc && (init <= x);
                                    case ">=" : return acc && (x <= init);
                                    case "/=" : return acc && (init != x);
                          }}, true);
             }}

    }
Expressions = (_ expr:Expression _ {return expr;})+
MayBeExpressions = (_ expr:Expression _ {return expr;})*
/*
(define (plus a b) (+ a b))
(plus 1 2)
*/

If = "("_ "if" _ cond_expr:Expression _ then_expr:Expression _ else_expr:Expression _ ")" {
    return {call: function(){console.log("Inside if");
            let condition = cond_expr.call();
            console.log("condition:", condition);
                    if(condition){
                        let then = then_expr.call();
                        console.log("then:", then);
                        return then;
                    }else{let else_ = else_expr.call(); console.log("else:", else_);  return else_;}
            }
    }
}

BuiltinFunctionCall = ArithmeticFunction / ArithmeticComparison / If // ArithmeticFunction / If / Display
ConstExpr = value:(Float / Int /  String / Boolean / Null) { return {call: () => value} }
Expression = ConstExpr / idx:Identifier {return {call: () => $get_current_scope()[idx].call() } }
            / BuiltinFunctionCall  / FunctionCall

MayBeDefinitions = (_ def:Definition _ {return def;})*
Definition
    = "("_ "define" _ defined:(DefineVar / DefineFunction)  _ ")" {return defined}

DefineVar = name:Identifier _ expr:Expression _{
    function define() {
        if (name in $get_current_scope()) {
            throw new Error("Variable already defined: " + name);
        }
        const scope = $get_current_scope();
        scope[name] = expr;
    }
    return {"definition": name, define: define}
}

Args = (_ arg:Identifier _ {return arg})*
// (define (plus a b) (define x 1) (+ a b))
DefineFunction = "(" _ name:Identifier _ args:Args _ ")"
                _ definitions:MayBeDefinitions
                _ exprs:Expressions _ {
    function define() {
        const current_scope = $get_current_scope();
        $$scope.push(current_scope);
        current_scope[name] = new Function(name, args, definitions, exprs);
        $$scope.pop();
     }
    return {"definition": name, args: args, define: define};

}

FunctionCall = "("_ name:Identifier _ args:MayBeExpressions _ ")" {
    return {call: () => $get_current_scope()[name].call(args)}
}
