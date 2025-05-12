function greet(person: string): string;
function greet(person: number): string;
function greet(person: any): string {
    if (typeof person === "string") {
        return `Hello, ${person}`;
    }
    return `Your number is ${person}`;
}

function def_vals(s: "a" | "b" | "c"): number {
    if (s == "a") return 1;
    if (s == "b") return 2;
    if (s == "c")
        return 3;
    throw new Error("Invalid value");

}

function my_plus(a: number, b: number): number;
function my_plus(a: string, b: string): string;
function my_plus(a: any, b: any): any {
    return a + b
}


console.log(def_vals("a"));
console.log(greet("John"));
console.log(greet(123));
console.log(my_plus(1, 2));
console.log(my_plus("1", "2"));
let t = {a: 1, b: 2, c: 3};
console.log(my_plus(...Object.values(t))); // 3
console.log(my_plus(1, 2, 3));
console.log(my_plus(1, ));
//console.log(my_plus("1", 2));
//console.log(greet({a: true}));
