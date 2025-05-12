var x = 2;
let y = "aaa";

const $obj = {
  x: 3,
  fa: function () {
    let x = 1;
    return this.x;
  },
  fb: () => {    
    console.log(y);
    return this.x;
  }
};

console.log($obj.fa()); // ?
console.log($obj.fb()); // ?
mate 