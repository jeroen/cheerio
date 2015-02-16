/* Cheerio bindings for R, Jeroen Ooms, 2015 */

if(typeof cheerio !== "function"){
  throw "Cheerio library not loaded."
}

/* helper function for generating random variable names */
var n = 0;
function random_var(){
  return ("_var_" + n++);
}

/* assign an object to a random variable in the global scope */
function create_object(x){
  var myvar = random_var();
  global[myvar] = x;
  return myvar;
}

/* this is the global document object,  */
function new_doc(html, opts){
  global.$ = cheerio.load(html, opts);
}

/* select a node and assign to random variable name */
function select_node(selector){
  return create_object($(selector));
}

