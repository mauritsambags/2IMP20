module Syntax

import Prelude;

/*
We will model booleans as separate lexical components, so they should not be considered as Id's.
So, exclude the Boolean qualifiers from the regexp that matches Id's.
Naturally, a boolean can only take the values "true" or "false", so these are the only allowed patterns.
*/
lexical Id  = ([a-z][a-z0-9]* !>> [a-z0-9]) \ (PicoKeywords | Boolean);
lexical Natural = [0-9]+ ;
lexical String = "\"" ![\"]*  "\"";
lexical Boolean = ("true" | "false");

/*
To signify their special functionality, it would make sense to reserve keywords for the logical operators.
If we were to re-use the mathematical operators such as "!","|","&", this may not be as clear.
Furthermore, it is likely that these mathematical operators may receive additional functionality in the future,
for example to perform bitwise operations.
Since the words "not", "and", and "or" should never (under common programming syntax guidelines) be used as
identifiers, we deem it safe to reserve them as keywords.
*/
keyword PicoKeywords = "begin" | "end" | 
                       "declare" | 
                       "if" | "then" | "else" | "fi" | 
                       "while" | "do" | "od" |
                       "not" | "and" | "or"
                       ;

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "%" ![%]+ "%"
   | @category="Comment" "%%" ![\n]* $
   ;

start syntax Program 
   = program: "begin" Declarations decls {Statement  ";"}* body "end" ;

syntax Declarations 
   = "declare" {Declaration ","}* decls ";" ;  
 
syntax Declaration = decl: Id id ":" Type tp;

/*
Naturally, booleans are a separate type that can be declared, so another constructor is added
for the Type syntax.
*/
syntax Type 
   = natural:"natural" 
   | string :"string" 
   | boolean :"boolean"
   ;

/*
The for-loop construct can be constructed in several ways; Rascal itself chooses to have a single argument
in the header of the for-loop, containing a generator. This seems overly complicated for the (seemingly imperative)
programming language that we are developing. On top of this, we would require a notion of generators and/or
iterators in order to then semantically describe the behaviour of a for-loop in terms of a while-loop.
Instead, we use the C++/Java-like notation of a for-loop, consisting of an initialisation statement,
a maintenance statement and a guard expression. This is useful because we can now easily express this
for-loop in terms of a while-loop like so:

{initialisation statement};
while ({guard expression}){
	{loop body};
	{maintenance statement};
}

This hopefully saves us a lot of headaches in the future when specifying semantics for our syntax.
*/
syntax Statement 
   = asgStat: Id var ":="  Expression val 
   | ifElseStat: "if" Expression cond "then" {Statement ";"}*  thenPart "else" {Statement ";"}* elsePart "fi"
   | whileStat: "while" Expression cond "do" {Statement ";"}* body "od"
   | forStat: "for" "(" Statement init ";" Statement maint ";" Expression guard ")" 
   	 "do" {Statement ";"}* body "od"
  ;  

/*
As discussed before, we will use the keywords "not", "and" and "or" to denote the boolean operators.
The associativity of these operators is not important, but we choose left-associativity to remain consistent
in parsing with respect to the other operators.
Note that we wish to parse expressions in order of significance of logical operators, 
i.e. not > and > or.
*/
syntax Expression 
   = id: Id name
   | strCon: String string
   | natCon: Natural natcon
   | bracket "(" Expression e ")"
   | left ( b_not: "not" Expression b
   		  | b_and: Expression lhs "and" Expression rhs
   	      | b_or: Expression lhs "or" Expression lhs
   		  )
   > left conc: Expression lhs "||" Expression rhs
   > left ( add: Expression lhs "+" Expression rhs
          | sub: Expression lhs "-" Expression rhs
          )
  ;

public start[Program] program(str s) {
  return parse(#start[Program], s);
}

public start[Program] program(str s, loc l) {
  return parse(#start[Program], s, l);
}
