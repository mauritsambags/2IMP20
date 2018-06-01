/*
Assignment 2 2IMP20.
Authors: Maurits Ambags (0771400), Jeanpierre Balster ().
GLT Group 36.
*/

module Typecheck

import Prelude;
import Abstract;
import Load;

alias TENV = tuple[ map[PicoId, TYPE] symbols, list[tuple[loc l, str msg]] errors]; 

TENV addError(TENV env, loc l, str msg) = env[errors = env.errors + <l, msg>];      

str required(TYPE t, str got) = "Required <getName(t)>, got <got>";                 
str required(TYPE t1, TYPE t2) = required(t1, getName(t2));

// compile Expressions.

/*
It should be noted that Expressions are checked, but once an error is found,
the rest of the Expression is not parsed further. This can be fixed by, instead
of just calling addError(), also continuing to call checkExp on the operands.
This was not added though, because if part of an Expression is incorrect, the user will
instantly be notified of any further errors as soon as the first error is resolved.
As such, this should not actually hinder code development.
Furthermore, in this case likely the red underlines would overlap between nested errors,
so the visual indication would not even be noticeable.
*/

TENV checkExp(exp:natCon(int N), TYPE req, TENV env) =                              
  req == natural() ? env : addError(env, exp@location, required(req, "natural"));

TENV checkExp(exp:strCon(str S), TYPE req, TENV env) =
 req == string() ? env : addError(env, exp@location, required(req, "string"));
 
// checkExp for boolean types.
TENV checkExp(exp:boolCon(bool b), TYPE req, TENV env) =
 req == boolean() ? env : addError(env, exp@location, required(req, "boolean"));

TENV checkExp(exp:id(PicoId Id), TYPE req, TENV env) {                              
  if(!env.symbols[Id]?)
     return addError(env, exp@location, "Undeclared variable <Id>");
  tpid = env.symbols[Id];
  return req == tpid ? env : addError(env, exp@location, required(req, tpid));
}

TENV checkExp(exp:add(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
                   : addError(env, exp@location, required(req, "natural"));
  
TENV checkExp(exp:sub(EXP E1, EXP E2), TYPE req, TENV env) =                      
  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
                   : addError(env, exp@location, required(req, "natural"));

TENV checkExp(exp:conc(EXP E1, EXP E2), TYPE req, TENV env) =                    
  req == string() ? checkExp(E1, string(), checkExp(E2, string(), env))
                   : addError(env, exp@location, required(req, "string"));

/*
"New" Expression checks for the added logical operators. These are pretty standard,
following the example of operators for other types.
*/                 
//Boolean not.
TENV checkExp(exp:b_not(EXP E1), TYPE req, TENV env) =
 req == boolean() ? checkExp(E1, boolean(), env) : addError(env, exp@location, required(req, "boolean"));
 
//Boolean and.
TENV checkExp(exp:b_and(EXP E1, EXP E2), TYPE req, TENV env) =
 req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
 	: addError(env, exp@location, required(req, "boolean"));

//Boolean or.
TENV checkExp(exp:b_or(EXP E1, EXP E2), TYPE req, TENV env) =
 req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
 	: addError(env, exp@location, required(req, "boolean"));
 	
/*
Sadly, the comparison operator (as well as !=) only checks for boolean operands.
We considered implementing different cases for this operator to distinguish comparisons between
booleans, naturals and strings separately. However, this required us to completely rewrite
the Expression Syntax in order for an Expression to "inherit" the type of its operands.
For example, if E1 = add(natural left, natural right), then E1 should be assumed to be of type "NatExp".
This was attempted but, since the parser does not actually report why it fails to parse (when the rascal code compiles),
this was reverted after failing to get the parser working again.
*/

//Boolean ==.
TENV checkExp(exp:eq(EXP E1, EXP E2), TYPE req, TENV env) =
	req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
		: addError(env, exp@location, required(req, "boolean"));
		
//Boolean !=.
TENV checkExp(exp:neq(EXP E1, EXP E2), TYPE req, TENV env) =
 req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
 	: addError(env, exp@location, required(req, "boolean"));


// check a statement

TENV checkStat(stat:asgStat(PicoId Id, EXP Exp), TENV env) {                        
  if(!env.symbols[Id]?)
     return addError(env, stat@location, "Undeclared variable <Id>");
  tpid = env.symbols[Id];
  return checkExp(Exp, tpid, env);
}
	
TENV checkStat(stat:ifElseStat(EXP Exp,                                             
                              list[STATEMENT] Stats1,
                              list[STATEMENT] Stats2),
               TENV env){
    env0 = checkExp(Exp, boolean(), env);
    env1 = checkStats(Stats1, env0);
    env2 = checkStats(Stats2, env1);
    return env2;
}

TENV checkStat(stat:whileStat(EXP Exp, 
                             list[STATEMENT] Stats1),
                 TENV env) {
    env0 = checkExp(Exp, boolean(), env);
    env1 = checkStats(Stats1, env0);
    return env1;
}

/*
The for statement consists of three lists of statements (init, maint, body) as well as one guard expression.
These can be checked in similar fashion as in the while statement.
*/

TENV checkStat(stat:forStat(list[STATEMENT] Stats1,
							EXP Exp, 
							list[STATEMENT] Stats2, 
							list[STATEMENT] Stats3),
				TENV env){
	env0 = checkStats(Stats1, env);
	env1 = checkExp(Exp, boolean(), env0);
	env2 = checkStats(Stats2, env1);
	env3 = checkStats(Stats3, env2);
	return env3;
}

// check a list of statements
TENV checkStats(list[STATEMENT] Stats1, TENV env) {                                 
  for(S <- Stats1){
      env = checkStat(S, env);
  }
  return env;
}
  
// check declarations

TENV checkDecls(list[DECL] Decls) =                                                 
    <( Id : tp  | decl(PicoId Id, TYPE tp) <- Decls), []>;

// check a Pico program

public TENV checkProgram(PROGRAM P){                                                
  if(program(list[DECL] Decls, list[STATEMENT] Series) := P){
     TENV env = checkDecls(Decls);
     return checkStats(Series, env);
  } else
    throw "Cannot happen";
}
                                                                                    
public list[tuple[loc l, str msg]] checkProgram(str txt) = checkProgram(load(txt)).errors;