module Typecheck

import Prelude;
import Abstract;
import Load;

alias TENV = tuple[ map[PicoId, TYPE] symbols, list[tuple[loc l, str msg]] errors]; 

TENV addError(TENV env, loc l, str msg) = env[errors = env.errors + <l, msg>];      

str required(TYPE t, str got) = "Required <getName(t)>, got <got>";                 
str required(TYPE t1, TYPE t2) = required(t1, getName(t2));

// compile Expressions.

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
 	
//Boolean ==.
//TENV checkExp(exp:eq(EXP E1, EXP E2), TYPE req, TENV env) =
// req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
// 	: addError(env, exp@location, required(req, "boolean"));
 	
TENV checkExp(exp:eq(EXP E1, EXP E2), TYPE req, TENV env) {
	if(req == boolean())	
		return checkExp(E1, boolean(), checkExp(E2, boolean(), env));
	else
	return addError(env, exp@location, required(req, "boolean"));
}
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

// check the for statement
// The for statement consist of 3 Statement lists and 1 Expression
// that need to be tested for Type errors and undeclared variables.

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