module Abstract

public data TYPE = natural() | string() | boolean();    
	  
public alias PicoId = str;                  
	  
public data PROGRAM =                       
  program(list[DECL] decls, list[STATEMENT] stats);

public data DECL =
  decl(PicoId name, TYPE tp);

// We extend this abstract syntax tree with the boolean operators:
// NOT, AND, OR
// The operator NOT only takes a single expression while AND, OR takes two.
// Then we also include the == and != operators to test whether the two
// boolean Expressions are equal or not.

public data EXP = 
       id(PicoId name)
     | natCon(int iVal)
     | strCon(str sVal)
     | boolCon(bool bVal)
     | add(EXP left, EXP right)
     | sub(EXP left, EXP right)
     | conc(EXP left, EXP right)
     | b_not(EXP val)
     | b_and(EXP left, EXP right)
     | b_or(EXP left, EXP right)
     | eq(EXP left, EXP right)
     | neq(EXP left, EXP right)
     ;
    
    
// We add the for loop statement here in order to Type check it
// The for-loop construct is already described in detail in Syntax.rsc
public data STATEMENT =
       asgStat(PicoId name, EXP exp)
     | ifElseStat(EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart)
     | whileStat(EXP exp, list[STATEMENT] body)
     | forStat(list[STATEMENT] init, EXP guard, list[STATEMENT] maint, list[STATEMENT] body)
     ;

anno loc TYPE@location;                   
anno loc PROGRAM@location;
anno loc DECL@location;
anno loc EXP@location;
anno loc STATEMENT@location;

public alias Occurrence = tuple[loc location, PicoId name, STATEMENT stat];