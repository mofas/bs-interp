type exp =   
  | Symbol(string)
  | Number(int)  
  | Plus(exp, exp)
  | Multi(exp, exp)
  | Lambda(string, exp)
  | Application(exp, exp)
  
and env = 
	| EmptyEnv
  | Env(string, ret, env)
  
and ret =
	| Int(int)
    | Error(string)
	| Closure(string, exp, env);

    

let extendEnv = (x, value, env) => Env(x, value, env);

let handleMulti = (v1, v2) => {
  switch (v1, v2) {
  | (Int(int1), Int(int2)) => Int(int1 * int2)
  | _ => Error("type Mismatch in Multi") 
  }
};

let handlePlus = (v1, v2) => {
  switch (v1, v2) {
  | (Int(int1), Int(int2)) => Int(int1 + int2)
  | _ => Error("type Mismatch in Plus") 
  }
};


let rec interp = (exp, env) => {
    switch exp {
      | Symbol(y) => applyEnv(y, env)
      | Number(num) => Int(num)
      | Plus(e1, e2) => handlePlus(interp(e1, env), interp(e2, env))
      | Multi(e1, e2) => handleMulti(interp(e1, env), interp(e2, env))
      | Lambda(x, e) => Closure(x, e, env)
      | Application(rator, rand) => applyClosure(interp(rator, env), interp(rand, env))
    }
}
and applyEnv = (y, env) => {
	switch env {
		| EmptyEnv => Error("Cannot find variable")
		| Env(x, value, env2) => if ( x == y ) { value } else { applyEnv(y, env2) }
  }
}
and applyClosure = (v1, v2) =>{
  switch v1 {
  | Closure(x, b, env) => interp(b, extendEnv(x, v2, env))  
  | _ => Error("type Mismatch in Application") 
  }
};


/* 4*3 = 12 */
interp(Multi(Plus(Number(1), Number(3)), Number(3)), EmptyEnv) |> Js.log;

/* x = 4 */
interp(Symbol("x"), Env("x", Int(4), EmptyEnv)) |> Js.log;

/* (x => x + 3)(5) */
interp(Application(Lambda("x", Plus(Symbol("x"), Number(3))), Number(5)), EmptyEnv) |> Js.log;

/* (x => x(2))(n => n*3) */
interp(
Application(
  Lambda("x", Application(Symbol("x"), Number(2))),
  Lambda("n", Multi(Symbol("n"), Number(3)))
), EmptyEnv) |> Js.log
