open Lip22Lib.Types       
open Lip22Lib.Prettyprint
open Lip22Lib.Main


(**********************************************************************
 trace test : (command, n_steps, location, expected value after n_steps)
 **********************************************************************)

let test_trace = [
    ("int x; x:=0; x:=x+1",5,"x",1);
    ("int x; int y; int w; proc f(val z) { z:=x; x:=y; y:=z }; x := 10; y := 20; f(0)",20,"x",20);
    ("int x; int y; proc f(val x) { x:=x+2 }; proc g(val y) { f(x) }; x := 10; g(0); x := x+1",20,"x",11);
    ("int x; proc f(ref y) { y := y+1 } proc g(ref y) { y := y-1 }; x:=51; f(x); g(x)",20,"x",51);
    ("int x; int res; proc f(ref x) { if x=0 then res:=1 else if x=1 then res:=0 else ( x:=x-2; f(x) ) }; x:=8; f(x)",50,"res",1);
    ("int x; int res; proc f(ref x) { if x=0 then res:=1 else if x=1 then res:=0 else ( x:=x-2; f(x) ) }; x:=9; f(x)",50,"res",0);
    ("int x; int r; proc f(val n) { if n=0 then r:=1 else ( f(n-1); r:=r*n ) }; x := 5; f(x)",50,"r",120);
    ("int x; x:=0; repeat if x<=5 then x:=x+1 else break forever",20,"x",6);
    ("array a[3]; int x; a[0]:=1; a[1]:=a[0]; a[2]:=a[0]+a[1]; x:=a[2]",10,"x",2);
]

              
let%test _ =
  print_newline();
  print_endline ("*** Testing trace...");  
  List.fold_left
    (fun b (ps,n,x,v) ->
       let p = parse ps in
       let t = last (trace n p) in (* actual result *)
       print_string (ps ^ " ->* " ^ string_of_conf (vars_of_prog p) t);
       let b' = (match t with
             St st -> apply st x = v
           | Cmd(_,_) -> failwith "program not terminated") in
       print_string (" " ^ (if b' then "[OK]" else "[NO : expected " ^ string_of_val v ^ "]"));
       print_newline();
       b && b')
    true
    test_trace
