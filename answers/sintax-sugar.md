### Ejercicios ###
1- a- let x : Nat = 2 in x + 1 === let (x:Nat) = 2 in x + 1
b) fun (x:Nat) -> x === 
c) let id (x:Nat) : Nat = x in id 10 === let (id: Nat -> Nat) = fun (x:Nat) -> x in id 10

d) let app5 (f : Nat -> Nat) : Nat = f 5 in app5 (fun (x : Nat) -> x + x) 
=== 
let (app5: (Nat -> Nat) -> Nat) = 
fun (f: Nat -> Nat) -> f 5 
in app5 (fun (x : Nat) -> x + x)

e) fun (x:Nat) (y:Nat) -> ifz x then y else 1 === fun (x:Nat) -> fun (y:Nat) -> ifz x then y else 1

### Ejercicio 2 ###
a) let rec doble (x:Nat) : Nat = ifz x then 0 else 2 + (doble (x - 1))
===
let (doble: Nat -> Nat) = fix (doble: Nat -> Nat)(x:Nat) -> ifz x then 0 else 2 + (doble (x - 1)) 

b) let rec ack (m:Nat) (n:Nat) : Nat =
ifz m
then n + 1
else (ifz n
then ack (m - 1) 1
else ack (m - 1) (ack m (n - 1)))
===
let (ack : Nat -> Nat -> Nat) = fix (ack: Nat -> Nat -> Nat)(m:Nat) -> fun (n:Nat) -> ifz m then n+1 else (ifz n then ack (m-1) 1 else ack (m-1) (ack m (n-1))) 