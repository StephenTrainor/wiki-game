type 'a t

val create : 'a -> 'a t
val size : 'a t -> int
val swap : 'a t -> int -> int -> unit
val append : 'a t -> 'a -> unit
val pop : 'a t -> 'a
