  (**[add_absent s x] inserts the element [x] into the set [s]. No
     result is returned. {b The element [x], or an element [y] that is
     equivalent to [x], must not already be a member of [s]}: otherwise,
     the data structure would become inconsistent. It is recommended to
     guard this operation with [assert (not (mem s x))]. This allows the
     code to be both safe (when runtime assertions are enabled) and
     efficient (when runtime assertions are disabled).

     If necessary, the capacity of the set [s] is increased.

     Time complexity: the cost of an insertion operation is often {m O(1)};
     however, if the capacity of the set must be increased, it is {m O(n)}.
     Because this costly event is infrequent, the amortized complexity of
     insertion is {m O(\log n)}. *)
  val add_absent : set -> element -> unit
