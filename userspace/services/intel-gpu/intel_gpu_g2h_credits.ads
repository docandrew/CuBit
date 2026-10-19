-- G2H reply credits, one pool for every GuC context (i915 intel_guc_ct.c:
-- ct->ctbs.recv.space, guc->submission_state's g2h accounting). An H2G
-- request whose answer comes back as a G2H message (scheduling MODE_SET,
-- DEREGISTER) reserves its answer's words before it is sent and gives them
-- back when the answer arrives. A request that cannot reserve waits (the
-- caller sees back pressure and retries on a later turn), so however many
-- contexts exist, answers in flight never exceed the G2H ring's room for
-- them and no answer is lost to an overflowing ring. One context holds at
-- most one answer's credit at a time, so no client can take the pool from
-- the others for longer than one GuC round trip.
package Intel_GPU_G2H_Credits with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- Most words any one pool may account (a 16 KiB G2H ring has 4096).
   Max_Words : constant := 2 ** 16;
   subtype Word_Count is Natural range 0 .. Max_Words;
   type Pool is private
     with Default_Initial_Condition =>
       Capacity (Pool) = 0 and In_Use (Pool) = 0;
   function Capacity (Object : Pool) return Word_Count;
   function In_Use (Object : Pool) return Word_Count;
   function Available (Object : Pool) return Word_Count is
     (if In_Use (Object) <= Capacity (Object)
      then Capacity (Object) - In_Use (Object) else 0);
   -- Once, before any request: the G2H words kept for answers.
   procedure Configure (Object : in out Pool; Words : Word_Count)
     with Pre => In_Use (Object) = 0,
          Post => Capacity (Object) = Words and In_Use (Object) = 0;
   -- A context's held credit changed from Before to After words (a request
   -- reserved, an answer arrived, a refused send rolled back). Accepted is
   -- False (and nothing changes) when the context never held Before or the
   -- pool lacks the room: the caller then fails closed.
   procedure Exchange
     (Object : in out Pool; Before, After : Word_Count; Accepted : out Boolean)
     with Post => Capacity (Object) = Capacity (Object)'Old and then
       In_Use (Object) <= Capacity (Object) and then
       (if Accepted then
          In_Use (Object) = In_Use (Object)'Old - Before + After
        else In_Use (Object) = In_Use (Object)'Old);
private
   type Pool is record
      Limit : Word_Count := 0;
      Used : Word_Count := 0;
   end record
     with Type_Invariant => Pool.Used <= Pool.Limit;
   function Capacity (Object : Pool) return Word_Count is (Object.Limit);
   function In_Use (Object : Pool) return Word_Count is (Object.Used);
end Intel_GPU_G2H_Credits;
