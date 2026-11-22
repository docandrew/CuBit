pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Child_Specs;

--  Supervision edges (SUP-001): procmgr is the only process creator, and
--  supervision is a typed edge it records, supervisor -> child, not
--  parentage. Start_Child adds one edge for each child it starts; the
--  child's end removes it; a supervisor's end marks each of its children
--  Reaping (procmgr stops them), and their own ends cascade to theirs.
--
--  The proved core works on an array the caller owns (procmgr keeps it in
--  a growable heap block) and its length. Well_Formed is the invariant:
--
--    * every edge names a child, a supervisor, and they differ;
--    * a process has at most one supervisor (no child appears twice);
--    * ranks order the edges by when they were added, and a supervisor
--      that is itself supervised was attached before each of its children
--      (its edge's rank is lower). Ranks strictly decrease along any
--      supervisor chain, so the edges have no cycle.
--
--  Add keeps it because a child is fresh (a new process identity, KERN-003:
--  in no edge, as child or supervisor) and takes the next rank. Remove
--  keeps it because every rule is about pairs of edges that remain.
--  Process identities are words; 0 is none. No I/O.
package Supervision_Edges with SPARK_Mode is
   package CS renames CuBit.Child_Specs;
   use type CS.Restart_Kind;

   subtype Process_Word is Unsigned_64;
   No_Process : constant Process_Word := 0;

   type Edge is record
      Child      : Process_Word := No_Process;
      Supervisor : Process_Word := No_Process;
      Rank       : Unsigned_64 := 0;
      Restart    : CS.Restart_Kind := CS.Temporary;
      --  Its supervisor ended and procmgr stopped it; its own end removes
      --  the edge.
      Reaping    : Boolean := False;
   end record;

   type Edge_Array is array (Positive range <>) of Edge;

   function Edge_Valid (E : Edge; Next_Rank : Unsigned_64) return Boolean is
     (E.Child /= No_Process and then E.Supervisor /= No_Process
      and then E.Child /= E.Supervisor and then E.Rank < Next_Rank);

   function One_Supervisor (Edges : Edge_Array; Length : Natural) return Boolean is
     (for all I in 1 .. Length =>
        (for all J in 1 .. Length => (if I /= J then Edges (I).Child /= Edges (J).Child)))
   with Pre => Edges'First = 1 and then Length <= Edges'Last;

   function Ordered (Edges : Edge_Array; Length : Natural) return Boolean is
     (for all I in 1 .. Length =>
        (for all J in 1 .. Length =>
           (if Edges (J).Child = Edges (I).Supervisor then Edges (J).Rank < Edges (I).Rank)))
   with Pre => Edges'First = 1 and then Length <= Edges'Last;

   function Well_Formed (Edges : Edge_Array; Length : Natural; Next_Rank : Unsigned_64) return Boolean is
     (Edges'First = 1 and then Length <= Edges'Last
      and then (for all I in 1 .. Length => Edge_Valid (Edges (I), Next_Rank))
      and then One_Supervisor (Edges, Length)
      and then Ordered (Edges, Length));

   --  Process is in no edge, as child or as supervisor.
   function Fresh (Edges : Edge_Array; Length : Natural; Process : Process_Word) return Boolean is
     (for all I in 1 .. Length =>
        Edges (I).Child /= Process and then Edges (I).Supervisor /= Process)
   with Pre => Edges'First = 1 and then Length <= Edges'Last;

   --  Child's edge, or 0.
   function Find_Child (Edges : Edge_Array; Length : Natural; Child : Process_Word) return Natural
   with Pre  => Edges'First = 1 and then Length <= Edges'Last,
        Post => Find_Child'Result <= Length
                and then (if Find_Child'Result = 0
                          then (for all I in 1 .. Length => Edges (I).Child /= Child)
                          else Edges (Find_Child'Result).Child = Child);

   --  Child's supervisor (No_Process when it has none). With one supervisor
   --  per process, this is the only one.
   function Supervisor_Of (Edges : Edge_Array; Length : Natural; Child : Process_Word) return Process_Word
   with Pre  => Edges'First = 1 and then Length <= Edges'Last,
        Post => (if Supervisor_Of'Result /= No_Process then
                   (for some I in 1 .. Length =>
                      Edges (I).Child = Child and then Edges (I).Supervisor = Supervisor_Of'Result));

   --  The supervisor/child edge, ranked Next_Rank, then Next_Rank one more.
   procedure Add
     (Edges : in out Edge_Array; Length : in out Natural; Next_Rank : in out Unsigned_64;
      Child, Supervisor : Process_Word; Restart : CS.Restart_Kind)
   with Pre  => Well_Formed (Edges, Length, Next_Rank)
                and then Length < Edges'Last
                and then Next_Rank < Unsigned_64'Last
                and then Child /= No_Process and then Supervisor /= No_Process
                and then Child /= Supervisor
                and then Fresh (Edges, Length, Child),
        Post => Well_Formed (Edges, Length, Next_Rank)
                and then Length = Length'Old + 1
                and then Next_Rank = Next_Rank'Old + 1
                and then Edges (Length).Child = Child
                and then Edges (Length).Supervisor = Supervisor
                and then (for all I in 1 .. Length'Old => Edges (I) = Edges'Old (I));

   --  Edge Index removed (the last edge takes its place).
   procedure Remove
     (Edges : in out Edge_Array; Length : in out Natural; Next_Rank : Unsigned_64; Index : Positive)
   with Pre  => Well_Formed (Edges, Length, Next_Rank) and then Index <= Length,
        Post => Well_Formed (Edges, Length, Next_Rank)
                and then Length = Length'Old - 1
                and then (for all I in 1 .. Length =>
                            Edges (I) = (if I = Index then Edges'Old (Length'Old) else Edges'Old (I)));

   --  The child of edge Index is being reaped.
   procedure Mark_Reaping
     (Edges : in out Edge_Array; Length : Natural; Next_Rank : Unsigned_64; Index : Positive)
   with Pre  => Well_Formed (Edges, Length, Next_Rank) and then Index <= Length,
        Post => Well_Formed (Edges, Length, Next_Rank) and then Edges (Index).Reaping;

   ---------------------------------------------------------------------------
   --  Restart intensity (OTP's intensity/period): a child restarted more
   --  than Intensity times within Window_Ms escalates instead. Times are
   --  CuBit.Monotonic milliseconds.
   ---------------------------------------------------------------------------
   type Restart_History is record
      Window_Start : Unsigned_64 := 0;
      Restarts     : Natural := 0;
   end record;

   type Restart_Decision is (Stay_Ended, Restart, Escalate);

   --  What to do when a supervised child ended. Abnormal: a nonzero code,
   --  killed or faulted. Reaped children (their supervisor ended) are
   --  never restarted.
   procedure Decide
     (Kind : CS.Restart_Kind; Intensity : CS.Intensity_Count; Window : CS.Window_Ms;
      Abnormal, Reaped : Boolean; Now : Unsigned_64;
      History : in out Restart_History; Decision : out Restart_Decision)
   with Post => (if Reaped or else Kind = CS.Temporary
                   or else (Kind = CS.Transient and then not Abnormal)
                 then Decision = Stay_Ended)
                and then (if Decision = Restart then History.Restarts <= Natural (Intensity));
end Supervision_Edges;
