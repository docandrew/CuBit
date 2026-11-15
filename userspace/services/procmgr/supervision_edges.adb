pragma Ada_2022;

package body Supervision_Edges with SPARK_Mode is

   function Find_Child (Edges : Edge_Array; Length : Natural; Child : Process_Word) return Natural is
   begin
      for I in 1 .. Length loop
         pragma Loop_Invariant (for all K in 1 .. I - 1 => Edges (K).Child /= Child);
         if Edges (I).Child = Child then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Child;

   function Supervisor_Of (Edges : Edge_Array; Length : Natural; Child : Process_Word) return Process_Word is
      Index : constant Natural := Find_Child (Edges, Length, Child);
   begin
      return (if Index = 0 then No_Process else Edges (Index).Supervisor);
   end Supervisor_Of;

   procedure Add
     (Edges : in out Edge_Array; Length : in out Natural; Next_Rank : in out Unsigned_64;
      Child, Supervisor : Process_Word; Restart : CS.Restart_Kind)
   is
      Old_Length : constant Natural := Length with Ghost;
   begin
      Length := Length + 1;
      Edges (Length) := (Child => Child, Supervisor => Supervisor, Rank => Next_Rank,
                         Restart => Restart, Reaping => False);
      Next_Rank := Next_Rank + 1;
      pragma Assert (for all I in 1 .. Old_Length => Edges (I).Child /= Child);
      pragma Assert (for all I in 1 .. Old_Length => Edges (I).Supervisor /= Child);
      pragma Assert (for all I in 1 .. Length => Edge_Valid (Edges (I), Next_Rank));
      pragma Assert (One_Supervisor (Edges, Length));
      pragma Assert (Ordered (Edges, Length));
   end Add;

   procedure Remove
     (Edges : in out Edge_Array; Length : in out Natural; Next_Rank : Unsigned_64; Index : Positive)
   is
      Old : constant Edge_Array := Edges with Ghost;
      Old_Length : constant Natural := Length with Ghost;
   begin
      Edges (Index) := Edges (Length);
      Length := Length - 1;
      --  Every remaining edge is an old edge, and distinct positions are
      --  distinct old positions.
      pragma Assert (for all I in 1 .. Length =>
                       Edges (I) = (if I = Index then Old (Old_Length) else Old (I)));
      pragma Assert (for all I in 1 .. Length => Edge_Valid (Edges (I), Next_Rank));
      pragma Assert (One_Supervisor (Edges, Length));
      pragma Assert (Ordered (Edges, Length));
   end Remove;

   procedure Mark_Reaping
     (Edges : in out Edge_Array; Length : Natural; Next_Rank : Unsigned_64; Index : Positive)
   is
      pragma Unreferenced (Next_Rank);   --  only the contracts name it
      Before : constant Edge_Array (Edges'Range) := Edges with Ghost;
   begin
      Edges (Index).Reaping := True;
      --  Only the flag changed: children, supervisors and ranks are as
      --  they were (stated so the order's double quantifier carries over).
      pragma Assert
        (for all I in 1 .. Length =>
           Edges (I).Child = Before (I).Child and then Edges (I).Supervisor = Before (I).Supervisor
           and then Edges (I).Rank = Before (I).Rank);
      pragma Assert (Ordered (Before, Length));
      pragma Assert (Ordered (Edges, Length));
   end Mark_Reaping;

   procedure Decide
     (Kind : CS.Restart_Kind; Intensity : CS.Intensity_Count; Window : CS.Window_Ms;
      Abnormal, Reaped : Boolean; Now : Unsigned_64;
      History : in out Restart_History; Decision : out Restart_Decision)
   is
   begin
      Decision := Stay_Ended;
      if Reaped or else Kind = CS.Temporary or else (Kind = CS.Transient and then not Abnormal) then
         return;
      end if;
      --  A new window when the last one has passed (or the clock went back).
      if Now < History.Window_Start
        or else Now - History.Window_Start >= Unsigned_64 (Window)
      then
         History := (Window_Start => Now, Restarts => 0);
      end if;
      if History.Restarts >= Natural (Intensity) then
         Decision := Escalate;
         return;
      end if;
      History.Restarts := History.Restarts + 1;
      Decision := Restart;
   end Decide;
end Supervision_Edges;
