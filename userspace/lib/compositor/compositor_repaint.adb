package body Compositor_Repaint with SPARK_Mode is
   function Full (Extent : D.Box) return D.State
     with Pre => D.Valid (Extent), Post => Queue_Valid (Full'Result, Extent) and
       D.Covers (Full'Result, Extent)
   is
      Q : D.State;
   begin
      D.Clear (Q);
      D.Add (Q, Extent);
      return Q;
   end Full;
   function Open (Extent : D.Box) return State is
     ((Extent, (others => Full (Extent))));
   procedure Invalidate (S : in out State; Area : D.Box) is
      Before : constant Queue_Array := S.Queues;
   begin
      for B in Slot loop
         D.Add (S.Queues (B), Area);
         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant
           (for all J in Slot =>
              (if J <= B then D.Covers (S.Queues (J), Area)
               else S.Queues (J) = Before (J)));
         pragma Loop_Invariant
           (for all J in Slot =>
              (for all I in 1 .. D.Count (Before (J)) =>
                 D.Covers (S.Queues (J), D.Item (Before (J), I))));
      end loop;
   end Invalidate;
   procedure Take (S : in out State; B : Slot; Areas : out D.State) is
   begin
      Areas := S.Queues (B);
      D.Clear (S.Queues (B));
   end Take;
   procedure Failed_Render (S : in out State; B : Slot) is
   begin
      S.Queues (B) := Full (S.Extent);
   end Failed_Render;
end Compositor_Repaint;
