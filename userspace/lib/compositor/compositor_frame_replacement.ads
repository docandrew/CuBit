with Compositor_Presentation;
with Compositor_Pool;
with Compositor_Damage;
-- Replace only a completed, never-published frame, at its next retry deadline.
-- No pixel operations: caller renders current scene into the existing writer.
package Compositor_Frame_Replacement with SPARK_Mode, Pure is
   package CP renames Compositor_Presentation;
   package BP renames Compositor_Pool;
   package D renames Compositor_Damage;
   use type CP.State, CP.ID, BP.State, BP.Ticket, D.State;
   function Eligible
     (Transfer : CP.State; Pool : BP.State; Pending, Frame : D.State; Now : CP.ID)
      return Boolean is
     (CP.Can_Attempt (Transfer, Now) and then D.Count (Pending) > 0 and then
      D.Count (Frame) > 0 and then BP.Displayed (Pool) /= BP.None and then
      BP.Writable (Pool, BP.Writer (Pool)));
   procedure Replace
     (Transfer : in out CP.State; Pool : in out BP.State;
      Pending, Frame : in out D.State; Now : CP.ID; Replaced : out Boolean)
     with Pre => BP.Valid (Pool) and D.Valid (Pending) and D.Valid (Frame),
       Post => BP.Valid (Pool) and D.Valid (Pending) and D.Valid (Frame) and
         Replaced = Eligible (Transfer'Old, Pool'Old, Pending'Old, Frame'Old, Now) and
         (if Replaced then CP.Writable (Transfer) and
            CP.Token (Transfer) = CP.Token (Transfer'Old) and
            CP.Session (Transfer) = CP.Session (Transfer'Old) and
            not BP.Faulted (Pool) and BP.Displayed (Pool) = BP.None and
            BP.Writer (Pool) = BP.Writer (Pool'Old) and
            BP.Front (Pool) = BP.Front (Pool'Old) and
            BP.Readback (Pool) = BP.Readback (Pool'Old) and
            BP.Writable (Pool, BP.Writer (Pool)) and D.Count (Frame) = 0 and
            (for all I in 1 .. D.Count (Pending'Old) =>
               D.Covers (Pending, D.Item (Pending'Old, I))) and
            (for all I in 1 .. D.Count (Frame'Old) =>
               D.Covers (Pending, D.Item (Frame'Old, I)))
          else Transfer = Transfer'Old and Pool = Pool'Old and
            Pending = Pending'Old and Frame = Frame'Old);
end Compositor_Frame_Replacement;
