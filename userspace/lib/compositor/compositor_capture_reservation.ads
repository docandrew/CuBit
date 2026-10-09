with Compositor_Pool;
with Compositor_Target_Damage;
-- Reserve before scene capture so its commands cover this target's history.
-- No GPU calls occur here. Cancellation is legal only before Start_Render.
package Compositor_Capture_Reservation with SPARK_Mode, Pure is
   package P renames Compositor_Pool;
   package D renames Compositor_Target_Damage;
   use type P.Ticket, P.State, D.State, P.Slot, D.D.State;
   procedure Reserve
     (Pool : in out P.State; Damage : in out D.State; Ticket : out P.Ticket; Replace_Ready : Boolean := False)
   with Pre => P.Valid (Pool) and D.Valid (Damage) and
      not D.Faulted (Damage) and D.Active (Damage) = 0,
     Post => P.Valid (Pool) and D.Valid (Damage) and
       (if Ticket = P.None then Damage = Damage'Old
        else P.Writable (Pool, Ticket) and
          D.Active (Damage) = Ticket.Buffer and not D.Faulted (Damage) and
          D.Painting (Damage) = D.Pending (Damage'Old, Ticket.Buffer)) and
       (for all I in P.Live_Slot =>
          D.Pending (Damage, I) = D.Pending (Damage'Old, I));
   -- Snapshot chosen before command capture; not the damage of another slot.
   procedure Repaint (Pool : P.State; Damage : D.State; Ticket : P.Ticket;
      Plan : out D.D.State; Accepted : out Boolean)
   with Pre => P.Valid (Pool) and D.Valid (Damage),
     Post => D.D.Valid (Plan) and
       Accepted = (P.Writable (Pool, Ticket) and not D.Faulted (Damage) and
         D.Active (Damage) = Ticket.Buffer) and
       (if Accepted then Plan = D.Painting (Damage) else D.D.Count (Plan) = 0);
   procedure Cancel
     (Pool : in out P.State; Damage : in out D.State;
      Ticket : P.Ticket; Accepted : out Boolean)
   with Pre => P.Valid (Pool) and D.Valid (Damage),
     Post => P.Valid (Pool) and D.Valid (Damage) and
       Accepted = (P.Writable (Pool'Old, Ticket) and
         not D.Faulted (Damage'Old) and D.Active (Damage'Old) = Ticket.Buffer) and
       (if Accepted then P.Writer (Pool) = P.None and D.Active (Damage) = 0
        else Pool = Pool'Old and Damage = Damage'Old) and
       P.Front (Pool) = P.Front (Pool'Old) and
       P.Readback (Pool) = P.Readback (Pool'Old) and
       (for all I in P.Live_Slot =>
          D.Pending (Damage, I) = D.Pending (Damage'Old, I));
end Compositor_Capture_Reservation;
