package body Compositor_Capture_Reservation with SPARK_Mode is
   procedure Reserve
     (Pool : in out P.State; Damage : in out D.State; Ticket : out P.Ticket; Replace_Ready : Boolean := False) is
   begin
      P.Acquire (Pool, Ticket, Replace_Ready);
      if Ticket /= P.None then
         D.Begin_Paint (Damage, Ticket.Buffer);
      end if;
   end Reserve;
   procedure Repaint (Pool : P.State; Damage : D.State; Ticket : P.Ticket;
      Plan : out D.D.State; Accepted : out Boolean) is
   begin
      D.D.Clear (Plan);
      Accepted := P.Writable (Pool, Ticket) and not D.Faulted (Damage) and
        D.Active (Damage) = Ticket.Buffer;
      if Accepted then Plan := D.Painting (Damage); end if;
   end Repaint;
   procedure Cancel
     (Pool : in out P.State; Damage : in out D.State;
      Ticket : P.Ticket; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not P.Writable (Pool, Ticket) or else D.Faulted (Damage) or else
        D.Active (Damage) /= Ticket.Buffer then return; end if;
      D.Finish (Damage, D.Cancelled);
      P.Abandon_Writer (Pool, Ticket, Accepted);
   end Cancel;
end Compositor_Capture_Reservation;
