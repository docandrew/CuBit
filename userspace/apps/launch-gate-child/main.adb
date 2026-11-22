--  launch-gate (CAP-012): a child launched into the user session reports
--  what it was given: the clock (in the envelope), not clock control or the
--  framebuffer (outside it). It then stays, so its requester can observe,
--  debug and stop it through its launch handle (CuBit.Launch_Handles): it
--  ends at a Stop, or by itself after Linger_Ms.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Control_Events;
with CuBit.Handles;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Process_Events;
with CuBit.Waits;
with Startup;

procedure Main is
   use type CuBit.Control_Events.Event_Kind, CuBit.Control_Events.Control_Kind;
   Within : constant Boolean := CuBit.Handles.Valid (Startup.Clock);
   Beyond : constant Boolean := Startup.Clock_Control.Present or else Startup.Framebuffer.Present;
   --  How long it waits for its requester's Stop before it ends anyway.
   Linger_Ms : constant := 30_000;
   Poll_Ms   : constant := 5;
   Until_Time : constant Unsigned_64 := CuBit.Monotonic.Milliseconds + Linger_Ms;
   Item : CuBit.Control_Events.Event;
   Found : Boolean;
begin
   debugPrint ("launch-gate-child: the clock given "
               & (if Within then "PASS" else "FAIL") & ASCII.LF);
   debugPrint ("launch-gate-child: clock control and the framebuffer withheld "
               & (if Beyond then "FAIL" else "PASS") & ASCII.LF);
   loop
      CuBit.Process_Events.Next (Item, Found);
      if Found and then Item.Kind = CuBit.Control_Events.Control
        and then Item.Control = CuBit.Control_Events.Stop
      then
         debugPrint ("launch-gate-child: stopped through the launch handle" & ASCII.LF);
         return;
      end if;
      exit when CuBit.Monotonic.Milliseconds >= Until_Time;
      CuBit.Waits.Sleep_For (Poll_Ms);
   end loop;
   debugPrint ("launch-gate-child: no Stop within its linger time" & ASCII.LF);
end Main;
