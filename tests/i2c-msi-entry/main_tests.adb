pragma Ada_2022;
with Ada.Text_IO; with Main; with Fixture; with Startup; with CuBit.Driver_Readiness; with Interfaces;
procedure Main_Tests is
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Context : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Context & Fixture.Scenario'Image; end if; end Check;
begin
   for Kind in 1 .. 23 loop
      Fixture.Reset (Kind);
      Startup.Controller.Present := Kind not in 7 | 10;
      Startup.Registration.Present := False;
      begin
         Main;
         Check (Kind not in 4 | 13 .. 15, "retained execution returned");
      exception
         when Fixture.Done => Check (Kind in 4 | 13 .. 15, "unexpected retention");
      end;
      Check (Fixture.Ready_Calls = 1, "one readiness report");
      if Kind in 9 .. 12 then
         Check (Fixture.Ready_Label = CuBit.Driver_Readiness.Not_Present_Label, "binding refused");
         Check (Fixture.Binds = 0 and Fixture.Prepares = 0 and Fixture.Waits = 0, "invalid binding has no hardware work");
      elsif Kind in 13 .. 14 then
         Check (Fixture.Ready_Label = CuBit.Driver_Readiness.Not_Present_Label, "startup refused");
         Check (Fixture.Prepares = (if Kind = 13 then 1 else 0), "startup clock before prepare");
         Check (Fixture.Health_Sleeps = 1 and Fixture.Waits = 0, "retained startup only supervises");
      else
         Check (Fixture.Ready_Label = CuBit.Driver_Readiness.Ready_Label, "ready after mask");
         if Kind = 7 then
            Check (Fixture.Prepares = 0 and Fixture.Waits = 0 and Fixture.Poll_Sleeps = 1, "explicit polling fallback");
         elsif Kind = 16 then
            Check (Fixture.Idle_Sleeps = 1 and Fixture.Waits = 0, "idle uses channel wait");
         elsif Kind = 15 then
            Check (Fixture.Arms = 1 and Fixture.Waits = 0 and Fixture.Rejects = 1, "arm failure retains without wait");
         else
            Check (Fixture.Waits = 1 and Fixture.Arms = 1 and Fixture.Deadline_Seen = 1000, "finite armed wait");
            Check (Fixture.Poll_Sleeps = 0 and Fixture.Idle_Sleeps = 0, "MSI active wait has no poll yield");
         end if;
         if Kind in 4 | 15 then
            Check (Fixture.Retained and Fixture.Payload and Fixture.Rejects = 1 and Fixture.Health_Sleeps = 1, "wait failure retains payload");
         else
            Check (Fixture.Stopped and not Fixture.Payload and Fixture.Masked, "drained stop");
         end if;
         if Kind in 3 | 18 then
            Check (Fixture.Open_Calls = 1 and Fixture.Reply_Calls = 1 and Fixture.Reply_Label = 16#F001#, "IPC serviced after wake");
         end if;
         if Kind in 1 | 6 | 17 | 19 .. 23 then Check (Fixture.Event_Calls = 0, "IRQ or peer event not forwarded as lifecycle"); end if;
         if Kind = 19 then Check (Fixture.Polled = 16, "event batch bounded at eight per turn"); end if;
         if Kind = 5 then Check (Fixture.Event_Calls = 1 and Fixture.Stop_Pending, "Stop dispatched while armed"); end if;
         if Kind = 8 then Check (Fixture.Turns = 3, "software progress before hardware wait"); end if;
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("I2C native Main checks " & Checks'Image);
end Main_Tests;
