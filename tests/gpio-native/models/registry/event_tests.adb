pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with GPIO_Event_Policy;
with CuBit.Process_IDs;
with CuBit.Control_Events;
with Channel_Wake_ABI;
procedure Event_Tests is
   package E renames GPIO_Event_Policy;
   package P renames CuBit.Process_IDs;
   package C renames CuBit.Control_Events;
   use type E.Action, Interfaces.Unsigned_8, Interfaces.Unsigned_64;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin if not OK then raise Program_Error with Why; end if; Checks := Checks + 1; end Check;
   type Identities is array (Positive range <>) of P.Process_ID;
   Publishers : constant Identities :=
     [P.From_Word (16#0100_0001#), P.From_Word (16#FFFF_FFFF_0100_0001#), P.From_Word (Interfaces.Unsigned_64'Last)];
begin
   for Length in Interfaces.Unsigned_8 loop
      Check (E.Classify (P.No_Process, C.Control_Label, Length, 1, 16#FFFF_FFFF_0100_0001#, 0) =
         (if Length = C.Control_Words then E.Stop_After_Drain else E.Ignore), "kernel stop requires exact frame length");
      Check (E.Classify (P.No_Process, C.Grant_Revoked_Label, Length, 0, 0, 0) =
         (if Length = C.Grant_Event_Words then E.Disable_Logging else E.Ignore), "grant revocation requires exact frame length");
      Check (E.Classify (P.No_Process, C.Grant_Returned_Label, Length, 0, 0, 0) =
         (if Length = C.Grant_Event_Words then E.Disable_Logging else E.Ignore), "grant return requires exact frame length");
      Check (E.Classify (P.No_Process, Channel_Wake_ABI.Wake_Label, Length, 0, 0, 0) =
         (if Length in 1 .. Channel_Wake_ABI.Wakes_Per_Event then E.Route_Channel_Wake else E.Ignore), "only bounded channel notice is routed");
      for Publisher of Publishers loop
         Check (E.Classify (Publisher, C.Control_Label, Length, 1, 0, 0) = E.Ignore, "process publisher cannot trigger lifecycle action");
         Check (E.Classify (Publisher, C.Grant_Revoked_Label, Length, 0, 0, 0) = E.Ignore, "process publisher cannot end logging grants");
         Check (E.Classify (Publisher, Channel_Wake_ABI.Wake_Label, Length, 0, 0, 0) = E.Ignore, "process publisher cannot synthesize kernel channel notice");
      end loop;
   end loop;
   for Kind in Interfaces.Unsigned_64 range 0 .. 8 loop
      Check (E.Classify (P.No_Process, C.Control_Label, C.Control_Words, Kind, 0, 0) =
         (if Kind = 1 then E.Stop_After_Drain else E.Ignore), "only stop control drains the GPIO owner");
   end loop;
   Check (E.Classify (P.No_Process, 0, 0, 0, 0, 0) = E.Ignore, "unknown event does not synthesize IRQ work");
   Ada.Text_IO.Put_Line ("GPIO event policy checks" & Checks'Image);
end Event_Tests;
