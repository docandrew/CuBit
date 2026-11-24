------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TEST STAND-IN for acpi.svc's power sources (PWR-004, tests/headless
--  power-sources): its AC and battery observer pages, their identities,
--  its power-source watch and its demand reads. Never part of a production
--  image.
--
--  @description
--  It registers the ACPI role, then plays acpi.svc on a scripted machine:
--    1. Silent_Ms: it receives nothing (acpi.svc's AML startup); requests
--       wait in its endpoint;
--    2. until Ready_At_Ms: every page answers Not_Ready and the watch
--       Unavailable (acpi.svc before its local runtime);
--    3. at Ready_At_Ms the first publication: an AC adapter offline and one
--       battery discharging, 36500 of 50000 mWh at 10000 mW (73%, 3 h 39
--       min left); the watch moves on;
--    4. Plug_After_Ms later, a Notify on the adapter and the battery: the
--       adapter online, the battery charging, 37000 of 50000 mWh at 12500
--       mW (74%, 1 h 2 min to full); the watch moves on. It says how long
--       no page was read before it (power.svc reads only when told);
--    5. a battery inventory read finding the sample older than its age,
--       at most once a second, is kept: the battery is sampled again
--       (Sample_Ms; it has charged Sample_Step mWh more), the watch moves
--       on, and the kept read is answered fresh.
--  The pages are answered by acpi.svc's own codecs (Power_ACPI_Fixture),
--  the paths chunked by its own ACPI_Source_Paths, the watch kept by its
--  own policy (ACPI_Power_Inventory_Watches, through the same adapter as
--  ACPI_Native_Power_Inventory). The demand read is a MODEL of
--  ACPI_Native_Battery_Reads: kept on Too_Old, coalesced, one sample,
--  never retried. Any other request is refused (16#F001#, Denied).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Console_Route;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Waits;
with ACPI_Protocol;
with ACPI_Power_Observation_Protocol;
with ACPI_Battery_Observation_Protocol;
with ACPI_Power_Identity_Protocol;
with ACPI_Power_Watch_Protocol;
with ACPI_Power_Inventory_Watches;
with Power_ACPI_Fixture;
with Startup;

procedure Main is
   use ASCII;
   package FX renames Power_ACPI_Fixture;
   package WP renames ACPI_Power_Watch_Protocol;
   package H renames CuBit.Handles;
   package IPC renames CuBit.Handles.IPC;
   use type ACPI_Protocol.Role_Tag, H.Reply;

   Silent_Ms     : constant := 1_500;
   Ready_At_Ms   : constant := 4_000;
   Plug_After_Ms : constant := 5_000;
   Sample_Ms     : constant := 200;
   --  acpi.svc's minimum demand interval (ACPI_Demand_Refresh), in ms.
   Demand_Interval_Ms : constant := 1_000;
   Sample_Step : constant := 100;
   Microseconds_Per_Millisecond : constant := 1_000;

   Read_Metrics : constant Unsigned_32 := 0;
   Request_Words : constant := 4;
   Reply_Error : constant Unsigned_32 := 16#F001#;
   Denied_Outcome : constant Unsigned_64 := 1;
   Inventory_Selector : constant Unsigned_64 := 0;

   type Phase is (Starting, Ready, Plugged);
   Now_Phase : Phase := Starting;

   Starting_Machine : constant FX.Scenario :=
     (AC => FX.Not_Ready, Batteries_Ready => False, Count => 0, others => <>);
   Discharging : FX.Scenario :=
     (AC => FX.Offline, Batteries_Ready => True, Count => 1,
      Batteries => [1 => (Kind => FX.Sampled, Unit => FX.Energy, Design => 57_000, Last_Full => 50_000,
                          Design_Voltage => 11_550, Direction => FX.Discharging, Critical => False,
                          Charge_Limiting => False, Rate => 10_000, Remaining => 36_500, Voltage => 12_100),
                    others => <>],
      AC_Age => 0, Battery_Age => 0, Generation => 1, AC_Generation => 1, others => <>);

   --  The watch: acpi.svc's own policy over the kernel's saved replies.
   procedure Save_Current (Token : out H.Reply; Saved : out Boolean) is
      R : constant IPC.Saved_Reply := IPC.Save_Reply;
   begin
      Saved := R.Saved;
      Token := (if R.Saved then R.Handle else H.No_Reply);
   end Save_Current;
   function Encode (Id : Unsigned_64; Status : WP.Status; Generation : Unsigned_64) return Message is
      M : Message := NULL_MESSAGE;
   begin
      M.tag := (label => WP.Watch, length => Request_Words, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := Id;
      M.words (1) := Unsigned_64 (WP.Status'Enum_Rep (Status));
      M.words (2) := Generation;
      M.words (3) := WP.Schema;
      return M;
   end Encode;
   procedure Answer_Saved (Token : in out H.Reply; Id : Unsigned_64; Status : WP.Status;
                           Generation : Unsigned_64; Delivered : out Boolean) is
   begin
      Delivered := IPC.Answer (Token, Encode (Id, Status, Generation));
      Token := H.No_Reply;
   end Answer_Saved;
   package Watches is new ACPI_Power_Inventory_Watches
     (H.Reply, H.No_Reply, Save_Current => Save_Current, Answer_Saved => Answer_Saved);
   Inventory : Watches.State;

   --  Kept demand reads.
   Maximum_Kept : constant := 8;
   type Kept is record
      Token : H.Reply := H.No_Reply;
      Words : FX.Words := [others => 0];
   end record;
   Kept_Reads : array (1 .. Maximum_Kept) of Kept;
   Sample_Due : Unsigned_64 := 0;
   Last_Demand : Unsigned_64 := 0;
   Demands : Natural := 0;

   T0, AC_At, Battery_At, Last_Read : Unsigned_64 := 0;

   From : Process_ID;
   Request : Message;
   Found : Boolean;
   Activity : Activity_Result;
   Ignore : Unsigned_64;

   function Image (V : Unsigned_64) return String is
      T : constant String := Unsigned_64'Image (V);
   begin
      return T (T'First + 1 .. T'Last);
   end Image;

   --  The fixture's sample ages, as of Now.
   procedure Age (Now : Unsigned_64) is
      S : FX.Scenario := FX.Current;
   begin
      if Now_Phase /= Starting then
         S.AC_Age := (Now - AC_At) * Microseconds_Per_Millisecond;
         S.Battery_Age := (Now - Battery_At) * Microseconds_Per_Millisecond;
         FX.Set (S);
      end if;
   end Age;

   function Answer_Of (Words : FX.Words) return Message is
      Label : Unsigned_32;
      Answered : FX.Words;
      Handled : Boolean;
      M : Message := NULL_MESSAGE;
   begin
      FX.Answer (Words, Label, Answered, Handled);
      M.tag := (label => Label, length => Request_Words, protocol => 0, ipc => <>, reserved => 0);
      for I in Answered'Range loop
         M.words (I) := Answered (I);
      end loop;
      return M;
   end Answer_Of;

   function Advance_Watches return Boolean is
      Result : Watches.Delivery;
      use type Watches.Delivery;
   begin
      Watches.Advance (Inventory, Now_Phase /= Starting, Result);
      return Result /= Watches.No_Attempt;
   end Advance_Watches;

   procedure Publish (Why : String) is
   begin
      Watches.Changed (Inventory);
      while Advance_Watches loop
         null;
      end loop;
      debugPrint ("power-standin: " & Why & LF);
   end Publish;

   procedure Step (Now : Unsigned_64) is
      S : FX.Scenario;
   begin
      if Now_Phase = Starting and then Now >= T0 + Ready_At_Ms then
         Now_Phase := Ready;
         AC_At := Now;
         Battery_At := Now;
         FX.Set (Discharging);
         Publish ("ready: the first publication (an adapter offline, a battery discharging)");
      elsif Now_Phase = Ready and then Now >= T0 + Ready_At_Ms + Plug_After_Ms then
         Now_Phase := Plugged;
         S := FX.Current;
         S.AC := FX.Online;
         S.Batteries (1).Direction := FX.Charging;
         S.Batteries (1).Remaining := 37_000;
         S.Batteries (1).Rate := 12_500;
         S.Generation := S.Generation + 1;
         S.AC_Generation := S.AC_Generation + 1;
         AC_At := Now;
         Battery_At := Now;
         FX.Set (S);
         Publish ("the adapter is plugged in (Notify ADP1 and BAT0, 16#80#); no page was read for " &
                  Image (Now - Last_Read) & " ms before it");
      end if;
      if Sample_Due /= 0 and then Now >= Sample_Due then
         Sample_Due := 0;
         S := FX.Current;
         S.Batteries (1).Remaining := S.Batteries (1).Remaining + Sample_Step;
         S.Generation := S.Generation + 1;
         Battery_At := Now;
         FX.Set (S);
         Age (Now);
         for K of Kept_Reads loop
            if K.Token /= H.No_Reply then
               declare
                  Delivered : constant Boolean := IPC.Answer (K.Token, Answer_Of (K.Words));
                  pragma Unreferenced (Delivered);
               begin
                  K := (others => <>);
               end;
            end if;
         end loop;
         Publish ("demand read" & Natural'Image (Demands) & ": the battery sampled again (_BST), " &
                  Image (S.Batteries (1).Remaining) & " mWh");
      end if;
   end Step;

   function Next_Deadline return Unsigned_64 is
      D : Unsigned_64 :=
        (case Now_Phase is
           when Starting => T0 + Ready_At_Ms,
           when Ready    => T0 + Ready_At_Ms + Plug_After_Ms,
           when Plugged  => Wait_Forever);
   begin
      if Sample_Due /= 0 and then Sample_Due < D then
         D := Sample_Due;
      end if;
      return D;
   end Next_Deadline;

   --  A battery inventory read that found the sample too old: kept for one
   --  sample (True), or answered at once (rate limited, no room: False).
   function Keep_For_Demand (Now : Unsigned_64; Words : FX.Words) return Boolean is
      S : constant FX.Scenario := FX.Current;
   begin
      if Now_Phase = Starting or else Words (0) /= ACPI_Battery_Observation_Protocol.Battery_Page
        or else Words (2) /= Inventory_Selector or else S.Battery_Age <= Words (1)
      then
         return False;
      end if;
      if Sample_Due = 0 and then Last_Demand /= 0 and then Now - Last_Demand < Demand_Interval_Ms then
         return False;
      end if;
      for K of Kept_Reads loop
         if K.Token = H.No_Reply then
            declare
               Saved : constant IPC.Saved_Reply := IPC.Save_Reply;
            begin
               if not Saved.Saved then
                  return False;
               end if;
               K := (Token => Saved.Handle, Words => Words);
            end;
            if Sample_Due = 0 then
               Sample_Due := Now + Sample_Ms;
               Last_Demand := Now;
               Demands := Demands + 1;
            end if;
            return True;
         end if;
      end loop;
      return False;
   end Keep_For_Demand;

   procedure Serve (Now : Unsigned_64) is
      Words : FX.Words;
      Answer : Message := NULL_MESSAGE;
   begin
      if Request.tag.label = WP.Watch then
         declare
            R : Watches.Response;
            Deferred : Boolean;
            Previous : Watches.Delivery;
         begin
            Watches.Watch
              (Inventory,
               (Request.authorityTag, To_Word (From), Unsigned_64 (Request.tag.length),
                Request.words (0), Request.words (1), Request.words (2), Request.words (3),
                Unsigned_64 (Flag_Byte (Request.tag)), Unsigned_64 (Request.tag.reserved)),
               Now_Phase /= Starting, R, Deferred, Previous);
            if not Deferred then
               Ignore := reply (From, Encode (R.ID, R.Status, R.Generation));
            end if;
         end;
         return;
      end if;
      if Request.tag.label /= Read_Metrics or else Request.tag.length /= Request_Words
        or else ACPI_Protocol.Role_Tag (Request.authorityTag) /= ACPI_Protocol.Observer_Tag
      then
         Answer.tag := (label => Reply_Error, length => Request_Words, protocol => 0, ipc => <>, reserved => 0);
         Answer.words (0) := Denied_Outcome;
         Ignore := reply (From, Answer);
         return;
      end if;
      for I in Words'Range loop
         Words (I) := Request.words (I);
      end loop;
      if Words (0) in ACPI_Power_Observation_Protocol.Source_Page | ACPI_Battery_Observation_Protocol.Battery_Page
                    | ACPI_Power_Identity_Protocol.Identity_Page
      then
         Last_Read := Now;
      end if;
      Age (Now);
      if Keep_For_Demand (Now, Words) then
         return;
      end if;
      Ignore := reply (From, Answer_Of (Words));
   end Serve;

   Now : Unsigned_64;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Discharging.AC_Path := FX.Path_Of ("\_SB_.ADP1");
   Discharging.Battery_Paths (1) := FX.Path_Of ("\_SB_.PCI0.LPCB.EC0_.BAT0");
   FX.Set (Starting_Machine);
   if registerDriver (DRIVER_ACPI, Startup.Registration) = Unsigned_64'Last then
      debugPrint ("power-standin: registration failed" & LF);
      return;
   end if;
   T0 := CuBit.Monotonic.Milliseconds;
   Last_Read := T0;
   debugPrint ("power-standin: registered (STAND-IN for acpi.svc's power sources); silent for" &
               Unsigned_64'Image (Silent_Ms) & " ms, ready at" & Unsigned_64'Image (Ready_At_Ms) & " ms" & LF);
   CuBit.Waits.Sleep_Until ((T0 + Silent_Ms) * Microseconds_Per_Millisecond);
   loop
      Activity := Wait_For_Activity_Until (Next_Deadline);
      exit when Activity = Unavailable;
      Now := CuBit.Monotonic.Milliseconds;
      Step (Now);
      loop
         Poll_Service_Request (From, Request, Found);
         exit when not Found;
         Serve (Now);
      end loop;
      while Advance_Watches loop
         null;
      end loop;
   end loop;
end Main;
