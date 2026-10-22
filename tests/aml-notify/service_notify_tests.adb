with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Service_Notify_Tests is
   procedure No_Delay (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin Result := AML_Delays.Unavailable; end No_Delay;
   package Core is new ACPI_Service_Core
     (Perform_Delay => No_Delay, Notification_Capacity => 2);
   use Core;
   use type Firmware_Tables.Byte;
   use type Firmware_Tables.Bytes;
   use type Values.Access_Status;
   use type AML_Decode.Integer_Value;
   use type Namespace.Notification_Class;
   use type Namespace.Lookup_Status;
   use type AML_Execute.Execution_Status;
   subtype Bytes is Firmware_Tables.Bytes;
   type Service_Access is access all State;
   S : constant Service_Access := new State (2, 4096, 4096);
   Other : constant Service_Access := new State (2, 4096, 4096);
   Checks : Natural := 0;
   Status : Values.Access_Status;
   Loaded : Install_Status;
   R : AML_Execute.Execution_Result;
   Event : Notification_Observation;
   Has_Event : Boolean;
   function Enc (Text : String) return Bytes is
      B : Bytes (1 .. Text'Length);
   begin
      for I in B'Range loop B (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return B;
   end Enc;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Bytes'[16#14#, Firmware_Tables.Byte (6 + Code'Length)] & Enc (Name) & [0] & Code);
   Notify_Code : constant Bytes := [16#86#] & Enc ("DEV0") & [16#0A#, 16#80#];
   Code : constant Bytes := [16#5B#, 16#82#, 5] & Enc ("DEV0") &
     Method ("ONCE", Notify_Code & [16#A4#, 1]) &
     Method ("WIDE", [16#86#] & Enc ("DEV0") & [16#0C#, 0, 0, 1, 0, 16#A4#, 1]) &
     Method ("WRAP", [16#86#] & Enc ("DEV0") & [16#0E#, 0, 0, 0, 0, 1, 0, 0, 0, 16#A4#, 1]) &
     Method ("FULL", Notify_Code & Notify_Code & Notify_Code & [16#A4#, 1]) &
     Method ("FAIL", Notify_Code & [16#A4#] & Enc ("MISS"));
   function Table return Bytes is
      B : Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Firmware_Tables.Byte := 0;
   begin
      B (1 .. 4) := Enc ("DSDT"); B (5) := Firmware_Tables.Byte (B'Length mod 256);
      B (6) := Firmware_Tables.Byte (B'Length / 256); B (9) := 2; B (37 .. B'Last) := Code;
      for Item of B loop Sum := Sum + Item; end loop;
      B (10) := 0 - Sum; return B;
   end Table;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   procedure Load_And_Seal (Target : in out State) is
      Report : Namespace.Initialization_Report;
   begin
      Install (Target, 1, DSDT, Table, Loaded); Check (Loaded = Installed, "install");
      Initialize_Members (Target, Report, Status); Check (Status = Values.Available, "seal");
   end Load_And_Seal;
   procedure Call (Name : String) is
      Located : Namespace.Lookup_Result;
   begin
      Find_Object (S.all, Namespace.Root, Name, Located, Status);
      Check (Status = Values.Available and then Located.Status = Namespace.Found, "method lookup");
      Invoke_Scalar (S.all, Located.Node, [others => 0], 0, 100, R);
   end Call;
begin
   Configure_Notifications (S.all, True, Status);
   Check (Status = Values.Wrong_Phase and then Pending_Notifications (S.all) = 0, "uninitialized configuration");
   Load_And_Seal (S.all); Load_And_Seal (Other.all);
   Call ("ONCE");
   Check (R.Status = AML_Execute.Returned and then Pending_Notifications (S.all) = 0, "disabled observer");
   Configure_Notifications (S.all, True, Status); Check (Status = Values.Available, "enable");
   Call ("ONCE");
   Check (R.Status = AML_Execute.Returned and then Pending_Notifications (S.all) = 1, "admission");
   Check (Pending_Notifications (Other.all) = 0, "service isolation");
   Configure_Notifications (S.all, False, Status);
   Check (Status = Values.Available and then Pending_Notifications (S.all) = 1, "disable preserves admitted event");
   Take_Notification (S.all, Event, Has_Event, Status);
   Check (Status = Values.Available and then Has_Event and then Pending_Notifications (S.all) = 0, "post-invocation drain");
   Check (Event.Number = 16#80# and then Event.Class = Namespace.Device_Notify
     and then Event.Current_Target and then Event.Path_Length = 1 and then Event.Path (1) = "DEV0",
     "copied target and notification description");
   Take_Notification (S.all, Event, Has_Event, Status);
   Check (Status = Values.Available and then not Has_Event, "empty drain");
   Configure_Notifications (S.all, True, Status); Check (Status = Values.Available, "reenable");
   Call ("WIDE"); Check (R.Status = AML_Execute.Returned, "wide notification call");
   Call ("WRAP"); Check (R.Status = AML_Execute.Returned, "wrapped notification call");
   Take_Notification (S.all, Event, Has_Event, Status);
   Check (Status = Values.Available and then Has_Event and then Event.Number = 0
     and then Event.Class = Namespace.Device_Notify, "class precedes value truncation");
   Take_Notification (S.all, Event, Has_Event, Status);
   Check (Status = Values.Available and then Has_Event and then Event.Number = 0
     and then Event.Class = Namespace.System_Notify, "32-bit class conversion");
   Call ("FAIL");
   Check (R.Status = AML_Execute.Unknown_Name and then Pending_Notifications (S.all) = 1, "later failure preserves admitted effect");
   Take_Notification (S.all, Event, Has_Event, Status); Check (Status = Values.Available and then Has_Event, "drain failed method event");
   Call ("FULL");
   Check (R.Status = AML_Execute.Notify_Limit and then Pending_Notifications (S.all) = 2, "full queue reports admission failure");
   for I in 1 .. 2 loop
      Take_Notification (S.all, Event, Has_Event, Status); Check (Status = Values.Available and then Has_Event, "full queue retains earlier events");
   end loop;
   Check (Pending_Notifications (S.all) = 0 and then Retained_Results (S.all) = 0, "drain leaves no retained values");
   Ada.Text_IO.Put_Line ("Service Notify checks" & Natural'Image (Checks));
end Service_Notify_Tests;
