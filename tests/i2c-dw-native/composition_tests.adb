with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with System.Storage_Elements;
with CuBit.Messages;
with CuBit.Handles.IPC;
with CuBit.Handles.Devices;
with I2C_Native_Controller;
with I2C_Mapping_Policy;
procedure Composition_Tests is
   package N is new I2C_Native_Controller (Capacity => 64);
   package C renames N.Controller;
   package W renames N.Registers;
   package P renames I2C_Mapping_Policy;
   package M renames CuBit.Messages;
   package I renames CuBit.Handles.IPC;
   package D renames CuBit.Handles.Devices;
   use type C.Admission;
   use type C.Phase;
   use type Interfaces.Unsigned_64;
   use type P.Address;
   type Page_Array is array (Natural range 0 .. 1_023) of Interfaces.Unsigned_32;
   Page : aliased Page_Array := [others => 16#A5A5_A5A5#] with Alignment => 4096;
   Virtual : P.Address := P.Address (System.Storage_Elements.To_Integer (Page'Address));
   Physical : constant := 16#1234_5000#;
   Facts : C.Startup := (Vendor => 16#8086#, Device => 16#54E9#,
     BAR_Base => Physical, Window_Bytes => 4096,
     Reset_Budget => 64, Configuration_Budget => 64, Owner_Budget => 64);
   State : C.State;
   Mapping : W.Report;
   Admission : C.Admission;
   Started : Boolean;
   Checks : Natural := 0;
   Which : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Check (OK : Boolean; Why : String) is
   begin
      if not OK then raise Program_Error with Why; end if;
      Checks := Checks + 1;
   end Check;
begin
   M.Sample := 10;
   I.Metadata := (Current => True, Kind => CuBit.Handles.ABI.Device_Memory,
     Held => (True, True), Object => Physical, Parameter => 4096);
   D.Expected_Physical := Physical; D.Expected_Virtual := Interfaces.Unsigned_64 (Virtual);
   case Which is
      when 0 => null;
      when 1 => Facts.Device := 0;
      when 2 => M.Sample := Interfaces.Unsigned_64'Last;
      when 3 => M.Sample := 0;
      when 4 => I.Metadata := (Current => False);
      when 5 => D.Allow_Map := False;
      when 6 => I.Metadata.Held.Write := False;
      when 7 => Facts.BAR_Base := Physical + 4096;
      when 8 => I.Metadata.Kind := CuBit.Handles.ABI.No_Capability;
      when 9 => Virtual := Virtual + 1;
      when 10 => I.Metadata.Parameter := 4095;
      when others => raise Program_Error with "unknown case";
   end case;
   N.Initialize (1, Facts, Virtual, State, Mapping, Admission, Started);
   Check (Page = Page_Array'[others => 16#A5A5_A5A5#], "initialization maps without MMIO");
   Check (M.Calls = 0, "work-bounded admission never samples GETTIME");
   if Which in 0 | 2 | 3 then
      Check (Started and Admission = C.Accepted and W.Ready and
        C.Observe (State).At_Phase = C.Resetting, "mapping starts bounded owner only");
      Check (I.Calls = 1 and D.Calls = 1, "exactly one inspect and map");
      if Which = 0 then
         declare
            package Other is new I2C_Native_Controller (Capacity => 32);
            use type Other.Controller.Admission;
            use type Other.Controller.Phase;
            use type Other.Registers.Outcome;
            Other_State : Other.Controller.State;
            Other_Facts : constant Other.Controller.Startup :=
              (Facts.Vendor, Facts.Device, Physical + 4096, Facts.Window_Bytes,
               Facts.Reset_Budget, Facts.Configuration_Budget,
               Other.Controller.Work_Limit (Facts.Owner_Budget));
            Other_Mapping : Other.Registers.Report;
            Other_Admission : Other.Controller.Admission;
            Other_Started : Boolean;
         begin
            Other.Initialize (1, Other_Facts, Virtual, Other_State, Other_Mapping,
               Other_Admission, Other_Started);
            Check (not Other_Started and Other_Admission = Other.Controller.Invalid_Resource
               and Other.Controller.Observe (Other_State).At_Phase = Other.Controller.Cold,
               "second generic instance cannot borrow an admitted mapping");
            Check (Other_Mapping.Status = Other.Registers.Already_Attempted
               and I.Calls = 1 and D.Calls = 1 and M.Calls = 0,
               "second instance performs no inspect/map/clock operations");
         end;
      end if;
   else
      Check (not Started and not W.Ready and C.Observe (State).At_Phase = C.Cold,
        "refused facts/authority cannot start controller");
      Check (I.Calls = (if Which = 1 then 0 else 1) and then
             D.Calls = (if Which = 5 then 1 else 0), "policy checks precede mapping");
      Check (Admission = C.Invalid_Resource, "typed refusal preserved");
   end if;
   Check (Page = Page_Array'[others => 16#A5A5_A5A5#], "all admission paths preserve register page");
   declare
      Before_Inspect : constant Natural := I.Calls;
      Before_Map : constant Natural := D.Calls;
   begin
      N.Initialize (1, Facts, Virtual, State, Mapping, Admission, Started);
      Check (not Started and Admission = C.Busy and M.Calls = 0 and
        I.Calls = Before_Inspect and D.Calls = Before_Map,
        "one process-lifetime initialization attempt, without retry");
   end;
   Ada.Text_IO.Put_Line ("composition case" & Which'Image & " checks" & Checks'Image);
end Composition_Tests;
