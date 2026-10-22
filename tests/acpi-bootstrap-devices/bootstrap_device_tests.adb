pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Bootstrap; use ACPI_Bootstrap;
with ACPI_Service;
with ACPI_Device_Policy;
with Migration_Tables;
procedure Bootstrap_Device_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Firmware_Tables.Bytes;
   use type ACPI_Service.Install_Status;
   use type ACPI_Device_Policy.Region_Outcome;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Result;
   end Enc;
   function Pkg_Length (Value : Natural) return Bytes is
   begin
      if Value < 64 then return [Unsigned_8 (Value)]; end if;
      if Value < 4_096 then return [Unsigned_8 (16#40# + Value mod 16), Unsigned_8 (Value / 16)]; end if;
      if Value < 1_048_576 then
         return [Unsigned_8 (16#80# + Value mod 16), Unsigned_8 ((Value / 16) mod 256), Unsigned_8 (Value / 4_096)];
      end if;
      raise Program_Error with "fixture package too large";
   end Pkg_Length;
   function Block (Opcode, Payload : Bytes) return Bytes is
      Width : Positive := 1;
   begin
      loop
         declare
            Length : constant Bytes := Pkg_Length (Payload'Length + Width);
         begin
            if Length'Length = Width then return Opcode & Length & Payload; end if;
            Width := Length'Length;
         end;
      end loop;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));

   -- Root sets MARK; system-bus initializer fails if it runs out of order.
   -- Temporary package declaration and returned buffer exercise cleanup.
   Root_Code : constant Bytes := [16#70#,1] & Enc ("MARK") &
     [8] & Enc ("TEMP") & Block ([16#12#], [1,1]) &
     [16#A4#] & Block ([16#11#], [1,16#A5#]);
   Bus_Code : constant Bytes := Block ([16#A0#], [16#92#,16#93#] & Enc ("MARK") & [1] &
     [16#70#,1] & Enc ("MISS")) & [16#70#,16#0A#,2] & Enc ("MARK");
   Code : constant Bytes := [8] & Enc ("MARK") & [0] & Method ("_INI", Root_Code) &
     Block ([16#10#], Enc ("_SB_") & Method ("_INI", Bus_Code)) &
     Block ([16#5B#,16#82#], Enc ("DEV0") & Method ("_INI", [16#FE#]));
   Raw : constant Bytes := Migration_Tables.Make (Code);
   Module_Cost : Natural := 0;
   procedure Load (S : in out State) is
      Accepted : Boolean;
      Result : ACPI_Service.Install_Status;
   begin
      Start (S, 1, Accepted); Check (Accepted, "start");
      Import_Table (S, 1, ACPI_Service.DSDT, Raw, Result);
      Check (Result = ACPI_Service.Installed, "stage");
   end Load;
begin
   declare
      S : State (1,1024,1024);
   begin
      Load (S); Finish (S);
      Check (Current (S) = Complete and Completion (S).Stage = Finished, "snapshot complete independently");
      Check (Device_Completion (S).Stage = Devices_Incomplete and then
        Device_Completion (S).Region_Status = ACPI_Device_Policy.Regions_Unavailable,
        "unavailable region barrier explicit");
      Check (Device_Completion (S).Initialization_Methods = 2 and then
        Device_Completion (S).Errors = 0, "root before system bus exactly once and no device walk");
      Check (Pending_Members (S) = 0, "temporary package cleanup preserves complete invariant");
      Check (Startup_Charged (S) = Completion (S).Execution.Modules.Charged +
        Device_Completion (S).Charged, "aggregate fuel");
      Module_Cost := Completion (S).Execution.Modules.Charged;
      Check (Table_Byte (S,1,0) = Character'Pos ('D'), "snapshot bytes remain available");
      declare Saved : constant Device_Report := Device_Completion (S);
         Charge : constant Natural := Startup_Charged (S);
      begin
         Finish (S,0);
         Check (Device_Completion (S) = Saved and then Startup_Charged (S) = Charge,
           "replayed finish cannot repeat root effects");
      end;
   end;
   declare
      S : State (1,1024,1024);
   begin
      Load (S); Finish (S, Startup_Budget (Module_Cost));
      Check (Current (S) = Complete, "zero residual does not erase completed snapshot");
      Check (Device_Completion (S).Stage = Devices_Incomplete, "device residual exhaustion");
      Check (Startup_Charged (S) <= Module_Cost and then Device_Completion (S).Charged = 0,
        "no reset of exhausted startup fuel");
   end;
   declare
      S : State (1,1024,1024);
   begin
      Load (S); Finish (S,0);
      Check (Current (S) = Failed and Device_Completion (S).Stage = Devices_Not_Started,
        "module failure does not run initializers");
      Check (Startup_Charged (S) = 0, "zero startup charge");
   end;
   Ada.Text_IO.Put_Line ("Bootstrap device checks" & Checks'Image);
end Bootstrap_Device_Tests;
