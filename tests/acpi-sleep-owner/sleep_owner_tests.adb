pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Sleep_Objects;
with ACPI_Requests.Sleep_States;
with AML_Execute;
with Firmware_Tables;
with Interfaces;
procedure Sleep_Owner_Tests is
   package Q renames ACPI_Requests;
   package Driver is new ACPI_Sleep_Objects (ACPI_Service);
   package Owner is new Q.Sleep_States (Driver);
   package C renames Owner.Controller;
   use type Q.Outcome, Q.Revision_Number;
   use type C.Outcome, Driver.Query_Outcome, Driver.System_State;
   use type Driver.Register_Model, Driver.Sleep_Type, Driver.Support_Indication;
   use type AML_Execute.Execution_Status;
   use type Ada.Streams.Stream_Element_Offset;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Message; end if;
   end Check;
   function Read_Table return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Last = Raw'Last, "complete table read");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Server : Q.State (1, Table'Length, Table'Length, 1, 0);
   Reply : Q.Response;
   Result : C.Report;
   procedure Observe (Registers : Driver.Register_Model; Budget : Natural) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Owner.Observe (Server, Registers, Budget, Result);
      Check (Q.Valid (Server) and then Q.Revision (Server) = Before, "same owner valid and revision unchanged");
      Check (Result.Charged <= Budget, "one total caller budget");
      for Kind in Driver.System_State loop
         Check (Result.Items (Kind).Kind = Kind and then Result.Items (Kind).Registers = Registers,
           "every report labels the exact state and descriptive model");
      end loop;
   end Observe;
   procedure Send (Label : Interfaces.Unsigned_32) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), 1, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "owner protocol succeeds");
   end Send;
begin
   Observe (Driver.PM1A_And_B, 10000);
   Check (Result.Status = C.Not_Ready and then Result.Examined = 0 and then not Result.Recovery_Needed,
     "unstarted owner executes no sleep AML");
   Send (Q.Start_Snapshot);
   Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, ACPI_Service.DSDT, Table, Reply);
   Check (Reply.Status = Q.OK, "same owner table imported");
   Observe (Driver.PM1A_And_B, 10000);
   Check (Result.Status = C.Not_Ready and then Result.Examined = 0, "receiving owner cannot query sleep declarations");
   Q.Handle (Server, Q.Snapshot_Provider,
     (Label => Q.Finish_Snapshot, Data => [Q.Revision (Server), 0, 0, 0], others => <>), Reply);
   Check (Reply.Status = Q.OK, "complete actual module and device initialization");
   Observe (Driver.PM1A_And_B, 0);
   Check (Result.Status = C.Budget_Exhausted and then Result.Examined = 0
     and then Result.Charged = 0, "zero budget does not start lookup/evaluation");
   Observe (Driver.PM1A_And_B, 1);
   Check (Result.Status = C.Budget_Exhausted and then Result.Examined = 1
     and then Result.Charged = 1 and then not Result.Recovery_Needed,
     "host attempt shares the total budget with AML");
   Observe (Driver.PM1A_And_B, 10000);
   if Mode = "normal" then
      Check (Result.Status = C.Observed and then Result.Examined = 6 and then not Result.Recovery_Needed,
        "all six root state observations finish");
      Check (Result.Items (Driver.S0).Outcome = Driver.Types_Available
        and then Result.Items (Driver.S0).Type_A = 0 and then Result.Items (Driver.S0).Type_B = 0,
        "zero sleep types are observations rather than absence");
      Check (Result.Items (Driver.S2).Outcome = Driver.Not_Declared
        and then Result.Items (Driver.S2).Support = Driver.No_Declaration,
        "missing optional state remains explicit");
      Check (Result.Items (Driver.S3).Type_A = 3 and then Result.Items (Driver.S3).Type_B = 4,
        "paired sleep type data retained without writes");
      Check (Result.Items (Driver.S4).Type_A = 1, "S3 evaluated exactly once before S4 reads its effect");
      Check (Result.Items (Driver.S5).Type_A = 5 and then Result.Items (Driver.S5).Type_B = 6,
        "root S5 observations available to owner");
      Observe (Driver.No_Sleep_Registers, 10000);
      Check (Result.Status = C.Observed and then Result.Items (Driver.S3).Outcome = Driver.Return_Ignored
        and then not Result.Items (Driver.S3).A_Available and then not Result.Items (Driver.S3).B_Available,
        "unused register model evaluates present object but exposes no types");
      Observe (Driver.PM1A_Only, 10000);
      Check (Result.Status = C.Observed and then Result.Items (Driver.S4).Type_A = 3
        and then not Result.Items (Driver.S4).B_Available,
        "explicit repeated point observation runs each method once and ignores unused B");
   elsif Mode = "empty" then
      Check (Result.Status = C.Observed and then Result.Examined = 6,
        "observed absence is not a transition capability");
      for Kind in Driver.System_State loop
         Check (Result.Attempted (Kind) and then Result.Items (Kind).Outcome = Driver.Not_Declared
           and then Result.Items (Kind).Support =
             (if Kind in Driver.S0 | Driver.S5 then Driver.Required_State else Driver.No_Declaration),
           "required missing objects distinguished from optional declarations");
      end loop;
   elsif Mode = "invalid" then
      Check (Result.Status = C.Object_Failed and then Result.Examined = 2
        and then Result.Items (Driver.S1).Outcome = Driver.Invalid_Type_Value
        and then not Result.Recovery_Needed, "invalid type data is refused without truncation");
      Check (not Result.Attempted (Driver.S2) and then not Result.Attempted (Driver.S3)
        and then not Result.Items (Driver.S1).A_Available, "refusal stops before later fatal AML");
   else
      Check (Mode = "fatal", "known fixture mode");
      Check (Result.Status = C.Recovery_Required and then Result.Examined = 2
        and then Result.Recovery_Needed and then Result.Items (Driver.S1).Execution = AML_Execute.Fatal_Requested,
        "fatal observation requires owner recovery");
      Check (not Result.Attempted (Driver.S2) and then not Result.Attempted (Driver.S3),
        "fatal state prevents further AML in this scan");
   end if;
   Ada.Text_IO.Put_Line ("ACPI sleep owner checks" & Checks'Image);
end Sleep_Owner_Tests;
