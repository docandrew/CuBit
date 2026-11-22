pragma Ada_2022;
with AML_Objects; with Ada.Text_IO; with Ada.Command_Line; with Ada.Streams; with Ada.Streams.Stream_IO;
with ACPI_Service; with ACPI_Requests; with ACPI_Requests.Cached_S5; with AML_Execute; with Firmware_Tables; with Interfaces;
procedure Cached_S5_Tests is
   package Q renames ACPI_Requests;
   package Cache renames Q.Cached_S5;
   package O renames Cache.Cache.Objects;
   use type Q.Outcome, O.Query_Outcome, O.Sleep_Type, AML_Execute.Execution_Status;
   use type Ada.Streams.Stream_Element_Offset;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Message : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Message; end if; end Check;
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
   Server : Q.State (1, Table'Length, Table'Length, 1, 0, Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes, Value_Element_Capacity => AML_Objects.Max_Elements);
   Reply : Q.Response;
   Item : Cache.Snapshot;
   Result : O.Query_Report;
   procedure Prepare (Budget : Natural := 32) is
   begin
      Cache.Prepare (Server, Item, O.PM1A_And_B, Budget, Result);
      Check (Q.Valid (Server) and then Result.Charged <= Budget, "bounded valid same owner");
      Check (Result.AML_Charged = 0 and then not Q.Retained_Results_Pending (Server), "no AML or retained roots");
   end Prepare;
   procedure Send (Label : Interfaces.Unsigned_32) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), 1, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "owner protocol succeeds");
   end Send;
begin
   Prepare; Check (not Cache.Current_Types (Server, Item).Available, "unstarted refuses");
   Send (Q.Start_Snapshot);
   Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, ACPI_Service.DSDT, Table, Reply);
   Check (Reply.Status = Q.OK, "import table");
   Prepare; Check (not Cache.Current_Types (Server, Item).Available, "receiving refuses");
   Q.Handle (Server, Q.Snapshot_Provider, (Label => Q.Finish_Snapshot, Data => [Q.Revision (Server), 0, 0, 0], others => <>), Reply);
   Check (Reply.Status = Q.OK, "startup complete");
   Prepare;
   if Mode = "normal" then
      Check (Result.Outcome = O.Types_Available and then Cache.Current_Types (Server, Item).Available, "stored S5 types available despite fatal firmware methods");
      Check (Cache.Current_Types (Server, Item).Type_A = 5 and then Cache.Current_Types (Server, Item).Type_B = 6, "exact copied types");
   else
      Check (not Cache.Current_Types (Server, Item).Available, "no fabricated types");
      if Mode = "dynamic" then Check (Result.Outcome = O.Evaluation_Failed and then Result.Execution = AML_Execute.No_Return, "method not executed");
      elsif Mode = "invalid" then Check (Result.Outcome = O.Invalid_Type_Value, "type outside three bits refused");
      else Check (Result.Outcome = O.Not_Declared, "missing S5 refused"); end if;
   end if;
   Cache.Invalidate (Item); Check (not Cache.Current_Types (Server, Item).Available, "explicit invalidate");
   Prepare (0); Check (not Cache.Current_Types (Server, Item).Available, "zero budget clears prior copy");
   Prepare;
   declare Other : Q.State (1, Table'Length, Table'Length, 1, 0, Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes, Value_Element_Capacity => AML_Objects.Max_Elements); begin
      Check (not Cache.Current_Types (Other, Item).Available, "different owner cannot use snapshot");
   end;
   Cache.Retire (Item); Prepare; Check (not Cache.Current_Types (Server, Item).Available, "retired stays closed");
   Ada.Text_IO.Put_Line ("Cached S5 checks" & Checks'Image);
end Cached_S5_Tests;
