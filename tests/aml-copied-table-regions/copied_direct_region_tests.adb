pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
with AML_Copied_Tables;
with AML_Field_Data;
procedure Copied_Direct_Region_Tests is
   Boot_Quota : constant Positive := 2;
   Copy_Quota : constant Natural := 2;
   Registry_Quota : constant Positive := Boot_Quota + Copy_Quota;
   Fuel : constant Natural := 10000;
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Copied_Table_Capacity => Copy_Quota,
      Copied_Byte_Capacity => 4096);
   package V renames C.Values;
   package N renames C.Namespace;
   package F renames Firmware_Tables;
   use type Ada.Streams.Stream_Element_Offset;
   use type C.Stage_Status, C.Freeze_Status, C.Import_Phase, C.Execution_Phase_Result;
   use type V.Access_Status, V.Module_Outcome, AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value, F.Byte, N.Definition_Identity;
   use type AML_Copied_Tables.Outcome, AML_Copied_Tables.Table_ID;
   use type AML_Decode.Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read (Argument : Positive) return F.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : F.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File); Check (Last = Raw'Last, "whole fixture");
         for I in Data'Range loop Data (I) := F.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read;
   Boot : constant F.Bytes := Read (1);
   Wrapper : F.Bytes := Read (2);
   Mode : constant String := Ada.Command_Line.Argument (3);
   Short : constant Boolean := Mode = "short";
   Wrapper_Image : constant F.Bytes := Wrapper;
   Child_Table_Bytes : constant Positive := 62;
   Octet_Bits : constant Positive := 8;
   Field_First_Bit : constant Natural := F.Table_Header_Size * Octet_Bits +
     (if Mode in "unaligned" | "alias" then 3 elsif Mode in "partial" | "trailing" then 7 else 0);
   Field_Bits : constant Positive := Child_Table_Bytes * Octet_Bits -
     (if Short then Octet_Bits elsif Mode = "partial" then 1 else 0);
   Field_Bytes : constant Positive := (Field_Bits + Octet_Bits - 1) / Octet_Bits;
   Copied_Report : AML_Copied_Tables.Report;
   Service : aliased C.State (Boot_Quota, Boot'Length + Wrapper'Length, Positive'Max (Boot'Length, Wrapper'Length), Registry_Quota);
   Stage : C.Stage_Status;
   Freeze : C.Freeze_Status;
   Status : V.Access_Status;
   Execution : C.Execution_Report;
   Initial : N.Initialization_Report;
   Item : C.Node_Descriptor;
   Node : N.Node_ID;
   Value : V.Result;
   Root_Definition, Added_Definition : N.Definition_Identity;
   Success : constant AML_Decode.Integer_Value :=
     (if Boot (9) < 2 then 16#FFFF_FFFF# else AML_Decode.Integer_Value'Last);
   procedure Find (Name : String) is
   begin
      C.Find_Child (Service, N.Root, Name, Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "find " & Name);
      C.Describe_Node (Service, Node, Item, Status); Check (Status = V.Available, "describe " & Name);
   end Find;
   procedure Value_Is (Name : String; Expected : AML_Decode.Integer_Value) is
   begin
      Find (Name); C.Evaluate_Named (Service, Item, Fuel, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Returned
        and then Value.Number = Expected, "value " & Name);
   end Value_Is;
begin
   C.Stage_Table (Service, 11, C.DSDT, Boot, Stage); Check (Stage = C.Staged, "boot");
   C.Stage_Copied_Source (Service, Wrapper, Fuel, Copied_Report, Status);
   Check (Status = V.Available and then Copied_Report.Status = AML_Copied_Tables.Added
     and then Copied_Report.ID = 1, "owned copied wrapper");
   Wrapper := [others => 0];
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (Service, Fuel, Execution, Status);
   Check (Status = V.Available and then Execution.Phase = C.Module_Execution_Reported
     and then Execution.Modules.Outcome = V.Modules_Completed, "startup");
   C.Initialize_Members (Service, Initial, Status); Check (Status = V.Available, "ready");
   Value_Is ("RREV", AML_Decode.Integer_Value (Wrapper_Image (9)));
   Value_Is ("REVB", AML_Decode.Integer_Value (Wrapper_Image (9)));
   Find ("RBUF"); C.Evaluate_Named (Service, Item, Fuel, Value, Status);
   Check (Status = V.Available and then Value.Status = AML_Execute.Object_Returned,
     "method copied Buffer");
   declare Held : V.Value_Handle := Value.Handle; begin
      C.Release_Result (Service, Held, Status); Check (Status = V.Available, "release method Buffer");
   end;
   Find ("TBUF"); C.Evaluate_Named (Service, Item, Fuel, Value, Status);
   Check (Status = V.Available and then Value.Status = AML_Execute.Object_Returned,
     "copied Field materializes Buffer " & Status'Image & " " & Value.Status'Image);
   declare
      Held : V.Value_Handle := Value.Handle;
      Data : AML_Decode.Bytes (1 .. Field_Bytes);
      Copied : Natural;
      Snapshot : AML_Field_Data.Read_Result;
      function Expected (Index : Natural) return F.Byte is
         Sum, Position : Natural := 0;
      begin
         for Bit in 0 .. Octet_Bits - 1 loop
            if Index * Octet_Bits + Bit < Field_Bits then
               Position := Field_First_Bit + Index * Octet_Bits + Bit;
               if (Natural (Wrapper_Image (Position / Octet_Bits + 1)) /
                 (2 ** (Position mod Octet_Bits))) mod 2 = 1
               then Sum := Sum + 2 ** Bit; end if;
            end if;
         end loop;
         return F.Byte (Sum);
      end Expected;
   begin
      C.Read_Result_Bytes (Service, Held, 0, Data, Copied, Status);
      Check (Status = V.Available and then Copied = Field_Bytes, "copied Buffer extent");
      Check ((for all I in Data'Range => Data (I) = Expected (I - 1)), "copied Buffer bit model");
      Find ("TBUF"); Snapshot := C.Read_Namespace_Field (Service, Node);
      Check (Snapshot.Status = AML_Decode.Accepted and then Snapshot.Length = Field_Bytes,
        "observer copied Field extent");
      Check ((for all I in Data'Range => Snapshot.Content (I) = Expected (I - 1)),
        "observer copied Field bit model");
      C.Release_Result (Service, Held, Status); Check (Status = V.Available, "release copied Buffer");
   end;
   Value_Is ("BCNT", 0); Value_Is ("LTGT", 0);
   Find ("LDTB"); C.Describe_Definition (Service, Item, Root_Definition, Status);
   Check (Status = V.Available and then Root_Definition = C.Retained_Definition (Service, 1), "boot registry identity");
   C.Invoke_With_Arguments (Service, Node, [others => (V.Immediate_Argument, 0)], 0, 1, Value, Status);
   Check (Status = V.Available and then Value.Status = AML_Execute.Budget_Exceeded, "pre-effect fuel refusal " & Status'Image & " " & Value.Status'Image);
   Value_Is ("BCNT", 0); Value_Is ("LTGT", 0);
   if Short then
      Find ("LDTB");
      C.Invoke_With_Arguments (Service, Node, [others => (V.Immediate_Argument, 0)], 0, Fuel, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Buffer_Field_Limit,
        "short table field refusal " & Status'Image & " " & Value.Status'Image);
      Value_Is ("BCNT", 0); Value_Is ("LTGT", 0); Value_Is ("TTYP", 1);
      C.Find_Child (Service, N.Root, "DNUM", Node, Status);
      Check (Status = V.Available and then Node = N.Root, "no child declaration after short refusal");
      Check (C.Valid (Service) and then C.Current_Import_Phase (Service) = C.Catalog_Executed,
        "short refusal preserves ready owner");
      Ada.Text_IO.Put_Line ("Table field checks" & Checks'Image); return;
   end if;
   Value_Is ("LDTB", Success); Value_Is ("LTGT", Success); Value_Is ("TTYP", 1);
   Value_Is ("BCNT", 1); Value_Is ("DNUM", 42);
   Find ("DNUM"); C.Describe_Definition (Service, Item, Added_Definition, Status);
   Check (Status = V.Available and then N.Has_Definition (Added_Definition)
     and then Added_Definition /= Root_Definition, "copied definition owns method");
   Find ("LDTB");
   C.Invoke_With_Arguments (Service, Node, [others => (V.Immediate_Argument, 0)], 0, Fuel, Value, Status);
   Check (Status = V.Available and then Value.Status = AML_Execute.Duplicate_Name, "duplicate consumed bytes do not replay");
   Value_Is ("BCNT", 1); Value_Is ("DNUM", 42);
   Check (C.Valid (Service) and then C.Current_Import_Phase (Service) = C.Catalog_Executed
     and then C.Table_Count (Service) = 1, "ready owner and immutable boot catalog");
   Ada.Text_IO.Put_Line ("Table field checks" & Checks'Image);
end Copied_Direct_Region_Tests;
