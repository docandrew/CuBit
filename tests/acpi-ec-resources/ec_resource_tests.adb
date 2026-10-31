with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;
with Firmware_Tables;
with ACPI_Service_Core;
with ACPI_Resource_Reader;
with ACPI_EC_Resources;
with ACPI_Resource_Data;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Names;
procedure EC_Resource_Tests is
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 256,
      Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 4);
   package Reader is new ACPI_Resource_Reader (C);
   package P renames ACPI_EC_Resources;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type C.Values.Value_Handle, C.Values.Value_Kind;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type Reader.Copy_Status, P.Status, ACPI_Resource_Data.Byte_Count;
   use type Interfaces.Unsigned_8;
   Service : aliased C.State (1, 8192, 8192, 1);
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with "EC resource check" & Checks'Image; end if; end Check;
   function Find (Parent : C.Namespace.Node_ID; Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, Parent, Name, Node, Status); Check (Status = C.Values.Available);
      C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available);
      return Item;
   end Find;
   function Device (Name : AML_Names.Segment) return C.Node_Descriptor is
     (Find (C.Namespace.Root, Name));
   procedure Release (Handle : in out C.Values.Value_Handle) is
      Status : C.Values.Access_Status;
   begin C.Release_Result (Service, Handle, Status); Check (Status = C.Values.Available and then Handle = C.Values.No_Value); end Release;
   procedure Scalar (Name, Member : AML_Names.Segment; Expected : AML_Decode.Integer_Value) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Evaluate_Named (Service, Find (C.Values.Position (Device (Name)), Member), 100, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = Expected);
   end Scalar;
   procedure Resources (Name : AML_Names.Segment; Expected : P.Status;
                        Data_Address : Interfaces.Unsigned_64 := 0;
                        Control_Address : Interfaces.Unsigned_64 := 0;
                        Space : Interfaces.Unsigned_8 := 1;
                        Decode10 : Boolean := False; GPIO : Boolean := False) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
      Small : Firmware_Tables.Bytes (1 .. 1);
      Buffer : Firmware_Tables.Bytes (1 .. 1024);
      Report : Reader.Report;
      Item : P.Result;
      Handle : C.Values.Value_Handle;
   begin
      C.Evaluate_Named (Service, Find (C.Values.Position (Device (Name)), "_CRS"), 100, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Object_Returned);
      Handle := Value.Handle;
      Reader.Copy_Template (Service, Handle, Small, Report);
      Check (Report.Status = Reader.Insufficient_Capacity and then Report.Required > 1 and then Small (1) = 0);
      Reader.Copy_Template (Service, Handle, Buffer, Report);
      Check (Report.Status = Reader.Copied_Template and then Report.Copied = Report.Required);
      declare Data : constant Firmware_Tables.Bytes := Buffer (1 .. Natural (Report.Copied)); begin
         Ada.Text_IO.Put ("CRS " & Name & " ");
         for Byte of Data loop
            declare Hex_Digits : constant String := "0123456789ABCDEF"; begin
               Ada.Text_IO.Put (Hex_Digits (Natural (Byte) / 16 + 1));
               Ada.Text_IO.Put (Hex_Digits (Natural (Byte) mod 16 + 1));
            end;
         end loop;
         Ada.Text_IO.New_Line;
         Item := P.Describe (Data); Check (Item.Outcome = Expected);
         if Item.Outcome = P.Described then
            Check (Item.Registers (P.Data_Register).Address.Address = Data_Address
                   and then Item.Registers (P.Control_Register).Address.Address = Control_Address);
            Check (Item.Registers (P.Data_Register).Address.Space = Space
                   and then Item.Registers (P.Control_Register).Address.Space = Space);
            Check (Item.Registers (P.Data_Register).Address.Width = 8
                   and then Item.Registers (P.Control_Register).Address.Access_Size = 1);
            Check (Item.Registers (P.Data_Register).IO_Decode_10 = Decode10
                   and then Item.GPIO_Interrupt_Present = GPIO);
            if Name = "ECFM" then
               Check (Item.Registers (P.Data_Register).Writeability_Declared
                      and then not Item.Registers (P.Data_Register).Declared_Writeable
                      and then Item.Registers (P.Control_Register).Declared_Writeable);
            end if;
         end if;
         if Name = "ECIO" then
            for Count in 0 .. Data'Length - 1 loop
               Check (P.Describe (Data (1 .. Count)).Outcome = P.Bad_Template);
            end loop;
            Check (P.Describe (Data, Maximum_Items => 1).Outcome = P.Bad_Template);
            declare Mutated : Firmware_Tables.Bytes := Data; begin
               Mutated (5) := 16#63#; Check (P.Describe (Mutated).Outcome = P.Unsupported_Layout);
               Mutated := Data; Mutated (2) := 16#FF#; Check (P.Describe (Mutated).Outcome = P.Malformed_Descriptor);
               Mutated := Data; Mutated (11) := 16#62#; Mutated (13) := 16#62#;
               Check (P.Describe (Mutated).Outcome = P.Aliased_Registers);
            end;
            declare Mutated : Firmware_Tables.Bytes := Data; begin
               Mutated (3) := 0; Mutated (5) := 0;
               Item := P.Describe (Mutated);
               Check (Item.Outcome = P.Described and then Item.Registers (P.Data_Register).Address.Address = 0);
            end;
            declare Prefix : Firmware_Tables.Bytes (1 .. Data'Length + 1); begin
               Prefix (1) := 16#70#; Prefix (2 .. Prefix'Last) := Data;
               Check (P.Describe (Prefix).Outcome = P.Unsupported_Descriptor);
            end;
            declare Shifted : Firmware_Tables.Bytes (Positive'Last - Data'Length + 1 .. Positive'Last); begin
               for I in Data'Range loop Shifted (Shifted'First + (I - Data'First)) := Data (I); end loop;
               Item := P.Describe (Shifted); Check (Item.Outcome = P.Described);
            end;
         end if;
      end;
      if Name = "ECFI" then
         declare Data : Firmware_Tables.Bytes := Buffer (1 .. Natural (Report.Copied)); begin
            Data (3) := 4;
            Item := P.Describe (Data);
            Check (Item.Outcome = P.Described and then Item.Registers (P.Data_Register).IO_Decode_10
                   and then Item.Registers (P.Data_Register).Address.Address = 16#462#);
         end;
      end if;
      Release (Handle);
      Check (C.Retained_Results (Service) = 0);
   end Resources;
begin
   Check (Ada.Command_Line.Argument_Count = 1);
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Bytes : Ada.Streams.Stream_Element_Array (1 .. 8192);
      Last : Ada.Streams.Stream_Element_Offset;
      Stage : C.Stage_Status; Freeze : C.Freeze_Status; Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report; Status : C.Values.Access_Status;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Bytes, Last); Ada.Streams.Stream_IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Image, Stage); Check (Stage = C.Staged);
      end;
      C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK);
      C.Execute_Frozen (Service, 1000, Report, Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed);
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available);
   end;
   Scalar ("ECFI", "_HID", 16#090C_D041#);
   Scalar ("ECIO", "_GLK", 1); Scalar ("ECIO", "_GPE", 9); Scalar ("ECFM", "_GLK", 0);
   Resources ("ECIO", P.Described, 16#62#, 16#66#);
   Resources ("ECFI", P.Described, 16#62#, 16#66#, Decode10 => True);
   Resources ("ECFM", P.Described, 16#FF00_0003#, 16#FF00_0007#, Space => 0);
   Resources ("ECMM", P.Described, 16#1_0000_1003#, 16#1_0000_1007#, Space => 0);
   Resources ("ECGI", P.Described, 16#FF00_0003#, 16#FF00_0007#, Space => 0, GPIO => True);
   Resources ("ECXX", P.Extra_Registers); Resources ("ECNO", P.Missing_Registers);
   Resources ("ECIR", P.Described, 16#62#, 16#66#, Decode10 => True);
   Resources ("ECLM", P.Described, 16#FE00_0003#, 16#FE00_0007#, Space => 0);
   Resources ("ECAI", P.Described, 16#62#, 16#66#);
   Resources ("ECEX", P.Described, 16#62#, 16#66#, Decode10 => True);
   Resources ("ECDP", P.Ambiguous_Interrupt);
   declare Value : C.Values.Result; Status : C.Values.Access_Status; begin
      C.Observe_Named_Value (Service, C.Values.Position (Find (C.Namespace.Root, "CNT0")), Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = 1);
   end;
   declare
      Value : C.Values.Result; Status : C.Values.Access_Status;
      Parent, Child : C.Values.Value_Handle; Target : C.Node_Descriptor;
      Shape : C.Values.Value_Description;
   begin
      C.Evaluate_Named (Service, Find (C.Values.Position (Device ("ECMM")), "GPES"), 100, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Object_Returned);
      Parent := Value.Handle;
      C.Read_Result_Element (Service, Parent, 0, Child, Status); Check (Status = C.Values.Available);
      C.Read_Result_Device (Service, Child, Target, Status);
      Check (Status = C.Values.Available and then C.Values.Node_Name (Target) = "GPEB"); Release (Child);
      C.Read_Result_Element (Service, Parent, 1, Child, Status); Check (Status = C.Values.Available);
      C.Describe_Result (Service, Child, Shape, Status);
      Check (Status = C.Values.Available and then Shape.Kind = C.Values.Integer_Description and then Shape.Number = 16#11#);
      Release (Child); Release (Parent);
   end;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0);
   Ada.Text_IO.Put_Line ("EC resource checks:" & Checks'Image);
end EC_Resource_Tests;
