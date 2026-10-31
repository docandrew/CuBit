pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_HID_I2C;
with ACPI_HID_I2C_Resources;
with ACPI_Resource_Reader;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure HID_Resource_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 8);
   package Q is new ACPI_HID_I2C (C);
   package Reader is new ACPI_Resource_Reader (C);
   package H renames ACPI_HID_I2C_Resources;
   package R renames H.R;
   package S renames H.S;
   package G renames H.G;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type Q.Identification_Outcome;
   use type Q.Query_Outcome;
   use type Q.Status_Outcome;
   use type Q.Descriptor_Register;
   use type Reader.Copy_Status;
   use type R.Byte_Count;
   use type R.Item_Count;
   use type R.Template_Status;
   use type H.Analysis_Outcome;
   use type S.Slave_Address;
   use type S.Rate;
   use type S.Address_Mode;
   use type G.Pin_Number;
   use type G.Interrupt_Mode;
   use type G.Interrupt_Polarity;
   use type Firmware_Tables.Byte;
   Service : aliased C.State (1, 4096, 4096, 1);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   function Find (Parent : C.Namespace.Node_ID; Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, Parent, Name, Node, Status);
      Check (Status = C.Values.Available, "find " & Name);
      C.Describe_Node (Service, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Name);
      return Item;
   end Find;
   Device, Setter, Resource_Node : C.Node_Descriptor;
   procedure Set_Mode (Mode : Natural) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter), [0 => AML_Decode.Integer_Value (Mode), others => 0], 1, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "set mode");
   end Set_Mode;
   function Number (Name : String) return AML_Decode.Integer_Value is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Evaluate_Named (Service, Find (C.Namespace.Root, Name), 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned, "counter");
      return Value.Number;
   end Number;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
   declare
      Input : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (Input, Stream, Last); Ada.Streams.Stream_IO.Close (Input);
      declare
         Data : Firmware_Tables.Bytes (1 .. Natural (Last));
         Staged : C.Stage_Status;
         Frozen : C.Freeze_Status;
         Execution : C.Execution_Report;
         Members : C.Namespace.Initialization_Report;
         Status : C.Values.Access_Status;
      begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Data, Staged); Check (Staged = C.Staged, "stage");
         C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
         C.Execute_Frozen (Service, 1000, Execution, Status);
         Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "execute");
         C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
      end;
   end;
   Device := Find (C.Namespace.Root, "TPD0"); Setter := Find (C.Namespace.Root, "SETM");
   Resource_Node := Find (C.Values.Position (Device), "_CRS");
   for Mode in 0 .. 8 loop
      Set_Mode (Mode);
      declare
         Identity : Q.Identification_Report;
         Status_Observation : Q.Status_Report;
         Descriptor : Q.Query_Report;
         Value : C.Values.Result;
         Status : C.Values.Access_Status;
         Handle : C.Values.Value_Handle;
         Copy : Reader.Report;
         Small : Firmware_Tables.Bytes (1 .. 1);
         Flags : AML_Decode.Integer_Value;
         Charged : Natural;
      begin
         Q.Identify (Service, Device, 1000, Identity);
         Check (Identity.Outcome = Q.Compatible and then C.Retained_Results (Service) = 0, "candidate identity");
         Charged := Identity.Charged;
         Q.Observe_Status (Service, Device, 1000 - Charged, Status_Observation);
         Check (Status_Observation.Outcome = Q.Status_Available
           and then not Status_Observation.Defaulted, "status observation");
         Charged := Charged + Status_Observation.Charged; Flags := Status_Observation.Flags;
         Check (Flags = (if Mode = 8 then 16#0D# else 16#0F#), "enabled flag is independent of resources");
         C.Evaluate_Named (Service, Resource_Node, 1000 - Charged, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Object_Returned, "retained current resources");
         Charged := Charged + Value.Charged; Handle := Value.Handle;
         Reader.Copy_Template (Service, Handle, Small, Copy);
         Check (Copy.Status = Reader.Insufficient_Capacity and then Copy.Required > 1
           and then Copy.Copied = 0 and then Small (1) = 0 and then C.Retained_Results (Service) = 1,
           "sizing preserves original result");
         declare
            Data : Firmware_Tables.Bytes (11 .. 10 + Natural (Copy.Required));
            Analysis, Bad : H.Analysis_Report;
         begin
            Reader.Copy_Template (Service, Handle, Data, Copy);
            Check (Copy.Status = Reader.Copied_Template and then Copy.Copied = Copy.Required
              and then Number ("CALL") = 1, "copy retry without AML reevaluation");
            H.Analyze (Data, Analysis);
            case Mode is
               when 0 | 5 | 8 =>
                  Check (Analysis.Outcome = H.Connections_Found and then Analysis.I2C_Count = 1
                    and then Analysis.Interrupt_Count = 1, "one connection pair");
                  Check (Analysis.I2C.Information.Address = 16#15#
                    and then Analysis.I2C.Information.Connection_Speed = 400_000
                    and then Analysis.I2C.Information.Addressing = S.Seven_Bit, "typed bus settings");
                  Check (Analysis.Interrupt.Information.Mode = G.Level_Triggered
                    and then Analysis.Interrupt.Information.Polarity = G.Active_Low, "typed interrupt settings");
                  declare Pin : constant G.Pin_Result := G.Read_Pin (Data, Analysis.Interrupt_Offset, 1); begin
                     Check (Pin.Available and then Pin.Value = 16#2A#, "interrupt pin");
                  end;
                  if Mode = 5 then Check (Analysis.Other_Items = 3, "unrelated resources counted"); end if;
                  if Mode = 0 then
                     Check (Analysis.I2C.Vendor.Length = 1
                       and then Data (Data'First + Natural (Analysis.I2C.Vendor.First)) = 16#AA#,
                       "vendor data retained");
                  end if;
               when 1 => Check (Analysis.Outcome = H.Missing_Interrupt and then Analysis.I2C_Count = 1, "missing interrupt");
               when 2 => Check (Analysis.Outcome = H.Missing_I2C and then Analysis.Interrupt_Count = 1, "missing I2C");
               when 3 => Check (Analysis.Outcome = H.Ambiguous_Connections and then Analysis.I2C_Count = 2, "duplicate I2C");
               when 4 => Check (Analysis.Outcome = H.Ambiguous_Connections and then Analysis.Interrupt_Count = 2, "duplicate interrupts");
               when 6 => Check (Analysis.Outcome = H.Dependent_Alternatives, "no resource alternative selection");
               when 7 => Check (Analysis.Outcome = H.Missing_I2C and then Analysis.Framing.Status = R.Complete, "empty framed template");
            end case;
            for Length in 0 .. Data'Length - 1 loop
               H.Analyze (Data (Data'First .. Data'First + Length - 1), Bad);
               Check (Bad.Outcome = H.Invalid_Template, "truncated prefixes");
            end loop;
            H.Analyze (Data, Bad, 0);
            if Mode = 7 then
               Check (Bad.Outcome = H.Missing_I2C, "zero item quota permits EndTag");
            else
               Check (Bad.Outcome = H.Invalid_Template and then Bad.Framing.Status = R.Item_Limit, "item quota");
            end if;
            if Mode = 0 then
               declare
                  Changed : Firmware_Tables.Bytes := Data;
                  Sum : Firmware_Tables.Byte := 0;
                  High : Firmware_Tables.Bytes (Natural'Last - (Data'Length - 1) .. Natural'Last);
               begin
                  High := Data; H.Analyze (High, Bad);
                  Check (Bad.Outcome = H.Connections_Found and then Bad.I2C_Offset = Analysis.I2C_Offset, "high shifted bounds");
                  Changed (Changed'First + Natural (Analysis.I2C_Offset) + 3) := 0;
                  H.Analyze (Changed, Bad); Check (Bad.Outcome = H.Invalid_Connection, "bad serial revision");
                  Changed := Data;
                  Changed (Changed'First + Natural (Analysis.Interrupt_Offset) + 3) := 0;
                  H.Analyze (Changed, Bad); Check (Bad.Outcome = H.Invalid_Connection, "bad GPIO revision");
                  Changed := Data;
                  for B of Changed loop Sum := Sum + B; end loop;
                  Changed (Changed'Last) := (if Sum + 1 = 0 then 2 else 1);
                  H.Analyze (Changed, Bad); Check (Bad.Outcome = H.Invalid_Template and then Bad.Framing.Status = R.Bad_Checksum, "checksum");
               end;
            end if;
            if Mode in 0 | 5 | 8 then
               if (Flags and 2) /= 0 then
                  Q.Query_Descriptor (Service, Device, 1000 - Charged, Descriptor);
                  Check (Descriptor.Outcome = Q.Descriptor_Available and then Descriptor.Address = 16#20#
                    and then Descriptor.Charged <= 1000 - Charged, "descriptor after resource discovery");
               else
                  Check (Mode = 8 and then Analysis.Outcome = H.Connections_Found,
                    "valid descriptions do not enable a disabled device");
               end if;
            end if;
         end;
         C.Release_Result (Service, Handle, Status);
         Check (Status = C.Values.Available and then C.Retained_Results (Service) = 0, "release resource result");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("HID I2C resource checks" & Checks'Image);
end HID_Resource_Tests;
