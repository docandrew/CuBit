pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with ACPI_PCI_Routes;
with ACPI_PCI_Routing_Reader;
with AML_Execute;
with Firmware_Tables;
with Resource_Oracle;
procedure PCI_Routing_Tests is
   package P renames ACPI_PCI_Routes;
   package Reader is new ACPI_PCI_Routing_Reader (ACPI_Service);
   use type P.Device_Number;
   use type P.Interrupt_Pin;
   use type P.Global_Interrupt;
   use type P.Source_Index;
   use type Reader.Read_Status;
   use type Reader.Source_Kind;
   use type Reader.Route_Entry;
   use type Values.Access_Status;
   use type Values.Value_Handle;
   use type Values.Node_Descriptor;
   use type Values.Value_Kind;
   use type AML_Execute.Execution_Status;
   use type AML_Execute.Named_Metadata_Type;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
begin
   for High in Natural range 0 .. 65_535 loop
      declare R : constant P.Address_Result := P.Decode_Address (Unsigned_64 (High) * 65_536 + 65_535); begin
         Check (R.Valid = (High <= 31) and then (if R.Valid then R.Device = P.Device_Number (High)), "all PCI address upper words");
      end;
   end loop;
   for High in Natural range 30 .. 32 loop
      for Low in Natural range 0 .. 65_535 loop
         declare R : constant P.Address_Result := P.Decode_Address (Unsigned_64 (High) * 65_536 + Unsigned_64 (Low)); begin
            Check (R.Valid = (High <= 31 and then Low = 65_535), "all PCI function words at boundary");
         end;
      end loop;
   end loop;
   for Bit in Natural range 32 .. 63 loop
      Check (not P.Decode_Address (Shift_Left (Unsigned_64'(1), Bit) + 16#FFFF#).Valid, "all reserved high address bits");
   end loop;
   for Wide in Boolean loop
      declare
         Raw : Firmware_Tables.Bytes := Resource_Oracle.Raw_Table;
         Service : aliased State (1, Raw'Length, Raw'Length, Load_Capacity => 1);
         Staged_Result : Stage_Status;
         Frozen : Freeze_Status;
         Access_Result : Values.Access_Status;
         Execution : Execution_Report;
         Members : Namespace.Initialization_Report;
         Device, Link, Node : Namespace.Node_ID;
         Link_Item, Item : Node_Descriptor;
         Value : Values.Result;
         Table, Bad_Table, Handle, Saved : Values.Value_Handle := Values.No_Value;
         Route : Reader.Route_Entry;
         Report : Reader.Report;
         procedure Select_Item (Name : Firmware_Tables.Signature) is
         begin
            Find_Child (Service, Device, Name, Node, Access_Result);
            Check (Access_Result = Values.Available and then Node /= Namespace.Root, "routing fixture node");
            Describe_Node (Service, Node, Item, Access_Result);
            Check (Access_Result = Values.Available, "routing fixture descriptor");
         end Select_Item;
         procedure Evaluate (Name : Firmware_Tables.Signature; Result_Handle : out Values.Value_Handle) is
         begin
            Select_Item (Name);
            Evaluate_Named (Service, Item, 1_000, Value, Access_Result);
            Check (Access_Result = Values.Available and then Value.Status in AML_Execute.Object_Returned | AML_Execute.Metadata_Returned,
              "routing fixture retained value");
            Result_Handle := Value.Handle;
         end Evaluate;
         procedure Clean (Result_Handle : in out Values.Value_Handle) is
         begin
            Release_Result (Service, Result_Handle, Access_Result);
            Check (Access_Result = Values.Available and then Result_Handle = Values.No_Value, "routing fixture release");
         end Clean;
         procedure Pending_Empty is
         begin Check ((for all H of Report.Pending => H = Values.No_Value), "all internal roots released"); end Pending_Empty;
         procedure Parse (Source : Values.Value_Handle; Index : Natural) is
            Before : constant Namespace.Owned.Retained_Root_Count := Retained_Results (Service);
         begin
            Reader.Read_Entry (Service, Source, Index, Route, Report);
            Pending_Empty;
            Check (Retained_Results (Service) = Before, "routing reader retains no extra roots");
            if Report.Status /= Reader.Parsed then
               Check (Route = Reader.Route_Entry'(others => <>), "failed route output default");
            end if;
         end Parse;
      begin
         -- Select the normative DSDT integer width without altering the AML body.
         Raw (9) := (if Wide then 2 else 1); Raw (10) := 0;
         declare Sum : Firmware_Tables.Byte := 0; begin
            for B of Raw loop Sum := Sum + B; end loop; Raw (10) := 0 - Sum;
         end;
         Stage_Table (Service, 1, DSDT, Raw, Staged_Result);
         Check (Staged_Result = Staged, "stage routing fixture");
         Freeze_Catalog (Service, Frozen); Check (Frozen = Catalog_Frozen_OK, "freeze routing fixture");
         Execute_Frozen (Service, 100_000, Execution, Access_Result);
         Check (Access_Result = Values.Available and then Execution.Phase = Module_Execution_Reported, "routing module execution");
         Initialize_Members (Service, Members, Access_Result);
         Check (Access_Result = Values.Available, "routing member initialization");
         Find_Child (Service, Namespace.Root, "PIRQ", Device, Access_Result);
         Check (Access_Result = Values.Available and then Device /= Namespace.Root, "routing bridge fixture");
         Find_Child (Service, Device, "LNKA", Link, Access_Result);
         Check (Access_Result = Values.Available and then Link /= Namespace.Root, "routing link fixture");
         Describe_Node (Service, Link, Link_Item, Access_Result);
         Check (Access_Result = Values.Available, "routing link identity");
         Evaluate ("_PRT", Table);
         for Index in Natural range 0 .. 3 loop
            Parse (Table, Index);
            Check (Report.Status = Reader.Parsed and then Route.Pin = P.Interrupt_Pin'Val (Index)
              and then Route.Device = (case Index is when 0 => 0, when 1 => 2, when 2 => 3, when 3 => 31), "normal _PRT numeric fields " & Report.Status'Image & Report.Access_Status'Image & Index'Image & Wide'Image);
            if Index = 0 then
               Check (Route.Target.Kind = Reader.Global_Pool and then Route.Target.Interrupt = 23, "normal global interrupt");
            else
               Check (Route.Target.Kind = Reader.Link_Device and then Route.Target.Device = Link_Item
                 and then Route.Target.Index = 0 and then Current_Node (Service, Route.Target.Device), "forward/fullpath/alias link identity");
            end if;
         end loop;
         Parse (Table, 4); Check (Report.Status = Reader.Row_Out_Of_Bounds, "table row bounds");
         Parse (Table, Natural'Last); Check (Report.Status = Reader.Row_Out_Of_Bounds, "large row bounds");
         Parse (Values.No_Value, 0); Check (Report.Status = Reader.Invalid_Table, "invalid table handle");
         Evaluate ("NPKG", Handle); Parse (Handle, 0);
         Check (Report.Status = Reader.Not_A_Package, "nonpackage table"); Clean (Handle);
         Evaluate ("BADT", Bad_Table);
         for Index in Natural range 0 .. 17 loop
            declare
               Expected : constant Reader.Read_Status :=
                 (case Index is
                    when 0 => Reader.Not_A_Package,
                    when 1 | 2 | 16 => Reader.Bad_Row_Length,
                    when 3 | 11 => Reader.Wrong_Field_Type,
                    when 4 | 5 => Reader.Bad_Address,
                    when 6 => Reader.Bad_Pin,
                    when 7 .. 10 => Reader.Bad_Source,
                    when 12 => (if Wide then Reader.Bad_Source_Index else Reader.Parsed),
                    when 13 => (if Wide then Reader.Bad_Address else Reader.Parsed),
                    when 14 => (if Wide then Reader.Bad_Pin else Reader.Parsed),
                    when 15 => Reader.Field_Access_Failed,
                    when 17 => Reader.Parsed);
            begin
               Parse (Bad_Table, Index);
               Check (Report.Status = Expected, "exact malformed/width-normalized row outcome " & Report.Status'Image & Report.Access_Status'Image & Index'Image & Wide'Image);
               if Index = 15 then Check (Report.Access_Status = Values.Uninitialized_Element, "uninitialized row field"); end if;
               if Index = 17 then
                  Check (Route.Target.Kind = Reader.Link_Device and then Route.Target.Device = Link_Item
                    and then Route.Target.Index = P.Source_Index'Last, "full resource index retained without admission");
               end if;
            end;
         end loop;
         declare
            type Method_Names is array (Positive range <>) of Firmware_Tables.Signature;
            Names : constant Method_Names := ["RTPK", "RPNM"];
            Description : Values.Value_Description;
         begin
            for Name of Names loop
               Evaluate (Name, Handle);
               Read_Result_Element (Service, Handle, 0, Saved, Access_Result);
               Check (Access_Result = Values.Available, "runtime package method reference");
               Describe_Result (Service, Saved, Description, Access_Result);
               Check (Access_Result = Values.Available and then Description.Kind = Values.Metadata_Description
                 and then Description.Object_Type = AML_Execute.Method_Metadata, "method reference is metadata, not an invocation");
               Read_Result_Device (Service, Saved, Item, Access_Result);
               Check (Access_Result = Values.Wrong_Kind and then Item = Values.No_Node, "method metadata is not a device");
               Clean (Saved); Clean (Handle);
            end loop;
         end;
         Select_Item ("CNTM"); Evaluate_Named (Service, Item, 1_000, Value, Access_Result);
         Check (Access_Result = Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = 0, "method NamePath is inspected, never invoked");
         Select_Item ("RUNC"); Evaluate_Named (Service, Item, 1_000, Value, Access_Result);
         Check (Access_Result = Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = 1, "ordinary nested method call still executes");
         Select_Item ("CNTM"); Evaluate_Named (Service, Item, 1_000, Value, Access_Result);
         Check (Access_Result = Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = 1, "ordinary call has exactly one effect");
         Evaluate ("SNAP", Handle);
         Read_Result_Device (Service, Handle, Item, Access_Result);
         Check (Access_Result = Values.Wrong_Kind and then Item = Values.No_Node, "type-only metadata cannot become live device identity");
         Clean (Handle);
         -- Direct accessor owner/stale checks use an independently retained field.
         Read_Result_Element (Service, Table, 1, Handle, Access_Result);
         Check (Access_Result = Values.Available, "pin link row");
         Read_Result_Element (Service, Handle, 2, Saved, Access_Result);
         Check (Access_Result = Values.Available, "pin link source"); Clean (Handle);
         Read_Result_Device (Service, Saved, Item, Access_Result);
         Check (Access_Result = Values.Available and then Item = Link_Item, "live metadata descriptor");
         declare Other : State (1, 1, 1, Load_Capacity => 1); begin
            Read_Result_Device (Other, Saved, Item, Access_Result);
            Check (Access_Result /= Values.Available and then Item = Values.No_Node, "wrong service metadata handle");
         end;
         Handle := Saved; Clean (Saved);
         Read_Result_Device (Service, Handle, Item, Access_Result);
         Check (Access_Result /= Values.Available and then Item = Values.No_Node, "stale metadata handle");
         Clean (Bad_Table);
         -- At most two extra pins: force each acquisition boundary without callbacks.
         declare
            Slots : array (Positive range 1 .. Namespace.Owned.Retained_Root_Count'Last) of Values.Value_Handle := [others => Values.No_Value];
            Filled : Natural := 0;
         begin
            loop
               Read_Result_Element (Service, Table, 0, Handle, Access_Result);
               exit when Access_Result /= Values.Available;
               Filled := Filled + 1; Slots (Filled) := Handle;
            end loop;
            Check (Access_Result = Values.Root_Limit, "fill retained-root quota");
            Parse (Table, 0);
            Check (Report.Status = Reader.Field_Access_Failed and then Report.Access_Status = Values.Root_Limit, "row acquisition quota cleanup");
            Clean (Slots (Filled)); Filled := Filled - 1;
            Parse (Table, 0);
            Check (Report.Status = Reader.Field_Access_Failed and then Report.Access_Status = Values.Root_Limit, "field acquisition quota cleanup");
            Clean (Slots (Filled)); Filled := Filled - 1;
            Parse (Table, 0); Check (Report.Status = Reader.Parsed, "exact two-pin headroom");
            for I in 1 .. Filled loop Clean (Slots (I)); end loop;
         end;
         Clean (Table);
         Check (Current_Node (Service, Link_Item) and then Retained_Results (Service) = 0, "no roots escape routing projection");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PCI routing checks" & Checks'Image);
end PCI_Routing_Tests;
