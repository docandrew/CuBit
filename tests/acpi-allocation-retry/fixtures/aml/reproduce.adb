pragma Ada_2022;
with Ada.Real_Time; with AML_Work_Kinds;
with Ada.Command_Line; with Ada.Text_IO;
with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces;
with System;
with AML_Field_Access; with AML_Field_Layout; with AML_Identity.Issuer;
with ACPI_PCI_Config_Reads;
with Firmware_Tables;
with AML_Decode; with AML_Delays;
with ACPI_Service_Core;
with Laptop_Profile;
with AML_Storage_Capacities;
procedure Reproduce is
   use Interfaces;
   Delays, Clocks : Natural := 0;
   procedure Delay_Call (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin Delays := Delays + 1; Result := AML_Delays.Unavailable; end Delay_Call;
   procedure Clock (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
   begin Clocks := Clocks + 1; Value := 0; Available := True; end Clock;
   package RT renames Ada.Real_Time;
   use type RT.Time,RT.Time_Span,AML_Work_Kinds.Kind;
   type Counters is array (AML_Work_Kinds.Kind) of Natural;
   type Durations is array (AML_Work_Kinds.Kind) of Duration;
   Calls : Counters := [others => 0];
   Totals,Worst : Durations := [others => 0.0];
   type Frame is record Kind : AML_Work_Kinds.Kind; Began : RT.Time; end record;
   Frames : array (Positive range 1 .. 16) of Frame;
   Depth : Natural := 0;
   procedure Observe_Work (Item : AML_Work_Kinds.Kind; Entering : Boolean) is
      Now : constant RT.Time := RT.Clock;
      Elapsed : Duration;
   begin
      if Entering then
         if Depth = Frames'Length then raise Program_Error with "profile depth"; end if;
         Depth := Depth+1; Frames (Depth) := (Item,Now); Calls (Item) := Calls (Item)+1;
      else
         if Depth = 0 or else Frames (Depth).Kind /= Item then raise Program_Error with "profile pairing"; end if;
         Elapsed := RT.To_Duration (Now-Frames (Depth).Began);
         Totals (Item) := Totals (Item)+Elapsed; Worst (Item) := Duration'Max (Worst (Item),Elapsed);
         Depth := Depth-1;
      end if;
   end Observe_Work;
   package Core is new ACPI_Service_Core
     (Perform_Delay => Delay_Call, Read_Microseconds => Clock, Observe_Work => Observe_Work,
      Namespace_Node_Capacity => Laptop_Profile.Namespace_Nodes,
      Aggregate_Method_Capacity => Laptop_Profile.Method_Bytes, Enable_PCI_Context => True);
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package P renames ACPI_PCI_Config_Reads;
   use type A.Endpoint_ID, AML_Identity.Identity, L.Register_Geometry;
   use type P.Outcome;
   Maximum_Model_Targets : constant Positive := 512;
   type Geometry_List is array (Positive range 1 .. Maximum_Model_Targets) of L.Register_Geometry;
   NVS_First : constant AML_Decode.Integer_Value := 16#3EB7F000#;
   NVS_End : constant AML_Decode.Integer_Value := 16#41B2F000#;
   type Model_Context is record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Saved : Geometry_List;
      Used, NVS_Reads, PCI_Reads, Lock_Entries : Natural := 0;
   end record;
   IO : Model_Context;
   Issued : Boolean;
   function Pending (Context : Model_Context) return Boolean is
      pragma Unreferenced (Context);
   begin return False; end Pending;
   procedure Describe (Context : Model_Context; Target : A.Target;
     Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>);
      Accepted := Target.Provider = Context.Owner and then Target.Endpoint = 1
        and then Target.Epoch = 1 and then Target.Register in 1 .. A.Register_ID (Context.Used);
      if Accepted then Geometry := Context.Saved (Positive (Target.Register)); end if;
   end Describe;
   procedure Dispatch (Context : in out Model_Context; Item : A.Request; Reply : out A.Response) is
      Geometry : L.Register_Geometry;
      Accepted : Boolean;
      Value : AML_Decode.Integer_Value := 0;
   begin
      Reply := (others => <>);
      Describe (Context, Item.Destination, Geometry, Accepted);
      if not Accepted or else Item.For_Write then return; end if;
      if Geometry.Region.Space = L.PCI_Config_Space then
         Context.PCI_Reads := Context.PCI_Reads + 1;
         Value := (if Geometry.Byte_Offset = 16#60# then 16#E000_0000# else 0);
         Ada.Text_IO.Put_Line ("MODEL_PCI_READ seg=" & Geometry.Region.PCI.Device.Segment'Image
           & " bus=" & Geometry.Region.PCI.Device.Bus'Image
           & " dev=" & Geometry.Region.PCI.Device.Device'Image
           & " fn=" & Geometry.Region.PCI.Device.Func'Image
           & " offset=" & Geometry.Byte_Offset'Image);
      else
         Context.NVS_Reads := Context.NVS_Reads + 1;
         if Geometry.Region.Base = 16#41AE3018# then
            Ada.Text_IO.Put_Line ("MODEL_PNVA_READ offset=" & Geometry.Byte_Offset'Image & " width=" & Geometry.Width'Image);
         end if;
         if Geometry.Region.Base = 16#41AE3418# and then Geometry.Byte_Offset = 398 then
            Value := AML_Decode.Integer_Value'Value (Ada.Command_Line.Argument (2));
         elsif Geometry.Region.Base = 16#41AD8018# and then Geometry.Byte_Offset = 2120 then
            Value := AML_Decode.Integer_Value'Value (Ada.Command_Line.Argument (3));
         elsif Geometry.Region.Base = 16#41AE3018# and then Geometry.Byte_Offset = 658 then
            Value := 16#18#; -- PMBS modeled as 0x1800, not captured hardware data.
         end if;
      end if;
      Reply := (A.Completed, Context.Owner, Value, A.No_Operation);
   end Dispatch;
   procedure Confirmed (Context : Model_Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (Context, Expected);
   begin Accepted := False; end Confirmed;
   procedure Authorize (Context : Model_Context; Target : A.Target;
     For_Write : Boolean; Accepted : out Boolean) is
      Geometry : L.Register_Geometry;
   begin Describe (Context, Target, Geometry, Accepted); Accepted := Accepted and then not For_Write; end Authorize;
   procedure Resolve (Context : in out Model_Context; Geometry : L.Register_Geometry;
     Target : in out A.Target; Accepted : in out Boolean) is
      View : P.View;
   begin
      Accepted := False;
      if Geometry.Region.Space = L.Memory_Space then
         Accepted := not Geometry.Region.PCI.Known and then Geometry.Region.Base >= NVS_First
           and then Geometry.Region.Base < NVS_End
           and then Geometry.Region.Length <= NVS_End - Geometry.Region.Base;
      elsif Geometry.Region.Space = L.PCI_Config_Space and then Geometry.Region.PCI.Known then
         View := P.Plan ((Device => Geometry.Region.PCI.Device,
           Base => Geometry.Region.Base, Length => Geometry.Region.Length),
           Geometry.Byte_Offset, Geometry.Width, P.Bus_Bytes);
         Accepted := Ada.Command_Line.Argument (4) = "pci" and then View.Status = P.Read_Ready;
         Ada.Text_IO.Put_Line ("MODEL_PCI_RESOLVE status=" & View.Status'Image & " admitted=" & Accepted'Image);
      end if;
      if not Accepted then return; end if;
      for I in 1 .. Context.Used loop
         if Context.Saved (I) = Geometry then
            Target := (Provider => Context.Owner, Endpoint => 1, Epoch => 1, Register => A.Register_ID (I)); return;
         end if;
      end loop;
      if Context.Used = Maximum_Model_Targets then Accepted := False; return; end if;
      Context.Used := Context.Used + 1; Context.Saved (Context.Used) := Geometry;
      Target := (Provider => Context.Owner, Endpoint => 1, Epoch => 1, Register => A.Register_ID (Context.Used));
   end Resolve;
   procedure Begin_Field (Context : in out Model_Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
   begin
      Accepted := not For_Write;
      if Accepted and then Requires_Lock then Context.Lock_Entries := Context.Lock_Entries + 1; end if;
   end Begin_Field;
   procedure End_Field (Context : in out Model_Context; Accepted : in out Boolean) is
      pragma Unreferenced (Context, Accepted);
   begin null; end End_Field;
   package Fields is new Core.Field_IO (Model_Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize, Resolve_Module_Target => Resolve,
     Begin_Field => Begin_Field, End_Field => End_Field, Allow_Locked_NVS_Reads => True);
   use type Core.Stage_Status, Core.Freeze_Status;
   Staged : Core.Stage_Status; Frozen : Core.Freeze_Status;
   Report : Core.Execution_Report; Status : Core.Values.Access_Status;
   Stats : Core.Metrics;
   Table_Count : constant Positive := Ada.Command_Line.Argument_Count - 3;
   Total : Natural := 0;
   Largest : Positive := 1;
   function Table_Path (Index : Positive) return String is
     (Ada.Command_Line.Argument (if Index = 1 then 1 else Index + 3));
   function Read_Table (Name : String) return Firmware_Tables.Bytes is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Name);
      declare
         Size : constant Positive := Positive (Ada.Streams.Stream_IO.Size (File));
         Buffer : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Size);
         use type Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (File, Buffer, Last); Ada.Streams.Stream_IO.Close (File);
         if Last /= Buffer'Last then raise Program_Error with "short read"; end if;
         for I in Data'Range loop Data (I) := Unsigned_8 (Buffer (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
begin
   AML_Identity.Issuer.Issue (IO.Owner, Issued);
   if not Issued then raise Program_Error; end if;
   for Index in 1 .. Table_Count loop
      declare Data : constant Firmware_Tables.Bytes := Read_Table (Table_Path (Index)); begin
         if Data'Length > Natural'Last - Total then raise Program_Error with "input extent"; end if;
         Total := Total + Data'Length; Largest := Positive'Max (Largest, Data'Length);
      end;
   end loop;
   if not Core.Storage_Input_Supported (Total) then raise Program_Error with "input sizing refused"; end if;
   declare
      Limits : constant AML_Storage_Capacities.Limits := Core.Initial_Storage_Limits (Total);
      subtype Sized_State is Core.State (Table_Count, Total, Largest, Table_Count,
        Limits.Objects, Limits.Bytes, Limits.Elements);
      type State_Access is access Sized_State;
      Service : constant State_Access := new Sized_State;
   begin
      Ada.Text_IO.Put_Line ("limits tables=" & Table_Count'Image & " input=" & Total'Image &
        " nodes=" & Laptop_Profile.Namespace_Nodes'Image & " methods=" & Laptop_Profile.Method_Bytes'Image &
        " objects=" & Limits.Objects'Image & " bytes=" & Limits.Bytes'Image &
        " elements=" & Limits.Elements'Image & " outer-state-bytes=" &
        Long_Long_Integer'Image (Long_Long_Integer (Service.all'Size / System.Storage_Unit)));
      for Index in 1 .. Table_Count loop
         declare Data : constant Firmware_Tables.Bytes := Read_Table (Table_Path (Index)); begin
            Core.Stage_Table (Service.all, Index, (if Index = 1 then Core.DSDT else Core.SSDT), Data, Staged);
            if Staged /= Core.Staged then raise Program_Error with "stage " & Index'Image & Staged'Image; end if;
         end;
      end loop;
      Core.Freeze_Catalog (Service.all, Frozen);
      if Frozen /= Core.Catalog_Frozen_OK then raise Program_Error with "freeze " & Frozen'Image; end if;
      Fields.Execute_Frozen (Service.all, IO, [others => AML_Field_Access.No_Target], 0, Laptop_Profile.Startup_Fuel, Report, Status);
      Check_Profile : declare begin
         if Depth /= 0 then raise Program_Error with "unclosed profile"; end if;
         for Item in AML_Work_Kinds.Kind loop
            Ada.Text_IO.Put_Line ("WORK " & Item'Image & " calls=" & Calls (Item)'Image &
              " inclusive_s=" & Totals (Item)'Image & " max_s=" & Worst (Item)'Image);
         end loop;
      end Check_Profile;
      Stats := Core.Observe (Service.all);
      Ada.Text_IO.Put_Line ("COLLECTIONS attempted=" & Stats.Collections.Attempted'Image & " completed=" & Stats.Collections.Completed'Image);
      Ada.Text_IO.Put_Line ("result access=" & Status'Image & " phase=" & Report.Phase'Image &
        " outcome=" & Report.Modules.Outcome'Image & " failure=" & Report.Modules.Failure'Image &
        " charged=" & Report.Modules.Charged'Image & " completed=" & Report.Modules.Completed'Image &
        " nodes=" & Stats.Objects'Image & " methods=" & Stats.Method_Bytes'Image &
        " values=" & Stats.Value_Objects'Image & " value-bytes=" & Stats.Value_Bytes'Image &
        " elements=" & Stats.Package_Elements'Image & " clocks=" & Clocks'Image & " delays=" & Delays'Image);
      Ada.Text_IO.Put_Line ("model nvs=" & IO.NVS_Reads'Image & " pci=" & IO.PCI_Reads'Image & " locks=" & IO.Lock_Entries'Image);
      if Report.Modules.Field_Failure.Present then
         Ada.Text_IO.Put_Line ("failed-field name=" & Report.Modules.Field_Failure.Name &
           " region=" & Report.Modules.Field_Failure.Region &
           " space=" & Report.Modules.Field_Failure.Space'Image &
           " write=" & Report.Modules.Field_Failure.For_Write'Image);
      end if;
   end;
end Reproduce;
