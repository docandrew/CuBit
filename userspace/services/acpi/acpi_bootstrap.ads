pragma Ada_2022;
with ACPI_Service;
with Firmware_Tables;
package ACPI_Bootstrap with SPARK_Mode is
   type Phase is (Receiving, Complete, Failed);
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is private;
   function Current (Session : State) return Phase;
   function Expected (Session : State) return Natural;
   function Installed (Session : State) return Natural;
   function Observe (Session : State) return ACPI_Service.Metrics with
     Post => Observe'Result.Tables <= Session.Table_Capacity
       and then Observe'Result.Bytes <= Session.Byte_Capacity;
   function Snapshot (Session : State) return ACPI_Service.Namespace.State;
   function Table_Info (Session : State; Index : Positive) return ACPI_Service.Table_Metadata
     with Pre => Index <= Installed (Session),
       Post => Table_Info'Result.Extent <= Session.Table_Byte_Limit;
   function Table_Byte (Session : State; Index : Positive; Offset : Natural) return Firmware_Tables.Byte
     with Pre => Index <= Installed (Session) and then Offset < Table_Info (Session, Index).Extent;
   -- The trusted provider advertises one DSDT first, followed by ordered SSDTs
   -- and other immutable description tables. Completion
   -- concerns this advertised snapshot, not AML activation or device presence.
   function Start
     (Table_Count : Natural;
      Table_Capacity : Positive := ACPI_Service.Max_Tables;
      Byte_Capacity : Positive := ACPI_Service.Max_Total_Bytes;
      Table_Byte_Limit : Positive := ACPI_Service.Max_Table_Bytes) return State with
     Post => Start'Result.Table_Capacity = Table_Capacity
       and then Start'Result.Byte_Capacity = Byte_Capacity
       and then Start'Result.Table_Byte_Limit = Table_Byte_Limit
       and then Installed (Start'Result) = 0 and then
       Current (Start'Result) = (if Table_Count in 1 .. Table_Capacity then Receiving else Failed)
       and then (if Current (Start'Result) = Receiving then Expected (Start'Result) = Table_Count);
   procedure Import_Table
     (Session : in out State; ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out ACPI_Service.Install_Status)
     with Post =>
       (if Current (Session'Old) /= Receiving then Session = Session'Old)
       and then (if Current (Session) = Complete then Current (Session'Old) = Complete);
   -- A failed table or premature finish is sticky for this session.
   procedure Finish (Session : in out State) with
     Post =>
       (if Current (Session'Old) = Receiving then
          Current (Session) = (if Installed (Session'Old) = Expected (Session'Old) then Complete else Failed)
        else Session = Session'Old);
private
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is record
      Core : ACPI_Service.State (Table_Capacity, Byte_Capacity, Table_Byte_Limit);
      Stage : Phase := Failed;
      Required : Natural := 0;
   end record with Type_Invariant =>
     State.Required <= State.Table_Capacity
     and then (if State.Stage in Receiving | Complete then
        State.Required > 0 and then ACPI_Service.Observe (State.Core).Tables <= State.Required)
     and then (if State.Stage = Complete then
        ACPI_Service.Observe (State.Core).Tables = State.Required);
end ACPI_Bootstrap;
