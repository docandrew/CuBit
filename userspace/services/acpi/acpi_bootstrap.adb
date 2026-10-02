pragma Ada_2022;
package body ACPI_Bootstrap with SPARK_Mode is
   use type ACPI_Service.Install_Status;
   function Current (Session : State) return Phase is (Session.Stage);
   function Expected (Session : State) return Natural is (Session.Required);
   function Installed (Session : State) return Natural is (ACPI_Service.Observe (Session.Core).Tables);
   function Observe (Session : State) return ACPI_Service.Metrics is (ACPI_Service.Observe (Session.Core));
   function Snapshot (Session : State) return ACPI_Service.Namespace.State is (ACPI_Service.Snapshot (Session.Core));
   function Table_Info (Session : State; Index : Positive) return ACPI_Service.Table_Metadata is
     (ACPI_Service.Table_Info (Session.Core, Index));
   function Table_Byte (Session : State; Index : Positive; Offset : Natural) return Firmware_Tables.Byte is
     (ACPI_Service.Table_Byte (Session.Core, Index, Offset));
   function Start
     (Table_Count : Natural;
      Table_Capacity : Positive := ACPI_Service.Max_Tables;
      Byte_Capacity : Positive := ACPI_Service.Max_Total_Bytes;
      Table_Byte_Limit : Positive := ACPI_Service.Max_Table_Bytes) return State is
   begin
      return
        (Table_Capacity => Table_Capacity, Byte_Capacity => Byte_Capacity,
         Table_Byte_Limit => Table_Byte_Limit,
         Core => ACPI_Service.Fresh (Table_Capacity, Byte_Capacity, Table_Byte_Limit),
         Stage => (if Table_Count in 1 .. Table_Capacity then Receiving else Failed),
         Required => (if Table_Count in 1 .. Table_Capacity then Table_Count else 0));
   end Start;
   procedure Import_Table
     (Session : in out State; ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out ACPI_Service.Install_Status) is
   begin
      if Session.Stage /= Receiving then Result := ACPI_Service.Wrong_Order; return; end if;
      if Installed (Session) = Session.Required then
         Session.Stage := Failed; Result := ACPI_Service.Table_Limit; return;
      end if;
      ACPI_Service.Install (Session.Core, ID, Kind, Data, Result);
      if Result /= ACPI_Service.Installed then Session.Stage := Failed; end if;
      -- The core admits at most one table per call. Verify the advertised
      -- count at this boundary before exposing Receiving/Complete again.
      if ACPI_Service.Observe (Session.Core).Tables > Session.Required then Session.Stage := Failed; end if;
   end Import_Table;
   procedure Finish (Session : in out State) is
   begin
      if Session.Stage = Receiving then
         Session.Stage := (if Installed (Session) = Session.Required then Complete else Failed);
      end if;
   end Finish;
end ACPI_Bootstrap;
