pragma Ada_2022;
with ACPI_Firmware_Data_Records;
with ACPI_Firmware_Data_Frames;
generic
   with package Records is new ACPI_Firmware_Data_Records (<>);
   Rows : in out Records.Context;
-- Adapter closes over the existing retained grant/mapping/record owner.
-- It creates no mapping or data authority. Use Transfer_Record only behind
-- the retained operation history and live data-scope controller. The record
-- owner's Scope_Usable callback must bind Scopes.Check_Active.
package ACPI_Firmware_Data_Record_Access with SPARK_Mode is
   package F renames ACPI_Firmware_Data_Frames;
   procedure Check_Selection (Caller : F.Word; Record_ID : F.Identifier;
      Offset : F.Word; Width : F.T.Access_Width; Accepted : out Boolean);
   procedure Transfer_Record (Caller : F.Word; Record_ID : F.Identifier;
      Offset : F.Word; Width : F.T.Access_Width; For_Write : Boolean; Input : F.Word;
      Deadline : F.IPC.Time_Instant; Output : out F.Word; Status : out F.IPC.Outcome);
end ACPI_Firmware_Data_Record_Access;
