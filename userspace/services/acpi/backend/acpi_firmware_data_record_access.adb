pragma Ada_2022;
package body ACPI_Firmware_Data_Record_Access with SPARK_Mode is
   procedure Check_Selection (Caller : F.Word; Record_ID : F.Identifier;
      Offset : F.Word; Width : F.T.Access_Width; Accepted : out Boolean) is
   begin
      Accepted := Records.Selection_Valid (Rows,Caller,Records.Record_ID (Record_ID),Offset,Width);
   end Check_Selection;
   procedure Transfer_Record (Caller : F.Word; Record_ID : F.Identifier;
      Offset : F.Word; Width : F.T.Access_Width; For_Write : Boolean; Input : F.Word;
      Deadline : F.IPC.Time_Instant; Output : out F.Word; Status : out F.IPC.Outcome) is
      Attempt : Records.Operation;
      Result : Records.Outcome;
   begin
      -- The outer history permits exactly this first callback dispatch; the
      -- inner limited attempt prevents repeat use within the record owner.
      Records.Access_Record (Rows,Attempt,Caller,Records.Record_ID (Record_ID),
         Offset,Width,For_Write,Input,Deadline,Output,Result);
      case Result is
         when Records.Done => Status := F.IPC.Done;
         when Records.Transfer_Indeterminate | Records.Late_Completed =>
            Status := F.IPC.Indeterminate;
         when others => Output := 0; Status := F.IPC.Denied;
      end case;
   end Transfer_Record;
end ACPI_Firmware_Data_Record_Access;
