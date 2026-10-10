with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_References;
with AML_Table_Backing;
procedure Sync_Metadata_Tests is
   use type Byte;
   use type Integer_Value;
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   A, Foreign : Arena;
   OK : Boolean;
   Loaded_Status : Load_Status;
   Checks : Natural := 0;
   Event_Code : constant Bytes := [16#5B#, 2, 69, 86, 84, 48];
   Mutex_Code : constant Bytes := [16#5B#, 1, 77, 84, 88, 48];
   Input : aliased AML_Table_Backing.State (1, 1);
   Ref : AML_References.Reference;
   Metadata : Reference_Metadata;
   Result : Execution_Result;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Reject (Code : Bytes; Expected : Load_Status) is
      Before : constant NS.State := Snapshot (A);
   begin
      Load (A, Code, Bits_64, Loaded_Status);
      Check (Loaded_Status = Expected and then Snapshot (A) = Before);
   end Reject;
   procedure Run (Declaration, Body_Code : Bytes; Expected : Execution_Status;
                  Number : Integer_Value := 0) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Declaration & Bytes'[16#14#, Byte (6 + Body_Code'Length),
        84, 69, 83, 84, 0] & Body_Code, Bits_64, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 2, [others => <>], 0, 100, Result);
      Check (Result.Status = Expected);
      if Expected = Returned then Check (Result.Value = Number); end if;
   end Run;
begin
   for Flags in Byte loop
      Reset (A, OK); Check (OK);
      Load (A, Event_Code & Mutex_Code & Bytes'[1 => Flags], Bits_64, Loaded_Status);
      Check (Loaded_Status = Loaded and then Node_Count (A) = 2);
      Check (Kind (A, 1) = Event_Object and then Kind (A, 2) = Mutex_Object);
      Check (Mutex_Data (A, 2).Raw_Flags = Flags);
      Check (Mutex_Data (A, 2).Level = NS.Sync_Level (Flags mod 16));
      Check (Mutex_Data (A, 2).Canonical = (Flags <= 15));
      Check (Values_Used (A).Objects = 0 and then Values_Used (A).Bytes = 0
        and then Values_Used (A).Elements = 0);
   end loop;
   Make_Named_Identity (A, 1, Ref, OK); Check (OK);
   Describe_Named_Identity (A, Ref, Metadata);
   Check (Metadata.Kind = Metadata_Only and then Metadata.Object_Type = Event_Metadata);
   Describe_Named_Identity (Foreign, Ref, Metadata);
   Check (Metadata.Kind = Invalid_Reference);
   declare
      Before : constant NS.State := Snapshot (A);
      Stored : Execution_Status;
   begin
      Store_Reference_Value (A, Ref, Bits_64, (Integer_Datum, 42, Ordinary_Integer), Stored);
      Check (Stored = Unsupported_Value and then Snapshot (A) = Before);
   end;
   Reset (A, OK); Check (OK);
   Describe_Named_Identity (A, Ref, Metadata);
   Check (Metadata.Kind = Invalid_Reference);
   Reject (Bytes'[1 => 16#5B#], Unsupported_Opcode);
   Reject (Bytes'[16#5B#,2], Bad_Name);
   Reject (Bytes'[16#5B#,2,0], Bad_Name);
   Reject (Mutex_Code, Bad_Integer);
   Reject (Event_Code & Event_Code, Duplicate_Name);
   Reject (Event_Code & Bytes'[16#5B#,1,69,86,84,48,0], Duplicate_Name);
   Reject (Event_Code & Bytes'[16#10#,5,69,86,84,48], Missing_Scope);
   Reject (Bytes'[16#5B#,2,16#5E#,69,86,84,48], Missing_Scope);
   declare High : constant Bytes (Positive'Last - Event_Code'Length + 1 .. Positive'Last) := Event_Code; begin
      Load (A, High, Bits_32, Loaded_Status); Check (Loaded_Status = Loaded);
   end;
   Reset (A, OK); Check (OK);
   for Suffix in 0 .. 15 loop
      Load (A, Bytes'[16#5B#,2,69,86,84,Byte (65 + Suffix)], Bits_64, Loaded_Status);
      Check (Loaded_Status = Loaded);
   end loop;
   Reject (Mutex_Code & Bytes'[1 => 0], Storage_Full);
   Reset (A, OK); Check (OK);
   -- Scoped declarations share existing exact parent and leaf rules.
   Load (A, Bytes'[16#5B#,16#82#,11,68,69,86,48] & Event_Code,
     Bits_64, Loaded_Status);
   Check (Loaded_Status = Loaded and then Node_Count (A) = 2 and then Parent (Snapshot (A), 2) = 1);
   Reset (A, OK); Check (OK);
   Load (A, Bytes'[16#5B#,2,16#5C#,69,86,84,48], Bits_64, Loaded_Status);
   Check (Loaded_Status = Loaded and then Parent (Snapshot (A), 1) = Root);
   Reset (A, OK); Check (OK);
   Load (A, Bytes'[16#5B#,16#82#,12,68,69,86,48,16#5B#,2,16#5E#,69,86,84,48], Bits_64, Loaded_Status);
   Check (Loaded_Status = Loaded and then Parent (Snapshot (A), 2) = Root);
   for Multi in Boolean loop
      Reset (A, OK); Check (OK);
      Load (A, Bytes'[16#5B#,16#82#,5,68,69,86,48], Bits_64, Loaded_Status);
      Check (Loaded_Status = Loaded);
      if Multi then
         Load (A, Bytes'[16#5B#,2,16#5C#,16#2F#,2,68,69,86,48,69,86,84,48], Bits_64, Loaded_Status);
      else
         Load (A, Bytes'[16#5B#,2,16#5C#,16#2E#,68,69,86,48,69,86,84,48], Bits_64, Loaded_Status);
      end if;
      Check (Loaded_Status = Loaded and then Parent (Snapshot (A), 2) = 1);
   end loop;
   Reset (A, OK); Check (OK);
   Reject (Bytes'[16#5B#,16#82#,11,68,69,86,48] & Mutex_Code & Bytes'[1 => 0], Bad_Integer);
   Run (Event_Code, Bytes'[16#A4#,16#8E#,69,86,84,48], Returned, 7);
   Run (Mutex_Code & Bytes'[1 => 15], Bytes'[16#A4#,16#8E#,77,84,88,48], Returned, 9);
   Run (Event_Code, Bytes'[16#A4#,16#8E#,16#71#,69,86,84,48], Returned, 7);
   Run (Mutex_Code & Bytes'[1 => 0], Bytes'[16#A4#,16#8E#,16#71#,77,84,88,48], Returned, 9);
   -- No static metadata success may imply a working lock or wait operation.
   Run (Mutex_Code & Bytes'[1 => 0], Bytes'[16#A4#,16#5B#,16#23#,77,84,88,48,0,0], Unsupported);
   Run (Event_Code, Bytes'[16#A4#,16#5B#,16#25#,69,86,84,48,0], Unsupported);
   Run (Event_Code, Event_Code & Bytes'[16#A4#,0], Unsupported);
   Run (Event_Code, Bytes'[16#9D#,1,69,86,84,48,16#A4#,0], Unsupported_Value);
   Ada.Text_IO.Put_Line ("Sync metadata checks" & Checks'Image);
end Sync_Metadata_Tests;
