with AML_Delays;
with Ada.Text_IO;
with AML_Data;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Buffer_Completion_Tests is
   use type AML_Objects.Allocation_Status;
   use type Integer_Value;
   use type AML_Objects.State;
   use type AML_References.Reference;
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package NS is new AML_Namespace (32, AML_Delays.Unavailable_Provider, Capture);
   use NS; use NS.Owned;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Width : Integer_Width := Bits_32;
   type Scenario is (Unassigned_Metadata, Assigned_Metadata, Full_Objects,
     Full_Bytes, Preserved_Effects, Raw_Bytes);
   Current : Scenario := Unassigned_Metadata;
   Checks, Calls : Natural := 0;
   S : Execution_Status := No_Return;
   Token, Later : Name_Reservation := No_Name_Reservation;
   function Path (Name : String) return AML_Names.Name_Result is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return AML_Names.Read_Name (Data);
   end Path;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Current'Image & Width'Image & Checks'Image & S'Image; end if;
   end Check;
   procedure Complete (Data : Bytes; Count : AML_Data.Count_Result) is
   begin
      Complete_Runtime_Buffer (A, Token, Width, Data, Count, S);
   end Complete;
   Buffer_Data : constant Bytes := [16#11#, 6, 16#68#, 16#70#, 16#08#, 16#A4#, 16#11#];
   Good_Count : constant AML_Data.Count_Result := (Accepted, 2, 1);
   procedure Reject_Metadata is
      Before : constant NS.State := Snapshot (A);
   begin
      Complete (Buffer_Data, (Unsupported, 2, 1));
      Check (S = Unsupported_Value and then Snapshot (A) = Before and then Reservation_Matches (A, Token));
      Complete (Buffer_Data, (Accepted, 2, 0));
      Check (S = Unsupported_Value and then Snapshot (A) = Before and then Reservation_Matches (A, Token));
      Complete (Buffer_Data, (Accepted, 2, Buffer_Data'Length));
      Check (S = Unsupported_Value and then Snapshot (A) = Before and then Reservation_Matches (A, Token));
      Complete (Buffer_Data & Bytes'(1 => 0), Good_Count);
      Check (S = Unsupported_Value and then Snapshot (A) = Before and then Reservation_Matches (A, Token));
      Complete ([16#12#, 2, 0], Good_Count);
      Check (S = Unsupported_Value and then Snapshot (A) = Before and then Reservation_Matches (A, Token));
      Complete ([16#11#, 1], Good_Count);
      Check (S = Unsupported_Value and then Snapshot (A) = Before and then Reservation_Matches (A, Token));
      Complete ([16#11#, 6, 16#68#], Good_Count);
      Check (S = Unsupported_Value and then Snapshot (A) = Before and then Reservation_Matches (A, Token));
   end Reject_Metadata;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      Before : NS.State;
      Item : Datum;
      ID, Original : AML_Objects.Object_ID;
      Allocation : AML_Objects.Allocation_Status;
      Old : Reference;
      Position : Node_ID;
      Located : Lookup_Result;
   begin
      Value := 0; Available := True; Calls := Calls + 1;
      Reserve_Name (A, 1, Path ("TEMP"), Token, S);
      Check (S = Returned and then Reservation_Matches (A, Token));
      if Current in Assigned_Metadata | Full_Objects | Full_Bytes then
         Store_Reference_Value (A, Reservation_Reference (Token), Width, (Integer_Datum, 1, AML_Constant), S);
         Check (S = Returned);
         Position := Node_ID (AML_References.Named_Node (Reservation_Reference (Token)));
         Original := Data_Object (Snapshot (A), Position);
         if Current = Full_Objects then
            while Values_Used (A).Objects < AML_Objects.Max_Objects loop
               Append (A, Bytes'(1 .. 0 => 0), ID, Allocation);
               Check (Allocation = AML_Objects.Allocated);
            end loop;
         elsif Current = Full_Bytes then
            Append (A, Bytes'(1 .. AML_Objects.Max_Bytes - Values_Used (A).Bytes => 0), ID, Allocation);
            Check (Allocation = AML_Objects.Allocated);
         end if;
         Reject_Metadata;
         Before := Snapshot (A);
         Complete (Buffer_Data, (if Current in Full_Objects | Full_Bytes then
           AML_Data.Count_Result'(Accepted, Max_Buffer_Length + 1, 1) else Good_Count));
         Check (S = Returned and then not Reservation_Matches (A, Token));
         Check (Value_Store (Snapshot (A)) = Value_Store (Before));
         Check (Data_Object (Snapshot (A), Position) = Original);
         Resolve_Value (A, Reservation_Reference (Token), Item, S);
         Check (S = Returned and then Item.Value_Kind = Integer_Datum
           and then Item.Number = 1 and then Item.Origin = AML_Constant);
      elsif Current = Unassigned_Metadata then
         Reject_Metadata;
         Abort_Name (A, Token, S); Check (S = Returned);
      elsif Current = Preserved_Effects then
         Old := Reservation_Reference (Token);
         Replace_Value (A, 1, Path ("\MARK"), Width, (Integer_Datum, 9, Ordinary_Integer), S);
         Check (S = Returned);
         Reserve_Name (A, 1, Path ("LATE"), Later, S); Check (S = Returned);
         Store_Reference_Value (A, Reservation_Reference (Later), Width, (Integer_Datum, 7, Ordinary_Integer), S);
         Check (S = Returned);
         Complete_Name (A, Later, AML_References.No_Object_Handle, S); Check (S = Returned);
         Reject_Metadata;
         Before := Snapshot (A);
         Complete (Buffer_Data, (Accepted, Max_Buffer_Length + 1, 1));
         Check (S = Value_Limit and then Snapshot (A) = Before and then Reservation_Matches (A, Token));
         Abort_Name (A, Token, S); Check (S = Returned and then not Matches (A, Old));
         Check (Value_Store (Snapshot (A)) = Value_Store (Before));
         Resolve_Value (A, Reservation_Reference (Later), Item, S);
         Check (S = Returned and then Item.Number = 7);
         Located := Resolve (Snapshot (A), Root, Path ("MARK")); Check (Located.Status = Found);
         Check (AML_Objects.Integer_Data (Value_Store (Snapshot (A)), Data_Object (Snapshot (A), Located.Node)) = 9);
         Reserve_Name (A, 1, Path ("TEMP"), Token, S); Check (S = Returned);
         Check (not Matches (A, Old) and then Reservation_Reference (Token) /= Old);
         Abort_Name (A, Token, S); Check (S = Returned);
      else
         declare
            High : constant Bytes (Natural'Last - Buffer_Data'Length + 1 .. Natural'Last) := Buffer_Data;
         begin
            Complete (High, Good_Count); Check (S = Returned);
         end;
         Position := Node_ID (AML_References.Named_Node (Reservation_Reference (Token)));
         ID := Data_Object (Snapshot (A), Position);
         Check (AML_Objects.Length (Value_Store (Snapshot (A)), ID) = 4);
         Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), ID) = Bytes'(16#70#, 16#08#, 16#A4#, 16#11#));
         Check (Node_Count (A) = 3);
      end if;
   end Capture;
   OK : Boolean;
   Loaded_Status : Load_Status;
   Result : Execution_Result;
   Fixture : constant Bytes := [16#14#, 10, 84, 69, 83, 84, 0, 16#5B#, 16#33#, 16#A4#, 0,
     16#08#, 77, 65, 82, 75, 0];
begin
   for W in Integer_Width loop
      Width := W;
      for Case_ID in Scenario loop
         Current := Case_ID; Calls := 0;
         Reset (A, OK); Check (OK);
         Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
         Invoke (A, Input, 1, [others => (Integer_Datum, 0, Ordinary_Integer)], 0, 100, Result);
         Check (Result.Status = Returned and then Calls = 1);
         Check (not Reservation_Matches (A, Token));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("BUFFER-COMPLETION PASS" & Checks'Image);
end Buffer_Completion_Tests;
