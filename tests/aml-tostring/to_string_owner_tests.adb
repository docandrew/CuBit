with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
procedure To_String_Owner_Tests is
   package N is new AML_Namespace (32, Perform_Delay => AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   use type AML_Objects.Object_Kind;
   use type AML_Decode.Integer_Value;
   A, Other : Arena;
   OK : Boolean;
   Loaded_Status : Load_Status;
   Status : Execution_Status;
   Result_Value, Cell_Value, Source, Saved : Datum;
   Ref, Alias_Ref : AML_References.Reference;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & Status'Image; end if; end Check;
   procedure Read_Node (Node : Node_ID; Value : out Datum) is
      Handle : AML_References.Object_Handle;
   begin
      Make_Source (A, Data_Object (Snapshot (A), Node), Handle, OK); Check (OK);
      Read_Source (A, Handle, Value, Status); Check (Status = Returned);
   end Read_Node;
   procedure Verify (Value : Datum; Data : Bytes) is
   begin
      Check (Value.Value_Kind = Object_Datum);
      Check (AML_Objects.Kind (Value_Store (Snapshot (A)), Value.Object.ID) = AML_Objects.String_Object);
      Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Value.Object.ID) = Data);
   end Verify;
   procedure Reject (Value : Datum; Width : Integer_Width := Bits_64; Length : Integer_Value := 1) is
      Before : constant N.State := Snapshot (A);
   begin
      To_String_And_Attach (A, Width, Value, Length, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
      Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
   end Reject;
   procedure Setup (Width : Integer_Width) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,84,82,48,16#0D#,81,81,81,81,0,
        16#08#,66,85,70,48,16#11#,8,16#0A#,5,65,10,255,0,66,
        16#08#,68,83,84,48,16#0A#,42), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
   end Setup;
   procedure Quota (Width : Integer_Width; Byte_Pool : Boolean) is
      Last, Source_Copy : Datum;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,84,82,48,16#0D#,49,50,0,
        16#08#,68,83,84,48) &
        (if Byte_Pool then Bytes'(16#0D#,0) else Bytes'(16#0A#,42)), Width, Loaded_Status);
      Check (Loaded_Status = Loaded); Read_Node (1, Source_Copy);
      Make_Named_Reference (A, 2, Ref, OK); Check (OK);
      if Byte_Pool then
         To_Buffer_And_Attach (A, Width, (Integer_Datum, 1, Ordinary_Integer),
           (Kind => Detached_Result), Last, Cell_Value, Status); Check (Status = Returned);
         while Last.Object.Size < AML_Objects.Max_Bytes / 2 loop
            declare Part : constant Concatenation_Operand := (Data_Operand, Last); begin
               Concatenate_And_Attach (A, Width, Part, Part, (Kind => Detached_Result), Last, Cell_Value, Status);
               Check (Status = Returned);
            end;
         end loop;
         if Width = Bits_64 then
            Mid_And_Attach (A, Width, (Integer_Datum, 1, Ordinary_Integer), 0, 4,
              (Kind => Detached_Result), Last, Cell_Value, Status); Check (Status = Returned);
         end if;
         Check (Values_Used (A).Bytes = AML_Objects.Max_Bytes - 2);
      else
         while Values_Used (A).Objects < AML_Objects.Max_Objects - 1 loop
            To_String_And_Attach (A, Width, Source_Copy, 0,
              (Kind => Detached_Result), Last, Cell_Value, Status); Check (Status = Returned);
         end loop;
      end if;
      declare Before : constant N.State := Snapshot (A); begin
         To_String_And_Attach (A, Width, Source_Copy, 2,
           (Reference_Attachment, Ref, (if Byte_Pool then Explicit_Result_Target else Argument_Indirect_Target)), Result_Value, Cell_Value, Status);
         Check (Status = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
      To_String_And_Attach (A, Width, Source_Copy, 2, (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Result_Value.Object.ID = Cell_Value.Object.ID); Verify (Result_Value, Bytes'(49,50));
   end Quota;
   procedure Shared_Explicit (Width : Integer_Width) is
      Source_Buffer, Matching_Buffer, New_String, Named_String : Datum;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,66,85,70,48,16#11#,5,16#0A#,2,65,66,
        16#08#,73,78,84,48,16#0A#,42,
        16#08#,83,84,82,48,16#0D#,81,81,0,
        16#08#,66,85,70,49,16#11#,7,16#0A#,4,1,2,3,4), Width, Loaded_Status);
      Check (Loaded_Status = Loaded); Read_Node (1, Source_Buffer); Read_Node (4, Matching_Buffer);
      Make_Named_Reference (A, 4, Ref, OK); Check (OK);
      To_Buffer_And_Attach (A, Width, Source_Buffer, (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Read_Node (4, Cell_Value);
      Check (Cell_Value.Object.ID = Matching_Buffer.Object.ID and then Cell_Value.Object.ID /= Source_Buffer.Object.ID);
      Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Cell_Value.Object.ID) = Bytes'(65,66,0,0));
      for Target in Node_ID range 2 .. 3 loop
         Make_Named_Reference (A, Target, Ref, OK); Check (OK);
         To_Buffer_And_Attach (A, Width, Source_Buffer, (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status);
         Check (Status = Returned); Read_Node (Target, Cell_Value);
         Check (Cell_Value.Object.ID = Source_Buffer.Object.ID);
      end loop;
      Make_Named_Reference (A, 2, Ref, OK); Check (OK);
      To_String_And_Attach (A, Width, Source_Buffer, 2, (Reference_Attachment, Ref, Explicit_Result_Target), New_String, Cell_Value, Status);
      Check (Status = Returned); Read_Node (2, Named_String);
      Check (Named_String.Object.ID = New_String.Object.ID);
      Make_Index (A, New_String.Object.Source, 0, Alias_Ref, Status); Check (Status = Returned);
      Store_Integer (A, Alias_Ref, 90, Status); Check (Status = Returned);
      Read_Node (2, Named_String); Verify (Named_String, Bytes'(90,66));
      Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Source_Buffer.Object.ID) = Bytes'(65,66));
      Saved := Named_String;
      To_String_And_Attach (A, Width, Source_Buffer, 2, (Reference_Attachment, Ref, Explicit_Result_Target), New_String, Cell_Value, Status);
      Check (Status = Returned); Read_Node (2, Named_String);
      Check (Named_String.Object.ID = Saved.Object.ID and then New_String.Object.ID /= Named_String.Object.ID);
      Make_Index (A, New_String.Object.Source, 0, Alias_Ref, Status); Check (Status = Returned);
      Store_Integer (A, Alias_Ref, 89, Status); Check (Status = Returned);
      Read_Node (2, Named_String); Verify (Named_String, Bytes'(65,66)); Verify (New_String, Bytes'(89,66));
   end Shared_Explicit;
begin
   for Width in Integer_Width loop
      Setup (Width); Read_Node (2, Source);
      for Length in 0 .. 6 loop
         To_String_And_Attach (A, Width, Source, Integer_Value (Length), (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Returned);
         declare Data : constant Bytes := [65,10,255]; begin Verify (Result_Value, Data (1 .. Natural'Min (Length,3))); end;
      end loop;
      To_String_And_Attach (A, Width, (Integer_Datum, 16#44434241#, Ordinary_Integer), 4, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Verify (Result_Value, Bytes'(65,66,67,68));
      To_String_And_Attach (A, Width, (Integer_Datum, 16#4847464544434241#, Ordinary_Integer),
        (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last),
        (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Returned);
      if Width = Bits_32 then Verify (Result_Value, Bytes'(65,66,67,68));
      else Verify (Result_Value, Bytes'(65,66,67,68,69,70,71,72)); end if;
      Read_Node (1, Saved); Make_Index (A, Saved.Object.Source, 0, Alias_Ref, Status); Check (Status = Returned);
      Make_Named_Reference (A, 1, Ref, OK); Check (OK);
      To_String_And_Attach (A, Width, Source, 3, (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Read_Node (1, Cell_Value); Check (Cell_Value.Object.ID = Saved.Object.ID); Verify (Cell_Value, Bytes'(65,10,255));
      Store_Integer (A, Alias_Ref, 90, Status); Check (Status = Returned); Read_Node (1, Cell_Value); Verify (Cell_Value, Bytes'(90,10,255)); Verify (Result_Value, Bytes'(65,10,255));
      To_String_And_Attach (A, Width, (Integer_Datum, 16#44434241#, Ordinary_Integer), 4, (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Read_Node (1, Cell_Value); Check (Cell_Value.Object.ID = Saved.Object.ID); Verify (Cell_Value, Bytes'(65,66,67,68));
      To_String_And_Attach (A, Width, Source, 1, (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Read_Node (1, Cell_Value); Check (Cell_Value.Object.ID = Saved.Object.ID); Verify (Cell_Value, Bytes'(1 => 65));
      Reject ((Reference_Datum, AML_References.No_Reference));
      if Width = Bits_32 then Reject (Source, Width, 16#1_0000_0000#); end if;
      Reset (Other, OK); Check (OK);
      declare Before : constant N.State := Snapshot (Other); begin
         To_String_And_Attach (Other, Width, Source, 1, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Snapshot (Other) = Before);
      end;
      Load (Other, Bytes'(16#08#,79,84,72,48,0), Width, Loaded_Status); Check (Loaded_Status = Loaded);
      Make_Named_Reference (Other, 1, Alias_Ref, OK); Check (OK);
      declare Before : constant N.State := Snapshot (A); begin
         To_String_And_Attach (A, Width, Source, 1,
           (Reference_Attachment, Alias_Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      end;
      Saved := Source; Reset (A, OK); Check (OK); Reject (Saved);
      declare Before : constant N.State := Snapshot (A); begin
         To_String_And_Attach (A, Width, (Integer_Datum, 65, Ordinary_Integer), 1,
           (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      end;
      Quota (Width, False); Quota (Width, True); Shared_Explicit (Width);
   end loop;
   Ada.Text_IO.Put_Line ("ToString owner checks" & Checks'Image);
end To_String_Owner_Tests;
