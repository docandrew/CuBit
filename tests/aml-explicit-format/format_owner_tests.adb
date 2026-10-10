with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
procedure Format_Owner_Tests is
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
   function Octets (S : String) return Bytes is
      R : Bytes (1 .. S'Length);
   begin
      for I in S'Range loop R (I - S'First + 1) := Character'Pos (S (I)); end loop;
      return R;
   end Octets;
   procedure Setup (Width : Integer_Width) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,84,82,48,16#0D#,65,66,67,0,
        16#08#,66,85,70,48,16#11#,5,16#0A#,2,0,255,
        16#08#,68,83,84,48,16#0A#,42,
        16#08#,80,75,71,48,16#12#,4,1,16#0A#,7), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
   end Setup;
   procedure Rejected (Item : Datum; Target : Concatenation_Destination := (Kind => Detached_Result)) is
      Before : constant N.State := Snapshot (A);
   begin
      Format_String_And_Attach (A, Bits_64, Decimal_String, Item, Target, Result_Value, Cell_Value, Status);
      Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
      Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
   end Rejected;
   procedure Quota (Width : Integer_Width; Byte_Pool : Boolean) is
      Last, Borrowed : Datum;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,84,82,48,16#0D#,49,50,0,
        16#08#,68,83,84,48) &
        (if Byte_Pool then Bytes'(16#0D#,0) else Bytes'(16#0A#,42)), Width, Loaded_Status);
      Check (Loaded_Status = Loaded); Read_Node (1, Borrowed);
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
            To_String_And_Attach (A, Width, Borrowed, 0,
              (Kind => Detached_Result), Last, Cell_Value, Status); Check (Status = Returned);
         end loop;
      end if;
      declare Before : constant N.State := Snapshot (A); begin
         Format_String_And_Attach (A, Width, Decimal_String, (Integer_Datum, 15, Ordinary_Integer),
           (Reference_Attachment, Ref, (if Byte_Pool then Explicit_Result_Target else Argument_Indirect_Target)), Result_Value, Cell_Value, Status);
         Check (Status = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
      Format_String_And_Attach (A, Width, Decimal_String, (Integer_Datum, 15, Ordinary_Integer),
        (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Result_Value.Object.ID = Cell_Value.Object.ID); Verify (Result_Value, Octets ("15"));
      declare Before : constant N.State := Snapshot (A); begin
         Format_String_And_Attach (A, Width, Hexadecimal_String, Borrowed,
           (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Returned and then Result_Value.Object.ID = Borrowed.Object.ID and then Snapshot (A) = Before);
         Format_String_And_Attach (A, Width, Decimal_String, Borrowed,
           (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
         Check (Status = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
         Format_String_And_Attach (A, Width, Decimal_String, (Integer_Datum, 1, Ordinary_Integer),
           (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
   end Quota;
begin
   for Width in Integer_Width loop
      for Mode in Explicit_String_Mode loop
         Setup (Width); Read_Node (1, Source);
         declare Before : constant N.State := Snapshot (A); begin
            Format_String_And_Attach (A, Width, Mode, Source, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
            Check (Status = Returned and then Result_Value.Object.ID = Source.Object.ID and then Snapshot (A) = Before);
         end;
         Format_String_And_Attach (A, Width, Mode, Source, (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
         Check (Status = Returned and then Result_Value.Object.ID = Source.Object.ID and then Cell_Value.Object.ID /= Source.Object.ID);
         Saved := Cell_Value;
         Make_Index (A, Source.Object.Source, 0, Alias_Ref, Status); Check (Status = Returned);
         Store_Integer (A, Alias_Ref, 128, Status); Check (Status = Returned);
         Make_Index (A, Source.Object.Source, 1, Alias_Ref, Status); Check (Status = Returned);
         Store_Integer (A, Alias_Ref, 0, Status); Check (Status = Returned);
         Format_String_And_Attach (A, Width, Mode, Source, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Returned); Verify (Result_Value, Bytes'(128,0,67)); Verify (Saved, Octets ("ABC"));
         Read_Node (2, Source);
         Source.Object.Type_Code := 1; Source.Object.Size := 0;
         Format_String_And_Attach (A, Width, Mode, Source, (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
         Check (Status = Returned and then Result_Value.Object.ID = Cell_Value.Object.ID and then Result_Value.Object.ID /= Source.Object.ID);
         Verify (Result_Value, Octets ((if Mode = Decimal_String then "0,255" else "0x00,0xFF")));
         Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Source.Object.ID) = Bytes'(0,255));
         Format_String_And_Attach (A, Width, Mode, (Integer_Datum, Integer_Value'Last, Ordinary_Integer),
           (Kind => Detached_Result), Result_Value, Cell_Value, Status); Check (Status = Returned);
         if Mode = Decimal_String then Verify (Result_Value, Octets ((if Width = Bits_32 then "4294967295" else "18446744073709551615")));
         else Verify (Result_Value, Octets ((if Width = Bits_32 then "0xFFFFFFFF" else "0xFFFFFFFFFFFFFFFF"))); end if;
         Make_Named_Reference (A, 3, Ref, OK); Check (OK);
         Format_String_And_Attach (A, Width, Mode, (Integer_Datum, 65, Ordinary_Integer),
           (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status); Check (Status = Returned);
         Read_Node (3, Saved); Check (Saved.Object.ID = Result_Value.Object.ID);
         Format_String_And_Attach (A, Width, Mode, (Integer_Datum, 66, Ordinary_Integer),
           (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status); Check (Status = Returned);
         Read_Node (3, Cell_Value); Check (Cell_Value.Object.ID = Saved.Object.ID and then Cell_Value.Object.ID /= Result_Value.Object.ID);
         Format_String_And_Attach (A, Width, Mode, (Integer_Datum, 67, Ordinary_Integer),
           (Reference_Attachment, Ref, Argument_Indirect_Target), Result_Value, Cell_Value, Status); Check (Status = Returned);
         Read_Node (3, Cell_Value); Check (Cell_Value.Object.ID /= Result_Value.Object.ID);
         Read_Node (4, Source); Make_Index (A, Source.Object.Source, 0, Ref, Status); Check (Status = Returned);
         Format_String_And_Attach (A, Width, Mode, (Integer_Datum, 65, Ordinary_Integer),
           (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status); Check (Status = Returned);
         Resolve_Value (A, Ref, Cell_Value, Status); Check (Status = Returned and then Cell_Value.Object.ID /= Result_Value.Object.ID);
         Verify (Cell_Value, Octets ((if Mode = Decimal_String then "65" else "0x41")));
         Read_Node (2, Source); Make_Index (A, Source.Object.Source, 0, Ref, Status); Check (Status = Returned);
         Format_String_And_Attach (A, Width, Mode, (Integer_Datum, 65, Ordinary_Integer),
           (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status); Check (Status = Returned);
         Resolve_Value (A, Ref, Cell_Value, Status); Check (Status = Returned and then Cell_Value.Number = (if Mode = Decimal_String then 54 else 48));
      end loop;
      Setup (Width); Make_Named_Reference (A, 3, Ref, OK); Check (OK);
      Format_String_And_Attach (A, Width, Decimal_String, (Integer_Datum, 65, Ordinary_Integer),
        (Reference_Attachment, Ref, Direct_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Read_Node (3, Saved);
      Check (Saved.Value_Kind = Integer_Datum and then Saved.Number = 16#65#);
      Format_String_And_Attach (A, Width, Decimal_String, (Integer_Datum, 65, Ordinary_Integer),
        (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Read_Node (3, Saved); Verify (Saved, Octets ("65"));
      Check (Saved.Object.ID = Result_Value.Object.ID);
      Quota (Width, False); Quota (Width, True);
   end loop;
   Setup (Bits_64); Read_Node (2, Source); Saved := Source;
   Reset (Other, OK); Check (OK);
   Load (Other, Bytes'(16#08#,66,85,70,48,16#11#,4,16#0A#,1,42), Bits_64, Loaded_Status); Check (Loaded_Status = Loaded);
   declare H : AML_References.Object_Handle; begin
      Make_Source (Other, Data_Object (Snapshot (Other), 1), H, OK); Check (OK);
      Read_Source (Other, H, Source, Status); Check (Status = Returned); Rejected (Source);
   end;
   Source := Saved; Source.Object.ID := Source.Object.ID + 1; Rejected (Source);
   Rejected ((Reference_Datum, AML_References.No_Reference)); Read_Node (4, Source); Rejected (Source);
   Make_Named_Reference (A, 3, Ref, OK); Check (OK);
   Reset (A, OK); Check (OK); Rejected (Saved);
   Rejected ((Integer_Datum, 1, Ordinary_Integer), (Reference_Attachment, Ref, Explicit_Result_Target));
   Setup (Bits_64); Read_Node (2, Source);
   Rejected (Source, (Reference_Attachment, AML_References.No_Reference, Explicit_Result_Target));
   declare
      Last : Datum;
   begin
      To_Buffer_And_Attach (A, Bits_64, (Integer_Datum, 1, Ordinary_Integer), (Kind => Detached_Result), Last, Cell_Value, Status); Check (Status = Returned);
      while Last.Object.Size < 16_384 loop
         declare Part : constant Concatenation_Operand := (Data_Operand, Last); begin
            Concatenate_And_Attach (A, Bits_64, Part, Part, (Kind => Detached_Result), Last, Cell_Value, Status); Check (Status = Returned);
         end;
      end loop;
      declare Before : constant N.State := Snapshot (A); begin
         Format_String_And_Attach (A, Bits_64, Hexadecimal_String, Last, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
      end;
   end;
   Ada.Text_IO.Put_Line ("Format owner checks" & Checks'Image);
end Format_Owner_Tests;
