with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
procedure Mid_Owner_Tests is
   package N is new AML_Namespace (32, Perform_Delay => AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   A, Other : Arena;
   OK : Boolean;
   Loaded_Status : Load_Status;
   Status : Execution_Status;
   Result_Value, Cell_Value, Source, Saved : Datum;
   Ref : AML_References.Reference;
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
      Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Value.Object.ID) = Data);
   end Verify;
   procedure Reject (Value : Datum; Width : Integer_Width := Bits_64;
                     Start : Integer_Value := 0; Count : Integer_Value := 1) is
      Before : constant N.State := Snapshot (A);
   begin
      Mid_And_Attach (A, Width, Value, Start, Count, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
      Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
   end Reject;
   procedure Setup (Width : Integer_Width) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,84,82,48,16#0D#,49,50,0,
        16#08#,66,85,70,48,16#11#,5,16#0A#,2,65,66,
        16#08#,68,83,84,48,16#0A#,42,
        16#08#,80,75,71,48,16#12#,3,1,0), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
   end Setup;
   procedure Quota (Width : Integer_Width; Byte_Pool : Boolean) is
      Last : Datum;
      Source_Copy : Datum;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,84,82,48,16#0D#,49,50,0,
        16#08#,68,83,84,48,16#0A#,42), Width, Loaded_Status);
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
            Mid_And_Attach (A, Width, (Integer_Datum, 1, Ordinary_Integer), 0, 1,
              (Kind => Detached_Result), Last, Cell_Value, Status); Check (Status = Returned);
         end loop;
      end if;
      declare Before : constant N.State := Snapshot (A); begin
         Mid_And_Attach (A, Width, Source_Copy, 0, 2,
           (Reference_Attachment, Ref, Argument_Indirect_Target), Result_Value, Cell_Value, Status);
         Check (Status = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
      -- Fresh cell capture shares the single remaining allocation.
      Mid_And_Attach (A, Width, Source_Copy, 0, 2, (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Result_Value.Object.ID = Cell_Value.Object.ID);
      if not Byte_Pool then
         declare Before : constant N.State := Snapshot (A); begin
            Mid_And_Attach (A, Width, Source_Copy, 2, 0, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
            Check (Status = Value_Limit and then Snapshot (A) = Before);
         end;
      end if;
   end Quota;
begin
   for Width in Integer_Width loop
      Setup (Width);
      Read_Node (1, Source);
      Mid_And_Attach (A, Width, Source, 0, 2, (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Result_Value.Object.ID /= Source.Object.ID
        and then Result_Value.Object.ID = Cell_Value.Object.ID);
      Verify (Result_Value, Bytes'(49,50));
      Check (Result_Value.Object.Type_Code = 2);
      Mid_And_Attach (A, Width, Source, 1, 1, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Verify (Result_Value, Bytes'(1 => 50));
      Mid_And_Attach (A, Width, Source, 2, 1, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Verify (Result_Value, Bytes'(1 .. 0 => 0));
      Saved := Result_Value;
      Mid_And_Attach (A, Width, Saved, 0, 0, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Result_Value.Object.ID /= Saved.Object.ID);
      Mid_And_Attach (A, Width, (Integer_Datum,16#1122_3344_5566_7788#,Ordinary_Integer), 1, 2,
        (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Verify (Result_Value, Bytes'(16#77#,16#66#));
      Mid_And_Attach (A, Width, (Integer_Datum,0,Ordinary_Integer), 0,
        (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last),
        (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Result_Value.Object.Size = (if Width = Bits_32 then 4 else 8));
      -- Implicit fixed target converts String slice to integer; result stays String.
      Make_Named_Reference (A, 3, Ref, OK); Check (OK);
      Mid_And_Attach (A, Width, Source, 0, 2, (Reference_Attachment,Ref,Direct_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Integer_Data (Snapshot (A),3) = 16#12#); Verify (Result_Value, Bytes'(49,50));
      declare
         Path : constant AML_Names.Name_Result :=
           (Kind => AML_Names.Accepted, Rooted => False, Parents => 0, Count => 1,
            Parts => [1 => "DST0", others => "____"], Consumed => 4);
      begin
         Mid_And_Attach (A, Width, Source, 0, 2, (Named_Attachment,0,Path), Result_Value, Cell_Value, Status);
         Check (Status = Returned and then Integer_Data (Snapshot (A),3) = 16#12#);
      end;
      Mid_And_Attach (A, Width, Source, 0, 2, (Reference_Attachment,Ref,Argument_Indirect_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Kind (Snapshot (A),3) = String_Object);
      Check (Data_Object (Snapshot (A),3) /= Result_Value.Object.ID);
      declare Before : constant N.State := Snapshot (A); begin
         Mid_And_Attach (A, Width, Source, 0, 2, (Reference_Attachment,Ref,Explicit_Result_Target), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      end;
      Read_Node (4, Saved); Reject (Saved); Reject ((Reference_Datum,Ref));
      Make_Index (A, Saved.Object.Source, 0, Ref, Status); Check (Status = Returned);
      Mid_And_Attach (A, Width, Source, 0, 2, (Reference_Attachment,Ref,Direct_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned);
      Resolve_Value (A, Ref, Saved, Status); Check (Status = Returned and then Saved.Object.ID /= Result_Value.Object.ID);
      Verify (Saved, Bytes'(49,50));
      Read_Node (2, Saved); Make_Index (A, Saved.Object.Source, 1, Ref, Status); Check (Status = Returned);
      Mid_And_Attach (A, Width, Source, 0, 2, (Reference_Attachment,Ref,Direct_Target), Result_Value, Cell_Value, Status);
      Check (Status = Returned); Verify (Saved, Bytes'(65,49));
      declare Before : constant N.State := Snapshot (A); begin
         Mid_And_Attach (A, Width, Saved, 2, 0, (Reference_Attachment,Ref,Direct_Target), Result_Value, Cell_Value, Status);
         Check (Status = Empty_Buffer and then Snapshot (A) = Before);
      end;
      Reject (Source, Bits_32, 16#1_0000_0000#, 1); Reject (Source, Bits_32, 0, 16#1_0000_0000#);
      declare Forged : Datum := Source; begin Forged.Object.ID := Forged.Object.ID + 1; Reject (Forged); end;
      Reset (Other, OK); Check (OK);
      Mid_And_Attach (Other, Width, (Integer_Datum,1,Ordinary_Integer),0,1,
        (Kind => Detached_Result), Saved, Cell_Value, Status); Check (Status = Returned); Reject (Saved);
      Reset (A, OK); Check (OK); Reject (Source);
      declare Before : constant N.State := Snapshot (A); begin
         Mid_And_Attach (A, Width, (Integer_Datum,1,Ordinary_Integer), 0, 1,
           (Reference_Attachment,Ref,Direct_Target), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
      Quota (Width, False); Quota (Width, True);
   end loop;
   Ada.Text_IO.Put_Line ("Mid owner checks" & Checks'Image);
end Mid_Owner_Tests;
