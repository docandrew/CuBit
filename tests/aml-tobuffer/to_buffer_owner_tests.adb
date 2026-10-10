with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure To_Buffer_Owner_Tests is
   package NS is new AML_Namespace (16, Perform_Delay => AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   A, Other : Arena;
   OK : Boolean;
   S : Execution_Status;
   Loaded_Status : Load_Status;
   Result_Value, Cell_Value, Source, Other_Source : Datum;
   Ref : AML_References.Reference;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin Checks := Checks + 1; if not B then raise Program_Error with Checks'Image & S'Image; end if; end Check;
   procedure Read_Node (Node : Node_ID; Value : out Datum) is
      Handle : AML_References.Object_Handle;
   begin
      Make_Source (A, Data_Object (Snapshot (A), Node), Handle, OK); Check (OK);
      Read_Source (A, Handle, Value, S); Check (S = Returned);
   end Read_Node;
   procedure Reject (Item : Datum) is
      Before : constant NS.State := Snapshot (A);
   begin
      To_Buffer_And_Attach (A, Bits_64, Item, (Kind => Detached_Result), Result_Value, Cell_Value, S);
      Check (S = Unsupported_Value and then Snapshot (A) = Before);
      Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
      Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
   end Reject;
   procedure Byte_Quota (Width : Integer_Width) is
      Last : Datum;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,84,82,48,16#0D#,65,0,
        16#08#,68,83,84,48,16#0A#,42), Width, Loaded_Status);
      Check (Loaded_Status = Loaded); Read_Node (1, Source);
      Make_Named_Reference (A, 2, Ref, OK); Check (OK);
      To_Buffer_And_Attach (A, Width, (Integer_Datum, 1, Ordinary_Integer),
        (Kind => Detached_Result), Last, Cell_Value, S); Check (S = Returned);
      while Last.Object.Size < AML_Objects.Max_Bytes / 2 loop
         declare Part : constant Concatenation_Operand := (Data_Operand, Last); begin
            Concatenate_And_Attach (A, Width, Part, Part, (Kind => Detached_Result), Last, Cell_Value, S);
            Check (S = Returned);
         end;
      end loop;
      if Width = Bits_64 then
         To_Buffer_And_Attach (A, Bits_32, (Integer_Datum, 1, Ordinary_Integer),
           (Kind => Detached_Result), Result_Value, Cell_Value, S); Check (S = Returned);
      end if;
      Check (Values_Used (A).Bytes = AML_Objects.Max_Bytes - 3);
      declare Before : constant NS.State := Snapshot (A); begin
         To_Buffer_And_Attach (A, Width, Source, (Reference_Attachment, Ref, Argument_Indirect_Target),
           Result_Value, Cell_Value, S);
         Check (S = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
   end Byte_Quota;
   procedure Same_Cell (Width : Integer_Width; Frame_Target : Boolean) is
      Input : aliased AML_Table_Backing.State (1, 1);
      Returned_Value : Execution_Result;
      Target : constant Bytes := (if Frame_Target then Bytes'(16#71#,16#60#) else Bytes'(1 => 16#60#));
      Body_Code : constant Bytes := [16#70#,66,85,70,48,16#60#,16#96#,16#60#] & Target & [16#A4#,16#87#,16#60#];
      Definitions : constant Bytes := [16#08#,66,85,70,48,16#11#,3,1,7,
        16#14#,Byte (6 + Body_Code'Length),84,69,83,84,0] & Body_Code;
   begin
      Reset (A, OK); Check (OK); Load (A, Definitions, Width, Loaded_Status); Check (Loaded_Status = Loaded);
      -- Leave one slot for Store(BUF0,Local0); ToBuffer same-cell needs none.
      while Values_Used (A).Objects < AML_Objects.Max_Objects - 1 loop
         To_Buffer_And_Attach (A, Width, (Integer_Datum, 1, Ordinary_Integer),
           (Kind => Detached_Result), Result_Value, Cell_Value, S); Check (S = Returned);
      end loop;
      Invoke (A, Input, 2, [others => <>], 0, 100, Returned_Value);
      Check (Returned_Value.Status = Returned and then Returned_Value.Value = 1);
      Check (Values_Used (A).Objects = AML_Objects.Max_Objects);
   end Same_Cell;
begin
   for Width in Integer_Width loop
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,66,85,70,48,16#11#,4,16#0A#,2,65,
        16#08#,83,84,82,48,16#0D#,65,66,0,
        16#08#,68,83,84,48,16#0A#,42), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Read_Node (1, Source);
      declare Before : constant NS.State := Snapshot (A); begin
         To_Buffer_And_Attach (A, Width, Source, (Kind => Detached_Result), Result_Value, Cell_Value, S);
         Check (S = Returned and then Result_Value.Object.ID = Source.Object.ID and then Snapshot (A) = Before);
      end;
      To_Buffer_And_Attach (A, Width, Source, (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, S);
      Check (S = Returned and then Result_Value.Object.ID = Source.Object.ID and then Cell_Value.Object.ID /= Source.Object.ID);
      Read_Node (2, Source);
      To_Buffer_And_Attach (A, Width, Source, (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, S);
      Check (S = Returned and then Cell_Value.Object.ID = Result_Value.Object.ID);
      Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Result_Value.Object.ID) = Bytes'(65,66,0));
      To_Buffer_And_Attach (A, Width, (Integer_Datum, 16#1122#, Ordinary_Integer), (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, S);
      Check (S = Returned and then Cell_Value.Object.ID = Result_Value.Object.ID);
      Check (Result_Value.Object.Size = (if Width = Bits_32 then 4 else 8));
      Check (AML_Objects.Stored_Byte (Value_Store (Snapshot (A)), Result_Value.Object.ID, 0) = 16#22#);
      Make_Named_Reference (A, 3, Ref, OK); Check (OK);
      To_Buffer_And_Attach (A, Width, Source, (Reference_Attachment, Ref, Explicit_Result_Target), Result_Value, Cell_Value, S);
      Check (S = Returned and then Kind (Snapshot (A), 3) = Buffer_Object);
      Reject ((Reference_Datum, Ref));
      Reset (Other, OK); Check (OK);
      To_Buffer_And_Attach (Other, Width, (Integer_Datum, 1, Ordinary_Integer), (Kind => Detached_Result), Other_Source, Cell_Value, S);
      Check (S = Returned); Reject (Other_Source);
      declare Forged : Datum := Source; begin Forged.Object.ID := Forged.Object.ID + 1; Reject (Forged); end;
      -- Leave exactly one object for conversion, then force attachment clone failure.
      while Values_Used (A).Objects < AML_Objects.Max_Objects - 1 loop
         To_Buffer_And_Attach (A, Width, (Integer_Datum, 1, Ordinary_Integer), (Kind => Detached_Result), Result_Value, Cell_Value, S);
         Check (S = Returned);
      end loop;
      declare Before : constant NS.State := Snapshot (A); begin
         To_Buffer_And_Attach (A, Width, Source, (Reference_Attachment, Ref, Argument_Indirect_Target), Result_Value, Cell_Value, S);
         Check (S = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
      Reset (A, OK); Check (OK); Reject (Source);
      Same_Cell (Width, False); Same_Cell (Width, True); Byte_Quota (Width);
   end loop;
   Ada.Text_IO.Put_Line ("ToBuffer owner checks" & Checks'Image);
end To_Buffer_Owner_Tests;
