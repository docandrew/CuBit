with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
procedure Resources_Owner_Tests is
   package N is new AML_Namespace (32, Perform_Delay => AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   use type AML_Objects.Object_Kind;
   use type AML_Decode.Integer_Value;
   A : Arena;
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
      Check (AML_Objects.Kind (Value_Store (Snapshot (A)), Value.Object.ID) = AML_Objects.Buffer_Object);
      Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Value.Object.ID) = Data);
   end Verify;
   procedure Load_Buffer (Data : Bytes; Width : Integer_Width) is
   begin
      Reset (A,OK); Check (OK);
      Load (A,Bytes'(16#08#,66,85,70,48,16#11#,Byte(Data'Length+3),16#0A#,Byte(Data'Length)) & Data,
        Width,Loaded_Status); Check (Loaded_Status = Loaded); Read_Node(1,Source);
   end Load_Buffer;
   procedure Failure (Width : Integer_Width; Expected : Execution_Status) is
      Before : constant N.State := Snapshot(A);
   begin
      Concatenate_Resources_And_Attach (A,Width,Source,(Integer_Datum,16#79#,Ordinary_Integer),
        (Kind=>Detached_Result),Result_Value,Cell_Value,Status);
      Check(Status=Expected and then Snapshot(A)=Before);
      Check(Result_Value.Value_Kind=Integer_Datum and then Result_Value.Number=0);
      Check(Cell_Value.Value_Kind=Integer_Datum and then Cell_Value.Number=0);
   end Failure;
   procedure Quota (Width : Integer_Width; Byte_Pool : Boolean) is
      Last, Borrowed : Datum;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,84,82,48,16#11#,5,16#0A#,2,16#79#,0,
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
         Concatenate_Resources_And_Attach (A, Width, Borrowed, Borrowed,
           (Reference_Attachment, Ref, Argument_Indirect_Target), Result_Value, Cell_Value, Status);
         Check (Status = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
      Concatenate_Resources_And_Attach (A, Width, Borrowed, Borrowed,
        (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Result_Value.Object.ID = Cell_Value.Object.ID); Verify (Result_Value, Bytes'(16#79#,0));
      declare Before : constant N.State := Snapshot (A); begin
         Concatenate_Resources_And_Attach (A, Width, Borrowed, Borrowed,
           (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Value_Limit and then Snapshot (A) = Before);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
   end Quota;

begin
   for Width in Integer_Width loop
      Load_Buffer(Bytes'(16#79#,16#AA#,16#FF#),Width);
      Source.Object.Type_Code:=1; Source.Object.Size:=0;
      Concatenate_Resources_And_Attach(A,Width,Source,Source,(Kind=>Prepared_Cell_Copy),Result_Value,Cell_Value,Status);
      Check(Status=Returned and then Result_Value.Object.ID=Cell_Value.Object.ID and then Result_Value.Object.ID/=Source.Object.ID);
      Verify(Result_Value,Bytes'(16#79#,0));
      Load_Buffer(Bytes'(16#72#,16#79#,16#AA#,16#79#,0),Width);
      Concatenate_Resources_And_Attach(A,Width,Source,(Integer_Datum,16#79#,Ordinary_Integer),(Kind=>Detached_Result),Result_Value,Cell_Value,Status);
      Check(Status=Returned); Verify(Result_Value,Bytes'(16#72#,16#79#,16#AA#,16#79#,0));
      Load_Buffer(Bytes'(1=>16#79#),Width);Failure(Width,No_Resource_End_Tag);
      Load_Buffer(Bytes'(16#78#,0),Width);Failure(Width,Bad_Resource_Length);
      Load_Buffer(Bytes'(0,0),Width);Failure(Width,Invalid_Resource_Type);
      Load_Buffer(Bytes'(16#84#,0,0,16#79#,0),Width);Failure(Width,Resource_Buffer_Length);
      Saved:=Source;Source.Object.ID:=Source.Object.ID+1;Failure(Width,Unsupported_Value);
      Source:=Saved;Reset(A,OK);Check(OK);Failure(Width,Unsupported_Value);
      Quota(Width,False);Quota(Width,True);
   end loop;
   Ada.Text_IO.Put_Line("Resource owner checks" & Checks'Image);
end Resources_Owner_Tests;
