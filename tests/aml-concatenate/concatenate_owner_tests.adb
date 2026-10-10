with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Frame_Handles;
with AML_Table_Backing;
procedure Concatenate_Owner_Tests is
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type AML_Objects.State;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   A : Arena;
   OK : Boolean;
   Result_Value, Cell_Value : Datum;
   Status : Execution_Status;
   Checks : Natural := 0;
   Left : constant Concatenation_Operand := (Data_Operand, (Integer_Datum, 16#12#, Ordinary_Integer));
   Right : constant Concatenation_Operand := (Data_Operand, (Integer_Datum, 16#34#, Ordinary_Integer));
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image & Status'Image; end if;
   end Check;
   procedure Quota_Check (Width : Integer_Width; Byte_Quota : Boolean) is
      Loaded_Status : Load_Status;
      Ref, Package_Ref : AML_References.Reference;
      Package_Source : AML_References.Object_Handle;
      Last : Datum;
      Word_Bytes : constant Positive := (if Width = Bits_32 then 4 else 8);
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,68,83,84,48,16#0A#,42,
        16#08#,80,75,71,48,16#12#,3,1,0), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Make_Named_Reference (A, 1, Ref, OK); Check (OK);
      Make_Source (A, Data_Object (Snapshot (A), 2), Package_Source, OK); Check (OK);
      Make_Index (A, Package_Source, 0, Package_Ref, Status); Check (Status = Returned);
      Concatenate_And_Attach (A, Width, Left, Right, (Kind => Detached_Result), Last, Cell_Value, Status);
      Check (Status = Returned);
      if Byte_Quota then
         while Last.Object.Size < AML_Objects.Max_Bytes / 2 loop
            declare Source : constant Concatenation_Operand := (Data_Operand, Last); begin
               Concatenate_And_Attach (A, Width, Source, Source, (Kind => Detached_Result), Last, Cell_Value, Status);
               Check (Status = Returned);
            end;
         end loop;
         Check (Values_Used (A).Bytes = AML_Objects.Max_Bytes - 2 * Word_Bytes);
      else
         while Values_Used (A).Objects < AML_Objects.Max_Objects - 1 loop
            Concatenate_And_Attach (A, Width, Left, Right, (Kind => Detached_Result), Last, Cell_Value, Status);
            Check (Status = Returned);
         end loop;
      end if;
      declare
         type Attachment_Case is (Named_Case, Frame_Case, Package_Case);
         Destination : Concatenation_Destination;
      begin
         for Target in Attachment_Case loop
            case Target is
               when Named_Case => Destination := (Reference_Attachment, Ref, Argument_Indirect_Target);
               when Frame_Case => Destination := (Kind => Prepared_Cell_Copy);
               when Package_Case => Destination := (Reference_Attachment, Package_Ref, Direct_Target);
            end case;
            declare Before : constant NS.State := Snapshot (A); begin
               Concatenate_And_Attach (A, Width, Left, Right, Destination, Result_Value, Cell_Value, Status);
               Check (Status = Value_Limit and then Snapshot (A) = Before);
               Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
               Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
            end;
         end loop;
      end;
   end Quota_Check;

   procedure Authority_Check (Width : Integer_Width) is
      Loaded_Status : Load_Status;
      Ref : AML_References.Reference;
      Domain : AML_Frame_Handles.Invocation_Domain;
      Issued : Invocation_Status;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,78,65,77,48,1), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Make_Named_Reference (A, 1, Ref, OK); Check (OK);
      Reset (A, OK); Check (OK);
      declare Before : constant NS.State := Snapshot (A); begin
         Concatenate_And_Attach (A, Width, (Data_Operand, (Reference_Datum, Ref)), Right,
           (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      end;
      Begin_Invocation (A, Domain, Issued); Check (Issued = Available);
      Ref := AML_References.Bind_Frame (AML_Frame_Handles.Bind_Cell
        (AML_Frame_Handles.Bind_Frame (Domain, 1, 1), AML_Frame_Handles.Local_0));
      Begin_Invocation (A, Domain, Issued); Check (Issued = Available);
      declare Before : constant NS.State := Snapshot (A); begin
         Concatenate_And_Attach (A, Width, (Data_Operand, (Reference_Datum, Ref)), Right,
           (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      end;
      Reset (A, OK); Check (OK);
      declare Before : constant NS.State := Snapshot (A); begin
         Concatenate_And_Attach (A, Width, (Data_Operand, (Reference_Datum, Ref)), Right,
           (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      end;
   end Authority_Check;
   procedure Empty_Byte_Policy (Width : Integer_Width) is
      Code : constant Bytes := [16#08#,66,85,70,48,16#11#,3,1,1,
        16#70#,16#11#,2,0,16#88#,66,85,70,48,0,0,16#A4#,0];
      Loaded_Status : Load_Status;
      Input : aliased AML_Table_Backing.State (1, 1);
      Result : Execution_Result;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#14#, Byte (6 + Code'Length),84,69,83,84,0) & Code, Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 1, [others => <>], 0, 100, Result);
      Check (Result.Status = Empty_Buffer);
   end Empty_Byte_Policy;

   procedure Metadata_Check (Width : Integer_Width) is
      Input : aliased AML_Table_Backing.State (1, 1);
      Result : Execution_Result;
      Loaded_Status : Load_Status;
      procedure Run (Code : Bytes) is
      begin
         Reset (A, OK); Check (OK);
         Load (A, Bytes'(16#5B#,16#82#,5,68,69,86,48)
           & Bytes'(16#14#, Byte (6 + Code'Length),84,69,83,84,0) & Code, Width, Loaded_Status);
         Check (Loaded_Status = Loaded);
         Invoke (A, Input, 2, [others => <>], 0, 100, Result);
      end Run;
   begin
      Run (Bytes'(16#A4#,16#8E#,16#71#,68,69,86,48));
      Check (Result.Status = Returned and then Result.Value = 6);
      Run (Bytes'(16#A4#,16#83#,16#71#,68,69,86,48));
      Check (Result.Status = Unsupported_Value);
      Run (Bytes'(16#70#,0,16#71#,68,69,86,48,16#A4#,0));
      Check (Result.Status = Unsupported_Value);
   end Metadata_Check;

begin
   for Width in Integer_Width loop
      Metadata_Check (Width);
      Authority_Check (Width);
      Empty_Byte_Policy (Width);
      Quota_Check (Width, False);
      Quota_Check (Width, True);
      Reset (A, OK); Check (OK);
      Concatenate_And_Attach (A, Width, Left, Right, (Kind => Detached_Result), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Result_Value.Value_Kind = Object_Datum);
      declare
         Data : constant Bytes := AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Result_Value.Object.ID);
         Word_Bytes : constant Positive := (if Width = Bits_32 then 4 else 8);
      begin
         Check (Data'Length = 2 * Word_Bytes);
         Check (Data (1) = 16#12# and then Data (Word_Bytes + 1) = 16#34#);
         for I in Data'Range loop
            if I /= 1 and then I /= Word_Bytes + 1 then Check (Data (I) = 0); end if;
         end loop;
      end;
      Concatenate_And_Attach (A, Width, Left, Right, (Kind => Prepared_Cell_Copy), Result_Value, Cell_Value, Status);
      Check (Status = Returned and then Cell_Value.Value_Kind = Object_Datum);
      Check (Result_Value.Object.ID /= Cell_Value.Object.ID);
      Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Result_Value.Object.ID) = AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Cell_Value.Object.ID));
      declare Prior : constant AML_Objects.State := Value_Store (Snapshot (A)); begin
         Concatenate_And_Attach (A, Width, (Data_Operand, (Reference_Datum, AML_References.No_Reference)), Right,
           (Kind => Detached_Result), Result_Value, Cell_Value, Status);
         Check (Status = Unsupported_Value and then Value_Store (Snapshot (A)) = Prior);
         Check (Result_Value.Value_Kind = Integer_Datum and then Result_Value.Number = 0);
         Check (Cell_Value.Value_Kind = Integer_Datum and then Cell_Value.Number = 0);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Concatenate owner checks" & Checks'Image);
end Concatenate_Owner_Tests;
