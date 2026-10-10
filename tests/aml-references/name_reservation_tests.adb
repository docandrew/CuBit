with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Name_Reservation_Tests is
   use type Integer_Value;
   use type AML_References.Reference;
   use type AML_References.Node_Incarnation;
   use type AML_References.Node_Position;
   use type AML_Objects.Allocation_Status;
   Max_Stamp : constant AML_References.Incarnation_Budget := 64;
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Capture, Max_Node_Incarnation => Max_Stamp);
   use NS; use NS.Owned;
   A, B : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Token, Later, Bad, Old, Temp_Token : Name_Reservation := No_Name_Reservation;
   Inner, Inner_Later : Name_Reservation := No_Name_Reservation;
   Recursive_Token : Name_Reservation := No_Name_Reservation;
   Capture_Number : Natural := 0;
   Base_Method_Count : constant Node_ID := 9;
   Nested_Method : constant Node_ID := 6;
   Ref, Other_Ref : Reference;
   Source, Foreign_Source, Ref_Source : AML_References.Object_Handle;
   ID : AML_Objects.Object_ID;
   Alloc : AML_Objects.Allocation_Status;
   OK : Boolean;
   L : Load_Status;
   S : Execution_Status := No_Return;
   V : Datum;
   R : Execution_Result;
   Prior : NS.State;
   Checks : Natural := 0;
   Round : Positive := 1;
   function Path (Name : String) return AML_Names.Name_Result is
      Data : Bytes (1 .. Name'Length);
   begin
      for J in Name'Range loop Data (J - Name'First + 1) := Character'Pos (Name (J)); end loop;
      return AML_Names.Read_Name (Data);
   end Path;
   function Method (Name : String; Code : Bytes; Arguments : Byte := 0) return Bytes is
      Header : Bytes (1 .. 4);
   begin
      for J in Header'Range loop Header (J) := Character'Pos (Name (J)); end loop;
      return Bytes'(16#14#, Byte (Code'Length + 6)) & Header & Bytes'(1 => Arguments) & Code;
   end Method;
   Temp_Name : constant Bytes := [84,69,77,80];
   Fixture : constant Bytes :=
     Method ("TEST", [16#5B#,16#33#,16#A4#,0]) &
     Method ("READ", Bytes'(16#5B#,16#33#,16#A4#) & Temp_Name) &
     Method ("PRES", Bytes'(16#5B#,16#33#,16#A4#,16#5B#,16#12#) & Temp_Name & Bytes'(1 => 0)) &
     Method ("REF0", Bytes'(16#5B#,16#33#,16#A4#,16#71#) & Temp_Name) &
     Method ("CHLD", [16#5B#,16#33#,16#A4#,0]) &
     Method ("NEST", [16#A0#,6,16#68#,16#5B#,16#33#,16#A4#,0,
       16#5B#,16#33#,78,69,83,84,1,67,72,76,68,16#5B#,16#33#,16#A4#,0], 1) &
     Method ("RDR0", [16#A4#,82,69,65,68]) &
     Method ("PRR0", [16#A4#,80,82,69,83]) &
     Method ("RFR0", [16#A4#,82,69,70,48]);
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image & " " & S'Image; end if;
   end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      Found : Lookup_Result;
      Saved_Node : Node_ID;
      Stamp : AML_References.Node_Incarnation;
   begin
      Value := 0; Available := True;
      Capture_Number := Capture_Number + 1;
      if Round in 4 .. 6 then
         Reserve_Name (A, Node_ID (Round - 2), Path ("TEMP"), Temp_Token, S);
         Check (S = Returned and then Reservation_Matches (A, Temp_Token));
         return;
      end if;
      if Round = 7 then
         case Capture_Number is
            when 1 =>
               Reserve_Name (A, Nested_Method, Path ("OUTR"), Token, S); Check (S = Returned);
            when 2 =>
               -- Recursive NEST invocation shares owner6, whose call count is2.
               Reserve_Name (A, Nested_Method, Path ("RECU"), Recursive_Token, S); Check (S = Returned);
               Check (Reservation_Matches (A, Token));
            when 3 =>
               -- The recursive return must not clean up owner6's reservations.
               Check (Reservation_Matches (A, Recursive_Token) and then Reservation_Matches (A, Token));
               Reserve_Name (A, 5, Path ("IONE"), Inner, S); Check (S = Returned);
               Reserve_Name (A, 5, Path ("ITWO"), Inner_Later, S); Check (S = Returned);
               Abort_Name (A, Inner, S); Check (S = Returned);
               Check (Reservation_Matches (A, Token) and then Reservation_Matches (A, Inner_Later));
            when 4 =>
               -- CHLD has returned, leaving outer and recursive owner6 state.
               Check (not Reservation_Matches (A, Inner_Later));
               Check (Reservation_Matches (A, Token) and then Reservation_Matches (A, Recursive_Token));
               Prior := Snapshot (A); Abort_Name (A, Inner_Later, S);
               Check (S = Unsupported_Value and then Snapshot (A) = Prior);
            when others => Check (False);
         end case;
         return;
      end if;
      if Round = 3 then
         for Attempt in 1 .. Natural (Max_Stamp) loop
            Prior := Snapshot (A);
            Reserve_Name (A, 1, Path ("LIMT"), Token, S);
            if S = Namespace_Limit then
               Check (Snapshot (A) = Prior and then Token = No_Name_Reservation);
               Check (Last_Incarnation (Snapshot (A)) = Max_Stamp);
               return;
            end if;
            Check (S = Returned);
            Abort_Name (A, Token, S); Check (S = Returned);
         end loop;
         Check (False); return;
      end if;
      if Round = 2 then
         Check (not Reservation_Matches (A, Old));
         Reserve_Name (A, 1, Path ("STAL"), Token, S); Check (S = Returned);
         Check (Reservation_Reference (Token) /= Reservation_Reference (Old));
         Check (AML_References.Named_Node (Reservation_Reference (Token)) = AML_References.Named_Node (Ref));
         Check (not Matches (A, Ref) and then Reservation_Reference (Token) /= Ref);
         Check (not Matches (A, Reservation_Reference (Old)));
         Prior := Snapshot (A); Abort_Name (A, Old, S);
         Check (S = Unsupported_Value and then Snapshot (A) = Prior);
         Abort_Name (A, Token, S); Check (S = Returned); return;
      end if;
      Reserve_Name (A, 1, Path ("TEMP"), Token, S);
      Check (S = Returned and then Reservation_Matches (A, Token));
      Found := Resolve (Snapshot (A), 1, Path ("TEMP"));
      Check (Found.Status = NS.Found and then Kind (A, Found.Node) = Uninitialized_Name_Object);
      Saved_Node := Found.Node;
      Make_Named_Reference (A, Natural (Found.Node), Ref, OK);
      Check (OK and then Ref = Reservation_Reference (Token) and then Matches (A, Ref));
      Resolve_Value (A, Ref, V, S); Check (S = Uninitialized and then V.Number = 0);
      Prior := Snapshot (A); Reserve_Name (A, 1, Path ("TEMP"), Bad, S);
      Check (S = Duplicate_Name and then Snapshot (A) = Prior and then Bad = No_Name_Reservation);
      Complete_Name (A, Token, Foreign_Source, S);
      Check (S = Unsupported_Value and then Snapshot (A) = Prior);
      Complete_Name (A, Token, AML_References.No_Object_Handle, S);
      Check (S = Unsupported_Value and then Snapshot (A) = Prior);
      Complete_Name (A, Bad, Source, S); Check (S = Unsupported_Value and then Snapshot (A) = Prior);
      Abort_Name (A, Bad, S); Check (S = Unsupported_Value and then Snapshot (A) = Prior);
      declare Before : constant NS.State := Snapshot (B); begin
         Abort_Name (B, Token, S); Check (S = Unsupported_Value and then Snapshot (B) = Before);
         Complete_Name (B, Token, Foreign_Source, S); Check (S = Unsupported_Value and then Snapshot (B) = Before);
      end;
      Reserve_Name (A, 1, Path ("LATE"), Later, S); Check (S = Returned);
      Prior := Snapshot (A); Stamp := Last_Incarnation (Prior);
      Abort_Name (A, Token, S);
      Check (S = Returned and then Node_Count (A) = Count (Prior));
      Check (not Present (A, Saved_Node) and then not Matches (A, Ref));
      Check (Reservation_Matches (A, Later) and then Last_Incarnation (Snapshot (A)) = Stamp);
      pragma Assert (Abort_Frame (Snapshot (A), Prior, Saved_Node));
      Complete_Name (A, Later, Source, S);
      Check (S = Returned and then not Reservation_Matches (A, Later));
      Resolve_Value (A, Reservation_Reference (Later), V, S);
      Check (S = Returned and then V.Value_Kind = Object_Datum and then V.Object.Size = 3);
      Prior := Snapshot (A); Abort_Name (A, Later, S); Check (S = Unsupported_Value and then Snapshot (A) = Prior);
      Reserve_Name (A, 1, Path ("TEMP"), Token, S); Check (S = Returned);
      Check (not Matches (A, Ref) and then Reservation_Reference (Token) /= Ref);
      Store_Reference_Value (A, Reservation_Reference (Token), Bits_64, (Integer_Datum,2, AML_Decode.Ordinary_Integer), S);
      Check (S = Returned and then Reservation_Matches (A, Token));
      Complete_Name (A, Token, Foreign_Source, S); Check (S = Returned);
      Resolve_Value (A, Reservation_Reference (Token), V, S); Check (S = Returned and then V.Number = 2);
      Reserve_Name (A, 1, Path ("RVAL"), Temp_Token, S); Check (S = Returned);
      Store_Reference_Value (A, Reservation_Reference (Temp_Token), Bits_64,
        (Reference_Datum, Reservation_Reference (Token)), S); Check (S = Returned);
      Complete_Name (A, Temp_Token, AML_References.No_Object_Handle, S); Check (S = Returned);
      Found := Resolve (Snapshot (A), 1, Path ("RVAL"));
      Make_Source (A, Data_Object (Snapshot (A), Found.Node), Ref_Source, OK); Check (OK);
      Reserve_Name (A, 1, Path ("RREF"), Temp_Token, S); Check (S = Returned);
      Prior := Snapshot (A); Complete_Name (A, Temp_Token, Ref_Source, S);
      Check (S = Unsupported_Value and then Snapshot (A) = Prior);
      Abort_Name (A, Temp_Token, S); Check (S = Returned);
      Reserve_Name (A, 1, Path ("STAL"), Old, S); Check (S = Returned);
      Other_Ref := Reservation_Reference (Old);
   end Capture;
begin
   Reset (A, OK); Check (OK); Reset (B, OK); Check (OK);
   Load (A, Fixture, Bits_64, L); Check (L = Loaded);
   Append (A, [1,2,3], ID, Alloc); Check (Alloc = AML_Objects.Allocated);
   Make_Source (A, ID, Source, OK); Check (OK);
   Append (B, [4], ID, Alloc); Check (Alloc = AML_Objects.Allocated);
   Make_Source (B, ID, Foreign_Source, OK); Check (OK);
   Prior := Snapshot (A); Reserve_Name (A, 1, Path ("TEMP"), Token, S);
   Check (S = Unsupported_Value and then Snapshot (A) = Prior);
   Reserve_Name (A, 0, Path ("TEMP"), Token, S); Check (S = Unsupported_Value and then Snapshot (A) = Prior);
   Invoke (A, Input, 1, [others => (Integer_Datum,0, AML_Decode.Ordinary_Integer)], 0, 100, R); Check (R.Status = Returned);
   Check (Node_Count (A) = Base_Method_Count and then not Reservation_Matches (A, Old) and then not Matches (A, Other_Ref));
   Prior := Snapshot (A); Complete_Name (A, Old, Source, S);
   Check (S = Unsupported_Value and then Snapshot (A) = Prior);
   Round := 2; Invoke (A, Input, 1, [others => (Integer_Datum,0, AML_Decode.Ordinary_Integer)], 0, 100, R); Check (R.Status = Returned);
   for Mode in 4 .. 6 loop
      Round := Mode; Capture_Number := 0;
      Invoke (A, Input, Node_ID (Mode + 3), [others => (Integer_Datum,0, AML_Decode.Ordinary_Integer)], 0, 100, R);
      Check (Capture_Number = 1);
      Check (R.Status = (case Mode is when 4 => Uninitialized, when 5 => Returned, when others => Reference_Returned));
      if Mode = 5 then Check (R.Value = Integer_Value'Last); end if;
      Check (not Reservation_Matches (A, Temp_Token));
      if Mode = 6 then Check (R.Ref = Reservation_Reference (Temp_Token) and then not Matches (A, R.Ref)); end if;
   end loop;
   Round := 7; Capture_Number := 0;
   Invoke (A, Input, Nested_Method, [others => (Integer_Datum,0, AML_Decode.Ordinary_Integer)], 1, 100, R);
   Check (R.Status = Returned and then Capture_Number = 4);
   Check (not Reservation_Matches (A, Token) and then not Reservation_Matches (A, Recursive_Token));
   Round := 3; Invoke (A, Input, 1, [others => (Integer_Datum,0, AML_Decode.Ordinary_Integer)], 0, 100, R); Check (R.Status = Returned);
   Reset (A, OK); Check (OK); Prior := Snapshot (A);
   Abort_Name (A, Old, S); Check (S = Unsupported_Value and then Snapshot (A) = Prior);
   Ada.Text_IO.Put_Line ("NAME-RESERVATION PASS" & Checks'Image);
end Name_Reservation_Tests;
