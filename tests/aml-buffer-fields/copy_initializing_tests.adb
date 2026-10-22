with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Copy_Initializing_Tests is
   procedure Clock (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider, Clock);
   package O renames N.Owned;
   use type Integer_Value;
   use type N.State;
   use type N.Load_Status;
   use type N.Object_Kind;
   use type AML_Objects.Allocation_Status;
   use type AML_References.Node_Incarnation;
   A : O.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Current_Width : Integer_Width;
   Exhaust : Boolean;
   Checks, Calls : Natural := 0;
   TEMP : constant Bytes := [84,69,77,80];
   Fixture : constant Bytes := [16#14#,10,84,69,83,84,0,16#5B#,16#33#,16#A4#,0,
     16#08#,66,85,70,70,16#11#,3,1,65];
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Clock (Value : out Integer_Value; Available : out Boolean) is
      Source : AML_References.Object_Handle;
      Ref : O.Reference;
      Token : O.Name_Reservation;
      Copy, Readback : Datum;
      S : Execution_Status;
      OK : Boolean;
      Node : N.Node_ID;
      Stamp : AML_References.Node_Incarnation;
      ID : AML_Objects.Object_ID;
      Allocation : AML_Objects.Allocation_Status;
   begin
      Calls := Calls + 1;
      O.Make_Source (A, N.Data_Object (O.Snapshot (A), 2), Source, OK); Check (OK);
      O.Make_Index (A, Source, 0, Ref, S); Check (S = Returned);
      O.Reserve_Name (A, 1, AML_Names.Read_Name (TEMP), Token, S); Check (S = Returned);
      Node := N.Node_ID (AML_References.Named_Node (O.Reservation_Reference (Token)));
      Stamp := N.Incarnation_Of (O.Snapshot (A), Node);
      if Exhaust then
         while O.Values_Used (A).Objects < AML_Objects.Max_Objects loop
            O.Append (A, Bytes'(1 .. 0 => 0), ID, Allocation);
            Check (Allocation = AML_Objects.Allocated);
         end loop;
      end if;
      declare Before : constant N.State := O.Snapshot (A); begin
         O.Copy_And_Attach (A, (Named_Destination, 1, AML_Names.Read_Name (TEMP)),
           Current_Width, (Reference_Datum, Ref), Copy, S);
         if Exhaust then
            Check (S = Value_Limit and then O.Snapshot (A) = Before);
            Check (O.Reservation_Matches (A, Token));
            O.Abort_Name (A, Token, S); Check (S = Returned);
            Check (not O.Matches (A, O.Reservation_Reference (Token)));
         else
            Check (S = Returned and then Copy = (Reference_Datum, Ref));
            Check (O.Reservation_Matches (A, Token));
            Check (N.Kind (O.Snapshot (A), Node) = N.Reference_Object);
            Check (N.Incarnation_Of (O.Snapshot (A), Node) = Stamp);
            O.Complete_Name (A, Token, AML_References.No_Object_Handle, S);
            Check (S = Returned and then not O.Reservation_Matches (A, Token));
            Check (O.Matches (A, O.Reservation_Reference (Token)));
            Check (N.Kind (O.Snapshot (A), Node) = N.Reference_Object);
            Check (N.Incarnation_Of (O.Snapshot (A), Node) = Stamp);
            O.Resolve_Value (A, O.Reservation_Reference (Token), Readback, S);
            Check (S = Returned and then Readback = (Reference_Datum, Ref));
         end if;
      end;
      Value := 0; Available := True;
   end Clock;
   Loaded : N.Load_Status;
   OK : Boolean;
   Result : Execution_Result;
   Args : constant Value_Arguments := [others => (Integer_Datum, 0, Ordinary_Integer)];
begin
   for Width in Integer_Width loop
      Current_Width := Width;
      for Full in Boolean loop
         Exhaust := Full; Calls := 0;
         O.Reset (A, OK); Check (OK);
         O.Load (A, Fixture, Width, Loaded); Check (Loaded = N.Loaded);
         O.Invoke (A, Input, 1, Args, 0, 100, Result);
         Check (Result.Status = Returned and then Result.Value = 0);
         Check (Calls = 1 and then O.Valid (A));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Copy initializing checks" & Checks'Image);
end Copy_Initializing_Tests;
