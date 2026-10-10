with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Namespace_Retirement_Tests is
   use type AML_Objects.State;
   use type AML_References.Node_Incarnation;
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package NS is new AML_Namespace (32, AML_Delays.Unavailable_Provider, Capture);
   use NS; use NS.Owned;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Width : Integer_Width;
   Calls, Checks : Natural := 0;
   S : Execution_Status;
   Prior : NS.State;
   subtype Sample_Index is Positive range 1 .. 5;
   type Name_List is array (Sample_Index) of String (1 .. 4);
   Names : constant Name_List := ["INT0", "STR0", "BUF0", "PKG0", "REF0"];
   Expected : constant array (Sample_Index) of Object_Kind :=
     [Integer_Object, String_Object, Buffer_Object, Package_Object, Reference_Object];
   Tokens : array (Sample_Index) of Name_Reservation;
   Nodes : array (Sample_Index) of Node_ID;
   Sources : array (Sample_Index) of AML_References.Object_Handle;
   Descriptors : array (Sample_Index) of Datum;
   Keeper : Name_Reservation;
   function Segment (Name : String) return Bytes is
      Result : Bytes (1 .. Name'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Result;
   end Segment;
   function Path (Name : String) return AML_Names.Name_Result is
     (AML_Names.Read_Name (Segment (Name)));
   function Method (Name : String; Code : Bytes) return Bytes is
     (Bytes'(16#14#, Byte (Code'Length + 6)) & Segment (Name) & Bytes'(1 => 0) & Code);
   Timer : constant Bytes := [16#5B#,16#33#];
   Fixture : constant Bytes :=
     Method ("OUTR", Timer & Segment ("INNR") & Timer & Bytes'(16#A4#,0)) &
     Method ("INNR", Timer & Bytes'(16#A4#,0)) &
     Bytes'(1 => 16#08#) & Segment ("INT0") & Bytes'(16#0A#,7) &
     Bytes'(1 => 16#08#) & Segment ("STR0") & Bytes'(16#0D#,65,0) &
     Bytes'(1 => 16#08#) & Segment ("BUF0") & Bytes'(16#11#,4,16#0A#,1,9) &
     Bytes'(1 => 16#08#) & Segment ("PKG0") & Bytes'(16#12#,3,1,1);
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image & " stage" & Calls'Image & S'Image; end if;
   end Check;
   procedure Check_Retired (I : Sample_Index; Before : NS.State) is
      Readback : Datum;
      Node : constant Node_ID := Nodes (I);
   begin
      Check (not Present (A, Node) and then Kind (A, Node) = Scope_Object);
      Check (Parent (Snapshot (A), Node) = Parent (Before, Node)
        and then Name (Snapshot (A), Node) = Name (Before, Node)
        and then Incarnation_Of (Snapshot (A), Node) = Incarnation_Of (Before, Node));
      Check (not Matches (A, Reservation_Reference (Tokens (I))));
      Resolve_Value (A, Reservation_Reference (Tokens (I)), Readback, S);
      Check (S = Unsupported_Value);
      Read_Source (A, Sources (I), Readback, S);
      Check (S = Returned and then Readback = Descriptors (I));
   end Check_Retired;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      Item : Datum;
      Ref : Reference;
      OK : Boolean;
      Found : Lookup_Result;
      Owner : Node_ID;
      Before : NS.State;
   begin
      Value := 0; Available := True; Calls := Calls + 1;
      if Calls <= 2 then
         Owner := (if Calls = 1 then 1 else 2);
         for I in Sample_Index loop
            Reserve_Name (A, Owner, Path (Names (I)), Tokens (I), S);
            Check (S = Returned);
            if I = Sample_Index'Last then
               Make_Named_Reference (A, 3, Ref, OK); Check (OK);
               Item := (Reference_Datum, Ref);
            else
               Found := Resolve (Snapshot (A), Root, Path (Names (I)));
               Check (Found.Status = NS.Found);
               Make_Source (A, Data_Object (Snapshot (A), Found.Node), Sources (I), OK); Check (OK);
               Read_Source (A, Sources (I), Item, S); Check (S = Returned);
            end if;
            Store_Reference_Value (A, Reservation_Reference (Tokens (I)), Width, Item, S);
            Check (S = Returned and then Reservation_Matches (A, Tokens (I)));
            Nodes (I) := Node_ID (AML_References.Named_Node (Reservation_Reference (Tokens (I))));
            Check (Kind (A, Nodes (I)) = Expected (I));
            Make_Source (A, Data_Object (Snapshot (A), Nodes (I)), Sources (I), OK); Check (OK);
            Read_Source (A, Sources (I), Descriptors (I), S); Check (S = Returned);
         end loop;
         -- The outer owner keeps a later slot live during inner cleanup.
         Reserve_Name (A, 1, Path ((if Calls = 1 then "KEEP" else "LAST")), Keeper, S);
         Check (S = Returned);
         if Calls = 1 then
            for I in Sample_Index loop
               Before := Snapshot (A);
               Abort_Name (A, Tokens (I), S); Check (S = Returned);
               pragma Assert (Abort_Frame (Snapshot (A), Before, Nodes (I)));
               Check (Value_Store (Snapshot (A)) = Value_Store (Before));
               Check_Retired (I, Before);
               Check (Reservation_Matches (A, Keeper));
            end loop;
         else
            Prior := Snapshot (A);
         end if;
      else
         Check (Calls = 3);
         Check (Value_Store (Snapshot (A)) = Value_Store (Prior));
         Check (Count (Snapshot (A)) = Count (Prior));
         for I in Sample_Index loop Check_Retired (I, Prior); end loop;
         Check (Reservation_Matches (A, Keeper));
      end if;
   end Capture;
begin
   for W in Integer_Width loop
      Width := W; Calls := 0;
      declare
         OK : Boolean;
         Loaded_Status : Load_Status;
         Result : Execution_Result;
      begin
         Reset (A, OK); Check (OK);
         Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
         Invoke (A, Input, 1, [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Result);
         Check (Result.Status = Returned and then Calls = 3);
         Check (Node_Count (A) = 6);
         Check (not Reservation_Matches (A, Keeper));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("NAMESPACE-RETIREMENT PASS" & Checks'Image);
end Namespace_Retirement_Tests;
