with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
procedure Owner_Root_Tests is
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 2);
   package O renames AML_Objects;
   use N; use N.Owned;
   use type O.Allocation_Status;
   use type Object_Root_Set;
   A, B : Arena;
   Scratch : Root_Workspace;
   Keep, Expected : Object_Root_Set := [others => False];
   Root_Status : Root_Trace_Status;
   Status : Execution_Status;
   Allocation : O.Allocation_Status;
   ID, Other, Pack, Clone, Wrapper : O.Object_ID;
   Source, Foreign_Source, Package_Source, Cloned : AML_References.Object_Handle;
   Value, Broken, Copied : Datum;
   Ref, Cell, Foreign_Ref : Reference;
   Pin : Retained_Root;
   Retention : Retain_Status;
   Dropped : Release_Status;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Checks : Natural := 0;
   function Segment (Name : String) return Bytes is
      Result : Bytes (1 .. Name'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Result;
   end Segment;
   function Path (Name : String) return AML_Names.Name_Result is
     (AML_Names.Read_Name (Segment (Name)));
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image & Root_Status'Image & Status'Image; end if;
   end Check;
   procedure Check_Trace (Extra : Root_Values := []; Want : Root_Trace_Status := Roots_Traced) is
      Before : constant N.State := Snapshot (A);
   begin
      Trace_Owner_Roots (A, Extra, Scratch, Keep, Root_Status);
      Check (Root_Status = Want and then Keep = Expected and then Snapshot (A) = Before);
      if Want = Roots_Traced then pragma Assert (Owner_Roots_Traced (A, Extra, Keep, Scratch)); end if;
   end Check_Trace;
begin
   Check_Trace (Want => Uninitialized_Owner);
   Reset (A, OK); Check (OK); Reset (B, OK); Check (OK);
   Append (A, [1,2], ID, Allocation); Check (Allocation = O.Allocated);
   Append (A, [3], Other, Allocation); Check (Allocation = O.Allocated);
   Append (B, [9], Other, Allocation); Check (Allocation = O.Allocated);
   Make_Source (A, ID, Source, OK); Check (OK);
   Make_Source (B, Other, Foreign_Source, OK); Check (OK);
   Read_Source (A, Source, Value, Status); Check (Status = Returned);
   Check_Trace;
   Expected (ID) := True; Check_Trace ([Value]);
   Expected := [others => False];
   Broken := Value; Broken.Object.ID := ID + 1;
   Check_Trace ([Broken], Invalid_Root_Value);
   Check_Trace ([Value, Broken], Invalid_Root_Value);
   Broken := Value; Broken.Object.Source := Foreign_Source;
   Check_Trace ([Broken], Invalid_Root_Value);
   Check_Trace ([(Reference_Datum, AML_References.No_Reference)], Invalid_Root_Value);
   Retain (A, Value, Pin, Retention); Check (Retention = Retained);
   Expected (ID) := True; Check_Trace;
   Release (A, Pin, Dropped); Check (Dropped = Released);
   Expected := [others => False]; Check_Trace;
   Make_Index (A, Source, 0, Ref, Status); Check (Status = Returned);
   Expected (ID) := True; Check_Trace ([(Reference_Datum, Ref)]);
   Retain (A, (Reference_Datum, Ref), Pin, Retention); Check (Retention = Retained); Check_Trace;
   Release (A, Pin, Dropped); Check (Dropped = Released);
   Expected := [others => False];
   Make_Index (B, Foreign_Source, 0, Foreign_Ref, Status); Check (Status = Returned);
   Check_Trace ([(Reference_Datum, Foreign_Ref)]);
   -- Clone a package containing a reference, then overwrite the original cell.
   -- Only the supplied clone should keep its wrapper and orphan buffer alive.
   Load (A, Bytes'(16#08#,80,65,67,75,16#12#,3,1,0), Bits_64, Loaded_Status);
   Check (Loaded_Status = Loaded);
   Pack := Data_Object (Snapshot (A), 1);
   Make_Source (A, Pack, Package_Source, OK); Check (OK);
   Make_Index (A, Package_Source, 0, Cell, Status); Check (Status = Returned);
   Store_Reference_Value (A, Cell, Bits_64, (Reference_Datum, Ref), Status); Check (Status = Returned);
   Clone_Source (A, Package_Source, Cloned, Status); Check (Status = Returned);
   Clone := AML_References.Source (Cloned);
   Wrapper := O.Element (Value_Store (Snapshot (A)), Clone, 0);
   Store_Integer (A, Cell, 0, Status); Check (Status = Returned);
   Expected (Pack) := True;
   Expected (O.Element (Value_Store (Snapshot (A)), Pack, 0)) := True;
   Check_Trace;
   Read_Source (A, Cloned, Value, Status); Check (Status = Returned);
   Expected (Clone) := True; Expected (Wrapper) := True; Expected (ID) := True;
   Check_Trace ([Value]);
   Make_Index (A, Cloned, 0, Cell, Status); Check (Status = Returned);
   Check_Trace ([(Reference_Datum, Cell)]);
   Store_Reference_Value (A, Cell, Bits_64, (Reference_Datum, Foreign_Ref), Status);
   Check (Status = Returned);
   Expected (Wrapper) := False; Expected (ID) := False;
   Wrapper := O.Element (Value_Store (Snapshot (A)), Clone, 0);
   Expected (Wrapper) := True; Check_Trace ([Value]);
   -- Pending package roots survive replacement of the namespace attachment.
   Reset (A, OK); Check (OK); Expected := [others => False];
   Check_Trace ([Value], Invalid_Root_Value);
   Check_Trace ([(Reference_Datum, Ref)]);
   Load (A, Bytes'(16#08#,80,65,67,75,16#12#,6,1,76,65,84,69), Bits_64, Loaded_Status);
   Check (Loaded_Status = Loaded and then Pending_Members (A) = 1);
   Pack := Data_Object (Snapshot (A), 1);
   Expected (Pack) := True; Check_Trace;
   Copy_And_Attach (A, (Kind => Named_Destination, Scope => 0, Path => Path ("PACK")),
     Bits_64, (Integer_Datum,5,Ordinary_Integer), Copied, Status);
   Check (Status = Returned);
   Other := Data_Object (Snapshot (A), 1);
   Check (Other /= Pack); Expected (Other) := True; Check_Trace;
   declare Report : Initialization_Report; begin
      Initialize_Members (A, Report); Check (Report.Missing = 1 and then Pending_Members (A) = 0);
   end;
   Expected (Pack) := False; Check_Trace;
   Ada.Text_IO.Put_Line ("OWNER ROOTS" & Checks'Image);
end Owner_Root_Tests;
