with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_References;
procedure Match_Owner_Tests is
   package N is new AML_Namespace (32, Perform_Delay => AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   use type AML_Decode.Integer_Value;
   A, Other : Arena;
   OK : Boolean;
   Loaded_Status : Load_Status;
   Status : Execution_Status;
   Pkg, Source, Saved, Cell : Datum;
   Value : Integer_Value;
   Visited : Match_Visit_Count;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & Status'Image; end if; end Check;
   procedure Read_Node (Node : Node_ID; Item : out Datum) is
      H : AML_References.Object_Handle;
   begin
      Make_Source (A, Data_Object (Snapshot (A), Node), H, OK); Check (OK);
      Read_Source (A, H, Item, Status); Check (Status = Returned);
   end Read_Node;
   procedure Run (Width : Integer_Width; Op : Match_Operation; Item : Datum; Start, Expected : Integer_Value;
     Expected_Status : Execution_Status := Returned) is
      Before : constant N.State := Snapshot (A);
   begin
      Match_Package (A, Width, Pkg, Item, (Integer_Datum, 0, Ordinary_Integer), Op, Always_True, Start, 10, Value, Visited, Status);
      if Status /= Expected_Status or else Value /= Expected then
         Ada.Text_IO.Put_Line ("RUN " & Op'Image & " start" & Start'Image & " actual" & Value'Image & " expected" & Expected'Image);
      end if;
      Check (Status = Expected_Status and then Value = Expected);
      Check (Snapshot (A) = Before);
   end Run;
begin
   for Width in Integer_Width loop
      Reset (A, OK); Check (OK);
      -- PKG0 contains 2,3,4; STR0 is hexadecimal text "3".
      Load (A, Bytes'(16#08#,80,75,71,48,16#12#,8,3,16#0A#,2,16#0A#,3,16#0A#,4,
        16#08#,83,84,82,48,16#0D#,51,0), Width, Loaded_Status); Check (Loaded_Status = Loaded);
      Read_Node (1, Pkg); Read_Node (2, Source);
      for Op in Match_Operation loop
         Run (Width, Op, (Integer_Datum,3,Ordinary_Integer), 0,
           (case Op is when Always_True | Less_Or_Equal | Less_Than => 0,
                       when Equal_To | Greater_Or_Equal => 1, when Greater_Than => 2));
      end loop;
      Run (Width, Equal_To, Source, 0, (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
      Source.Object.Size := 0; Source.Object.Type_Code := 1;
      Run (Width, Equal_To, Source, 0, (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
      Run (Width, Always_True, Source, 3, 0, Package_Limit);
      Run (Width, Always_True, Source, Integer_Value'Last, 0,
        (if Width = Bits_32 then Unsupported_Value else Package_Limit));
      Saved := Pkg; Pkg.Object.ID := Pkg.Object.ID + 1;
      Run (Width, Always_True, Source, 0, 0, Unsupported_Value); Pkg := Saved;
      Run (Width, Always_True, (Reference_Datum, AML_References.No_Reference), 0, 0, Unsupported_Value);
      declare Before : constant N.State := Snapshot (A); begin
         Match_Package (A, Width, Pkg, (Integer_Datum,3,Ordinary_Integer),
           (Integer_Datum,4,Ordinary_Integer), Greater_Or_Equal, Less_Than, 0, 10, Value, Visited, Status);
         Check (Status = Returned and then Value = 1 and then Snapshot (A) = Before);
      end;
      Match_Package (A, Width, Pkg, (Integer_Datum,9,Ordinary_Integer),
        (Integer_Datum,0,Ordinary_Integer), Equal_To, Always_True, 0, 2, Value, Visited, Status);
      Check (Status = Budget_Exceeded and then Value = 0 and then Visited = 2);
      Match_Package (A, Width, Pkg, (Integer_Datum,9,Ordinary_Integer),
        (Integer_Datum,0,Ordinary_Integer), Equal_To, Always_True, 0, 3, Value, Visited, Status);
      Check (Status = Returned and then Visited = 3);
      Match_Package (A, Width, Pkg, (Integer_Datum,9,Ordinary_Integer),
        (Integer_Datum,0,Ordinary_Integer), Equal_To, Always_True, 3, 0, Value, Visited, Status);
      Check (Status = Package_Limit and then Value = 0 and then Visited = 0);
      Match_Package (A, Width, Pkg, (Integer_Datum,9,Ordinary_Integer),
        (Integer_Datum,0,Ordinary_Integer), Equal_To, Always_True, 0, 0, Value, Visited, Status);
      Check (Status = Budget_Exceeded and then Value = 0 and then Visited = 0);
      -- Fill object pool without changing package/byte content; scan allocates nothing.
      loop
         To_String_And_Attach (A, Width, Source, 0, (Kind => Detached_Result), Cell, Saved, Status);
         exit when Status = Value_Limit;
         Check (Status = Returned);
      end loop;
      Run (Width, Equal_To, (Integer_Datum,3,Ordinary_Integer), 0, 1);
      Saved := Pkg; Reset (A, OK); Check (OK); Pkg := Saved;
      Run (Width, Always_True, (Integer_Datum,0,Ordinary_Integer), 0, 0, Unsupported_Value);
      -- Initialized nested package matches MTR, trailing NULL does not.
      Load (A, Bytes'(16#08#,80,75,71,48,16#12#,8,3,16#12#,3,1,1,16#0A#,9), Width, Loaded_Status);
      Check (Loaded_Status = Loaded); Read_Node (1,Pkg);
      Run (Width,Always_True,(Integer_Datum,0,Ordinary_Integer),0,0);
      Run (Width,Always_True,(Integer_Datum,0,Ordinary_Integer),2,
        (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
      Run (Width,Equal_To,(Integer_Datum,9,Ordinary_Integer),0,1);
      Reset (A,OK); Check (OK);
      Load (A, Bytes'(16#08#,80,75,71,48,16#12#,7,2,16#11#,2,0,16#0A#,9), Width, Loaded_Status);
      Check (Loaded_Status = Loaded); Read_Node (1,Pkg);
      Run (Width,Equal_To,(Integer_Datum,9,Ordinary_Integer),0,1);
      Reset (A,OK); Check (OK);
      Load (A, Bytes'(16#08#,78,86,65,76,1,16#08#,80,75,71,48,16#12#,6,1,78,86,65,76), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      declare Report : Initialization_Report; begin
         Initialize_Members (A,Report);
         Check (Report.Bound = 1 and then Report.Missing = 0 and then Report.Unsupported = 0);
      end;
      Read_Node (2,Pkg);
      declare R : AML_References.Reference; begin
         Make_Named_Reference (A,1,R,OK); Check (OK);
         Store_Reference_Value (A,R,Width,(Integer_Datum,2,Ordinary_Integer),Status,Direct_Target);
         Check (Status = Returned);
      end;
      Run (Width,Equal_To,(Integer_Datum,2,Ordinary_Integer),0,0);
   end loop;
   Reset (Other, OK); Check (OK);
   Load (Other, Bytes'(16#08#,80,75,71,48,16#12#,3,1,1), Bits_64, Loaded_Status); Check (Loaded_Status = Loaded);
   declare H : AML_References.Object_Handle; begin
      Make_Source (Other, Data_Object (Snapshot (Other),1), H, OK); Check (OK);
      Read_Source (Other,H,Pkg,Status); Check (Status = Returned);
      Run (Bits_64,Always_True,(Integer_Datum,0,Ordinary_Integer),0,0,Unsupported_Value);
   end;
   Ada.Text_IO.Put_Line ("Match owner checks" & Checks'Image);
end Match_Owner_Tests;
