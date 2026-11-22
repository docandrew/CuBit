------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted checks of the debugger engine's Rust support (DBG-001) against
--  the real debug-demo-rs ELF, with binutils as the oracle
--  (tests/debug-engine/run-rs.sh passes what readelf, nm, c++filt and
--  addr2line say).
--
--  Arguments: ELF, source root, build-id, a file of "mangled TAB demangled"
--  lines (c++filt's, crate hashes removed), then triples of
--  "demangled-function address line".
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with CuBit.Debug; use CuBit.Debug;
with CuBit.Debug.Demangle;
with CuBit.Debug.Dwarf;
with CuBit.Debug_Target;

procedure Hosted_Rust_Check is
   package DW renames CuBit.Debug.Dwarf;
   package DM renames CuBit.Debug.Demangle;
   use type DW.Load_Result;
   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Name : String; Condition : Boolean; Detail : String := "") is
   begin
      Checks := Checks + 1;
      if Condition then
         Put_Line ("debug-engine-rs: " & Name & " PASS");
      else
         Failures := Failures + 1;
         Put_Line ("debug-engine-rs: " & Name & " FAIL " & Detail);
      end if;
   end Check;

   function Contains (S, Part : String) return Boolean is (Ada.Strings.Fixed.Index (S, Part) > 0);

   S : Session;
   R : Outcome;
   Info : Program_Info;
begin
   if Argument_Count < 4 then
      Put_Line ("usage: hosted_rust_check ELF SOURCE_ROOT BUILD_ID SYMBOLS [FUNCTION ADDRESS LINE]...");
      Set_Exit_Status (Failure);
      return;
   end if;
   Attach (CuBit.Debug_Target.Hosted (Argument (1), Argument (2)), Observe, Forever, S, R);
   Check ("attach to the Rust demo ELF", R = Ok, Outcome'Image (R));
   if R /= Ok then
      Set_Exit_Status (Failure);
      return;
   end if;
   Describe (S, Info, R);
   Check ("build-id matches readelf", Info.Build_Id = Argument (3), Info.Build_Id);

   --  v0 demangling against c++filt, every Rust symbol of the program.
   declare
      F : File_Type;
      Total, Wrong : Natural := 0;
      Out_Name : DM.Output;
   begin
      Open (F, In_File, Argument (4));
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
            Tab : constant Natural := Ada.Strings.Fixed.Index (Line, "" & ASCII.HT);
         begin
            if Tab > Line'First then
               Total := Total + 1;
               DM.Rust (Line (Line'First .. Tab - 1), Out_Name);
               if not Out_Name.Valid or else Out_Name.Text (1 .. Out_Name.Length) /= Line (Tab + 1 .. Line'Last) then
                  Wrong := Wrong + 1;
                  if Wrong <= 5 then
                     Put_Line ("debug-engine-rs:   " & Line (Line'First .. Tab - 1));
                     Put_Line ("debug-engine-rs:   want " & Line (Tab + 1 .. Line'Last));
                     Put_Line ("debug-engine-rs:   got  " & Out_Name.Text (1 .. Out_Name.Length)
                               & (if Out_Name.Valid then "" else " (invalid)"));
                  end if;
               end if;
            end if;
         end;
      end loop;
      Close (F);
      Check ("v0 demangling agrees with c++filt on" & Total'Image & " symbols", Total > 100 and then Wrong = 0,
             Wrong'Image & " differ");
   end;

   --  Functions and lines against addr2line, names demangled.
   declare
      Data : CuBit.Debug_Target.Buffer_Access;
      Conn : CuBit.Debug_Target.Connection_Access;
      Status : CuBit.Debug_Target.Status;
      Path : String (1 .. 512);
      Path_Length : Natural;
      Img : DW.Image_Access;
      Loaded : DW.Load_Result;
      I : Positive := 5;
   begin
      CuBit.Debug_Target.Connect (CuBit.Debug_Target.Hosted (Argument (1), Argument (2)), 0, Conn, Status);
      CuBit.Debug_Target.Load_Debug_File (Conn.all, "", 0, Data, Path, Path_Length, Status);
      DW.Load (Data, Img, Loaded);
      Check ("DWARF 4 loads", Loaded = DW.Loaded and then Img.Units (0).Version = 4, DW.Load_Result'Image (Loaded));
      --  A line-0 row takes the line of the row before it.
      if Ada.Environment_Variables.Exists ("DEBUG_LINE_ZERO") then
         declare
            Pair : constant String := Ada.Environment_Variables.Value ("DEBUG_LINE_ZERO");
            Space : constant Natural := Ada.Strings.Fixed.Index (Pair, " ");
            Row : DW.Row;
            Found : Boolean;
         begin
            if Space > Pair'First then
               DW.Line_At (Img.all, Unsigned_64'Value ("16#" & Pair (Pair'First .. Space - 1) & "#"), Row, Found);
               Check ("a line-0 row shows the line before it (" & Pair (Space + 1 .. Pair'Last) & ")",
                      Found and then Row.Line = Natural'Value (Pair (Space + 1 .. Pair'Last)),
                      (if Found then Row.Line'Image else "none"));
            else
               Check ("a line-0 row is in the demo's line table", False, "none found");
            end if;
         end;
      end if;
      while I + 2 <= Argument_Count loop
         declare
            Name : constant String := Argument (I);
            Address : constant Unsigned_64 := Unsigned_64'Value ("16#" & Argument (I + 1) & "#");
            Line : constant Natural := Natural'Value (Argument (I + 2));
            F : constant Integer := DW.Function_At (Img.all, Address);
            D : DW.Die;
            Good, Found : Boolean;
            Row : DW.Row;
            A : DW.Attribute;
            Demangled : DM.Output;
         begin
            Good := F >= 0;
            if Good then
               DW.Read_Die (Img.all, Img.Functions (F).Die_Offset, D, Good);
               DW.Find_Inherited (Img.all, D, DW.AT_Linkage_Name, A, Found);
               if Good and then Found then
                  DM.Rust (DW.Text_Of (Img.all, A), Demangled);
               end if;
            end if;
            Check ("function at " & Name, Demangled.Valid and then Demangled.Text (1 .. Demangled.Length) = Name,
                   Demangled.Text (1 .. Demangled.Length));
            DW.Line_At (Img.all, Address, Row, Found);
            Check ("line of " & Name & " is" & Line'Image, Found and then Row.Line = Line,
                   (if Found then Row.Line'Image & " in " & DW.File_Name (Img.all, Row.File) else "none"));
         end;
         I := I + 3;
      end loop;
   end;

   --  Statics: values from the file's own data.
   declare
      Gs : Ref_List;
      Node : Value_Node;
      Kids : Node_List;
      function Find (Name : String) return Value_Ref is
      begin
         for G of Gs.Items (1 .. Gs.Count) loop
            if Image (G.Name) = Name then
               return G;
            end if;
         end loop;
         return Null_Ref;
      end Find;
      procedure Show (Name : String; Want_Kind : Value_Kind; Want_Display, Want_Type : String) is
         Ref : constant Value_Ref := Find (Name);
      begin
         Value (S, Ref, Node, R);
         Check (Name & " = " & Want_Display,
                Ref.Of_Type /= No_Type and then Node.Kind = Want_Kind and then Image (Node.Display) = Want_Display
                and then Image (Node.Type_Name) = Want_Type,
                Value_Kind'Image (Node.Kind) & " " & Image (Node.Display) & " : " & Image (Node.Type_Name));
      end Show;
   begin
      Globals (S, "", Gs, R);
      for G of Gs.Items (1 .. Gs.Count) loop
         Put_Line ("debug-engine-rs:   static " & Image (G.Name));
      end loop;
      Check ("the crate's own statics only", R = Ok and then Gs.Count >= 7
             and then (for all G of Gs.Items (1 .. Gs.Count) => Contains (Image (G.Name), "debug_demo_rs::")),
             Gs.Count'Image);
      Show ("debug_demo_rs::PROGRAM", Slice, """debug-demo-rs""", "&str");
      Children (S, Find ("debug_demo_rs::GENERATION"), 0, 4, Kids, R);
      for K of Kids.Items (1 .. Kids.Count) loop
         Put_Line ("debug-engine-rs:   GENERATION child " & Image (K.Name) & " " & Value_Kind'Image (K.Kind)
                   & " " & Image (K.Type_Name) & " = " & Image (K.Display));
      end loop;
      Show ("debug_demo_rs::GENERATION", Scalar, "0", "Atomic<u64>");
      Show ("debug_demo_rs::DEFAULT_SHAPE", Rust_Enum, "Rect { width: 3, height: 4 }", "Shape");
      Value (S, Find ("debug_demo_rs::DEFAULT_SHAPE"), Node, R);
      Check ("an enum's variant and fields", Image (Node.Variant) = "Rect" and then Node.Child_Count = 2,
             Image (Node.Variant) & Node.Child_Count'Image);
      Show ("debug_demo_rs::CALIBRATION", Record_Value, "{ sensor: 7, celsius: 21 }", "Reading");
      Show ("debug_demo_rs::EMPTY_TREE", Rust_Enum, "None", "Option<Box<Sensor>>");
      Value (S, Find ("debug_demo_rs::CALIBRATED"), Node, R);
      Check ("Option<&Reading> (niche) is Some", Node.Kind = Rust_Enum and then Image (Node.Variant) = "Some"
             and then Image (Node.Display)'Length > 5 and then Image (Node.Display) (1 .. 5) = "Some(",
             Image (Node.Display));
      Children (S, Find ("debug_demo_rs::CALIBRATED"), 0, 4, Kids, R);
      Check ("Some's payload is the reference", Kids.Count = 1 and then Kids.Items (1).Kind = Pointer
             and then Kids.Items (1).Has_Target
             and then Kids.Items (1).Target.Address = Find ("debug_demo_rs::CALIBRATION").Address,
             Kids.Count'Image);
      Show ("debug_demo_rs::OWNER_NAMES", Array_Value, "[""ada"", ""grace"", ""linus"", ""barbara""]", "[&str; 4]");
   end;

   --  The graph follows the niche Option's reference.
   declare
      type Graph_Access is access Graph_Result;
      G : constant Graph_Access := new Graph_Result;
      Gs : Ref_List;
      Roots : Ref_List;
   begin
      Globals (S, "", Gs, R);
      for X of Gs.Items (1 .. Gs.Count) loop
         if Image (X.Name) = "debug_demo_rs::CALIBRATED" then
            Roots.Count := 1;
            Roots.Items (1) := X;
         end if;
      end loop;
      Graph (S, Roots, 4, 16, G.all, R);
      Check ("graph: Some(&Reading) is a node for the Reading",
             G.Node_Total = 1 and then Image (G.Nodes (1).Value.Type_Name) = "Reading",
             G.Node_Total'Image & (if G.Node_Total > 0 then " " & Image (G.Nodes (1).Value.Type_Name) else ""));
   end;

   declare
      Page : Source_Page;
   begin
      Source (S, "main.rs", 1, Page, R);
      Check ("source main.rs", (R = Ok or else R = More) and then Page.Count > 10
             and then Contains (Image (Page.Lines (1)), "CuBit"), Outcome'Image (R));
   end;
   Detach (S);
   Put_Line ("debug-engine-rs: hosted" & Checks'Image & " checks," & Failures'Image & " failed");
   if Failures > 0 then
      Set_Exit_Status (Failure);
   end if;
end Hosted_Rust_Check;
