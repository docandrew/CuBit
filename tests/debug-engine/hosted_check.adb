------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted checks of the debugger engine (DBG-001) against the real
--  debug-demo ELF, with binutils as the oracle (tests/debug-engine/run.sh
--  passes what readelf, nm and addr2line say).
--
--  Arguments: ELF, source root, build-id, then triples of
--  "symbol address line" for functions, as nm and addr2line give them.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with CuBit.Debug; use CuBit.Debug;
with CuBit.Debug.Dwarf;
with CuBit.Debug_Target;

procedure Hosted_Check is
   package DW renames CuBit.Debug.Dwarf;
   use type DW.Load_Result;
   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Name : String; Condition : Boolean; Detail : String := "") is
   begin
      Checks := Checks + 1;
      if Condition then
         Put_Line ("debug-engine: " & Name & " PASS");
      else
         Failures := Failures + 1;
         Put_Line ("debug-engine: " & Name & " FAIL " & Detail);
      end if;
   end Check;

   function Contains (S, Part : String) return Boolean is
     (Ada.Strings.Fixed.Index (S, Part) > 0);

   S : Session;
   R : Outcome;
   Info : Program_Info;
   Globals_List : Ref_List;
   Node : Value_Node;
   Kids : Node_List;
   World, Names : Value_Ref;
   Have_World, Have_Names : Boolean := False;
begin
   if Argument_Count < 3 then
      Put_Line ("usage: hosted_check ELF SOURCE_ROOT BUILD_ID [SYMBOL ADDRESS LINE]...");
      Set_Exit_Status (Failure);
      return;
   end if;
   Attach (CuBit.Debug_Target.Hosted (Argument (1), Argument (2)), Observe, Forever, S, R);
   Check ("attach to the demo ELF", R = Ok, Outcome'Image (R));
   if R /= Ok then
      Set_Exit_Status (Failure);
      return;
   end if;
   Describe (S, Info, R);
   Check ("build-id from the note matches readelf", Info.Build_Id = Argument (3), Info.Build_Id);
   Check ("attach is labelled cooperative", Image (Info.Attach_Mode) = "cooperative attach (preview)");

   --  Line tables and functions, against nm and addr2line.
   declare
      Data : CuBit.Debug_Target.Buffer_Access;
      Handle : constant CuBit.Debug_Target.Process_Handle := CuBit.Debug_Target.Hosted (Argument (1), Argument (2));
      Conn : CuBit.Debug_Target.Connection_Access;
      Status : CuBit.Debug_Target.Status;
      Path : String (1 .. 512);
      Path_Length : Natural;
      Img : DW.Image_Access;
      Loaded : DW.Load_Result;
      I : Positive := 4;
   begin
      CuBit.Debug_Target.Connect (Handle, 0, Conn, Status);
      CuBit.Debug_Target.Load_Debug_File (Conn.all, "", 0, Data, Path, Path_Length, Status);
      DW.Load (Data, Img, Loaded);
      Check ("DWARF loads", Loaded = DW.Loaded, DW.Load_Result'Image (Loaded));
      Check ("units indexed", Img.Unit_Count > 10, Img.Unit_Count'Image);
      Check ("functions indexed", Img.Function_Count > 100, Img.Function_Count'Image);
      Check ("line rows indexed", Img.Row_Count > 1000, Img.Row_Count'Image);
      while I + 2 <= Argument_Count loop
         declare
            Symbol : constant String := Argument (I);
            Address : constant Unsigned_64 := Unsigned_64'Value ("16#" & Argument (I + 1) & "#");
            Line : constant Natural := Natural'Value (Argument (I + 2));
            F : constant Integer := DW.Function_At (Img.all, Address);
            D : DW.Die;
            Good, Found : Boolean;
            Row : DW.Row;
         begin
            Good := F >= 0;
            if Good then
               DW.Read_Die (Img.all, Img.Functions (F).Die_Offset, D, Good);
            end if;
            Check ("function at " & Symbol, Good and then DW.Name_Of (Img.all, D) = Symbol,
                   (if Good then DW.Name_Of (Img.all, D) else "none"));
            DW.Line_At (Img.all, Address, Row, Found);
            Check ("line of " & Symbol & " is" & Line'Image, Found and then Row.Line = Line,
                   (if Found then Row.Line'Image & " in " & DW.File_Name (Img.all, Row.File) else "none"));
         end;
         I := I + 3;
      end loop;
   end;

   --  Globals and values (the file's initial data).
   Globals (S, "Demo_World", Globals_List, R);
   Check ("globals of Demo_World", R = Ok and then Globals_List.Count >= 2, Globals_List.Count'Image);
   for G of Globals_List.Items (1 .. Globals_List.Count) loop
      if Image (G.Name) = "Demo_World.The_World" then
         World := G;
         Have_World := True;
      elsif Image (G.Name) = "Demo_World.Names" then
         Names := G;
         Have_Names := True;
      end if;
   end loop;
   Check ("Demo_World.The_World found", Have_World);
   Check ("Demo_World.Names found", Have_Names);
   if Have_World then
      Value (S, World, Node, R);
      Check ("The_World is a record of 8 components", R = Ok and then Node.Kind = Record_Value
             and then Node.Child_Count = 8, Value_Kind'Image (Node.Kind) & Node.Child_Count'Image);
      Check ("The_World's type name", Image (Node.Type_Name) = "Demo_World.World", Image (Node.Type_Name));
      Check ("The_World is 128 bytes", Node.Size = 128, Node.Size'Image);
      Children (S, World, 0, 8, Kids, R);
      Check ("The_World's components", Kids.Count = 8
             and then Image (Kids.Items (1).Name) = "Generation"
             and then Image (Kids.Items (2).Name) = "Jobs"
             and then Kids.Items (2).Kind = Pointer
             and then Image (Kids.Items (8).Name) = "Window"
             and then Kids.Items (8).Kind = Array_Value,
             (if Kids.Count > 0 then Image (Kids.Items (1).Name) else ""));
      Check ("Jobs starts null", Kids.Count >= 2 and then Image (Kids.Items (2).Display) = "null",
             Image (Kids.Items (2).Display));
      Check ("Window has its bounds", Kids.Count = 8
             and then Contains (Image (Kids.Items (8).Type_Name), "(-4 .. 11)")
             and then Kids.Items (8).Child_Count = 16,
             Image (Kids.Items (8).Type_Name));
      Check ("Current is a variant record", Kids.Count >= 7 and then Kids.Items (7).Kind = Variant_Record,
             Value_Kind'Image (Kids.Items (7).Kind));
      Check ("Current's variant is Circle (zero)", Kids.Count >= 7 and then Image (Kids.Items (7).Variant) = "Circle",
             Image (Kids.Items (7).Variant) & " " & Image (Kids.Items (7).Display));
      --  A local held in a register (an -O2 local) is marked changed by
      --  its name and type when its value differs from the last read.
      declare
         Local : Value_Ref := (Name => To_Name ("Step"), Location => Register_Value, Bits => 5,
                               Of_Type => Kids.Items (1).Self.Of_Type, others => <>);
         First_Read, Same_Read, Moved_Read : Value_Node;
      begin
         Value (S, Local, First_Read, R);
         Value (S, Local, Same_Read, R);
         Local.Bits := 6;
         Value (S, Local, Moved_Read, R);
         Check ("a register local is marked changed only when it moved",
                not Same_Read.Changed and then Moved_Read.Changed
                and then Image (Moved_Read.Display) = "6",
                Image (First_Read.Display) & " " & Image (Moved_Read.Display));
      end;
      Check ("Generation is a ranged Natural", Kids.Items (1).Has_Range
             and then Kids.Items (1).Range_Low = 0 and then Kids.Items (1).Range_High = Integer_64 (Integer'Last),
             Kids.Items (1).Range_Low'Image & Kids.Items (1).Range_High'Image);
   end if;
   if Have_Names then
      Value (S, Names, Node, R);
      Check ("Names is an array of 8", Node.Kind = Array_Value and then Node.Child_Count = 8,
             Value_Kind'Image (Node.Kind) & Node.Child_Count'Image);
      Children (S, Names, 0, 8, Kids, R);
      Check ("Names (0) is ""index-files """, Kids.Count = 8
             and then Image (Kids.Items (1).Display) = """index-files """
             and then Image (Kids.Items (1).Name) = "(0)",
             Image (Kids.Items (1).Display));
      Check ("Names (7) is ""backup-notes""", Kids.Count = 8
             and then Image (Kids.Items (8).Display) = """backup-notes""",
             Image (Kids.Items (8).Display));
   end if;

   --  The program's own globals only: Demo_World's, not the runtime's or
   --  the binder's (Ada_Main) or generated units'.
   declare
      Own : Ref_List;
      Foreign : Natural := 0;
   begin
      Globals (S, "", Own, R);
      for G of Own.Items (1 .. Own.Count) loop
         if Image (G.Name)'Length < 11 or else Image (G.Name) (1 .. 11) /= "Demo_World." then
            Foreign := Foreign + 1;
            Put_Line ("debug-engine:   not own: " & Image (G.Name));
         end if;
      end loop;
      Check ("globals of the program's own units", R = Ok and then Own.Count >= 2 and then Foreign = 0,
             Own.Count'Image & Foreign'Image);
   end;

   --  Graph of the world (initially one node: its pointers are null).
   declare
      Roots : Ref_List;
      type Graph_Access is access Graph_Result;
      G : constant Graph_Access := new Graph_Result;
   begin
      Roots.Count := 1;
      Roots.Items (1) := World;
      Graph (S, Roots, 4, 32, G.all, R);
      Check ("graph of the initial world", R = Ok and then G.Node_Total = 1 and then G.Edge_Total = 0
             and then G.Nodes (1).Fields_Shown = 8 and then G.Nodes (1).Fields (2).Is_Null,
             G.Node_Total'Image & G.Edge_Total'Image);
      Check ("graph node id is address and type", G.Node_Total = 1
             and then G.Nodes (1).Id.Address = World.Address);
   end;

   --  Source by the line table's file name.
   declare
      Page : Source_Page;
   begin
      Source (S, "demo_world.ads", 1, Page, R);
      Check ("source demo_world.ads", (R = Ok or else R = More) and then Page.Count > 10
             and then Contains (Image (Page.Lines (2)), "CuBit"),
             Outcome'Image (R) & Page.Count'Image);
   end;

   Detach (S);
   Put_Line ("debug-engine: hosted" & Checks'Image & " checks," & Failures'Image & " failed");
   if Failures > 0 then
      Set_Exit_Status (Failure);
   end if;
end Hosted_Check;
