------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The CCL front end and compiler size their storage by the input (no
--  fixed program limit): a typed manifest with its schema, as the manifest
--  compiler evaluates it, is analysed and compiled in rooms grown past the
--  defaults the engine used to stop at (CCL.Language.DEFAULT_SOURCE_ROOM,
--  DEFAULT_NODE_ROOM; CCL.VM.DEFAULT_INSTRUCTIONS, DEFAULT_CONSTANTS), then
--  verified; and a plain CCL program twenty times past them is analysed,
--  compiled, verified and run to its value. Usage: capacity_main SCHEMA
--  MANIFEST; prints the sizes and "PASS", or each failed check and exits 1.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Evaluation;
with Interfaces;
with CCL.Language;
with CCL.VM;

procedure Capacity_Main is
   use Ada.Text_IO;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.VM.Program_Length;
   use type CCL.VM.Constant_Count;
   use type CCL.VM.Validation_Error;

   function File_Text (Path : String) return String is
      Size : constant Natural := Natural (Ada.Directories.Size (Path));
      subtype Text_Type is String (1 .. Size);
      package Text_IO is new Ada.Direct_IO (Text_Type);
      File : Text_IO.File_Type;
      Text : Text_Type;
   begin
      Text_IO.Open (File, Text_IO.In_File, Path);
      Text_IO.Read (File, Text);
      Text_IO.Close (File);
      return Text;
   end File_Text;

   --  The program's code bytes: without comments and line indentation,
   --  as the manifest compiler hands it to CCL (CCL.Manifests.Typed).
   function Code_Bytes (Program : String) return Natural is
      Count : Natural := 0;
      Line_Start, In_Comment, In_String : Boolean := True;
   begin
      In_Comment := False;
      In_String := False;
      for C of Program loop
         if C = ASCII.LF then
            Line_Start := True;
            In_Comment := False;
            Count := Count + 1;
         elsif In_Comment then
            null;
         elsif C = '"' then
            In_String := not In_String;
            Line_Start := False;
            Count := Count + 1;
         elsif not In_String and then C = '#' then
            In_Comment := True;
         elsif Line_Start and then C = ' ' then
            null;
         else
            Line_Start := False;
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Code_Bytes;

   Failed : Boolean := False;
   procedure Check (Ok : Boolean; What : String) is
   begin
      if not Ok then
         Put_Line ("ccl-manifest-capacity: FAIL " & What);
         Failed := True;
      end if;
   end Check;

   procedure Run (Program : String) is
      Analysis : constant CCL.Language.Sized_Analysis :=
        CCL.Language.Analysis_Of (Program, CCL.Catalog.Empty_Catalog);
      Nodes : constant Natural := CCL.Language.Analysis_Node_Count (Analysis);
   begin
      Check (CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded,
             "analysis: " & CCL.Language.Analysis_Diagnostic (Analysis)'Image);
      if Failed then return; end if;
      declare
         Compiled : constant CCL.Compiler.Sized_Compilation := CCL.Compiler.Compilation_Of (Analysis);
         Checked : CCL.VM.Sized_Validated_Program
           (Compiled.Last_Instruction, Compiled.Last_Constant, Compiled.Text_Room);
         Error : CCL.VM.Validation_Error;
      begin
         Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded,
                "compilation: " & Compiled.Status'Image);
         if Failed then return; end if;
         Put_Line ("code bytes" & Code_Bytes (Program)'Image & ", syntax nodes" & Nodes'Image &
                   ", instructions" & Compiled.Program.Length'Image &
                   ", constants" & Compiled.Program.Constants_Length'Image);
         --  The manifest's value is read from one object image
         --  (CCL.Objects), which bounds how far past the source and node
         --  rooms a manifest goes; the code and pool go well past theirs.
         Check (2 * Code_Bytes (Program) > 3 * CCL.Language.DEFAULT_SOURCE_ROOM,
                "the program is not over 1.5 times the old 8 KiB source limit");
         Check (Nodes > CCL.Language.DEFAULT_NODE_ROOM,
                "the tree is not over the old 512-node limit");
         Check (Compiled.Program.Length > 2 * CCL.VM.DEFAULT_INSTRUCTIONS,
                "the program is not over twice the old 256-instruction limit");
         Check (Compiled.Program.Constants_Length > 2 * CCL.VM.DEFAULT_CONSTANTS,
                "the pool is not over twice the old 32-constant limit");
         --  The grown program verifies like any other.
         CCL.VM.Verify (Compiled.Program, Checked, Error);
         Check (Error = CCL.VM.Valid, "verification: " & Error'Image);
      end;
   end Run;

   --  A balanced tree of additions, (+ (+ 1 1) (+ 1 1)) and so on, Levels
   --  deep (within the parser's nesting bound): 2 ** Levels leaves, about
   --  one instruction per node, in a program far past the old rooms, run
   --  to its value.
   procedure Run_Large_Program is
      Levels : constant := 12;
      Leaves : constant := 2 ** Levels;
      Fuel : constant := 1_000_000;
      function Tree (Level : Natural) return String is
        (if Level = 0 then "1" else "(+ " & Tree (Level - 1) & " " & Tree (Level - 1) & ")");
      Source : constant String := Tree (Levels);
      Analysis : constant CCL.Language.Sized_Analysis :=
        CCL.Language.Analysis_Of (Source, CCL.Catalog.Empty_Catalog);
      Compiled : constant CCL.Compiler.Sized_Compilation := CCL.Compiler.Compilation_Of (Analysis);
      Outcome : CCL.Language.Interpretation_Result;
      use type CCL.Language.Interpretation_Status;
      use type Interfaces.Integer_64;
   begin
      Put_Line ("large program: source bytes" & Source'Length'Image &
                ", syntax nodes" & CCL.Language.Analysis_Node_Count (Analysis)'Image &
                ", instructions" & Compiled.Program.Length'Image);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded,
             "large program compilation: " & Compiled.Status'Image & " " &
             CCL.Language.Analysis_Diagnostic (Analysis)'Image);
      Check (Source'Length > 2 * CCL.Language.DEFAULT_SOURCE_ROOM,
             "the large program is not over twice the old 8 KiB source limit");
      Check (CCL.Language.Analysis_Node_Count (Analysis) > 10 * CCL.Language.DEFAULT_NODE_ROOM,
             "the large program is not over ten times the old 512-node limit");
      Check (Compiled.Program.Length > 20 * CCL.VM.DEFAULT_INSTRUCTIONS,
             "the large program is not over twenty times the old 256-instruction limit");
      CCL.Evaluation.Evaluate (Source, Fuel, Outcome);
      Check (Outcome.Status = CCL.Language.Succeeded, "large program run: " & Outcome.Status'Image);
      Check (CCL.Language.Has_Scalar (Outcome) and then Outcome.Result_Value.Integer = Leaves,
             "large program value: " & Outcome.Result_Value.Integer'Image);
   end Run_Large_Program;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line ("usage: capacity_main SCHEMA MANIFEST");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   --  Storage sized by the input lives on the evaluating task's stack, as
   --  in ccl-manifest (Interpreter_Stack_For): give it room for both.
   declare
      Evaluation_Stack_Bytes : constant := 256 * 1024 * 1024;
      task Evaluation with Storage_Size => Evaluation_Stack_Bytes;
      task body Evaluation is
      begin
         Run (File_Text (Ada.Command_Line.Argument (1)) & ASCII.LF & File_Text (Ada.Command_Line.Argument (2)));
         Run_Large_Program;
      end Evaluation;
   begin
      null;
   end;
   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("ccl-manifest-capacity: PASS");
   end if;
end Capacity_Main;
