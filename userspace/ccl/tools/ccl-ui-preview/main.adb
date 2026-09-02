with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with CCL.Language;
with CCL.Compiler;
with CCL.Debug_Maps;
with CCL.VM;
with CuBit.UI;
with CuBit.UI.Editor.Cursors;
with CuBit.UI.Editor.Documents;
with CuBit.UI.Editor_History;
with CuBit.UI.Editor.Transactions;
with CuBit.UI.Editor.Viewports;
with CuBit.UI.Widgets;

--  HOSTED/LINUX Workbench preview.  Rendering below uses the SHARED CuBit UI
--  canvas.  Native_Window is the only hosted presentation/input boundary.
procedure Main is
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned;
   use type System.Address;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Debug_Maps.Validation_Error;
   use type CCL.VM.Validation_Error;
   use type CCL.VM.Execution_Status;
   use type CCL.VM.Instruction_Index;
   use type CCL.VM.Program_Length;
   use type CCL.VM.Value_Kind;
   use type CuBit.UI.Editor.Documents.Edit_Result;
   use type CuBit.UI.Editor.Cursors.Toggle_Result;
   use type CuBit.UI.Editor.Cursors.Add_Result;
   use type CuBit.UI.Scrollbar_Part;

   --  Compact native canvas: never downscale the toolkit's 11 px UI font.
   --  The hosted adapter scales this canvas upward when space permits.
   WIDTH  : constant Natural := 900;
   HEIGHT : constant Natural := 400;
   SOURCE_CAPACITY : constant := 4_096;
   BYTECODE_ROW_HEIGHT : constant Positive := CuBit.UI.UI_Text_Height + 3;

   type Pixel_Buffer is array (Natural range 0 .. WIDTH * HEIGHT - 1)
     of aliased Unsigned_32;
   Pixels : aliased Pixel_Buffer := [others => 0];

   Canvas : constant CuBit.UI.Canvas :=
     (addr => Pixels'Address, width => WIDTH, height => HEIGHT,
      pitch => WIDTH * 4, clipEnabled => False, clip => (others => 0));
   Colors : constant CuBit.UI.Theme := CuBit.UI.CuBit_Classic;

   Result_Text : String (1 .. 96) := [others => ' '];
   Result_Last : Natural := 5;
   Last_Outcome : CCL.Language.Interpretation_Result;
   Has_Run : Boolean := False;
   Compiled_Artifact : CCL.Compiler.Compilation_Result;
   Verified_Artifact : CCL.VM.Validated_Program;
   Has_Compiled : Boolean := False;
   Has_Verified : Boolean := False;
   Last_VM_Outcome : CCL.VM.Execution_Result;
   VM_Has_Run : Boolean := False;
   VM_State : CCL.VM.Machine_State;
   VM_Has_State : Boolean := False;
   VM_Continuous : Boolean := False;
   VM_Snapshot : CCL.VM.Machine_Snapshot;
   Debug_Map_Valid : Boolean := False;
   Active_Debug_Entry : CCL.Debug_Maps.Debug_Entry;
   Has_Active_Debug_Entry : Boolean := False;
   type Breakpoint_Array is
     array (CCL.VM.Instruction_Index) of Boolean;
   Breakpoints : Breakpoint_Array := [others => False];
   Breakpoint_Paused : Boolean := False;
   Ignore_Current_Breakpoint : Boolean := False;
   VM_Step_Over_Active : Boolean := False;
   Step_Over_End : CCL.VM.Program_Length := 0;
   Diagnostic_Line : Natural := 0;
   Diagnostic_Column : Natural := 0;
   Source : CuBit.UI.Editor.Documents.Document (SOURCE_CAPACITY);
   Source_Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
   package Source_Histories is new CuBit.UI.Editor_History
     (Capacity => SOURCE_CAPACITY, Depth => 32);
   Source_History : Source_Histories.History;
   Source_View : CuBit.UI.Editor.Viewports.Viewport;
   Source_Bounds : CuBit.UI.Rect := (others => 0);
   Source_Scrollbar : CuBit.UI.Rect := (others => 0);
   Bytecode_Content : CuBit.UI.Rect := (others => 0);
   Open_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 5, y => 47, w => 27, h => 27);
   Save_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 34, y => 47, w => 27, h => 27);
   Compile_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 102, y => 47, w => 27, h => 27);
   VM_Run_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 131, y => 47, w => 27, h => 27);
   Pause_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 160, y => 47, w => 27, h => 27);
   Stop_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 189, y => 47, w => 27, h => 27);
   Step_Into_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 228, y => 47, w => 27, h => 27);
   Step_Over_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 257, y => 47, w => 27, h => 27);
   Run_Button_Bounds : CuBit.UI.Rect := (others => 0);
   Run_Button_Pressed : Boolean := False;
   Compile_Button_Pressed : Boolean := False;
   VM_Run_Button_Pressed : Boolean := False;
   Pause_Button_Pressed : Boolean := False;
   Stop_Button_Pressed : Boolean := False;
   Step_Into_Button_Pressed : Boolean := False;
   Step_Over_Button_Pressed : Boolean := False;
   Pointer_X, Pointer_Y : Natural := 0;
   Pointer_Known : Boolean := False;
   Source_Scrollbar_Pressed : CuBit.UI.Scrollbar_Part :=
     CuBit.UI.Scrollbar_None;

   function Toolbar_Hint return String is
   begin
      if not Pointer_Known then
         return "CCL Workbench";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Open_Button_Bounds)
      then
         return "Open source - unavailable until filesystem handles are wired";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Save_Button_Bounds)
      then
         return "Save source - unavailable until filesystem handles are wired";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Compile_Button_Bounds)
      then
         return "Compile source to CCLB and verify the artifact";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Run_Button_Bounds)
      then
         return "Interpret source directly (F5 or Ctrl+Enter)";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, VM_Run_Button_Bounds)
      then
         return
           (if Has_Verified then "Run the verified CCLB artifact"
            else "Run verified CCLB - compile an artifact first");
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Pause_Button_Bounds)
      then
         return "Pause - available during resumable bytecode execution";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Stop_Button_Bounds)
      then
         return "Stop - available during resumable bytecode execution";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Step_Into_Button_Bounds)
      then
         return "Step Into - execute one bytecode instruction";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Step_Over_Button_Bounds)
      then
         return "Step Over - run until the active source expression exits";
      elsif Has_Compiled and then CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Bytecode_Content)
      then
         return "Click a bytecode row to toggle a mapped breakpoint";
      else
         return "CCL Workbench";
      end if;
   end Toolbar_Hint;

   function Window_Open (Width, Height : Interfaces.C.int) return System.Address
   with Import, Convention => C, External_Name => "ccl_window_open";
   function Window_Poll
     (Handle : System.Address; Kind : access Interfaces.C.int;
      Code, Modifiers : access Interfaces.C.unsigned;
      X, Y : access Interfaces.C.int) return Interfaces.C.int
   with Import, Convention => C, External_Name => "ccl_window_poll";
   function Window_Present
     (Handle, Pixels : System.Address;
      Pitch : Interfaces.C.int) return Interfaces.C.int
   with Import, Convention => C, External_Name => "ccl_window_present";
   procedure Window_Wait
   with Import, Convention => C, External_Name => "ccl_window_wait";
   function Window_Ticks return Interfaces.Unsigned_64
   with Import, Convention => C, External_Name => "ccl_window_ticks";
   procedure Window_Close (Handle : System.Address)
   with Import, Convention => C, External_Name => "ccl_window_close";

   procedure Set_Result (Text : String) is
      Length : constant Natural := Natural'Min (Text'Length, Result_Text'Length);
   begin
      Result_Text := [others => ' '];
      if Length > 0 then
         Result_Text (1 .. Length) := Text (Text'First .. Text'First + Length - 1);
      end if;
      Result_Last := Length;
   end Set_Result;

   procedure Invalidate_Run_Result is
   begin
      Has_Run := False;
      VM_Has_Run := False;
      VM_Has_State := False;
      VM_Continuous := False;
      Has_Compiled := False;
      Has_Verified := False;
      Debug_Map_Valid := False;
      Has_Active_Debug_Entry := False;
      Breakpoints := [others => False];
      Breakpoint_Paused := False;
      Ignore_Current_Breakpoint := False;
      VM_Step_Over_Active := False;
      Diagnostic_Line := 0;
      Diagnostic_Column := 0;
      Set_Result ("source changed");
   end Invalidate_Run_Result;

   procedure Reveal_Source_Cursor;

   procedure Run_Source is
      Outcome : CCL.Language.Interpretation_Result;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Line, Column : Positive;
      Text : constant String :=
        CuBit.UI.Editor.Documents.Content (Source);
   begin
      VM_Continuous := False;
      VM_Has_Run := False;
      CCL.Language.Interpret (Text, 4_096, Outcome);
      Last_Outcome := Outcome;
      Has_Run := True;
      Diagnostic_Line := 0;
      Diagnostic_Column := 0;
      if Outcome.Status = CCL.Language.Succeeded then
         if not Outcome.Has_Value then
            Set_Result ("ok");
         elsif Outcome.Result_Value.Kind = CCL.VM.Integer_Value then
            Set_Result (Integer_64'Image (Outcome.Result_Value.Integer));
         else
            Set_Result
              ((if Outcome.Result_Value.Boolean then "true" else "false"));
         end if;
      elsif Outcome.Diagnostic_Position > 0 then
         Position := CuBit.UI.Editor.Documents.Document_Position'Min
           (Outcome.Diagnostic_Position,
            CuBit.UI.Editor.Documents.Length (Source) + 1);
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, Position, Line, Column);
         Diagnostic_Line := Line;
         Diagnostic_Column := Column;
         Source_Histories.Break_Sequence (Source_History);
         CuBit.UI.Editor.Cursors.Initialize (Source_Cursors, Position);
         Reveal_Source_Cursor;
         Set_Result (CCL.Language.Diagnostic_Code'Image (Outcome.Diagnostic));
      else
         Set_Result (CCL.Language.Diagnostic_Code'Image (Outcome.Diagnostic));
      end if;
   end Run_Source;

   procedure Update_Active_Debug;

   procedure Compile_Source is
      Analysis : CCL.Language.Analysis_Result;
      Error    : CCL.VM.Validation_Error;
      Debug_Error : CCL.Debug_Maps.Validation_Error;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Line, Column : Positive;
      Text : constant String :=
        CuBit.UI.Editor.Documents.Content (Source);
   begin
      Has_Run := False;
      VM_Has_Run := False;
      VM_Has_State := False;
      VM_Continuous := False;
      Has_Compiled := False;
      Has_Verified := False;
      Debug_Map_Valid := False;
      Has_Active_Debug_Entry := False;
      Breakpoints := [others => False];
      Breakpoint_Paused := False;
      Ignore_Current_Breakpoint := False;
      VM_Step_Over_Active := False;
      Diagnostic_Line := 0;
      Diagnostic_Column := 0;

      CCL.Language.Analyze (Text, Analysis);
      if CCL.Language.Analysis_Status_Of (Analysis) /=
        CCL.Language.Analysis_Succeeded
      then
         if CCL.Language.Analysis_Diagnostic_Position (Analysis) > 0 then
            Position := CuBit.UI.Editor.Documents.Document_Position'Min
              (CCL.Language.Analysis_Diagnostic_Position (Analysis),
               CuBit.UI.Editor.Documents.Length (Source) + 1);
            CuBit.UI.Editor.Documents.Position_To_Line_Column
              (Source, Position, Line, Column);
            Diagnostic_Line := Line;
            Diagnostic_Column := Column;
            Source_Histories.Break_Sequence (Source_History);
            CuBit.UI.Editor.Cursors.Initialize (Source_Cursors, Position);
            Reveal_Source_Cursor;
         end if;
         Set_Result
           ("analysis: " & CCL.Language.Diagnostic_Code'Image
              (CCL.Language.Analysis_Diagnostic (Analysis)));
         return;
      end if;

      CCL.Compiler.Compile (Analysis, Compiled_Artifact);
      Has_Compiled :=
        Compiled_Artifact.Status = CCL.Compiler.Compilation_Succeeded;
      if not Has_Compiled then
         if Compiled_Artifact.Source_Position > 0 then
            Position := CuBit.UI.Editor.Documents.Document_Position'Min
              (Compiled_Artifact.Source_Position,
               CuBit.UI.Editor.Documents.Length (Source) + 1);
            CuBit.UI.Editor.Documents.Position_To_Line_Column
              (Source, Position, Line, Column);
            Diagnostic_Line := Line;
            Diagnostic_Column := Column;
            Source_Histories.Break_Sequence (Source_History);
            CuBit.UI.Editor.Cursors.Initialize (Source_Cursors, Position);
            Reveal_Source_Cursor;
         end if;
         Set_Result
           ("compile: " & CCL.Compiler.Compilation_Status'Image
              (Compiled_Artifact.Status));
         return;
      end if;

      CCL.VM.Verify
        (Compiled_Artifact.Program, Verified_Artifact, Error);
      Has_Verified := Error = CCL.VM.Valid;
      CCL.Debug_Maps.Validate
        (Compiled_Artifact.Debug, Compiled_Artifact.Program.Length,
         Debug_Error);
      Debug_Map_Valid := Debug_Error = CCL.Debug_Maps.Debug_Map_Valid;
      if Has_Verified then
         CCL.VM.Initialize (Verified_Artifact, 4_096, VM_State);
         VM_Has_State := True;
         VM_Snapshot := CCL.VM.Snapshot (VM_State);
         Update_Active_Debug;
         if Debug_Map_Valid then
            Set_Result
              ("compiled and verified:" &
               CCL.VM.Program_Length'Image
                 (Compiled_Artifact.Program.Length) & " instructions");
         else
            Set_Result ("VM valid; debug map rejected");
         end if;
      else
         Set_Result
           ("verification: " & CCL.VM.Validation_Error'Image (Error));
      end if;
   end Compile_Source;

   procedure Update_Active_Debug is
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Line, Column : Positive;
   begin
      Has_Active_Debug_Entry := False;
      if not Debug_Map_Valid or else not VM_Has_State then
         return;
      end if;
      CCL.Debug_Maps.Find_Innermost
        (Compiled_Artifact.Debug, VM_Snapshot.Instruction,
         Active_Debug_Entry, Has_Active_Debug_Entry);
      if Has_Active_Debug_Entry and then
        Active_Debug_Entry.Source_First > 0
      then
         Position := CuBit.UI.Editor.Documents.Document_Position'Min
           (Active_Debug_Entry.Source_First,
            CuBit.UI.Editor.Documents.Length (Source) + 1);
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, Position, Line, Column);
         CuBit.UI.Editor.Viewports.Ensure_Visible
           (Source_View, Line,
            CuBit.UI.Editor.Documents.Line_Count (Source));
      end if;
   end Update_Active_Debug;

   procedure Update_VM_Result is
   begin
      VM_Has_Run := True;
      Has_Run := False;
      Diagnostic_Line := 0;
      Diagnostic_Column := 0;
      if Last_VM_Outcome.Status = CCL.VM.Completed and then
        Last_VM_Outcome.Has_Value
      then
         if Last_VM_Outcome.Result_Value.Kind = CCL.VM.Integer_Value then
            Set_Result
              (Integer_64'Image (Last_VM_Outcome.Result_Value.Integer));
         else
            Set_Result
              ((if Last_VM_Outcome.Result_Value.Boolean then
                   "true"
                else "false"));
         end if;
      elsif Last_VM_Outcome.Status = CCL.VM.Paused and then VM_Continuous then
         Set_Result ("VM running");
      else
         Set_Result
           ("VM: " & CCL.VM.Execution_Status'Image
              (Last_VM_Outcome.Status));
      end if;
   end Update_VM_Result;

   procedure Advance_Bytecode (Instructions : Natural) is
   begin
      if not Has_Verified or else not VM_Has_State then
         return;
      end if;
      CCL.VM.Continue_Execution_For
        (Verified_Artifact, VM_State, Instructions, Last_VM_Outcome);
      VM_Snapshot := CCL.VM.Snapshot (VM_State);
      Update_Active_Debug;
      if VM_Snapshot.Terminal or else VM_Snapshot.Waiting then
         VM_Continuous := False;
      end if;
      Update_VM_Result;
   end Advance_Bytecode;

   procedure Start_Bytecode is
   begin
      if not Has_Verified then
         Set_Result ("compile and verify before VM run");
         return;
      end if;

      if not VM_Has_State or else VM_Snapshot.Terminal then
         CCL.VM.Initialize (Verified_Artifact, 4_096, VM_State);
         VM_Has_State := True;
         VM_Snapshot := CCL.VM.Snapshot (VM_State);
         Update_Active_Debug;
      end if;
      VM_Continuous := True;
      VM_Step_Over_Active := False;
      Breakpoint_Paused := False;
      Ignore_Current_Breakpoint := True;
      Set_Result ("VM running");
   end Start_Bytecode;

   procedure Pause_Bytecode is
   begin
      if VM_Has_State and then not VM_Snapshot.Terminal then
         VM_Continuous := False;
         VM_Step_Over_Active := False;
         Set_Result ("VM paused");
      end if;
   end Pause_Bytecode;

   procedure Stop_Bytecode is
   begin
      if VM_Has_State and then not VM_Snapshot.Terminal then
         CCL.VM.Stop (VM_State);
         VM_Continuous := False;
         VM_Step_Over_Active := False;
         Advance_Bytecode (0);
      end if;
   end Stop_Bytecode;

   procedure Step_Bytecode is
   begin
      if not Has_Verified then
         Set_Result ("compile and verify before stepping");
         return;
      end if;
      if not VM_Has_State then
         CCL.VM.Initialize (Verified_Artifact, 4_096, VM_State);
         VM_Has_State := True;
         VM_Snapshot := CCL.VM.Snapshot (VM_State);
         Update_Active_Debug;
      end if;
      if not VM_Snapshot.Terminal then
         VM_Continuous := False;
         VM_Step_Over_Active := False;
         Breakpoint_Paused := False;
         Advance_Bytecode (1);
      end if;
   end Step_Bytecode;

   procedure Step_Over_Bytecode is
      Position : CCL.VM.Program_Length;
   begin
      if not Has_Verified then
         Set_Result ("compile and verify before stepping");
         return;
      elsif not VM_Has_State or else VM_Snapshot.Terminal then
         Step_Bytecode;
         return;
      end if;

      Position := CCL.VM.Program_Length (VM_Snapshot.Instruction);
      if Has_Active_Debug_Entry and then
        Active_Debug_Entry.End_PC > Position + 1
      then
         Step_Over_End := Active_Debug_Entry.End_PC;
         VM_Step_Over_Active := True;
         VM_Continuous := True;
         Breakpoint_Paused := False;
         Ignore_Current_Breakpoint := True;
         Set_Result ("stepping over source expression");
      else
         Step_Bytecode;
      end if;
   end Step_Over_Bytecode;

   function Source_Cursor return CuBit.UI.Editor.Cursors.Cursor_State is
     (CuBit.UI.Editor.Cursors.Element
        (Source_Cursors,
         CuBit.UI.Editor.Cursors.Primary_Index (Source_Cursors)));

   procedure Store_Source_Cursor
     (Value : CuBit.UI.Editor.Cursors.Cursor_State)
   is
   begin
      CuBit.UI.Editor.Cursors.Set_Element
        (Source_Cursors,
         CuBit.UI.Editor.Cursors.Primary_Index (Source_Cursors), Value);
   end Store_Source_Cursor;

   procedure Collapse_Source_Cursors is
      Position : constant CuBit.UI.Editor.Documents.Document_Position :=
        Source_Cursor.Position;
   begin
      CuBit.UI.Editor.Cursors.Initialize (Source_Cursors, Position);
   end Collapse_Source_Cursors;

   procedure Reveal_Source_Cursor is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Line, Column : Positive;
      First_Cursor_Line : Positive :=
        CuBit.UI.Editor.Documents.Line_Count (Source);
      Last_Cursor_Line : Positive := 1;
      First_Visible : constant Positive :=
        CuBit.UI.Editor.Viewports.First_Line (Source_View);
      Visible_Lines : constant Positive :=
        CuBit.UI.Editor.Viewports.Line_Capacity (Source_View);
      Last_Visible : constant Positive := Positive'Min
        (CuBit.UI.Editor.Documents.Line_Count (Source),
         First_Visible + Visible_Lines - 1);
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, State.Position, Line, Column);
         First_Cursor_Line := Positive'Min (First_Cursor_Line, Line);
         Last_Cursor_Line := Positive'Max (Last_Cursor_Line, Line);
      end loop;
      if First_Cursor_Line < First_Visible then
         CuBit.UI.Editor.Viewports.Scroll_Lines
           (Source_View,
            Integer (First_Cursor_Line) - Integer (First_Visible),
            CuBit.UI.Editor.Documents.Line_Count (Source));
      elsif Last_Cursor_Line > Last_Visible then
         CuBit.UI.Editor.Viewports.Ensure_Visible
           (Source_View, Last_Cursor_Line,
            CuBit.UI.Editor.Documents.Line_Count (Source));
      end if;
   end Reveal_Source_Cursor;

   procedure Place_Source_Cursor
     (Position : CuBit.UI.Editor.Documents.Document_Position;
      Extend_Selection : Boolean; Preserve_Column : Boolean := False)
   is
      State : CuBit.UI.Editor.Cursors.Cursor_State := Source_Cursor;
      Line, Column : Positive;
   begin
      Source_Histories.Break_Sequence (Source_History);
      State.Position := Position;
      if not Extend_Selection then State.Anchor := Position; end if;
      if not Preserve_Column then
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, Position, Line, Column);
         State.Preferred_Column := Column;
      end if;
      Store_Source_Cursor (State);
      Reveal_Source_Cursor;
   end Place_Source_Cursor;

   procedure Insert_Source
     (Text : String; Changed : out Boolean;
      Operation : Source_Histories.Operation_Kind :=
        Source_Histories.Insert_Characters)
   is
      Result : CuBit.UI.Editor.Documents.Edit_Result;
      Plan : CuBit.UI.Editor.Transactions.Edit_Plan;
      Document_Length : constant Natural :=
        CuBit.UI.Editor.Documents.Length (Source);
   begin
      CuBit.UI.Editor.Transactions.Build
        (Source_Cursors, Document_Length, Plan);
      if Text'Length = 0 or else not CuBit.UI.Editor.Transactions.Final_Length_Fits
        (Plan, Document_Length, Text'Length, SOURCE_CAPACITY)
      then
         Changed := False;
         return;
      end if;
      Source_Histories.Save_Before_Edit
        (Source_History, Source, Source_Cursors, Operation);
      CuBit.UI.Editor.Transactions.Replace_All
        (Source, Source_Cursors, Text, Result);
      Changed := Result = CuBit.UI.Editor.Documents.Applied and then
        Text'Length > 0;
      if Changed then
         Invalidate_Run_Result;
         Reveal_Source_Cursor;
      end if;
   end Insert_Source;

   procedure Backspace_Source (Changed : out Boolean) is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Result : CuBit.UI.Editor.Documents.Edit_Result;
      Has_Deletion : Boolean := False;
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         if State.Position /= State.Anchor or else State.Position > 1 then
            Has_Deletion := True;
         end if;
      end loop;
      if not Has_Deletion then
         Changed := False;
         return;
      end if;
      Source_Histories.Save_Before_Edit
        (Source_History, Source, Source_Cursors,
         Source_Histories.Delete_Backward);
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         if State.Position = State.Anchor and then State.Position > 1 then
            State.Anchor := State.Position - 1;
            CuBit.UI.Editor.Cursors.Set_Element
              (Source_Cursors, Index, State);
         end if;
      end loop;
      CuBit.UI.Editor.Transactions.Replace_All
        (Source, Source_Cursors, "", Result);
      Changed := Has_Deletion and then
        Result = CuBit.UI.Editor.Documents.Applied;
      if Changed then
         Invalidate_Run_Result;
         Reveal_Source_Cursor;
      end if;
   end Backspace_Source;

   procedure Delete_Source_Forward (Changed : out Boolean) is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Result : CuBit.UI.Editor.Documents.Edit_Result;
      Has_Deletion : Boolean := False;
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         if State.Position /= State.Anchor or else
           State.Position <= CuBit.UI.Editor.Documents.Length (Source)
         then
            Has_Deletion := True;
         end if;
      end loop;
      if not Has_Deletion then
         Changed := False;
         return;
      end if;
      Source_Histories.Save_Before_Edit
        (Source_History, Source, Source_Cursors,
         Source_Histories.Delete_Forward);
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         if State.Position = State.Anchor and then
           State.Position <= CuBit.UI.Editor.Documents.Length (Source)
         then
            State.Anchor := State.Position + 1;
            CuBit.UI.Editor.Cursors.Set_Element
              (Source_Cursors, Index, State);
         end if;
      end loop;
      CuBit.UI.Editor.Transactions.Replace_All
        (Source, Source_Cursors, "", Result);
      Changed := Has_Deletion and then
        Result = CuBit.UI.Editor.Documents.Applied;
      if Changed then
         Invalidate_Run_Result;
         Reveal_Source_Cursor;
      end if;
   end Delete_Source_Forward;

   procedure Move_Source_Horizontal
     (Right, By_Word, Extend_Selection : Boolean)
   is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Text : constant String := CuBit.UI.Editor.Documents.Content (Source);
      Line, Column : Positive;

      function Is_Word_Character (Value : Character) return Boolean is
        ((Value >= 'a' and then Value <= 'z') or else
         (Value >= 'A' and then Value <= 'Z') or else
         (Value >= '0' and then Value <= '9') or else Value = '_');
   begin
      Source_Histories.Break_Sequence (Source_History);
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         Position := State.Position;
         if not Extend_Selection and then State.Position /= State.Anchor then
            Position :=
              (if Right then
                 CuBit.UI.Editor.Documents.Document_Position'Max
                   (State.Position, State.Anchor)
               else
                 CuBit.UI.Editor.Documents.Document_Position'Min
                   (State.Position, State.Anchor));
         elsif By_Word and then Right then
            while Position <= Text'Length and then
              Is_Word_Character (Text (Position))
            loop
               Position := Position + 1;
            end loop;
            while Position <= Text'Length and then
              not Is_Word_Character (Text (Position))
            loop
               Position := Position + 1;
            end loop;
         elsif By_Word then
            while Position > 1 and then
              not Is_Word_Character (Text (Position - 1))
            loop
               Position := Position - 1;
            end loop;
            while Position > 1 and then
              Is_Word_Character (Text (Position - 1))
            loop
               Position := Position - 1;
            end loop;
         elsif Right and then Position <= Text'Length then
            Position := Position + 1;
         elsif not Right and then Position > 1 then
            Position := Position - 1;
         end if;
         State.Position := Position;
         if not Extend_Selection then State.Anchor := Position; end if;
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, Position, Line, Column);
         State.Preferred_Column := Column;
         CuBit.UI.Editor.Cursors.Set_Element (Source_Cursors, Index, State);
      end loop;
      CuBit.UI.Editor.Cursors.Coalesce (Source_Cursors);
      Reveal_Source_Cursor;
   end Move_Source_Horizontal;

   procedure Move_Source_Vertical
     (Direction : CuBit.UI.Editor.Documents.Vertical_Direction;
      Extend_Selection : Boolean)
   is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Position : CuBit.UI.Editor.Documents.Document_Position;
   begin
      Source_Histories.Break_Sequence (Source_History);
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         CuBit.UI.Editor.Documents.Move_Vertically
           (Source, State.Position, State.Preferred_Column,
            Direction, Position);
         State.Position := Position;
         if not Extend_Selection then State.Anchor := Position; end if;
         CuBit.UI.Editor.Cursors.Set_Element (Source_Cursors, Index, State);
      end loop;
      CuBit.UI.Editor.Cursors.Coalesce (Source_Cursors);
      Reveal_Source_Cursor;
   end Move_Source_Vertical;

   procedure Add_Source_Cursor_Vertically
     (Direction : CuBit.UI.Editor.Documents.Vertical_Direction)
   is
      State : constant CuBit.UI.Editor.Cursors.Cursor_State := Source_Cursor;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Result : CuBit.UI.Editor.Cursors.Add_Result;
   begin
      Source_Histories.Break_Sequence (Source_History);
      CuBit.UI.Editor.Documents.Move_Vertically
        (Source, State.Position, State.Preferred_Column,
         Direction, Position);
      if Position = State.Position then
         return;
      end if;

      CuBit.UI.Editor.Cursors.Add_At
        (Source_Cursors, Position, State.Preferred_Column, Result);
      if Result = CuBit.UI.Editor.Cursors.Cursor_Limit_Reached then
         Set_Result ("cursor limit reached");
      end if;
      Reveal_Source_Cursor;
   end Add_Source_Cursor_Vertically;

   procedure Move_Source_Line_End
     (To_End, Extend_Selection : Boolean)
   is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Line, Column : Positive;
      Position : CuBit.UI.Editor.Documents.Document_Position;
   begin
      Source_Histories.Break_Sequence (Source_History);
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, State.Position, Line, Column);
         Position := CuBit.UI.Editor.Documents.Line_Column_To_Position
           (Source, Line,
            (if To_End then
               CuBit.UI.Editor.Documents.Line_Length (Source, Line) + 1
             else 1));
         State.Position := Position;
         if not Extend_Selection then
            State.Anchor := Position;
         end if;
         State.Preferred_Column :=
           (if To_End then
              CuBit.UI.Editor.Documents.Line_Length (Source, Line) + 1
            else 1);
         CuBit.UI.Editor.Cursors.Set_Element
           (Source_Cursors, Index, State);
      end loop;
      CuBit.UI.Editor.Cursors.Coalesce (Source_Cursors);
      Reveal_Source_Cursor;
   end Move_Source_Line_End;

   procedure Select_All_Source is
      State : CuBit.UI.Editor.Cursors.Cursor_State := Source_Cursor;
   begin
      Source_Histories.Break_Sequence (Source_History);
      State.Anchor := 1;
      State.Position := CuBit.UI.Editor.Documents.Length (Source) + 1;
      Store_Source_Cursor (State);
      Reveal_Source_Cursor;
   end Select_All_Source;

   function Source_Position_At (Pixel_X, Pixel_Y : Natural)
     return CuBit.UI.Editor.Documents.Document_Position
   is
      Line_Height : constant Natural := CuBit.UI.UI_Text_Height + 2;
      Relative_Line : constant Natural :=
        (if Pixel_Y <= Source_Bounds.y + 5 then 0
         else (Pixel_Y - Source_Bounds.y - 5) / Line_Height);
      Line : Positive := CuBit.UI.Editor.Viewports.First_Line (Source_View);
      Column : Positive := 1;
      Offset : constant Natural :=
        (if Pixel_X <= Source_Bounds.x + 6 then 0
         else Pixel_X - Source_Bounds.x - 6);
      Draw_X : Natural := 0;
      Width : Natural;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Text : constant String := CuBit.UI.Editor.Documents.Content (Source);
   begin
      Line := Positive'Min
        (Line + Relative_Line, CuBit.UI.Editor.Documents.Line_Count (Source));
      for Candidate in 1 .. CuBit.UI.Editor.Documents.Line_Length (Source, Line)
      loop
         Position := CuBit.UI.Editor.Documents.Line_Column_To_Position
           (Source, Line, Candidate);
         Width := CuBit.UI.UI_Text_Width (Text (Position .. Position));
         exit when Offset < Draw_X + (Width + 1) / 2;
         Draw_X := Draw_X + Width;
         Column := Candidate + 1;
      end loop;
      return CuBit.UI.Editor.Documents.Line_Column_To_Position
        (Source, Line, Column);
   end Source_Position_At;

   procedure Source_Scrollbar_Metrics
     (Track, Thumb : out CuBit.UI.Rect; Maximum_First : out Positive)
   is
      Lines : constant Positive :=
        CuBit.UI.Editor.Documents.Line_Count (Source);
      Page : constant Positive :=
        CuBit.UI.Editor.Viewports.Line_Capacity (Source_View);
      Extent : constant Natural := Natural'Min
        (Source_Scrollbar.w, Source_Scrollbar.h / 2);
      Track_Frame : constant CuBit.UI.Rect :=
        (x => Source_Scrollbar.x, y => Source_Scrollbar.y + Extent,
         w => Source_Scrollbar.w,
         h => (if Source_Scrollbar.h > Extent * 2 then
                  Source_Scrollbar.h - Extent * 2 else 0));
      Total : constant Natural := Lines;
      Shown : constant Natural := Natural'Min (Page, Total);
      Thumb_Height : Natural;
      Travel : Natural;
      Position : Natural;
   begin
      Track :=
        (x => Track_Frame.x + 2, y => Track_Frame.y + 2,
         w => (if Track_Frame.w > 4 then Track_Frame.w - 4 else 0),
         h => (if Track_Frame.h > 4 then Track_Frame.h - 4 else 0));
      Maximum_First := (if Shown >= Total then 1 else Lines - Shown + 1);
      Thumb_Height := Natural'Min
        (Track.h, Natural'Max (12, Track.h * Shown / Total));
      Travel := Track.h - Thumb_Height;
      Position := CuBit.UI.Editor.Viewports.First_Line (Source_View) - 1;
      Thumb :=
        (x => Track.x,
         y => Track.y +
           (if Maximum_First = 1 then 0
            else Position * Travel / (Maximum_First - 1)),
         w => Track.w, h => Thumb_Height);
   end Source_Scrollbar_Metrics;

   procedure Draw_Title_Controls is
      type Icon_Rows is array (Natural range 0 .. 8) of String (1 .. 9);
      --  Compact mask from CuBit's attributed Bluecurve window-icon atlas.
      Close_Icon : constant Icon_Rows :=
        [".........",
         "..#...#..",
         ".###.###.",
         "..#####..",
         "...###...",
         "..#####..",
         ".###.###.",
         "..#...#..",
         "........."];
      Minimize : constant CuBit.UI.Rect :=
        (x => WIDTH - 60, y => 3, w => 18, h => 17);
      Maximize : constant CuBit.UI.Rect :=
        (x => WIDTH - 40, y => 3, w => 18, h => 17);
      Close : constant CuBit.UI.Rect :=
        (x => WIDTH - 20, y => 3, w => 18, h => 17);
   begin
      CuBit.UI.Draw_Button
        (Canvas, Minimize, Colors, CuBit.UI.Button_Normal, "");
      CuBit.UI.Fill_Rect
        (Canvas, (x => Minimize.x + 5, y => Minimize.y + 11,
                  w => 7, h => 2), Colors.text);
      CuBit.UI.Draw_Button
        (Canvas, Maximize, Colors, CuBit.UI.Button_Normal, "");
      CuBit.UI.Stroke_Rect
        (Canvas, (x => Maximize.x + 4, y => Maximize.y + 4,
                  w => 10, h => 9), Colors.text, Colors.text);
      CuBit.UI.Fill_Rect
        (Canvas, (x => Maximize.x + 5, y => Maximize.y + 5,
                  w => 8, h => 1), Colors.text);
      CuBit.UI.Draw_Button
        (Canvas, Close, Colors, CuBit.UI.Button_Normal, "");
      for Icon_Y in Close_Icon'Range loop
         for Icon_X in Close_Icon (Icon_Y)'Range loop
            if Close_Icon (Icon_Y) (Icon_X) = '#' then
               CuBit.UI.Set_Pixel
                 (Canvas, Close.x + 3 + Icon_X, Close.y + 4 + Icon_Y,
                  Colors.text);
            end if;
         end loop;
      end loop;
   end Draw_Title_Controls;

   function Hex_Digit (Value : Natural) return Character is
     (if Value < 10 then Character'Val (Character'Pos ('0') + Value)
      else Character'Val (Character'Pos ('A') + Value - 10));

   function Op_Byte (Op : CCL.VM.Op_Code) return String is
      Value : constant Natural := CCL.VM.Op_Code'Pos (Op);
   begin
      return String'(1 => Hex_Digit (Value / 16),
                     2 => Hex_Digit (Value mod 16));
   end Op_Byte;

   function Instruction_Text (Item : CCL.VM.Instruction) return String is
   begin
      case Item.Op is
         when CCL.VM.Push_Integer =>
            return "PUSH_INTEGER" & Integer_64'Image (Item.Immediate);
         when CCL.VM.Push_Boolean =>
            return "PUSH_BOOLEAN " &
              (if Item.Immediate = 0 then "false" else "true");
         when CCL.VM.Jump | CCL.VM.Jump_If_False =>
            return CCL.VM.Op_Code'Image (Item.Op) &
              CCL.VM.Instruction_Index'Image (Item.Target);
         when CCL.VM.Initialize_Local | CCL.VM.Copy_Local |
              CCL.VM.Move_Local | CCL.VM.Drop_Local |
              CCL.VM.Borrow_Local_RO | CCL.VM.Return_Local_RO |
              CCL.VM.Borrow_Local_RW | CCL.VM.Return_Local_RW =>
            return CCL.VM.Op_Code'Image (Item.Op) &
              Natural'Image (Natural (Item.Local));
         when others =>
            return CCL.VM.Op_Code'Image (Item.Op);
      end case;
   end Instruction_Text;

   procedure Render is
      Execution_Content : CuBit.UI.Rect;
      Editor_Content : CuBit.UI.Rect;
      Cursor_State : CuBit.UI.Editor.Cursors.Cursor_State;
      Cursor_Visuals : CuBit.UI.Text_Cursor_States
        (1 .. CuBit.UI.Editor.Cursors.MAX_CURSORS) :=
          [others => (cursor => 1, selectionStart => 1, selectionEnd => 1)];
      Cursor_Count : constant Positive :=
        CuBit.UI.Editor.Cursors.Length (Source_Cursors);
      Visual_Count : Positive := Cursor_Count;
   begin
      CuBit.UI.Fill_Rect
        (Canvas, (x => 0, y => 0, w => WIDTH, h => HEIGHT), Colors.desktop);

      CuBit.UI.Fill_Rect
        (Canvas, (x => 0, y => 0, w => WIDTH, h => 22), Colors.accent);
      CuBit.UI.Draw_UI_Text
        (Canvas, 6, 4, "CuBit CCL Workbench",
         Colors.selectionText, Colors.accent);
      Draw_Title_Controls;

      CuBit.UI.Draw_Menu_Bar
        (Canvas, (x => 0, y => 22, w => WIDTH, h => 22), Colors);
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 3, y => 22, w => 34, h => 21), Colors,
         False, False, "File");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 38, y => 22, w => 34, h => 21), Colors,
         False, False, "Edit");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 73, y => 22, w => 38, h => 21), Colors,
         False, False, "View");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 112, y => 22, w => 34, h => 21), Colors,
         False, False, "Run");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 147, y => 22, w => 38, h => 21), Colors,
         False, False, "Help");

      CuBit.UI.Widgets.Toolbar
        (Canvas, (x => 0, y => 44, w => WIDTH, h => 34), Colors);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Open_Button_Bounds, Colors,
         CuBit.UI.Widgets.Open_Document, enabled => False);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Save_Button_Bounds, Colors,
         CuBit.UI.Widgets.Save_Document, enabled => False);
      CuBit.UI.Widgets.Toolbar_Separator
        (Canvas, (x => 63, y => 47, w => 8, h => 27), Colors);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Compile_Button_Bounds, Colors,
         CuBit.UI.Widgets.Compile_Program,
         pressed => Compile_Button_Pressed);
      Run_Button_Bounds := (x => 73, y => 47, w => 27, h => 27);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Run_Button_Bounds, Colors, CuBit.UI.Widgets.Interpret_Source,
         pressed => Run_Button_Pressed);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, VM_Run_Button_Bounds, Colors, CuBit.UI.Widgets.Run_Program,
         enabled => Has_Verified, pressed => VM_Run_Button_Pressed);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Pause_Button_Bounds, Colors,
         CuBit.UI.Widgets.Pause_Program,
         enabled => VM_Has_State and then VM_Continuous and then
           not VM_Snapshot.Terminal,
         pressed => Pause_Button_Pressed);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Stop_Button_Bounds, Colors,
         CuBit.UI.Widgets.Stop_Program,
         enabled => VM_Has_State and then not VM_Snapshot.Terminal,
         pressed => Stop_Button_Pressed);
      CuBit.UI.Widgets.Toolbar_Separator
        (Canvas, (x => 218, y => 47, w => 8, h => 27), Colors);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Step_Into_Button_Bounds, Colors,
         CuBit.UI.Widgets.Step_Into,
         enabled => Has_Verified and then not VM_Continuous and then
           (not VM_Has_State or else not VM_Snapshot.Terminal),
         pressed => Step_Into_Button_Pressed);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Step_Over_Button_Bounds, Colors,
         CuBit.UI.Widgets.Step_Over,
         enabled => Has_Verified and then not VM_Continuous and then
           (not VM_Has_State or else not VM_Snapshot.Terminal),
         pressed => Step_Over_Button_Pressed);

      CuBit.UI.Widgets.Group_Box
        (Canvas, (x => 8, y => 82, w => 220, h => 280), Colors,
         "Execution", Execution_Content, 8);
      CuBit.UI.Draw_UI_Text
        (Canvas, Execution_Content.x, Execution_Content.y,
         "Debugger", Colors.text, Colors.face);
      CuBit.UI.Draw_UI_Text
        (Canvas, Execution_Content.x, Execution_Content.y + 22,
         "F5 interprets source", Colors.muted, Colors.face);
      declare
         Status_Text : constant String :=
           (if VM_Continuous then "running"
            elsif Breakpoint_Paused then "breakpoint"
            elsif VM_Has_Run then
               CCL.VM.Execution_Status'Image (Last_VM_Outcome.Status)
            elsif Has_Run then
               CCL.Language.Interpretation_Status'Image (Last_Outcome.Status)
            elsif Has_Verified then "compiled + verified"
            elsif Has_Compiled then "compiled; verification failed"
            else "not run");
         Fuel_Text : constant String :=
           (if VM_Has_Run then
               Unsigned_32'Image (Last_VM_Outcome.Fuel_Remaining)
            elsif Has_Run then Natural'Image (Last_Outcome.Fuel_Remaining)
            else "n/a");
         Location_Text : constant String :=
           (if Diagnostic_Line > 0 then
               Natural'Image (Diagnostic_Line) & ":" &
               Natural'Image (Diagnostic_Column)
            else "n/a");
      begin
         CuBit.UI.Widgets.Key_Value
           (Canvas,
            (x => Execution_Content.x, y => Execution_Content.y + 56,
             w => Execution_Content.w, h => 24),
            Colors, "Status", Status_Text);
         CuBit.UI.Widgets.Key_Value
           (Canvas,
            (x => Execution_Content.x, y => Execution_Content.y + 86,
             w => Execution_Content.w, h => 24),
            Colors, "Result", Result_Text (1 .. Result_Last));
         CuBit.UI.Widgets.Key_Value
           (Canvas,
            (x => Execution_Content.x, y => Execution_Content.y + 116,
             w => Execution_Content.w, h => 24),
            Colors, "Fuel left", Fuel_Text);
         CuBit.UI.Widgets.Key_Value
           (Canvas,
            (x => Execution_Content.x, y => Execution_Content.y + 146,
             w => Execution_Content.w, h => 24),
            Colors, "Location", Location_Text);
      end;

      CuBit.UI.Widgets.Group_Box
        (Canvas, (x => 236, y => 82, w => 390, h => 280), Colors,
         "CCL source - shared multiline editor", Editor_Content, 8);
      CuBit.UI.Draw_UI_Text
        (Canvas, Editor_Content.x, Editor_Content.y,
         "F5 or Ctrl+Enter runs; bounded fuel and history",
         Colors.muted, Colors.face);
      Source_Bounds :=
        (x => Editor_Content.x, y => Editor_Content.y + 24,
         w => Editor_Content.w - 18, h => Editor_Content.h - 48);
      declare
         Line_Height : constant Positive := CuBit.UI.UI_Text_Height + 2;
         Usable_Height : constant Natural :=
           (if Source_Bounds.h > 6 then Source_Bounds.h - 6 else 1);
         Visible_Lines : constant Positive :=
           Positive'Max (1, Usable_Height / Line_Height);
      begin
         CuBit.UI.Editor.Viewports.Set_Line_Capacity
           (Source_View, Visible_Lines,
            CuBit.UI.Editor.Documents.Line_Count (Source));
      end;
      Source_Scrollbar :=
        (x => Source_Bounds.x + Source_Bounds.w + 2,
         y => Source_Bounds.y, w => 16, h => Source_Bounds.h);
      for Index in 1 .. Cursor_Count loop
         Cursor_State :=
           CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         Cursor_Visuals (Index) :=
           (cursor => Cursor_State.Position,
            selectionStart => Positive'Min
              (Cursor_State.Position, Cursor_State.Anchor),
            selectionEnd => Positive'Max
              (Cursor_State.Position, Cursor_State.Anchor));
      end loop;
      if Has_Active_Debug_Entry and then
        Visual_Count < CuBit.UI.Editor.Cursors.MAX_CURSORS and then
        Active_Debug_Entry.Source_First > 0 and then
        Active_Debug_Entry.Source_End > Active_Debug_Entry.Source_First
      then
         declare
            Debug_First : constant
              CuBit.UI.Editor.Documents.Document_Position :=
                CuBit.UI.Editor.Documents.Document_Position'Min
                  (Active_Debug_Entry.Source_First,
                   CuBit.UI.Editor.Documents.Length (Source) + 1);
            Debug_End : constant
              CuBit.UI.Editor.Documents.Document_Position :=
                CuBit.UI.Editor.Documents.Document_Position'Min
                  (Active_Debug_Entry.Source_End,
                   CuBit.UI.Editor.Documents.Length (Source) + 1);
         begin
            if Debug_End > Debug_First then
               Visual_Count := Visual_Count + 1;
               Cursor_Visuals (Visual_Count) :=
                 (cursor => Debug_First,
                  selectionStart => Debug_First,
                  selectionEnd => Debug_End);
            end if;
         end;
      end if;
      CuBit.UI.Draw_Multiline_Text_Edit_Multiple
        (CuBit.UI.With_Clip (Canvas, Source_Bounds), Source_Bounds, Colors,
         CuBit.UI.Editor.Documents.Content (Source),
         CuBit.UI.Editor.Viewports.First_Line (Source_View),
         CuBit.UI.Editor.Viewports.Line_Capacity (Source_View),
         Cursor_Visuals (1 .. Visual_Count),
         focused => True, hot => False);
      CuBit.UI.Draw_Vertical_Scrollbar
        (Canvas, Source_Scrollbar, Colors, 1,
         CuBit.UI.Editor.Documents.Line_Count (Source),
         CuBit.UI.Editor.Viewports.First_Line (Source_View),
         hot => False,
         active => Source_Scrollbar_Pressed /= CuBit.UI.Scrollbar_None,
         pageSize => CuBit.UI.Editor.Viewports.Line_Capacity (Source_View),
         pressedPart => Source_Scrollbar_Pressed);
      CuBit.UI.Draw_UI_Text
        (Canvas, Editor_Content.x,
         Editor_Content.y + Editor_Content.h - 18,
         "SHARED core - Linux presentation adapter",
         Colors.muted, Colors.face);

      CuBit.UI.Widgets.Group_Box
        (Canvas, (x => 634, y => 82, w => 258, h => 280), Colors,
         "CCLB bytecode", Bytecode_Content, 8);
      CuBit.UI.Draw_Table_Header
        (Canvas,
         (x => Bytecode_Content.x, y => Bytecode_Content.y,
          w => Bytecode_Content.w, h => 24),
         Colors, "PC", "Bytes", "Instruction");
      if Has_Compiled then
         declare
            Row_Height : constant Positive := BYTECODE_ROW_HEIGHT;
            Maximum_Rows : constant Natural :=
              (Bytecode_Content.h - 54) / Row_Height;
            Rows : constant Natural := Natural'Min
              (Natural (Compiled_Artifact.Program.Length), Maximum_Rows);
            Item : CCL.VM.Instruction;
            PC   : CCL.VM.Instruction_Index;
            Row_Y : Natural;
            Listing_Clip : constant CuBit.UI.Canvas := CuBit.UI.With_Clip
              (Canvas,
               (x => Bytecode_Content.x, y => Bytecode_Content.y + 28,
                w => Bytecode_Content.w,
                h => Bytecode_Content.h - 52));
         begin
            if Rows > 0 then
               for Row in 0 .. Rows - 1 loop
                  PC := CCL.VM.Instruction_Index (Row);
                  Item := Compiled_Artifact.Program.Code (PC);
                  Row_Y := Bytecode_Content.y + 31 + Row * Row_Height;
                  CuBit.UI.Draw_Table_Row
                    (Listing_Clip,
                     (x => Bytecode_Content.x, y => Row_Y,
                      w => Bytecode_Content.w, h => Row_Height),
                     Colors,
                     selected => VM_Has_State and then
                       VM_Snapshot.Instruction = PC,
                     hot => False,
                     c1 =>
                       ((if Breakpoints (PC) then "*" else " ") &
                        Natural'Image (Row)),
                     c2 => Op_Byte (Item.Op),
                     c3 => Instruction_Text (Item));
               end loop;
            end if;
            CuBit.UI.Draw_UI_Text
              (CuBit.UI.With_Clip
                 (Canvas,
                  (x => Bytecode_Content.x,
                   y => Bytecode_Content.y + Bytecode_Content.h - 20,
                   w => Bytecode_Content.w, h => 18)),
               Bytecode_Content.x,
               Bytecode_Content.y + Bytecode_Content.h - 20,
               (if Has_Verified then "VM verifier: VALID"
                else "VM verifier: REJECTED"),
               (if Has_Verified then Colors.text else Colors.muted),
               Colors.face);
         end;
      else
         CuBit.UI.Draw_UI_Text
           (CuBit.UI.With_Clip
              (Canvas,
               (x => Bytecode_Content.x, y => Bytecode_Content.y + 34,
                w => Bytecode_Content.w, h => 44)),
            Bytecode_Content.x, Bytecode_Content.y + 34,
            "No CCLB artifact", Colors.text, Colors.face);
         CuBit.UI.Draw_UI_Text
           (CuBit.UI.With_Clip
              (Canvas,
               (x => Bytecode_Content.x, y => Bytecode_Content.y + 54,
                w => Bytecode_Content.w, h => 44)),
            Bytecode_Content.x, Bytecode_Content.y + 54,
            "Interpret mode does not emit bytecode",
            Colors.muted, Colors.face);
      end if;

      CuBit.UI.Draw_Status_Bar
        (Canvas, (x => 0, y => HEIGHT - 26, w => WIDTH, h => 26), Colors,
         Toolbar_Hint,
         "bounded document • proved viewport");
   end Render;

begin
   declare
      Source_Result : CuBit.UI.Editor.Documents.Edit_Result;
   begin
      CuBit.UI.Editor.Documents.Initialize
        (Source,
         "(let ((answer (+ 20 22)))" & ASCII.LF &
         "  (if (= answer 42)" & ASCII.LF &
         "      (+ answer 6)" & ASCII.LF &
         "      0))" & ASCII.LF,
         Source_Result);
      if Source_Result /= CuBit.UI.Editor.Documents.Applied then
         raise Program_Error;
      end if;
      CuBit.UI.Editor.Cursors.Initialize (Source_Cursors, 1);
      Source_Histories.Initialize (Source_History);
      CuBit.UI.Editor.Viewports.Initialize (Source_View, 15);
   end;
   Result_Text (1 .. Result_Last) := "ready";
   declare
      Handle : constant System.Address :=
        Window_Open (Interfaces.C.int (WIDTH), Interfaces.C.int (HEIGHT));
      Kind : aliased Interfaces.C.int := 0;
      Code : aliased Interfaces.C.unsigned := 0;
      Modifiers : aliased Interfaces.C.unsigned := 0;
      Mouse_X : aliased Interfaces.C.int := 0;
      Mouse_Y : aliased Interfaces.C.int := 0;
      Running : Boolean := Handle /= System.Null_Address;
      Dragging : Boolean := False;
      Dragging_Scrollbar : Boolean := False;
      Scrollbar_Grab_Offset : Natural := 0;
      Next_Scrollbar_Repeat : Interfaces.Unsigned_64 := 0;
      SCROLL_REPEAT_DELAY : constant Interfaces.Unsigned_64 := 350;
      SCROLL_REPEAT_INTERVAL : constant Interfaces.Unsigned_64 := 60;
      Changed : Boolean;
      Extend : Boolean;
      By_Word : Boolean;
      Cursor_Toggle : CuBit.UI.Editor.Cursors.Toggle_Result;
   begin
      if not Running then raise Program_Error; end if;
      Render;
      while Running loop
         while Running and then
           Window_Poll
             (Handle, Kind'Access, Code'Access, Modifiers'Access,
              Mouse_X'Access, Mouse_Y'Access) /= 0
         loop
            case Kind is
               when 1 => Running := False;
               when 2 =>
                  if Code >= 32 and then Code <= 126 then
                     Insert_Source
                       (String'(1 => Character'Val (Code)), Changed);
                  end if;
               when 3 =>
                  Backspace_Source (Changed);
               when 4 =>
                  Insert_Source
                    (String'(1 => ASCII.LF), Changed,
                     Source_Histories.Other_Edit);
               when 5 | 6 =>
                  Extend := (Modifiers and 1) /= 0;
                  By_Word := (Modifiers and 2) /= 0;
                  Move_Source_Horizontal
                    (Right => Kind = 6, By_Word => By_Word,
                     Extend_Selection => Extend);
               when 7 =>
                  Move_Source_Line_End
                    (To_End => False,
                     Extend_Selection => (Modifiers and 1) /= 0);
               when 8 =>
                  Move_Source_Line_End
                    (To_End => True,
                     Extend_Selection => (Modifiers and 1) /= 0);
               when 9 =>
                  Delete_Source_Forward (Changed);
               when 10 =>
                  Select_All_Source;
               when 11 | 14 | 15 =>
                  Dragging_Scrollbar := False;
                  Source_Scrollbar_Pressed := CuBit.UI.Scrollbar_None;
                  Next_Scrollbar_Repeat := 0;
                  if Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Compile_Button_Bounds)
                  then
                     Compile_Button_Pressed := True;
                     Dragging := False;
                  elsif Has_Verified and then Mouse_X >= 0 and then
                    Mouse_Y >= 0 and then CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       VM_Run_Button_Bounds)
                  then
                     VM_Run_Button_Pressed := True;
                     Dragging := False;
                  elsif VM_Has_State and then VM_Continuous and then
                    not VM_Snapshot.Terminal and then Mouse_X >= 0 and then
                    Mouse_Y >= 0 and then CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Pause_Button_Bounds)
                  then
                     Pause_Button_Pressed := True;
                     Dragging := False;
                  elsif VM_Has_State and then not VM_Snapshot.Terminal and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Stop_Button_Bounds)
                  then
                     Stop_Button_Pressed := True;
                     Dragging := False;
                  elsif Has_Verified and then not VM_Continuous and then
                    (not VM_Has_State or else not VM_Snapshot.Terminal) and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Step_Into_Button_Bounds)
                  then
                     Step_Into_Button_Pressed := True;
                     Dragging := False;
                  elsif Has_Verified and then not VM_Continuous and then
                    (not VM_Has_State or else not VM_Snapshot.Terminal) and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Step_Over_Button_Bounds)
                  then
                     Step_Over_Button_Pressed := True;
                     Dragging := False;
                  elsif Has_Compiled and then Mouse_X >= 0 and then
                    Mouse_Y >= 0 and then CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Bytecode_Content)
                    and then Natural (Mouse_Y) >= Bytecode_Content.y + 31
                  then
                     declare
                        Row : constant Natural :=
                          (Natural (Mouse_Y) - Bytecode_Content.y - 31) /
                            BYTECODE_ROW_HEIGHT;
                     begin
                        if Row < Natural (Compiled_Artifact.Program.Length) then
                           Breakpoints (CCL.VM.Instruction_Index (Row)) :=
                             not Breakpoints (CCL.VM.Instruction_Index (Row));
                           Set_Result
                             ((if Breakpoints
                                (CCL.VM.Instruction_Index (Row))
                               then "breakpoint set at PC"
                               else "breakpoint cleared at PC") &
                              Natural'Image (Row));
                        end if;
                        Dragging := False;
                     end;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Run_Button_Bounds)
                  then
                     Run_Button_Pressed := True;
                     Dragging := False;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Source_Bounds)
                  then
                     if (Modifiers and 2) /= 0 then
                        Source_Histories.Break_Sequence (Source_History);
                        CuBit.UI.Editor.Cursors.Toggle_At
                          (Source_Cursors,
                           Source_Position_At
                             (Natural (Mouse_X), Natural (Mouse_Y)),
                           Cursor_Toggle);
                        if Cursor_Toggle =
                          CuBit.UI.Editor.Cursors.Cursor_Limit_Reached
                        then
                           Set_Result ("cursor limit reached");
                        end if;
                        Dragging := False;
                        Reveal_Source_Cursor;
                     else
                        declare
                           Click_Position : constant
                             CuBit.UI.Editor.Documents.Document_Position :=
                               Source_Position_At
                                 (Natural (Mouse_X), Natural (Mouse_Y));
                        begin
                           CuBit.UI.Editor.Cursors.Initialize
                             (Source_Cursors, Click_Position);
                           Place_Source_Cursor
                             (Click_Position,
                              Extend_Selection => (Modifiers and 1) /= 0);
                        end;
                        Dragging := True;
                     end if;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Source_Scrollbar)
                  then
                     declare
                        Lines : constant Positive :=
                          CuBit.UI.Editor.Documents.Line_Count (Source);
                        Extent : constant Natural := Source_Scrollbar.w;
                        Track, Thumb : CuBit.UI.Rect;
                        Maximum_First : Positive;
                        Relative_Y : Natural;
                        Target : Positive;
                     begin
                        Source_Scrollbar_Metrics
                          (Track, Thumb, Maximum_First);
                        Dragging := False;
                        if Maximum_First > 1 and then
                          CuBit.UI.Point_In_Rect
                            (Natural (Mouse_X), Natural (Mouse_Y), Thumb)
                        then
                           Dragging_Scrollbar := True;
                           Source_Scrollbar_Pressed :=
                             CuBit.UI.Scrollbar_Thumb;
                           Scrollbar_Grab_Offset :=
                             Natural (Mouse_Y) - Thumb.y;
                        elsif Natural (Mouse_Y) <
                          Source_Scrollbar.y + Extent
                        then
                           if CuBit.UI.Editor.Viewports.First_Line
                             (Source_View) > 1
                           then
                              Source_Scrollbar_Pressed :=
                                CuBit.UI.Scrollbar_Decrement;
                              Next_Scrollbar_Repeat :=
                                Window_Ticks + SCROLL_REPEAT_DELAY;
                              CuBit.UI.Editor.Viewports.Scroll_Lines
                                (Source_View, -1, Lines);
                           end if;
                        elsif Natural (Mouse_Y) >=
                          Source_Scrollbar.y + Source_Scrollbar.h - Extent
                        then
                           if CuBit.UI.Editor.Viewports.First_Line
                             (Source_View) < Maximum_First
                           then
                              Source_Scrollbar_Pressed :=
                                CuBit.UI.Scrollbar_Increment;
                              Next_Scrollbar_Repeat :=
                                Window_Ticks + SCROLL_REPEAT_DELAY;
                              CuBit.UI.Editor.Viewports.Scroll_Lines
                                (Source_View, 1, Lines);
                           end if;
                        else
                           if Maximum_First > 1 then
                              Source_Scrollbar_Pressed :=
                                CuBit.UI.Scrollbar_Track;
                           end if;
                           Relative_Y :=
                             (if Natural (Mouse_Y) <= Track.y then 0
                              else Natural'Min
                                (Natural (Mouse_Y) - Track.y, Track.h - 1));
                           Target := 1 + Relative_Y * (Maximum_First - 1) /
                             Natural'Max (1, Track.h - 1);
                           CuBit.UI.Editor.Viewports.Scroll_Lines
                             (Source_View,
                              Integer (Target) - Integer
                                (CuBit.UI.Editor.Viewports.First_Line
                                   (Source_View)),
                              Lines);
                        end if;
                     end;
                  end if;
               when 12 =>
                  if Dragging_Scrollbar and then Mouse_Y >= 0 then
                     declare
                        Track, Thumb : CuBit.UI.Rect;
                        Maximum_First : Positive;
                        Travel, Relative_Y : Natural;
                        Pointer_Y : constant Natural := Natural (Mouse_Y);
                        Target : Positive;
                     begin
                        Source_Scrollbar_Metrics
                          (Track, Thumb, Maximum_First);
                        Travel := Track.h - Thumb.h;
                        if Pointer_Y <= Track.y + Scrollbar_Grab_Offset then
                           Relative_Y := 0;
                        else
                           Relative_Y := Natural'Min
                             (Pointer_Y - Track.y - Scrollbar_Grab_Offset,
                              Travel);
                        end if;
                        Target := 1 + Relative_Y * (Maximum_First - 1) /
                          Natural'Max (1, Travel);
                        CuBit.UI.Editor.Viewports.Scroll_Lines
                          (Source_View,
                           Integer (Target) - Integer
                             (CuBit.UI.Editor.Viewports.First_Line
                                (Source_View)),
                           CuBit.UI.Editor.Documents.Line_Count (Source));
                     end;
                  elsif Dragging and then Mouse_X >= 0 then
                     if Mouse_Y >= 0 then
                        Place_Source_Cursor
                          (Source_Position_At
                             (Natural (Mouse_X), Natural (Mouse_Y)),
                           Extend_Selection => True);
                     end if;
                  end if;
               when 13 =>
                  if Compile_Button_Pressed and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Compile_Button_Bounds)
                  then
                     Compile_Source;
                  end if;
                  if VM_Run_Button_Pressed and then Has_Verified and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       VM_Run_Button_Bounds)
                  then
                     Start_Bytecode;
                  end if;
                  if Pause_Button_Pressed and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Pause_Button_Bounds)
                  then
                     Pause_Bytecode;
                  end if;
                  if Stop_Button_Pressed and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Stop_Button_Bounds)
                  then
                     Stop_Bytecode;
                  end if;
                  if Step_Into_Button_Pressed and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Step_Into_Button_Bounds)
                  then
                     Step_Bytecode;
                  end if;
                  if Step_Over_Button_Pressed and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Step_Over_Button_Bounds)
                  then
                     Step_Over_Bytecode;
                  end if;
                  if Run_Button_Pressed and then
                    Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Run_Button_Bounds)
                  then
                     Run_Source;
                  end if;
                  Compile_Button_Pressed := False;
                  VM_Run_Button_Pressed := False;
                  Pause_Button_Pressed := False;
                  Stop_Button_Pressed := False;
                  Step_Into_Button_Pressed := False;
                  Step_Over_Button_Pressed := False;
                  Run_Button_Pressed := False;
                  Dragging := False;
                  Dragging_Scrollbar := False;
                  Source_Scrollbar_Pressed := CuBit.UI.Scrollbar_None;
                  Next_Scrollbar_Repeat := 0;
               when 16 | 17 =>
                  if (Modifiers and 6) = 6 or else
                    (Modifiers and 3) = 3
                  then
                     Add_Source_Cursor_Vertically
                       ((if Kind = 16 then
                           CuBit.UI.Editor.Documents.Up
                         else CuBit.UI.Editor.Documents.Down));
                  else
                     Move_Source_Vertical
                       ((if Kind = 16 then
                           CuBit.UI.Editor.Documents.Up
                         else CuBit.UI.Editor.Documents.Down),
                        Extend_Selection => (Modifiers and 1) /= 0);
                  end if;
               when 18 =>
                  CuBit.UI.Editor.Viewports.Scroll_Lines
                    (Source_View, -3,
                     CuBit.UI.Editor.Documents.Line_Count (Source));
               when 19 =>
                  CuBit.UI.Editor.Viewports.Scroll_Lines
                    (Source_View, 3,
                     CuBit.UI.Editor.Documents.Line_Count (Source));
               when 20 | 21 =>
                  CuBit.UI.Editor.Viewports.Scroll_Lines
                    (Source_View,
                     (if Kind = 20 then
                         -Integer
                           (CuBit.UI.Editor.Viewports.Line_Capacity
                              (Source_View))
                      else Integer
                        (CuBit.UI.Editor.Viewports.Line_Capacity
                           (Source_View))),
                     CuBit.UI.Editor.Documents.Line_Count (Source));
               when 22 =>
                  Source_Histories.Break_Sequence (Source_History);
                  Collapse_Source_Cursors;
                  Reveal_Source_Cursor;
               when 23 =>
                  if Source_Histories.Can_Undo (Source_History)
                  then
                     Source_Histories.Undo
                       (Source_History, Source, Source_Cursors);
                     Invalidate_Run_Result;
                     Reveal_Source_Cursor;
                  end if;
               when 24 =>
                  if Source_Histories.Can_Redo (Source_History)
                  then
                     Source_Histories.Redo
                       (Source_History, Source, Source_Cursors);
                     Invalidate_Run_Result;
                     Reveal_Source_Cursor;
                  end if;
               when 25 =>
                  Run_Source;
               when 26 =>
                  if Mouse_X >= 0 and then Mouse_Y >= 0 then
                     Pointer_X := Natural (Mouse_X);
                     Pointer_Y := Natural (Mouse_Y);
                     Pointer_Known := True;
                  end if;
               when others => null;
            end case;
         end loop;
         exit when not Running;
         if (Source_Scrollbar_Pressed = CuBit.UI.Scrollbar_Decrement or else
             Source_Scrollbar_Pressed = CuBit.UI.Scrollbar_Increment) and then
           Window_Ticks >= Next_Scrollbar_Repeat
         then
            declare
               Lines : constant Positive :=
                 CuBit.UI.Editor.Documents.Line_Count (Source);
               Maximum_First : constant Positive :=
                 (if CuBit.UI.Editor.Viewports.Line_Capacity (Source_View) >=
                    Lines
                  then 1
                  else Lines -
                    CuBit.UI.Editor.Viewports.Line_Capacity (Source_View) + 1);
               Moving_Up : constant Boolean :=
                 Source_Scrollbar_Pressed = CuBit.UI.Scrollbar_Decrement;
            begin
               if (Moving_Up and then
                   CuBit.UI.Editor.Viewports.First_Line (Source_View) > 1) or else
                 (not Moving_Up and then
                  CuBit.UI.Editor.Viewports.First_Line (Source_View) <
                    Maximum_First)
               then
                  CuBit.UI.Editor.Viewports.Scroll_Lines
                    (Source_View, (if Moving_Up then -1 else 1), Lines);
                  Next_Scrollbar_Repeat :=
                    Window_Ticks + SCROLL_REPEAT_INTERVAL;
               else
                  Source_Scrollbar_Pressed := CuBit.UI.Scrollbar_None;
                  Next_Scrollbar_Repeat := 0;
               end if;
            end;
         end if;
         if VM_Continuous then
            declare
               Current_PC : constant CCL.VM.Instruction_Index :=
                 VM_Snapshot.Instruction;
            begin
               if Breakpoints (Current_PC) and then
                 not Ignore_Current_Breakpoint
               then
                  VM_Continuous := False;
                  VM_Step_Over_Active := False;
                  Breakpoint_Paused := True;
                  Set_Result
                    ("breakpoint at PC" &
                     Natural'Image (Natural (Current_PC)));
               else
                  Ignore_Current_Breakpoint := False;
                  Advance_Bytecode (1);
                  if VM_Step_Over_Active and then
                    (VM_Snapshot.Terminal or else VM_Snapshot.Waiting or else
                     CCL.VM.Program_Length (VM_Snapshot.Instruction) >=
                       Step_Over_End)
                  then
                     VM_Continuous := False;
                     VM_Step_Over_Active := False;
                     if not VM_Snapshot.Terminal and then
                       not VM_Snapshot.Waiting
                     then
                        Set_Result ("step over complete");
                     end if;
                  end if;
               end if;
            end;
         end if;
         Render;
         exit when Window_Present
           (Handle, Pixels'Address, Interfaces.C.int (WIDTH * 4)) /= 0;
         Window_Wait;
      end loop;
      Window_Close (Handle);
   end;
end Main;
