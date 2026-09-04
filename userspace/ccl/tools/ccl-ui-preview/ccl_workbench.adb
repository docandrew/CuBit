with Interfaces; use Interfaces;
with System;
with CCL.Catalog;
with CCL.Interfaces.Clock;
with CCL.Language;
with CCL.Compiler;
with CCL.Debug_Maps;
with CCL.Ownership;
with CCL.VM;
with CCL_Workbench_Platform;
with CuBit.UI;
with CuBit.UI.Editor;
with CuBit.UI.Editor.Cursors;
with CuBit.UI.Editor.Documents;
with CuBit.UI.Editor_History;
with CuBit.UI.Editor.Search;
with CuBit.UI.Editor.Transactions;
with CuBit.UI.Editor.Viewports;
with CuBit.UI.Layout;
with CuBit.UI.Widgets;

--  Shared CCL Workbench. Rendering uses the CuBit UI canvas; the selected
--  platform adapter is the only presentation, input, and host-service edge.
package body CCL_Workbench is
   use type System.Address;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Catalog.Link_Result;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Debug_Maps.Validation_Error;
   use type CCL.VM.Validation_Error;
   use type CCL.VM.Execution_Status;
   use type CCL.VM.Instruction_Index;
   use type CCL.VM.Program_Length;
   use type CCL.VM.Stack_Depth;
   use type CCL.VM.Value_Kind;
   use type CCL.VM.Authority_Class;
   use type CuBit.UI.Editor.Documents.Edit_Result;
   use type CuBit.UI.Editor.Cursors.Toggle_Result;
   use type CuBit.UI.Editor.Cursors.Add_Result;
   use type CuBit.UI.Editor.Search.Search_Status;
   use type CuBit.UI.Scrollbar_Part;

   --  Compact native canvas: never downscale the toolkit's 11 px UI font.
   --  The hosted adapter scales this canvas upward when space permits.
   WIDTH  : constant Natural := 900;
   HEIGHT : constant Natural := 400;
   MAXIMUM_WIDTH  : constant Natural := 1_280;
   MAXIMUM_HEIGHT : constant Natural := 720;
   SOURCE_CAPACITY : constant := 4_096;
   MAX_SOURCE_STYLE_SPANS : constant Positive := SOURCE_CAPACITY;
   BYTECODE_ROW_HEIGHT : constant Positive := CuBit.UI.Code_Text_Height + 3;
   WORKSPACE_MARGIN : constant Natural := 8;
   function Window_Has_System_Chrome return Integer_32
   with Import, Convention => C,
        External_Name => "ccl_window_has_system_chrome";

   CLIENT_TITLE_HEIGHT : constant Natural :=
     (if Window_Has_System_Chrome /= 0 then 0 else 22);
   WORKSPACE_TOP : constant Natural := CLIENT_TITLE_HEIGHT + 60;
   WORKSPACE_BOTTOM : constant Natural := 38;
   MINIMUM_INSPECTOR_WIDTH : constant Natural := 170;
   MINIMUM_SOURCE_WIDTH : constant Natural := 320;
   MINIMUM_DISASSEMBLY_WIDTH : constant Natural := 210;
   SPLITTER_WIDTH : constant Natural := 8;
   MINIMUM_TABLE_COLUMN_WIDTH : constant Natural := 30;
   MINIMUM_INSTRUCTION_COLUMN_WIDTH : constant Natural := 80;
   Bytecode_Columns : CuBit.UI.Table_Column_Layout :=
     (First_Width => 42, Second_Width => 50, Cell_Padding => 5);

   --  HOSTED/LINUX adapter binding.  This is deliberately not part of the
   --  language, compiler, or VM: a CuBit linker will resolve the same pinned
   --  interface operation to an authorized service endpoint.
   CLOCK_HOST_BINDING : constant Unsigned_32 := 16#0001_0001#;
   type Pixel_Buffer is
     array (Natural range 0 .. MAXIMUM_WIDTH * MAXIMUM_HEIGHT - 1)
     of aliased Unsigned_32;
   Pixels : aliased Pixel_Buffer := [others => 0];

   Canvas : CuBit.UI.Canvas :=
     (addr => Pixels'Address, width => WIDTH, height => HEIGHT,
      pitch => MAXIMUM_WIDTH * 4,
      clipEnabled => False, clip => (others => 0));
   Colors : constant CuBit.UI.Theme := CuBit.UI.CuBit_Alloy;
   Visible_Interfaces : CCL.Catalog.Interface_Catalog;
   Granted_Interfaces : CCL.Catalog.Granted_Bindings;

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
   VM_Inspection : CCL.VM.Inspection_Snapshot;
   Has_VM_Inspection : Boolean := False;
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
   Find_Query : CuBit.UI.Editor.Edit_State;
   Find_Active : Boolean := False;
   Source_Styles : CuBit.UI.Text_Style_Spans
     (1 .. MAX_SOURCE_STYLE_SPANS) :=
       [others => (firstPosition => 1, lastPosition => 1,
                   foreground => 0,
                   decoration => CuBit.UI.No_Text_Decoration,
                   decorationColor => 0)];
   Source_Style_Count : Natural range 0 .. MAX_SOURCE_STYLE_SPANS := 0;
   Source_Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
   package Source_Histories is new CuBit.UI.Editor_History
     (Capacity => SOURCE_CAPACITY, Depth => 32);
   Source_History : Source_Histories.History;
   Source_View : CuBit.UI.Editor.Viewports.Viewport;
   Source_Bounds : CuBit.UI.Rect := (others => 0);
   Source_Scrollbar : CuBit.UI.Rect := (others => 0);
   Source_Horizontal_Scrollbar : CuBit.UI.Rect := (others => 0);
   Bytecode_Content : CuBit.UI.Rect := (others => 0);
   Bytecode_Table : CuBit.UI.Table_Regions :=
     (Header => (others => 0), Rows => (others => 0));
   Inspector_Splitter : CuBit.UI.Rect := (others => 0);
   Disassembly_Splitter : CuBit.UI.Rect := (others => 0);
   First_Column_Divider : CuBit.UI.Rect := (others => 0);
   Second_Column_Divider : CuBit.UI.Rect := (others => 0);
   Inspector_Width : Natural := 220;
   Disassembly_Width : Natural := 258;
   type Resize_Target is
     (No_Resize, Inspector_Pane, Disassembly_Pane,
      First_Table_Column, Second_Table_Column);
   Active_Resize : Resize_Target := No_Resize;
   Open_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 5, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
   Save_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 34, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
   Compile_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 102, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
   VM_Run_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 131, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
   Pause_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 160, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
   Stop_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 189, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
   Step_Into_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 228, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
   Step_Over_Button_Bounds : constant CuBit.UI.Rect :=
     (x => 257, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
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
   Source_Horizontal_Scrollbar_Pressed : CuBit.UI.Scrollbar_Part :=
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
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Inspector_Splitter)
      then
         return "Drag to resize the execution inspector";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Disassembly_Splitter)
      then
         return "Drag to resize the disassembly pane";
      elsif CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, First_Column_Divider) or else
        CuBit.UI.Point_In_Rect
          (Pointer_X, Pointer_Y, Second_Column_Divider)
      then
         return "Drag to resize the disassembly columns";
      elsif Has_Compiled and then CuBit.UI.Point_In_Rect
        (Pointer_X, Pointer_Y, Bytecode_Content)
      then
         return "Click a bytecode row to toggle a mapped breakpoint";
      else
         return "CCL Workbench";
      end if;
   end Toolbar_Hint;

   function Window_Open (Width, Height : Integer_32) return System.Address
   with Import, Convention => C, External_Name => "ccl_window_open";
   function Window_Poll
     (Handle : System.Address; Kind : access Integer_32;
      Code, Modifiers : access Unsigned_32;
      X, Y : access Integer_32) return Integer_32
   with Import, Convention => C, External_Name => "ccl_window_poll";
   function Window_Prepare_Frame
     (Handle : System.Address;
      Minimum_Width, Minimum_Height : Integer_32;
      Maximum_Width, Maximum_Height : Integer_32;
      Width, Height : access Integer_32) return Integer_32
   with Import, Convention => C, External_Name => "ccl_window_prepare_frame";
   function Window_Present
     (Handle, Pixels : System.Address;
      Pitch : Integer_32) return Integer_32
   with Import, Convention => C, External_Name => "ccl_window_present";
   procedure Window_Wait
   with Import, Convention => C, External_Name => "ccl_window_wait";
   function Window_Ticks return Interfaces.Unsigned_64
   with Import, Convention => C, External_Name => "ccl_window_ticks";
   function Window_Clock_Monotonic
     (Success : access Integer_32) return Interfaces.Unsigned_64
   with Import, Convention => C,
        External_Name => "ccl_window_clock_monotonic";
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
      Has_VM_Inspection := False;
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

   procedure Initialize_Visible_Interfaces is
      Resolved   : CCL.Catalog.Resolved_Operation;
      Error      : CCL.Catalog.Catalog_Error;
      Grant      : CCL.Catalog.Grant_Result;
      Found      : Boolean;
   begin
      CCL.Catalog.Initialize (Visible_Interfaces);
      CCL.Catalog.Initialize (Granted_Interfaces);
      CCL.Interfaces.Clock.Publish (Visible_Interfaces, Error);
      if Error /= CCL.Catalog.Catalog_Valid then
         raise Program_Error with "invalid hosted CCL interface catalog";
      end if;
      CCL.Interfaces.Clock.Resolve_Monotonic_Ms
        (Visible_Interfaces, Resolved, Found);
      if not Found then
         raise Program_Error with "hosted clock interface not discoverable";
      end if;
      CCL.Catalog.Install
        (Granted_Interfaces, Resolved, CLOCK_HOST_BINDING, Grant);
      if Grant /= CCL.Catalog.Grant_Added then
         raise Program_Error with "hosted clock authority not installed";
      end if;
   end Initialize_Visible_Interfaces;

   procedure Run_Source is
      Outcome : CCL.Language.Interpretation_Result;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Line, Column : Positive;
      Text : constant String :=
        CuBit.UI.Editor.Documents.Content (Source);
   begin
      VM_Continuous := False;
      VM_Has_Run := False;
      CCL.Language.Interpret (Text, 4_096, Visible_Interfaces, Outcome);
      Last_Outcome := Outcome;
      Has_Run := True;
      Diagnostic_Line := 0;
      Diagnostic_Column := 0;
      if Outcome.Status = CCL.Language.Succeeded then
         if not Outcome.Has_Value then
            Set_Result ("ok");
         elsif Outcome.Has_Text then
            if Outcome.Result_Text.Length = 0 then
               Set_Result ("");
            else
               Set_Result
                 (Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length));
            end if;
         elsif Outcome.Has_Character then
            Set_Result (String'(1 => Outcome.Result_Character));
         elsif Outcome.Result_Value.Kind = CCL.VM.Integer_Value then
            Set_Result (Integer_64'Image (Outcome.Result_Value.Integer));
         else
            Set_Result
              ((if Outcome.Result_Value.Boolean then "true" else "false"));
         end if;
      elsif Outcome.Status = CCL.Language.Host_Import_Required then
         Set_Result ("host import requires compiled VM mode");
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
   procedure Update_VM_Inspection;

   procedure Compile_Source is
      Analysis : CCL.Language.Analysis_Result;
      Error    : CCL.VM.Validation_Error;
      Link_Error : CCL.Catalog.Link_Result;
      Debug_Error : CCL.Debug_Maps.Validation_Error;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Line, Column : Positive;
      Text : constant String :=
        CuBit.UI.Editor.Documents.Content (Source);
   begin
      Has_Run := False;
      VM_Has_Run := False;
      VM_Has_State := False;
      Has_VM_Inspection := False;
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

      CCL.Language.Analyze (Text, Visible_Interfaces, Analysis);
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

      CCL.Catalog.Link_Program
        (Granted_Interfaces, Compiled_Artifact.Linkage,
         Compiled_Artifact.Program, Link_Error);
      if Link_Error /= CCL.Catalog.Link_Valid then
         Has_Compiled := False;
         Set_Result
           ("link: " & CCL.Catalog.Link_Result'Image (Link_Error));
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
         Update_VM_Inspection;
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

   procedure Update_VM_Inspection is
   begin
      if Has_Verified and then VM_Has_State then
         CCL.VM.Inspect (Verified_Artifact, VM_State, VM_Inspection);
         Has_VM_Inspection := True;
      else
         Has_VM_Inspection := False;
      end if;
   end Update_VM_Inspection;

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
      if Last_VM_Outcome.Status = CCL.VM.Waiting_For_Host and then
        Last_VM_Outcome.Requested_Authority = CCL.VM.Observe_Authority and then
        Last_VM_Outcome.Requested_Binding = CLOCK_HOST_BINDING
      then
         declare
            Clock_OK : aliased Integer_32 := 0;
            Clock_Value : constant Unsigned_64 :=
              Window_Clock_Monotonic (Clock_OK'Access);
         begin
            CCL.VM.Complete_Host_Call
              (Verified_Artifact, VM_State,
               CCL.VM.Integer_Constant
                 (Integer_64
                    (Unsigned_64'Min
                       (Clock_Value, Unsigned_64 (Integer_64'Last)))),
               Clock_OK /= 0);
         end;
         CCL.VM.Continue_Execution_For
           (Verified_Artifact, VM_State, 0, Last_VM_Outcome);
      end if;
      VM_Snapshot := CCL.VM.Snapshot (VM_State);
      Update_Active_Debug;
      Update_VM_Inspection;
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
         Update_VM_Inspection;
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
         Update_VM_Inspection;
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

   function Maximum_Source_Columns return Positive is
      Maximum : Positive := 1;
   begin
      for Line in 1 .. CuBit.UI.Editor.Documents.Line_Count (Source) loop
         Maximum := Positive'Max
           (Maximum,
            CuBit.UI.Editor.Documents.Line_Length (Source, Line) + 1);
      end loop;
      return Maximum;
   end Maximum_Source_Columns;

   procedure Reveal_Source_Cursor is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Line, Column : Positive;
      First_Cursor_Line : Positive :=
        CuBit.UI.Editor.Documents.Line_Count (Source);
      Last_Cursor_Line : Positive := 1;
      First_Cursor_Column : Positive := Maximum_Source_Columns;
      Last_Cursor_Column : Positive := 1;
      First_Visible : constant Positive :=
        CuBit.UI.Editor.Viewports.First_Line (Source_View);
      Visible_Lines : constant Positive :=
        CuBit.UI.Editor.Viewports.Line_Capacity (Source_View);
      Last_Visible : constant Positive := Positive'Min
        (CuBit.UI.Editor.Documents.Line_Count (Source),
         First_Visible + Visible_Lines - 1);
      First_Visible_Column : constant Positive :=
        CuBit.UI.Editor.Viewports.First_Column (Source_View);
      Visible_Columns : constant Positive :=
        CuBit.UI.Editor.Viewports.Column_Capacity (Source_View);
      Last_Visible_Column : constant Positive :=
        First_Visible_Column + Visible_Columns - 1;
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, State.Position, Line, Column);
         First_Cursor_Line := Positive'Min (First_Cursor_Line, Line);
         Last_Cursor_Line := Positive'Max (Last_Cursor_Line, Line);
         First_Cursor_Column := Positive'Min (First_Cursor_Column, Column);
         Last_Cursor_Column := Positive'Max (Last_Cursor_Column, Column);
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
      if First_Cursor_Column < First_Visible_Column then
         CuBit.UI.Editor.Viewports.Scroll_Columns
           (Source_View,
            Integer (First_Cursor_Column) - Integer (First_Visible_Column),
            Maximum_Source_Columns);
      elsif Last_Cursor_Column > Last_Visible_Column then
         CuBit.UI.Editor.Viewports.Ensure_Column_Visible
           (Source_View, Last_Cursor_Column, Maximum_Source_Columns);
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

      function Is_Whitespace (Value : Character) return Boolean is
        (Value = ' ' or else Value = ASCII.HT or else
         Value = ASCII.LF or else Value = ASCII.CR);
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
            if Position <= Text'Length and then
              Is_Word_Character (Text (Position))
            then
               while Position <= Text'Length and then
                 Is_Word_Character (Text (Position))
               loop
                  Position := Position + 1;
               end loop;
               while Position <= Text'Length and then
                 Is_Whitespace (Text (Position))
               loop
                  Position := Position + 1;
               end loop;
            elsif Position <= Text'Length and then
              Is_Whitespace (Text (Position))
            then
               while Position <= Text'Length and then
                 Is_Whitespace (Text (Position))
               loop
                  Position := Position + 1;
               end loop;
            elsif Position <= Text'Length then
               Position := Position + 1;
            end if;
         elsif By_Word then
            if Position > 1 and then
              Is_Whitespace (Text (Position - 1))
            then
               while Position > 1 and then
                 Is_Whitespace (Text (Position - 1))
               loop
                  Position := Position - 1;
               end loop;
            end if;
            if Position > 1 and then
              Is_Word_Character (Text (Position - 1))
            then
               while Position > 1 and then
                 Is_Word_Character (Text (Position - 1))
               loop
                  Position := Position - 1;
               end loop;
            elsif Position > 1 then
               Position := Position - 1;
            end if;
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

   procedure Select_Next_Occurrence is
      Text : constant String := CuBit.UI.Editor.Documents.Content (Source);
      State : CuBit.UI.Editor.Cursors.Cursor_State := Source_Cursor;
      Selection_First : CuBit.UI.Editor.Documents.Document_Position :=
        CuBit.UI.Editor.Documents.Document_Position'Min
          (State.Position, State.Anchor);
      Selection_End : CuBit.UI.Editor.Documents.Document_Position :=
        CuBit.UI.Editor.Documents.Document_Position'Max
          (State.Position, State.Anchor);
      Candidate : Natural := 0;
      Line, Column : Positive;
      Add_Result : CuBit.UI.Editor.Cursors.Add_Result;
      Wrapped : Boolean := False;

      function Is_Word_Character (Value : Character) return Boolean is
        ((Value >= 'a' and then Value <= 'z') or else
         (Value >= 'A' and then Value <= 'Z') or else
         (Value >= '0' and then Value <= '9') or else Value = '_');
   begin
      Source_Histories.Break_Sequence (Source_History);
      if State.Position = State.Anchor then
         declare
            Index : Natural := 0;
            First, Last : Positive;
         begin
            if State.Position <= Text'Length and then
              Is_Word_Character (Text (State.Position))
            then
               Index := State.Position;
            elsif State.Position > 1 and then
              Is_Word_Character (Text (State.Position - 1))
            then
               Index := State.Position - 1;
            end if;
            if Index = 0 then
               Set_Result ("Ctrl+D requires a word or selection");
               return;
            end if;
            First := Index;
            Last := Index;
            while First > 1 and then
              Is_Word_Character (Text (First - 1))
            loop
               First := First - 1;
            end loop;
            while Last < Text'Length and then
              Is_Word_Character (Text (Last + 1))
            loop
               Last := Last + 1;
            end loop;
            State.Anchor := First;
            State.Position := Last + 1;
            CuBit.UI.Editor.Documents.Position_To_Line_Column
              (Source, State.Position, Line, Column);
            State.Preferred_Column := Column;
            CuBit.UI.Editor.Cursors.Set_Element
              (Source_Cursors,
               CuBit.UI.Editor.Cursors.Primary_Index (Source_Cursors), State);
            Reveal_Source_Cursor;
            Set_Result ("selected word; Ctrl+D finds the next occurrence");
            return;
         end;
      end if;

      Selection_First := CuBit.UI.Editor.Documents.Document_Position'Min
        (State.Position, State.Anchor);
      Selection_End := CuBit.UI.Editor.Documents.Document_Position'Max
        (State.Position, State.Anchor);
      declare
         Pattern_Length : constant Positive := Selection_End - Selection_First;
         Pattern : constant String :=
           Text (Selection_First .. Selection_End - 1);
         Last_Candidate : constant Positive :=
           Text'Length - Pattern_Length + 1;
         Whole_Word : Boolean := True;

         function Occurrence_Available (First : Positive) return Boolean is
            Last : constant Positive := First + Pattern_Length;
            Existing : CuBit.UI.Editor.Cursors.Cursor_State;
            Existing_First, Existing_Last : Positive;
         begin
            for Index in 1 ..
              CuBit.UI.Editor.Cursors.Length (Source_Cursors)
            loop
               Existing :=
                 CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
               Existing_First := Positive'Min
                 (Existing.Position, Existing.Anchor);
               Existing_Last := Positive'Max
                 (Existing.Position, Existing.Anchor);
               if (Existing_First = Existing_Last and then
                   Existing_First >= First and then Existing_First <= Last) or
                 else
                 (Existing_First < Existing_Last and then
                  First < Existing_Last and then Existing_First < Last)
               then
                  return False;
               end if;
            end loop;
            return True;
         end Occurrence_Available;

         procedure Find_In_Range (First, Last : Positive) is
            Next : Positive := First;
            Search : CuBit.UI.Editor.Search.Search_Result;
         begin
            while Next <= Last loop
               CuBit.UI.Editor.Search.Find_Next
                 (Text, Pattern, Start_At => Next, Wrap => False,
                  Whole_Word => Whole_Word, Case_Sensitive => True,
                  Result => Search);
               exit when Search.Status /=
                 CuBit.UI.Editor.Search.Match_Found or else
                 Search.First > Last;
               if Occurrence_Available (Search.First) then
                  Candidate := Search.First;
                  return;
               elsif Search.First = Text'Length then
                  return;
               else
                  Next := Search.First + 1;
               end if;
            end loop;
         end Find_In_Range;
      begin
         for Position in Selection_First .. Selection_End - 1 loop
            if not Is_Word_Character (Text (Position)) then
               Whole_Word := False;
               exit;
            end if;
         end loop;
         if Whole_Word then
            Whole_Word :=
              (Selection_First = 1 or else
               not Is_Word_Character (Text (Selection_First - 1))) and then
              (Selection_End > Text'Length or else
               not Is_Word_Character (Text (Selection_End)));
         end if;

         if Selection_End <= Last_Candidate then
            Find_In_Range (Selection_End, Last_Candidate);
         end if;
         if Candidate = 0 and then Selection_End > 1 then
            Find_In_Range
              (1, Positive'Min (Last_Candidate, Selection_End - 1));
            Wrapped := Candidate > 0;
         end if;
         if Candidate = 0 then
            Set_Result ("no unselected occurrence");
            return;
         end if;

         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, Candidate + Pattern_Length, Line, Column);
         CuBit.UI.Editor.Cursors.Add_Selection
           (Source_Cursors,
            Anchor => Candidate,
            Position => Candidate + Pattern_Length,
            Preferred_Column => Column,
            Result => Add_Result);
         if Add_Result = CuBit.UI.Editor.Cursors.Cursor_Limit_Reached then
            Set_Result ("cursor limit reached");
         elsif Add_Result = CuBit.UI.Editor.Cursors.Cursor_Added then
            Reveal_Source_Cursor;
            Set_Result
              ((if Wrapped then "wrapped; " else "") &
               "selected next occurrence");
         else
            Set_Result ("occurrence already selected");
         end if;
      end;
   end Select_Next_Occurrence;

   procedure Open_Find is
      State : constant CuBit.UI.Editor.Cursors.Cursor_State := Source_Cursor;
      First : constant Positive := Positive'Min
        (State.Position, State.Anchor);
      Last : constant Positive := Positive'Max
        (State.Position, State.Anchor);
      Accepted : Boolean;
   begin
      if Last > First and then
        Last - First <= CuBit.UI.Editor.MAX_TEXT_LENGTH
      then
         CuBit.UI.Editor.Initialize
           (Find_Query,
            CuBit.UI.Editor.Documents.Content (Source) (First .. Last - 1),
            Accepted);
      else
         CuBit.UI.Editor.Initialize (Find_Query, "", Accepted);
      end if;
      Find_Active := Accepted;
      Set_Result ("find: Enter next, Escape close");
   end Open_Find;

   procedure Find_Next_Query is
      Text : constant String := CuBit.UI.Editor.Documents.Content (Source);
      Pattern : constant String := CuBit.UI.Editor.Content (Find_Query);
      Start_At : constant Positive := Positive'Max
        (Source_Cursor.Position, Source_Cursor.Anchor);
      Search : CuBit.UI.Editor.Search.Search_Result;
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Line, Column : Positive;
   begin
      CuBit.UI.Editor.Search.Find_Next
        (Text, Pattern, Start_At => Start_At, Wrap => True,
         Whole_Word => False, Case_Sensitive => True, Result => Search);
      case Search.Status is
         when CuBit.UI.Editor.Search.Match_Found =>
            CuBit.UI.Editor.Cursors.Initialize
              (Source_Cursors, Search.Last);
            State := Source_Cursor;
            State.Anchor := Search.First;
            State.Position := Search.Last;
            CuBit.UI.Editor.Documents.Position_To_Line_Column
              (Source, State.Position, Line, Column);
            State.Preferred_Column := Column;
            Store_Source_Cursor (State);
            Reveal_Source_Cursor;
            Set_Result
              ((if Search.First < Start_At then "find wrapped; " else "") &
               "match selected");
         when CuBit.UI.Editor.Search.No_Match =>
            Set_Result ("no match");
         when CuBit.UI.Editor.Search.Empty_Pattern =>
            Set_Result ("enter text to find");
         when CuBit.UI.Editor.Search.Pattern_Too_Long =>
            Set_Result ("search text exceeds bounded search limit");
      end case;
   end Find_Next_Query;

   procedure Find_Matching_Paren
     (Text : String;
      Cursor : CuBit.UI.Editor.Documents.Document_Position;
      Found : out Boolean;
      Candidate, Match : out Positive)
   is
      type Paren_Stack is array (Positive range 1 .. SOURCE_CAPACITY)
        of Positive;
      Stack : Paren_Stack := [others => 1];
      Depth : Natural range 0 .. SOURCE_CAPACITY := 0;
      Target : Natural := 0;
      Offset : Natural := 0;
      Position, Open_Position : Positive;

      function Is_Paren (Value : Character) return Boolean is
        (Value = '(' or else Value = ')');
   begin
      Found := False;
      Candidate := 1;
      Match := 1;
      if Cursor <= Text'Length and then
        Is_Paren (Text (Text'First + Cursor - 1))
      then
         Target := Cursor;
      elsif Cursor > 1 and then Cursor - 1 <= Text'Length and then
        Is_Paren (Text (Text'First + Cursor - 2))
      then
         Target := Cursor - 1;
      end if;
      if Target = 0 then
         return;
      end if;

      while Offset < Text'Length loop
         if Text (Text'First + Offset) = '#' then
            while Offset < Text'Length and then
              Text (Text'First + Offset) /= ASCII.LF and then
              Text (Text'First + Offset) /= ASCII.CR
            loop
               Offset := Offset + 1;
            end loop;
         elsif Text (Text'First + Offset) = '(' then
            Depth := Depth + 1;
            Stack (Depth) := Offset + 1;
            Offset := Offset + 1;
         elsif Text (Text'First + Offset) = ')' then
            Position := Offset + 1;
            if Depth > 0 then
               Open_Position := Stack (Depth);
               if Target = Open_Position or else Target = Position then
                  Candidate := Target;
                  Match :=
                    (if Target = Open_Position then Position
                     else Open_Position);
                  Found := True;
                  return;
               end if;
               Depth := Depth - 1;
            end if;
            Offset := Offset + 1;
         else
            Offset := Offset + 1;
         end if;
      end loop;
   end Find_Matching_Paren;

   --  This bounded scanner controls presentation only.  Parsing, diagnostics,
   --  types, and execution always use CCL.Language's independent parser.
   procedure Build_Source_Styles (Text : String) is
      Offset : Natural := 0;

      COMMENT_COLOR : constant CuBit.UI.Color := 16#55705A#;
      FORM_COLOR : constant CuBit.UI.Color := 16#704A7C#;
      LITERAL_COLOR : constant CuBit.UI.Color := 16#8B4A19#;
      BOOLEAN_COLOR : constant CuBit.UI.Color := 16#245F88#;
      DELIMITER_COLOR : constant CuBit.UI.Color := 16#42515A#;
      MATCH_COLOR : constant CuBit.UI.Color := 16#C8AD63#;

      function Is_Name_Character (Item : Character) return Boolean is
        ((Item >= 'a' and then Item <= 'z') or else
         (Item >= 'A' and then Item <= 'Z') or else
         (Item >= '0' and then Item <= '9') or else
         Item = '-' or else Item = '_' or else Item = '.' or else
         Item = '?' or else Item = '+' or else Item = '=');

      procedure Add_Style
        (First_Offset, End_Offset : Natural; Foreground : CuBit.UI.Color)
      is
      begin
         if End_Offset > First_Offset and then
           Source_Style_Count < MAX_SOURCE_STYLE_SPANS
         then
            Source_Style_Count := Source_Style_Count + 1;
            Source_Styles (Source_Style_Count) :=
              (firstPosition => First_Offset + 1,
               lastPosition => End_Offset,
               foreground => Foreground,
               decoration => CuBit.UI.No_Text_Decoration,
               decorationColor => 0);
         end if;
      end Add_Style;
   begin
      Source_Style_Count := 0;
      while Offset < Text'Length loop
         declare
            Start : constant Natural := Offset;
            Item : constant Character := Text (Text'First + Offset);
         begin
            if Item = '#' then
               while Offset < Text'Length and then
                 Text (Text'First + Offset) /= ASCII.LF and then
                 Text (Text'First + Offset) /= ASCII.CR
               loop
                  Offset := Offset + 1;
               end loop;
               Add_Style (Start, Offset, COMMENT_COLOR);
            elsif Item = '(' or else Item = ')' then
               Offset := Offset + 1;
               Add_Style (Start, Offset, DELIMITER_COLOR);
            elsif Item >= '0' and then Item <= '9' then
               while Offset < Text'Length and then
                 Text (Text'First + Offset) >= '0' and then
                 Text (Text'First + Offset) <= '9'
               loop
                  Offset := Offset + 1;
               end loop;
               Add_Style (Start, Offset, LITERAL_COLOR);
            elsif Item = '-' and then Offset + 1 < Text'Length and then
              Text (Text'First + Offset + 1) >= '0' and then
              Text (Text'First + Offset + 1) <= '9'
            then
               Offset := Offset + 1;
               while Offset < Text'Length and then
                 Text (Text'First + Offset) >= '0' and then
                 Text (Text'First + Offset) <= '9'
               loop
                  Offset := Offset + 1;
               end loop;
               Add_Style (Start, Offset, LITERAL_COLOR);
            elsif Is_Name_Character (Item) then
               while Offset < Text'Length and then
                 Is_Name_Character (Text (Text'First + Offset))
               loop
                  Offset := Offset + 1;
               end loop;
               declare
                  Name : constant String :=
                    Text (Text'First + Start .. Text'First + Offset - 1);
               begin
                  if Name = "true" or else Name = "false" then
                     Add_Style (Start, Offset, BOOLEAN_COLOR);
                  elsif Name = "let" or else Name = "if" or else
                    Name = "not" or else Name = "+" or else Name = "="
                  then
                     Add_Style (Start, Offset, FORM_COLOR);
                  elsif (for some Character_Of_Name of Name =>
                           Character_Of_Name = '.')
                  then
                     Add_Style (Start, Offset, Colors.accent);
                  end if;
               end;
            elsif Item = ' ' or else Item = ASCII.HT or else
              Item = ASCII.LF or else Item = ASCII.CR
            then
               Offset := Offset + 1;
            else
               Offset := Offset + 1;
               Add_Style (Start, Offset, Colors.danger);
            end if;
         end;
      end loop;
      declare
         Found : Boolean;
         Candidate, Match : Positive;
      begin
         Find_Matching_Paren
           (Text, Source_Cursor.Position, Found, Candidate, Match);
         if Found then
            for Index in 1 .. Source_Style_Count loop
               if Source_Styles (Index).firstPosition = Candidate or else
                 Source_Styles (Index).firstPosition = Match
               then
                  Source_Styles (Index).decoration := CuBit.UI.Text_Underline;
                  Source_Styles (Index).decorationColor := MATCH_COLOR;
               end if;
            end loop;
         end if;
      end;
   end Build_Source_Styles;

   procedure Jump_To_Matching_Paren (Extend_Selection : Boolean) is
      Text : constant String := CuBit.UI.Editor.Documents.Content (Source);
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Candidate, Match : Positive;
      Found, Any_Found : Boolean := False;
      Target : CuBit.UI.Editor.Documents.Document_Position;
      Line, Column : Positive;
   begin
      Source_Histories.Break_Sequence (Source_History);
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         Find_Matching_Paren
           (Text, State.Position, Found, Candidate, Match);
         if Found then
            Target :=
              (if Extend_Selection and then Match > Candidate then Match + 1
               else Match);
            State.Position := Target;
            if not Extend_Selection then
               State.Anchor := Target;
            end if;
            CuBit.UI.Editor.Documents.Position_To_Line_Column
              (Source, Target, Line, Column);
            State.Preferred_Column := Column;
            CuBit.UI.Editor.Cursors.Set_Element
              (Source_Cursors, Index, State);
            Any_Found := True;
         end if;
      end loop;
      CuBit.UI.Editor.Cursors.Coalesce (Source_Cursors);
      if Any_Found then
         Reveal_Source_Cursor;
      else
         Set_Result ("no matching parenthesis");
      end if;
   end Jump_To_Matching_Paren;

   function Source_Position_At (Pixel_X, Pixel_Y : Natural)
     return CuBit.UI.Editor.Documents.Document_Position
   is
      Line_Height : constant Natural := CuBit.UI.Code_Text_Height + 2;
      Relative_Line : constant Natural :=
        (if Pixel_Y <= Source_Bounds.y + 5 then 0
         else (Pixel_Y - Source_Bounds.y - 5) / Line_Height);
      Line : Positive := CuBit.UI.Editor.Viewports.First_Line (Source_View);
      Column : Positive;
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
      Column := Positive'Min
        (CuBit.UI.Editor.Viewports.First_Column (Source_View),
         CuBit.UI.Editor.Documents.Line_Length (Source, Line) + 1);
      for Candidate in Column ..
        CuBit.UI.Editor.Documents.Line_Length (Source, Line)
      loop
         Position := CuBit.UI.Editor.Documents.Line_Column_To_Position
           (Source, Line, Candidate);
         Width := CuBit.UI.Code_Text_Width (Text (Position .. Position));
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

   procedure Source_Horizontal_Scrollbar_Metrics
     (Track, Thumb : out CuBit.UI.Rect; Maximum_First : out Positive)
   is
      Columns : constant Positive := Maximum_Source_Columns;
      Page : constant Positive :=
        CuBit.UI.Editor.Viewports.Column_Capacity (Source_View);
      Extent : constant Natural := Natural'Min
        (Source_Horizontal_Scrollbar.h,
         Source_Horizontal_Scrollbar.w / 2);
      Track_Frame : constant CuBit.UI.Rect :=
        (x => Source_Horizontal_Scrollbar.x + Extent,
         y => Source_Horizontal_Scrollbar.y,
         w => (if Source_Horizontal_Scrollbar.w > Extent * 2 then
                  Source_Horizontal_Scrollbar.w - Extent * 2 else 0),
         h => Source_Horizontal_Scrollbar.h);
      Total : constant Natural := Columns;
      Shown : constant Natural := Natural'Min (Page, Total);
      Thumb_Width : Natural;
      Travel : Natural;
      Position : Natural;
   begin
      Track :=
        (x => Track_Frame.x + 2, y => Track_Frame.y + 2,
         w => (if Track_Frame.w > 4 then Track_Frame.w - 4 else 0),
         h => (if Track_Frame.h > 4 then Track_Frame.h - 4 else 0));
      Maximum_First :=
        (if Shown >= Total then 1 else Columns - Shown + 1);
      Thumb_Width := Natural'Min
        (Track.w, Natural'Max (12, Track.w * Shown / Total));
      Travel := Track.w - Thumb_Width;
      Position := CuBit.UI.Editor.Viewports.First_Column (Source_View) - 1;
      Thumb :=
        (x => Track.x +
           (if Maximum_First = 1 then 0
            else Position * Travel / (Maximum_First - 1)),
         y => Track.y, w => Thumb_Width, h => Track.h);
   end Source_Horizontal_Scrollbar_Metrics;

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
        (x => Canvas.width - 60, y => 3, w => 18, h => 17);
      Maximize : constant CuBit.UI.Rect :=
        (x => Canvas.width - 40, y => 3, w => 18, h => 17);
      Close : constant CuBit.UI.Rect :=
        (x => Canvas.width - 20, y => 3, w => 18, h => 17);
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

   function Value_Text (Item : CCL.VM.Value) return String is
   begin
      case Item.Kind is
         when CCL.VM.Integer_Value =>
            return "int" & Integer_64'Image (Item.Integer);
         when CCL.VM.Boolean_Value =>
            return "bool " & (if Item.Boolean then "true" else "false");
      end case;
   end Value_Text;

   function Natural_Text (Value : Natural) return String is
      Image : constant String := Natural'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Natural_Text;

   function Local_Name_Text
     (Local : CCL.Ownership.Binding_Id) return String
   is
      Identifier : constant CCL.Language.Name :=
        CCL.Debug_Maps.Local_Name (Compiled_Artifact.Debug, Local);
   begin
      if CCL.Debug_Maps.Has_Local_Name (Compiled_Artifact.Debug, Local) and then
        Identifier.Length > 0
      then
         return Identifier.Data (1 .. Identifier.Length);
      else
         return "L" & Natural_Text (Natural (Local));
      end if;
   end Local_Name_Text;

   function Ownership_Mode_Text
     (Mode : CCL.Ownership.Ownership_Mode) return String
   is
   begin
      case Mode is
         when CCL.Ownership.Unrestricted =>
            return "unrestricted";
         when CCL.Ownership.Move_Only =>
            return "move-only";
         when CCL.Ownership.Must_Handle =>
            return "must-handle";
      end case;
   end Ownership_Mode_Text;

   procedure Render is
      Execution_Content : CuBit.UI.Rect;
      Editor_Content : CuBit.UI.Rect;
      Workspace : constant CuBit.UI.Rect :=
        (x => WORKSPACE_MARGIN, y => WORKSPACE_TOP,
         w => (if Canvas.width > WORKSPACE_MARGIN * 2 then
                  Canvas.width - WORKSPACE_MARGIN * 2 else 0),
         h => (if Canvas.height > WORKSPACE_TOP + WORKSPACE_BOTTOM then
                  Canvas.height - WORKSPACE_TOP - WORKSPACE_BOTTOM else 0));
      Inspector_Bounds : CuBit.UI.Rect;
      Source_Pane_Bounds : CuBit.UI.Rect;
      Disassembly_Bounds : CuBit.UI.Rect;
      Cursor_State : CuBit.UI.Editor.Cursors.Cursor_State;
      Cursor_Visuals : CuBit.UI.Text_Cursor_States
        (1 .. CuBit.UI.Editor.Cursors.MAX_CURSORS) :=
          [others => (cursor => 1, selectionStart => 1, selectionEnd => 1)];
      Cursor_Count : constant Positive :=
        CuBit.UI.Editor.Cursors.Length (Source_Cursors);
      Visual_Count : Positive := Cursor_Count;
   begin
      Build_Source_Styles (CuBit.UI.Editor.Documents.Content (Source));
      Inspector_Width := Natural'Max
        (MINIMUM_INSPECTOR_WIDTH,
         Natural'Min
           (Inspector_Width,
            Workspace.w - MINIMUM_SOURCE_WIDTH -
              MINIMUM_DISASSEMBLY_WIDTH - SPLITTER_WIDTH * 2));
      Disassembly_Width := Natural'Max
        (MINIMUM_DISASSEMBLY_WIDTH,
         Natural'Min
           (Disassembly_Width,
            Workspace.w - Inspector_Width - MINIMUM_SOURCE_WIDTH -
              SPLITTER_WIDTH * 2));
      declare
         Frame : CuBit.UI.Layout.Dock_Frame :=
           CuBit.UI.Layout.Begin_Dock (Workspace);
      begin
         Inspector_Bounds :=
           CuBit.UI.Layout.Dock_Left (Frame, Inspector_Width);
         Inspector_Splitter :=
           CuBit.UI.Layout.Dock_Left (Frame, SPLITTER_WIDTH);
         Disassembly_Bounds :=
           CuBit.UI.Layout.Dock_Right (Frame, Disassembly_Width);
         Disassembly_Splitter :=
           CuBit.UI.Layout.Dock_Right (Frame, SPLITTER_WIDTH);
         Source_Pane_Bounds := CuBit.UI.Layout.Fill (Frame);
      end;

      CuBit.UI.Fill_Rect
        (Canvas, (x => 0, y => 0, w => Canvas.width, h => Canvas.height),
         Colors.desktop);

      if CLIENT_TITLE_HEIGHT > 0 then
         CuBit.UI.Fill_Vertical_Gradient
           (Canvas, (x => 0, y => 0, w => Canvas.width, h => 22),
            Colors.activeTitleTop, Colors.activeTitleBottom);
         CuBit.UI.Draw_UI_Text_Transparent
           (Canvas, 6, 4, "CuBit CCL Workbench", Colors.selectionText);
         Draw_Title_Controls;
      end if;

      CuBit.UI.Draw_Menu_Bar
        (Canvas, (x => 0, y => CLIENT_TITLE_HEIGHT,
                  w => Canvas.width, h => 22), Colors);
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 3, y => CLIENT_TITLE_HEIGHT, w => 34, h => 21), Colors,
         False, False, "File");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 38, y => CLIENT_TITLE_HEIGHT, w => 34, h => 21), Colors,
         False, False, "Edit");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 73, y => CLIENT_TITLE_HEIGHT, w => 38, h => 21), Colors,
         False, False, "View");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 112, y => CLIENT_TITLE_HEIGHT, w => 34, h => 21), Colors,
         False, False, "Run");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 147, y => CLIENT_TITLE_HEIGHT, w => 38, h => 21), Colors,
         False, False, "Help");

      CuBit.UI.Widgets.Toolbar
        (Canvas, (x => 0, y => CLIENT_TITLE_HEIGHT + 22,
                  w => Canvas.width, h => 34), Colors);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Open_Button_Bounds, Colors,
         CuBit.UI.Widgets.Open_Document, enabled => False);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Save_Button_Bounds, Colors,
         CuBit.UI.Widgets.Save_Document, enabled => False);
      CuBit.UI.Widgets.Toolbar_Separator
        (Canvas, (x => 63, y => CLIENT_TITLE_HEIGHT + 25,
                  w => 8, h => 27), Colors);
      CuBit.UI.Widgets.Toolbar_Button
        (Canvas, Compile_Button_Bounds, Colors,
         CuBit.UI.Widgets.Compile_Program,
         pressed => Compile_Button_Pressed);
      Run_Button_Bounds :=
        (x => 73, y => CLIENT_TITLE_HEIGHT + 25, w => 27, h => 27);
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
        (Canvas, (x => 218, y => CLIENT_TITLE_HEIGHT + 25,
                  w => 8, h => 27), Colors);
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
        (Canvas, Inspector_Bounds, Colors,
         "Execution", Execution_Content, 8);
      declare
         Execution_Canvas : constant CuBit.UI.Canvas :=
           CuBit.UI.With_Clip (Canvas, Execution_Content);
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
            elsif VM_Has_State then
               "PC" & Natural'Image (Natural (VM_Snapshot.Instruction))
            else "n/a");
         INFO_LINE_STEP : constant Natural := CuBit.UI.UI_Text_Height + 2;
         STATUS_TOP_OFFSET : constant Natural := 0;
         RESULT_TOP_OFFSET : constant Natural :=
           STATUS_TOP_OFFSET + INFO_LINE_STEP;
         FUEL_TOP_OFFSET : constant Natural :=
           RESULT_TOP_OFFSET + INFO_LINE_STEP;
         DIVIDER_TOP_OFFSET : constant Natural :=
           FUEL_TOP_OFFSET + CuBit.UI.UI_Text_Height + 3;
         STACK_HEADING_TOP_OFFSET : constant Natural :=
           DIVIDER_TOP_OFFSET + 7;
         STACK_FIRST_TOP_OFFSET : constant Natural :=
           STACK_HEADING_TOP_OFFSET + CuBit.UI.UI_Text_Height + 2;
         STACK_LINE_STEP : constant Natural := CuBit.UI.UI_Text_Height;
         MAX_VISIBLE_STACK_VALUES : constant Natural := 2;
         LOCALS_LABEL_TOP_OFFSET : constant Natural := 140;
         LOCALS_ROW_HEIGHT : constant Natural := 36;
         LOCALS_TOP_OFFSET : constant Natural := 158;
         CATALOG_TEXT_HEIGHT : constant Natural := 18;
         CATALOG_TABLE_GAP : constant Natural := 6;
         Catalog_Y : constant Natural :=
           Execution_Content.y + Execution_Content.h - CATALOG_TEXT_HEIGHT;
         Locals_Table : constant CuBit.UI.Rect :=
           (x => Execution_Content.x,
            y => Execution_Content.y + LOCALS_TOP_OFFSET,
            w => Execution_Content.w,
            h => (if Execution_Content.h >
                     LOCALS_TOP_OFFSET + CATALOG_TEXT_HEIGHT +
                       CATALOG_TABLE_GAP
                  then Execution_Content.h - LOCALS_TOP_OFFSET -
                    CATALOG_TEXT_HEIGHT - CATALOG_TABLE_GAP
                  else 0));
         Locals_Regions : constant CuBit.UI.Table_Regions :=
           CuBit.UI.Layout_Table (Locals_Table);
         Locals_Columns : constant CuBit.UI.Table_Column_Layout :=
           (First_Width => 54, Second_Width => 44, Cell_Padding => 2);
      begin
         CuBit.UI.Draw_UI_Text
           (Execution_Canvas, Execution_Content.x,
            Execution_Content.y + STATUS_TOP_OFFSET,
            "Status: " & Status_Text, Colors.text, Colors.face);
         CuBit.UI.Draw_UI_Text
           (Execution_Canvas, Execution_Content.x,
            Execution_Content.y + RESULT_TOP_OFFSET,
            "Result: " & Result_Text (1 .. Result_Last),
            Colors.text, Colors.face);
         CuBit.UI.Draw_UI_Text
           (Execution_Canvas, Execution_Content.x,
            Execution_Content.y + FUEL_TOP_OFFSET,
            "Fuel: " & Fuel_Text & "   " & Location_Text,
            Colors.text, Colors.face);
         CuBit.UI.Fill_Rect
           (Execution_Canvas,
            (x => Execution_Content.x,
             y => Execution_Content.y + DIVIDER_TOP_OFFSET,
             w => Execution_Content.w, h => 1),
            Colors.shadow);
         CuBit.UI.Draw_UI_Text
           (Execution_Canvas, Execution_Content.x,
            Execution_Content.y + STACK_HEADING_TOP_OFFSET,
            "Operand stack (top first)", Colors.text, Colors.face);

         if Has_VM_Inspection and then VM_Inspection.Stack_Length > 0 then
            declare
               Visible : constant Natural := Natural'Min
                 (Natural (VM_Inspection.Stack_Length),
                  MAX_VISIBLE_STACK_VALUES);
            begin
               for Position in 0 .. Visible - 1 loop
                  CuBit.UI.Draw_UI_Text
                    (Execution_Canvas, Execution_Content.x + 5,
                     Execution_Content.y + STACK_FIRST_TOP_OFFSET +
                       Position * STACK_LINE_STEP,
                     Natural'Image (Position) & ": " &
                       Value_Text
                         (VM_Inspection.Stack
                            (CCL.VM.Stack_Index (Position))),
                     Colors.text, Colors.face);
               end loop;
               if Natural (VM_Inspection.Stack_Length) > Visible then
                  CuBit.UI.Draw_UI_Text
                    (Execution_Canvas, Execution_Content.x + 5,
                     Execution_Content.y + STACK_FIRST_TOP_OFFSET +
                       Visible * STACK_LINE_STEP,
                     "+" & Natural'Image
                       (Natural (VM_Inspection.Stack_Length) - Visible) &
                       " more",
                     Colors.muted, Colors.face);
               end if;
            end;
         else
            CuBit.UI.Draw_UI_Text
              (Execution_Canvas, Execution_Content.x + 5,
               Execution_Content.y + STACK_FIRST_TOP_OFFSET,
               "(empty)", Colors.muted, Colors.face);
         end if;

         CuBit.UI.Draw_UI_Text
           (Execution_Canvas, Execution_Content.x,
            Execution_Content.y + LOCALS_LABEL_TOP_OFFSET,
            "Locals", Colors.text, Colors.face);
         CuBit.UI.Draw_Table_Viewport
           (Execution_Canvas, Locals_Table, Colors);
         CuBit.UI.Draw_Table_Header
           (Execution_Canvas, Locals_Regions.Header,
            Colors, "Local", "Value", "Ownership", Locals_Columns);
         if Has_VM_Inspection and then VM_Inspection.Locals_Length > 0 and then
           Locals_Regions.Rows.h > 0
         then
            declare
               Visible : constant Natural := Natural'Min
                 (Natural (VM_Inspection.Locals_Length),
                  Locals_Regions.Rows.h / LOCALS_ROW_HEIGHT);
               Local : CCL.Ownership.Binding_Id;
            begin
               if Visible > 0 then
                  for Position in 0 .. Visible - 1 loop
                     Local := CCL.Ownership.Binding_Id (Position);
                     CuBit.UI.Draw_Table_Row
                       (Execution_Canvas,
                        (x => Locals_Regions.Rows.x,
                         y => Locals_Regions.Rows.y +
                           Position * LOCALS_ROW_HEIGHT,
                         w => Locals_Regions.Rows.w, h => LOCALS_ROW_HEIGHT),
                        Colors, selected => False, hot => False,
                        c1 => Local_Name_Text (Local),
                        c2 => Value_Text
                          (VM_Inspection.Locals (Local).Value),
                        c3 => CCL.Ownership.Binding_State'Image
                          (VM_Inspection.Locals (Local).Ownership_State),
                        layout => Locals_Columns,
                        textStyle => CuBit.UI.Table_Code_Text,
                        detail3 => Ownership_Mode_Text
                          (VM_Inspection.Locals (Local).Mode));
                  end loop;
               else
                  CuBit.UI.Draw_UI_Text
                    (Execution_Canvas, Locals_Regions.Rows.x + 6,
                     Locals_Regions.Rows.y + 3,
                     "resize to inspect locals", Colors.muted, Colors.field);
               end if;
            end;
         else
            CuBit.UI.Draw_UI_Text
              (Execution_Canvas, Locals_Regions.Rows.x + 6,
               Locals_Regions.Rows.y + 3,
               "(none)", Colors.muted, Colors.field);
         end if;

         CuBit.UI.Fill_Rect
           (Execution_Canvas,
            (x => Execution_Content.x,
             y => (if Catalog_Y > 7 then Catalog_Y - 7 else Catalog_Y),
             w => Execution_Content.w, h => 1),
            Colors.shadow);
         CuBit.UI.Draw_UI_Text
           (Execution_Canvas, Execution_Content.x,
            Catalog_Y,
            "Catalog:" & Natural'Image
              (Natural (CCL.Catalog.Length (Visible_Interfaces))) &
              " visible /" & Natural'Image
              (Natural (CCL.Catalog.Length (Granted_Interfaces))) &
              " granted",
            Colors.text, Colors.face);
      end;

      CuBit.UI.Widgets.Group_Box
        (Canvas, Source_Pane_Bounds, Colors,
         "Source", Editor_Content, 8);
      CuBit.UI.Draw_UI_Text
        (Canvas, Editor_Content.x, Editor_Content.y,
         "untitled.ccl  |  F5 interpret  |  Compile for VM",
         Colors.muted, Colors.face);
      if Find_Active then
         CuBit.UI.Draw_UI_Text
           (Canvas, Editor_Content.x, Editor_Content.y + 27,
            "Find:", Colors.text, Colors.face);
         CuBit.UI.Draw_Text_Edit_Field
           (Canvas,
            (x => Editor_Content.x + 38, y => Editor_Content.y + 21,
             w => (if Editor_Content.w > 44 then
                      Editor_Content.w - 44 else 1),
             h => 22),
            Colors, CuBit.UI.Editor.Content (Find_Query),
            cursor => CuBit.UI.Editor.Cursor (Find_Query) - 1,
            selectionStart =>
              CuBit.UI.Editor.Selection_First (Find_Query) - 1,
            selectionEnd => CuBit.UI.Editor.Selection_Last (Find_Query) - 1,
            focused => True, hot => False);
      end if;
      Source_Bounds :=
        (x => Editor_Content.x,
         y => Editor_Content.y + (if Find_Active then 48 else 24),
         w => Editor_Content.w - 18,
         h =>
           (if Editor_Content.h > (if Find_Active then 69 else 45) then
               Editor_Content.h - (if Find_Active then 69 else 45)
            else 1));
      declare
         Line_Height : constant Positive := CuBit.UI.Code_Text_Height + 2;
         Column_Width : constant Positive :=
           Positive'Max (1, CuBit.UI.Code_Text_Width ("M"));
         Usable_Height : constant Natural :=
           (if Source_Bounds.h > 6 then Source_Bounds.h - 6 else 1);
         Usable_Width : constant Natural :=
           (if Source_Bounds.w > 12 then Source_Bounds.w - 12 else 1);
         Visible_Lines : constant Positive :=
           Positive'Max (1, Usable_Height / Line_Height);
         Visible_Columns : constant Positive :=
           Positive'Max (1, Usable_Width / Column_Width);
      begin
         CuBit.UI.Editor.Viewports.Set_Line_Capacity
           (Source_View, Visible_Lines,
            CuBit.UI.Editor.Documents.Line_Count (Source));
         CuBit.UI.Editor.Viewports.Set_Column_Capacity
           (Source_View, Visible_Columns, Maximum_Source_Columns);
      end;
      Source_Scrollbar :=
        (x => Source_Bounds.x + Source_Bounds.w + 2,
         y => Source_Bounds.y, w => 16, h => Source_Bounds.h);
      Source_Horizontal_Scrollbar :=
        (x => Source_Bounds.x,
         y => Source_Bounds.y + Source_Bounds.h + 2,
         w => Source_Bounds.w, h => 16);
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
      if Source_Style_Count > 0 then
         CuBit.UI.Draw_Multiline_Text_Edit_Multiple_Styled
           (CuBit.UI.With_Clip (Canvas, Source_Bounds), Source_Bounds, Colors,
            CuBit.UI.Editor.Documents.Content (Source),
            CuBit.UI.Editor.Viewports.First_Line (Source_View),
            CuBit.UI.Editor.Viewports.Line_Capacity (Source_View),
            Cursor_Visuals (1 .. Visual_Count),
            Source_Styles (1 .. Source_Style_Count),
            focused => not Find_Active, hot => False,
            firstColumn =>
              CuBit.UI.Editor.Viewports.First_Column (Source_View));
      else
         CuBit.UI.Draw_Multiline_Text_Edit_Multiple
           (CuBit.UI.With_Clip (Canvas, Source_Bounds), Source_Bounds, Colors,
            CuBit.UI.Editor.Documents.Content (Source),
            CuBit.UI.Editor.Viewports.First_Line (Source_View),
            CuBit.UI.Editor.Viewports.Line_Capacity (Source_View),
            Cursor_Visuals (1 .. Visual_Count),
            focused => not Find_Active, hot => False,
            firstColumn =>
              CuBit.UI.Editor.Viewports.First_Column (Source_View));
      end if;
      CuBit.UI.Draw_Vertical_Scrollbar
        (Canvas, Source_Scrollbar, Colors, 1,
         CuBit.UI.Editor.Documents.Line_Count (Source),
         CuBit.UI.Editor.Viewports.First_Line (Source_View),
         hot => False,
         active => Source_Scrollbar_Pressed /= CuBit.UI.Scrollbar_None,
         pageSize => CuBit.UI.Editor.Viewports.Line_Capacity (Source_View),
         pressedPart => Source_Scrollbar_Pressed);
      CuBit.UI.Draw_Horizontal_Scrollbar
        (Canvas, Source_Horizontal_Scrollbar, Colors, 1,
         Maximum_Source_Columns,
         CuBit.UI.Editor.Viewports.First_Column (Source_View),
         hot => False,
         active => Source_Horizontal_Scrollbar_Pressed /=
           CuBit.UI.Scrollbar_None,
         pageSize => CuBit.UI.Editor.Viewports.Column_Capacity (Source_View),
         pressedPart => Source_Horizontal_Scrollbar_Pressed);
      CuBit.UI.Widgets.Group_Box
        (Canvas, Disassembly_Bounds, Colors,
         "Disassembly", Bytecode_Content, 8);
      CuBit.UI.Draw_Table_Viewport
        (Canvas,
         (x => Bytecode_Content.x, y => Bytecode_Content.y,
          w => Bytecode_Content.w,
          h => (if Bytecode_Content.h > 22 then
                   Bytecode_Content.h - 22 else Bytecode_Content.h)),
         Colors);
      Bytecode_Table := CuBit.UI.Layout_Table
        ((x => Bytecode_Content.x, y => Bytecode_Content.y,
          w => Bytecode_Content.w,
          h => (if Bytecode_Content.h > 22 then
                   Bytecode_Content.h - 22 else Bytecode_Content.h)));
      Bytecode_Columns.First_Width := Natural'Max
        (MINIMUM_TABLE_COLUMN_WIDTH,
         Natural'Min
           (Bytecode_Columns.First_Width,
            Bytecode_Table.Header.w - MINIMUM_TABLE_COLUMN_WIDTH -
              MINIMUM_INSTRUCTION_COLUMN_WIDTH));
      Bytecode_Columns.Second_Width := Natural'Max
        (MINIMUM_TABLE_COLUMN_WIDTH,
         Natural'Min
           (Bytecode_Columns.Second_Width,
            Bytecode_Table.Header.w - Bytecode_Columns.First_Width -
              MINIMUM_INSTRUCTION_COLUMN_WIDTH));
      First_Column_Divider :=
        (x => Bytecode_Table.Header.x + Bytecode_Columns.First_Width - 3,
         y => Bytecode_Table.Header.y, w => 6,
         h => CuBit.UI.Table_Header_Height);
      Second_Column_Divider :=
        (x => Bytecode_Table.Header.x + Bytecode_Columns.First_Width +
           Bytecode_Columns.Second_Width - 3,
         y => Bytecode_Table.Header.y, w => 6,
         h => CuBit.UI.Table_Header_Height);
      CuBit.UI.Draw_Table_Header
        (Canvas, Bytecode_Table.Header,
         Colors, "PC", "Bytes", "Instruction", BYTECODE_COLUMNS);
      if Has_Compiled then
         declare
            Row_Height : constant Positive := BYTECODE_ROW_HEIGHT;
            Maximum_Rows : constant Natural :=
              Bytecode_Table.Rows.h / Row_Height;
            Rows : constant Natural := Natural'Min
              (Natural (Compiled_Artifact.Program.Length), Maximum_Rows);
            Item : CCL.VM.Instruction;
            PC   : CCL.VM.Instruction_Index;
            Row_Y : Natural;
            Listing_Clip : constant CuBit.UI.Canvas := CuBit.UI.With_Clip
              (Canvas, Bytecode_Table.Rows);
         begin
            if Rows > 0 then
               for Row in 0 .. Rows - 1 loop
                  PC := CCL.VM.Instruction_Index (Row);
                  Item := Compiled_Artifact.Program.Code (PC);
                  Row_Y := Bytecode_Table.Rows.y + Row * Row_Height;
                  CuBit.UI.Draw_Table_Row
                    (Listing_Clip,
                     (x => Bytecode_Table.Rows.x, y => Row_Y,
                      w => Bytecode_Table.Rows.w,
                      h => Row_Height),
                     Colors,
                     selected => VM_Has_State and then
                       VM_Snapshot.Instruction = PC,
                     hot => False,
                     c1 =>
                       ((if Breakpoints (PC) then "*" else " ") &
                        Natural'Image (Row)),
                     c2 => Op_Byte (Item.Op),
                     c3 => Instruction_Text (Item),
                     layout => BYTECODE_COLUMNS,
                     textStyle => CuBit.UI.Table_Code_Text);
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
            Bytecode_Content.x + 7, Bytecode_Content.y + 34,
            "No CCLB artifact", Colors.text, Colors.field);
         CuBit.UI.Draw_UI_Text
           (CuBit.UI.With_Clip
              (Canvas,
               (x => Bytecode_Content.x, y => Bytecode_Content.y + 54,
                w => Bytecode_Content.w, h => 44)),
            Bytecode_Content.x + 7, Bytecode_Content.y + 54,
            "Interpret mode does not emit bytecode",
            Colors.muted, Colors.field);
      end if;

      CuBit.UI.Draw_Vertical_Splitter
        (Canvas, Inspector_Splitter, Colors,
         hot => Pointer_Known and then
           CuBit.UI.Point_In_Rect
             (Pointer_X, Pointer_Y, Inspector_Splitter),
         active => Active_Resize = Inspector_Pane);
      CuBit.UI.Draw_Vertical_Splitter
        (Canvas, Disassembly_Splitter, Colors,
         hot => Pointer_Known and then
           CuBit.UI.Point_In_Rect
             (Pointer_X, Pointer_Y, Disassembly_Splitter),
         active => Active_Resize = Disassembly_Pane);

      CuBit.UI.Draw_Status_Bar
        (Canvas,
         (x => 0, y => Canvas.height - 26, w => Canvas.width, h => 26), Colors,
         Toolbar_Hint,
         "bounded document • proved viewport");
   end Render;

procedure Run is
begin
   CCL_Workbench_Platform.Activate;
   Initialize_Visible_Interfaces;
   declare
      Source_Result : CuBit.UI.Editor.Documents.Edit_Result;
      Find_Accepted : Boolean;
   begin
      CuBit.UI.Editor.Documents.Initialize
         (Source,
         "# Strings are immutable and indexes start at one." & ASCII.LF &
         "# Run with Interpret; string bytecode arrives with CCLB v4." &
           ASCII.LF &
         "(let ((elapsed-ms (* 3661 1000)))" & ASCII.LF &
         "  (let ((label ""uptime""))" & ASCII.LF &
         "    (let ((initial (at label 1)))" & ASCII.LF &
         "      (let ((hours (/ elapsed-ms 3600000)))" & ASCII.LF &
         "        (let ((minutes (/ (mod elapsed-ms 3600000) 60000)))" &
           ASCII.LF &
         "          (let ((seconds (/ (mod elapsed-ms 60000) 1000)))" &
           ASCII.LF &
         "            (concat" & ASCII.LF &
         "              (if (= (length (to-string hours)) 1)" & ASCII.LF &
         "                  (concat ""0"" (to-string hours)) " &
           "(to-string hours))" & ASCII.LF &
         "              (concat "":""" & ASCII.LF &
         "                (concat" & ASCII.LF &
         "                  (if (= (length (to-string minutes)) 1)" &
           ASCII.LF &
         "                      (concat ""0"" (to-string minutes)) " &
           "(to-string minutes))" & ASCII.LF &
         "                  (concat "":""" & ASCII.LF &
         "                    (if (= (length (to-string seconds)) 1)" &
           ASCII.LF &
         "                        (concat ""0"" (to-string seconds))" &
           ASCII.LF &
         "                        (to-string seconds))))))))))))" &
           ASCII.LF,
         Source_Result);
      if Source_Result /= CuBit.UI.Editor.Documents.Applied then
         raise Program_Error;
      end if;
      CuBit.UI.Editor.Cursors.Initialize (Source_Cursors, 1);
      Source_Histories.Initialize (Source_History);
      CuBit.UI.Editor.Viewports.Initialize (Source_View, 15);
      CuBit.UI.Editor.Initialize (Find_Query, "", Find_Accepted);
      if not Find_Accepted then
         raise Program_Error;
      end if;
   end;
   Result_Text (1 .. Result_Last) := "ready";
   declare
      Handle : constant System.Address :=
        Window_Open (Integer_32 (WIDTH), Integer_32 (HEIGHT));
      Kind : aliased Integer_32 := 0;
      Code : aliased Unsigned_32 := 0;
      Modifiers : aliased Unsigned_32 := 0;
      Mouse_X : aliased Integer_32 := 0;
      Mouse_Y : aliased Integer_32 := 0;
      Surface_Width : aliased Integer_32 := Integer_32 (WIDTH);
      Surface_Height : aliased Integer_32 := Integer_32 (HEIGHT);
      Running : Boolean := Handle /= System.Null_Address;
      Dragging : Boolean := False;
      Dragging_Scrollbar : Boolean := False;
      Dragging_Horizontal_Scrollbar : Boolean := False;
      Scrollbar_Grab_Offset : Natural := 0;
      Horizontal_Scrollbar_Grab_Offset : Natural := 0;
      Next_Scrollbar_Repeat : Interfaces.Unsigned_64 := 0;
      Next_Horizontal_Scrollbar_Repeat : Interfaces.Unsigned_64 := 0;
      SCROLL_REPEAT_DELAY : constant Interfaces.Unsigned_64 := 350;
      SCROLL_REPEAT_INTERVAL : constant Interfaces.Unsigned_64 := 60;
      Needs_Render : Boolean := True;
      Changed : Boolean;
      Extend : Boolean;
      By_Word : Boolean;
      Cursor_Toggle : CuBit.UI.Editor.Cursors.Toggle_Result;

      function Clamp_Width
        (Value, Minimum, Maximum : Natural) return Natural
      is
      begin
         if Maximum <= Minimum then
            return Minimum;
         else
            return Natural'Max (Minimum, Natural'Min (Value, Maximum));
         end if;
      end Clamp_Width;

      procedure Prepare_Surface is
         Old_Width : constant Natural := Canvas.width;
         Old_Height : constant Natural := Canvas.height;
      begin
         if Window_Prepare_Frame
           (Handle, Integer_32 (WIDTH), Integer_32 (HEIGHT),
            Integer_32 (MAXIMUM_WIDTH),
            Integer_32 (MAXIMUM_HEIGHT),
            Surface_Width'Access, Surface_Height'Access) /= 0
         then
            Running := False;
         else
            Canvas.width := Natural (Surface_Width);
            Canvas.height := Natural (Surface_Height);
            if Canvas.width /= Old_Width or else Canvas.height /= Old_Height then
               Needs_Render := True;
            end if;
         end if;
      end Prepare_Surface;
   begin
      if not Running then raise Program_Error; end if;
      Prepare_Surface;
      if not Running then raise Program_Error; end if;
      while Running loop
         while Running and then
           Window_Poll
             (Handle, Kind'Access, Code'Access, Modifiers'Access,
              Mouse_X'Access, Mouse_Y'Access) /= 0
         loop
            Needs_Render := True;
            case Kind is
               when 1 => Running := False;
               when 2 =>
                  if Find_Active and then Code >= 32 and then Code <= 126 then
                     CuBit.UI.Editor.Insert
                       (Find_Query, String'(1 => Character'Val (Code)), Changed);
                  elsif Code >= 32 and then Code <= 126 then
                     Insert_Source
                       (String'(1 => Character'Val (Code)), Changed);
                  end if;
               when 3 =>
                  if Find_Active then
                     CuBit.UI.Editor.Backspace (Find_Query, Changed);
                  else
                     Backspace_Source (Changed);
                  end if;
               when 4 =>
                  if Find_Active then
                     Find_Next_Query;
                  else
                     Insert_Source
                       (String'(1 => ASCII.LF), Changed,
                        Source_Histories.Other_Edit);
                  end if;
               when 5 | 6 =>
                  Extend := (Modifiers and 1) /= 0;
                  By_Word := (Modifiers and 2) /= 0;
                  if Find_Active then
                     CuBit.UI.Editor.Move
                       (Find_Query,
                        (if By_Word and then Kind = 6 then
                            CuBit.UI.Editor.Move_Word_Right
                         elsif By_Word then CuBit.UI.Editor.Move_Word_Left
                         elsif Kind = 6 then CuBit.UI.Editor.Move_Right
                         else CuBit.UI.Editor.Move_Left),
                        Extend_Selection => Extend);
                  else
                     Move_Source_Horizontal
                       (Right => Kind = 6, By_Word => By_Word,
                        Extend_Selection => Extend);
                  end if;
               when 7 =>
                  if Find_Active then
                     CuBit.UI.Editor.Move
                       (Find_Query, CuBit.UI.Editor.Move_Start,
                        Extend_Selection => (Modifiers and 1) /= 0);
                  else
                     Move_Source_Line_End
                       (To_End => False,
                        Extend_Selection => (Modifiers and 1) /= 0);
                  end if;
               when 8 =>
                  if Find_Active then
                     CuBit.UI.Editor.Move
                       (Find_Query, CuBit.UI.Editor.Move_End,
                        Extend_Selection => (Modifiers and 1) /= 0);
                  else
                     Move_Source_Line_End
                       (To_End => True,
                        Extend_Selection => (Modifiers and 1) /= 0);
                  end if;
               when 9 =>
                  if Find_Active then
                     CuBit.UI.Editor.Delete_Forward (Find_Query, Changed);
                  else
                     Delete_Source_Forward (Changed);
                  end if;
               when 10 =>
                  if Find_Active then
                     CuBit.UI.Editor.Select_All (Find_Query);
                  else
                     Select_All_Source;
                  end if;
               when 11 | 14 | 15 =>
                  Dragging_Scrollbar := False;
                  Dragging_Horizontal_Scrollbar := False;
                  Source_Scrollbar_Pressed := CuBit.UI.Scrollbar_None;
                  Source_Horizontal_Scrollbar_Pressed :=
                    CuBit.UI.Scrollbar_None;
                  Next_Scrollbar_Repeat := 0;
                  Next_Horizontal_Scrollbar_Repeat := 0;
                  if Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Inspector_Splitter)
                  then
                     Active_Resize := Inspector_Pane;
                     Dragging := False;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Disassembly_Splitter)
                  then
                     Active_Resize := Disassembly_Pane;
                     Dragging := False;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       First_Column_Divider)
                  then
                     Active_Resize := First_Table_Column;
                     Dragging := False;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Second_Column_Divider)
                  then
                     Active_Resize := Second_Table_Column;
                     Dragging := False;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
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
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Bytecode_Table.Rows)
                  then
                     declare
                        Row : constant Natural :=
                          (Natural (Mouse_Y) - Bytecode_Table.Rows.y) /
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
                     Find_Active := False;
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
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y),
                       Source_Horizontal_Scrollbar)
                  then
                     declare
                        Columns : constant Positive := Maximum_Source_Columns;
                        Extent : constant Natural :=
                          Source_Horizontal_Scrollbar.h;
                        Track, Thumb : CuBit.UI.Rect;
                        Maximum_First : Positive;
                        Relative_X : Natural;
                        Target : Positive;
                     begin
                        Source_Horizontal_Scrollbar_Metrics
                          (Track, Thumb, Maximum_First);
                        Dragging := False;
                        if Maximum_First > 1 and then
                          CuBit.UI.Point_In_Rect
                            (Natural (Mouse_X), Natural (Mouse_Y), Thumb)
                        then
                           Dragging_Horizontal_Scrollbar := True;
                           Source_Horizontal_Scrollbar_Pressed :=
                             CuBit.UI.Scrollbar_Thumb;
                           Horizontal_Scrollbar_Grab_Offset :=
                             Natural (Mouse_X) - Thumb.x;
                        elsif Natural (Mouse_X) <
                          Source_Horizontal_Scrollbar.x + Extent
                        then
                           if CuBit.UI.Editor.Viewports.First_Column
                             (Source_View) > 1
                           then
                              Source_Horizontal_Scrollbar_Pressed :=
                                CuBit.UI.Scrollbar_Decrement;
                              Next_Horizontal_Scrollbar_Repeat :=
                                Window_Ticks + SCROLL_REPEAT_DELAY;
                              CuBit.UI.Editor.Viewports.Scroll_Columns
                                (Source_View, -1, Columns);
                           end if;
                        elsif Natural (Mouse_X) >=
                          Source_Horizontal_Scrollbar.x +
                            Source_Horizontal_Scrollbar.w - Extent
                        then
                           if CuBit.UI.Editor.Viewports.First_Column
                             (Source_View) < Maximum_First
                           then
                              Source_Horizontal_Scrollbar_Pressed :=
                                CuBit.UI.Scrollbar_Increment;
                              Next_Horizontal_Scrollbar_Repeat :=
                                Window_Ticks + SCROLL_REPEAT_DELAY;
                              CuBit.UI.Editor.Viewports.Scroll_Columns
                                (Source_View, 1, Columns);
                           end if;
                        else
                           if Maximum_First > 1 then
                              Source_Horizontal_Scrollbar_Pressed :=
                                CuBit.UI.Scrollbar_Track;
                           end if;
                           Relative_X :=
                             (if Natural (Mouse_X) <= Track.x then 0
                              else Natural'Min
                                (Natural (Mouse_X) - Track.x, Track.w - 1));
                           Target := 1 + Relative_X * (Maximum_First - 1) /
                             Natural'Max (1, Track.w - 1);
                           CuBit.UI.Editor.Viewports.Scroll_Columns
                             (Source_View,
                              Integer (Target) - Integer
                                (CuBit.UI.Editor.Viewports.First_Column
                                   (Source_View)),
                              Columns);
                        end if;
                     end;
                  end if;
               when 12 =>
                  if Active_Resize /= No_Resize and then Mouse_X >= 0 then
                     declare
                        Pointer : constant Natural := Natural (Mouse_X);
                        Maximum : Natural;
                        Desired : Natural;
                     begin
                        case Active_Resize is
                           when Inspector_Pane =>
                              Maximum :=
                                Canvas.width - WORKSPACE_MARGIN * 2 -
                                SPLITTER_WIDTH * 2 - Disassembly_Width -
                                MINIMUM_SOURCE_WIDTH;
                              Desired :=
                                (if Pointer <= WORKSPACE_MARGIN then 0
                                 else Pointer - WORKSPACE_MARGIN);
                              Inspector_Width := Clamp_Width
                                (Desired, MINIMUM_INSPECTOR_WIDTH, Maximum);
                           when Disassembly_Pane =>
                              Maximum :=
                                Canvas.width - WORKSPACE_MARGIN * 2 -
                                SPLITTER_WIDTH * 2 - Inspector_Width -
                                MINIMUM_SOURCE_WIDTH;
                              Desired :=
                                (if Pointer + SPLITTER_WIDTH / 2 +
                                    WORKSPACE_MARGIN >= Canvas.width
                                 then 0
                                 else Canvas.width - WORKSPACE_MARGIN -
                                   Pointer - SPLITTER_WIDTH / 2);
                              Disassembly_Width := Clamp_Width
                                (Desired, MINIMUM_DISASSEMBLY_WIDTH, Maximum);
                           when First_Table_Column =>
                              Maximum :=
                                Bytecode_Table.Header.w -
                                Bytecode_Columns.Second_Width -
                                MINIMUM_INSTRUCTION_COLUMN_WIDTH;
                              Desired :=
                                (if Pointer <= Bytecode_Table.Header.x then 0
                                 else Pointer - Bytecode_Table.Header.x);
                              Bytecode_Columns.First_Width := Clamp_Width
                                (Desired, MINIMUM_TABLE_COLUMN_WIDTH, Maximum);
                           when Second_Table_Column =>
                              Maximum :=
                                Bytecode_Table.Header.w -
                                Bytecode_Columns.First_Width -
                                MINIMUM_INSTRUCTION_COLUMN_WIDTH;
                              Desired :=
                                (if Pointer <= Bytecode_Table.Header.x +
                                    Bytecode_Columns.First_Width
                                 then 0
                                 else Pointer - Bytecode_Table.Header.x -
                                   Bytecode_Columns.First_Width);
                              Bytecode_Columns.Second_Width := Clamp_Width
                                (Desired, MINIMUM_TABLE_COLUMN_WIDTH, Maximum);
                           when No_Resize => null;
                        end case;
                     end;
                  elsif Dragging_Scrollbar and then Mouse_Y >= 0 then
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
                  elsif Dragging_Horizontal_Scrollbar and then Mouse_X >= 0 then
                     declare
                        Track, Thumb : CuBit.UI.Rect;
                        Maximum_First : Positive;
                        Travel, Relative_X : Natural;
                        Pointer_X : constant Natural := Natural (Mouse_X);
                        Target : Positive;
                     begin
                        Source_Horizontal_Scrollbar_Metrics
                          (Track, Thumb, Maximum_First);
                        Travel := Track.w - Thumb.w;
                        if Pointer_X <=
                          Track.x + Horizontal_Scrollbar_Grab_Offset
                        then
                           Relative_X := 0;
                        else
                           Relative_X := Natural'Min
                             (Pointer_X - Track.x -
                                Horizontal_Scrollbar_Grab_Offset,
                              Travel);
                        end if;
                        Target := 1 + Relative_X * (Maximum_First - 1) /
                          Natural'Max (1, Travel);
                        CuBit.UI.Editor.Viewports.Scroll_Columns
                          (Source_View,
                           Integer (Target) - Integer
                             (CuBit.UI.Editor.Viewports.First_Column
                                (Source_View)),
                           Maximum_Source_Columns);
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
                  Active_Resize := No_Resize;
                  Dragging := False;
                  Dragging_Scrollbar := False;
                  Dragging_Horizontal_Scrollbar := False;
                  Source_Scrollbar_Pressed := CuBit.UI.Scrollbar_None;
                  Source_Horizontal_Scrollbar_Pressed :=
                    CuBit.UI.Scrollbar_None;
                  Next_Scrollbar_Repeat := 0;
                  Next_Horizontal_Scrollbar_Repeat := 0;
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
               when 27 | 28 =>
                  CuBit.UI.Editor.Viewports.Scroll_Columns
                    (Source_View, (if Kind = 27 then -3 else 3),
                     Maximum_Source_Columns);
               when 29 | 30 =>
                  Jump_To_Matching_Paren
                    (Extend_Selection => Kind = 30);
               when 31 =>
                  if not Find_Active then
                     Select_Next_Occurrence;
                  end if;
               when 32 =>
                  Open_Find;
               when 33 =>
                  if Find_Active or else
                    CuBit.UI.Editor.Length (Find_Query) > 0
                  then
                     Find_Next_Query;
                  else
                     Open_Find;
                  end if;
               when 22 =>
                  if Find_Active then
                     Find_Active := False;
                  else
                     Source_Histories.Break_Sequence (Source_History);
                     Collapse_Source_Cursors;
                     Reveal_Source_Cursor;
                  end if;
               when 23 =>
                  if not Find_Active and then
                    Source_Histories.Can_Undo (Source_History)
                  then
                     Source_Histories.Undo
                       (Source_History, Source, Source_Cursors);
                     Invalidate_Run_Result;
                     Reveal_Source_Cursor;
                  end if;
               when 24 =>
                  if not Find_Active and then
                    Source_Histories.Can_Redo (Source_History)
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
            Needs_Render := True;
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
         if (Source_Horizontal_Scrollbar_Pressed =
               CuBit.UI.Scrollbar_Decrement or else
             Source_Horizontal_Scrollbar_Pressed =
               CuBit.UI.Scrollbar_Increment) and then
           Window_Ticks >= Next_Horizontal_Scrollbar_Repeat
         then
            Needs_Render := True;
            declare
               Columns : constant Positive := Maximum_Source_Columns;
               Maximum_First : constant Positive :=
                 (if CuBit.UI.Editor.Viewports.Column_Capacity (Source_View) >=
                    Columns
                  then 1
                  else Columns -
                    CuBit.UI.Editor.Viewports.Column_Capacity (Source_View) +
                      1);
               Moving_Left : constant Boolean :=
                 Source_Horizontal_Scrollbar_Pressed =
                   CuBit.UI.Scrollbar_Decrement;
            begin
               if (Moving_Left and then
                   CuBit.UI.Editor.Viewports.First_Column (Source_View) > 1) or
                 else
                 (not Moving_Left and then
                  CuBit.UI.Editor.Viewports.First_Column (Source_View) <
                    Maximum_First)
               then
                  CuBit.UI.Editor.Viewports.Scroll_Columns
                    (Source_View, (if Moving_Left then -1 else 1), Columns);
                  Next_Horizontal_Scrollbar_Repeat :=
                    Window_Ticks + SCROLL_REPEAT_INTERVAL;
               else
                  Source_Horizontal_Scrollbar_Pressed :=
                    CuBit.UI.Scrollbar_None;
                  Next_Horizontal_Scrollbar_Repeat := 0;
               end if;
            end;
         end if;
         if VM_Continuous then
            Needs_Render := True;
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
         Prepare_Surface;
         exit when not Running;
         if Needs_Render then
            Render;
            exit when Window_Present
              (Handle, Pixels'Address,
               Integer_32 (MAXIMUM_WIDTH * 4)) /= 0;
            Needs_Render := False;
         end if;
         Window_Wait;
      end loop;
      Window_Close (Handle);
   end;
end Run;
end CCL_Workbench;
