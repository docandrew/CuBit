with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.File_Selection; use CuBit.File_Selection;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.File_Dialogs; use CuBit.UI.File_Dialogs;
with CCL_Workspace; use CCL_Workspace;

procedure Main is
   Files, Before : CuBit.File_Selection.File_List;
   Result : Storage_Result;
   Text : Source_Buffer;
   Length : Source_Length;
   Name : File_Name;
   Accepted : Boolean;
   Dialog : Dialog_State;
   Action : Dialog_Action;
   type Surface is array (0 .. 399, 0 .. 899) of Color;
   Buffer : aliased Surface := [others => [others => 16#526272#]];
   C : constant Canvas := (Buffer'Address, 900, 400, 3600, others => <>);
   Output : Ada.Text_IO.File_Type;

   procedure Key (Kind : Event_Kind; Shift : Boolean := False) is
   begin
      Handle (Dialog, (Kind => Kind, Shift => Shift, others => <>), 900, 400, Action);
   end Key;
   procedure Type_Name (S : String) is
   begin
      Key (Select_All);
      for Ch of S loop
         Handle (Dialog, (Kind => Text_Input, Character_Value => Ch, others => <>),
           900, 400, Action);
      end loop;
   end Type_Name;
   procedure Click (X, Y : Natural) is
   begin
      Handle (Dialog, (Kind => Pointer_Down, X => X, Y => Y, others => <>), 900, 400, Action);
      Handle (Dialog, (Kind => Pointer_Up, X => X, Y => Y, others => <>), 900, 400, Action);
   end Click;
begin
   pragma Assert (Valid_Leaf ("clock.ccl"));
   pragma Assert (not Valid_Leaf ("../secrets.ccl"));
   pragma Assert (not Valid_Leaf ("@nvme:0/private.ccl"));
   pragma Assert (not Valid_Leaf ("/clock.ccl"));
   pragma Assert (not Valid_Leaf ("a\b.ccl"));
   pragma Assert (not Valid_Leaf (""));
   pragma Assert (not Valid_Source_Name ("clock.ccl.pending"));
   declare
      Offset_Name : constant String (9 .. 17) := "clock.ccl";
   begin
      pragma Assert (Valid_Source_Name (Offset_Name));
   end;
   List_Files (Files, Result);
   pragma Assert (Result = Succeeded and Files.Count = 2);
   Before := Files;
   Save_New ("../bad.ccl", "bad", Result);
   pragma Assert (Result = Invalid_Name);
   Save_New ("broken.ccl", String'(1 => Character'Val (255)), Result);
   pragma Assert (Result = Invalid_Source);
   List_Files (Files, Result);
   pragma Assert (Files = Before);
   Save_New ("test.ccl", "(+ 20 22)", Result);
   pragma Assert (Result = Succeeded);
   Save_New ("test.ccl", "overwrite", Result);
   pragma Assert (Result = Conflict);
   Load ("test.ccl", Text, Length, Result);
   pragma Assert (Result = Succeeded and Text (1 .. Length) = "(+ 20 22)");
   Load ("missing.ccl", Text, Length, Result);
   pragma Assert (Result = Not_Found and Length = 0);
   Save_New ("empty.ccl", "", Result);
   pragma Assert (Result = Succeeded);
   Load ("empty.ccl", Text, Length, Result);
   pragma Assert (Result = Succeeded and Length = 0);
   Save_New ("maximum.ccl", String'(1 .. Maximum_Source_Bytes => 'x'), Result);
   pragma Assert (Result = Succeeded);
   Load ("maximum.ccl", Text, Length, Result);
   pragma Assert (Result = Succeeded and Length = Maximum_Source_Bytes);
   Save_New ("too-large.ccl", String'(1 .. Maximum_Source_Bytes + 1 => 'x'), Result);
   pragma Assert (Result = Invalid_Source);

   List_Files (Files, Result);
   Show (Dialog, Open_File, Files, Location);
   pragma Assert (Is_Open (Dialog) and Filename (Dialog) = "hello.ccl");
   Key (Down);
   pragma Assert (Filename (Dialog) = "arithmetic.ccl");
   Key (Enter);
   pragma Assert (Action = Submit and Is_Open (Dialog)); -- Caller owns completion.
   Set_Error (Dialog, "Backend unavailable; source is unchanged");
   Key (Escape);
   pragma Assert (Action = Cancelled and not Is_Open (Dialog));
   Suggest_Name (Name, Result);
   pragma Assert (Result = Succeeded);
   Show (Dialog, Save_New_File, Files, Location, Value (Name));
   Type_Name ("my-clock.ccl");
   pragma Assert (Filename (Dialog) = "my-clock.ccl");
   Key (Left);
   Key (Backspace);
   pragma Assert (Filename (Dialog) = "my-clock.cl");
   Key (Escape);
   Show (Dialog, Save_New_File, Files, Location, "new.ccl");
   Key (Tab); Key (Tab); Key (Enter);
   pragma Assert (Action = Cancelled and not Is_Open (Dialog));
   Show (Dialog, Save_New_File, Files, Location, "new.ccl");
   Type_Name ("../secret.ccl");
   Key (Enter);
   pragma Assert (Action = No_Action and Is_Open (Dialog));
   Type_Name (String'(1 .. Maximum_Name_Length + 10 => 'a'));
   pragma Assert (Filename (Dialog)'Length = Maximum_Name_Length);
   Key (Escape);
   --  Modal button activation requires press and release on the same button.
   Show (Dialog, Open_File, Files, Location);
   Handle (Dialog, (Kind => Pointer_Down, X => 565, Y => 355, others => <>), 900, 400, Action);
   Handle (Dialog, (Kind => Pointer_Up, X => 10, Y => 10, others => <>), 900, 400, Action);
   pragma Assert (Action = No_Action and Is_Open (Dialog));
   Click (565, 355);
   pragma Assert (Action = Submit);
   Click (680, 355);
   pragma Assert (Action = Cancelled);
   --  A full list scrolls and retains a single selection across page movement.
   Files := (others => <>);
   for I in 1 .. Maximum_Files loop
      Append (Files, "file" & Integer'Image (I) & ".ccl", Accepted);
      pragma Assert (Accepted);
   end loop;
   Before := Files;
   Append (Files, "overflow.ccl", Accepted);
   pragma Assert (not Accepted and Files = Before);
   Show (Dialog, Open_File, Files, Location);
   for I in 1 .. 20 loop Key (Page_Down); end loop;
   pragma Assert (Filename (Dialog) = "file 64.ccl");
   for I in 1 .. 20 loop Key (Page_Up); end loop;
   pragma Assert (Filename (Dialog) = "file 1.ccl");
   --  Scrollbar thumb drag then first visible row must select a later file.
   Handle (Dialog, (Kind => Pointer_Down, X => 730, Y => 132, others => <>), 900, 400, Action);
   Handle (Dialog, (Kind => Pointer_Drag, X => 730, Y => 225, others => <>), 900, 400, Action);
   Handle (Dialog, (Kind => Pointer_Up, X => 730, Y => 225, others => <>), 900, 400, Action);
   Click (200, 118);
   pragma Assert (Filename (Dialog) /= "file 1.ccl");
   Key (Escape);
   --  Check the mock's bounded storage capacity and failure without overwrite.
   for I in 1 .. Maximum_Files loop
      Save_New ("fill" & Integer'Image (I) & ".ccl", "1", Result);
      exit when Result = Limit_Reached;
      pragma Assert (Result = Succeeded);
   end loop;
   pragma Assert (Result = Limit_Reached);
   Load ("test.ccl", Text, Length, Result);
   pragma Assert (Result = Succeeded and Text (1 .. Length) = "(+ 20 22)");
   --  Optional visual capture of the same reusable widget used by Workbench.
   Show (Dialog, Save_New_File, Files, Location, "clock.ccl");
   Draw (C, Dialog, CuBit_Alloy);
   for Y in Buffer'Range (1) loop
      for X in Buffer'Range (2) loop
         if not Point_In_Rect (X, Y, Bounds (900, 400)) then
            pragma Assert (Buffer (Y, X) = 16#526272#);
         end if;
      end loop;
   end loop;
   if Ada.Command_Line.Argument_Count = 1 then
      Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Ada.Command_Line.Argument (1));
      Ada.Text_IO.Put_Line (Output, "P3");
      Ada.Text_IO.Put_Line (Output, "900 400");
      Ada.Text_IO.Put_Line (Output, "255");
      for Y in Buffer'Range (1) loop
         for X in Buffer'Range (2) loop
            Ada.Text_IO.Put_Line (Output,
              Unsigned_32'Image (Shift_Right (Buffer (Y, X), 16) and 255) &
              Unsigned_32'Image (Shift_Right (Buffer (Y, X), 8) and 255) &
              Unsigned_32'Image (Buffer (Y, X) and 255));
         end loop;
      end loop;
      Ada.Text_IO.Close (Output);
   end if;
   Ada.Text_IO.Put_Line ("File dialog and Linux mock workspace tests PASS");
end Main;
