with Ada.Text_IO;
with Bookmark_IO_Stub;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; with Interfaces; use Interfaces; with System;
with CuBit.UI; use CuBit.UI; with CuBit.UI.Input; use CuBit.UI.Input;
with Servo_Bookmarks; with Servo_Bookmark_Model;
procedure Bookmark_Preview is
   Pixels : aliased array (0 .. 800 * 600 - 1) of Color;
   C : constant Canvas := (addr => Pixels'Address, width => 800, height => 600, pitch => 3200, others => <>);
   D, Other : Servo_Bookmarks.Dialog;
   Result : Servo_Bookmarks.Outcome;
   use type Servo_Bookmarks.Outcome;
   use type Servo_Bookmark_Model.Store;
   procedure Input (Kind, A : Unsigned_64; B : Unsigned_64 := 0) is
   begin
      Servo_Bookmarks.Handle (D, (kind => Kind, payload0 => A, payload1 => B, others => <>), Result);
      if Servo_Bookmarks.Is_Open (D) then Servo_Bookmarks.Draw (C, D, CuBit_Alloy); end if;
   end Input;
   procedure Click (X, Y : Natural) is
   begin
      Input (INPUT_POINTER_DOWN, Unsigned_64 (X) + Shift_Left (Unsigned_64 (Y), 32), 1);
      Input (INPUT_POINTER_UP, Unsigned_64 (X) + Shift_Left (Unsigned_64 (Y), 32), 0);
   end Click;
   package IO renames Ada.Streams.Stream_IO;
   F : IO.File_Type; RGB : Stream_Element_Array (1 .. 800 * 600 * 3); At_Byte : Stream_Element_Offset := 1;
begin
   Fill_Rect (C, (0, 0, 800, 600), CuBit_Alloy.panel);
   Servo_Bookmarks.Open (D, "https://servo.org/", "Servo", True);
   Servo_Bookmarks.Draw (C, D, CuBit_Alloy);
   for P of Pixels loop
      RGB (At_Byte) := Stream_Element (Shift_Right (P, 16) and 255);
      RGB (At_Byte + 1) := Stream_Element (Shift_Right (P, 8) and 255);
      RGB (At_Byte + 2) := Stream_Element (P and 255); At_Byte := At_Byte + 3;
   end loop;
   IO.Create (F, IO.Out_File, "/tmp/cubit-bookmarks-preview.ppm");
   String'Write (IO.Stream (F), "P6" & ASCII.LF & "800 600" & ASCII.LF & "255" & ASCII.LF);
   IO.Write (F, RGB); IO.Close (F);
   -- Save failure leaves the draft and current page bookmark intact.
   Input (INPUT_KEY_DOWN, 16#1E#, 2);
   for Letter of String'("Renamed Servo") loop Input (INPUT_TEXT, Character'Pos (Letter)); end loop;
   Bookmark_IO_Stub.Fail_Save := True;
   Input (INPUT_KEY_DOWN, 16#1C#); pragma Assert (Bookmark_IO_Stub.Saves = 0 and Servo_Bookmarks.Is_Open (D));
   Bookmark_IO_Stub.Fail_Save := False;
   Input (INPUT_KEY_DOWN, 16#1C#); pragma Assert (Bookmark_IO_Stub.Saves = 1);
   pragma Assert (Servo_Bookmark_Model.Title (Bookmark_IO_Stub.Saved, 2) = "Renamed Servo");
   Servo_Bookmarks.Open (Other, "https://servo.org/", "Servo", True);
   -- New folder, save, and two-step deletion through retained pointer actions.
   Click (230, 136);
   Input (INPUT_KEY_DOWN, 16#1C#); pragma Assert (Bookmark_IO_Stub.Saves = 2);
   Click (462, 386); pragma Assert (Bookmark_IO_Stub.Saves = 2);
   Click (462, 386); pragma Assert (Bookmark_IO_Stub.Saves = 3);
   -- A stale editor in another window cannot overwrite newer data.
   Servo_Bookmarks.Handle (Other, (kind => INPUT_KEY_DOWN, payload0 => 16#1C#, others => <>), Result);
   pragma Assert (Bookmark_IO_Stub.Saves = 3);
   Servo_Bookmarks.Open (D, "https://servo.org/", "Servo", False);
   Servo_Bookmarks.Draw (C, D, CuBit_Alloy);
   Click (548, 386); pragma Assert (Result = Servo_Bookmarks.Navigate);
   pragma Assert (Servo_Bookmarks.Location (D) = "https://servo.org/");
   Ada.Text_IO.Put_Line ("PASS bookmark dialog: edit/retry, pointer folder creation/deletion, stale-window rejection, open URL");
end Bookmark_Preview;
