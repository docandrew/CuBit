with CCL_Application;
with Ada.Unchecked_Deallocation;
------------------------------------------------------------------------------
--  Linux/SDL supplies the ccl_window_* boundary from native_window.c.
------------------------------------------------------------------------------

package body CCL_Desktop_Platform is
   procedure Finish_Input is null;
   function Host_Yield return Interfaces.Integer_32
     with Import, Convention => C, External_Name => "sched_yield";
   procedure Yield_Input is
      Ignore : Interfaces.Integer_32;
   begin
      Ignore := Host_Yield;
   end Yield_Input;

   type Image is array (Natural range <>) of Interfaces.Unsigned_32;
   type Image_Access is access Image;
   procedure Free is new Ada.Unchecked_Deallocation (Image, Image_Access);
   Pixels : Image_Access;
   Width, Height : Natural := 0;
   procedure Begin_Frame
     (Canvas : in out CuBit.UI.Canvas; Changed : CuBit.UI.Rect;
      Repair : out CuBit.UI.Rect; Ready : out Boolean)
   is
   begin
      Ready := False;
      Repair := (others => 0);
      if Canvas.width = 0 or Canvas.height = 0 then return; end if;
      Canvas.clipEnabled := False;
      Repair := CuBit.UI.Clamp_Rect (Canvas, Changed);
      if Pixels = null or else Width /= Canvas.width or else Height /= Canvas.height then
         Free (Pixels);
         Width := Canvas.width; Height := Canvas.height;
         Pixels := new Image'(0 .. Width * Height - 1 => 0);
         Repair := (0, 0, Width, Height);
      end if;
      Canvas.addr := Pixels.all'Address;
      Canvas.pitch := Width * 4;
      Ready := not CuBit.UI.Is_Empty (Repair);
   end Begin_Frame;
   function Present
     (Handle, Pixels : System.Address; Pitch, X, Y, Width, Height : Interfaces.Integer_32)
      return Interfaces.Integer_32
     with Import, Convention => C, External_Name => "ccl_window_present";
   function Submit_Frame
     (Handle : System.Address; Canvas : in out CuBit.UI.Canvas;
      Rendered : CuBit.UI.Rect) return Boolean
   is
      use type Interfaces.Integer_32;
   begin
      return Present (Handle, Canvas.addr, Interfaces.Integer_32 (Canvas.pitch),
        Interfaces.Integer_32 (Rendered.x), Interfaces.Integer_32 (Rendered.y),
        Interfaces.Integer_32 (Rendered.w), Interfaces.Integer_32 (Rendered.h)) = 0;
   end Submit_Frame;
   function Frame_Pending return Boolean is (False);
   function Frame_Deadline (Application : Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64 is (Application);

   procedure Live_Label_Changed (Event : Live_Label_Event) is null;
   procedure REPL_Completed (Result : String) is null;
   procedure Set_Title (Title : System.Address; Length : Interfaces.Integer_32)
     with Import, Convention => C, External_Name => "ccl_window_set_title";
   procedure Activate (Name, Title : String) is
   begin
      CCL_Application.Set_Name (Name);
      Set_Title (Title'Address, Interfaces.Integer_32 (Title'Length));
   end Activate;
   --  Linux preview: an in-process clipboard (no desktop, no gestures).
   PREVIEW_CLIPBOARD : constant := 8_192;
   Clip_Text : String (1 .. PREVIEW_CLIPBOARD);
   Clip_Last : Natural := 0;
   Clip_Typed : Boolean := False;
   function Keep (Text : String; Typed : Boolean) return Boolean is
   begin
      if Text'Length > PREVIEW_CLIPBOARD then return False; end if;
      Clip_Text (1 .. Text'Length) := Text;
      Clip_Last := Text'Length;
      Clip_Typed := Typed;
      return True;
   end Keep;
   function Copy_Text (Text : String) return Boolean is (Keep (Text, False));
   function Copy_Value (Type_Name, Text, Literal : String) return Boolean is
     (Type_Name'Length > 0 and then Text'Length > 0 and then Keep (Literal, True));
   procedure Paste
     (Text : out String; Last : out Natural; Typed, Pasted : out Boolean) is
      Count : constant Natural := Natural'Min (Clip_Last, Text'Length);
   begin
      Text := [others => ' '];
      Text (Text'First .. Text'First + Count - 1) := Clip_Text (1 .. Count);
      Last := Text'First + Count - 1;
      Typed := Clip_Typed;
      Pasted := Count > 0;
   end Paste;
end CCL_Desktop_Platform;
