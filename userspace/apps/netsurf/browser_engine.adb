package body Browser_Engine is
   use type C.int;
   use type System.Address;
   use type CuBit.UI.Pointer_Cursor_Style;

   procedure Copy
     (Target : in out Text_Buffer; Source : System.Address; Length : C.size_t)
   is
      Count : constant Natural :=
        (if Source = System.Null_Address then 0
         else Natural (C.size_t'Min (Length, C.size_t (Max_Text))));
   begin
      if Count = 0 then
         Target.Length := 0;
         return;
      end if;
      declare
         Chars : constant String (1 .. Count)
           with Import, Address => Source;
      begin
         Target.Data (1 .. Count) := Chars;
         Target.Length := Count;
      end;
   end Copy;

   function To_Natural (Value : C.int) return Natural is
     (if Value < 0 then 0 else Natural (Value));

   procedure On_Invalidate (X, Y, W, H : C.int) is
      Area : constant CuBit.UI.Rect :=
        (To_Natural (X), To_Natural (Y), To_Natural (W), To_Natural (H));
   begin
      if W <= 0 or else H <= 0 then
         Damage_All := True;
      elsif not Damage_All then
         Damage := CuBit.UI.Union_Rect (Damage, Area);
      end if;
   end On_Invalidate;

   procedure On_URL (Text : System.Address; Length : C.size_t) is
   begin
      Copy (URL, Text, Length);
      URL_Changed := True;
      Chrome_Changed := True;
   end On_URL;

   procedure On_Title (Text : System.Address; Length : C.size_t) is
   begin
      Copy (Title, Text, Length);
      Title_Changed := True;
   end On_Title;

   procedure On_Status (Text : System.Address; Length : C.size_t) is
   begin
      Copy (Status, Text, Length);
      Chrome_Changed := True;
   end On_Status;

   procedure On_Extent (Width, Height : C.int) is
   begin
      Extent_Width := To_Natural (Width);
      Extent_Height := To_Natural (Height);
      Chrome_Changed := True;
   end On_Extent;

   procedure On_Scroll (X, Y : C.int) is
   begin
      Scroll_X := To_Natural (X);
      Scroll_Y := To_Natural (Y);
      Chrome_Changed := True;
   end On_Scroll;

   procedure On_Busy (Busy_Now : C.int) is
   begin
      Busy := Busy_Now /= 0;
      Chrome_Changed := True;
   end On_Busy;

   procedure On_Pointer (Shape : C.int) is
      --  NetSurf's gui_pointer_shape; the desktop has no hand cursor yet.
      Next : constant CuBit.UI.Pointer_Cursor_Style :=
        (case Shape is
            when 2 => CuBit.UI.Pointer_Text,
            when 4 | 5 => CuBit.UI.Pointer_Resize_Vertical,
            when 6 | 7 => CuBit.UI.Pointer_Resize_Horizontal,
            when 8 .. 11 => CuBit.UI.Pointer_Resize_Diagonal,
            when others => CuBit.UI.Pointer_Default);
   begin
      if Next /= Cursor then
         Cursor := Next;
         Cursor_Changed := True;
      end if;
   end On_Pointer;
end Browser_Engine;
