with CuBit.UI.Theme_CCL;
with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Streams.Stream_IO;
with Files_Limits;
with Files_Mock_Service;

package body Files_Host_Support is
   DEFAULT_SCRATCH : constant String := "/home/doc/cubit-build-tmp/files-scratch";
   BYTES_PER_PIXEL : constant := 4;
   CHANNEL_BITS : constant := 8;
   CHANNEL_MASK : constant := 16#FF#;
   START : constant Time := Clock;

   function New_Surface (Width, Height : Positive) return Surface is
     ((Width, Height, new Pixels'(0 .. Width * Height - 1 => 0)));

   function Canvas (S : Surface) return CuBit.UI.Canvas is
     ((addr => S.Image.all'Address, width => S.Width, height => S.Height, pitch => S.Width * BYTES_PER_PIXEL,
       others => <>));

   function Canvas (S : Surface; Damage : CuBit.UI.Rect) return CuBit.UI.Canvas is
     (CuBit.UI.With_Clip (Canvas (S), Damage));

   function Pixel (S : Surface; X, Y : Natural) return Unsigned_32 is (S.Image (Y * S.Width + X));

   procedure Save_PPM (S : Surface; Path : String) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Header : constant String :=
        "P6" & ASCII.LF & Natural'Image (S.Width) & Natural'Image (S.Height) & ASCII.LF & "255" & ASCII.LF;
      Row : Stream_Element_Array (1 .. Stream_Element_Offset (S.Width * 3));
   begin
      Create (File, Out_File, Path);
      for C of Header loop
         Stream_Element_Array'Write (Stream (File), [1 => Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. S.Height - 1 loop
         for X in 0 .. S.Width - 1 loop
            declare
               P : constant Unsigned_32 := Pixel (S, X, Y);
               Base : constant Stream_Element_Offset := Stream_Element_Offset (X * 3);
            begin
               Row (Base + 1) := Stream_Element (Shift_Right (P, 2 * CHANNEL_BITS) and CHANNEL_MASK);
               Row (Base + 2) := Stream_Element (Shift_Right (P, CHANNEL_BITS) and CHANNEL_MASK);
               Row (Base + 3) := Stream_Element (P and CHANNEL_MASK);
            end;
         end loop;
         Stream_Element_Array'Write (Stream (File), Row);
      end loop;
      Close (File);
   end Save_PPM;

   function Now_Us return Unsigned_64 is
     (Unsigned_64 (To_Duration (Clock - START) * 1_000_000));

   function Scratch_Root return String is
     (if Ada.Environment_Variables.Exists ("FILES_SCRATCH") then Ada.Environment_Variables.Value ("FILES_SCRATCH")
      else DEFAULT_SCRATCH);

   protected Wake_Event is
      procedure Signal;
      entry Wait;
      procedure Clear;
   private
      Raised : Boolean := False;
   end Wake_Event;
   protected body Wake_Event is
      procedure Signal is
      begin
         Raised := True;
      end Signal;
      entry Wait when Raised is
      begin
         Raised := False;
      end Wait;
      procedure Clear is
      begin
         Raised := False;
      end Clear;
   end Wake_Event;

   procedure Woken is
   begin
      Wake_Event.Signal;
   end Woken;

   procedure Wait_For_Wake (Timeout_Ms : Positive) is
   begin
      select
         Wake_Event.Wait;
      or
         delay Duration (Timeout_Ms) / 1_000;
      end select;
   end Wait_For_Wake;

   procedure Install_Themes (Scheme : CuBit.Appearance.Color_Scheme) is
      use type CuBit.Appearance.Color_Scheme;
   begin
      for Each in CuBit.Appearance.Color_Scheme loop
         declare
            Name : constant String :=
              (if Each = CuBit.Appearance.Alloy_Light then "FILES_THEME_LIGHT" else "FILES_THEME_DARK");
            Fallback : constant CuBit.UI.Theme := CuBit.UI.Default_Palette (Each);
            Loaded : CuBit.UI.Theme_CCL.Result;
         begin
            CuBit.UI.Install_Palette (Each, Fallback);
            if Ada.Environment_Variables.Exists (Name) then
               declare
                  Source : constant String := Ada.Environment_Variables.Value (Name);
               begin
                  if Source'Length in 1 .. CuBit.UI.Theme_CCL.Maximum_Source then
                     CuBit.UI.Theme_CCL.Load (Source, Fallback, Loaded);
                     if Loaded.Success then
                        CuBit.UI.Install_Palette (Each, Loaded.Value);
                     else
                        Ada.Text_IO.Put_Line ("files: invalid " & Name & "; using the built-in palette");
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
      CuBit.UI.Set_Color_Scheme (Scheme);
   end Install_Themes;

   procedure Configure_Service (Host_Root : String) is
   begin
      Files_Mock_Service.Configure (Host_Root, Scratch_Root);
      Files_Mock_Service.Set_Wake_Hook (Woken'Access);
   end Configure_Service;

   function Settle (View : in out Files_View.View_State; Timeout_Ms : Positive := 10_000) return Boolean is
      Deadline : constant Time := Clock + Milliseconds (Timeout_Ms);
      Busy, Changed : Boolean;
      PUMP_BUDGET : constant Files_Limits.Work_Budget := 1_000_000;
      --  A bound on one wait (deadlines inside the view are shorter).
      WAKE_WAIT_MS : constant := 50;
   begin
      loop
         Files_View.Pump (View, PUMP_BUDGET, Now_Us, Busy, Changed);
         if not Busy and then not Files_View.Waiting_For_IO (View) then
            return True;
         elsif Clock > Deadline then
            return False;
         end if;
         --  Idle with requests out: sleep until the service's wake.
         if not Busy then
            Wait_For_Wake (WAKE_WAIT_MS);
         end if;
      end loop;
   end Settle;

   --  The control holding the pointer between a press and its release,
   --  as CuBit.UI.App.Run keeps it: moves and the release go to it even
   --  outside its bounds.
   Captured : CuBit.UI.Controls.Control_ID := CuBit.UI.Controls.NO_CONTROL;
   Held : Boolean := False;
   Hovered : CuBit.UI.Controls.Control_ID := CuBit.UI.Controls.NO_CONTROL;

   procedure Pointer
     (View : in out Files_View.View_State; UI : in out CuBit.UI.State.UI_State;
      Map : in out CuBit.UI.Controls.Control_Map; Action : CuBit.UI.Controls.Pointer_Action; X, Y : Natural;
      Time_Ms : Unsigned_64 := 0; Control, Shift : Boolean := False; Secondary : Boolean := False;
      Middle : Boolean := False)
   is
      use CuBit.UI.Controls;
      use type CuBit.UI.Controls.Pointer_Action;
      Hit_Target : constant Control_ID := Hit (Map, X, Y);
      Target : Control_ID;
      Changed, Handled, Redraw : Boolean := False;
   begin
      if Secondary or else Middle then
         --  The secondary and middle buttons never capture a control
         --  (context menus; closing a tab).
         Files_View.Handle
           (View, (Kind => Files_View.Pointer_Event, Action => Action, X => X, Y => Y, Time_Ms => Time_Ms,
                   Control => Control, Shift => Shift, Secondary => Secondary, Middle => Middle, others => <>),
            Map, Redraw);
         return;
      end if;
      --  Hover transitions repaint the faces left and entered.
      if Hit_Target /= Hovered then
         if Hovered /= NO_CONTROL then
            Files_View.Note_Damage (View, Visual_Damage (Map, Hovered));
         end if;
         if Hit_Target /= NO_CONTROL then
            Files_View.Note_Damage (View, Visual_Damage (Map, Hit_Target));
         end if;
         Hovered := Hit_Target;
      end if;
      case Action is
         when Pointer_Press =>
            if Captured /= NO_CONTROL then
               Dispatch_Pointer (Map, Captured, Pointer_Cancel, X, Y, Changed, Handled);
            end if;
            Captured := Hit_Target;
            Held := True;
            Target := Hit_Target;
            CuBit.UI.State.Set_Pointer (UI, X, Y, True, pressed => True);
         when Pointer_Release =>
            Target := Captured;
            CuBit.UI.State.Set_Pointer (UI, X, Y, False, released => True);
         when others =>
            Target := (if Held then Captured else NO_CONTROL);
            CuBit.UI.State.Set_Pointer (UI, X, Y, Held);
      end case;
      if Target /= NO_CONTROL then
         Dispatch_Pointer (Map, Target, Action, X, Y, Changed, Handled);
         --  What App.Run repaints: a changed or continuous control's action
         --  region, else its face.
         if (Handled and then Changed) or else Has_Continuous_Action (Map, Target) then
            Files_View.Note_Damage (View, Action_Damage (Map, Target));
         elsif Action /= Pointer_Move then
            Files_View.Note_Damage (View, Visual_Damage (Map, Target));
         end if;
      end if;
      if Action = Pointer_Release then
         Captured := NO_CONTROL;
         Held := False;
      end if;
      Files_View.Handle
        (View, (Kind => Files_View.Pointer_Event, Action => Action, X => X, Y => Y, Time_Ms => Time_Ms,
                Control => Control, Shift => Shift, others => <>), Map, Redraw);
   end Pointer;
end Files_Host_Support;
