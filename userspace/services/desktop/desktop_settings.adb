with System.Storage_Elements; use System.Storage_Elements;
with Desktop_Wallpaper;

package body Desktop_Settings is
   use CuBit.UI;
   use type CuBit.Appearance.Preferences;
   package A renames CuBit.Appearance;
   use type A.Color_Scheme;
   use type A.Background;
   use type A.Placement;

   function Box (Item : Control; B : Rect) return Rect is
   begin
      case Item is
         when Light => return (B.x + 280, B.y + 56, 140, 30);
         when Dark => return (B.x + 430, B.y + 56, 140, 30);
         when Image => return (B.x + 280, B.y + 128, 68, 30);
         when Cubie => return (B.x + 354, B.y + 128, 68, 30);
         when Slate => return (B.x + 428, B.y + 128, 68, 30);
         when Ocean => return (B.x + 502, B.y + 128, 70, 30);
         when Fill => return (B.x + 280, B.y + 200, 92, 30);
         when Fit => return (B.x + 380, B.y + 200, 92, 30);
         when Center => return (B.x + 480, B.y + 200, 92, 30);
         when Apply => return (B.x + 366, B.y + 334, 100, 30);
         when Revert => return (B.x + 476, B.y + 334, 100, 30);
         when None => return (others => 0);
      end case;
   end Box;

   procedure Open (View : out State; Current : A.Preferences) is
   begin
      View := (Pending => Current, Applied => Current, others => <>);
   end Open;

   procedure Draw (View : State; C : Canvas; Bounds : Rect) is
      Colors : constant Theme := Current_Theme;
      Preview_Colors : constant Theme := Palette (View.Pending.Scheme);
      Preview : constant Rect := (Bounds.x + 20, Bounds.y + 56, 236, 150);
      P : constant Rect := Clamp_Rect (C, Preview);
      function Label (Item : Control) return String is
        (case Item is when Light => "Alloy Light", when Dark => "Alloy Dark",
         when Image => "Cubes", when Cubie => "Cubie", when Slate => "Slate",
         when Ocean => "Ocean", when Fill => "Fill", when Fit => "Fit",
         when Center => "Center", when Apply => "Apply", when Revert => "Revert",
         when None => "");
      function Selected (Item : Control) return Boolean is
        (case Item is
         when Light => View.Pending.Scheme = A.Alloy_Light,
         when Dark => View.Pending.Scheme = A.Alloy_Dark,
         when Image => View.Pending.Backdrop = A.Wallpaper,
         when Cubie => View.Pending.Backdrop = A.Cubie,
         when Slate => View.Pending.Backdrop = A.Slate,
         when Ocean => View.Pending.Backdrop = A.Ocean,
         when Fill => View.Pending.Position = A.Fill,
         when Fit => View.Pending.Position = A.Fit,
         when Center => View.Pending.Position = A.Center,
         when others => False);
      procedure Text (X, Y : Natural; Value : String) is
      begin
         Draw_UI_Text (C, Bounds.x + X, Bounds.y + Y, Value, Colors.text, Colors.face);
      end Text;
      use type A.Color_Scheme;
      use type A.Background;
      use type A.Placement;
   begin
      Fill_Rect (C, Bounds, Colors.face);
      Text (20, 18, "Appearance");
      Text (280, 32, "Alloy theme");
      Text (280, 104, "Background");
      Text (280, 176, "Wallpaper placement");
      if not Is_Empty (P) then
         Desktop_Wallpaper.Paint
           (C.addr + Storage_Offset (Preview.y * C.pitch + Preview.x * 4),
            Preview.w, Preview.h, C.pitch,
            P.x - Preview.x, P.y - Preview.y, P.w, P.h, View.Pending);
      end if;
      Stroke_Rect (C, Preview, Colors.shadow, Colors.highlight);
      declare
         Window : constant Rect := (Preview.x + 38, Preview.y + 32, 160, 94);
      begin
         Fill_Rect (C, Window, Preview_Colors.face);
         Stroke_Rect (C, Window, Preview_Colors.highlight, Preview_Colors.shadow);
         Fill_Vertical_Gradient (C, (Window.x + 3, Window.y + 3, 154, 20),
           Preview_Colors.activeTitleTop, Preview_Colors.activeTitleBottom);
         Draw_UI_Text (C, Window.x + 8, Window.y + 5, "Alloy", 16#FFFFFF#,
           Preview_Colors.activeTitleTop);
         Draw_Button (C, (Window.x + 24, Window.y + 42, 110, 28),
           Preview_Colors, Button_Normal, "Preview");
      end;
      Text (20, 218, "Preview");
      Text (20, 262, "Changes apply to this desktop and shared-toolkit applications.");
      Text (20, 286, "Apply reloads theme CCL from Config. No restart needed.");
      for Item in Control range Light .. Revert loop
         Draw_Button (C, Box (Item, Bounds), Colors,
           (if Item = View.Pressed and Item = View.Hovered then Button_Pressed
            elsif Selected (Item) then Button_Active
            elsif Item = View.Hovered then Button_Hot else Button_Normal), Label (Item));
         if Item = View.Focused then
            declare B : constant Rect := Box (Item, Bounds); begin
               Stroke_Rect (C, (B.x + 3, B.y + 3, B.w - 6, B.h - 6),
                 Colors.shadow, Colors.shadow);
            end;
         end if;
      end loop;
      if View.Pending /= View.Applied then
         Text (20, 340, "Unapplied changes");
      elsif View.Status = Saved_In_Config then
         Text (20, 340, "Applied / stored in Config");
      elsif View.Status = Session_Only then
         Text (20, 340, "Applied / Config unavailable");
      else
         Text (20, 340, "Tab to navigate; Enter to choose");
      end if;
   end Draw;

   procedure Activate (View : in out State; Item : Control; Apply_Requested : out Boolean) is
   begin
      Apply_Requested := False;
      case Item is
         when Light => View.Pending.Scheme := A.Alloy_Light;
         when Dark => View.Pending.Scheme := A.Alloy_Dark;
         when Image => View.Pending.Backdrop := A.Wallpaper;
         when Cubie => View.Pending.Backdrop := A.Cubie;
         when Slate => View.Pending.Backdrop := A.Slate;
         when Ocean => View.Pending.Backdrop := A.Ocean;
         when Fill => View.Pending.Position := A.Fill;
         when Fit => View.Pending.Position := A.Fit;
         when Center => View.Pending.Position := A.Center;
         when Apply => Apply_Requested := True;
         when Revert => View.Pending := View.Applied;
         when None => null;
      end case;
   end Activate;

   procedure Pointer
     (View : in out State; Bounds : Rect;
      Pointer : Pointer_State; Apply_Requested : out Boolean)
   is
      Hit : Control := None;
   begin
      Apply_Requested := False;
      for Item in Control range Light .. Revert loop
         if Point_In_Rect (Pointer.x, Pointer.y, Box (Item, Bounds)) and then
           Point_In_Rect (Pointer.x, Pointer.y, Bounds)
         then Hit := Item; end if;
      end loop;
      View.Hovered := Hit;
      if Pointer.pressed then
         View.Pressed := Hit;
         if Hit /= None then View.Focused := Hit; end if;
      elsif Pointer.released then
         if View.Pressed = Hit then Activate (View, Hit, Apply_Requested); end if;
         View.Pressed := None;
      end if;
   end Pointer;

   procedure Key
     (View : in out State; Scan_Code : Natural; Backwards : Boolean;
      Apply_Requested : out Boolean) is
   begin
      Apply_Requested := False;
      if Scan_Code = 16#0F# then
         View.Focused :=
           (if Backwards then (if View.Focused = Light then Revert else Control'Pred (View.Focused))
            else (if View.Focused = Revert then Light else Control'Succ (View.Focused)));
      elsif Scan_Code in 16#1C# | 16#39# then
         Activate (View, View.Focused, Apply_Requested);
      end if;
   end Key;
end Desktop_Settings;
