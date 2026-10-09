with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.State;
with CuBit.UI.Controls;
with CuBit.UI.Menus;
procedure Main is
   package App renames CuBit.UI.App;
   package Controls renames CuBit.UI.Controls;
   package Menus renames CuBit.UI.Menus;
   use type Controls.Control_ID;
   Win : App.Window;
   UI : CuBit.UI.State.UI_State;
   Map : Controls.Control_Map;
   Menu_State : Menus.Menu_State;
   Definition : Menus.Model;
   File_Text : aliased constant String := "File";
   First_Text : aliased constant String := "First action";
   Second_Text : aliased constant String := "Second action";
   Ready, Pending_Repair : Boolean := False;
   Ignore : Unsigned_64;
   procedure Check (OK : Boolean; Text : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL native-menu " & Text & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
      end if;
   end Check;
   procedure Render (Win : in out App.Window; Damage : Rect) is
      C : constant Canvas := App.Canvas (Win, Damage);
   begin
      CuBit.UI.State.Begin_Frame (UI);
      Controls.Clear (Map);
      Fill_Rect (C, App.Full_Rect (Win), CuBit_Classic.face);
      Fill_Rect (C, (300,180,8,8), 16#33AA66#);
      Menus.Draw (C, Map, Menu_State, Definition, 1,
                  (0,0,C.width,24), CuBit_Classic);
      if Pending_Repair and then Menus.Is_Open (Menu_State) then
         Check (Controls.Hit (Map,20,40)=Menus.Item_ID (1,1), "lost item after repair");
         if Damage.w <= 8 and then Damage.h <= 8 then
            debugPrint ("TEST: native-menu partial repair retained item" & ASCII.LF);
            Pending_Repair := False;
         end if;
      end if;
      CuBit.UI.State.Finish_Frame (UI);
      if not Ready then
         Ready := True;
         debugPrint ("TEST: native-menu ready" & ASCII.LF);
      end if;
   end Render;
   procedure Handle (Win : in out App.Window; Event : App.Input_Event;
                     Dirty : in out Rect; Running : in out Boolean) is
      Command : Natural := 0;
      Handled : Boolean := False;
      Action : Controls.Pointer_Action;
      Target : Controls.Control_ID;
      Was_Open : constant Boolean := Menus.Is_Open (Menu_State);
   begin
      if Event.kind = App.INPUT_KEY_DOWN then
         if Event.payload0 = 16#40# then -- F6: independent small content repair.
            Check (Was_Open, "repair requested without menu");
            Pending_Repair := True;
            Dirty := Union_Rect (Dirty, (300,180,8,8));
            return;
         elsif Event.payload0 = 16#44# then
            Menus.Handle_Key (Menu_State,Definition,Menus.Activate,Command,Handled);
         elsif Event.payload0 = 16#50# then
            Menus.Handle_Key (Menu_State,Definition,Menus.Down,Command,Handled);
         elsif Event.payload0 = 16#1C# then
            Menus.Handle_Key (Menu_State,Definition,Menus.Enter,Command,Handled);
         elsif Event.payload0 = App.KEY_ESC then
            Menus.Handle_Key (Menu_State,Definition,Menus.Escape,Command,Handled);
         elsif Event.payload0 = App.KEY_Q then Running := False;
         end if;
      elsif Event.kind in App.INPUT_POINTER_MOVE | App.INPUT_POINTER_DOWN | App.INPUT_POINTER_UP then
         Target := Controls.Hit (Map,Natural(Event.payload0 and 16#FFFFFFFF#),
                                      Natural(Shift_Right(Event.payload0,32)));
         Action := (if Event.kind=App.INPUT_POINTER_DOWN then Controls.Pointer_Press
                    elsif Event.kind=App.INPUT_POINTER_UP then Controls.Pointer_Release
                    else Controls.Pointer_Move);
         -- Run already applied capture. The fixture contains no underlying controls,
         -- so outside-menu presses cannot capture or activate application content.
         Menus.Handle_Pointer (Menu_State,Definition,Map,1,Target,Action,Command,Handled);
      end if;
      if Handled then Dirty := App.Full_Rect (Win); end if;
      if not Was_Open and then Menus.Is_Open (Menu_State) then
         debugPrint ("TEST: native-menu opened" & ASCII.LF);
      end if;
      if Command /= 0 then
         debugPrint ("TEST: native-menu command=" & Command'Image & ASCII.LF);
      end if;
   end Handle;
   procedure Run is new App.Run (UI,Map,Render=>Render,Handle_Event=>Handle);
   OK : Boolean;
begin
   Definition.Menu_Count:=1;Definition.Item_Count:=2;
   Definition.Menus(1):=(File_Text'Unchecked_Access,'F');
   Definition.Items(1):=(Caption=>First_Text'Unchecked_Access,Command=>101,others=><>);
   Definition.Items(2):=(Caption=>Second_Text'Unchecked_Access,Command=>202,others=><>);
   App.Open (Win,420,240,App.WINDOW_FLAG_DECORATED or App.WINDOW_FLAG_CLOSEABLE,
             OK,title=>"Native menu regression",protected_frames=>True);
   Check (OK,"window open");Run(Win);App.Close(Win);
   Ignore:=syscall(SYSCALL_EXIT,0);
end Main;
