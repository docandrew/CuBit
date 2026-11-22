with Ada.Text_IO; with CuBit.UI; with CuBit.UI.State;
with CuBit.UI.Controls; with CuBit.UI.Widgets; with CuBit.UI.Surfaces;
with CuBit.UI.Menus; with CuBit.UI.Combo_Boxes;
procedure Check is
 use CuBit.UI;
 use type Color;
 use type CuBit.UI.Controls.Control_ID;
 type Pixel_Array is array (0..599,0..799) of Color;
 Pixels : aliased Pixel_Array := [others => [others => 0]];
 C : Canvas := (addr=>Pixels'Address,width=>300,height=>200,pitch=>3200,others=><>);
 M : CuBit.UI.Controls.Control_Map;
 S : CuBit.UI.State.UI_State;
 R : Widget_Result;
 procedure Draw (Target : Canvas) is
 begin
  CuBit.UI.Controls.Clear(M);
  CuBit.UI.Widgets.Button(Target,S,M,1,(15,14,82,26),(8,8,284,38),CuBit_Classic,"Refresh",R,retainedInput=>True);
 end;
 procedure Check_Pixels (L,T,W,H : Natural; Expected : Color) is
   function Edge (Origin, N : Natural) return Natural is
     (((Origin+N)*C.densityNumerator+C.densityDenominator-1)/C.densityDenominator - (Origin*C.densityNumerator+C.densityDenominator-1)/C.densityDenominator);
   Left : constant Natural:=Edge(C.originX,L);
   Top : constant Natural:=Edge(C.originY,T);
   Right : constant Natural:=Edge(C.originX,L+W);
   Bottom : constant Natural:=Edge(C.originY,T+H);
 begin
   for Y in Pixels'Range(1) loop
    for X in Pixels'Range(2) loop
     if X>=Left and X<Right and Y>=Top and Y<Bottom then
      pragma Assert(Pixels(Y,X)=Expected);
     else
      pragma Assert(Pixels(Y,X)=0);
     end if;
    end loop;
   end loop;
 end;
begin
 for Scale in 1..3 loop
 C.densityNumerator:=(if Scale=1 then 4 elsif Scale=2 then 5 else 8);
 C.densityDenominator:=4; C.originX:=3;C.originY:=7;
 Draw(C);pragma Assert(CuBit.UI.Controls.Hit(M,50,27)=1);
 Pixels:=[others=>[others=>0]];
 Draw(With_Repair_Clip(C,(270,60,20,120)));
 pragma Assert(CuBit.UI.Controls.Hit(M,50,27)=1);
 Check_Pixels(0,0,0,0,0); -- Button is wholly outside repair: no writes.
 Draw(With_Clip(With_Repair_Clip(C,(270,60,20,120)),(40,10,20,40)));
 pragma Assert(CuBit.UI.Controls.Hit(M,50,27)=1);
 pragma Assert(CuBit.UI.Controls.Hit(M,30,27)=0);
 Draw(With_Repair_Clip(With_Clip(C,(40,10,20,40)),(270,60,20,120)));
 pragma Assert(CuBit.UI.Controls.Hit(M,50,27)=1);
 pragma Assert(CuBit.UI.Controls.Hit(M,30,27)=0);
 Check_Pixels(0,0,0,0,0);
 declare
  Repair : constant Canvas := With_Repair_Clip(C,(30,20,20,10));
  Nested : constant Canvas := With_Clip(Repair,(35,0,10,200));
  V : constant Canvas := CuBit.UI.Surfaces.View(Nested,(20,10,100,100));
 begin
  pragma Assert(Input_Rect(V,(0,0,100,100))=(15,0,10,100));
  pragma Assert(Clamp_Rect(V,(0,0,100,100))=(15,10,10,10));
  Pixels:=[others=>[others=>0]];Fill_Rect(V,(0,0,100,100),16#123456#);
  Check_Pixels(35,20,10,10,16#123456#);
 end;
 declare
  package Menus renames CuBit.UI.Menus;
  package Combos renames CuBit.UI.Combo_Boxes;
  Caption : aliased constant String := "File";
  Menu_Model : Menus.Model;
  Combo_Model : Combos.Model;
  Menu_State : Menus.Menu_State;
  Combo_State : Combos.Combo_State;
  Reference : CuBit.UI.Controls.Control_Map;
  Command : Natural; Handled,Changed : Boolean;
  procedure Draw_Extras(Target : Canvas; Combo : Boolean) is
  begin
   CuBit.UI.Controls.Clear(M);
   if Combo then
    Combos.Draw(Target,M,Combo_State,Combo_Model,1,(15,70,82,26),CuBit_Classic);
   else
    Menus.Draw(Target,M,Menu_State,Menu_Model,1,(0,0,300,24),CuBit_Classic);
   end if;
  end;
 begin
  Menu_Model.Menu_Count:=1;Menu_Model.Item_Count:=1;
  Menu_Model.Menus(1).Caption:=Caption'Unchecked_Access;
  Menu_Model.Items(1).Caption:=Caption'Unchecked_Access;
  Menu_Model.Items(1).Command:=1;
  Combo_Model.Count:=1;Combo_Model.Choices(1).Caption:=Caption'Unchecked_Access;
  Menus.Handle_Key(Menu_State,Menu_Model,Menus.Activate,Command,Handled);
  Combos.Handle_Key(Combo_State,Combo_Model,Combos.Toggle,Changed,Handled);
  for Combo in Boolean loop
   Draw_Extras(C,Combo);Reference:=M;
   Pixels:=[others=>[others=>0]];
   Draw_Extras(With_Repair_Clip(C,(270,160,20,20)),Combo);
   for Y in 0..199 loop
    for X in 0..299 loop
     pragma Assert(CuBit.UI.Controls.Hit(M,X,Y)=CuBit.UI.Controls.Hit(Reference,X,Y));
    end loop;
   end loop;
   Check_Pixels(0,0,0,0,0);
  end loop;
 end;
 Draw(With_Clip(With_Repair_Clip(C,(0,0,300,200)),(0,0,0,0)));
 pragma Assert(CuBit.UI.Controls.Hit(M,50,27)=0);
 end loop;
 Ada.Text_IO.Put_Line("PASS retained hit geometry, nested clips, translated views and pixel sentinels at 100/125/200 percent");
end Check;
