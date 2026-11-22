--  Hosted test for CuBit.UI.Focus (keyboard focus order).
with Ada.Text_IO;
with CuBit.UI.Focus;
with CuAlloc_Host;
pragma Unreferenced (CuAlloc_Host);

procedure Main is
begin
   --  Keyboard focus (CuBit.UI.Focus): Tab order is registration order.
   declare
      package F renames CuBit.UI.Focus;
      Focus : F.Focus_State;
   begin
      F.Begin_Order (Focus);
      F.Register (Focus, 5);
      F.Register (Focus, 9);
      F.Register (Focus, 9);   --  registered once
      F.Register (Focus, 2);
      F.End_Order (Focus);
      pragma Assert (Focus.Count = 3 and then Focus.Focused = F.No_Control);
      F.Move (Focus, Forward => True);
      pragma Assert (F.Is_Focused (Focus, 5));
      F.Move (Focus, Forward => False);
      pragma Assert (F.Is_Focused (Focus, 2), "Shift+Tab wraps to the last");
      F.Move (Focus, Forward => True);
      pragma Assert (F.Is_Focused (Focus, 5), "Tab wraps to the first");
      F.Set (Focus, 9);
      pragma Assert (F.Is_Focused (Focus, 9));
      F.Set (Focus, 77);
      pragma Assert (F.Is_Focused (Focus, 9), "an unregistered control takes no focus");
      --  The focused control is no longer drawn: its successor takes it.
      F.Begin_Order (Focus);
      F.Register (Focus, 5);
      F.Register (Focus, 2);
      F.End_Order (Focus);
      pragma Assert (F.Is_Focused (Focus, 2));
      F.Begin_Order (Focus);
      F.End_Order (Focus);
      pragma Assert (Focus.Focused = F.No_Control, "nothing drawn: no focus");
      F.Move (Focus, Forward => True);
      pragma Assert (Focus.Focused = F.No_Control);
   end;
   Ada.Text_IO.Put_Line ("PASS: keyboard focus order");
end Main;
