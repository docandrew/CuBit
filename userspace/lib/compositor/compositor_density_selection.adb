package body Compositor_Density_Selection with SPARK_Mode is
   function Intersects (Screen : G.Output; Window : G.Logical_Rectangle) return Boolean is
      B : constant G.Logical_Rectangle := G.Bounds (Screen);
   begin
      return Window.Left < Window.Right and then Window.Top < Window.Bottom and then
        Window.Left < B.Right and then B.Left < Window.Right and then
        Window.Top < B.Bottom and then B.Top < Window.Bottom;
   end Intersects;
   function Choose
     (Screens : Outputs; Count, Primary : Output_Index;
      Window : G.Logical_Rectangle) return Output_Index
   is
      Winner : Output_Index := Primary;
      Found : Boolean := False;
   begin
      for I in 1 .. Count loop
         if Intersects (Screens (I), Window) then
            if not Found or else Rank (Screens (I).Scale) > Rank (Screens (Winner).Scale) then
               Winner := I;
            end if;
            Found := True;
         end if;
         pragma Loop_Invariant (Winner <= Count);
         pragma Loop_Invariant
           (Found = (for some J in 1 .. I => Intersects (Screens (J), Window)));
         pragma Loop_Invariant
           (if Found then Intersects (Screens (Winner), Window) else Winner = Primary);
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              (if Intersects (Screens (J), Window) then
                 Rank (Screens (Winner).Scale) >= Rank (Screens (J).Scale)));
      end loop;
      return Winner;
   end Choose;
end Compositor_Density_Selection;
