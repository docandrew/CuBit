pragma Ada_2022;
package body CuBit.Display_Layouts with SPARK_Mode is
   use type G.Logical_Coordinate;

   function Valid_Primary (Ready : Layout; Choice : Primary_Selection)
      return Boolean is
     (Choice.Available = (Ready.Count > 0) and then
      (if Choice.Available then
         Choice.Index <= Ready.Count and then
         Choice.Display = Ready.Items (Choice.Index).Display));

   function Select_Primary
     (Ready : Layout; Preferred : Named_Display_ID;
      Previous : Primary_Selection := (Available => False);
      Policy : Primary_Update := Preserve_Usable_Primary)
      return Primary_Selection
   is
      Preferred_Index, Previous_Index : Viewport_Count := 0;
      Selected : Viewport_Index := 1;
   begin
      if Ready.Count = 0 then
         return (Available => False);
      end if;
      for I in 1 .. Ready.Count loop
         if Ready.Items (I).Display = Preferred then
            Preferred_Index := I;
         end if;
         --  Resolve identity afresh; Previous.Index belongs to an old layout.
         if Previous.Available and then
           Ready.Items (I).Display = Previous.Display
         then
            Previous_Index := I;
         end if;
         if Ready.Items (I).Display < Ready.Items (Selected).Display then
            Selected := I;
         end if;
         pragma Loop_Invariant (Selected <= I);
         pragma Loop_Invariant (Preferred_Index <= I);
         pragma Loop_Invariant (Previous_Index <= I);
      end loop;
      if Policy = Preserve_Usable_Primary and then Previous_Index /= 0 then
         Selected := Previous_Index;
      elsif Preferred_Index /= 0 then
         Selected := Preferred_Index;
      end if;
      return (Available => True, Index => Selected,
              Display => Ready.Items (Selected).Display);
   end Select_Primary;

   function Overlap (Left, Right : G.Logical_Rectangle) return Boolean is
     (G.Logical_Coordinate'Max (Left.Left, Right.Left) <
        G.Logical_Coordinate'Min (Left.Right, Right.Right) and then
      G.Logical_Coordinate'Max (Left.Top, Right.Top) <
        G.Logical_Coordinate'Min (Left.Bottom, Right.Bottom));

   function Adjacent (Left, Right : G.Logical_Rectangle) return Boolean is
     (Left.Left < Left.Right and then Left.Top < Left.Bottom and then
      Right.Left < Right.Right and then Right.Top < Right.Bottom and then
      (((Left.Right = Right.Left or else Right.Right = Left.Left) and then
        G.Logical_Coordinate'Max (Left.Top, Right.Top) <
          G.Logical_Coordinate'Min (Left.Bottom, Right.Bottom)) or else
       ((Left.Bottom = Right.Top or else Right.Bottom = Left.Top) and then
        G.Logical_Coordinate'Max (Left.Left, Right.Left) <
          G.Logical_Coordinate'Min (Left.Right, Right.Right))));

   function Pairwise_Valid
     (Candidate : Layout; Through : Viewport_Count) return Boolean is
     (for all I in 1 .. Through =>
        (for all J in 1 .. I - 1 =>
           Candidate.Items (I).Display /= Candidate.Items (J).Display
           and then not Overlap (G.Bounds (Candidate.Items (I).Geometry),
                                 G.Bounds (Candidate.Items (J).Geometry))));

   function Valid_Tree (Candidate : Layout; Tree : Connection_Tree)
     return Boolean is
     (Candidate.Count > 0 and then
      Tree (1).Parent = 1 and then Tree (1).Depth = 1 and then
      (for all I in 1 .. Candidate.Count =>
         (if I /= 1 and then Tree (I).Depth > 0 then
            Tree (I).Parent <= Candidate.Count and then
            Tree (Tree (I).Parent).Depth > 0 and then
            Tree (Tree (I).Parent).Depth < Tree (I).Depth and then
            Adjacent (G.Bounds (Candidate.Items (I).Geometry),
              G.Bounds (Candidate.Items (Tree (I).Parent).Geometry)))));

   function Complete_Tree (Candidate : Layout; Tree : Connection_Tree)
     return Boolean is
     (Valid_Tree (Candidate, Tree) and then
      (for all I in 1 .. Candidate.Count => Tree (I).Depth > 0));

   function Validate
     (Candidate : Layout;
      Policy : Empty_Layout_Policy := Require_Interactive_Output)
      return Validation_Result
   is
      Result : Validation_Result;
   begin
      if Candidate.Count = 0 then
         Result.Status := (if Policy = Permit_Headless then Accepted
                           else Empty_Not_Allowed);
         return Result;
      end if;
      for I in 1 .. Candidate.Count loop
         for J in 1 .. I - 1 loop
            if Candidate.Items (I).Display = Candidate.Items (J).Display then
               Result.Status := Repeated_Display;
               Result.First := I;
               Result.Second := J;
               return Result;
            elsif Overlap (G.Bounds (Candidate.Items (I).Geometry),
                           G.Bounds (Candidate.Items (J).Geometry))
            then
               Result.Status := Overlapping_Displays;
               Result.First := I;
               Result.Second := J;
               return Result;
            end if;
            pragma Loop_Invariant
              (for all K in 1 .. J =>
                 Candidate.Items (I).Display /= Candidate.Items (K).Display
                 and then not Overlap
                   (G.Bounds (Candidate.Items (I).Geometry),
                    G.Bounds (Candidate.Items (K).Geometry)));
         end loop;
         pragma Loop_Invariant (Pairwise_Valid (Candidate, I));
      end loop;

      Result.Tree (1) := (Parent => 1, Depth => 1);
      --  At most Count-1 edges are needed in a simple connecting path.
      --  Each pass uses only earlier depths, independent of detection order.
      for Depth in 2 .. Candidate.Count loop
         pragma Loop_Invariant (Valid_Tree (Candidate, Result.Tree));
         for Child in 1 .. Candidate.Count loop
            pragma Loop_Invariant (Valid_Tree (Candidate, Result.Tree));
            if Result.Tree (Child).Depth = 0 then
               for Parent in 1 .. Candidate.Count loop
                  pragma Loop_Invariant
                    (Valid_Tree (Candidate, Result.Tree));
                  if Result.Tree (Parent).Depth > 0 and then
                    Result.Tree (Parent).Depth < Depth and then
                    Adjacent (G.Bounds (Candidate.Items (Child).Geometry),
                              G.Bounds (Candidate.Items (Parent).Geometry))
                  then
                     Result.Tree (Child) := (Parent, Depth);
                     exit;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
      for I in 1 .. Candidate.Count loop
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 => Result.Tree (J).Depth > 0);
         if Result.Tree (I).Depth = 0 then
            Result.First := I;
            return Result;
         end if;
      end loop;
      Result.Status := Accepted;
      return Result;
   end Validate;
end CuBit.Display_Layouts;
