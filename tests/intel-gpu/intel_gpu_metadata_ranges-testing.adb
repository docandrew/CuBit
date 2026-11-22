package body Intel_GPU_Metadata_Ranges.Testing is
   function Valid (Object : Tree) return Boolean is
      Stack : array (1 .. 64) of Node_Access := [others => null];
      Depth, Seen : Natural := 0;
      Current : Node_Access := Object.Root;
      Previous_End : Unsigned_64 := 0;
      LH, RH : Natural;
   begin
      if Current /= null and then Current.Parent /= null then return False; end if;
      while Current /= null or else Depth /= 0 loop
         while Current /= null loop
            if Depth = Stack'Length then return False; end if;
            if not Current.Linked or else Current.Base = 0 or else Current.Bytes = 0 or else
              Current.Bytes > Unsigned_64'Last - Current.Base then return False; end if;
            if Current.Left /= null and then Current.Left.Parent /= Current then return False; end if;
            if Current.Right /= null and then Current.Right.Parent /= Current then return False; end if;
            LH := (if Current.Left = null then 0 else Current.Left.Level);
            RH := (if Current.Right = null then 0 else Current.Right.Level);
            if abs (Integer (LH) - Integer (RH)) > 1 or else
              Current.Level /= 1 + Natural'Max (LH, RH) then return False; end if;
            Depth := Depth + 1; Stack (Depth) := Current; Current := Current.Left;
         end loop;
         Current := Stack (Depth); Depth := Depth - 1;
         if Seen = Object.Size or else Current.Base < Previous_End then return False; end if;
         Seen := Seen + 1; Previous_End := Current.Base + Current.Bytes;
         Current := Current.Right;
      end loop;
      return Seen = Object.Size;
   end Valid;
end Intel_GPU_Metadata_Ranges.Testing;
