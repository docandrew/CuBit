package body CCL.List_Operations with SPARK_Mode => On is

   procedure Take_Bounds
     (Item : Operation; N : Integer_64; Length : Natural;
      From : out Positive; To : out Natural)
   is
      Count : constant Natural :=
        (if N <= 0 then 0
         elsif N >= Integer_64 (Length) then Length
         else Natural (N));
   begin
      case Item is
         when First_Items => From := 1; To := Count;
         when Last_Items => From := Length - Count + 1; To := Length;
         when others => From := Count + 1; To := Length;
      end case;
   end Take_Bounds;

   procedure Range_Length
     (Low, High : Integer_64; Capacity : Natural;
      Size : out Natural; Fits : out Boolean)
   is
   begin
      Size := 0;
      Fits := True;
      if High < Low then
         return;
      end if;
      --  High - Low cannot overflow when Low >= 0 or High < 0; otherwise the
      --  span exceeds Integer_64'Last, far beyond any capacity.
      if (Low < 0 and then High >= 0) and then High > Integer_64'Last + Low then
         Fits := False;
      elsif High - Low >= Integer_64 (Capacity) then
         Fits := False;
      else
         Size := Natural (High - Low) + 1;
      end if;
   end Range_Length;

   procedure Heap_Sort (Length : Natural; Good : out Boolean) is
      --  Restore the max-heap below Start within 1 .. Last.
      procedure Sift (Start, Last : Positive)
      with Pre => Last <= Length;
      procedure Sift (Start, Last : Positive) is
         Root : Positive := Start;
         Child : Positive;
         Before : Boolean;
      begin
         while Good and then Root <= Last / 2 loop
            pragma Loop_Variant (Increases => Root);
            Child := 2 * Root;
            if Child < Last then
               Less (Child, Child + 1, Before, Good);
               exit when not Good;
               if Before then Child := Child + 1; end if;
            end if;
            Less (Root, Child, Before, Good);
            exit when not Good or else not Before;
            Swap (Root, Child, Good);
            Root := Child;
         end loop;
      end Sift;
   begin
      Good := True;
      if Length < 2 then
         return;
      end if;
      for Root in reverse 1 .. Length / 2 loop
         Sift (Root, Length);
         exit when not Good;
      end loop;
      for Last in reverse 2 .. Length loop
         exit when not Good;
         Swap (1, Last, Good);
         exit when not Good;
         if Last > 2 then Sift (1, Last - 1); end if;
      end loop;
   end Heap_Sort;
end CCL.List_Operations;
