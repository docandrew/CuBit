package body Text_Fit with SPARK_Mode is
   function Kept (Text : String; Available : Natural) return Natural is
      Count : Natural;
   begin
      if Text'Length = 0 or else Width (Text) <= Available then
         return Text'Length;
      end if;
      Count := Natural'Min (Hint (Text, Available), Text'Length - 1);
      if Prefix_Fits (Text, Count, Available) then
         --  Grow while one more character still fits.
         while Count + 1 < Text'Length and then Prefix_Fits (Text, Count + 1, Available) loop
            pragma Loop_Invariant (Count + 1 < Text'Length);
            pragma Loop_Invariant (Prefix_Fits (Text, Count + 1, Available));
            pragma Loop_Variant (Increases => Count);
            Count := Count + 1;
         end loop;
         return Count;
      end if;
      --  Shrink until a prefix fits; the one above it did not.
      while Count > 0 loop
         pragma Loop_Invariant (Count < Text'Length);
         pragma Loop_Invariant (not Prefix_Fits (Text, Count, Available));
         pragma Loop_Variant (Decreases => Count);
         if Prefix_Fits (Text, Count - 1, Available) then
            return Count - 1;
         end if;
         Count := Count - 1;
      end loop;
      return 0;
   end Kept;

   function Fitted (Text : String; Available : Natural) return String is
      Count : Natural;
   begin
      if Text'Length = 0 or else Width (Text) <= Available then
         return Text;
      end if;
      Count := Kept (Text, Available);
      if Prefix_Fits (Text, Count, Available) then
         return Text (Text'First .. Text'First - 1 + Count) & Ellipsis;
      end if;
      return "";
   end Fitted;

   function Kept_Tail (Text : String; Available : Natural) return Natural is
      Count : Natural;
      Total : Natural;
   begin
      if Text'Length = 0 or else Width (Text) <= Available then
         return Text'Length;
      end if;
      --  Guess: drop as many characters as overflow; the loops settle it.
      Total := Width (Text);
      Count := Text'Length - Natural'Min
        (Hint (Text, Total - Available), Text'Length);
      Count := Natural'Min (Count, Text'Length - 1);
      if Suffix_Fits (Text, Count, Available) then
         while Count + 1 < Text'Length and then Suffix_Fits (Text, Count + 1, Available) loop
            pragma Loop_Invariant (Count + 1 < Text'Length);
            pragma Loop_Invariant (Suffix_Fits (Text, Count + 1, Available));
            pragma Loop_Variant (Increases => Count);
            Count := Count + 1;
         end loop;
         return Count;
      end if;
      while Count > 0 loop
         pragma Loop_Invariant (Count < Text'Length);
         pragma Loop_Invariant (not Suffix_Fits (Text, Count, Available));
         pragma Loop_Variant (Decreases => Count);
         if Suffix_Fits (Text, Count - 1, Available) then
            return Count - 1;
         end if;
         Count := Count - 1;
      end loop;
      return 0;
   end Kept_Tail;

   function Fitted_Tail (Text : String; Available : Natural) return String is
      Count : Natural;
   begin
      if Text'Length = 0 or else Width (Text) <= Available then
         return Text;
      end if;
      Count := Kept_Tail (Text, Available);
      if Suffix_Fits (Text, Count, Available) then
         return Ellipsis & Text (Text'Last + 1 - Count .. Text'Last);
      end if;
      return "";
   end Fitted_Tail;
end Text_Fit;
