package body Box_Distribution with SPARK_Mode is

   function Merge (Left, Right : Size_Hint) return Size_Hint is
      Minimum   : constant Extent := Natural'Max (Left.Minimum, Right.Minimum);
      Preferred : constant Extent := Natural'Max (Left.Preferred, Right.Preferred);
      Kind      : constant Policy :=
        (if Left.Kind = Expanding or else Right.Kind = Expanding then Expanding
         elsif Left.Kind = Fixed and then Right.Kind = Fixed then Fixed
         else Box_Distribution.Preferred);
   begin
      --  Two Fixed hints have one length each, so their merge has one too.
      return (Minimum => Minimum, Preferred => Preferred, Kind => Kind,
              Stretch => Natural'Max (Left.Stretch, Right.Stretch));
   end Merge;

   type Extent_Array is array (Item_Index range <>) of Extent;
   type Wide is range 0 .. 2 ** 62;

   procedure Distribute
     (Hints     : Hint_Array;
      Start     : Extent;
      Available : Extent;
      Spacing   : Extent;
      Spans     : out Span_Array;
      Status    : out Fit)
   is
      Total  : constant Natural := Required (Hints, Spacing);
      --  What the children may take beyond their minimums.
      Extra  : constant Natural := (if Total <= Available then Available - Total else 0);
      Budget : constant Natural := Total + Extra;
      Grow   : Extent_Array (Hints'Range) := [others => 0];
      --  The sum of the room each child has to grow to its preferred
      --  length, and of the expanding children's stretch factors.
      Want   : Natural := 0;
      Weight : Natural := 0;
      Given  : Natural;
   begin
      Spans := [others => (Offset => 0, Length => 0)];
      Status := (if Total <= Available then Fits else Too_Small);
      if Hints'Length = 0 then
         return;
      end if;

      for I in Hints'Range loop
         Want := Want + (Hints (I).Preferred - Hints (I).Minimum);
         if Hints (I).Kind = Expanding then
            Weight := Weight + Hints (I).Stretch;
         end if;
         pragma Loop_Invariant (Want <= (I - Hints'First + 1) * MAXIMUM_EXTENT);
         pragma Loop_Invariant (Weight <= (I - Hints'First + 1) * MAXIMUM_STRETCH);
      end loop;

      --  Toward the preferred lengths, in proportion to the room to grow;
      --  Given is what that took.
      declare
         Pool : constant Natural := Natural'Min (Extra, Want);
         Left : Natural := Pool;
      begin
         for I in Hints'Range loop
            declare
               Room  : constant Extent := Hints (I).Preferred - Hints (I).Minimum;
               Share : Extent := 0;
            begin
               if Want > 0 then
                  Share := Extent (Wide'Min (Wide (Pool) * Wide (Room) / Wide (Want), Wide (Room)));
               end if;
               Grow (I) := Natural'Min (Share, Left);
               Left := Left - Grow (I);
            end;
            pragma Loop_Invariant (Left <= Pool);
            pragma Loop_Invariant
              (for all J in Hints'Range => Grow (J) <= Hints (J).Preferred - Hints (J).Minimum);
         end loop;
         Given := Pool - Left;
      end;

      --  What is left, to the expanding children by stretch; the last one
      --  takes what rounding leaves.
      if Weight > 0 then
         declare
            Spare : constant Natural := Extra - Given;
            Left  : Natural := Spare;
            Seen  : Natural := 0;
         begin
            for I in Hints'Range loop
               if Hints (I).Kind = Expanding then
                  Seen := Seen + Hints (I).Stretch;
                  declare
                     Share : Natural :=
                       (if Seen >= Weight then Left
                        else Natural (Wide'Min (Wide (Spare) * Wide (Hints (I).Stretch) / Wide (Weight),
                                                Wide (Left))));
                  begin
                     Share := Natural'Min (Share, MAXIMUM_EXTENT - Grow (I));
                     Grow (I) := Grow (I) + Share;
                     Left := Left - Share;
                  end;
               end if;
               pragma Loop_Invariant (Left <= Spare);
               pragma Loop_Invariant (Seen <= (I - Hints'First + 1) * MAXIMUM_STRETCH);
               pragma Loop_Invariant
                 (for all J in Hints'Range =>
                    (if Hints (J).Kind /= Expanding then Grow (J) <= Hints (J).Preferred - Hints (J).Minimum));
            end loop;
         end;
      end if;

      --  Place them, end to end with Spacing between. Free is what the
      --  children may still take beyond their minimums; never more than
      --  Extra in all, so the last one ends within Budget.
      declare
         Cursor : Position := Start;
         Free   : Natural := Extra;
      begin
         for I in Hints'Range loop
            pragma Loop_Invariant (Free <= Extra);
            pragma Loop_Invariant (Cursor >= Start);
            pragma Loop_Invariant (Cursor - Start + Need (Hints, I, Spacing) + Free <= Budget);
            pragma Loop_Invariant (if I = Hints'First then Cursor = Start);
            pragma Loop_Invariant
              (if I > Hints'First then Cursor = Spans (I - 1).Offset + Spans (I - 1).Length + Spacing);
            pragma Loop_Invariant (Spans (Hints'First).Offset = Start or else I = Hints'First);
            pragma Loop_Invariant
              (for all J in Hints'First .. I - 1 =>
                 Spans (J).Length >= Hints (J).Minimum
                 and then (if Hints (J).Kind /= Expanding then Spans (J).Length <= Hints (J).Preferred)
                 and then (if Status = Too_Small then Spans (J).Length = Hints (J).Minimum));
            pragma Loop_Invariant
              (for all J in Hints'First .. I - 1 =>
                 (if J < I - 1 then Spans (J + 1).Offset = Spans (J).Offset + Spans (J).Length + Spacing));
            declare
               Given : constant Natural := Natural'Min (Grow (I), Free);
            begin
               Spans (I) := (Offset => Cursor, Length => Hints (I).Minimum + Given);
               Free := Free - Given;
               if I < Hints'Last then
                  Cursor := Cursor + Spans (I).Length + Spacing;
               end if;
            end;
         end loop;
      end;
   end Distribute;

   procedure Cross
     (Hint      : Size_Hint;
      Available : Extent;
      Length    : out Extent;
      Status    : out Fit)
   is
   begin
      if Hint.Minimum > Available then
         Length := Hint.Minimum;
         Status := Too_Small;
      else
         Length := (if Hint.Kind = Expanding then Available else Natural'Min (Hint.Preferred, Available));
         Status := Fits;
      end if;
   end Cross;
end Box_Distribution;
