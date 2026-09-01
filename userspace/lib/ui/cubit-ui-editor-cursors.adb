package body CuBit.UI.Editor.Cursors with SPARK_Mode is
   procedure Initialize
     (Cursors : out Cursor_Set; Position : Cursor_Position := 1)
   is
   begin
      Cursors := (others => <>);
      Cursors.Items (1) :=
        (Position => Position, Anchor => Position, Preferred_Column => 1);
   end Initialize;

   function Length (Cursors : Cursor_Set) return Cursor_Count is
     (Cursors.Last);

   function Primary_Index (Cursors : Cursor_Set) return Cursor_Index is
     (Cursors.Primary);

   function Element
     (Cursors : Cursor_Set; Index : Cursor_Index) return Cursor_State is
     (Cursors.Items (Index));

   procedure Set_Element
     (Cursors : in out Cursor_Set; Index : Cursor_Index;
      Value : Cursor_State)
   is
   begin
      Cursors.Items (Index) := Value;
   end Set_Element;

   procedure Toggle_At
     (Cursors : in out Cursor_Set; Position : Cursor_Position;
      Result : out Toggle_Result)
   is
      Match : Natural := 0;
   begin
      for Index in 1 .. Cursors.Last loop
         pragma Loop_Invariant (Match <= Cursors.Last);
         if Cursors.Items (Index).Position = Position and then
           Cursors.Items (Index).Anchor = Position
         then
            Match := Index;
            exit;
         end if;
      end loop;

      if Match = 0 then
         if Cursors.Last = MAX_CURSORS then
            Result := Cursor_Limit_Reached;
         else
            Cursors.Last := Cursors.Last + 1;
            Cursors.Items (Cursors.Last) :=
              (Position => Position, Anchor => Position,
               Preferred_Column => 1);
            Cursors.Primary := Cursors.Last;
            Result := Cursor_Added;
         end if;
      elsif Cursors.Last = 1 then
         Cursors.Items (1) :=
           (Position => Position, Anchor => Position,
            Preferred_Column => 1);
         Cursors.Primary := 1;
         Result := Primary_Moved;
      else
         for Index in Cursor_Index (Match) .. Cursors.Last - 1 loop
            Cursors.Items (Index) := Cursors.Items (Index + 1);
         end loop;
         Cursors.Last := Cursors.Last - 1;
         if Cursors.Primary > Cursors.Last then
            Cursors.Primary := Cursors.Last;
         elsif Cursors.Primary > Match then
            Cursors.Primary := Cursors.Primary - 1;
         elsif Cursors.Primary = Match then
            Cursors.Primary := Cursor_Index'Min
              (Cursor_Index (Match), Cursors.Last);
         end if;
         Result := Cursor_Removed;
      end if;
   end Toggle_At;

   procedure Add_At
     (Cursors : in out Cursor_Set; Position : Cursor_Position;
      Preferred_Column : CuBit.UI.Editor.Documents.Display_Column;
      Result : out Add_Result)
   is
   begin
      for Index in 1 .. Cursors.Last loop
         if Cursors.Items (Index).Position = Position and then
           Cursors.Items (Index).Anchor = Position
         then
            Result := Cursor_Already_Present;
            return;
         end if;
      end loop;

      if Cursors.Last = MAX_CURSORS then
         Result := Cursor_Limit_Reached;
         return;
      end if;

      Cursors.Last := Cursors.Last + 1;
      Cursors.Items (Cursors.Last) :=
        (Position => Position, Anchor => Position,
         Preferred_Column => Preferred_Column);
      Cursors.Primary := Cursors.Last;
      Result := Cursor_Added;
   end Add_At;

   procedure Coalesce (Cursors : in out Cursor_Set) is
      Left_Index : Cursor_Index := 1;
      Right_Index : Natural;
      Left_First, Left_Last, Right_First, Right_Last : Cursor_Position;
      Forward : Boolean;
   begin
      while Left_Index < Cursors.Last loop
         pragma Loop_Invariant (Left_Index <= Cursors.Last);
         pragma Loop_Invariant (Cursors.Primary <= Cursors.Last);
         Right_Index := Left_Index + 1;
         while Right_Index <= Cursors.Last loop
            pragma Loop_Invariant (Left_Index < Right_Index);
            pragma Loop_Invariant (Right_Index <= Cursors.Last + 1);
            pragma Loop_Invariant (Cursors.Primary <= Cursors.Last);
            Left_First := Cursor_Position'Min
              (Cursors.Items (Left_Index).Position,
               Cursors.Items (Left_Index).Anchor);
            Left_Last := Cursor_Position'Max
              (Cursors.Items (Left_Index).Position,
               Cursors.Items (Left_Index).Anchor);
            Right_First := Cursor_Position'Min
              (Cursors.Items (Cursor_Index (Right_Index)).Position,
               Cursors.Items (Cursor_Index (Right_Index)).Anchor);
            Right_Last := Cursor_Position'Max
              (Cursors.Items (Cursor_Index (Right_Index)).Position,
               Cursors.Items (Cursor_Index (Right_Index)).Anchor);

            if Left_First <= Right_Last and then
              Right_First <= Left_Last
            then
               Forward := Cursors.Items (Left_Index).Position >=
                 Cursors.Items (Left_Index).Anchor;
               Left_First := Cursor_Position'Min (Left_First, Right_First);
               Left_Last := Cursor_Position'Max (Left_Last, Right_Last);
               Cursors.Items (Left_Index).Position :=
                 (if Forward then Left_Last else Left_First);
               Cursors.Items (Left_Index).Anchor :=
                 (if Forward then Left_First else Left_Last);

               for Index in Cursor_Index (Right_Index) .. Cursors.Last - 1 loop
                  Cursors.Items (Index) := Cursors.Items (Index + 1);
               end loop;
               Cursors.Last := Cursors.Last - 1;
               if Cursors.Primary = Right_Index then
                  Cursors.Primary := Left_Index;
               elsif Cursors.Primary > Right_Index then
                  Cursors.Primary := Cursors.Primary - 1;
               end if;
               pragma Assert (Cursors.Primary <= Cursors.Last);
            else
               Right_Index := Right_Index + 1;
            end if;
         end loop;
         exit when Left_Index = Cursors.Last;
         Left_Index := Left_Index + 1;
      end loop;
   end Coalesce;
end CuBit.UI.Editor.Cursors;
