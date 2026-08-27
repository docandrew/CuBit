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
end CuBit.UI.Editor.Cursors;
