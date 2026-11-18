package body Compositor_Pointer_Over with SPARK_Mode is
   use Interfaces;

   function Over (Source, Frame : Word) return Word is
      subtype Channel is Natural range 0 .. 255;
      Alpha : constant Channel := Channel (Shift_Right (Source, 24) and 16#FF#);
      Inverse : constant Channel := 255 - Alpha;
      function Part (Value : Word; Shift : Natural) return Channel is
        (Channel (Shift_Right (Value, Shift) and 16#FF#))
        with Pre => Shift <= 24;
      function Mix (S, D : Channel) return Channel is
        (Natural'Min (S + (D * Inverse + 127) / 255, 255));
   begin
      if Alpha = 0 then
         return Frame;
      elsif Alpha = 255 then
         return Source and 16#00FF_FFFF#;
      end if;
      return Shift_Left (Word (Mix (Part (Source, 16), Part (Frame, 16))), 16) or
             Shift_Left (Word (Mix (Part (Source, 8), Part (Frame, 8))), 8) or
             Word (Mix (Part (Source, 0), Part (Frame, 0)));
   end Over;

   procedure Over_Row (Frame : in out Words; Source : Words; Map : Column_Map) is
   begin
      for I in Frame'Range loop
         if Map (I) /= No_Column then
            Frame (I) := Over (Source (Map (I)), Frame (I));
         end if;
         pragma Loop_Invariant
           (for all J in Frame'Range =>
              Frame (J) = (if J > I or else Map (J) = No_Column then Frame'Loop_Entry (J)
                           else Over (Source (Map (J)), Frame'Loop_Entry (J))));
      end loop;
   end Over_Row;
end Compositor_Pointer_Over;
