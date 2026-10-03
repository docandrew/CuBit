package body Compositor_Software_Text_Target with SPARK_Mode => Off is
   procedure Paint (S : in out R.State;
                    Target : F.Image; Capacity : F.Byte_Count; Key : R.C.Key;
                    Screen : R.G.Output; Origin : R.G.Logical_Point;
                    Damage : R.G.Physical_Rectangle; Tint : F.Word; Success : out Boolean) is
   begin
      Success := False;
      if not Supported (Target, Capacity, Screen) then return; end if;
      declare
         Pitch : constant Positive := Positive (Target.Pitch / 4);
         Pixels : R.Software.Pixels (0 .. Pitch * Natural (Target.Height) - 1)
           with Import, Address => Target.Pixels, Alignment => 1;
      begin
         R.Paint (S, Key, Screen, Origin, Damage, Pixels, Pitch, Tint, Success);
      end;
   end Paint;
end Compositor_Software_Text_Target;
