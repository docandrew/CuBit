package body CCL.Checked_Arithmetic with
   SPARK_Mode => On
is
   type Wide_Integer is range -(2 ** 64) .. 2 ** 64;
   for Wide_Integer'Size use 128;
   subtype Narrow_Wide_Integer is Wide_Integer range
     Wide_Integer (Interfaces.Integer_64'First) ..
     Wide_Integer (Interfaces.Integer_64'Last);

   procedure Add
     (Left, Right : Interfaces.Integer_64;
      Sum         : out Interfaces.Integer_64;
      Overflow    : out Boolean)
   is
      Wide_Sum : Wide_Integer;
   begin
      Wide_Sum := Wide_Integer (Left) + Wide_Integer (Right);
      if Wide_Sum in
        Wide_Integer (Interfaces.Integer_64'First) ..
        Wide_Integer (Interfaces.Integer_64'Last)
      then
         declare
            Narrow_Sum : constant Narrow_Wide_Integer :=
              Narrow_Wide_Integer (Wide_Sum);
         begin
            Sum := Interfaces.Integer_64 (Narrow_Sum);
            Overflow := False;
         end;
      else
         Sum := 0;
         Overflow := True;
      end if;
   end Add;
end CCL.Checked_Arithmetic;
