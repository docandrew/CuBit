--  An instance for gnatprove (Publisher_Slots is generic).
pragma Ada_2022;
with Interfaces; use Interfaces;
with Publisher_Slots;
package Slots_Proof with SPARK_Mode is
   type Views is array (1 .. 4096) of Boolean;
   Live_View : Views := [others => False];
   Used : Natural range 0 .. 4096 := 0;
   function Count return Natural is (Used);
   function Live (I : Positive) return Boolean is (if I <= 4096 then Live_View (I) else False);
   function Owner (I : Positive) return Unsigned_64 is (Unsigned_64 (I));
   function Authority (I : Positive) return Unsigned_64 is (if I > 0 then 1 else 0);
   package Slots is new Publisher_Slots (Count, Live, Owner, Authority);
end Slots_Proof;
