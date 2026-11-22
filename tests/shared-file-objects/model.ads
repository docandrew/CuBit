with Interfaces; use Interfaces;
with Shared_Objects;
package Model with SPARK_Mode is
   type Volume is (Memory_Volume, NVMe_Volume);
   type Identity is record
      Device : Volume;
      Inode : Natural;
   end record;
   function Hash (Key : Identity) return Unsigned_32 is
     (Unsigned_32 (Key.Inode mod 2 ** 30) * 16#9E37_79B9# +
      Unsigned_32 (Volume'Pos (Key.Device)));
   package Objects is new Shared_Objects
     (Capacity => 32, Object_Key => Identity,
      Empty_Key => (Memory_Volume, 0),
      Object_Value => Natural, Empty_Value => 0, Hash => Hash);
   --  Every key in one home slot: probe windows and Full in the extreme.
   function Same_Home (Key : Identity) return Unsigned_32 is (0 * Unsigned_32 (Key.Inode mod 2));
   package Crowded is new Shared_Objects
     (Capacity => 128, Object_Key => Identity,
      Empty_Key => (Memory_Volume, 0),
      Object_Value => Natural, Empty_Value => 0, Hash => Same_Home,
      Max_Holders => 4);
end Model;
