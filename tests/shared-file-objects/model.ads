with Shared_Objects;
package Model with SPARK_Mode is
   type Volume is (Memory_Volume, NVMe_Volume);
   type Identity is record
      Device : Volume;
      Inode : Natural;
   end record;
   package Objects is new Shared_Objects
     (Capacity => 32, Object_Key => Identity,
      Empty_Key => (Memory_Volume, 0),
      Object_Value => Natural, Empty_Value => 0);
end Model;
