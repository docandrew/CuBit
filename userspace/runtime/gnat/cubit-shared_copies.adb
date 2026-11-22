pragma Ada_2022;
package body CuBit.Shared_Copies with SPARK_Mode => Off is
   procedure Copy_With (Into : out Block) is
   begin
      for I in Into'Range loop
         Into (I) := Read (I);
      end loop;
   end Copy_With;

   procedure Copy_At (Location : System.Address; Into : out Block) is
      type Shared_Block is array (Into'Range) of Element with Volatile_Components;
      Shared : Shared_Block with Import, Address => Location;
      function Read (I : Index) return Element is (Shared (I));
      procedure Copy is new Copy_With (Read);
   begin
      Copy (Into);
   end Copy_At;
end CuBit.Shared_Copies;
