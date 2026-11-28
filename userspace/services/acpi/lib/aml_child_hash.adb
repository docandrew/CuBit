pragma Ada_2022;
with Interfaces;
package body AML_Child_Hash with SPARK_Mode is
   function Bucket
     (Scope : Natural; Part : AML_Names.Segment; Capacity : Positive)
      return Positive
   is
      use type Interfaces.Unsigned_64;
      Offset_Basis : constant Interfaces.Unsigned_64 := 14_695_981_039_346_656_037;
      Prime : constant Interfaces.Unsigned_64 := 1_099_511_628_211;
      Value : Interfaces.Unsigned_64 := Offset_Basis xor Interfaces.Unsigned_64 (Scope);
   begin
      for C of Part loop
         Value := (Value xor Interfaces.Unsigned_64 (Character'Pos (C))) * Prime;
      end loop;
      declare
         Result : constant Interfaces.Unsigned_64 :=
           Value mod Interfaces.Unsigned_64 (Capacity);
      begin
         --  The guard also makes the machine-integer conversion explicit to proof.
         if Result >= Interfaces.Unsigned_64 (Capacity) then
            return Capacity;
         end if;
         return Positive'Min (Positive (Result + 1), Capacity);
      end;
   end Bucket;
end AML_Child_Hash;
