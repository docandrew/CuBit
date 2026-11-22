pragma Ada_2022;
with CuBit.Memory_Grants;
package body I2C_Native_Grant_Copy is
   package G renames CuBit.Memory_Grants;
   procedure Acquire
     (Reference : CuBit.Grant_References.Reference;
      Owner : CuBit.Process_IDs.Process_ID; Length : Interfaces.Unsigned_64;
      Writable : Boolean; Address : out System.Address; Acquired : out Boolean) is
   begin
      G.Acquire (reference => Reference, expectedOwner => Owner,
         byteOffset => 0, byteLength => Length,
         requiredAccess => (if Writable then G.Write_Access else G.Read_Access),
         mappedAddress => Address, success => Acquired);
   end Acquire;
   procedure Release
     (Reference : CuBit.Grant_References.Reference; Returned : out Boolean) is
   begin
      G.Return_Acquisition (Reference, Returned);
   end Release;
end I2C_Native_Grant_Copy;
