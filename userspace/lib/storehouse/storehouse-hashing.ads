private with SPARKTLSCrypto.Hashing.SHA256;

--  SHA-256 identities through the verified SPARK stack (SPARKTLSCrypto over
--  SPARKNaCl), streamed so a file of any size is hashed in pieces.
package Storehouse.Hashing with SPARK_Mode is

   type Context is private;

   procedure Start (Hash : out Context) with Global => null, Always_Terminates;
   procedure Add (Hash : in out Context; Data : Byte_Array) with Global => null, Always_Terminates;
   procedure Finish (Hash : in out Context; Result : out Digest) with Global => null, Always_Terminates;

   function Of_Bytes (Data : Byte_Array) return Digest with Global => null;

private
   type Context is record
      State : SPARKTLSCrypto.Hashing.SHA256.Context;
   end record;
end Storehouse.Hashing;
