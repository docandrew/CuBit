with CBOR;

--  Portable storage boundary, never the local IPC representation. This codec
--  encodes data only; it does not evaluate CCL or acquire resource authority.
package CCL.Objects.Persistence with SPARK_Mode is
   --  [1, schema:bytes32, cells:[[u64,u64],...], text:bytes]
   --  Shortest, definite CBOR. Schema words are big-endian in bytes32.
   --  Cells retain CCL.Objects' schema-defined preorder semantics. Text uses
   --  bytes because today's CCL Character/String is not a UTF-8 promise.
   Maximum_Encoded_Bytes : constant := 64 + Maximum_Cells * 19 + Maximum_Text_Bytes;
   subtype Encoded_Length is Natural range 0 .. Maximum_Encoded_Bytes;
   type Packet is record
      Length : Encoded_Length := 0;
      Data : CBOR.Byte_Array (1 .. Maximum_Encoded_Bytes) := [others => 0];
   end record;
   type Outcome is (Success, Invalid_Object, Invalid_Encoding);
   procedure Encode
     (Object : Image; Contract : Binding; Data : out Packet; Result : out Outcome);
   --  Failed decoding exposes no partial value. Contract is supplied by the
   --  authorized schema registry, never constructed from incoming bytes.
   procedure Decode
     (Data : CBOR.Byte_Array; Contract : Binding; Object : out Image; Result : out Outcome)
     with Post => (if Result = Success then Validate (Object, Contract)
                   else Object = Empty (Contract));
end CCL.Objects.Persistence;
