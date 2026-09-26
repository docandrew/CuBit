with CBOR;

--  Portable data-only type metadata, not executable CCL or authority.
--  [1, key:bytes32, root:uint, definitions:[[name:bytes, form:uint,
--      parts:[[name:bytes, payload:uint],...]],...]]
--  Native wire type IDs remain explicit; schema words are big-endian.
--  Names use bytes like native CCL strings (no implied Unicode normalization).
package CCL.Objects.Schemas.Persistence with SPARK_Mode is
   Maximum_Encoded_Bytes : constant :=
     64 + Types.Maximum_Declarations * (64 + Types.Maximum_Components * 48);
   subtype Encoded_Length is Natural range 0 .. Maximum_Encoded_Bytes;
   type Packet is record
      Length : Encoded_Length := 0;
      Data : CBOR.Byte_Array (1 .. Maximum_Encoded_Bytes) := [others => 0];
   end record;
   type Outcome is (Success, Invalid_Binding, Invalid_Encoding);
   procedure Encode (Contract : Binding; Data : out Packet; Result : out Outcome);
   --  Decode validates declaration/layout rules via the shared native schema
   --  importer. Success is NOT permission to install this binding; source,
   --  namespace authority and same-key conflicts are the caller's obligations.
   --  Rejects noncanonical encodings and exposes no partial binding on failure.
   procedure Decode (Data : CBOR.Byte_Array; Contract : out Binding; Result : out Outcome);
end CCL.Objects.Schemas.Persistence;
