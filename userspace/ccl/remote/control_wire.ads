with Interfaces;
with CBOR;
with CCL.Control;

--  Bounded lab protocol. Shape validation grants no authority.
package Control_Wire with SPARK_Mode is
   Max_Source : constant := 1024;
   Max_Request : constant := 1100;
   Max_Response : constant := 8192;
   type Request is record
      Id : Interfaces.Unsigned_64 := 0;
      Target : Interfaces.Unsigned_64 := 0;
      Op : CCL.Control.Operation := CCL.Control.Inspect_Bindings;
      Source : String (1 .. Max_Source) := [others => ' '];
      Length : Natural range 0 .. Max_Source := 0;
   end record;
   type Response is record
      Data : CBOR.Byte_Array (1 .. Max_Response) := [others => 0];
      Length : Natural range 0 .. Max_Response := 0;
   end record;
   procedure Decode (Data : CBOR.Byte_Array; Value : out Request; Valid : out Boolean);
   procedure Encode
     (Query : Request; Value : CCL.Control.Response; Data : out Response);
end Control_Wire;
