with Interfaces;
with CBOR;
with CCL.Control;

--  Bounded lab protocol. Shape validation grants no authority.
package Control_Wire with SPARK_Mode is
   Max_Source : constant := 1024;
   Max_Request : constant := 1100;
   Max_Response : constant := 8192;
   --  CCL.Image_Store.Maximum_Side: a Read_Image_Rows first row is below it.
   Max_Image_Side : constant := 512;
   --  Every request and response begins [Protocol_Version, id, ...].
   --  Version 2: a request names its session, [2, id, session, op, ...].
   Protocol_Version : constant := 2;
   --  A browser tab's session, chosen at random by the browser. It keeps
   --  the tab's definitions and streams apart from other tabs'; it is not
   --  a credential (this lab adapter has no login yet). No_Session asks for
   --  a fresh session discarded after the request.
   No_Session : constant := 0;
   type Request is record
      Id : Interfaces.Unsigned_64 := 0;
      Session : Interfaces.Unsigned_64 := No_Session;
      Target : Interfaces.Unsigned_64 := 0;   --  a monitor generation, or an image id
      Row : Interfaces.Unsigned_64 := 0;      --  Read_Image_Rows: the first row
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
