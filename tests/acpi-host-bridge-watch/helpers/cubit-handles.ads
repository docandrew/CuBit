pragma Ada_2022;
package CuBit.Handles with Pure is
   type Endpoint is private;
   No_Endpoint, Model_Endpoint : constant Endpoint;
   function Valid (Item : Endpoint) return Boolean;
   type Reply is private;
   No_Reply : constant Reply;
   function Valid (Item : Reply) return Boolean;
private
   Maximum_Slots : constant := 4;
   type Reply is record Slot : Natural range 0 .. Maximum_Slots := 0; end record;
   No_Reply : constant Reply := (Slot => 0);
   type Endpoint is record Present : Boolean; end record;
   No_Endpoint : constant Endpoint := (Present => False);
   Model_Endpoint : constant Endpoint := (Present => True);
end CuBit.Handles;
