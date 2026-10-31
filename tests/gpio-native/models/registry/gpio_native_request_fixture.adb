pragma Ada_2022;
package body GPIO_Native_Request_Fixture is
   package P renames CuBit.Process_IDs;
   function Authorized (Caller : P.Process_ID; Badge : GPIO_Control_Wire.Word) return Boolean is
     (Registry.Authorized (Caller, Badge));
   procedure Offer (Caller : P.Process_ID; Badge : GPIO_Control_Wire.Word;
      Item : GPIO_Control_Wire.Request; Reply : CuBit.Handles.Reply; Taken : out Boolean) is
   begin
      Registry.Offer (Caller, Badge, Item, Reply, Taken);
   end Offer;
end GPIO_Native_Request_Fixture;
