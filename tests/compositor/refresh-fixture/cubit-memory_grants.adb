with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles.Fixture;
with Desktop_Endpoints;
package body CuBit.Memory_Grants is
   procedure Create_Via_Capability
     (slot : CuBit.Handles.Endpoint; localAddr : System.Address; numPages : Natural;
      readWrite : Boolean; reference : out Grant_Reference; success : out Boolean) is
      use type System.Address;
   begin
      pragma Assert (CuBit.Handles.Fixture.Word_Of (slot) =
                     CuBit.Handles.Fixture.Word_Of (Desktop_Endpoints.Config_Service) and localAddr = Storage'Address and numPages = 2 and readWrite);
      reference := (8, 9);
      success := True;
   end Create_Via_Capability;
end CuBit.Memory_Grants;
