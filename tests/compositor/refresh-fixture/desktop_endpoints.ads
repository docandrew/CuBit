--  Hosted stand-in for Desktop's startup endpoints (CAP-002): handles made
--  by the test fixture, so the mocked transport can check which one a call
--  went through. Only what the refresh path names.
with CuBit.Handles;
with CuBit.Handles.Fixture;
package Desktop_Endpoints is
   --  The fixture's word for Desktop's "config" entry.
   Config_Service : constant CuBit.Handles.Endpoint := CuBit.Handles.Fixture.Endpoint_Of (7);
end Desktop_Endpoints;
