--  Hosted stand-in for Desktop's startup endpoints (CAP-002): the render
--  entry is present (a fixture handle) or absent, as Test_Control says.
with CuBit.Handles;
package Desktop_Endpoints is
 function Render return CuBit.Handles.Endpoint;
end Desktop_Endpoints;
