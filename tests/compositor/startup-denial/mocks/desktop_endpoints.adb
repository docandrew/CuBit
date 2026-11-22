with CuBit.Handles.Fixture; with Test_Control;
package body Desktop_Endpoints is
 --  Any fixture word: the mocks only test whether a render entry is held.
 Render_Word : constant := 25;
 function Render return CuBit.Handles.Endpoint is
  (if Test_Control.Render_Absent then CuBit.Handles.No_Endpoint
   else CuBit.Handles.Fixture.Endpoint_Of (Render_Word));
end Desktop_Endpoints;
