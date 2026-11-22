with CuBit.Handles.Fixture;
package body CuBit.Handles.Startup is
   function Optional_Endpoint_Named (Label : String) return Optional_Endpoint is
     (if Label = "filesystem" and then Filesystem_Present
      then (Present => True, Value => CuBit.Handles.Fixture.Endpoint_Of (Filesystem_Word))
      else (Present => False));
end CuBit.Handles.Startup;
