with Interfaces; use Interfaces;
--  Hosted fixture of the startup table: one optional "filesystem" endpoint.
package CuBit.Handles.Startup is
   type Optional_Endpoint (Present : Boolean := False) is record
      case Present is
         when True  => Value : Endpoint;
         when False => null;
      end case;
   end record;
   Filesystem_Present : Boolean := True;
   Filesystem_Word : Unsigned_64 := 12;
   function Optional_Endpoint_Named (Label : String) return Optional_Endpoint;
end CuBit.Handles.Startup;
