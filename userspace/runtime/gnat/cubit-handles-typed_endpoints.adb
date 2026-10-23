------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Startup;

package body CuBit.Handles.Typed_Endpoints with SPARK_Mode => Off is

   function Named (Label : String) return Endpoint is
     ((Value => CuBit.Handles.Startup.Endpoint_Named (Label)));

   function Optional_Named (Label : String) return Optional_Endpoint is
      Found : constant CuBit.Handles.Startup.Optional_Endpoint :=
        CuBit.Handles.Startup.Optional_Endpoint_Named (Label);
   begin
      if Found.Present then
         return (Present => True, Value => (Value => Found.Value));
      end if;
      return (Present => False);
   end Optional_Named;

end CuBit.Handles.Typed_Endpoints;
