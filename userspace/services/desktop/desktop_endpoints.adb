with Startup;
package body Desktop_Endpoints with SPARK_Mode => Off is
   Render_Session : constant CuBit.Handles.Endpoint :=
     (if Startup.Render.Present then Startup.Render.Value else CuBit.Handles.No_Endpoint);
   function Render return CuBit.Handles.Endpoint is (Render_Session);
end Desktop_Endpoints;
