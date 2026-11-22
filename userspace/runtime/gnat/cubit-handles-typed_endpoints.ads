------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  An endpoint typed by the service it reaches (CAP-003).
--
--  @description
--  One instance per catalog client (CuBit.Handles.Services): its Endpoint
--  can be given only to the client code for that service (a log
--  publisher takes a Logstore endpoint, a launcher a Process_Manager one),
--  so passing another service's endpoint is a compile error. Programs get
--  one from their generated Startup package (the catalog's (client X)
--  column names the instance), never by building one.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Startup_State;

generic
package CuBit.Handles.Typed_Endpoints with SPARK_Mode is

   type Endpoint is private;

   type Optional_Endpoint (Present : Boolean := False) is record
      case Present is
         when True  => Value : Endpoint;
         when False => null;
      end case;
   end record;

   --  The handle to call through.
   function Handle (Item : Endpoint) return CuBit.Handles.Endpoint;

   --  The startup entry Label (an Endpoint), for the generated Startup
   --  package: the null handle (never resolves) when the table has none,
   --  which the package's elaboration check reports before the program runs.
   function Named (Label : String) return Endpoint
   with Global => CuBit.Startup_State.Loaded_Table;
   function Optional_Named (Label : String) return Optional_Endpoint
   with Global => CuBit.Startup_State.Loaded_Table;

   --  An endpoint to this service that a launcher handed over at run time
   --  (a driver's logstore grant from devmgr), until CAP-002's Connect
   --  returns typed handles. The caller vouches for what it reaches.
   function Granted (Item : CuBit.Handles.Endpoint) return Endpoint;

private
   type Endpoint is record
      Value : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   end record;

   function Handle (Item : Endpoint) return CuBit.Handles.Endpoint is (Item.Value);
   function Granted (Item : CuBit.Handles.Endpoint) return Endpoint is ((Value => Item));
end CuBit.Handles.Typed_Endpoints;
