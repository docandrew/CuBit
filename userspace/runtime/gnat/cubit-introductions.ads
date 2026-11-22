------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A program's side of late introduction (CuBit.Late_Introduction): wait
--  for a service its manifest declared optional and that was not running
--  when it started.
--
--  @description
--  Await blocks the calling thread until procmgr introduces the service
--  (or refuses). In_Background makes one thread for it, which calls
--  Arrived with the endpoint and ends: the program's own threads never
--  wait, and the program's completion queue is not used.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles;

package CuBit.Introductions is
   subtype Endpoint is CuBit.Handles.Endpoint;

   type Outcome is
     (Introduced,     --  Value is the service's endpoint
      Not_Absent,     --  no Absent optional endpoint entry by that name
      No_Introducer,  --  the startup table holds no introducer endpoint
      Not_Declared,   --  procmgr kept no decision for it
      Withdrawn,      --  procmgr dropped it without an introduction
      Failed);        --  the call did not complete, or no endpoint came
   type Result (Kind : Outcome := Failed) is record
      case Kind is
         when Introduced => Value : Endpoint;
         when others     => null;
      end case;
   end record;

   --  Whether Label names an Absent optional endpoint entry of this
   --  program's startup table and the table holds the introducer: what
   --  Await needs.
   function Awaitable (Label : String) return Boolean;

   --  Wait (this thread) until procmgr introduces the service of the
   --  Absent optional entry Label.
   function Await (Label : String) return Result;

   --  Await on a thread of its own; Arrived runs on that thread with the
   --  endpoint, then the thread ends. Started False: nothing to wait for
   --  (not Awaitable) or no thread could be made; Arrived is never called.
   type Arrival is access procedure (Value : Endpoint);
   procedure In_Background (Label : String; Arrived : not null Arrival; Started : out Boolean);

end CuBit.Introductions;
