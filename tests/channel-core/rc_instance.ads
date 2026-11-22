--  A request channel instance for the hosted test: 8 request slots, 4
--  completion slots (so completion space limits taking).
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Request_Channels;
package RC_Instance is
   type Request is record
      Value : Unsigned_64 := 0;
   end record;
   type Result is record
      Value : Unsigned_64 := 0;
   end record;
   package RC is new CuBit.Request_Channels (Request, Result, Request_Bits => 3, Completion_Bits => 2);
end RC_Instance;
