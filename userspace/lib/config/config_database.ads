with System;
with Interfaces;
with CCL.Objects.Persistence;
with Config_Worker_Protocol;
with Config_Worker_Storage;

--  Thin in-process FFI adapter. Context is an exclusively borrowed Rust
--  Database, NOT an IPC handle. No pointer crosses a process boundary or is
--  retained by Invoke. The owner controls creation/destruction and must not
--  invoke concurrently. Raw address/lifetime obligations are outside SPARK.
package Config_Database is
   procedure Invoke
     (Context_Handle : System.Address;
      Action : Config_Worker_Protocol.Operation;
      Name, Context : String; Expected_Revision : Config_Worker_Protocol.Number;
      Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
      Output : out Config_Worker_Storage.Reply);
private
   -- Shared in-process layout, not client IPC. Each exported function has
   -- its own operation type; all raw scalars admit every bit pattern.
   use Interfaces;
   type Request is record
      Action, Name_Length, Context_Length, Reserved : Unsigned_32 := 0;
      Expected_Revision : Unsigned_64 := 0;
      Schema : CCL.Objects.Schema_Key := CCL.Objects.No_Schema;
      Name, Context : String (1 .. 128) := [others => Character'Val (0)];
      Input_Length : Unsigned_64 := 0;
      Input : System.Address := System.Null_Address;
   end record with Size => 328 * 8, Alignment => 8;
   for Request use record
      Action at 0 range 0 .. 31;
      Name_Length at 4 range 0 .. 31;
      Context_Length at 8 range 0 .. 31;
      Reserved at 12 range 0 .. 31;
      Expected_Revision at 16 range 0 .. 63;
      Schema at 24 range 0 .. 255;
      Name at 56 range 0 .. 1023;
      Context at 184 range 0 .. 1023;
      Input_Length at 312 range 0 .. 63;
      Input at 320 range 0 .. 63;
   end record;
end Config_Database;
