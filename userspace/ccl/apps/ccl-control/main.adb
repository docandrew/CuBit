with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols;
with CCL.Control;
with CCL.Sessions;
with CCL.Catalog;
with CCL.Interfaces.Clock;
with Control_Transport;

procedure Main is
   Session : CCL.Sessions.Session;
   Catalog : CCL.Catalog.Interface_Catalog;
   Catalog_Error : CCL.Catalog.Catalog_Error;
   Host : CCL.Control.Observation;
   Result : CCL.Control.Response;
   Header : String (1 .. 8);
   Source : String (1 .. 1024);
   Count, Code : Natural;
   Success : Boolean;
   Request : Message;
   Tag : MessageTag;
   use type CCL.Catalog.Catalog_Error;
begin
   debugPrint ("ccl-control: DEVELOPMENT PLAINTEXT; own bindings only" & ASCII.LF);
   Host.Process_Id := syscall (SYSCALL_GETPID);
   Control_Transport.Open (Host.Network_Process, Success);
   if not Success then
      debugPrint ("ccl-control: relay connection failed" & ASCII.LF);
      Control_Transport.Close; return;
   end if;
   CCL.Catalog.Initialize (Catalog);
   Host.Clock_Process := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_CLOCK);
   if Host.Clock_Process /= 0 and Host.Clock_Process /= Unsigned_64'Last then
      CCL.Interfaces.Clock.Publish (Catalog, Catalog_Error);
      if Catalog_Error /= CCL.Catalog.Catalog_Valid then Control_Transport.Close; return; end if;
   end if;
   CCL.Sessions.Initialize (Session, Catalog);
   debugPrint ("ccl-control: relay connected" & ASCII.LF);
   loop
      Control_Transport.Read_Exact (Header, Success);
      exit when not Success;
      Code := Character'Pos (Header (4));
      Count := Character'Pos (Header (7)) * 256 + Character'Pos (Header (8));
      --  Reject invalid frames before slicing, enum conversion, or evaluation.
      exit when Header (1 .. 3) /= "CC" & Character'Val (1) or else
        Code not in 1 .. 3 or else Count > Source'Length or else
        (Header (5) = Character'Val (0) and Header (6) = Character'Val (0)) or else
        (Code /= 2 and Count /= 0);
      Control_Transport.Read_Exact (Source (1 .. Count), Success);
      exit when not Success;
      exit when (for some C of Source (1 .. Count) =>
        C not in ' ' .. '~' | ASCII.HT | ASCII.CR | ASCII.LF);
      Request := NULL_MESSAGE;
      Request.tag := (label => CuBit.Protocols.CLOCK_OP_MONOTONIC_MS,
                      length => 1, flags => 0, reserved => 0);
      Tag := capCall (CAP_SLOT_CLOCK, Request);
      Host.Clock_Available := Tag.label = 16#F000# and Tag.length = 1;
      if Host.Clock_Available then Host.Monotonic_Ms := Request.words (0); end if;
      CCL.Control.Execute
        (Session, CCL.Control.Operation'Enum_Val (Code), Source (1 .. Count), Host, Result);
      Header (4) := Character'Val (Code + 128);
      Header (7) := Character'Val (Result.Length / 256);
      Header (8) := Character'Val (Result.Length mod 256);
      Control_Transport.Write_All (Header, Success);
      exit when not Success;
      Control_Transport.Write_All (Result.Data (1 .. Result.Length), Success);
      exit when not Success;
      debugPrint ("ccl-control: request completed" & ASCII.LF);
   end loop;
   Control_Transport.Close;
   debugPrint ("ccl-control: disconnected; session discarded" & ASCII.LF);
end Main;
