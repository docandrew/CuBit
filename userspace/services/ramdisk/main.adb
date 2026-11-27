with CuBit.Console_Route;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with CuBit.Owned_Reservations;
with CuBit.Process_Calls;
with Launch_Configuration;
with Cpio;
with Ram_Device;

with Startup;

procedure Main is
   function toAddr (Value : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (Value)));
   Archive : Cpio.Archive;
   Valid, View_OK, Ready : Boolean := False;
   Index : Natural;
   Image : System.Address;
   Image_Size, Heap : Unsigned_64;
   Tag : MessageTag;
   Boot_Image_Bytes : Unsigned_64 := 0;

   --  Release (CuBit.Owned_Reservations.Release) this process's read-only
   --  view of the boot image, a bounded number of pages per call.
   procedure Release_Boot_View is
      Page_Bytes : constant := CuBit.Owned_Reservations.Page_Bytes;
      Pages : constant Unsigned_64 := (Boot_Image_Bytes + Page_Bytes - 1) / Page_Bytes;
      Done, Count : Unsigned_64 := 0;
   begin
      while Done < Pages loop
         Count := Unsigned_64'Min (MAX_MAP_INTO_PAGES_PER_CALL, Pages - Done);
         if not CuBit.Owned_Reservations.Release
                  (Launch_Configuration.Boot_Image_Address + Done * Page_Bytes, Count * Page_Bytes)
         then
            debugPrint ("ramdisk: boot image view not released" & ASCII.LF);
            return;
         end if;
         Done := Done + Count;
      end loop;
      debugPrint ("ramdisk: boot image view released" & ASCII.LF);
   end Release_Boot_View;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console;
   --  queued until devmgr introduces logstore (CuBit.Late_Introduction).
   CuBit.Console_Route.Route_Named ("logstore");
   --  Stop (devmgr's, at shutdown): nothing to finish; the runtime ends
   --  the process when a receive returns it (CuBit.Messages).
   CuBit.Messages.Exit_On_Stop;
   --  The boot image devmgr mapped for us, from its launch configuration.
   declare
      Configuration : Launch_Configuration.Boot_Image_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Configured);
      Valid := Configured and then Configuration.Boot_Image /= 0;
      if Valid then
         Boot_Image_Bytes := Configuration.Boot_Image;
         Cpio.init
           (Archive, toAddr (Launch_Configuration.Boot_Image_Address),
            Configuration.Boot_Image, Valid);
      end if;
   end;
   if Valid then
      Index := Cpio.findFile (Archive, "live-rw.ext2");
      if Index < Archive.count then
         Cpio.fileView (Archive, Index, Image, Image_Size, View_OK);
         if View_OK and then Image_Size > 0 and then
           Image_Size mod Ram_Device.Block_Size = 0 and then
           Image_Size <= Unsigned_64 (Natural'Last)
         then
            Heap := CuBit.Process_Calls.Grow_Heap (Image_Size);
            if Heap /= Unsigned_64'Last and then Heap /= 0 then
               declare
                  Source : String (1 .. Natural (Image_Size))
                    with Import, Address => Image;
                  Destination : String (1 .. Natural (Image_Size))
                    with Import, Address => toAddr (Heap);
               begin
                  Destination := Source;
               end;
               Ram_Device.Initialize (toAddr (Heap), Image_Size, Ready);
            end if;
         end if;
      end if;
   end if;
   --  The seed is copied: this driver never reads the boot image again.
   --  Its whole view goes back, so the seed's pages return to the kernel
   --  once devmgr releases its own view of them (boot module lifetime).
   if Ready then
      Release_Boot_View;
   end if;
   if not Ready then
      debugPrint ("ramdisk: image initialization failed" & ASCII.LF);
      CuBit.Process_Calls.Exit_Process;
   end if;
   debugPrint ("ramdisk: volatile block device ready" & ASCII.LF);
   Tag := capSend
     (Startup.Ready, (tag => (label => 16#FF00#, length => 0, protocol => 0, ipc => <>, reserved => 0),
           authorityTag => 0, words => [others => 0]),
      CuBit.Messages.Wait_Forever);
   loop
      Ram_Device.Serve_One;
   end loop;
end Main;
