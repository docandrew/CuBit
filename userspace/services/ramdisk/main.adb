with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Cpio;
with Ram_Device;

procedure Main is
   function toAddr (Value : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (Value)));
   Archive : Cpio.Archive;
   Valid, View_OK, Ready : Boolean := False;
   Index : Natural;
   Image : System.Address;
   Image_Size, Heap : Unsigned_64;
   Request, Response : Message;
   Sender : ProcessID;
   Ignore : Unsigned_64;
   Tag : MessageTag;
begin
   Cpio.init
     (Archive, toAddr (getInfo (SYSINFO_RAMDISK_ADDRESS)),
      getInfo (SYSINFO_RAMDISK_SIZE), Valid);
   if Valid then
      Index := Cpio.findFile (Archive, "live-rw.ext2");
      if Index < Archive.count then
         Cpio.fileView (Archive, Index, Image, Image_Size, View_OK);
         if View_OK and then Image_Size > 0 and then
           Image_Size mod Ram_Device.Block_Size = 0 and then
           Image_Size <= Unsigned_64 (Natural'Last)
         then
            Heap := syscall (SYSCALL_SBRK, Image_Size);
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
   if not Ready then
      debugPrint ("ramdisk: image initialization failed" & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT);
      return;
   end if;
   debugPrint ("ramdisk: volatile block device ready" & ASCII.LF);
   Tag := capSend
     (15, (tag => (label => 16#FF00#, length => 0, flags => 0, reserved => 0),
           authorityTag => 0, words => [others => 0]));
   loop
      receive (Sender, Request);
      Ram_Device.Handle (Sender, Request, Response);
      Ignore := reply (Sender, Response);
   end loop;
end Main;
