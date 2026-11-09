------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  System generations on this machine (LIVE-001 services, REMOTE-001
--  "remote staging and live switch"; cubit-remote system activate |
--  rollback | generations): like Nix, a change to what runs is a new
--  generation that pins programs by content address, and activating it
--  switches the running system; rollback activates the generation before.
--
--  @description
--  In the system volume's storehouse (Remote_Files.Storehouse_Root, the
--  same one ccl-build fills):
--
--    executables/<sha256>      a pinned program, named by its bytes' SHA-256
--    profiles/system/<n>       generation n (n >= 1): one typed CCL value,
--                              (System_Generation number => n previous => p
--                                pins =>
--                                [(Service_Pin service => "acpi.svc"
--                                  executable => "<sha256 hex>") ...])
--    profiles/system/current   the current generation's number (decimal);
--                              absent: 0
--
--  Generation 0 is the boot image: no pins, every startup-plan service
--  runs its image's program. A pin overrides one service's program. The
--  records are read back by the CCL type checker against their type
--  (System_Generation_Records.Schema)
--  (CCL.Typed_Values), never by a hand-written reader; a record that does
--  not check is a typed failure.
--
--  previous: the generation that was current when n was made (its base);
--  a rollback activates the current generation's previous, so a generation
--  that failed its activation is never rolled back to.
--
--  Activating generation T from the current C: the services whose pins
--  differ are switched by procmgr (CuBit.Launching.Switch_Service: stop,
--  start as the same startup-plan entry, health window, rollback to the
--  program before on failure); only when that succeeds does "current"
--  name T (written by rename). v1 switches at most
--  Maximum_Changed_Pins service per activation (Too_Many_Changes beyond),
--  so a failure always leaves the system as C was.
--
--  Not yet (seams): the boot does not apply the current generation (so on
--  an installed system a reboot runs the image's programs again until
--  procmgr reads the pins at startup, the next step; on the live USB the
--  storehouse is in RAM and goes with the reboot); the kernel and drivers
--  launched by devmgr are not pinned (a kernel pin takes effect at the
--  next boot, BOOT-002); programs are not signed (AUTH-001).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Remote_Wire;
with System_Generation_Records.Reading;

package Remote_Generations is
   package W renames Remote_Wire;

   --  Named ceilings: generations kept, and the pins one activation
   --  switches (CAPACITY / Too_Many_Changes beyond). The record's type and
   --  pins: System_Generation_Records.
   Maximum_Generations : constant := System_Generation_Records.Reading.Maximum_Generations;
   Maximum_Changed_Pins : constant := 1;

   type Report is record
      Result   : W.Generation_Result := W.Unchanged;
      Detail   : Interfaces.Unsigned_8 := 0;
      Current  : W.Generation := W.No_Generation;
      Target   : W.Generation := W.No_Generation;
      Pin      : W.Digest := [others => 0];
      Running  : Interfaces.Unsigned_64 := 0;
      Service  : String (1 .. W.Maximum_Service_Name_Bytes) := [others => ' '];
      Service_Length : Natural range 0 .. W.Maximum_Service_Name_Bytes := 0;
   end record;

   --  Product (a workspace name) becomes Service's pin in a new generation,
   --  which is then activated. Status: OK (Item says how it ended), or why
   --  nothing was done (Not_Found: no such product; a filesystem failure;
   --  Failed: a generation record does not check; Capacity).
   procedure Activate
     (Product, Service : String; Grace, Health : Interfaces.Unsigned_32;
      Item : out Report; Status : out W.Status)
     with Pre => Product'Length in 1 .. W.Maximum_Name_Bytes
                 and then Service'Length in 1 .. W.Maximum_Service_Name_Bytes;

   --  Activate the current generation's previous one.
   procedure Rollback
     (Grace, Health : Interfaces.Unsigned_32; Item : out Report; Status : out W.Status);

   --  The records, one per line, then "current <n>".
   procedure List (Output : out W.Byte_Array; Length : out Natural; Status : out W.Status)
     with Pre => Output'First = 1;

   procedure Encode (Item : Report; Data : in out W.Byte_Array)
     with Pre => Data'First = 1 and then Data'Length >= W.Generation_Response_Bytes;

   --  The activity line: "acpi.svc: generation 3 active (pid 0x..)".
   function Image (Item : Report) return String;
end Remote_Generations;
