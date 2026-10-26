--  A stand-in installer for the hosted tests: disk 0 with a serial, disk 1
--  without (as QEMU's UFS model), and an install that succeeds only when
--  the confirmation names the disk's model, serial and geometry (as
--  installer.svc decides). It records the last request so the test can
--  check decoding.
with CuBit.Device_Identities;
with Install_Record;

package body CCL_Installer is
   package DId renames CuBit.Device_Identities;
   use type IP.Disk_Ordinal, Interfaces.Unsigned_64, Interfaces.Unsigned_32;

   type Disk_Table is array (IP.Disk_Ordinal range 0 .. 1) of IP.Disk_Record;
   Disks : constant Disk_Table :=
     [0 => (Blocks => 262_144, Block_Size => 4096,
            Identity => (Model => DId.To_Text ("QEMU UFS"), Serial => DId.To_Text ("cubitufs"))),
      1 => (Blocks => 31_246_336, Block_Size => 4096,
            Identity => (Model => DId.To_Text ("NO SERIAL"), Serial => DId.To_Text ("")))];

   procedure Disk_Info
     (Number : IP.Disk_Ordinal; Item : out IP.Disk_Record; Present : out Boolean;
      Result : out Result_Kind) is
   begin
      Present := Number <= Disks'Last;
      Item := (if Present then Disks (Number) else (others => <>));
      Result := Answered;
   end Disk_Info;

   procedure Install
     (Request : IP.Install_Request; Outcome : out IP.Install_Outcome;
      Files, Bytes : out Interfaces.Unsigned_64; Proof : out IP.Identity_Proof;
      Result : out Result_Kind) is
   begin
      Install_Record.Last := Request;
      Install_Record.Seen := True;
      Files := 0;
      Bytes := 0;
      Proof := IP.Serial_Number;
      Result := Answered;
      if Request.Disk > Disks'Last then
         Outcome := IP.No_Such_Disk;
         return;
      end if;
      declare
         Disk : IP.Disk_Record renames Disks (Request.Disk);
      begin
         if not DId.Same (Request.Confirmation.Model, Disk.Identity.Model)
           or else not DId.Same (Request.Confirmation.Serial, Disk.Identity.Serial)
           or else Request.Confirm_Blocks /= Disk.Blocks
           or else Request.Confirm_Block_Size /= Disk.Block_Size
         then
            Outcome := IP.Confirmation_Mismatch;
         else
            Outcome := IP.Installed;
            Proof := (if Disk.Identity.Serial.Length = 0 then IP.Model_And_Geometry else IP.Serial_Number);
            Files := 7;
            Bytes := 12_345;
         end if;
      end;
   end Install;
end CCL_Installer;
