--  A stand-in installer for the hosted tests: one disk, and an install
--  that succeeds only when the confirmation names it (as installer.svc
--  decides). It records the last request so the test can check decoding.
with CuBit.Device_Identities;
with Install_Record;

package body CCL_Installer is
   package DId renames CuBit.Device_Identities;
   use type IP.Disk_Ordinal;

   The_Disk : constant IP.Disk_Record :=
     (Blocks => 262_144, Block_Size => 4096,
      Identity => (Model => DId.To_Text ("QEMU UFS"), Serial => DId.To_Text ("cubitufs")));

   procedure Disk_Info
     (Number : IP.Disk_Ordinal; Item : out IP.Disk_Record; Present : out Boolean;
      Result : out Result_Kind) is
   begin
      Present := Number = 0;
      Item := (if Present then The_Disk else (others => <>));
      Result := Answered;
   end Disk_Info;

   procedure Install
     (Request : IP.Install_Request; Outcome : out IP.Install_Outcome;
      Files, Bytes : out Interfaces.Unsigned_64; Result : out Result_Kind) is
   begin
      Install_Record.Last := Request;
      Install_Record.Seen := True;
      Files := 0;
      Bytes := 0;
      Result := Answered;
      if Request.Disk /= 0 then
         Outcome := IP.No_Such_Disk;
      elsif not DId.Same (Request.Confirmation.Model, The_Disk.Identity.Model)
        or else not DId.Same (Request.Confirmation.Serial, The_Disk.Identity.Serial)
      then
         Outcome := IP.Confirmation_Mismatch;
      else
         Outcome := IP.Installed;
         Files := 7;
         Bytes := 12_345;
      end if;
   end Install;
end CCL_Installer;
