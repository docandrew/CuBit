--  A stand-in installer for the hosted tests: disk 0 with a serial, disk 1
--  without (as QEMU's UFS model), and an install that succeeds only when
--  the confirmation names the disk's model, serial and geometry (as
--  installer.svc decides). It records the last request so the test can
--  check decoding. Like the service it answers Start at once; the first
--  Status after reports the install running halfway, the next finished.
with Interfaces; use Interfaces;
with CuBit.Device_Identities;
with Install_Record;

package body CCL_Installer is
   package DId renames CuBit.Device_Identities;
   use type IP.Disk_Ordinal, IP.Install_State;

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

   --  The started install's result, reported once a Status has seen it run.
   Final : IP.Install_Progress;
   Seen_Running : Boolean := False;

   procedure Start
     (Request : IP.Install_Request; Started : out IP.Start_Result; Result : out Result_Kind)
   is
      Outcome : IP.Install_Outcome;
      Proof : IP.Identity_Proof := IP.Serial_Number;
      Files, Bytes : Interfaces.Unsigned_64 := 0;
   begin
      Result := Answered;
      if Final.State = IP.Running then
         Started := IP.Busy;
         return;
      end if;
      Install_Record.Last := Request;
      Install_Record.Seen := True;
      if Request.Disk > Disks'Last then
         Outcome := IP.No_Such_Disk;
      else
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
      end if;
      Final := (State => IP.Running, Outcome => Outcome, Proof => Proof,
                Files_Done => Files, Files => Files, Bytes_Done => Bytes, Bytes => Bytes);
      Seen_Running := False;
      Started := IP.Started;
   end Start;

   procedure Status (Progress : out IP.Install_Progress; Result : out Result_Kind) is
   begin
      Result := Answered;
      if Final.State = IP.Running and then not Seen_Running then
         Seen_Running := True;
         Progress := (State => IP.Running, Files_Done => Final.Files / 2, Files => Final.Files,
                      Bytes_Done => Final.Bytes / 2, Bytes => Final.Bytes, others => <>);
      else
         if Final.State = IP.Running then
            Final.State := IP.Finished;
         end if;
         Progress := Final;
      end if;
   end Status;
end CCL_Installer;
