--  Hosted tests of system.disks and system.install (CCL.Interfaces.Install,
--  CCL_Install_Bindings): typed at a CCL session, decoded into the
--  installer protocol's request, refused or answered as typed records.
with Ada.Text_IO;
with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL_Install_Bindings;
with CuBit.Installer_Protocol;
with Install_Record;

procedure Main is
   use type CuBit.Installer_Protocol.Disk_Ordinal;
   use type Interfaces.Unsigned_32;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
      end if;
      Ada.Text_IO.Put_Line ((if Condition then "PASS: " else "FAIL: ") & Label);
   end Check;

   type No_Context is null record;
   Host : No_Context;
   procedure Invoke
     (Context : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      CCL_Install_Bindings.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Submit is new CCL.Sessions.Submit_With_Values (No_Context, Invoke);

   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Installed : Boolean;

   function Run (Source : String) return String is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Submit (Session, Source, CCL.Sessions.Default_Fuel, Grants, Host, Outcome);
      return CCL.Sessions.Result_Image (Outcome);
   end Run;

   function Request (Model, Serial : String; Ratio : String := "262144") return String is
     ("(system.install (Install_Request disk => 0 confirm => (Disk_Identity """ & Model &
      """ """ & Serial & """) inode_ratio => " & Ratio &
      " esp_mib => 260 source => ""@nvme:0/install""))");
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   CCL_Install_Bindings.Install (Catalog, Grants, Installed);
   Check (Installed, "system.* is published and granted");
   CCL.Sessions.Initialize (Session, Catalog);
   declare
      Listed : constant String := Run ("(system.disks)");
   begin
      Check (Listed = "List<Disk>: [(Disk 0 (Disk_Identity ""QEMU UFS"" ""cubitufs"") 262144 4096)]",
             "system.disks lists the disk: " & Listed);
   end;
   declare
      Refused : constant String := Run (Request ("QEMU UFS", "another-disk"));
   begin
      Check (Refused = "Install_Report: (Install_Report Install_Outcome.Wrong_Disk 0 0)",
             "a confirmation naming another disk is refused: " & Refused);
   end;
   declare
      Done : constant String := Run (Request ("QEMU UFS", "cubitufs"));
   begin
      Check (Done = "Install_Report: (Install_Report Install_Outcome.Installed 7 12345)",
             "the confirmed install is answered: " & Done);
      Check (Install_Record.Seen and then Install_Record.Last.Disk = 0
             and then Install_Record.Last.Ratio = 262_144 and then Install_Record.Last.ESP_Size = 260
             and then Install_Record.Last.Source (1 .. Install_Record.Last.Length) = "@nvme:0/install",
             "the request decodes into the installer protocol");
   end;
   declare
      Low : constant String := Run (Request ("QEMU UFS", "cubitufs", Ratio => "4096"));
   begin
      Check (Low'Length > 0 and then Low (Low'First .. Low'First + 14) /= "Install_Report:",
             "an inode ratio below the range is refused before the installer: " & Low);
   end;
   declare
      Missing : constant String := Run ("(system.install (Install_Request disk => 0 inode_ratio => 262144 esp_mib => 260 source => ""x""))");
   begin
      Check (Missing'Length > 0 and then Missing (Missing'First .. Missing'First + 14) /= "Install_Report:",
             "no confirmation, no install (nothing is defaulted): " & Missing);
   end;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("CCL-INSTALL-CHECK: PASS");
   else
      Ada.Text_IO.Put_Line ("CCL-INSTALL-CHECK: FAIL");
   end if;
end Main;
