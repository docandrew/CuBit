with CCL.Interfaces.Install;
with CCL.Objects;
with CCL.Objects.Views;
with CCL_Installer;
with CuBit.Device_Identities;
with CuBit.Failures;
with CuBit.Installer_Protocol;

package body CCL_Install_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Build_Result;
   use type CCL_Installer.Result_Kind;
   package Install_Interface renames CCL.Interfaces.Install;
   package IP renames CuBit.Installer_Protocol;
   package DId renames CuBit.Device_Identities;
   package Views renames CCL.Objects.Views;
   package Failures renames CuBit.Failures;

   Contracts : Install_Interface.Contracts;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in Install_Interface.FIRST_BINDING ..
                 Install_Interface.Binding_Of (Install_Interface.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Install_Interface.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Install_Interface.DISKS_KEY, Contracts.Disks);
      CCL.Catalog.Resolve_Schema (Catalog, Install_Interface.REQUEST_KEY, Contracts.Request);
      CCL.Catalog.Resolve_Schema (Catalog, Install_Interface.REPORT_KEY, Contracts.Report);
      Success := CCL.Objects.Is_Bound (Contracts.Disks) and then CCL.Objects.Is_Bound (Contracts.Request)
        and then CCL.Objects.Is_Bound (Contracts.Report);
      for Op in Install_Interface.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "system." & Install_Interface.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, Install_Interface.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   NOT_GRANTED : constant Failures.Failure := Failures.Failed
     (Failures.Not_Granted, "installing needs installer.svc",
      "the program's manifest must request the installer service (service ""installer"" ""installer""), " &
      "which only the trusted startup plan grants");
   UNAVAILABLE : constant Failures.Failure := Failures.Failed
     (Failures.Unavailable, "the installer did not answer");

   procedure List_Disks (Reply : in out CCL.Host_Values.Call_Result) is
      type Disk_List is array (IP.Disk_Ordinal) of IP.Disk_Record;
      Disks : Disk_List;
      Count : Natural := 0;
      Present : Boolean;
      Result : CCL_Installer.Result_Kind;
      Image : CCL.Objects.Image := CCL.Objects.Empty (Contracts.Disks);
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (Cell : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
      end Put;
      procedure Put_Text (Text : String) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Image, Text, Step); end if;
      end Put_Text;
      function Number (Value : Unsigned_64) return CCL.Objects.Cell is
        (CCL.Objects.Integer_Cell (Integer_64 (Unsigned_64'Min (Value, Unsigned_64 (Integer_64'Last)))));
   begin
      --  Disks are numbered densely from 0: the first absent ends the list.
      for N in IP.Disk_Ordinal loop
         CCL_Installer.Disk_Info (N, Disks (N), Present, Result);
         if Result = CCL_Installer.Not_Granted then
            Reply.Why := NOT_GRANTED;
            return;
         elsif Result /= CCL_Installer.Answered then
            Reply.Why := UNAVAILABLE;
            return;
         end if;
         exit when not Present;
         Count := Count + 1;
      end loop;
      Put (CCL.Objects.Sequence_Cell (Count));
      for N in 0 .. Count - 1 loop
         declare
            Item : IP.Disk_Record renames Disks (IP.Disk_Ordinal (N));
         begin
            Put (CCL.Objects.Product_Cell (Install_Interface.DISK_FIELDS));
            Put (Number (Unsigned_64 (N)));
            Put (CCL.Objects.Product_Cell (Install_Interface.IDENTITY_FIELDS));
            Put_Text (Item.Identity.Model.Value (1 .. Item.Identity.Model.Length));
            Put_Text (Item.Identity.Serial.Value (1 .. Item.Identity.Serial.Length));
            Put (Number (Item.Blocks));
            Put (Number (Unsigned_64 (Item.Block_Size)));
         end;
      end loop;
      if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Contracts.Disks) then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
      else
         Reply.Why := Failures.Failed (Failures.Exhausted, "the disk list does not fit one result");
      end if;
   end List_Disks;

   procedure Run_Install (Argument : CCL.Host_Values.Value; Reply : in out CCL.Host_Values.Call_Result) is
      Object : Views.Snapshot;
      Captured : Boolean := False;
      Request : IP.Install_Request;
      Outcome : IP.Install_Outcome;
      Files, Bytes : Unsigned_64;
      Proof : IP.Identity_Proof;
      Result : CCL_Installer.Result_Kind;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Contracts.Request, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := Failures.Failed (Failures.Invalid_Argument, "system.install takes an Install_Request");
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         Confirm : constant Views.Cursor := Views.Field (Object, Root, Install_Interface.REQUEST_CONFIRM);
         Disk : constant Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Root, Install_Interface.REQUEST_DISK)));
         Ratio : constant Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Root, Install_Interface.REQUEST_RATIO)));
         ESP : constant Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Root, Install_Interface.REQUEST_ESP)));
         Model : constant String := Views.Text (Object, Views.Field (Object, Confirm, Install_Interface.IDENTITY_MODEL));
         Serial : constant String := Views.Text (Object, Views.Field (Object, Confirm, Install_Interface.IDENTITY_SERIAL));
         Source : constant String := Views.Text (Object, Views.Field (Object, Root, Install_Interface.REQUEST_SOURCE));
         Blocks : constant Integer_64 := CCL.Objects.Integer_Of
           (Views.Scalar (Object, Views.Field (Object, Confirm, Install_Interface.IDENTITY_BLOCKS)));
         Block_Bytes : constant Integer_64 := CCL.Objects.Integer_Of
           (Views.Scalar (Object, Views.Field (Object, Confirm, Install_Interface.IDENTITY_BLOCK_BYTES)));
      begin
         --  The CCL range types check these too; the protocol's are the same.
         if Disk not in 0 .. Integer_64 (IP.Disk_Ordinal'Last)
           or else Ratio not in Integer_64 (IP.Inode_Ratio'First) .. Integer_64 (IP.Inode_Ratio'Last)
           or else ESP not in Integer_64 (IP.ESP_MiB'First) .. Integer_64 (IP.ESP_MiB'Last)
           or else Source'Length not in IP.Source_Length
           or else Model'Length > DId.Maximum_Text or else Serial'Length > DId.Maximum_Text
           or else DId.To_Text (Model).Length /= Model'Length
           or else DId.To_Text (Serial).Length /= Serial'Length
           or else (for some C of Source => C not in ' ' .. '~')
           or else Blocks < 0 or else Block_Bytes not in 0 .. Integer_64 (Unsigned_32'Last)
         then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument, "an Install_Request field is out of range",
               "disk 0-15, inode_ratio 8192-16777216, esp_mib 260-65536, a source path of " &
               "at most 255 characters, a printable model and serial of at most 64");
            return;
         end if;
         Request.Disk := IP.Disk_Ordinal (Disk);
         Request.Confirmation := (Model => DId.To_Text (Model), Serial => DId.To_Text (Serial));
         Request.Confirm_Blocks := Unsigned_64 (Blocks);
         Request.Confirm_Block_Size := Unsigned_32 (Block_Bytes);
         Request.Ratio := IP.Inode_Ratio (Ratio);
         Request.ESP_Size := IP.ESP_MiB (ESP);
         Request.Length := Source'Length;
         Request.Source (1 .. Source'Length) := Source;
      end;
      CCL_Installer.Install (Request, Outcome, Files, Bytes, Proof, Result);
      if Result = CCL_Installer.Not_Granted then
         Reply.Why := NOT_GRANTED;
         return;
      elsif Result /= CCL_Installer.Answered then
         Reply.Why := UNAVAILABLE;
         return;
      end if;
      declare
         Image : CCL.Objects.Image := CCL.Objects.Empty (Contracts.Report);
         Step : CCL.Objects.Build_Result := CCL.Objects.Added;
         procedure Put (Cell : CCL.Objects.Cell) is
         begin
            if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
         end Put;
      begin
         Put (CCL.Objects.Product_Cell (Install_Interface.REPORT_FIELDS));
         Put (CCL.Objects.Variant_Cell (IP.Install_Outcome'Pos (Outcome) + 1));
         Put (CCL.Objects.Unit_Cell);
         Put (CCL.Objects.Variant_Cell (IP.Identity_Proof'Pos (Proof) + 1));
         Put (CCL.Objects.Unit_Cell);
         Put (CCL.Objects.Integer_Cell (Integer_64 (Unsigned_64'Min (Files, Unsigned_64 (Integer_64'Last)))));
         Put (CCL.Objects.Integer_Cell (Integer_64 (Unsigned_64'Min (Bytes, Unsigned_64 (Integer_64'Last)))));
         if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Contracts.Report) then
            Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
         else
            Reply.Why := UNAVAILABLE;
         end if;
      end;
   end Run_Install;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result) is
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then
         return;
      end if;
      case Install_Interface.Operation'Val (Binding - Install_Interface.FIRST_BINDING) is
         when Install_Interface.Disks => List_Disks (Reply);
         when Install_Interface.Install => Run_Install (Argument, Reply);
      end case;
   end Invoke;
end CCL_Install_Bindings;
