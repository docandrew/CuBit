with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Handles;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CCL_Native_Bindings;

--  installer.svc over CuBit.Installer_Protocol: one page of this
--  program's memory, granted to the service once, carries each request's
--  record and each disk's description.
package body CCL_Installer is
   use Interfaces;

   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer : System.Address := System.Null_Address;
   Ready : Boolean := False;

   procedure Prepare (Result : out Result_Kind) is
      Raw : Unsigned_64;
   begin
      if not CuBit.Handles.Valid (CCL_Native_Bindings.Installer) then
         Result := Not_Granted;
         return;
      end if;
      if not Ready then
         Raw := syscall (SYSCALL_SBRK, 2 * IP.Page_Bytes);
         if Raw = 0 or else Raw = Unsigned_64'Last then
            Result := Unavailable;
            return;
         end if;
         Buffer := To_Address (Integer_Address ((Raw + IP.Page_Bytes - 1) and not (IP.Page_Bytes - 1)));
         CuBit.Memory_Grants.Create_Via_Capability
           (CCL_Native_Bindings.Installer, Buffer, 1, True, Loan, Ready);
      end if;
      Result := (if Ready then Answered else Unavailable);
   end Prepare;

   function Call (Label : Unsigned_32; Words : MessageWords) return Message is
      Msg : Message := (tag => (label => Label, length => IP.Request_Words, protocol => 0,
                                ipc => <>, reserved => 0),
                        authorityTag => 0, words => Words);
   begin
      Msg.tag := capCall (CCL_Native_Bindings.Installer, Msg, Wait_Forever);
      return Msg;
   end Call;

   procedure Disk_Info
     (Number : IP.Disk_Ordinal; Item : out IP.Disk_Record; Present : out Boolean;
      Result : out Result_Kind)
   is
      Reply : Message;
      Valid : Boolean;
   begin
      Item := (others => <>);
      Present := False;
      Prepare (Result);
      if Result /= Answered then
         return;
      end if;
      Reply := Call (IP.Disk_Info, [Unsigned_64 (Number), Loan.slot, Loan.generation, 0]);
      if Reply.tag.label = IP.Reply_No_Device then
         return;
      elsif Reply.tag.label /= IP.Reply_OK then
         Result := Unavailable;
         return;
      end if;
      declare
         Shared : constant IP.Page with Import, Address => Buffer;
         Copy : constant IP.Page := Shared;
      begin
         IP.Decode_Disk (Copy, Item, Valid);
      end;
      Present := Valid;
      if not Valid then
         Result := Unavailable;
      end if;
   end Disk_Info;

   procedure Install
     (Request : IP.Install_Request; Outcome : out IP.Install_Outcome;
      Files, Bytes : out Interfaces.Unsigned_64; Proof : out IP.Identity_Proof;
      Result : out Result_Kind)
   is
      Reply : Message;
   begin
      Outcome := IP.Bad_Request;
      Files := 0;
      Bytes := 0;
      Proof := IP.Serial_Number;
      Prepare (Result);
      if Result /= Answered then
         return;
      end if;
      declare
         Shared : IP.Page with Import, Address => Buffer;
      begin
         Shared := IP.Encode_Install (Request);
      end;
      Reply := Call (IP.Install, [Loan.slot, Loan.generation, 0, 0]);
      if Reply.tag.label /= IP.Reply_OK
        or else Reply.words (0) > IP.Install_Outcome'Enum_Rep (IP.Install_Outcome'Last)
      then
         Result := Unavailable;
         return;
      end if;
      Outcome := IP.Install_Outcome'Enum_Val (Reply.words (0));
      Files := Reply.words (1);
      Bytes := Reply.words (2);
      Proof := (if Reply.words (3) = IP.Identity_Proof'Enum_Rep (IP.Model_And_Geometry)
                then IP.Model_And_Geometry else IP.Serial_Number);
   end Install;
end CCL_Installer;
