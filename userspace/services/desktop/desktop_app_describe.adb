------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Compositor_Requests;
with Desktop_Endpoints;

package body Desktop_App_Describe is
   package CR renames Compositor_Requests;
   package MG renames CuBit.Memory_Grants;

   Page_Bytes : constant := AN.Page_Bytes;
   Flight : CR.State;
   Grant : MG.Grant_Reference;
   Page_Address : System.Address := System.Null_Address;
   Attempted, Initialized : Boolean := False;
   Asked : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;

   procedure Quarantine is
   begin
      CR.Quarantine (Flight);
      debugPrint ("desktop: app describe uncertain; page retained" & ASCII.LF);
   end Quarantine;

   procedure Initialize is
      Raw, Aligned : Unsigned_64;
      OK : Boolean;
   begin
      if Attempted then return; end if;
      Attempted := True;
      --  Two pages' worth, so one page-aligned page lies inside.
      Raw := syscall (SYSCALL_SBRK, 2 * Page_Bytes);
      if Raw > Unsigned_64'Last - 2 * Page_Bytes then return; end if;
      Aligned := (Raw + (Page_Bytes - 1)) and not Unsigned_64 (Page_Bytes - 1);
      Page_Address := To_Address (Integer_Address (Aligned));
      MG.Create_Via_Capability (Desktop_Endpoints.Process_Manager, Page_Address, 1, True, Grant, OK);
      Initialized := OK;
      if not OK then
         debugPrint ("desktop: app describe page grant to procmgr failed" & ASCII.LF);
      end if;
   end Initialize;

   function Ready return Boolean is (Initialized and then CR.Available (Flight));
   function Unavailable return Boolean is
     ((Attempted and then not Initialized) or else CR.Faulted (Flight));
   function Token return Unsigned_64 is (CR.Token (Flight));

   procedure Submit
     (Owner : CuBit.Process_IDs.Process_ID; Sequence : in out Unsigned_64; Submitted : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
      New_Token : Unsigned_64;
      Accepted : Boolean;
   begin
      Submitted := False;
      if not Ready or else Owner = CuBit.Process_IDs.No_Process then return; end if;
      Msg.tag := (label => AN.Describe_Operation, length => AN.Request_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words (AN.Grant_Word) := CuBit.Grant_References.Encode (Grant);
      Msg.words (AN.Process_Word) := CuBit.Process_IDs.To_Word (Owner);
      CR.Allocate (Sequence, New_Token);
      CR.Begin_Request (Flight, New_Token, Accepted);
      if not Accepted then Quarantine; return; end if;
      Asked := Owner;
      if capSubmit (Desktop_Endpoints.Process_Manager, Msg, New_Token) then
         Submitted := True;
      else
         Quarantine;
      end if;
   end Submit;

   procedure Collect (C : CompletionEntry; Result : out Answer) is
      Envelope : constant Boolean := C.valid and then C.status = COMPLETION_OK and then
        C.msg.tag.length = 1 and then C.msg.tag.protocol = 0 and then C.msg.tag.reserved = 0;
      Answered : constant Boolean := Envelope and then
        C.msg.tag.label in AN.Status'Enum_Rep (AN.OK) | AN.Status'Enum_Rep (AN.Denied)
                         | AN.Status'Enum_Rep (AN.Invalid_Request) | AN.Status'Enum_Rep (AN.Unknown)
        and then C.msg.words (0) = Unsigned_64 (C.msg.tag.label);
   begin
      Result := (Owner => Asked, others => <>);
      --  Every answer follows procmgr's return of the page (or its never
      --  taking it): only a malformed one leaves its use uncertain.
      CR.Complete (Flight, C.token, Answered);
      if not CR.Available (Flight) then Quarantine; return; end if;
      if not Answered or else C.msg.tag.label /= AN.Status'Enum_Rep (AN.OK) then
         return;
      end if;
      declare
         Page : array (0 .. AN.Record_Bytes - 1) of Unsigned_8 with Import, Address => Page_Address;
         Name_Length : constant Natural := Natural (Page (AN.Name_Length_Offset));
         Identity_Length : constant Natural := Natural (Page (AN.Identity_Length_Offset));
      begin
         if Name_Length > AN.Maximum_Name_Bytes or else Identity_Length > AN.Maximum_Identity_Bytes then
            return;
         end if;
         Result.Known := True;
         for J in 1 .. Identity_Length loop
            Result.Identity (J) := Character'Val (Page (AN.Identity_Offset + J - 1));
         end loop;
         Result.Identity_Length := Identity_Length;
         declare
            Name : String (1 .. Name_Length);
         begin
            for J in Name'Range loop
               Name (J) := Character'Val (Page (AN.Name_Offset + J - 1));
            end loop;
            if Name_Length > 0 and then AN.Valid_Name (Name) then
               Result.Name (1 .. Name_Length) := Name;
               Result.Name_Length := Name_Length;
            end if;
         end;
      end;
   end Collect;
end Desktop_App_Describe;
