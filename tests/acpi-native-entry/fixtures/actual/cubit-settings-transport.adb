------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Config_Protocol;
with CuBit.Handles;

package body CuBit.Settings.Transport is
   use type System.Address;
   package CP renames CuBit.Config_Protocol;

   --  Config's data labels (CuBit.Config_Protocol.Operation, services/
   --  config) and replies.
   Get_Label    : constant Unsigned_32 := 16#0600#;
   Set_Label    : constant Unsigned_32 := 16#0601#;
   Delete_Label : constant Unsigned_32 := 16#0602#;
   List_Label   : constant Unsigned_32 := 16#0603#;
   Reply_OK       : constant Unsigned_32 := 16#F000#;
   Reply_Denied   : constant Unsigned_32 := 16#F007#;
   Reply_Missing  : constant Unsigned_32 := 16#F060#;
   --  List answers at most this many keys.
   Maximum_Names : constant := 256;

   Page_Bytes : constant := 4096;
   Buffer_Pages : constant := 2;
   Buffer : System.Address := System.Null_Address;
   Grant : CuBit.Memory_Grants.Grant_Reference;
   Lent : Boolean := False;

   --  Lend the buffer to Config, once (Through: the program's Config endpoint).
   procedure Ensure_Lent (Through : Endpoint);
   procedure Ensure_Lent (Through : Endpoint) is
      Raw, Aligned : Unsigned_64;
      OK : Boolean;
   begin
      if Lent then
         return;
      end if;
      if Buffer = System.Null_Address then
         --  One page more, to start on a page boundary.
         Raw := syscall (SYSCALL_SBRK, (Buffer_Pages + 1) * Page_Bytes);
         if Raw = Unsigned_64'Last then
            return;
         end if;
         Aligned := (Raw + Page_Bytes - 1) and not Unsigned_64 (Page_Bytes - 1);
         Buffer := To_Address (Integer_Address (Aligned));
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (CuBit.Handles.Services.Config.Handle (Through), Buffer, Buffer_Pages, True, Grant, OK);
      Lent := OK;
   end Ensure_Lent;

   function Status_Of (Label : Unsigned_32) return Status is
     (if Label = Reply_OK then OK
      elsif Label = Reply_Missing then Not_Found
      elsif Label = Reply_Denied then Refused
      else Failed);

   procedure Put (At_Offset : Natural; Item : String);
   procedure Put (At_Offset : Natural; Item : String) is
      Bytes : String (1 .. At_Offset + Item'Length) with Import, Address => Buffer;
   begin
      Bytes (At_Offset + 1 .. At_Offset + Item'Length) := Item;
   end Put;

   function Call (Through : Endpoint; Label : Unsigned_32; Key_Length, Value_Length : Natural)
     return Message;
   function Call (Through : Endpoint; Label : Unsigned_32; Key_Length, Value_Length : Natural)
     return Message
   is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (label => Label, length => 4, protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [Grant.slot, Grant.generation, Unsigned_64 (Key_Length), Unsigned_64 (Value_Length)];
      Msg.tag := capCall (CuBit.Handles.Services.Config.Handle (Through), Msg, CuBit.Messages.Wait_Forever);
      return Msg;
   end Call;

   procedure Get (Through : Endpoint; Key : String; Value : out Text; Result : out Status) is
      Reply : Message;
   begin
      Value := (others => <>);
      Result := Failed;
      if Key'Length not in 1 .. CP.Maximum_Key then
         return;
      end if;
      Ensure_Lent (Through);
      if not Lent then
         return;
      end if;
      Put (0, Key);
      Reply := Call (Through, Get_Label, Key'Length, 0);
      Result := Status_Of (Reply.tag.label);
      if Result = OK then
         if Reply.tag.length /= 1 or else Reply.words (0) > CP.Maximum_Value then
            Result := Failed;
            return;
         end if;
         declare
            Bytes : constant String (1 .. Natural (Reply.words (0))) with Import, Address => Buffer;
         begin
            Value.Length := Bytes'Length;
            Value.Data (1 .. Bytes'Length) := Bytes;
         end;
      end if;
   end Get;

   procedure Set (Through : Endpoint; Key : String; Value : String; Result : out Status) is
   begin
      Result := Failed;
      if Key'Length not in 1 .. CP.Maximum_Key or else Value'Length > CP.Maximum_Value then
         return;
      end if;
      Ensure_Lent (Through);
      if not Lent then
         return;
      end if;
      Put (0, Key);
      Put (Key'Length, Value);
      Result := Status_Of (Call (Through, Set_Label, Key'Length, Value'Length).tag.label);
   end Set;

   procedure Delete (Through : Endpoint; Key : String; Result : out Status) is
   begin
      Result := Failed;
      if Key'Length not in 1 .. CP.Maximum_Key then
         return;
      end if;
      Ensure_Lent (Through);
      if not Lent then
         return;
      end if;
      Put (0, Key);
      Result := Status_Of (Call (Through, Delete_Label, Key'Length, 0).tag.label);
   end Delete;

   procedure List (Through : Endpoint; Prefix : String; Names : out Text; Count : out Natural;
                   Result : out Status)
   is
      Reply : Message;
   begin
      Names := (others => <>);
      Count := 0;
      Result := Failed;
      if Prefix'Length > CP.Maximum_Key then
         return;
      end if;
      Ensure_Lent (Through);
      if not Lent then
         return;
      end if;
      Put (0, Prefix);
      Reply := Call (Through, List_Label, Prefix'Length, 0);
      Result := Status_Of (Reply.tag.label);
      if Result = OK then
         if Reply.tag.length /= 1 or else Reply.words (0) > Maximum_Names then
            Result := Failed;
            return;
         end if;
         Count := Natural (Reply.words (0));
         --  Count NUL-terminated names, within the value's room.
         declare
            Bytes : constant String (1 .. CP.Maximum_Value) with Import, Address => Buffer;
            Seen : Natural := 0;
            Last : Natural := 0;
         begin
            while Seen < Count and then Last < Bytes'Last loop
               Last := Last + 1;
               if Bytes (Last) = ASCII.NUL then
                  Seen := Seen + 1;
               end if;
            end loop;
            Names.Length := Last;
            Names.Data (1 .. Last) := Bytes (1 .. Last);
            Count := Seen;
         end;
      end if;
   end List;

end CuBit.Settings.Transport;
