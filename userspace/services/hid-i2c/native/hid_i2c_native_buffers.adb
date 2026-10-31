pragma Ada_2022;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Process_IDs;
with Interfaces;
package body HID_I2C_Native_Buffers is
   package M renames CuBit.Messages;
   package G renames CuBit.Grant_References;
   package Addresses renames System.Storage_Elements;
   package Access_Bytes is new System.Address_To_Access_Conversions (D.Byte);
   use type W.Word, Interfaces.Unsigned_16, M.IPC_Flags;
   Maximum_Pages : constant W.Word := (W.Word (W.Count'Last) + G.Page_Bytes - 1) / G.Page_Bytes;
   procedure Allocate (Bytes : W.Word; Address : out W.Word; OK : out Boolean) is
      Raw : W.Word;
   begin
      Address := 0; OK := False;
      if Bytes = 0 or else Bytes mod G.Page_Bytes /= 0 or else Bytes > Maximum_Pages * G.Page_Bytes then return; end if;
      Raw := M.syscall (M.SYSCALL_ALLOCATE_OWNED_MEMORY, Bytes);
      if Raw = 0 or else Raw = W.Word'Last then return; end if;
      Address := Raw; OK := True;
   end Allocate;
   procedure Release (Address, Bytes : W.Word; OK : out Boolean) is
   begin OK := M.syscall (M.SYSCALL_RELEASE_OWNED_MEMORY, Address, Bytes) = 0; end Release;
   procedure Clear (Address, Bytes : W.Word) is
   begin
      if Bytes = 0 then return; end if;
      for N in W.Word range 0 .. Bytes - 1 loop
         Access_Bytes.To_Pointer (Addresses.To_Address (Addresses.Integer_Address (Address + N))).all := 0;
      end loop;
   end Clear;
   procedure Store (Address : W.Word; Data : D.Bytes) is
      Offset : W.Word := 0;
   begin
      for B of Data loop
         Access_Bytes.To_Pointer (Addresses.To_Address (Addresses.Integer_Address (Address + Offset))).all := B;
         Offset := Offset + 1;
      end loop;
   end Store;
   procedure Load (Address : W.Word; Data : out D.Bytes) is
      Offset : W.Word := 0;
   begin
      for B of Data loop
         B := Access_Bytes.To_Pointer (Addresses.To_Address (Addresses.Integer_Address (Address + Offset))).all;
         Offset := Offset + 1;
      end loop;
   end Load;
   procedure Create (Address, Pages : W.Word; Writable : Boolean;
                     Reference : out G.Reference; Status : out HID_I2C_Grant_Creation.Status) is
      OK : Boolean;
   begin
      Reference := (others => <>); Status := HID_I2C_Grant_Creation.Refused;
      if Pages not in 1 .. Maximum_Pages then return; end if;
      CuBit.Memory_Grants.Create_Via_Capability (Controller,
        Addresses.To_Address (Addresses.Integer_Address (Address)), Natural (Pages),
        Writable, Reference, OK, notify => False);
      Status := (if OK then HID_I2C_Grant_Creation.Created else HID_I2C_Grant_Creation.Uncertain);
   end Create;
   procedure Receive
     (S : in out Client.State; Entry_Value : M.CompletionEntry;
      Now : W.Word; Clock_Available : Boolean;
      Result : out Client.Transactions.Completion_Result)
   is
      Delivery : Client.Transactions.Delivery_Status;
      Length : Natural := Natural (Entry_Value.msg.tag.length);
   begin
      Result := Client.Transactions.Unrelated;
      if not Entry_Value.valid then return; end if;
      case Entry_Value.status is
         when M.COMPLETION_OK => Delivery := Client.Transactions.Delivered;
         when M.COMPLETION_TARGET_DIED => Delivery := Client.Transactions.Target_Died;
         when M.COMPLETION_CANCELLED => Delivery := Client.Transactions.Cancelled;
         when M.COMPLETION_QUEUE_OVERFLOW => Delivery := Client.Transactions.Queue_Overflow;
         when others => Delivery := Client.Transactions.Transport_Error;
      end case;
      if Length > Entry_Value.msg.words'Length or else Entry_Value.msg.tag.reserved /= 0 or else Entry_Value.msg.tag.ipc /= M.No_IPC_Flags then Length := 0; end if;
      declare
         Words : W.Message_Words (1 .. Length);
      begin
         for N in Words'Range loop Words (N) := Entry_Value.msg.words (N - 1); end loop;
         Client.Receive (S, Entry_Value.token,
           CuBit.Process_IDs.From_Word (Entry_Value.from), Delivery,
           Entry_Value.msg.tag.label, W.Word (Entry_Value.msg.tag.protocol),
           Words, Now, Clock_Available, Result);
      end;
   end Receive;
end HID_I2C_Native_Buffers;
