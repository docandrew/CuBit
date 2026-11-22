pragma Ada_2022;
with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Process_List;
with CuBit.Process_Observer;

package body CuBit.Process_Observer_Lists is
   package PO renames CuBit.Process_Observer;
   package PL renames CuBit.Process_List;

   --  The page procmgr fills (one at a time).
   Page : array (0 .. PO.Page_Bytes - 1) of Unsigned_8 := [others => 0]
     with Alignment => PO.Page_Bytes;

   function Signed is new Ada.Unchecked_Conversion (Unsigned_16, Integer_16);

   --  Reply words: records written, processes in total.
   Written_Word : constant := 1;
   Total_Word   : constant := 2;

   function Little (Offset, Bytes : Natural) return Unsigned_64;
   function Little (Offset, Bytes : Natural) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Bytes - 1 loop
         Value := Shift_Left (Value, 8) or Unsigned_64 (Page (Offset + I));
      end loop;
      return Value;
   end Little;

   function Read
     (Observer : CuBit.Handles.Endpoint; Into : System.Address; Bytes : Unsigned_64)
      return Unsigned_64
   is
      Capacity : constant Unsigned_64 := Bytes / PL.Entry_Bytes;
      Count : Unsigned_64 := 0;
      Written : Natural;
      Total : Unsigned_64;
   begin
      loop
         declare
            Loan : CuBit.Memory_Grants.Grant_Reference;
            Lent, Revoked : Boolean;
            Request : Message := NULL_MESSAGE;
            Reply_Tag : MessageTag;
         begin
            CuBit.Memory_Grants.Create_Via_Capability (Observer, Page'Address, 1, True, Loan, Lent);
            if not Lent then
               return Unsigned_64'Last;
            end if;
            Request.tag := (label => PO.List_Label, length => PO.Paged_Request_Words,
                            protocol => 0, ipc => <>, reserved => 0);
            Request.words (0) := CuBit.Grant_References.Encode (Loan);
            Request.words (PO.First_Record_Word) := Count;
            Reply_Tag := capCall (Observer, Request, Wait_Forever);
            CuBit.Memory_Grants.Revoke (Loan, Revoked);
            if Reply_Tag.label /= Unsigned_32 (PO.Status'Enum_Rep (PO.OK)) then
               return (if Count = 0 then Unsigned_64'Last else Count);
            end if;
            Written := Natural (Unsigned_64'Min (Request.words (Written_Word), PO.Page_Records));
            Total := Request.words (Total_Word);
         end;
         for Index in 0 .. Written - 1 loop
            exit when Count >= Capacity;
            declare
               Base : constant Natural := Index * PO.Record_Bytes;
               Name_Length : constant Natural :=
                 Natural'Min (Natural (Page (Base + PO.Name_Length_Offset)), PL.Name_Bytes);
               Item : PL.Process_Entry;
               Target : PL.Process_Entry
                 with Import, Address => Into + Storage_Offset (Count * PL.Entry_Bytes);
            begin
               Item.Identity := CuBit.Process_IDs.From_Word (Little (Base + PO.Pid_Offset, PO.Pid_Bytes));
               Item.State := Page (Base + PO.State_Offset);
               Item.CPU := Page (Base + PO.CPU_Offset);
               Item.Priority := Signed (Unsigned_16 (Little (Base + PO.Priority_Offset, 2)));
               Item.Frames := Unsigned_32 (Little (Base + PO.Frames_Offset, 4));
               for I in 1 .. Name_Length loop
                  Item.Name (I) := Character'Val (Page (Base + PO.Name_Offset + I - 1));
               end loop;
               Target := Item;
               Count := Count + 1;
            end;
         end loop;
         exit when Written = 0 or else Count >= Total or else Count >= Capacity;
      end loop;
      return Count;
   end Read;

end CuBit.Process_Observer_Lists;
