pragma Ada_2022;
package body Test_Snapshot is

   function Raw (D : F.Catalog.Descriptor) return F.Bytes is
      Data : F.Bytes (1 .. D.Extent);
      Sum : F.Byte := 0;
      Length_First : constant Positive := 5;
      Revision_Byte : constant Positive := 9;
      Checksum_Byte : constant Positive := 10;
      Prime : constant := 251;
   begin
      for I in Data'Range loop Data (I) := F.Byte ((I * 7 + D.Extent) mod Prime); end loop;
      for I in D.Name'Range loop Data (I) := Character'Pos (D.Name (I)); end loop;
      for I in 0 .. 3 loop
         Data (Length_First + I) := F.Byte (Shift_Right (Unsigned_32 (D.Extent), 8 * I) mod 256);
      end loop;
      Data (Revision_Byte) := D.Revision;
      Data (Checksum_Byte) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (Checksum_Byte) := 0 - Sum;
      return Data;
   end Raw;

   procedure Append (D : F.Catalog.Descriptor) is
      Good : Boolean;
   begin
      S.Append (State, D, Raw (D), Good);
      if not Good then raise Program_Error with "table refused: " & D.Name; end if;
   end Append;

   procedure Begin_Partial is
   begin
      S.Begin_Snapshot (State, Table_Total);
      Append (Large);
      Append (Small);
   end Begin_Partial;

   procedure Finish is
   begin
      Append (Tiny);
      S.Seal (State);
   end Finish;

   function Ready return Boolean is (S."=" (S.Current (State), S.Ready));
   function Count return Natural is (S.Count (State));
   function Describe (Index : Natural) return S.Metadata_Result is (S.Describe (State, Index));

   procedure Copy_Part
     (Index, Offset : Natural; Destination : out F.Bytes;
      Result : out S.Transfers.Transfer_Result) is
   begin
      S.Transfers.Copy_Part (State, Index, Offset, Destination, Result);
   end Copy_Part;

   procedure Write_User (Destination : Unsigned_64; Source : F.Bytes; Success : out Boolean) is
   begin
      Success := False;
      if Destination < User_Base
        or else Destination - User_Base > Unsigned_64 (User_Bytes)
        or else Unsigned_64 (Source'Length) > Unsigned_64 (User_Bytes) - (Destination - User_Base)
      then
         return;
      end if;
      for I in Source'Range loop
         if Writes = Write_Limit then return; end if;
         User (Natural (Destination - User_Base) + 1 + (I - Source'First)) := Source (I);
         Writes := Writes + 1;
      end loop;
      Success := True;
   end Write_User;

   procedure Write_Reply (Item : ABI.Reply; Success : out Boolean) is
   begin
      Success := not Reply_Refused;
      if Success then
         Last_Reply := Item;
         Replies := Replies + 1;
      end if;
   end Write_Reply;
end Test_Snapshot;
