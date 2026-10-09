with Owner_Record_Slabs;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
procedure Owner_Record_Slabs_Test is
   type Page is array (1 .. 512) of Unsigned_64 with Alignment => 4096;
   RAM : array (1 .. 128) of aliased Page;
   Owners : array (RAM'Range) of Unsigned_64 := [others => 0];
   Counts : array (1 .. 4) of Natural := [others => 0];
   Fail : Boolean := False;
   procedure Allocate (Owner : Unsigned_64; Result : out Address; OK : out Boolean) is
   begin
      Result := Null_Address;
      OK := False;
      if Fail then return; end if;
      for I in Owners'Range loop
         if Owners (I) = 0 then
            Owners (I) := Owner;
            Counts (Natural (Owner)) := Counts (Natural (Owner)) + 1;
            Result := RAM (I)'Address;
            OK := True;
            return;
         end if;
      end loop;
   end Allocate;
   procedure Free (Value : Address) is
      Index : constant Natural := Natural ((To_Integer (Value) - To_Integer (RAM'Address)) / 4096) + 1;
   begin
      pragma Assert (Index in Owners'Range and then Owners (Index) /= 0);
      Counts (Natural (Owners (Index))) := Counts (Natural (Owners (Index))) - 1;
      Owners (Index) := 0;
   end Free;
   package S is new Owner_Record_Slabs (Unsigned_64, 64, Allocate, Free);
   use type S.Arena;
   use type S.Reference;
   A, B : S.Arena;
   Items : array (1 .. 130) of S.Reference;
   Other : S.Reference;
   Queued, Moved : S.List;
   Popped : S.Reference;
   OK, Rejected : Boolean;
begin
   Fail := True;
   S.Open (1, 4096, A, OK); pragma Assert (not OK and A = S.No_Arena);
   pragma Assert (S.Metadata_Bytes = 0);
   Fail := False;
   S.Open (1, 4096, A, OK); pragma Assert (OK and Counts (1) = 1);
   S.Reserve (A, 1, 4096, Other, OK); pragma Assert (not OK and Other = null);
   Fail := True;
   S.Reserve (A, 1, 8192, Other, OK); pragma Assert (not OK and Other = null);
   pragma Assert (S.Metadata_Bytes = 4096 and Counts (1) = 1);
   Fail := False;
   for I in Items'Range loop
      S.Reserve (A, Unsigned_64 (I), 65536, Items (I), OK); pragma Assert (OK);
   end loop;
   pragma Assert (Counts (1) = 4 and S.Metadata_Bytes = 16384);
   for I in Items'Range loop pragma Assert (S.Value (Items (I)) = Unsigned_64 (I)); end loop;
   S.Push (Queued, Items (1));
   Rejected := False;
   begin S.Push (Queued, Items (1)); exception when Program_Error => Rejected := True; end;
   pragma Assert (Rejected);
   Rejected := False;
   begin S.Release (Items (1)); exception when Program_Error => Rejected := True; end;
   pragma Assert (Rejected and Items (1) /= null);
   Rejected := False;
   begin S.Set_Value (Items (1), 999); exception when Program_Error => Rejected := True; end;
   pragma Assert (Rejected and S.Value (Items (1)) = 1);
   Rejected := False;
   begin S.Move (Queued, Queued); exception when Program_Error => Rejected := True; end;
   pragma Assert (Rejected);
   for I in 2 .. Items'Last loop S.Push (Queued, Items (I)); end loop;
   S.Move (Queued, Moved);
   pragma Assert (S.Empty (Queued) and not S.Empty (Moved));
   S.Close (A); pragma Assert (A = S.No_Arena and Counts (1) = 4);
   S.Open (2, 65536, B, OK); pragma Assert (OK);
   S.Reserve (B, 999, 65536, Other, OK); pragma Assert (OK);
   for I in Items'Range loop
      S.Pop (Moved, Popped);
      pragma Assert (Popped = Items (I) and S.Value (Popped) = Unsigned_64 (I));
      S.Release (Popped);
      Items (I) := null;
   end loop;
   S.Pop (Moved, Popped); pragma Assert (Popped = null and S.Empty (Moved));
   pragma Assert (Counts (1) = 0 and Counts (2) = 2 and S.Value (Other) = 999);
   S.Release (Other); pragma Assert (Counts (2) = 1);
   for Round in 1 .. 2000 loop
      S.Reserve (B, Unsigned_64 (Round), 65536, Other, OK); pragma Assert (OK);
      S.Release (Other);
      pragma Assert (Counts (2) = 1 and S.Metadata_Bytes = 4096);
   end loop;
   S.Close (B);
   pragma Assert (S.Metadata_Bytes = 0 and Counts = [0, 0, 0, 0]);
   Ada.Text_IO.Put_Line ("PASS owner slabs: stable 130 records, separate incarnations, growth failure, close with live records, final-page refunds and 2000 bounded churn cycles");
end Owner_Record_Slabs_Test;
