--  Hosted tests for CuBit.Volume_Descriptions (docs/filesystem-protocol-v2.md
--  step 7): round trips, every accepted record canonical (it re-encodes to
--  the same bytes), damaged and random records refused without faults.
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with CuBit.Volume_Descriptions; use CuBit.Volume_Descriptions;

procedure Main is
   Failures, Checks, Accepted_Random : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 20 then
            Ada.Text_IO.Put_Line ("FAIL: " & What);
         end if;
      end if;
   end Check;

   subtype Small is Natural range 0 .. 1_000_000;
   package Random is new Ada.Numerics.Discrete_Random (Small);
   G : Random.Generator;
   function R (N : Positive) return Natural is (Random.Random (G) mod N);
   function R64 return Unsigned_64 is
     (Shift_Left (Unsigned_64 (R (1_000_000)), 40) xor Unsigned_64 (R (1_000_000)));

   function Make return Description is
      Item : Description;
   begin
      Item.Kind := Volume_Kind'Val (R (4));
      Item.Block := Shift_Left (Unsigned_32 (Smallest_Block), R (8));
      Item.Total_Blocks := R64;
      Item.Total_Inodes := R64;
      Item.Length := 1 + R (Maximum_Name_Bytes);
      for I in 1 .. Item.Length loop
         Item.Name (I) := Unsigned_8 (Character'Pos ('a') + R (26));
      end loop;
      if Item.Kind in ISO_9660 | Boot_Archive then
         Item.Flags := Read_Only;
      else
         Item.Free_Blocks := (if Item.Total_Blocks = 0 then 0 else R64 mod (Item.Total_Blocks + 1));
         Item.Releasing_Blocks := (if R (2) = 0 then 0
                                   else (Item.Total_Blocks - Item.Free_Blocks) / 2);
         Item.Free_Inodes := (if Item.Total_Inodes = 0 then 0 else R64 mod (Item.Total_Inodes + 1));
         Item.Flags := Unsigned_8 (R (2)) * Read_Only + Unsigned_8 (R (2)) * Durable_Flush
           + (if Item.Kind = Ext3 then Unsigned_8 (R (2)) * Journaled else 0);
      end if;
      return Item;
   end Make;

   Item, Back : Description;
   Bytes, Again : Record_Image;
   OK : Boolean;
begin
   Random.Reset (G, 2026_1009);
   for Round in 1 .. 50_000 loop
      Item := Make;
      Check (Valid (Item), "generated valid");
      Encode (Item, Bytes);
      Decode (Bytes, Back, OK);
      Check (OK and then Back = Item, "round trip");
      --  One byte changed: refused, or still canonical.
      declare
         Damaged : Record_Image := Bytes;
         At_Byte : constant Record_Index := R (Record_Bytes);
      begin
         Damaged (At_Byte) := Damaged (At_Byte) xor Unsigned_8 (1 + R (255));
         Decode (Damaged, Back, OK);
         if OK then
            Encode (Back, Again);
            Check (Again = Damaged, "accepted damage is canonical");
         end if;
      end;
      --  Random bytes never fault; whatever is accepted is canonical.
      for B of Bytes loop
         B := Unsigned_8 (R (256));
      end loop;
      if Round mod 2 = 0 then
         Bytes (Version_At) := Version;
         Bytes (Version_At + 1) := 0;
         Bytes (Kind_At) := Unsigned_8 (1 + R (4));
         Bytes (Name_Length_At + 1 .. Block_Size_At - 1) := [others => 0];
      end if;
      Decode (Bytes, Back, OK);
      if OK then
         Accepted_Random := Accepted_Random + 1;
         Encode (Back, Again);
         Check (Again = Bytes, "accepted random is canonical");
      end if;
   end loop;
   --  Specific refusals.
   Item := Make;
   Item.Kind := Ext2;
   Item.Flags := 0;
   Encode (Item, Bytes);
   Again := Bytes;
   Again (Flags_At) := Journaled;
   Decode (Again, Back, OK);
   Check (not OK, "journaled ext2 refused");
   Again := Bytes;
   Again (Name_At) := Character'Pos ('/');
   Decode (Again, Back, OK);
   Check (not OK, "slash in name refused");
   Again := Bytes;
   Again (Name_Length_At) := 0;
   Decode (Again, Back, OK);
   Check (not OK, "empty name refused");
   Again := Bytes;
   Again (Block_Size_At) := 1;   --  not a power of two
   Decode (Again, Back, OK);
   Check (not OK, "odd block size refused");
   Ada.Text_IO.Put_Line ("VOLUME-DESCRIPTIONS:" &
     (if Failures = 0 then Checks'Image & " checks PASS" else Failures'Image & " of" & Checks'Image & " FAIL")
     & " (random records accepted:" & Accepted_Random'Image & ")");
end Main;
