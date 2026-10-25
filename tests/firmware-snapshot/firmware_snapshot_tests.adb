pragma Ada_2022;
--  Hosted tests of FIRMWARE_SNAPSHOT (Firmware_Snapshot_ABI) and the
--  kernel's service body (Firmware_Snapshot_Service) over a snapshot built
--  here, with a caller memory that can refuse after a prefix.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with Firmware_Snapshot_ABI;
with Firmware_Snapshot_Service;
with Test_Snapshot;

procedure Firmware_Snapshot_Tests is
   package ABI renames Firmware_Snapshot_ABI;
   package T renames Test_Snapshot;
   package F renames Firmware_Tables;
   use type ABI.Outcome, ABI.Operation, ABI.Request, ABI.Reply, ABI.Catalog,
            ABI.Description, ABI.Copy_Plan, ABI.Reply_Index, F.Bytes;

   package Service is new Firmware_Snapshot_Service
     (Snapshot_Ready => T.Ready, Table_Count => T.Count, Describe => T.Describe,
      Copy_Part => T.Copy_Part, Write_User => T.Write_User, Write_Reply => T.Write_Reply);

   type Word_List is array (Positive range <>) of Unsigned_64;
   Checks, Failures : Natural := 0;
   procedure Check (Pass : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Pass then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   Info_Op     : constant Unsigned_64 := ABI.Operation'Enum_Rep (ABI.Catalog_Info);
   Describe_Op : constant Unsigned_64 := ABI.Operation'Enum_Rep (ABI.Describe_Table);
   Copy_Op     : constant Unsigned_64 := ABI.Operation'Enum_Rep (ABI.Copy_Table_Part);

   function Serve (Words : ABI.Request_Words) return ABI.Outcome is
      Result : ABI.Outcome;
   begin
      T.Last_Reply := ABI.Empty_Reply;
      Service.Serve (Words, Result);
      return Result;
   end Serve;

   function Copy_Words (Table, Offset, Capacity : Unsigned_64;
                        Destination : Unsigned_64 := T.User_Base) return ABI.Request_Words is
     ((Op => Copy_Op, Table => Table, Offset => Offset, Destination => Destination,
       Capacity => Capacity));

   ---------------------------------------------------------------------------
   procedure Wire_Codes is
   begin
      for O in ABI.Outcome loop
         Check (ABI.To_Outcome (ABI.Outcome_Word (O)) = O, "outcome round trip");
      end loop;
      Check (ABI.To_Outcome (0) = ABI.Malformed, "zero is no outcome");
      Check (ABI.To_Outcome (9) = ABI.Malformed, "past the last outcome");
      Check (ABI.To_Outcome (Unsigned_64'Last) = ABI.Malformed, "full-width outcome word");
   end Wire_Codes;

   ---------------------------------------------------------------------------
   procedure Request_Decoding is
      Item : ABI.Request;
      Valid : Boolean;
      procedure Refused (W : ABI.Request_Words; Name : String) is
      begin
         ABI.Decode (W, Item, Valid);
         Check (not Valid and then Item = ABI.Request'(others => <>), "decode refuses " & Name);
         Check (Serve (W) = ABI.Malformed, "serve refuses " & Name);
      end Refused;
      procedure Accepted (R : ABI.Request; Name : String) is
      begin
         ABI.Decode (ABI.Encode (R), Item, Valid);
         Check (Valid and then Item = R, "round trip " & Name);
      end Accepted;
   begin
      Accepted ((Op => ABI.Catalog_Info, others => <>), "info");
      Accepted ((Op => ABI.Describe_Table, Table => 7, others => <>), "describe");
      Accepted ((ABI.Copy_Table_Part, Unsigned_32'Last, Unsigned_64'Last, 16#7000#, 4_096),
                "copy at full width");
      Accepted ((ABI.Copy_Table_Part, 1, 0, Unsigned_64'Last - 4_096, 4_096), "copy to the top");
      Refused ((others => 0), "operation zero");
      Refused ((Op => 4, others => 0), "unknown operation");
      Refused ((Op => Unsigned_64'Last, others => 0), "full-width operation");
      Refused ((Op => Info_Op, Table => 1, others => 0), "info table word");
      Refused ((Op => Info_Op, Offset => 1, others => 0), "info offset word");
      Refused ((Op => Info_Op, Destination => 1, others => 0), "info destination word");
      Refused ((Op => Info_Op, Capacity => 1, others => 0), "info capacity word");
      Refused ((Op => Describe_Op, Table => 1, Offset => 1, others => 0), "describe offset word");
      Refused ((Op => Describe_Op, Table => 1, Destination => 1, others => 0), "describe destination");
      Refused ((Op => Describe_Op, Table => 1, Capacity => 1, others => 0), "describe capacity");
      Refused ((Op => Describe_Op, Table => 2 ** 32, others => 0), "table past 32 bits");
      Refused (Copy_Words (2 ** 32 + 1, 0, 16), "copy table past 32 bits (not narrowed)");
      Refused (Copy_Words (1, 0, 4_097), "capacity past Chunk_Max");
      Refused (Copy_Words (1, 0, Unsigned_64'Last), "full-width capacity");
      Refused (Copy_Words (1, 0, 16, Unsigned_64'Last - 15 + 1), "wrapping destination");
   end Request_Decoding;

   ---------------------------------------------------------------------------
   procedure Plans is
   begin
      Check (ABI.Plan_Copy (10, 11, 4) = (ABI.Beyond_End, 0), "plan beyond end");
      Check (ABI.Plan_Copy (10, Unsigned_64'Last, 4) = (ABI.Beyond_End, 0), "plan full-width offset");
      Check (ABI.Plan_Copy (10, 9, 0) = (ABI.Empty_Before_End, 0), "plan empty before end");
      Check (ABI.Plan_Copy (10, 10, 0) = (ABI.Ok, 0), "plan EOF empty");
      Check (ABI.Plan_Copy (10, 10, 4) = (ABI.Ok, 0), "plan EOF");
      Check (ABI.Plan_Copy (10, 8, 4) = (ABI.Ok, 2), "plan tail");
      Check (ABI.Plan_Copy (10, 0, 4) = (ABI.Ok, 4), "plan full chunk");
   end Plans;

   ---------------------------------------------------------------------------
   procedure Replies is
      C : ABI.Catalog;
      D : ABI.Description;
      Copied : ABI.Capacity_Bytes;
      Valid : Boolean;
      Good_Info : constant ABI.Catalog := (True, 3, 65_573, 65_725);
      Good_Description : constant ABI.Description := (5, [68, 83, 68, 84], 2, 65_573);
      W : ABI.Reply;
   begin
      ABI.Decode_Info (ABI.Encode_Info (Good_Info), C, Valid);
      Check (Valid and then C = Good_Info, "info reply round trip");
      ABI.Decode_Info (ABI.Encode_Info ((others => <>)), C, Valid);
      Check (Valid and then not C.Ready, "unready info reply");
      W := ABI.Encode_Info (Good_Info);
      W (ABI.Info_Version) := 2;
      ABI.Decode_Info (W, C, Valid);
      Check (not Valid and then C = ABI.Catalog'(others => <>), "info version refused");
      W := ABI.Encode_Info (Good_Info);
      W (ABI.Info_Ready) := 2;
      ABI.Decode_Info (W, C, Valid);
      Check (not Valid, "info ready word refused");
      W := ABI.Encode_Info (Good_Info);
      W (ABI.Info_Chunk) := 8_192;
      ABI.Decode_Info (W, C, Valid);
      Check (not Valid, "info chunk word refused");
      W := ABI.Encode_Info (Good_Info);
      W (ABI.Info_Count) := 2 ** 32;
      ABI.Decode_Info (W, C, Valid);
      Check (not Valid, "info count past 32 bits refused");
      W := ABI.Encode_Info ((others => <>));
      W (ABI.Info_Count) := 1;
      ABI.Decode_Info (W, C, Valid);
      Check (not Valid, "unready inventory refused");
      W := ABI.Encode_Info (Good_Info);
      W (ABI.Info_Largest) := W (ABI.Info_Total) + 1;
      ABI.Decode_Info (W, C, Valid);
      Check (not Valid, "largest above total refused");

      ABI.Decode_Description (ABI.Encode_Description (Good_Description), 5, D, Valid);
      Check (Valid and then D = Good_Description, "description round trip");
      ABI.Decode_Description (ABI.Encode_Description (Good_Description), 6, D, Valid);
      Check (not Valid and then D = ABI.Description'(others => <>), "description of another table");
      for I in ABI.Describe_Used + 1 .. ABI.Reply_Index'Last loop
         W := ABI.Encode_Description (Good_Description);
         W (I) := 1;
         ABI.Decode_Description (W, 5, D, Valid);
         Check (not Valid, "description reserved word refused");
      end loop;
      W := ABI.Encode_Description (Good_Description);
      W (ABI.Describe_Signature) := 2 ** 32;
      ABI.Decode_Description (W, 5, D, Valid);
      Check (not Valid, "signature past four bytes refused");
      W := ABI.Encode_Description (Good_Description);
      W (ABI.Describe_Revision) := 256;
      ABI.Decode_Description (W, 5, D, Valid);
      Check (not Valid, "revision past a byte refused");

      ABI.Decode_Copy (ABI.Encode_Copy (17), 17, Copied, Valid);
      Check (Valid and then Copied = 17, "copy reply round trip");
      ABI.Decode_Copy (ABI.Encode_Copy (17), 16, Copied, Valid);
      Check (not Valid and then Copied = 0, "copied above capacity refused");
      for I in ABI.Copy_Used + 1 .. ABI.Reply_Index'Last loop
         W := ABI.Encode_Copy (1);
         W (I) := 1;
         ABI.Decode_Copy (W, 4, Copied, Valid);
         Check (not Valid, "copy reserved word refused");
      end loop;
   end Replies;

   ---------------------------------------------------------------------------
   procedure Unready is
      Before : Natural;
      C : ABI.Catalog;
      Valid : Boolean;
   begin
      --  Empty: no inventory, nothing to describe or copy.
      Check (Serve ((Op => Info_Op, others => 0)) = ABI.Ok, "empty info is Ok");
      ABI.Decode_Info (T.Last_Reply, C, Valid);
      Check (Valid and then not C.Ready and then C.Table_Count = 0, "empty snapshot not ready");
      Check (Serve ((Op => Describe_Op, Table => 1, others => 0)) = ABI.Not_Ready,
             "empty describe");
      Before := T.Writes;
      Check (Serve (Copy_Words (1, 0, 64)) = ABI.Not_Ready, "empty copy");
      Check (T.Writes = Before, "a refusal writes nothing");
      --  Building: the appended prefix is not observable.
      T.Begin_Partial;
      Check (Serve ((Op => Info_Op, others => 0)) = ABI.Ok, "building info is Ok");
      ABI.Decode_Info (T.Last_Reply, C, Valid);
      Check (Valid and then not C.Ready and then C.Table_Count = 0 and then C.Total = 0,
             "building snapshot exposes no partial inventory");
      Check (Serve ((Op => Describe_Op, Table => 1, others => 0)) = ABI.Not_Ready,
             "building describe");
      Check (Serve (Copy_Words (1, 0, 64)) = ABI.Not_Ready, "building copy");
      Check (T.Writes = Before, "building refusals write nothing");
      T.Finish;
   end Unready;

   ---------------------------------------------------------------------------
   --  Reads a whole table through the service, Chunk_Max at a time, from
   --  the extent its description gives; checks bytes, checksum and EOF.
   procedure Whole_Table (Table : Unsigned_64; Expected : F.Bytes; Chunk : Unsigned_64) is
      D : ABI.Description;
      Valid : Boolean;
      Copied : ABI.Capacity_Bytes;
      Assembled : F.Bytes (1 .. Expected'Length) := [others => 0];
      Offset : Unsigned_64 := 0;
      Sum : F.Byte := 0;
      Steps : Natural := 0;
   begin
      Check (Serve ((Op => Describe_Op, Table => Table, others => 0)) = ABI.Ok, "describe ok");
      ABI.Decode_Description (T.Last_Reply, ABI.Table_Number (Table), D, Valid);
      Check (Valid and then D.Extent = Unsigned_64 (Expected'Length), "extent from descriptor");
      Check ((for all I in ABI.Signature_Index => D.Signature (I) = Expected (I)),
             "signature bytes");
      Check (D.Revision = Expected (9), "revision");
      while Offset < D.Extent loop
         T.User := [others => 16#AA#];
         Check (Serve (Copy_Words (Table, Offset, Chunk)) = ABI.Ok, "chunk ok");
         ABI.Decode_Copy (T.Last_Reply, ABI.Capacity_Bytes (Chunk), Copied, Valid);
         Check (Valid and then Copied > 0, "chunk progress");
         Check (Unsigned_64 (Copied) = Unsigned_64'Min (Chunk, D.Extent - Offset), "chunk length");
         for I in 1 .. Natural (Copied) loop
            Assembled (Natural (Offset) + I) := T.User (I);
         end loop;
         --  The whole capacity was written: the table's bytes, then zeros.
         Check ((for all I in Natural (Copied) + 1 .. Natural (Chunk) => T.User (I) = 0),
                "zero suffix");
         Check (Natural (Chunk) = T.User'Length or else T.User (Natural (Chunk) + 1) = 16#AA#,
                "nothing past the capacity");
         Offset := Offset + Unsigned_64 (Copied);
         Steps := Steps + 1;
      end loop;
      Check (Assembled = Expected, "exact reassembly");
      for B of Assembled loop Sum := Sum + B; end loop;
      Check (Sum = 0, "table checksum");
      Check (Steps = (Expected'Length + Natural (Chunk) - 1) / Natural (Chunk), "chunk count");
      --  EOF: Ok with nothing copied, and still the zero suffix.
      T.User := [others => 16#AA#];
      Check (Serve (Copy_Words (Table, Offset, Chunk)) = ABI.Ok, "EOF is Ok");
      ABI.Decode_Copy (T.Last_Reply, ABI.Capacity_Bytes (Chunk), Copied, Valid);
      Check (Valid and then Copied = 0, "EOF copies nothing");
      Check ((for all I in 1 .. Natural (Chunk) => T.User (I) = 0), "EOF zero fills");
      Check (Serve (Copy_Words (Table, Offset, 0)) = ABI.Ok, "EOF with capacity 0 is Ok");
      Check (Serve (Copy_Words (Table, Offset + 1, Chunk)) = ABI.Beyond_End, "beyond end");
      Check (Serve (Copy_Words (Table, Unsigned_64'Last, Chunk)) = ABI.Beyond_End,
             "full-width offset beyond end");
      Check (Serve (Copy_Words (Table, 0, 0)) = ABI.Empty_Before_End, "empty before end");
      Check (Serve (Copy_Words (Table, Offset - 1, 0)) = ABI.Empty_Before_End,
             "empty one before end");
   end Whole_Table;

   ---------------------------------------------------------------------------
   procedure Ready_Snapshot is
      C : ABI.Catalog;
      Valid : Boolean;
      Before : Natural;
      Large : constant F.Bytes := T.Raw (T.Large);
   begin
      Check (Serve ((Op => Info_Op, others => 0)) = ABI.Ok, "info ok");
      ABI.Decode_Info (T.Last_Reply, C, Valid);
      Check (Valid and then C.Ready and then C.Table_Count = 3, "info count");
      Check (C.Largest = Unsigned_64 (T.Large_Length), "info largest (above 64 KiB)");
      Check (C.Total = Unsigned_64 (T.Byte_Total), "info total");
      for Chunk of Word_List'[4_096, 1, 511, 512, 513, 1_000] loop
         Whole_Table (1, Large, Chunk);
      end loop;
      Whole_Table (2, T.Raw (T.Small), 4_096);
      Whole_Table (3, T.Raw (T.Tiny), 4_096);
      for Table of Word_List'[0, 4, 2 ** 31, Unsigned_64 (Unsigned_32'Last)] loop
         Check (Serve ((Op => Describe_Op, Table => Table, others => 0)) = ABI.Unknown_Table,
                "describe unknown table");
         Check (Serve (Copy_Words (Table, 0, 16)) = ABI.Unknown_Table, "copy unknown table");
      end loop;
      Check (T.Last_Reply = ABI.Empty_Reply, "no reply for a refusal");
      --  The caller's memory refuses after a prefix: Copy_Failed, no reply.
      T.Write_Limit := T.Writes + 700;
      Before := T.Replies;
      Check (Serve (Copy_Words (1, 0, 4_096)) = ABI.Copy_Failed, "copy failed after prefix");
      Check (T.Replies = Before, "no reply after a failed copy");
      T.Write_Limit := Natural'Last;
      Check (Serve (Copy_Words (1, 0, 4_096, T.User_Base + T.User_Bytes - 100)) = ABI.Copy_Failed,
             "destination past the caller's memory");
      --  The reply itself refused.
      T.Reply_Refused := True;
      Check (Serve (Copy_Words (1, 0, 64)) = ABI.Copy_Failed, "copy reply refused");
      Check (Serve ((Op => Describe_Op, Table => 1, others => 0)) = ABI.Copy_Failed,
             "describe reply refused");
      Check (Serve ((Op => Info_Op, others => 0)) = ABI.Copy_Failed, "info reply refused");
      T.Reply_Refused := False;
   end Ready_Snapshot;

begin
   Wire_Codes;
   Request_Decoding;
   Plans;
   Replies;
   Unready;
   Ready_Snapshot;
   if Failures = 0 then
      Put_Line ("firmware-snapshot: PASS" & Natural'Image (Checks) & " checks");
   else
      Put_Line ("firmware-snapshot: FAIL" & Natural'Image (Failures) & " of" &
                Natural'Image (Checks));
      raise Program_Error;
   end if;
end Firmware_Snapshot_Tests;
