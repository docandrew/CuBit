with Jbd2_Revokes;

package body Jbd2_Recovery is
   All_Ones : constant Unsigned_32 := 16#FFFF_FFFF#;
   Word_Bytes : constant := 4;

   function Stored_Be32 (Data : Block; Offset : Natural) return Unsigned_32 is
     (Be32 (Data, Offset));

   --  Checksum of a whole block with the 4-byte field at Field zeroed.
   function Block_Checksum
     (Seed : Unsigned_32; Data : Block; Size : Block_Bytes; Field : Natural)
      return Unsigned_32
   is
      Copy : Block := Data;
   begin
      Copy (Field .. Field + Word_Bytes - 1) := [others => 0];
      return Crc32c (Seed, Copy, 0, Size);
   end Block_Checksum;

   function Superblock_Checksum_Matches (Data : Block) return Boolean is
      Copy : Block := Data;
   begin
      Copy (Superblock_Checksum_Offset .. Superblock_Checksum_Offset + Word_Bytes - 1) :=
        [others => 0];
      return Crc32c (All_Ones, Copy, 0, Superblock_Bytes) =
        Stored_Be32 (Data, Superblock_Checksum_Offset);
   end Superblock_Checksum_Matches;

   procedure Recover
     (Super : Journal_Superblock; Size : Block_Bytes;
      Filesystem_Blocks : Unsigned_32;
      Result : out Outcome; Next_Sequence : out Unsigned_32;
      Transactions, Blocks_Written : out Natural)
   is
      Incompat : constant Unsigned_32 := Super.Incompat;
      Checksums : constant Boolean := Checksummed (Incompat);
      --  v1: crc32 of each transaction's descriptor and data blocks, recorded
      --  in its commit block. Superseded by per-block v2/v3 checksums.
      Transaction_Checksums : constant Boolean :=
        not Checksums and then Has (Super.Compat, Compat_Checksum);
      Seed : constant Unsigned_32 :=
        (if Checksums then Crc32c_UUID (All_Ones, Super.Identity) else 0);
      Log_Blocks : constant Unsigned_32 := Super.Max_Length - Super.First;
      Limit : constant Natural := Usable_Bytes (Size, Incompat);
      Revokes : Jbd2_Revokes.Table := Jbd2_Revokes.Empty;
      End_Sequence : Unsigned_32 := Super.Sequence;
      Data, Payload : Block;
      Ok : Boolean;

      --  Journal-relative successor, wrapping inside the log area.
      function Advance (Position : Unsigned_32; Count : Unsigned_32) return Unsigned_32 is
        (Super.First + (Position - Super.First + Count mod Log_Blocks) mod Log_Blocks);

      type Pass is (Scan, Revoke, Replay);

      --  Walk the log from Start. In Scan, stop at the first block that does
      --  not continue a valid transaction and set End_Sequence to the first
      --  sequence not wholly committed. Later passes stop at End_Sequence.
      procedure Walk (Stage : Pass) is
         Position : Unsigned_32 := Super.Start;
         Sequence : Unsigned_32 := Super.Sequence;
         Consumed : Unsigned_32 := 0;
         Current : Header;
         Running_Sum : Unsigned_32 := All_Ones;
         Sum_Seen : Boolean := False;
         Scan_Sums : constant Boolean := Stage = Scan and then Transaction_Checksums;
      begin
         while Stage = Scan or else Sequence /= End_Sequence loop
            exit when Consumed >= Log_Blocks;
            Read_Log (Position, Data, Ok);
            if not Ok then
               Result := Read_Failure;
               return;
            end if;
            Current := Header_Of (Data);
            exit when Current.Magic_Value /= Magic or else Current.Sequence /= Sequence;
            if Current.Kind = Descriptor_Kind then
               if Checksums and then
                 Block_Checksum (Seed, Data, Size, Size - Tail_Bytes) /=
                   Stored_Be32 (Data, Size - Tail_Bytes)
               then
                  exit;
               end if;
               if Scan_Sums then
                  Running_Sum := Crc32_Be (Running_Sum, Data, 0, Size);
               end if;
               declare
                  Offset : Natural := Header_Bytes;
                  Tag : Block_Tag;
                  Found : Boolean;
                  Tags : Unsigned_32 := 0;
                  Valid : Boolean := True;
               begin
                  loop
                     Next_Tag (Data, Limit, Incompat, Offset, Tag, Found);
                     exit when not Found;
                     Tags := Tags + 1;
                     if Tags >= Log_Blocks then
                        Valid := False;
                        exit;
                     end if;
                     declare
                        Location : constant Unsigned_32 := Advance (Position, Tags);
                     begin
                        if Stage = Replay or else
                          (Stage = Scan and then (Checksums or else Transaction_Checksums))
                        then
                           Read_Log (Location, Payload, Ok);
                           if not Ok then
                              Result := Read_Failure;
                              return;
                           end if;
                        end if;
                        if Scan_Sums then
                           Running_Sum := Crc32_Be (Running_Sum, Payload, 0, Size);
                        end if;
                        case Stage is
                           when Scan =>
                              if Tag.Home >= Unsigned_64 (Filesystem_Blocks) then
                                 Valid := False;
                              elsif Checksums then
                                 declare
                                    Computed : constant Unsigned_32 := Crc32c
                                      (Crc32c_Be32 (Seed, Sequence), Payload, 0, Size);
                                 begin
                                    if (if Has (Incompat, Incompat_Csum_V3)
                                        then Computed /= Tag.Checksum
                                        else (Computed and 16#FFFF#) /= Tag.Checksum)
                                    then
                                       Valid := False;
                                    end if;
                                 end;
                              end if;
                           when Revoke => null;
                           when Replay =>
                              case Jbd2_Revokes.Action
                                (Revokes, Tag.Home, Sequence, Filesystem_Blocks)
                              is
                                 when Jbd2_Revokes.Write_Home =>
                                    if Has (Tag.Flags, Tag_Escape) then
                                       Payload (0) := Unsigned_8 (Shift_Right (Magic, 24));
                                       Payload (1) := Unsigned_8 (Shift_Right (Magic, 16) and 16#FF#);
                                       Payload (2) := Unsigned_8 (Shift_Right (Magic, 8) and 16#FF#);
                                       Payload (3) := Unsigned_8 (Magic and 16#FF#);
                                    end if;
                                    Write_Home (Tag.Home, Payload, Ok);
                                    if not Ok then
                                       Result := Write_Failure;
                                       return;
                                    end if;
                                    Blocks_Written := Blocks_Written + 1;
                                 when Jbd2_Revokes.Skip_Revoked | Jbd2_Revokes.Out_Of_Range =>
                                    null; -- Scan rejected out-of-range transactions
                              end case;
                        end case;
                     end;
                     exit when not Valid or else Has (Tag.Flags, Tag_Last);
                  end loop;
                  exit when not Valid;
                  Position := Advance (Position, Tags + 1);
                  Consumed := Consumed + Tags + 1;
               end;
            elsif Current.Kind = Revoke_Kind then
               if not Revoke_Count_Valid (Data, Size, Incompat) or else
                 (Checksums and then
                  Block_Checksum (Seed, Data, Size, Size - Tail_Bytes) /=
                    Stored_Be32 (Data, Size - Tail_Bytes))
               then
                  exit;
               end if;
               if Stage = Revoke then
                  for I in 0 .. Revoke_Records (Data, Size, Incompat) - 1 loop
                     declare
                        Stored : Boolean;
                     begin
                        Jbd2_Revokes.Record_Revoke
                          (Revokes, Revoked_Block (Data, Size, Incompat, I), Sequence, Stored);
                        if not Stored then
                           Result := Too_Many_Revokes;
                           return;
                        end if;
                     end;
                  end loop;
               end if;
               Position := Advance (Position, 1);
               Consumed := Consumed + 1;
            elsif Current.Kind = Commit_Kind then
               if Checksums and then
                 Block_Checksum (Seed, Data, Size, Commit_Checksum_Offset) /=
                   Stored_Be32 (Data, Commit_Checksum_Offset)
               then
                  exit;
               end if;
               if Scan_Sums then
                  --  As jbd2: a matching crc32, or (before any was seen) a
                  --  commit that records no checksum at all.
                  if Data (Commit_Checksum_Type_Offset) = Checksum_Type_Crc32 and then
                    Data (Commit_Checksum_Size_Offset) = Crc32_Checksum_Bytes and then
                    Stored_Be32 (Data, Commit_Checksum_Offset) = Running_Sum
                  then
                     Sum_Seen := True;
                  elsif Sum_Seen or else Data (Commit_Checksum_Type_Offset) /= 0 or else
                    Data (Commit_Checksum_Size_Offset) /= 0 or else
                    Stored_Be32 (Data, Commit_Checksum_Offset) /= 0
                  then
                     exit;
                  end if;
                  Running_Sum := All_Ones;
               end if;
               Sequence := Sequence + 1;
               if Stage = Scan then
                  End_Sequence := Sequence;
               end if;
               Position := Advance (Position, 1);
               Consumed := Consumed + 1;
            else
               exit;
            end if;
         end loop;
      end Walk;
   begin
      Result := Recovered;
      Blocks_Written := 0;
      Transactions := 0;
      Next_Sequence := Super.Sequence;
      for Stage in Pass loop
         Walk (Stage);
         if Result /= Recovered then
            return;
         end if;
      end loop;
      Transactions := Natural (End_Sequence - Super.Sequence);
      --  As jbd2 does, skip End_Sequence itself: blocks of the incomplete
      --  transaction may carry it, and must never look like a new one.
      Next_Sequence := End_Sequence + 1;
   end Recover;
end Jbd2_Recovery;
