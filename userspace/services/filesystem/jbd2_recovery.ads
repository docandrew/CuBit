with Interfaces; use Interfaces;
with Jbd2_Format; use Jbd2_Format;

--  JBD2 journal recovery (replay), following Linux's three passes: find the
--  end of the log, collect revokes of committed transactions, then write each
--  committed block home unless a revoke cancels it. The block I/O is the
--  instantiator's. Decoding, checksums, the revoke table and each replay
--  decision are the proved units Jbd2_Format and Jbd2_Revokes.
--
--  Stricter than Linux on damaged logs: a transaction is replayed only if
--  every one of its blocks is valid (checksums, tag counts, home locations
--  inside the filesystem); the first invalid or incomplete transaction ends
--  the log. Replay writes are a pure function of the log, so running it again
--  after an interruption writes the same blocks: it is idempotent.
generic
   --  Log_Block is a journal-relative block number (0 .. Max_Length - 1).
   with procedure Read_Log
     (Log_Block : Unsigned_32; Data : out Block; Ok : out Boolean);
   with procedure Write_Home
     (Home : Unsigned_64; Data : Block; Ok : out Boolean);
package Jbd2_Recovery is
   type Outcome is
     (Recovered,          -- zero or more transactions replayed
      Read_Failure, Write_Failure,
      Too_Many_Revokes);  -- over Jbd2_Revokes.Capacity: refused

   --  Super must be a valid, dirty (Start /= 0) superblock from
   --  Decode_Superblock. Next_Sequence is the sequence the clean journal
   --  must expect next: one past the first incomplete transaction, as jbd2.
   procedure Recover
     (Super : Journal_Superblock; Size : Block_Bytes;
      Filesystem_Blocks : Unsigned_32;
      Result : out Outcome; Next_Sequence : out Unsigned_32;
      Transactions, Blocks_Written : out Natural)
     with Pre => Super.Start /= 0 and then Super.First < Super.Max_Length;

   --  Superblock checksum as jbd2 computes it: crc32c(~0) over its 1024
   --  bytes with the stored checksum field zeroed.
   function Superblock_Checksum_Matches (Data : Block) return Boolean;
end Jbd2_Recovery;
