with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_ADLN_Barrier;
with Intel_GPU_Live_Ring_Publish;
procedure Live_Ring_Publish_Tests is
   package Init renames Intel_GPU_ADLN_Context_Init;
   Owner : Boolean := True;
   Steps, Fail_At, Lose_At, Writes, Tail_Writes : Natural := 0;
   Marker : Unsigned_64 := 1;
   Saved_Tail, Start, Visible_Tail : Unsigned_32 := 384;
   Expected_Bytes : Unsigned_32 := 384;
   Memory : array (Unsigned_32 range 0 .. 4095) of Unsigned_32 := [others => 0];
   function Owned return Boolean is (Owner);
   function Step return Boolean is
   begin
      Steps := Steps + 1;
      if Steps = Lose_At then Owner := False; end if;
      return Steps /= Fail_At;
   end Step;
   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean) is
   begin Value := Marker; OK := Step; end;
   procedure Read_Tail (Value : out Unsigned_32; OK : out Boolean) is
   begin Value := Saved_Tail; OK := Step; end;
   procedure Write_Word (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Offset = (Start + Unsigned_32 (Writes) * 4) mod 16384);
      pragma Assert (Offset < 16384 and Offset mod 4 = 0);
      pragma Assert (Tail_Writes = 0);
      Memory (Offset / 4) := Value; Writes := Writes + 1; OK := Step;
   end;
   function Publish_Words (Offset, Bytes : Unsigned_32) return Boolean is
   begin
      if Start > 16320 - Expected_Bytes then
         if Offset = Start then
            pragma Assert (Bytes = 16384 - Start and Unsigned_32 (Writes) = Bytes / 4);
            return Step;
         end if;
         pragma Assert (Offset = 0 and Bytes = Expected_Bytes and
           Unsigned_32 (Writes) = (16384 - Start + Bytes) / 4);
      else
         pragma Assert (Offset = Start and Bytes = Expected_Bytes and Unsigned_32 (Writes) = Bytes / 4);
      end if;
      Visible_Tail := Offset + Bytes;
      return Step;
   end;
   procedure Write_Tail (Value : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Visible_Tail = Value and Value =
        (if Start > 16320 - Expected_Bytes then Expected_Bytes else Start + Expected_Bytes));
      Tail_Writes := Tail_Writes + 1; Saved_Tail := Value; OK := Step;
   end;
   function Tail_Visible return Boolean is
   begin
      pragma Assert (Tail_Writes = 1 and Saved_Tail = Visible_Tail);
      return Step;
   end;
   package Ring is new Intel_GPU_Live_Ring_Publish
     (Owned, Read_Marker, Read_Tail, Write_Word, Publish_Words,
      Write_Tail, Tail_Visible);
   use type Ring.Result;
   use type Ring.Phase;
   Status : Ring.Result;
   procedure Reset is
   begin
      Owner := True; Steps := 0; Fail_At := 0; Lose_At := 0;
      Writes := 0; Tail_Writes := 0; Marker := 1;
      Saved_Tail := 384; Start := 384; Visible_Tail := 384;
      Expected_Bytes := 384;
      Memory := [others => 16#DEADBEEF#];
   end;
begin
   for Lose in Boolean loop
      for At_Step in 0 .. 35 loop
         declare Object : Ring.Channel; Saved : Natural; begin
            Reset; Expected_Bytes := 120;
            if Lose then Lose_At := At_Step; else Fail_At := At_Step; end if;
            Ring.Append (Object, Intel_GPU_ADLN_Barrier.Build (2), Status);
            if At_Step = 0 then
               pragma Assert (Status = Ring.Published and Writes = 30 and
                 Ring.Tail (Object) = 504 and Ring.Sequence (Object) = 2);
               pragma Assert (Memory (126) = 16#DEADBEEF#);
               -- Barrier post-syncs hit PPHWSP scratch; only the final
               -- breadcrumb writes the timeline slot (+0x200).
               pragma Assert (Memory (96 + 2) = 16#D0# and Memory (96 + 9) = 16#D0#);
               pragma Assert (Memory (96 + 24) = 16#200# and Memory (96 + 26) = 2 and
                              Memory (96 + 27) = 0);
            else
               pragma Assert (Status /= Ring.Published and
                 Ring.State (Object) = Ring.Quarantined and Steps = At_Step);
               Saved := Steps; Owner := True;
               Ring.Append (Object, Intel_GPU_ADLN_Barrier.Build (2), Status);
               pragma Assert (Status = Ring.Rejected and Steps = Saved);
            end if;
         end;
      end loop;
   end loop;
   -- Two reads, 96 stores, commands-visible, tail-store, tail-visible.
   for Lose in Boolean loop
      for At_Step in 1 .. 101 loop
         declare Object : Ring.Channel; Saved : Natural; begin
            Reset;
            if Lose then Lose_At := At_Step; else Fail_At := At_Step; end if;
            Ring.Append (Object, Init.Build (True, 0, 2), Status);
            pragma Assert (Status /= Ring.Published and
                           Ring.State (Object) = Ring.Quarantined);
            if Lose then pragma Assert (Status = Ring.Ownership_Lost); end if;
            pragma Assert (Steps = At_Step);
            Saved := Steps; Owner := True;
            Ring.Append (Object, Init.Build (True, 0, 2), Status);
            pragma Assert (Status = Ring.Rejected and Steps = Saved);
         end;
      end loop;
   end loop;
   for Bad in 0 .. 3 loop
      declare Object : Ring.Channel; begin
         Reset;
         case Bad is
            when 0 => Marker := 0;
            when 1 => Marker := 2;
            when 2 => Marker := 16#100000001#;
            when others => Saved_Tail := 0;
         end case;
         Ring.Append (Object, Init.Build (True, 0, 2), Status);
         pragma Assert (Status = (if Bad = 3 then Ring.Tail_Mismatch
                                  else Ring.Prior_Not_Complete));
         pragma Assert (Writes = 0 and Ring.State (Object) = Ring.Quarantined);
      end;
   end loop;
   declare Object : Ring.Channel; Saved : Natural; begin
      Reset;
      Ring.Append (Object, Init.Build (True, 0, 1), Status);
      pragma Assert (Status = Ring.Rejected and Steps = 0);
      for Seq in Unsigned_32 range 2 .. 42 loop
         Start := Ring.Tail (Object); Marker := Unsigned_64 (Seq - 1);
         Writes := 0; Tail_Writes := 0;
         Ring.Append (Object, Init.Build (True, 0, Seq), Status);
         pragma Assert (Status = Ring.Published and Ring.Sequence (Object) = Seq);
         pragma Assert (Ring.Tail (Object) = Seq * 384);
         pragma Assert (Memory ((Start + 90 * 4) / 4) = Seq);
         pragma Assert (Memory ((Start + 88 * 4) / 4) = 16#200#);
      end loop;
      -- Initial commands and end guard were never overwritten.
      pragma Assert (for all I in Unsigned_32 range 0 .. 95 => Memory (I) = 16#DEADBEEF#);
      pragma Assert (for all I in Unsigned_32 range 4080 .. 4095 => Memory (I) = 16#DEADBEEF#);
      pragma Assert (for all Seq in Unsigned_32 range 2 .. 42 =>
        Memory (((Seq - 1) * 384 + 90 * 4) / 4) = Seq);
      Saved := Writes;
      Start := Ring.Tail (Object); Marker := 42; Writes := 0; Tail_Writes := 0;
      Ring.Append (Object, Init.Build (True, 0, 43), Status);
      pragma Assert (Status = Ring.Published and Ring.Tail (Object) = 384);
      pragma Assert (Writes = 160 and Saved = 96);
      pragma Assert (for all I in Unsigned_32 range 4032 .. 4095 => Memory (I) = 0);
      pragma Assert (Memory (90) = 43);
   end;
   for Lose in Boolean loop
      for At_Step in 1 .. 166 loop
         declare Object : Ring.Channel; Before : Natural; begin
            Reset;
            for Seq in Unsigned_32 range 2 .. 42 loop
               Start := Ring.Tail (Object); Marker := Unsigned_64 (Seq - 1);
               Writes := 0; Tail_Writes := 0;
               Ring.Append (Object, Init.Build (True, 0, Seq), Status);
               pragma Assert (Status = Ring.Published);
            end loop;
            Start := Ring.Tail (Object); Marker := 42;
            Steps := 0; Writes := 0; Tail_Writes := 0;
            if Lose then Lose_At := At_Step; else Fail_At := At_Step; end if;
            Ring.Append (Object, Init.Build (True, 0, 43), Status);
            pragma Assert (Status /= Ring.Published and Ring.State (Object) = Ring.Quarantined);
            pragma Assert (Ring.Tail (Object) = 16128 and Ring.Sequence (Object) = 42);
            pragma Assert (Steps = At_Step);
            Before := Steps;
            Ring.Append (Object, Init.Build (True, 0, 43), Status);
            pragma Assert (Status = Ring.Rejected and Steps = Before);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Live ring publication PASS: 202 append +332 wrap callback faults, protected prior segment, padding/flush/tail order, no retry");
end Live_Ring_Publish_Tests;
