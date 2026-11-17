pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
package body Compositor_Stream_Copy with SPARK_Mode => Off is
   Line : constant := 64;

   --  Ordinary stores for the unaligned edges of a row.
   procedure Copy_Words (Target, Source : System.Address; Words : Natural) is
      type Words_Array is array (1 .. Words) of Unsigned_32;
      --  SEC-025: own T is only written (the lent scanout buffer, never read
      --  back here); S is Desktop's private compose buffer, no peer maps it.
      T : Words_Array with Import, Address => Target;
      --  SEC-025: own (Desktop's private compose buffer).
      S : constant Words_Array with Import, Address => Source;
   begin
      T := S;
   end Copy_Words;

   --  Chunks of 64 bytes, Target 16-byte aligned.
   procedure Stream_Chunks (Target, Source : System.Address; Chunks : Natural) is
      D : System.Address := Target;
      S : System.Address := Source;
      N : Unsigned_64 := Unsigned_64 (Chunks);
   begin
      if Chunks = 0 then return; end if;
      Asm ("1:" & ASCII.LF &
           "movdqu   (%1), %%xmm0" & ASCII.LF &
           "movdqu 16(%1), %%xmm1" & ASCII.LF &
           "movdqu 32(%1), %%xmm2" & ASCII.LF &
           "movdqu 48(%1), %%xmm3" & ASCII.LF &
           "movntdq %%xmm0,   (%0)" & ASCII.LF &
           "movntdq %%xmm1, 16(%0)" & ASCII.LF &
           "movntdq %%xmm2, 32(%0)" & ASCII.LF &
           "movntdq %%xmm3, 48(%0)" & ASCII.LF &
           "add $64, %0" & ASCII.LF &
           "add $64, %1" & ASCII.LF &
           "dec %2" & ASCII.LF &
           "jnz 1b",
           Outputs => (System.Address'Asm_Output ("+r", D),
                       System.Address'Asm_Output ("+r", S),
                       Unsigned_64'Asm_Output ("+r", N)),
           Clobber => "memory,cc,xmm0,xmm1,xmm2,xmm3",
           Volatile => True);
   end Stream_Chunks;

   procedure Copy_Box
     (Target : System.Address; Target_Pitch : Natural;
      Source : System.Address; Source_Pitch : Natural;
      X, Y, Width, Height : Natural)
   is
   begin
      if Width = 0 or else Height = 0 then return; end if;
      for Row in Y .. Y + Height - 1 loop
         declare
            T : constant System.Address := Target + Storage_Offset (Row * Target_Pitch + X * 4);
            S : constant System.Address := Source + Storage_Offset (Row * Source_Pitch + X * 4);
            Bytes : constant Natural := Width * 4;
            --  Words until T is 16-byte aligned (pixels are 4-byte aligned).
            Head : constant Natural :=
              Natural'Min (Width, Natural ((16 - To_Integer (T) mod 16) mod 16) / 4);
            Chunks : constant Natural := (Bytes - Head * 4) / Line;
            Tail : constant Natural := (Bytes - Head * 4 - Chunks * Line) / 4;
         begin
            Copy_Words (T, S, Head);
            Stream_Chunks (T + Storage_Offset (Head * 4), S + Storage_Offset (Head * 4), Chunks);
            Copy_Words (T + Storage_Offset (Head * 4 + Chunks * Line),
                        S + Storage_Offset (Head * 4 + Chunks * Line), Tail);
         end;
      end loop;
   end Copy_Box;

   procedure Fence is
   begin
      Asm ("sfence", Clobber => "memory", Volatile => True);
   end Fence;
end Compositor_Stream_Copy;
