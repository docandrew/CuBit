with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GuC_CTB; use Intel_GPU_GuC_CTB;
procedure GuC_CTB_Tests is
   S, R : Plan;
   Count : Natural := 0;
begin
   for Size in Ring_Size range 2 .. 32 loop
      for Head in Unsigned_32 range 0 .. Size - 1 loop
         for Tail in Unsigned_32 range 0 .. Size - 1 loop
            for Payload in Unsigned_32 range 1 .. 33 loop
               declare
                  Used : constant Unsigned_32 :=
                    (if Tail >= Head then Tail - Head else Size - Head + Tail);
               begin
                  S := Send (Size, Head, Tail, Tail, 0, Payload, 16#CAFE#);
                  pragma Assert (S.State = (if Payload + 1 <= Size - Used - 1 then Ready else Full));
                  R := Receive (Size, Head, Tail, Head, 0, Header (16#CAFE#, Payload));
                  pragma Assert (R.State = (if Used = 0 then Empty elsif Payload + 1 <= Used then Ready else Truncated));
                  if S.State = Ready then
                     pragma Assert (S.Next_Cursor = (Tail + Payload + 1) mod Size and S.Fence = 16#CAFE#);
                  end if;
                  if R.State = Ready then
                     pragma Assert (R.Next_Cursor = (Head + Payload + 1) mod Size and R.Fence = 16#CAFE#);
                  end if;
                  Count := Count + 1;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   for Low in Unsigned_32 range 0 .. 65_535 loop
      R := Receive (1024, 0, 512, 0, 0, 16#CAFE0000# or Low);
      pragma Assert (R.State = (if Low in 1 .. 255 then Ready else Invalid_Message));
   end loop;
   S := Send (1024, 0, 0, 1, 0, 1, 0);
   pragma Assert (S.State = Invalid_Descriptor);
   R := Receive (1024, 0, 1, 1, 0, 1);
   pragma Assert (R.State = Invalid_Descriptor);
   for Bad in Unsigned_32 range 1 .. 15 loop
      S := Send (1024, 0, 0, 0, Bad, 1, 0);
      R := Receive (1024, 0, 2, 0, Bad, 1);
      pragma Assert (S.State = Invalid_Descriptor and R.State = Invalid_Descriptor);
   end loop;
   Ada.Text_IO.Put_Line ("GuC CTB plans PASS ring cases=" & Count'Image & " header cases=65536");
end GuC_CTB_Tests;
