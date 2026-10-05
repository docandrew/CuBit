------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Select with SPARK_Mode is

   procedure Gather
     (Count : Limit; Sets : Given; Read, Write, Error : Descriptor_Set;
      Polls : out Poll_Array; Used : out Limit)
   is
      Events : Integer_16;
   begin
      Polls := [others => (Descriptor => 0, Events => 0, Returned => 0)];
      Used := 0;
      for D in 0 .. Count - 1 loop
         Events := Event_Bits (Sets.Read and then Read (D),
                               Sets.Write and then Write (D),
                               Sets.Error and then Error (D));
         if Events /= 0 then
            Polls (Used) := (Descriptor => Interfaces.C.int (D),
                             Events => Events, Returned => 0);
            Used := Used + 1;
         end if;
         pragma Loop_Invariant (Used <= D + 1);
         pragma Loop_Invariant
           (for all K in 0 .. Used - 1 =>
              Polls (K).Descriptor in 0 .. Interfaces.C.int (D)
              and then Polls (K).Events /= 0 and then Polls (K).Returned = 0);
      end loop;
   end Gather;

   procedure Scatter
     (Polls : Poll_Array; Used : Limit; Sets : Given;
      Read, Write, Error : out Descriptor_Set; Ready : out Natural;
      Invalid : out Boolean)
   is
      function Has (Bits, Flag : Integer_16) return Boolean is
        ((Unsigned_16'Mod (Bits) and Unsigned_16'Mod (Flag)) /= 0);
   begin
      Read := [others => False];
      Write := [others => False];
      Error := [others => False];
      Ready := 0;
      Invalid := False;
      for K in 0 .. Used - 1 loop
         if Has (Polls (K).Returned, POLLNVAL) then
            Invalid := True;
         end if;
      end loop;
      for K in 0 .. Used - 1 loop
         declare
            D : constant Descriptor := Descriptor (Polls (K).Descriptor);
            Got : constant Integer_16 := Polls (K).Returned;
            Want : constant Integer_16 := Polls (K).Events;
         begin
            if Sets.Read and then Has (Want, POLLIN)
              and then Has (Got, POLLIN + POLLHUP + POLLERR)
            then
               Read (D) := True;
               Ready := Ready + 1;
            end if;
            if Sets.Write and then Has (Want, POLLOUT)
              and then Has (Got, POLLOUT + POLLERR)
            then
               Write (D) := True;
               Ready := Ready + 1;
            end if;
            if Sets.Error and then Has (Want, POLLPRI) and then Has (Got, POLLPRI)
            then
               Error (D) := True;
               Ready := Ready + 1;
            end if;
         end;
         pragma Loop_Invariant (Ready <= 3 * (K + 1));
         pragma Loop_Invariant (if not Sets.Read then Empty (Read));
         pragma Loop_Invariant (if not Sets.Write then Empty (Write));
         pragma Loop_Invariant (if not Sets.Error then Empty (Error));
      end loop;
   end Scatter;

end CuBit.Libc_Select;
