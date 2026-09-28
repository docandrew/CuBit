------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Wire with SPARK_Mode is

   function Be16 (Options : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (Options (I)), 8) or Unsigned_16 (Options (I + 1)))
   with Pre => Options'First = 0 and then Options'Length <= Maximum_Options and then
               I < Options'Last;

   function Be32 (Options : Bytes; I : Natural) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Options (I)), 24) or Shift_Left (Unsigned_32 (Options (I + 1)), 16) or
      Shift_Left (Unsigned_32 (Options (I + 2)), 8) or Unsigned_32 (Options (I + 3)))
   with Pre => Options'First = 0 and then Options'Length <= Maximum_Options and then
               Options'Length >= 4 and then I <= Options'Length - 4;

   procedure Parse (Options : Bytes; Found : out TCP_Options.Received) is
      I : Natural := 0;
      L : Natural;
   begin
      Found := (others => <>);
      loop
         pragma Loop_Invariant (I <= Maximum_Options);
         pragma Loop_Variant (Increases => I);
         exit when I >= Options'Length or else Options (I) = End_Of_List_Kind;
         if Options (I) = No_Operation_Kind then
            I := I + 1;
         else
            exit when I + 1 >= Options'Length;
            L := Natural (Options (I + 1));
            exit when L < 2 or else L > Options'Length - I;
            case Options (I) is
               when MSS_Kind =>
                  if L = MSS_Length then
                     Found.Has_MSS := True;
                     Found.MSS := Be16 (Options, I + 2);
                  end if;
               when Window_Scale_Kind =>
                  if L = Window_Scale_Length then
                     Found.Has_Window_Scale := True;
                     Found.Shift_Count := Options (I + 2);
                  end if;
               when SACK_Permitted_Kind =>
                  if L = SACK_Permitted_Length then
                     Found.SACK_Permitted := True;
                  end if;
               when Timestamps_Kind =>
                  if L = Timestamps_Length then
                     Found.Has_Timestamps := True;
                     Found.TSval := Be32 (Options, I + 2);
                     Found.TSecr := Be32 (Options, I + 6);
                  end if;
               when others =>
                  null;
            end case;
            I := I + L;
         end if;
      end loop;
   end Parse;

end TCP_Wire;
