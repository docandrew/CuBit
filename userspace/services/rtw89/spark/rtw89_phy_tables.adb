------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body RTW89_PHY_Tables with SPARK_Mode is

   function Select_Headline (T : Table; RFE, Cut : Unsigned_8) return Selection is
      Size     : Natural := 0;
      Wanted   : Unsigned_32;
      Cut_Max  : Unsigned_8 := 0;
      Chosen   : Entry_Index := T'First;
      Matched  : Boolean := False;
   begin
      for I in T'Range loop
         pragma Loop_Invariant (Size = I - T'First);
         exit when Kind (T (I).Address) /= Headline_Kind;
         Size := Size + 1;
      end loop;
      if Size = 0 then
         --  No headlines: rtw89 checks branches against entry 0's target.
         return (Found => True, Headlines => 0, Cfg_Target => Target (T (T'First).Address));
      end if;

      --  1: RFE and cut match.
      Wanted := Compare (RFE, Cut);
      for I in T'First .. T'First + Size - 1 loop
         if Target (T (I).Address) = Wanted then
            return (Found => True, Headlines => Size, Cfg_Target => Target (T (I).Address));
         end if;
      end loop;
      --  2: RFE match, cut "don't care".
      Wanted := Compare (RFE, Dont_Care);
      for I in T'First .. T'First + Size - 1 loop
         if Target (T (I).Address) = Wanted then
            return (Found => True, Headlines => Size, Cfg_Target => Target (T (I).Address));
         end if;
      end loop;
      --  3: RFE match, the largest cut listed.
      for I in T'First .. T'First + Size - 1 loop
         pragma Loop_Invariant (Chosen in T'Range);
         if RFE_Of (T (I).Address) = RFE and then Cut_Of (T (I).Address) >= Cut_Max then
            Cut_Max := Cut_Of (T (I).Address);
            Chosen := I;
            Matched := True;
         end if;
      end loop;
      if Matched then
         return (Found => True, Headlines => Size, Cfg_Target => Target (T (Chosen).Address));
      end if;
      --  4: RFE "don't care", the largest cut listed (rtw89 carries Cut_Max
      --  over from case 3, where nothing matched, so it is still 0).
      for I in T'First .. T'First + Size - 1 loop
         pragma Loop_Invariant (Chosen in T'Range);
         if RFE_Of (T (I).Address) = Dont_Care and then Cut_Of (T (I).Address) >= Cut_Max then
            Cut_Max := Cut_Of (T (I).Address);
            Chosen := I;
            Matched := True;
         end if;
      end loop;
      if Matched then
         return (Found => True, Headlines => Size, Cfg_Target => Target (T (Chosen).Address));
      end if;
      return (Found => False);
   end Select_Headline;

   procedure Step (W : in out Walk; Cfg_Target : Unsigned_32; E : Table_Entry; Do_It : out Action) is
      K : constant Unsigned_32 := Kind (E.Address);
   begin
      Do_It := Skip;
      if K = Branch_If or else K = Branch_Else_If then
         W.Branch := Target (E.Address);
      elsif K = Branch_Else then
         W.Matched := False;
         if not W.Found then
            Do_It := Stop;
         end if;
      elsif K = Branch_End then
         W.Matched := True;
         W.Found := False;
      elsif K = Branch_Check then
         if W.Found then
            W.Matched := False;
         elsif W.Branch = Cfg_Target then
            W.Matched := True;
            W.Found := True;
         else
            W.Matched := False;
            W.Found := False;
         end if;
      elsif W.Matched then
         Do_It := Apply;
      end if;
   end Step;

   procedure Load (T : Table; RFE, Cut : Unsigned_8; Applied : out Natural; Status : out Load_Status) is
      Chosen : constant Selection := Select_Headline (T, RFE, Cut);
      W : Walk;
      Do_It : Action;
   begin
      Applied := 0;
      if not Chosen.Found then
         Status := No_Configuration;
         return;
      end if;
      Status := Loaded;
      if Chosen.Headlines = T'Length then
         return;
      end if;
      for I in T'First + Chosen.Headlines .. T'Last loop
         pragma Loop_Invariant (Applied <= I - T'First);
         Step (W, Chosen.Cfg_Target, T (I), Do_It);
         case Do_It is
            when Apply =>
               Write (T (I));
               Applied := Applied + 1;
            when Skip =>
               null;
            when Stop =>
               Status := Aborted;
               return;
         end case;
      end loop;
   end Load;

end RTW89_PHY_Tables;
