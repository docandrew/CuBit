------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Desktop's launch request (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Kernel_ABI;
with CuBit.Launch_Arguments;
with CuBit.Launch_Handles;
with CuBit.Process_IDs;

package body Desktop_Launch_Request with SPARK_Mode is
   package LA renames CuBit.Launch_Arguments;
   package LH renames CuBit.Launch_Handles;

   function Classify
     (Delivered : Boolean; Label : Unsigned_32; Length : Unsigned_8;
      Words : Reply_Words) return Reply_Kind
   is
   begin
      if not Delivered then
         return Unreadable;
      elsif Label = CuBit.Kernel_ABI.Reply_OK then
         return
           (if Length = LH.Launched_Reply_Words
              and then CuBit.Process_IDs.Is_Process (CuBit.Process_IDs.From_Word (Words (0)))
              and then LH.Valid_Rights_Word (Words (1))
              and then LH.Is_Outcome_Word (Words (2))
              and then Words (3) = 0
            then Started else Unreadable);
      elsif Label = CuBit.Kernel_ABI.Reply_Error then
         --  One word, the reason; or Request_Withheld and its label.
         return
           (if (Length = 1 and then LA.Is_Failure (Words (0))
                and then Words (1) = 0 and then Words (2) = 0 and then Words (3) = 0)
              or else
                (Length = LA.Launch_Request_Words
                 and then Words (0) = LA.Launch_Failure'Enum_Rep (LA.Request_Withheld))
            then Refused else Unreadable);
      else
         return Unreadable;
      end if;
   end Classify;

   procedure Remember (S : in out Stale_Tokens; Token : Unsigned_64) is
   begin
      for I in Slot_Index loop
         if S (I) = 0 then
            S (I) := Token;
            return;
         end if;
         pragma Loop_Invariant (for all J in Slot_Index'First .. I => S (J) /= 0);
         pragma Loop_Invariant (S = S'Loop_Entry);
      end loop;
      --  Unreachable: not Full.
      pragma Assert (Full (S));
   end Remember;

   procedure Forget (S : in out Stale_Tokens; Token : Unsigned_64) is
   begin
      for I in Slot_Index loop
         if Token /= 0 and then S (I) = Token then
            S (I) := 0;
         end if;
         pragma Loop_Invariant
           (Token = 0 or else (for all J in Slot_Index'First .. I => S (J) /= Token));
         pragma Loop_Invariant
           (for all J in Slot_Index => S (J) = S'Loop_Entry (J) or else
              (S'Loop_Entry (J) = Token and then S (J) = 0));
      end loop;
   end Forget;
end Desktop_Launch_Request;
