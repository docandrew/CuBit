------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Eject (FS-034): one typed request to the filesystem service that makes
--  a removable volume safe to pull out, and its typed answer.
--
--  @description
--  Queue_Eject (CuBit.Filesystem_Queues) names the volume by its locator
--  in the arena ("@usb:0") and says in Options what to do about handles
--  still open on it (Open_Handle_Policy): refuse, or close them. The
--  caller must hold write access to the whole volume (a scope over its
--  root): whoever may overwrite all of it may also take it away.
--
--  The answer's Value is one Outcome and, for Open_Handles, how many
--  handles are open (other programs' and the caller's own). Safe_To_Remove
--  means: every buffered write was written, the volume flushed and marked
--  clean, and it is detached: its handles answer that the volume was
--  ejected, and it is not mounted again until it is plugged in again.
--
--  This package is the pure wire encoding.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Volume_Ejection with Pure, SPARK_Mode is

   type Open_Handle_Policy is (Refuse_If_Open, Close_Open_Handles);
   for Open_Handle_Policy use (Refuse_If_Open => 1, Close_Open_Handles => 2);

   type Outcome is
     (Safe_To_Remove,      --  flushed, clean, detached: pull it out
      Open_Handles,        --  refused: handles are open (the count says how many)
      Not_Removable,       --  refused: not a removable volume (a fixed disk, @system)
      Not_Attached,        --  refused: no such volume attached (gone, or ejected already)
      Flush_Failed,        --  refused: its writes could not be made durable (still attached)
      Access_Denied);      --  refused: the caller may not write the whole volume
   for Outcome use
     (Safe_To_Remove => 1, Open_Handles => 2, Not_Removable => 3, Not_Attached => 4,
      Flush_Failed => 5, Access_Denied => 6);

   subtype Open_Count is Unsigned_32;

   type Answer is record
      Result : Outcome := Not_Attached;
      Open : Open_Count := 0;
   end record;

   --  A count only with Open_Handles, and then at least one.
   function Well_Formed (Item : Answer) return Boolean is
     (if Item.Result = Open_Handles then Item.Open > 0 else Item.Open = 0);

   --  The outcome in the low half, the count in the high half.
   Count_Shift : constant := 32;
   Low_Half : constant Unsigned_64 := 16#FFFF_FFFF#;

   function Code_Of (Item : Outcome) return Unsigned_64 is (Unsigned_64 (Outcome'Enum_Rep (Item)));

   function Encode (Item : Answer) return Unsigned_64 is
     (Code_Of (Item.Result) or Shift_Left (Unsigned_64 (Item.Open), Count_Shift))
     with Pre => Well_Formed (Item);

   procedure Decode (Value : Unsigned_64; Item : out Answer; Valid : out Boolean)
     with Post =>
       (if Valid then Well_Formed (Item) and then Encode (Item) = Value)
       and then Valid =
         ((Value and Low_Half) in Code_Of (Outcome'First) .. Code_Of (Outcome'Last)
          and then (if (Value and Low_Half) = Code_Of (Open_Handles)
                    then Shift_Right (Value, Count_Shift) /= 0
                    else Shift_Right (Value, Count_Shift) = 0));

   function Policy_Code (Item : Open_Handle_Policy) return Unsigned_32 is
     (Unsigned_32 (Open_Handle_Policy'Enum_Rep (Item)));

   procedure Decode_Policy
     (Code : Unsigned_32; Item : out Open_Handle_Policy; Valid : out Boolean)
     with Post =>
       (if Valid then Policy_Code (Item) = Code)
       and then Valid = (Code in Policy_Code (Refuse_If_Open) | Policy_Code (Close_Open_Handles));

   --  What the service does for a request it is allowed to act on.
   type Step is (Refuse_Not_Attached, Refuse_Not_Removable, Refuse_Open, Close_Then_Flush, Flush_Only);

   function Plan
     (Attached, Removable : Boolean; Open : Open_Count; Policy : Open_Handle_Policy) return Step is
     (if not Attached then Refuse_Not_Attached
      elsif not Removable then Refuse_Not_Removable
      elsif Open = 0 then Flush_Only
      elsif Policy = Refuse_If_Open then Refuse_Open
      else Close_Then_Flush)
     with Post =>
       --  Nothing is closed unless the caller asked for it.
       (if Plan'Result = Close_Then_Flush then Policy = Close_Open_Handles and then Open > 0)
       --  A volume with open handles is never flushed and detached under them unasked.
       and then (if Open > 0 and then Policy = Refuse_If_Open then Plan'Result /= Flush_Only
                                                                 and then Plan'Result /= Close_Then_Flush)
       and then (if Plan'Result in Flush_Only | Close_Then_Flush then Attached and then Removable);

   --  The answer once the flush ran (Flushed) for a plan that flushes.
   function After_Flush (Flushed : Boolean) return Answer is
     (if Flushed then (Result => Safe_To_Remove, Open => 0)
      else (Result => Flush_Failed, Open => 0))
     with Post => Well_Formed (After_Flush'Result)
                  and then (After_Flush'Result.Result = Safe_To_Remove) = Flushed;

end CuBit.Volume_Ejection;
