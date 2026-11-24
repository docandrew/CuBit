------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Keymaps with SPARK_Mode is

   --  Index is one-to-one: a slot belongs to exactly one chord.
   procedure Lemma_Index_Injective (Left, Right : Keys.Chord)
   with Ghost, Global => null,
        Post => (if Index (Left) = Index (Right) then Left = Right);
   procedure Lemma_Index_Injective (Left, Right : Keys.Chord) is
   begin
      if Index (Left) = Index (Right) then
         pragma Assert (Keys.Key'Pos (Left.Pressed) = Keys.Key'Pos (Right.Pressed));
         pragma Assert (Left.Pressed = Right.Pressed);
         pragma Assert (Modifier_Index (Left.Modifiers) = Modifier_Index (Right.Modifiers));
         pragma Assert (for all M in Keys.Modifier => Left.Modifiers (M) = Right.Modifiers (M));
         pragma Assert (Left.Modifiers = Right.Modifiers);
      end if;
   end Lemma_Index_Injective;

   procedure Clear (Map : out Keymap) is
   begin
      Map := (Slots => [others => (Kind => Free, Target => Command'First, Item => Keys.No_Chord)],
              Hints => [others => Keys.No_Chord]);
   end Clear;

   procedure Find (Map : Keymap; Item : Keys.Chord; Found : out Boolean; Target : out Command) is
   begin
      Found := Is_Bound (Map, Item);
      Target := (if Found then Command_At (Map, Item) else Command'First);
   end Find;

   function Hint (Map : Keymap; Target : Command) return Keys.Chord is (Map.Hints (Target));

   --  The first chord (in slot order) bound to Target, or No_Chord.
   function First_Chord (Map : Keymap; Target : Command) return Keys.Chord
   with Pre => Slots_Valid (Map),
        Post => (if First_Chord'Result = Keys.No_Chord then not Has_Binding (Map, Target)
                 else Is_Bound (Map, First_Chord'Result) and then Command_At (Map, First_Chord'Result) = Target)
   is
   begin
      for J in Slot_Index loop
         if Map.Slots (J).Kind = Command_Slot and then Map.Slots (J).Target = Target then
            pragma Assert (Index (Map.Slots (J).Item) = J);
            pragma Assert (Keys.Is_Chord (Map.Slots (J).Item));
            return Map.Slots (J).Item;
         end if;
         pragma Loop_Invariant
           (for all K in Slot_Index'First .. J =>
              not (Map.Slots (K).Kind = Command_Slot and then Map.Slots (K).Target = Target));
      end loop;
      return Keys.No_Chord;
   end First_Chord;

   procedure Bind (Map : in out Keymap; Item : Keys.Chord; Target : Command; Result : out Bind_Result) is
      Where : constant Slot_Index := Index (Item);
   begin
      Result := Check (Map, Item, Target);
      if Result /= Bound then
         return;
      end if;
      if Is_Bound (Map, Item) then
         --  Already Target's (Check refused another command's).
         return;
      end if;
      --  The slot is free: an occupied slot holds the chord indexing it
      --  (Slots_Valid), so it would be Item, bound or a mnemonic.
      pragma Assert
        (if Map.Slots (Where).Kind /= Free then Index (Map.Slots (Where).Item) = Where);
      Lemma_Index_Injective (Map.Slots (Where).Item, Item);
      pragma Assert (Map.Slots (Where).Kind = Free);
      declare
         Old : constant Keymap := Map with Ghost;
      begin
         Map.Slots (Where) := (Kind => Command_Slot, Target => Target, Item => Item);
         pragma Assert (for all C in Command => (if C /= Target then
                          (Has_Binding (Map, C) = Has_Binding (Old, C))));
         pragma Assert (for all C in Command => (if Old.Hints (C) /= Keys.No_Chord then
                          Index (Old.Hints (C)) /= Where));
         if Map.Hints (Target) = Keys.No_Chord then
            Map.Hints (Target) := Item;
         end if;
      end;
   end Bind;

   procedure Unbind (Map : in out Keymap; Item : Keys.Chord; Target : Command; Result : out Bind_Result) is
      Where : constant Slot_Index := Index (Item);
   begin
      if not (Is_Bound (Map, Item) and then Command_At (Map, Item) = Target) then
         Result := Not_Bound;
         return;
      end if;
      Result := Bound;
      declare
         Old : constant Keymap := Map with Ghost;
      begin
         Map.Slots (Where) := (Kind => Free, Target => Command'First, Item => Keys.No_Chord);
         pragma Assert (Slots_Valid (Map));
         pragma Assert (for all C in Command => (if C /= Target then
                          (Has_Binding (Map, C) = Has_Binding (Old, C))));
         pragma Assert (for all C in Command => (if C /= Target and then Old.Hints (C) /= Keys.No_Chord then
                          Index (Old.Hints (C)) /= Where));
         if Map.Hints (Target) = Item then
            Map.Hints (Target) := First_Chord (Map, Target);
         else
            pragma Assert (Map.Hints (Target) /= Keys.No_Chord);
            Lemma_Index_Injective (Map.Hints (Target), Item);
         end if;
      end;
   end Unbind;

   procedure Remap
     (Map : in out Keymap; Target : Command; From, To : Keys.Chord; Result : out Bind_Result)
   is
      Was_Hint : constant Boolean := Map.Hints (Target) = From;
      Unbound : Bind_Result;
   begin
      if not (Is_Bound (Map, From) and then Command_At (Map, From) = Target) then
         Result := Not_Bound;
         return;
      end if;
      Result := Check (Map, To, Target);
      if Result /= Bound or else From = To then
         return;
      end if;
      Lemma_Index_Injective (From, To);
      pragma Assert (Index (From) /= Index (To));
      Bind (Map, To, Target, Result);
      pragma Assert (Result = Bound);
      pragma Assert (Is_Bound (Map, From) and then Command_At (Map, From) = Target);
      Unbind (Map, From, Target, Unbound);
      pragma Assert (Unbound = Bound);
      pragma Assert (Is_Bound (Map, To) and then Command_At (Map, To) = Target);
      if Was_Hint then
         Map.Hints (Target) := To;
      end if;
   end Remap;

   procedure Reserve (Map : in out Keymap; Item : Keys.Chord; Result : out Bind_Result) is
      Where : constant Slot_Index := Index (Item);
   begin
      if not Allowed (Item) then
         Result := (if not Keys.Is_Chord (Item) then Not_A_Chord
                    elsif Keys.Is_Reserved (Item) then Reserved_Chord
                    else Other_Layer);
         return;
      elsif Is_Bound (Map, Item) then
         Result := Taken;
         return;
      end if;
      Result := Bound;
      if Map.Slots (Where).Kind /= Free then
         Lemma_Index_Injective (Map.Slots (Where).Item, Item);
      end if;
      declare
         Old : constant Keymap := Map with Ghost;
      begin
         Map.Slots (Where) := (Kind => Reserved_Slot, Target => Command'First, Item => Item);
         pragma Assert (for all C in Command => Has_Binding (Map, C) = Has_Binding (Old, C));
         pragma Assert (for all C in Command => (if Old.Hints (C) /= Keys.No_Chord then
                          Index (Old.Hints (C)) /= Where));
      end;
   end Reserve;

   procedure Reserve_Mnemonic (Map : in out Keymap; Letter : Keys.Key; Result : out Bind_Result) is
   begin
      Reserve (Map, Mnemonic_Chord (Letter), Result);
   end Reserve_Mnemonic;

   procedure Load (Map : out Keymap; Defaults : Binding_List; Refused : out Natural) is
      Result : Bind_Result;
   begin
      Clear (Map);
      Refused := 0;
      for K in Defaults'Range loop
         Bind (Map, Defaults (K).Item, Defaults (K).Target, Result);
         if Result /= Bound and then Refused < Natural'Last then
            Refused := Refused + 1;
         end if;
         pragma Loop_Invariant (Valid (Map));
      end loop;
   end Load;

   procedure Apply (Map : in out Keymap; Remaps : Remap_List; Refused : out Natural) is
      Result : Bind_Result;
   begin
      Refused := 0;
      for K in Remaps'Range loop
         Remap (Map, Remaps (K).Target, Remaps (K).From, Remaps (K).To, Result);
         if Result /= Bound and then Refused < Natural'Last then
            Refused := Refused + 1;
         end if;
         pragma Loop_Invariant (Valid (Map));
      end loop;
   end Apply;
end CuBit.UI.Keymaps;
