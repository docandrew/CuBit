------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A remappable keymap (UI-052): chords bound to an application's typed
--  commands, the one source of key dispatch and of every shortcut hint.
--
--  @description
--  An application declares its commands as an enumeration and its default
--  chords in one table (Load). Key input goes through Find to a command;
--  every hint (menus, tooltips, buttons, the button bar) shows Hint, the
--  command's current chord, so a remap changes dispatch and every hint at
--  once, and a command without a chord shows no keycap.
--
--  Bindings are checked when made, never at use: Bind refuses a chord that
--  is another command's (Taken), a reserved one's (Reserved_Taken: a menu
--  mnemonic, Alt and a title's letter, or a toolkit standard chord such as
--  the clipboard's, CuBit.UI.Standard_Keys), the secure attention chord (Reserved_Chord,
--  UI-045) and a chord of the other layer (Other_Layer: Super chords are
--  the environment's, UI-052). A command may have several chords (F2 and
--  Ctrl+R both refresh); its hint is the first bound, or after a remap the
--  chord it was moved to.
--
--  Proved (tests/ui-keys, level 2): every operation keeps the keymap Valid
--  (a hint is always a chord bound to its command, and a command with no
--  hint has no chord), Bind and Remap refuse exactly the cases above and
--  change nothing when they refuse, and Find agrees with Command_At.
--  No allocation: a keymap is one table of every possible chord.
------------------------------------------------------------------------------
with CuBit.UI.Keys;

generic
   type Command is (<>);
   Layer : CuBit.UI.Keys.Layer := CuBit.UI.Keys.Application_Layer;
package CuBit.UI.Keymaps with SPARK_Mode is
   package Keys renames CuBit.UI.Keys;
   use type Keys.Chord;
   use type Keys.Layer, Keys.Key, Keys.Modifier;

   type Bind_Result is
     (Bound,           --  done (or already so)
      Not_A_Chord,     --  no key, or a modifier key by itself
      Reserved_Chord,  --  the secure attention chord: nobody's
      Other_Layer,     --  a Super chord in an app's keymap, or one without Super in the environment's
      Reserved_Taken,  --  a menu mnemonic's (Alt and a title's letter) or a toolkit standard chord's
      Taken,           --  another command's chord
      Not_Bound);      --  Unbind/Remap: the chord is not this command's

   type Keymap is private;

   function Valid (Map : Keymap) return Boolean with Ghost;
   procedure Clear (Map : out Keymap)
   with Post => Valid (Map) and then (for all C in Command => not Has_Binding (Map, C));

   --  What a chord is in this keymap.
   function Is_Bound (Map : Keymap; Item : Keys.Chord) return Boolean;
   function Is_Toolkit_Chord (Map : Keymap; Item : Keys.Chord) return Boolean;
   function Command_At (Map : Keymap; Item : Keys.Chord) return Command
   with Pre => Is_Bound (Map, Item);
   --  Some chord is bound to Target.
   function Has_Binding (Map : Keymap; Target : Command) return Boolean;

   --  A and B agree on every chord but Item (what Bind and Unbind change).
   function Same_Except (A, B : Keymap; Item : Keys.Chord) return Boolean with Ghost;

   --  Dispatch: the command a pressed chord runs.
   procedure Find (Map : Keymap; Item : Keys.Chord; Found : out Boolean; Target : out Command)
   with Post => Found = Is_Bound (Map, Item) and then (if Found then Target = Command_At (Map, Item));

   --  The chord every hint shows for Target; No_Chord when it has none.
   function Hint (Map : Keymap; Target : Command) return Keys.Chord
   with Pre => Valid (Map),
        Post => (if Hint'Result = Keys.No_Chord then not Has_Binding (Map, Target)
                 else Is_Bound (Map, Hint'Result) and then Command_At (Map, Hint'Result) = Target);

   --  Whether a chord may be bound at all in this keymap's layer.
   function Allowed (Item : Keys.Chord) return Boolean is
     (Keys.Is_Chord (Item) and then not Keys.Is_Reserved (Item) and then Keys.Layer_Of (Item) = Layer);

   --  Why Bind of Item to Target would be refused (Bound: it would not).
   function Check (Map : Keymap; Item : Keys.Chord; Target : Command) return Bind_Result is
     (if not Keys.Is_Chord (Item) then Not_A_Chord
      elsif Keys.Is_Reserved (Item) then Reserved_Chord
      elsif Keys.Layer_Of (Item) /= Layer then Other_Layer
      elsif Is_Toolkit_Chord (Map, Item) then Reserved_Taken
      elsif Is_Bound (Map, Item) and then Command_At (Map, Item) /= Target then Taken
      else Bound)
   with Post => Check'Result in Bound .. Taken;

   procedure Bind (Map : in out Keymap; Item : Keys.Chord; Target : Command; Result : out Bind_Result)
   with Pre => Valid (Map),
        Post => Valid (Map) and then Result = Check (Map'Old, Item, Target)
          and then Same_Except (Map, Map'Old, Item)
          and then (if Result = Bound then Is_Bound (Map, Item) and then Command_At (Map, Item) = Target
                      and then (for all C in Command =>
                                  (if C /= Target then Hint (Map, C) = Hint (Map'Old, C)))
                      and then (if Hint (Map'Old, Target) /= Keys.No_Chord
                                then Hint (Map, Target) = Hint (Map'Old, Target)
                                else Hint (Map, Target) = Item)
                    else Map = Map'Old);

   --  Take a chord away from Target (Not_Bound if it is not Target's).
   procedure Unbind (Map : in out Keymap; Item : Keys.Chord; Target : Command; Result : out Bind_Result)
   with Pre => Valid (Map),
        Post => Valid (Map)
          and then (Result = Bound) = (Is_Bound (Map'Old, Item) and then Command_At (Map'Old, Item) = Target)
          and then Same_Except (Map, Map'Old, Item)
          and then (if Result = Bound then not Is_Bound (Map, Item)
                      and then (for all C in Command =>
                                  (if C /= Target then Hint (Map, C) = Hint (Map'Old, C)))
                    else Result = Not_Bound and then Map = Map'Old);

   --  Move Target's chord From to To: To must be free (or Target's) and
   --  allowed, From must be Target's. To becomes the hint if From was.
   procedure Remap
     (Map : in out Keymap; Target : Command; From, To : Keys.Chord; Result : out Bind_Result)
   with Pre => Valid (Map),
        Post => Valid (Map)
          and then (if not (Is_Bound (Map'Old, From) and then Command_At (Map'Old, From) = Target)
                    then Result = Not_Bound
                    else Result = Check (Map'Old, To, Target))
          and then (if Result = Bound then
                      Is_Bound (Map, To) and then Command_At (Map, To) = Target
                      and then (if From /= To then not Is_Bound (Map, From))
                      and then (if Hint (Map'Old, Target) = From then Hint (Map, Target) = To)
                    else Map = Map'Old);

   --  Reserve a chord for the toolkit: a menu title's mnemonic
   --  (CuBit.UI.Menus: Alt+letter opens it) or a standard chord the toolkit
   --  handles itself (CuBit.UI.Standard_Keys: the clipboard's). Part of the
   --  conflict check both ways: an app command can't take a reserved chord
   --  and a reservation can't take an app command's chord.
   procedure Reserve (Map : in out Keymap; Item : Keys.Chord; Result : out Bind_Result)
   with Pre => Valid (Map),
        Post => Valid (Map)
          and then Result = (if not Allowed (Item) then
                               (if not Keys.Is_Chord (Item) then Not_A_Chord
                                elsif Keys.Is_Reserved (Item) then Reserved_Chord
                                else Other_Layer)
                             elsif Is_Bound (Map'Old, Item) then Taken
                             else Bound)
          and then (if Result = Bound then Is_Toolkit_Chord (Map, Item)
                      and then (for all C in Command => Hint (Map, C) = Hint (Map'Old, C))
                    else Map = Map'Old);
   --  A menu title's mnemonic: Reserve (Alt + Letter).
   function Mnemonic_Chord (Letter : Keys.Key) return Keys.Chord is ((Keys.Alt + Letter));
   procedure Reserve_Mnemonic (Map : in out Keymap; Letter : Keys.Key; Result : out Bind_Result)
   with Pre => Valid (Map),
        Post => Valid (Map)
          and then (if Result = Bound then Is_Toolkit_Chord (Map, Mnemonic_Chord (Letter))
                      and then (for all C in Command => Hint (Map, C) = Hint (Map'Old, C))
                    else Map = Map'Old);

   --  An application's defaults, declared in one place, and the user's
   --  remaps (typed settings) applied over them.
   type Binding is record
      Item : Keys.Chord;
      Target : Command;
   end record;
   type Binding_List is array (Positive range <>) of Binding;
   --  Clear, then bind each in order. Refused counts the defaults that
   --  conflicted (a mistake in the table; tests assert none).
   procedure Load (Map : out Keymap; Defaults : Binding_List; Refused : out Natural)
   with Post => Valid (Map);

   type Remap_Entry is record
      Target : Command;
      From, To : Keys.Chord;
   end record;
   type Remap_List is array (Positive range <>) of Remap_Entry;
   --  Each remap that is refused leaves the keymap as it was; Refused
   --  counts them.
   procedure Apply (Map : in out Keymap; Remaps : Remap_List; Refused : out Natural)
   with Pre => Valid (Map), Post => Valid (Map);

private
   use type Keys.Modifier_Set;
   KEY_COUNT : constant := Keys.Key'Pos (Keys.Key'Last) + 1;
   MODIFIER_COMBINATIONS : constant := 16;
   subtype Modifier_Number is Natural range 0 .. MODIFIER_COMBINATIONS - 1;
   subtype Slot_Index is Natural range 0 .. MODIFIER_COMBINATIONS * KEY_COUNT - 1;

   function Modifier_Index (Set : Keys.Modifier_Set) return Modifier_Number is
     (Boolean'Pos (Set (Keys.Shift)) + 2 * Boolean'Pos (Set (Keys.Ctrl))
      + 4 * Boolean'Pos (Set (Keys.Alt)) + 8 * Boolean'Pos (Set (Keys.Super)));
   function Index (Item : Keys.Chord) return Slot_Index is
     (Modifier_Index (Item.Modifiers) * KEY_COUNT + Keys.Key'Pos (Item.Pressed));

   type Slot_Kind is (Free, Command_Slot, Reserved_Slot);
   type Slot is record
      Kind : Slot_Kind := Free;
      Target : Command := Command'First;
      Item : Keys.Chord := Keys.No_Chord;
   end record;
   type Slot_Table is array (Slot_Index) of Slot;
   type Hint_Table is array (Command) of Keys.Chord;
   type Keymap is record
      Slots : Slot_Table := [others => (Kind => Free, Target => Command'First, Item => Keys.No_Chord)];
      Hints : Hint_Table := [others => Keys.No_Chord];
   end record;

   function Is_Bound (Map : Keymap; Item : Keys.Chord) return Boolean is
     (Map.Slots (Index (Item)).Kind = Command_Slot and then Map.Slots (Index (Item)).Item = Item);
   function Is_Toolkit_Chord (Map : Keymap; Item : Keys.Chord) return Boolean is
     (Map.Slots (Index (Item)).Kind = Reserved_Slot and then Map.Slots (Index (Item)).Item = Item);
   function Command_At (Map : Keymap; Item : Keys.Chord) return Command is
     (Map.Slots (Index (Item)).Target);
   function Has_Binding (Map : Keymap; Target : Command) return Boolean is
     (for some J in Slot_Index => Map.Slots (J).Kind = Command_Slot and then Map.Slots (J).Target = Target);

   --  Every occupied slot holds the chord that indexes it (an allowed one),
   --  and each command's hint is one of its chords, or none when it has none.
   function Slots_Valid (Map : Keymap) return Boolean is
     (for all J in Slot_Index =>
        (if Map.Slots (J).Kind /= Free then
           Index (Map.Slots (J).Item) = J and then Allowed (Map.Slots (J).Item)))
   with Ghost;
   function Hints_Valid (Map : Keymap) return Boolean is
     (for all C in Command =>
        (if Map.Hints (C) = Keys.No_Chord then not Has_Binding (Map, C)
         else Is_Bound (Map, Map.Hints (C)) and then Command_At (Map, Map.Hints (C)) = C))
   with Ghost;
   function Valid (Map : Keymap) return Boolean is (Slots_Valid (Map) and then Hints_Valid (Map));
   function Same_Except (A, B : Keymap; Item : Keys.Chord) return Boolean is
     (for all J in Slot_Index => (if J /= Index (Item) then A.Slots (J) = B.Slots (J)));
end CuBit.UI.Keymaps;
