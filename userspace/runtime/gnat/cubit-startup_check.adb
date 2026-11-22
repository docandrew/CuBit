------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Launch_Block;
pragma Elaborate_All (CuBit.Launch_Block);
with CuBit.Messages;
with CuBit.Startup_Tables;

package body CuBit.Startup_Check is
   package SS renames CuBit.Startup_State;
   package ST renames CuBit.Startup_Tables;
   use type SS.Verification_Status;

   procedure Explain
     (Expected : CuBit.Startup_State.Expectations;
      Program  : String;
      Ready    : out Boolean;
      Text     : out String;
      Length   : out Text_Length)
   is
      Found : constant SS.Verification := SS.Verify (Expected);

      procedure Put (Part : String);
      procedure Put (Part : String) is
      begin
         for C of Part loop
            exit when Length = Maximum_Text;
            Length := Length + 1;
            Text (Length) := C;
         end loop;
      end Put;

      procedure Put_Label (Item : ST.Name);
      procedure Put_Label (Item : ST.Name) is
      begin
         Put ("""");
         for I in 1 .. Item.Length loop
            Put ((1 => Item.Text (I)));
         end loop;
         Put ("""");
      end Put_Label;
   begin
      Text := [others => ' '];
      Length := 0;
      Ready := SS.Ready (Found);
      if Ready then
         return;
      end if;
      Put ("startup: ");
      Put (Program);
      Put (": ");
      if Found.Status = SS.Not_Loaded then
         if CuBit.Launch_Block.Table_Present then
            Put ("its startup table did not load (");
            case CuBit.Launch_Block.Outcome is
               when SS.Malformed      => Put ("malformed");
               when SS.Incomplete     => Put ("incomplete");
               when SS.Loaded         => Put ("loaded");
               when SS.Already_Loaded => Put ("already loaded");
            end case;
            Put (")");
         else
            Put ("the launch block carried no startup table (the program's entry glue dropped it)");
         end if;
      else
         case Found.Status is
            when SS.Missing     => Put ("its startup table has no ");
            when SS.Wrong_Kind  => Put ("its startup table has another kind of ");
            when SS.Wrong_Need  => Put ("its startup table disagrees on the need of ");
            when SS.Not_Granted => Put ("it was not granted its required ");
            when SS.Ready | SS.Not_Loaded => null;
         end case;
         Put_Label (Expected (Found.Index).Label);
      end if;
      Put ("; stopping (CAP-003 startup check)");
   end Explain;

   procedure Stop is
      Ignore : Unsigned_64;
   begin
      loop
         Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_EXIT, Startup_Refused);
      end loop;
   end Stop;

   procedure Require
     (Expected : CuBit.Startup_State.Expectations;
      Program  : String)
   is
      Ready : Boolean;
      Text : String (1 .. Maximum_Text);
      Length : Text_Length;
   begin
      Explain (Expected, Program, Ready, Text, Length);
      if not Ready then
         CuBit.Messages.debugPrint (Text (1 .. Length));
         CuBit.Messages.debugPrint ((1 => ASCII.LF));
         Stop;
      end if;
   end Require;

end CuBit.Startup_Check;
