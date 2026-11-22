with Intel_GPU_Display_Enable;
with Intel_GPU_Display_Lease;
package body Intel_GPU_Display_Power is
   use Intel_GPU_Display_Topology;
   Limit : Positive := 1;
   generic
      Item : Request_Well;
   package Binding is
      procedure Hold (Added, Success : out Boolean);
      procedure Drop (Added : Boolean; Success : out Boolean);
   end Binding;
   package body Binding is
      Was_Added : Boolean := False;
      procedure Post (Success : out Boolean) is
      begin Post_Enable (Item, Success); end Post;
      procedure Pre (Success : out Boolean) is
      begin Pre_Disable (Item, Success); end Pre;
      package Transaction is new Intel_GPU_Display_Enable
        (Item, Read_32, Write_32, Now_Us, Pause, Post, Pre);
      use type Transaction.Result;
      procedure Hold (Added, Success : out Boolean) is
         Result : Transaction.Result;
      begin
         Transaction.Execute (True, Limit, Added, Result);
         Success := Result = Transaction.Ready;
         if Success then Was_Added := Added; end if;
      end Hold;
      procedure Drop (Added : Boolean; Success : out Boolean) is
         Result : Transaction.Result;
      begin
         Success := False;
         if Added /= Was_Added then return; end if;
         Transaction.Release (Result);
         Success := Result = Transaction.Released;
      end Drop;
   end Binding;
   package One is new Binding (PW1);
   package Two is new Binding (PW2);
   package PA is new Binding (PWA);
   package PB is new Binding (PWB);
   package PC is new Binding (PWC);
   package PD is new Binding (PWD);
   procedure Hold (Item : Well; Added, Success : out Boolean) is
   begin
      case Item is
         when PW1 => One.Hold (Added, Success);
         when DC_Off => Hold_DC_Off (Added, Success);
         when PW2 => Two.Hold (Added, Success);
         when PWA => PA.Hold (Added, Success);
         when PWB => PB.Hold (Added, Success);
         when PWC => PC.Hold (Added, Success);
         when PWD => PD.Hold (Added, Success);
      end case;
   end Hold;
   procedure Drop (Item : Well; Added : Boolean; Success : out Boolean) is
   begin
      case Item is
         when PW1 => One.Drop (Added, Success);
         when DC_Off => Drop_DC_Off (Added, Success);
         when PW2 => Two.Drop (Added, Success);
         when PWA => PA.Drop (Added, Success);
         when PWB => PB.Drop (Added, Success);
         when PWC => PC.Drop (Added, Success);
         when PWD => PD.Drop (Added, Success);
      end case;
   end Drop;
   package Lease is new Intel_GPU_Display_Lease (Well, Valid, Hold, Drop);
   use type Lease.State_Kind;
   function State return Ownership_State is
     (case Lease.State is when Lease.Idle => Idle, when Lease.Held => Held,
      when Lease.Faulted => Faulted);
   function Retained return Interfaces.Unsigned_64 is (Lease.Retained);
   function Uncertain return Interfaces.Unsigned_64 is (Lease.Uncertain);
   procedure Acquire
     (Item : Pipe; Authority_Ready : Boolean; Poll_Limit : Positive; Success : out Boolean) is
   begin
      Success := False;
      if not Authority_Ready or else Lease.State /= Lease.Idle then return; end if;
      Limit := Poll_Limit;
      Lease.Acquire (Required (Item), Success);
   end Acquire;
   procedure Release (Success : out Boolean) is
   begin Lease.Release (Success); end Release;
end Intel_GPU_Display_Power;
