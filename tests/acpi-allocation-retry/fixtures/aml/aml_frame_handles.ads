with AML_Identity;
package AML_Frame_Handles with SPARK_Mode, Pure is
   Max_Call_Depth : constant := 32;
   Max_Active_Frames : constant := Max_Call_Depth + 1;
   subtype Frame_Capacity is Positive range 1 .. Max_Active_Frames;
   type Frame_Index is range 0 .. Max_Active_Frames;
   type Invocation_Serial is new Natural;
   type Frame_Generation is new Natural;
   subtype Generation_Budget is Frame_Generation range 1 .. Frame_Generation'Last;
   type Cell_ID is
     (Local_0, Local_1, Local_2, Local_3, Local_4, Local_5, Local_6, Local_7,
      Arg_0, Arg_1, Arg_2, Arg_3, Arg_4, Arg_5, Arg_6);
   type Invocation_Domain is private;
   No_Domain : constant Invocation_Domain;
   function Bind_Domain (Owner : AML_Identity.Identity; Serial : Invocation_Serial)
     return Invocation_Domain;
   function Has_Authority (Domain : Invocation_Domain) return Boolean;
   type Frame_Handle is private;
   No_Frame : constant Frame_Handle;
   function Bind_Frame
     (Domain : Invocation_Domain; Index : Frame_Index; Generation : Frame_Generation)
      return Frame_Handle;
   function Present (Frame : Frame_Handle) return Boolean;
   function Belongs_To (Frame : Frame_Handle; Domain : Invocation_Domain) return Boolean;
   function Index_Of (Frame : Frame_Handle) return Frame_Index;
   function Generation_Of (Frame : Frame_Handle) return Frame_Generation;
   type Cell_Handle is private;
   No_Cell : constant Cell_Handle;
   function Bind_Cell (Frame : Frame_Handle; Cell : Cell_ID) return Cell_Handle;
   function Present (Cell : Cell_Handle) return Boolean;
   function Belongs_To (Cell : Cell_Handle; Domain : Invocation_Domain) return Boolean;
   function Matches (Cell : Cell_Handle; Frame : Frame_Handle) return Boolean;
   function Index_Of (Cell : Cell_Handle) return Frame_Index;
   function Generation_Of (Cell : Cell_Handle) return Frame_Generation;
   function Cell_Of (Cell : Cell_Handle) return Cell_ID;
   -- No domain/token extractor: possessing one cell does not provide a frame
   -- handle or the domain used to manufacture handles to other cells.
private
   use type AML_Identity.Identity;
   type Invocation_Domain is record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Serial : Invocation_Serial := 0;
   end record;
   No_Domain : constant Invocation_Domain := (others => <>);
   function Has_Authority (Domain : Invocation_Domain) return Boolean is
     (Domain.Owner /= AML_Identity.No_Identity and then Domain.Serial /= 0);
   function Bind_Domain (Owner : AML_Identity.Identity; Serial : Invocation_Serial)
     return Invocation_Domain is
     (if Owner = AML_Identity.No_Identity or else Serial = 0 then No_Domain
      else (Owner => Owner, Serial => Serial));
   type Frame_Handle is record
      Domain : Invocation_Domain := No_Domain;
      Index : Frame_Index := 0;
      Generation : Frame_Generation := 0;
   end record;
   No_Frame : constant Frame_Handle := (others => <>);
   function Bind_Frame
     (Domain : Invocation_Domain; Index : Frame_Index; Generation : Frame_Generation)
      return Frame_Handle is
     (if Index = 0 or else Generation = 0 then No_Frame
      else (Domain => Domain, Index => Index, Generation => Generation));
   function Present (Frame : Frame_Handle) return Boolean is
     (Frame.Index /= 0 and then Frame.Generation /= 0);
   function Belongs_To (Frame : Frame_Handle; Domain : Invocation_Domain) return Boolean is
     (Present (Frame) and then Frame.Domain = Domain);
   function Index_Of (Frame : Frame_Handle) return Frame_Index is (Frame.Index);
   function Generation_Of (Frame : Frame_Handle) return Frame_Generation is (Frame.Generation);
   type Cell_Handle is record
      Frame : Frame_Handle := No_Frame;
      Cell : Cell_ID := Local_0;
   end record;
   No_Cell : constant Cell_Handle := (others => <>);
   function Bind_Cell (Frame : Frame_Handle; Cell : Cell_ID) return Cell_Handle is
     (if not Present (Frame) or else not Has_Authority (Frame.Domain) then No_Cell
      else (Frame => Frame, Cell => Cell));
   function Present (Cell : Cell_Handle) return Boolean is
     (Present (Cell.Frame) and then Has_Authority (Cell.Frame.Domain));
   function Belongs_To (Cell : Cell_Handle; Domain : Invocation_Domain) return Boolean is
     (Present (Cell) and then Cell.Frame.Domain = Domain);
   function Matches (Cell : Cell_Handle; Frame : Frame_Handle) return Boolean is
     (Present (Cell) and then Present (Frame) and then Cell.Frame = Frame);
   function Index_Of (Cell : Cell_Handle) return Frame_Index is (Cell.Frame.Index);
   function Generation_Of (Cell : Cell_Handle) return Frame_Generation is (Cell.Frame.Generation);
   function Cell_Of (Cell : Cell_Handle) return Cell_ID is (Cell.Cell);
end AML_Frame_Handles;
