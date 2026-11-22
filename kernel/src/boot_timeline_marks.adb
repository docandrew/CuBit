pragma Ada_2022;
with Interfaces; use Interfaces;
with Boot_Timeline;
with Boot_Timeline_Layout;
with Process_Identities;
with Service_Registry;

package body Boot_Timeline_Marks is
    package BT renames Boot_Timeline_Layout;

    -- The driver role whose registration authority may mark each
    -- milestone: the service that owns the event.
    Owner : constant array (BT.Milestone) of Service_Registry.DriverID :=
      [BT.Startup_Profile_Started  => Service_Registry.DRIVER_PROCMGR,
       BT.Startup_Profile_Launched => Service_Registry.DRIVER_PROCMGR,
       BT.Display_Leased           => Service_Registry.DRIVER_DISPLAY,
       BT.First_Frame_Presented    => Service_Registry.DRIVER_DESKTOP,
       BT.First_Input_Accepted     => Service_Registry.DRIVER_DESKTOP];

    function Is_Mark (Request : Process.Message) return Boolean is
      (Request.tag.label = BT.Mark_Label);

    -- A notification capability covers role: its object is the range of
    -- roles (first, count) it registers and publishes.
    function Covers (Cap : Capabilities.Capability; Role : Unsigned_64) return Boolean is
      (Cap.object.param > 0 and then Role >= Cap.object.ref
       and then Role - Cap.object.ref < Cap.object.param);

    procedure Call (Caller    : Process.ProcessID;
                    Authority : Capabilities.Capability;
                    Request   : Process.Message;
                    Reply     : out Process.Message)
    is
        use type Capabilities.CapabilityType;
        Outcome : BT.Mark_Outcome := BT.Unknown_Milestone;
        Code : constant Unsigned_64 := Request.words (BT.Milestone_Word);
    begin
        if Request.tag.length /= BT.Mark_Request_Words then
            Outcome := BT.Malformed;
        else
            for Item in BT.Milestone loop
                if Code = BT.Milestone'Enum_Rep (Item) then
                    if Authority.capType = Capabilities.CAP_NOTIFICATION
                      and then Authority.rights (Capabilities.RIGHT_WRITE)
                      and then Covers (Authority, Unsigned_64 (Owner (Item)))
                    then
                        Boot_Timeline.Mark_Milestone
                          (Item, Process_Identities.To_Word (Process.identityOf (Caller)));
                        Outcome := BT.Marked;
                    else
                        Outcome := BT.Not_Owner;
                    end if;
                end if;
            end loop;
        end if;
        Reply := Process.NULL_MESSAGE;
        Reply.tag.label := Request.tag.label;
        Reply.tag.length := BT.Mark_Reply_Words;
        Reply.words (BT.Outcome_Word) := Unsigned_64 (BT.Mark_Outcome'Enum_Rep (Outcome));
    end Call;
end Boot_Timeline_Marks;
