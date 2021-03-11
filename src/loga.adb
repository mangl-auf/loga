with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Environment_Variables;
with GNAT.String_Split;

package body Loga is
   ----------------
   -- New_Logger --
   ----------------

   procedure New_Logger (Self : in out Logger; Name : String) is
      package Env renames Ada.Environment_Variables;

      Debug_Env_Var         : Unbounded_String;
      Vars_In_Debug_Env_Var : GNAT.String_Split.Slice_Set;
   begin
      begin
         Debug_Env_Var := To_Unbounded_String (Env.Value ("DEBUG"));
      exception
         when Constraint_Error =>
            Self.Disabled := True;
            return;
      end;
      GNAT.String_Split.Create (S => Vars_In_Debug_Env_Var,
                               From => To_String (Debug_Env_Var),
                               Separators => ",",
                               Mode => GNAT.String_Split.Multiple);

      for I in 1 .. GNAT.String_Split.Slice_Count (Vars_In_Debug_Env_Var) loop
         if GNAT.String_Split.Slice (Vars_In_Debug_Env_Var, I) = Name then
            Self.Disabled := False;
            exit;
         else
            Self.Disabled := True;
         end if;
      end loop;

      if Self.Disabled then
         return;
      end if;

      Self.Name := To_Unbounded_String (Name);
      if Current_Color > Colors_As_Integer (Colors'Last) then
         Current_Color := Colors_As_Integer (Colors'Val (1));
      end if;
      Self.Color    := Colors'Val (Current_Color - 30);
      Current_Color := Current_Color + 1;
   end New_Logger;

   ---------
   -- Log --
   ---------

   procedure Log (Self : Logger; Message : String) is
   begin
      if not Self.Disabled then
         Colorize_Output (Self.Color);
         Put (Start_Bold & "  " & To_String (Self.Name) & End_Bold & " ");
         Colorize_Output (Normal);
         Put_Line (Message);
      end if;
   end Log;

   -- Private part --

   procedure Colorize_Output (Color : Colors) is
      use Ada.Integer_Text_IO;
   begin
      Put (Character'Val (27) & "[");
      Put (Colors_As_Integer (Color), Width => 0);
      Put ("m");
   end Colorize_Output;

   function Start_Bold return String is
   begin
      return Character'Val (27) & "[1m";
   end Start_Bold;

   function End_Bold return String is
   begin
      return Character'Val (27) & "[0m";
   end End_Bold;
end Loga;