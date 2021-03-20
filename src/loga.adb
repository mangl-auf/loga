with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Environment_Variables;
with GNAT.String_Split;

package body Loga is
   ----------------
   -- New_Logger --
   ----------------

   Current_Color : Natural range Colors_As_Integer (Colors'Val (1)) ..
      Colors_As_Integer (Colors'Last) := Colors_As_Integer (Red);

   procedure New_Logger (Self  : in out Logger;
                         Name  : String;
                         Color : Colors := Normal)
   is
      function Contains_Wildcard (Var : String; Index : out Positive)
        return Boolean;

      function Contains_Wildcard (Var : String; Index : out Positive)
        return Boolean is
      begin
         for I in Var'Range loop
            if Var (I) = '*' then
               Index := I;
               return True;
            end if;
         end loop;

         return False;
      end Contains_Wildcard;

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
         declare
            Var : constant String :=
              GNAT.String_Split.Slice (Vars_In_Debug_Env_Var, I);
            Index_Of_Wildcard : Positive;
         begin
            if Name'Length >= Var'Length - 1
              and then Contains_Wildcard (Var, Index_Of_Wildcard)
            then
               if Index_Of_Wildcard = 1
                 and then Var (2 .. Var'Length) =
                   Name (Name'Last - Var'Length + 2 .. Name'Last)
               then
                  Self.Disabled := False;
               elsif Index_Of_Wildcard = Var'Last
                 and then Var (Var'First .. Var'Last - 1) =
                   Name (Name'First .. Var'Length - 1)
               then
                  Self.Disabled := False;
               end if;
            elsif Var = Name then
               Self.Disabled := False;
               exit;
            end if;
         end;
      end loop;

      if Self.Disabled then
         return;
      end if;

      Self.Name := To_Unbounded_String (Name);
      if Current_Color > Colors_As_Integer (Colors'Last) then
         Current_Color := Colors_As_Integer (Colors'Val (1));
      end if;

      if Color /= Normal then
         Self.Color := Color;
      else
         Self.Color := Colors'Val (Current_Color - 30);
      end if;
      Current_Color := Current_Color + 1;
   end New_Logger;

   ---------
   -- Log --
   ---------

   procedure Log (Self : Logger; Message : String) is
      procedure Colorize_Output (Color : Colors);
      function Start_Bold return String;
      function End_Bold return String;

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
   begin
      if not Self.Disabled then
         Colorize_Output (Self.Color);
         Put (Start_Bold & "  " & To_String (Self.Name) & End_Bold & " ");
         Colorize_Output (Normal);
         Put_Line (Message);
      end if;
   end Log;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color (Self : Logger) return Colors is
   begin
      return Self.Color;
   end Get_Color;
end Loga;
