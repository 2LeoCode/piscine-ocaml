class alkane_combustion (alkanes : Alkane.alkane list) =
  object
    inherit Reaction.reaction
    method get_start = alkane
  end
